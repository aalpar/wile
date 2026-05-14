# Stderr flush on exit — `io` extension Closeable + main refactor

**Date**: 2026-05-14
**Source**: Side discussion during the port unification design
(`plans/2026-05-14-port-unification-design.md`). Independent of port
unification; can ship before, during, or after.
**Status**: Design.
**Priority**: Medium. Quality-of-implementation; not a conformance bug.

## Problem

Scheme output written to `(current-error-port)` is silently dropped on
many exit paths. Two-fault chain:

1. **`internal/extensions/io/state.go:89-92`** wires
   `current-error-port` to `*bufio.Writer(os.Stderr)` via
   `NewCharacterOutputPortFromWriter`. The default `bufio.Writer`
   buffer is 4096 bytes. Until that buffer fills (or
   `(flush-output-port)` is called), nothing reaches the underlying
   `os.Stderr`.
2. **`engine.go:1054-1068`** — `Engine.Close()` walks `p.closers`,
   which only contains extensions implementing `registry.Closeable`.
   The `io` extension does **not** implement `Closeable` (verified by
   grep on `internal/extensions/io/register.go`). So port parameters
   are never closed by engine shutdown, and `flushThenClose` (which
   would flush the bufio buffer) never runs.
3. **`cmd/wile/main.go`** calls `os.Exit` at 9 sites (lines 168, 170,
   176, 204, 208, 480, 491, 495, 497). `os.Exit` skips deferred
   functions — including the `defer eng.Close()` at line 260. So even
   if the io extension were made `Closeable`, most CLI exit paths
   would still bypass cleanup.

Result: a Scheme program that writes one error message and exits has
its error silently dropped. The pattern is common enough that this is
worth fixing.

```scheme
;; This output is lost on most exit paths today.
(display "error: bad config" (current-error-port))
(newline (current-error-port))
(exit 1)
```

## Why now

Independent of port unification, but raised in that design's review.
Two fixes are small (~80 LOC), local, and don't conflict with the
unification's surface area. Stacking them with port unification would
muddle review; shipping separately is cleaner.

## Design decisions

### D1 — Two coupled changes, separable into two commits

The fix is two changes that are individually correct but together
yield the user-visible benefit:

- **Change A** (io extension Closeable): `Engine.Close()` flushes
  ports on normal completion.
- **Change B** (main refactor): every CLI exit path runs deferred
  cleanup, including `eng.Close()`.

Either change shipped alone partially helps but doesn't fix the
common case. Both shipped together fix:
- Normal CLI exits (via `return` from main) → eng.Close() runs → ports flush.
- Panics during CLI execution → defer runs → eng.Close() runs → ports flush.
- Embedded use (caller controls `eng.Close()`) — Change A alone is
  sufficient; Change B is irrelevant.

Recommendation: one PR, two commits. Change A first (it's local to
`internal/extensions/io`); Change B second (it touches `cmd/wile/main.go`
broadly).

### D2 — Change A: `io` extension implements `Closeable`

`registry.Closeable` is a one-method interface
(`registry/extension.go:43`):

```go
type Closeable interface {
    Close() error
}
```

The `io` extension is constructed at `internal/extensions/io/register.go:34`
via `registry.NewDescribedExtension`, which returns an `*ExtensionFunc`.
`ExtensionFunc` (`registry/extension.go:48-52`) has only three fields
(`name`, `description`, `fn`) — no close hook. The `Close()` method
must be on whatever type backs the `Extension` returned to
`engine.go:303-306`, which does the `ext.(registry.Closeable)` assertion.

**Resolution**: add a new constructor `NewCloseableExtension` in
`registry/extension.go`. It returns a new internal struct that embeds
`ExtensionFunc` and carries a close callback:

```go
// registry/extension.go (additions)

// NewCloseableExtension creates an Extension that also implements
// Closeable. Engine.Close() invokes the closeFn for cleanup. Use this
// when an extension owns process-wide resources (goroutines, file
// handles, port parameters) that must be released on shutdown.
func NewCloseableExtension(name, description string,
    addFn func(*Registry) error, closeFn func() error) Extension {
    return &closeableExtensionFunc{
        ExtensionFunc: ExtensionFunc{
            name:        name,
            description: description,
            fn:          addFn,
        },
        closeFn: closeFn,
    }
}

type closeableExtensionFunc struct {
    ExtensionFunc
    closeFn func() error
}

func (p *closeableExtensionFunc) Close() error {
    if p.closeFn == nil {
        return nil
    }
    return p.closeFn()
}
```

Why this shape over the alternatives:

- **vs. adding `Close()` directly to `ExtensionFunc`**: That would
  make *every* extension satisfy `Closeable`, causing
  `engine.go:303-306` to register all of them as closers (the type
  assertion succeeds even when `closeFn` is nil). Wasteful and
  semantically wrong — `Closeable` should mean "this extension owns
  resources," not "this extension exists."
- **vs. a custom struct only in `internal/extensions/io`**:
  Generalizes for future extensions that need cleanup (process,
  network, time-of-day caches, etc.). The cost is one extra
  constructor + one private struct in `registry/`; net zero
  complexity for callers that don't need it.

io extension change at `register.go:34`:

```go
var Extension = registry.NewCloseableExtension("io",
    "I/O ports: reading, writing, string/bytevector ports, display, write.",
    AddToRegistry,
    closeIO)
```

`closeIO` implementation (in a new sibling file or appended to
`state.go`):

```go
func closeIO() error {
    var errs []error
    for _, getParam := range []func() *machine.Parameter{
        GetCurrentInputPortParam,
        GetCurrentOutputPortParam,
        GetCurrentErrorPortParam,
    } {
        param := getParam()
        if param == nil {
            continue
        }
        // Accessor returns the current binding (after any parameterize).
        // Q-2 below: confirm the exact accessor name during impl.
        port, ok := param.CurrentValue().(values.Port)
        if !ok || port == nil {
            continue
        }
        err := port.Close()
        if err != nil {
            errs = append(errs, err)
        }
    }
    return errors.Join(errs...)
}
```

**Open question (Q-2)**: `machine.Parameter` accessor for current
value without a `MachineContext`. The default value is set at
construction (`state.go:89-92`), but parameters can be `parameterize`d
to other values during execution. At engine shutdown, we want the
*currently-active* value, not the default. Verify what the parameter
type exposes during impl.

**Open question (Q-3)**: idempotence. If a Scheme program already
called `(close-port (current-error-port))`, the second `Close()` from
the io extension must be a no-op. The `portBase` already tracks
`closed` state; `flushThenClose` likely short-circuits on
already-closed. Verify.

### D3 — Change B: `main()` always returns; one `os.Exit` site

Refactor `cmd/wile/main.go`:

```go
func main() {
    os.Exit(run())
}

func run() (exitCode int) {
    // ... all current main() body ...
    // every `os.Exit(N)` becomes `return N`
    // every `Failf(err, "msg")` becomes `return fail(err, "msg")`
}

func fail(err error, format string, args ...any) int {
    fmt.Fprintf(os.Stderr, "Error: ")
    fmt.Fprintf(os.Stderr, format, args...)
    if err != nil {
        fmt.Fprintf(os.Stderr, ": %v", err)
    }
    fmt.Fprintln(os.Stderr)
    return EX_IOERR  // or pass exit code as parameter
}
```

The 9 `os.Exit` call sites and the `Failf` helper collapse to
`return` statements inside `run()`. The deferred `eng.Close()` at
line 260 becomes reachable on every path (it was previously bypassed
by the direct `os.Exit` calls inside the function body — they hit
before any defer scope completed).

**Open question (Q-4)**: SIGQUIT/SIGINT handlers
(`cmd/wile/main.go:140-153`). These currently dump goroutine stacks
and continue. Should they trigger graceful shutdown instead? Out of
scope for this plan; flag for follow-up. The current SIGQUIT-as-debug
behavior is intentional.

**Open question (Q-5)**: panics inside `run()`. A panic propagates
through deferred functions, so `defer eng.Close()` will run. The
panic then terminates the process — `run()` never returns, `main()`
never calls `os.Exit`, the Go runtime sets exit code 2 and prints the
panic trace. Stderr from the panic itself goes through the Go
runtime's own stderr writes (which bypass Wile's bufio). So panics
*do* get their cleanup, just via a different path. No special
handling needed.

### D4 — Direct `fmt.Fprintf(os.Stderr, ...)` writes are unchanged

The 9 direct `os.Stderr` writes in `cmd/wile/main.go` (e.g., line
145–147, 263, 493) bypass Wile's `current-error-port` buffer
entirely. They write directly to `os.Stderr`, which is unbuffered at
the Go `os.File` level. These writes are already reliable.

This means CLI-level error messages already arrive correctly today.
The bug only affects **Scheme-level writes** through
`(current-error-port)`.

## What does NOT change

- The `bufio.Writer` 4096-byte buffer size (Go default). No
  per-message flushing is added; only flush-on-engine-shutdown.
- `os.Stderr`-as-target. Stderr stays unbuffered at the OS level;
  only Wile's wrapper is buffered.
- The behavior of `current-output-port` and `current-input-port`.
  Stdout faces the same buffering issue, but its data is typically
  visible because (a) most programs flush stdout via newline-driven
  conventions less reliable here, but (b) the engine cleanup added
  by Change A flushes all three ports symmetrically. Fix is incidental.
- `runtime.SetFinalizer` / `runtime.AddCleanup` — Go explicitly does
  not guarantee these run before exit. Not used.

## R7RS conformance impact

None. R7RS §6.13.3 says `display`, `write`, etc. are not required to
flush — `flush-output-port` is the explicit flush mechanism. Adding
*more* flush triggers (port close on engine shutdown) does not
violate the spec; the spec sets a floor, not a ceiling.

## Risks

### R-1 — `Engine.Close()` error propagation

If `port.Close()` returns an error (e.g., write failure on the
underlying file), `Engine.Close()` should aggregate it. Already
handled via `errors.Join` in the existing implementation. New `io`
`Close()` follows the same pattern.

### R-2 — Repeated `eng.Close()` calls

`engine.go:1055-1057` already short-circuits on `p.closed`. Safe.

### R-3 — Embedders that close ports themselves

An embedder that explicitly calls `(close-port (current-error-port))`
before `eng.Close()` results in a double-close attempt. `portBase`
tracks closed state; second close is a no-op or error. Verify Q-3.

### R-4 — Exit-code semantics drift

The refactor (Change B) must preserve every existing exit code. Audit
each `os.Exit(N)` → `return N` change. Add a test that exercises
exit-code-producing CLI paths (already exists in
`cmd/wile/main_test.go` — extend if needed).

### R-5 — Test suite assumptions

Any test that calls `main()` directly and expects it to call
`os.Exit` will break. Search for such patterns. Most tests likely call
the `run()`-equivalent helper or use subprocess execution.

## Verification

Two new integration tests:

```scheme
;; integration/stderr_flush_on_exit.scm
;; Run as a subprocess; assert stderr contains the message.
(display "boom" (current-error-port))
(newline (current-error-port))
```

```scheme
;; integration/stderr_flush_on_exit_with_error.scm
(display "before-error" (current-error-port))
(newline (current-error-port))
(error "deliberate")
```

The Go test harness execs the wile binary, captures stderr, asserts
both `"boom"` and `"before-error"` appear in the captured output.

Existing tests must continue to pass — `make ci`.

## Sequencing

| Step | Action | Owner |
|------|--------|-------|
| 1 | Branch `feat/stderr-flush-on-exit` from master | impl |
| 2 | Commit 1: io extension Closeable + tests for Close() flushing | impl |
| 3 | Commit 2: `main()` → `run() int` refactor + integration tests | impl |
| 4 | `make ci` green; verify SIGQUIT still works; verify --mcp still works | impl |
| 5 | Open PR; dual review (Copilot + crosscheck) | review |
| 6 | Merge | merge |

Independent of port unification. May land before or after.

## Estimated size

- LOC delta: +60 to +100 (`io` extension `Close()` ~30 lines; main
  refactor ~50 lines moved; ~20 lines of tests).
- Diff: ~200 lines changed.
- Time: 2–4 hours.
- Reviewer load: low (small, localized).

## Cross-references

- Port unification design: `plans/2026-05-14-port-unification-design.md`
  (this plan was raised during that review).
- Engine.Close pattern: `engine.go:1054-1068`.
- Closeable interface: `registry/extension.go:39-45`.
- Port construction: `values/character_output_port.go:36-42`.
- Direct stderr writes (unchanged): `cmd/wile/main.go:145-153,
  203-208, 263, 493`.
