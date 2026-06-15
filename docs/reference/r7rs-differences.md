# R7RS Semantic Differences

This document catalogs differences between the current implementation and the R7RS-small specification. These are semantic differences where the implementation produces results but may not match R7RS behavior for certain inputs.

**Reference:** [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)

**Last Updated:** 2026-06-12

---

## Summary

Five known differences exist:
1. Non-blocking I/O detection (`char-ready?`, `u8-ready?`) always returns `#t`. Conservative safe behavior with minimal practical impact.
2. `parameterize` uses continuation marks instead of `dynamic-wind`. This fixes composable continuation bugs at the cost of a minor semantic difference when mutating parameters via `(p val)` inside `parameterize`.
3. `set-current-directory!` changes the process-global working directory via `os.Chdir`, which is inherently shared across all Wile engines and goroutines in the same OS process.
4. Pair and vector literals are **immutable** — mutating one **raises an error** (R7RS permits but does not require this detection), matching immutable string literals.
5. **Default** (opt out with `WithMutableTopLevel`): a defined-once, never-`set!`-in-unit top-level `define` in the user program is immutable, so a later `set!` **raises an error**, and code already compiled against a sealed base binding does not observe a later shadowing re-`define` (Chez two-environment model). User-loaded libraries stay mutable. Use `WithMutableTopLevel()` for strict R7RS top-level mutability.

---

## Non-Blocking I/O Detection

**Affected Primitives:** `char-ready?`, `u8-ready?`

**R7RS §6.13.2 Requirement:**
> Returns `#t` if a character (or byte) is ready on the input port and returns `#f` otherwise. If `char-ready?` returns `#t` then the next `read-char` (or `read-u8`) operation on the given port is guaranteed not to hang.

**Wile Behavior:** Always returns `#t`.

**Rationale:**

Go's `io.Reader` interface does not expose readiness status or non-blocking I/O semantics. Implementing true non-blocking detection would require:
1. OS-specific syscalls (`select`/`poll` on Unix, overlapped I/O on Windows)
2. Platform-specific build tags and dependencies (`golang.org/x/sys/unix`, `golang.org/x/sys/windows`)
3. Handling buffered readers (`bufio.Reader`) where buffered data makes reads non-blocking even when the underlying descriptor would block
4. Significant complexity in the I/O layer with cross-platform maintenance burden

The conservative behavior (always returning `#t`) is **safe**: it may cause blocking where R7RS code expected non-blocking, but never claims data is available when it isn't (which would violate R7RS guarantees).

**Workaround:**

Use Go channels or goroutines for non-blocking I/O patterns:

```scheme
;; Instead of polling with char-ready?:
(if (char-ready? port)
    (read-char port)
    'not-ready)

;; Use a thread to read asynchronously:
(let ((ch (make-channel)))
  (thread-start!
    (make-thread
      (lambda ()
        (channel-send! ch (read-char port)))))
  (channel-receive ch))
```

**Impact:** **LOW** — `char-ready?` and `u8-ready?` are rarely used in modern Scheme code. These predicates were designed for select-style event loops, a pattern largely superseded by async/await and channel-based concurrency. Most I/O in Wile is either:
- File-based (always ready, blocking is acceptable)
- Network streams where blocking semantics are expected
- Interactive REPL input where immediate blocking is desired

**Estimated implementation effort:** 4-8 hours including cross-platform support and testing.

**ROI analysis:** Documentation (15 minutes) provides clear expectations at far better ROI than implementation (4-8 hours) for an exotic edge case.

---

## Parameterize Implementation (Marks-Based)

**Affected Form:** `parameterize`

**R7RS §4.2.6:** The R7RS reference implementation uses `dynamic-wind` to save/restore parameter values. Wile uses `with-continuation-mark` instead, storing parameter bindings as continuation marks keyed by the parameter object.

**Why:** The `dynamic-wind` approach has bugs when composable continuations (`call-with-composable-continuation`) cross `parameterize` boundaries. The after-thunk captures the "old" value at definition time. When a composable continuation is invoked from a different `parameterize` context, the stale old value clobbers the outer binding. Marks-based `parameterize` eliminates this class of bugs because bindings ride on the continuation frames structurally.

**Semantic difference:** `(p val)` (calling a parameter with 1 argument) inside `parameterize` sets the parameter's base value. With `dynamic-wind`-based `parameterize`, the mutation is visible within the extent and undone on exit. With marks-based `parameterize`, the mark shadows the base value, so the mutation is invisible while the `parameterize` is active but persists after it exits.

This difference is observable only when code mutates a parameter via `(p val)` inside a `parameterize` body — a rare pattern. The standard pattern of reading `(p)` inside `parameterize` is unaffected.

**Impact:** **LOW** — standard R7RS programs use `parameterize` for scoped binding, not direct mutation. The marks-based approach matches Racket's semantics and is correct for composable continuations.

---

## Immutable Pair and Vector Literals

**Affected Primitives:** `set-car!`, `set-cdr!`, `list-set!`, `vector-set!`, `vector-fill!`

**R7RS §6.4 Requirement:**
> It is an error to attempt to store in a literal.

R7RS §1.3.2 clarifies "it is an error" as a case implementations are not required to detect, but "encouraged to detect ... so as to help the programmer detect them."

**Wile Behavior:** Mutating a pair or vector literal **raises an error** (`ErrImmutablePair` / `ErrImmutableVector`), matching the long-standing immutable-string behavior. Wile detects the case R7RS encourages detecting.

```scheme
(set-car! '(a b c) 999)        ; raises ErrImmutablePair
(vector-set! '#(1 2 3) 0 'x)   ; raises ErrImmutableVector
(string-set! "abc" 0 #\X)      ; raises ErrImmutableString
```

Because Wile shares structure for same-shape literals (`(eq? '(a b c) '(a b c)) → #t`, R7RS-permissible), detection also prevents non-local corruption: mutating one literal through a shared instance — which would have been visible through every syntactically identical quotation — is now rejected rather than silently propagated.

**Implementation:**

`*values.Pair` (`[2]Value`) and `*values.Vector` (`[]Value`) are not structs, so an inline `immutable` flag like `*values.String`'s is not available without growing the 32-byte cons cell ~25% (the dominant heap object). Instead, an engine-scoped side-set (`environment.ImmutableLiterals`, a `sync.Map` keyed by pointer identity) records literal pairs/vectors. The set is populated once at compile time when the quote hook interns a literal (`machine/compilation/compile_validated.go`, `markLiteralImmutable`) and read on the cold mutation path by the five mutator primitives. Membership is by pointer identity, so structure-shared siblings are covered by a single mark, and only literals — never `list`/`cons`/`make-vector` allocations — are members. Internal Go `SetCar`/`Set` calls bypass the guard (it lives in the primitive, not in `values`), so scratch reuse of literal structure is unaffected.

**Workaround (for programs that must mutate):**

Construct with `list`, `cons`, `make-vector`, or `vector-copy` to obtain an allocation not shared with any literal:

```scheme
;; Error — literal is immutable:
(let ((xs '(1 2 3))) (set-car! xs 99) xs)

;; Right — guaranteed mutable, no aliasing with literals:
(let ((xs (list 1 2 3))) (set-car! xs 99) xs)
(let ((v (vector-copy '#(1 2 3)))) (vector-set! v 0 99) v)
```

**Impact:** **LOW** — programs that mutate literals are already non-portable across R7RS implementations; Wile now rejects them rather than silently corrupting shared structure.

---

## Immutable Top-Level Definitions (default; opt out with `WithMutableTopLevel`)

**Affected:** `set!` and re-`define` of top-level variables. **Enabled by default.** Opt out per-engine with `WithMutableTopLevel()` to restore strict R7RS top-level mutability. `WithImmutableTopLevel()` remains as an explicit (now-redundant) selector for the default.

**R7RS §4.1.6 / §5.3.1:** top-level variables are mutable (`set!`) and redefinable.

**Wile Behavior (default):** A top-level `define` in the user program that is *defined exactly once* and *never `set!` within its compilation unit* is marked rebind-stable. A subsequent `set!` of such a binding **raises `ErrImmutableBinding`** at compile time:

```scheme
;; Default (immutable top level):
(define f 5)
(set! f 6)        ; raises ErrImmutableBinding — f is stable

;; Still mutable: a define that IS set! within its own unit is not stable.
(begin (define g 5) (set! g 6) g)   ; => 6, permitted

;; Opt out for strict R7RS mutability:
;;   wile.NewEngine(ctx, wile.WithMutableTopLevel())
```

**Why this is now the default (the layered-environment carve).** Each engine splits its
runtime bindings into an immutable **sealed base** (Go primitives + sealed stdlib
procedures) parented by a **mutable runtime** (user defines). Resolution walks
`mutable-runtime → sealed-base`. This dissolves the two blockers that previously kept
immutability opt-in:

- A user or stdlib **re-`define` of a sealed name is a shadow, not a rebind.** `(define
  car …)` or `(import (scheme cxr))` creates a *new* binding in the mutable child (the
  name is absent from that frame's own map), so the redefinition guard never fires. The
  sealed `car` is untouched.
- **Enforcement is scoped to the compiled program (the engine's root namespace).** Defines
  landing in the engine's own user runtime are frozen, and the sealed base's *defined-once*
  bootstrap procedures (`map`, `assoc`, the `cxr` accessors, …) **are** stamped `Stable` and
  frozen too: a top-level `(set! map …)` is rejected. (Go primitives are stamped `Stable`
  only when they are *capture-safe*, so `(set! car …)` is rejected but `(set! vector-ref …)`
  — not capture-safe — stays permitted.) Two contexts stay **mutable**: **user-loaded
  libraries** — a library body's cross-form `(define *x* …)` / `(set! *x* …)` works — and
  **interactive / first-class eval environments** — the REPL, the `--mcp` session, and the
  namespaces built by `(environment …)` / `scheme-report-environment` are mutable scratch
  spaces (Chez interaction-environment model), where a redefine is a permitted shadow. The
  frame-reclaim GC payoff therefore applies to compiled programs (files, `-e` batches), not
  to interactive redefinition.

**Redefine-visibility deviation (Chez two-environment model).** Because a sealed binding
is resolved and pinned at *compile time*, an already-compiled closure over a sealed name
keeps seeing the **sealed** value after a later shadow:

```scheme
(define (use-car p) (car p))   ; car pinned to the sealed base at compile time
(define car (lambda (x) 99))   ; a shadow in the mutable runtime
(use-car '(7 8))               ; => 7  (the REAL car — the shadow is not observed)
(car '(7 8))                   ; => 99 (this call compiled AFTER the shadow)
```

This mirrors Chez's sealed-base + interaction-environment split: code already compiled
against the base does not observe later interactive redefines. It is an intentional,
documented deviation from R7RS's single mutable top level.

**define/set! asymmetry on sealed names.** `(define caar …)` *introduces* a child-frame
shadow (closures keep the sealed binding); `(set! caar …)` *mutates the existing* binding
in place (closures observe it) — and in the compiled program, `set!` of a `Stable` sealed
name (a capture-safe primitive such as `car`, or any defined-once bootstrap procedure such
as `caar`/`map`) is rejected with `ErrImmutableBinding`. This is ordinary Scheme semantics
(`define` introduces, `set!` mutates) applied to the sealed/mutable split.

**Rationale:** This is the language-level enforcement half of the frame-reclamation optimizer (`plans/2026-06-11-escape-gated-frame-allocation.local.md`). The optimizer may release a function's stack frame at a tail call only if every callee it relies on provably never captures a continuation; proving that for a *top-level* callee requires knowing the binding will not be rebound to a capturing procedure. Rather than *infer* unit-closure (undecidable for an incremental/embedded system), the engine *enforces* it — the "compile for speed" contract used by sealed-module Schemes (Racket modules, Chez `optimize-level 3`).

**Implementation:** Pure compile-time, scoped to the engine's **root** namespace. The redefinition guard fires only for a define landing in the root's own user runtime or sealed base (`compile_validated.go`); child namespaces report `ImmutableTopLevel() == false`, so REPL / `(environment …)` / `scheme-report-environment` redefines are permitted. The compiler stamps `BindingMeta.Stable`; the `set!` guard keys on `IsStable()` **directly** (not on the namespace flag), so a `Stable` anchor copied into a mutable child stays `set!`-protected — preserving frame-reclaim soundness while still allowing define-shadow. Imported-binding `set!` rejection (always on) is unchanged.

**Impact:** Programs that rebind their own never-mutated top-level definitions via `set!` are rejected by default; the common case (define-once, call-many) is unaffected and gains the optimization. Use `WithMutableTopLevel()` for strict R7RS top-level mutability.

---

## Extensions Beyond R7RS

These are Wile-specific features that extend R7RS. They do not conflict with R7RS behavior — standard Scheme programs behave identically. These extensions use reader prefixes in the `#` dispatch space that R7RS leaves implementation-defined.

### Arbitrary-Precision Number Literals

Wile provides reader syntax for explicitly constructing arbitrary-precision numbers. These are not part of any Scheme standard (R5RS, R6RS, R7RS, or SRFIs).

| Prefix | Type | Exactness | Backed by | Examples |
|--------|------|-----------|-----------|----------|
| `#z` | BigInteger | exact | `math/big.Int` | `#z12345678901234567890`, `#z-42`, `#z+7` |
| `#m` | BigFloat | inexact | `math/big.Float` (256-bit) | `#m3.14159265358979323846`, `#m1.5e-10`, `#m.5` |

Both prefixes are case-insensitive (`#Z`, `#M` also work), following R7RS §7.1.1 conventions.

**BigInteger (`#z`)** supports radix prefixes: `#z#b101` (binary), `#z#o77` (octal), `#z#x1F` (hex).

**BigFloat (`#m`)** supports optional sign, decimal point, and exponent markers (`e`, `s`, `f`, `d`, `l`).

**Note:** R7RS requires implementations to support arbitrarily large exact integers (§6.2.3). Wile satisfies this via automatic overflow promotion from `Integer` (int64) to `BigInteger` — the `#z` prefix is a convenience for explicit construction, not a conformance requirement. Standard R7RS programs never need `#z` or `#m`.

### Process-Global Working Directory

**Primitive:** `set-current-directory!`

**Behavior:** Calls `os.Chdir`, which changes the working directory for the entire OS process. Multiple Wile engines in the same Go process share one working directory. Concurrent calls from different goroutines race on the same OS state. This is inherent to POSIX — there is no per-thread working directory.

**Mitigation:** The primitive is gated by `security.ResourceProcess` / `security.ActionWrite` / target `"cwd"`, so embedders can deny it via their authorizer. When denied, all file operations should use absolute paths.

R7RS does not specify directory operations. This follows SRFI-170 conventions.

### Guard Body Multiple Values

R7RS §7.3's reference implementation of `guard` uses `(let ((result (begin e1 e2 ...))) ...)`, which binds a single value. If the body produces multiple values via `(values v1 v2 ...)`, the `let` binding triggers an arity mismatch.

Wile's `guard` uses `call-with-values` to capture all values from the body, then re-emits them via `(apply values results)`. This means `(guard (e (#f)) (values 1 2))` correctly propagates both values, whereas the R7RS reference implementation would signal an error.

### Loss-Signal-Aware Numeric Conversion Primitives

Wile adds four primitives (in the math extension) that expose the
accuracy of `exact->inexact` conversion at the `float64` /
`complex128` boundary, surfacing Go's `big.Accuracy` three-valued
enum as `'below` / `'exact` / `'above` symbols:

| Primitive | Purpose |
|-----------|---------|
| `inexact-lossless?` | Predicate: `#t` iff conversion is lossless |
| `inexact-accuracy` | Returns accuracy symbol(s) without converting |
| `inexact-with-accuracy` | Returns `(values inexact-n acc-sym)` (real) or `(values inexact-c real-acc imag-acc)` (complex) |
| `complex-inexact-with-accuracy` | Uniform 3-value variant regardless of input domain |

R7RS `(exact->inexact)` (§6.2.6) is **unchanged** — it continues to
saturate silently to `+inf.0` / `-inf.0` on overflow. The new
primitives **expose** the rounding direction rather than gate it.
R7RS-strict programs that import only `(scheme base)` /
`(scheme inexact)` are unaffected; these primitives are reachable
only after loading the math extension (profile `Small` and above).

### FFI Numeric Argument Precision

For embedders using `wile.RegisterFunc` with Go functions taking
`float64` or `complex128` parameters: the default conversion is now
**precision-aware**. Passing a Scheme numeric value that cannot be
exactly represented in the Go fixed-precision type (e.g. `1/3`,
`*BigInteger` exceeding 2^53, `*BigFloat` overflowing magnitude)
returns `werr.ErrLossyConversion` instead of silently truncating.

The `wile.WithLossyConversionsAllowed()` engine option restores the
silent-truncation behavior for embedders that depended on it. This
is purely an embedder-API concern; Scheme programs are unaffected.

See `docs/numeric/tower.md` §"Conversion to Fixed-Precision Go Types"
and `memory/2026-05-14-numeric-loss-signals-design.md` for the
underlying design.


