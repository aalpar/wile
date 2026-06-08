# Channel Select — Scheme Surface Design

**Date**: 2026-06-08.
**Status**: Design draft. Implementation plan (`-impl.md`) to follow after approval.
**Library**: extends the `gointerop` extension (where channels already live) —
no new library. The macro ships as an embedded `.scm`, exactly as
`extensions/files/with_file_macros.scm` does.
**Consumer**: Scheme programs doing channel-based concurrency that must wait on
*several* channels at once (worker pools, fan-in, timeout/cancel multiplexing).
Today there is no way to block on "whichever of these N channels is ready first"
from Scheme.

## Motivation — the engine exists; only the Scheme wire is missing

The premise that a Scheme `select` "isn't possible because Go's `select` is a
compile-time statement" is false. Go has **two** selection mechanisms:

- the `select` *statement* — compile-time, fixed case set. Cannot be reflected
  into a Scheme primitive (its cases are not first-class).
- `reflect.Select([]reflect.SelectCase)` — runtime, a **slice** of cases built
  dynamically. This is purpose-built for exactly the dynamic case a Scheme
  `select` needs, and it is the only form that is worth exposing: a select over
  a *fixed* set of channels is pointless (you would hand-write the cases). The
  value is selecting over a **runtime-computed** collection of channels.

That engine is already built and tested: `values.ChannelSelect([]SelectCase)
(idx int, val Value, ok bool)` in `values/channel.go:226`, present since ~PR
#224, refined to a `SelectCaseKind` enum in PR #415. It:

- tries every case non-blocking first (`TrySend`/`TryReceive`) — `channel.go:233`;
- honors an explicit `SelectDefault` case if present — `channel.go:251`;
- otherwise **blocks via `reflect.Select`** for true multiplexing —
  `channel.go:303` — mapping the chosen reflect index back to the original case
  index through `originalIndices`;
- is TOCTOU-safe: a send racing a concurrent close panics inside
  `reflect.Select` with "send on closed channel"; `ChannelSelect` recovers and
  reports that case with `ok=false` instead of crashing — `channel.go:285`.

`values/channel_test.go` covers blocking, default, send, receive,
send-to-closed, receive-from-closed.

**What is missing is one wire.** The Scheme surface (registered in
`extensions/gointerop/prim_gointerop.go`) exposes `make-channel`, `channel?`,
`channel-send!`, `channel-receive`, `channel-try-send!`, `channel-try-receive`,
`channel-close!`, `channel-closed?`, `channel-length`, `channel-capacity` — but
**no `channel-select`**. The finished Go engine has never been reachable from
Scheme. There is no prior plan or TODO entry for it.

This is therefore a **wiring + ergonomics** task, not new concurrency
machinery. The plan is sized accordingly — small.

## Scope (v1)

A procedure primitive plus a `syntax-rules` macro layered on top:

1. **Descriptor constructors** — `select-recv`, `select-send`, `select-default`,
   returning an opaque, validated `<select-case>` value.
2. **`channel-select`** — the procedure: takes a *list* of descriptors, returns
   `(values idx val ok)`.
3. **`select`** — the macro: Go-shaped sugar that expands into a
   `channel-select` call plus `case` dispatch, so case bodies run lazily and a
   received value (plus its closed-channel flag) binds.

Out of scope for v1: a `with-timeout`-style helper (compose
`select-default` + a sleeper channel instead), context/cancellation threading
into the blocking call (no existing channel primitive threads ctx — see
"Blocking", below), and a `select`-over-ports event loop (superseded by channels
per `docs/reference/r7rs-differences.md:59`).

## Design rationale — why these shapes

**A procedure is the primitive; the macro is sugar (decided).** The dependency
points one way: `select` (macro) expands into `channel-select` (procedure),
which calls `values.ChannelSelect` (engine). One blocking codepath. The macro
inherits TOCTOU recovery and default-case semantics for free; it cannot drift
from the engine because it has no select logic of its own. Building the macro
first would invert the dependency and hide the dynamism behind fixed syntax.

**The procedure takes one list argument, not variadic cases.** The entire reason
to expose `reflect.Select` rather than hand-written cases is dynamism. A list
argument is the composability seam: `(channel-select (map select-recv chans))`
selects over a runtime vector of N worker channels. A variadic surface or a
fixed-arity macro-only surface would throw away exactly the capability that
makes this worth shipping. (See `feedback-blas-style-dispatch`,
`feedback-mathematical-tractability`: BFS-from-generators / runtime-shaped
inputs are preferred over enumerate-the-fixed-set.)

**Descriptors are opaque validated records, not tagged lists (decided).**
`(select-recv 5)` errors at construction ("not a channel") rather than failing
opaquely inside the select call. Matches the project's type-design and
robustness priorities (`feedback-algebra-design-goals`: robust > brevity).

**The procedure returns an index, not the matched descriptor.** Descriptors are
opaque — you cannot `case` on one. The 0-based index into the input list is the
minimal dispatchable identity, and it is already stable: `ChannelSelect` maps
`reflect.Select`'s chosen position back to the original case index via
`originalIndices` (`channel.go:304`).

**The `ok` third value is propagated, not swallowed.** Go's bare `select` would
panic on send-to-closed; `ChannelSelect` converts that to `ok=false`. Surfacing
`ok` lets a fan-in loop detect a closed worker channel and drop it. This mirrors
`channel-try-receive`, which already returns three values via `mc.SetValues`
(`prim_gointerop.go:134`).

## API

### Layer 1 — descriptor constructors (gointerop primitives)

| Primitive | Args | Builds | Validation |
|-----------|------|--------|------------|
| `(select-recv ch)` | 1 | `SelectReceive` case | `ch` must be a channel |
| `(select-send ch v)` | 2 | `SelectSend` case, value `v` | `ch` must be a channel |
| `(select-default)` | 0 | `SelectDefault` case | — |

Each returns an opaque `<select-case>` value:

```scheme
(select-send ch 42)   ; => #<select-case send>
(select-recv 5)       ; => error: channel-select: argument is not a channel
```

### Layer 2 — `channel-select` (gointerop primitive)

```scheme
(channel-select cases)  ; cases : proper list of <select-case>
                        ; => (values idx val ok)
```

- `idx` — exact integer, 0-based position in `cases` of the case that fired.
- `val` — received value for a recv case; `Void` for a send or default case.
- `ok` — boolean. `#f` when a recv fired on a closed channel, or a send raced a
  concurrent close. `#t` otherwise (including the default case).

**Edge cases.** Empty `cases` → wrapped sentinel error (a select over zero cases
is meaningless; `reflect.Select` itself panics on an empty slice — we reject
before reaching it, never surfacing a bare `idx = -1`). A list whose only entry
is `(select-default)` is valid and always selects default. A non-list argument,
or a list element that is not a `<select-case>`, → wrapped type error.

### Layer 3 — `select` macro (syntax-rules, embedded in the gointerop extension)

```scheme
(select
  (((v ok) (recv ch))   (if ok (process v) (drop-channel! ch)))  ; recv: binds value + ok
  ((send out 42)        'sent)                                    ; send
  (else                 'idle))                                   ; default → non-blocking
```

The receive clause binds **both** the received value and the closed-channel `ok`
flag (decided — most faithful to the 3-value procedure; lets loops detect and
drop a closed worker). Send and `else` clauses bind nothing. `else` is optional;
omitting it makes the `select` blocking.

**Expansion sketch** (hygienic temporaries shown as `i`/`val`/`ok*`):

```scheme
(let-values (((i val ok*)
              (channel-select
                (list (select-recv ch)        ; case 0
                      (select-send out 42)    ; case 1
                      (select-default)))))    ; case 2  (only if else present)
  (case i
    ((0) (let ((v val) (ok ok*)) (if ok (process v) (drop-channel! ch))))
    ((1) 'sent)
    ((2) 'idle)))
```

The macro binds the recv value/flag into fresh hygienic names and re-`let`s them
to the user's identifiers, so a body that ignores `ok` does not trip an
unused-binding warning at the procedure layer.

## Implementation outline

1. **`*values.SelectCaseValue`** — a Scheme value type wrapping the existing
   `SelectCase` struct, with `SchemeString` / `IsVoid` / `EqualTo`. `EqualTo` is
   identity (descriptors are single-use opaque tokens). Registered per the
   "ADDING A NEW value type" guide in `values/values.go` (7 items + numeric
   pointer). Lives in `values/channel.go` beside `SelectCase`.
2. **Four gointerop primitives** in `extensions/gointerop/prim_gointerop.go`,
   thin wrappers in the established mold:
   - `select-recv` / `select-send` — `helpers.RequireArg[*values.Channel]` for
     the channel, construct a `SelectCaseValue`.
   - `select-default` — nullary, construct a default `SelectCaseValue`.
   - `channel-select` — walk the list argument, type-assert each element to
     `*SelectCaseValue`, collect `[]values.SelectCase`, error on empty, call
     `values.ChannelSelect`, return `mc.SetValues(idx, val, ok)`. `val` is
     `Void` when the engine returns `nil`.
   - Register in `extensions/gointerop/register.go` alongside the channel group;
     document in `extensions/gointerop/CLAUDE.local.md` and `doc.go`.
3. **`select` macro** as a new embedded Scheme file
   `extensions/gointerop/select_macro.scm`, wired via `//go:embed` +
   `r.AddMacroSource(...)` in `extensions/gointerop/register.go` — the exact
   mechanism `extensions/files/register.go:110` uses for
   `with_file_macros.scm`. Pure `syntax-rules`; no Go logic. Three clause
   shapes: `((vars (recv ch)) body)`, `((send ch v) body)`, `(else body)`.
4. **Tests.** Go-level primitive tests in
   `extensions/gointerop/prim_gointerop_test.go` (or a sibling) mirroring
   `values/channel_test.go`: recv, send, default/non-blocking, closed-channel
   `ok=#f`, dynamic `(channel-select (map select-recv chans))` over N channels,
   empty-list error, bad-descriptor type error. Scheme-level macro tests for
   binding, dispatch, optional `else`. Integration test in `integration/` for a
   small fan-in loop that drops closed channels via `ok`.

## Blocking & concurrency

`channel-select` blocks the **goroutine**, not an OS thread, exactly as
`channel-receive` does (`prim_gointerop.go:86` calls `ch.Receive()` with no ctx
threading). No existing channel primitive threads a context for cancellation, so
v1 does not either — it follows the established pattern. A program that needs a
cancellable wait composes `select-default` (poll) or a dedicated cancel channel
as one of the cases. This is consistent with the memory note that Wile threads
buy concurrency for blocking I/O, not CPU parallelism
(`vm-no-cpu-parallelism.md`): `select` is a *blocking-coordination* tool, which
is precisely where threads pay off.

## Verification gates

`make lint && make covercheck && make ci` all green before PR. The build is not
clean until both lint and covercheck pass.

## Open questions

None blocking. All three design forks resolved 2026-06-08:
1. surface shape → procedure **and** macro sugar;
2. descriptor representation → constructor procedures + opaque record;
3. macro recv binding → `((v ok) (recv ch))`, binds value and closed-channel flag.
