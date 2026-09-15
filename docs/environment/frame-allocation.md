# Environment-Frame Allocation and Recovery

Every procedure call creates an environment frame. Most are recycled through a
freelist; a specific subset cannot be, and the reason is a load-bearing invariant
rather than an oversight.

This document explains which frames are recovered, which are not, and why the
obvious fix for the second group is unsound. It exists because that fix has been
proposed — and reverted — repeatedly.

> **Scope note (updated after let-slot merging).** This document was written when
> every `let` pushed its own frame. It no longer does. `canMergeLet`
> (`pkg/machine/compilation/merged_slots.go`) takes a merged `let`'s slots out of
> the enclosing frame, and a merged `let` emits neither `OpPushEnv` nor `OpPopEnv`.
> The enclosing frame is the lambda's parameter frame, a `syntax-case` clause's
> pattern-variable frame (`OperationBindPatternVars.MergedSlots`), or an outer
> unmerged `let`'s frame. The pushing form survives only where there is no frame
> to merge into: the outermost `let` of a top-level form (program, library or
> transformer compilers).
>
> So the `let`-frame cost analysed below is now a **narrow residual**, not the
> common case. Measured at `a204912d`, a self-tail loop wrapping one `let` and one
> wrapping two both allocate **0 frames per iteration**. What remains live and
> unchanged is everything about *parameter*-frame recovery and why a runtime
> recycle bit for it is unsound — which is the load-bearing half.

## Two kinds of frame, two very different costs

Two shapes remain, but the second is now rare — see the scope note. A `let`
inside a procedure body has no frame of its own at all.

| | parameter frame | **unmerged** `let` frame |
|---|---|---|
| created by | `Apply` (the closure-call path) | `OpPushEnv`: the outermost `let` of a top-level form only |
| construction | `acquireEnvFrame()` + `InitApplyFrameWithParent` | `NewLocalEnvironment` + `NewEnvironmentFrameWithParent` |
| pool-owned | **yes** | **no** |
| steady-state allocations | **~0** | **3** |

The parameter frame costs nothing in steady state because the apply path solved
the problem twice over: the frame struct and its bindings capacity come from a
freelist, and the *keys* map — the symbol-to-slot table — is not rebuilt at all.
`copyForApplyInto` shares it from the template's compile-time frame, which is a
per-template constant:

```go
dst.keys = p.keys        // SHARED, never rebuilt
dst.keysShared = true
if cap(dst.bindings) >= n { dst.bindings = dst.bindings[:n] } else { … }
```

`OpPushEnv` does neither. It builds a fresh keys map, a fresh bindings slice and a
fresh frame struct on every execution — three allocations, none recycled. That is
still true of `OpPushEnv`; what changed is how rarely a `let` reaches it. A `let`
in a procedure body compiles to `StoreLocal` into the enclosing parameter frame
and emits no bracket at all.

```mermaid
graph TD
    subgraph POOL["envFramePool — freelist, survives GC"]
        F1["free frame"]
        F2["free frame"]
    end

    A["Apply<br/>closure call"] -->|"acquireEnvFrame()"| PF["parameter frame<br/>envPooled = true"]
    POOL -.->|"reuse"| PF
    PF -.->|"released on return"| POOL

    L["OpPushEnv<br/>a let"] -->|"3 fresh allocations"| LF["let frame<br/>envPooled = false"]
    LF -.->|"never released"| GC["garbage collector"]

    style PF fill:#2d5016,color:#fff
    style LF fill:#5c1a1a,color:#fff
    style GC fill:#5c1a1a,color:#fff
```

## The environment chain during a call

Before let-slot merging, a procedure whose body contains a `let` built a two-frame
chain: the pooled parameter frame, with the `let` frame hanging off it as a
lexical child. Compiled today, `count-up` builds one frame and `j` is its slot 2;
the chain below is the shape an unmerged `let` still has.

```scheme
(define (count-up i n)        ; parameter frame — slots i, n — POOLED
  (let ((j (+ i 1)))          ; let frame       — slot  j    — not pooled
    (if (>= j n) j (* j n))))
```

```mermaid
graph RL
    LF["let frame<br/>slots: j<br/>NOT pooled"] -->|parent| PF["parameter frame<br/>slots: i, n<br/>POOLED"]
    PF -->|parent| GLOB["closure's captured env"]

    MCENV(["mc.env"]) -.->|"points here<br/>inside the let body"| LF

    style PF fill:#2d5016,color:#fff
    style LF fill:#5c1a1a,color:#fff
```

`mc.env` names the innermost frame. Everything the VM does to "the current frame"
— including releasing it — acts on whatever `mc.env` currently points at.

## `mc.envPooled`, and what it actually claims

`mc.envPooled` is one `bool` on `*machine.MachineContext` (`vm_state.go`). It is
**not** a property of a frame and **not** per-frame: it describes whichever frame
`mc.env` currently names, and it answers one question — *may this frame be
returned to the pool when it is overwritten?*

Four sites can make the current frame a **parent**. Three clear the flag in the
same step; `OpMakeClosure` clears it only in the retaining case, because flat
closures removed the general one:

| site | why it clears |
|---|---|
| `OpPushEnv` | the new `let` frame points at `mc.env` as its parent |
| `OpMakeClosure` | **only when the closure retains its lexical env.** A non-retaining closure records `env.TopLevel()` instead of `mc.env`, and its free variables travel by value in a vector, so it is not a parent and the flag stays set. The site's own comment calls this "narrowed to the retaining case … Keeping the flag set preserves H; it does not weaken it." |
| `OperationBindPatternVars` | `syntax-case` binds its pattern variables in a child frame (`OperationBindPatternVars.Apply`, `compilation/operation_syntax_case.go`) |
| `ClosureEnv`'s detached-env fallback | a `ForeignClosure` outliving its call is parented on `mc.env` when that frame has no mutable runtime (`MachineContext.ClosureEnv`, `machine_context.go`) |

`OpPushEnv` is now itself narrow (see the scope note), and `OpMakeClosure` clears
only when the closure retains. Each site carries its own clear. All four run under `Run` — being
"expand-time" is a claim about `syntax-case`'s phase, not about which loop
executes it. What is uniform across them has a name.

> **Invariant H.** A frame with `envPooled == true` is never any other frame's
> parent.

H is what makes the release cheap. `RestoreAndRelease` can hand `mc.env` back to
the freelist after a single check, with no walk over the frame's children,
because under H a releasable frame has none.

```mermaid
stateDiagram-v2
    [*] --> Pooled: Apply acquires a frame<br/>envPooled = true
    Pooled --> NotPooled: OpPushEnv (a let)<br/>frame becomes a parent
    Pooled --> NotPooled: OpMakeClosure<br/>closure captures the frame
    NotPooled --> NotPooled: OpPopEnv<br/>stays false — see below
    Pooled --> Released: RestoreAndRelease<br/>returns it to the pool
    NotPooled --> Leaked: RestoreAndRelease<br/>skips it
    Released --> [*]
    Leaked --> [*]
```

## Why the parameter frame is not recovered when a `let` runs

Once an unmerged `let` executes, the enclosing parameter frame is no longer
recoverable on return, and the mechanism differs by where the `let` sits. The
92% / 8% split below is the pre-merging population of `let` sites.

```mermaid
graph TD
    START["procedure body contains a let"] --> Q{"is the let in<br/>tail position?"}

    Q -->|"yes — 92% of sites"| T["no OpPopEnv is emitted"]
    T --> T2["mc.env is still the LET frame at return"]
    T2 --> T3["RestoreAndRelease releases only mc.env,<br/>which is not pooled"]
    T3 --> LEAK["parameter frame is never released"]

    Q -->|"no — 8% of sites"| N["OpPopEnv runs"]
    N --> N2["mc.env := parameter frame<br/>envPooled := false, unconditionally"]
    N2 --> N3["the flag now understates:<br/>the frame IS pooled"]
    N3 --> LEAK

    style LEAK fill:#5c1a1a,color:#fff
```

The two shapes, and what each compiled to before let-slot merging. Compiled today,
neither procedure emits `PushEnv` or `PopEnv`: `j` is a parameter-frame slot, and
both release that frame with `OpReleaseEnvFrame` before the tail call.

```scheme
;; TAIL — the let is the last thing the body evaluates.
;; Emitted (pre-merging): PushEnv=1  PopEnv=0
(define (tail-let i n)
  (if (>= i n)
      i
      (let ((j (+ i 1)))
        (helper j n))))         ; mc.env is still the LET frame at return

;; NON-TAIL — the let is an argument, so its value must come back.
;; Emitted (pre-merging): PushEnv=1  PopEnv=1
(define (arg-let i n)
  (+ (let ((j (+ i 1))) j)
     n))
```

In tail position the flag is *correct* — `mc.env` really is a non-pooled `let`
frame — and the limitation is **reach**: `RestoreAndRelease` releases exactly one
frame, and the pooled one is that frame's parent.

In non-tail position the flag is *conservative*: `OpPopEnv` restores `mc.env` to
the parameter frame but refuses to re-arm the release.

## Why `OpPopEnv` cannot simply restore the flag

The tempting fix is to have `OpPushEnv` save the old flag and `OpPopEnv` put it
back. It is unsound, and the reason is `OpMakeClosure`.

```mermaid
sequenceDiagram
    participant B as body
    participant MC as mc.envPooled
    participant PF as parameter frame

    Note over PF: acquired from the pool
    B->>MC: OpPushEnv — false (H: PF is now a parent)
    B->>MC: OpMakeClosure — false (closure captures the chain)
    Note over B: the closure escapes,<br/>and reaches PF through the let frame
    B->>MC: OpPopEnv — restores the SAVED true ❌
    B->>PF: RestoreAndRelease recycles PF
    Note over PF: use-after-release —<br/>the escaped closure still reads it
```

A program that walked that sequence before let-slot merging (today it emits only
`MakeClosure` of the three, and no `OpReleaseEnvFrame`):

```scheme
;; Emitted (pre-merging): PushEnv=1  MakeClosure=1  PopEnv=1 — all three of the diagram's ops.
(define (leaky i n)
  (cons (let ((j (+ i 1)))
          (lambda () (+ i j)))  ; escapes, and reads i from the PARAMETER frame
        n))                     ; the let is an argument to cons, so PopEnv runs
```

A closure created inside the `let` body parents to the `let` frame and therefore
reaches the parameter frame transitively. `OpMakeClosure` protects it by clearing
the flag — but a saved-and-restored copy would overwrite that protection with a
value captured *before* the capture happened. `OpPopEnv` refuses unconditionally
precisely because it cannot know whether the frame it is popping escaped.

This is not a hypothetical. A runtime scheme for exactly this class — a per-frame
`captured` bit set at capture chokepoints — was implemented and reverted **three
times**, most recently 2026-06-10. It fixed the leak and crashed two continuation
suites with use-after-release. The transferable finding:

> Childlessness in the lexical tree is not recycle-safety in the
> continuation-reachability graph. No single chokepoint is crossed by every
> capture, so a per-frame bit set at a fixed set of sites cannot be complete.

## What it costs, measured

**Read the counters below as PARAMETER frames only.** An unmerged `let` frame
never enters the pool in either direction — `OpPushEnv` allocates one directly and
nothing ever calls `releaseEnvFrame` on it — so those frames are invisible to
these numbers and their recovery rate is **0%, in every row**. What the hit rate
measures is how often `Apply`'s acquire found a recycled *parameter* frame waiting
instead of minting a new one.

That was the shape of the cost when every `let` pushed: three frames' worth of
objects never recycled, **and** the enclosing procedure's parameter frame knocked
out of recovery — a different frame, and the one the pool exists for. Since
let-slot merging both halves are gone for every `let` in a procedure body: there
is no `let` frame to allocate, and none left as `mc.env` to strand the parameter
frame. They remain for an unmerged `let`.

`VMCounters` over the whole program (`ClosuresApplied` acquired,
`EnvFramePoolReleases` released), `KitchenSink` profile and otherwise default
engine options, re-measured after let-slot merging:

| benchmark | parameter frames acquired | released back | recovered |
|---|---|---|---|
| `fib` (no `let`) | 2,692,550 | 2,692,550 | **100.0%** |
| `nqueens` | 8,503,623 | 7,549,863 | 88.8% |
| `sieve` | 1,040,948 | 694,592 | 66.7% |
| `peval` | 900,031 | 400,027 | 44.4% |

Acquires are unchanged from the pre-merging measurement. Only `nqueens` moved
materially (5,623,346 released, 66.1%, before merging); `sieve` and `peval` recover what they
did, so what they still lose is not procedure-body `let` frames. The original
table also had a misses column (the freelist's own factory counter, not
`VMCounters`), which showed `misses == in-flight-at-exit`; it was not re-read.

Isolating the cause, on three `fib` variants that differ only in where a `let`
sits:

```scheme
;; 1 — no let
(define (fib n)
  (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))

;; 2 — a let in non-tail position (an argument to +)
(define (fib n)
  (if (< n 2) n (+ (let ((a (- n 1))) (fib a)) (fib (- n 2)))))

;; 3 — a let wrapping ONLY the base branch, off the recursive path entirely
(define (fib n)
  (if (< n 2) (let ((b n)) b) (+ (fib (- n 1)) (fib (- n 2)))))
```

Driven by `(fib 22)`, before let-slot merging:

| body | parameter frames acquired | released back | recovered |
|---|---|---|---|
| no `let` | 57,313 | 57,313 | **100.0%** |
| `let` in non-tail position | 85,969 | 57,313 | 66.7% |
| `let` wrapping **only the base branch** | 57,313 | **28,656** | **50.0%** |

The third row was the sharpest: that `let` is off the recursive path and changed
no acquire count at all, yet half the *parameter* frames stopped coming back. The
28,657 activations that reach the base branch were exactly the ones that stopped
releasing. It is a *tail* `let`, so it compiled to `PushEnv` with no `PopEnv`,
same as `tail-let` above.

Re-measured after merging, every row recovers **100.0%** (57,313 / 57,313;
85,969 / 85,969; 57,313 / 57,313): both `let`s merge into the parameter frame, so
the variants no longer differ in recovery.

Per-iteration allocation for a loop, by shape:

```scheme
(define (tail-loop i n)
  (if (>= i n) i (tail-loop (+ i 1) n)))

(define (arg-loop i n)
  (if (>= i n) i (arg-loop (let ((j (+ i 1))) j) n)))

(define (nested-loop i n)
  (if (>= i n) i
      (let ((j (+ i 1)))
        (nested-loop j n))))

(define (deep-loop i n)
  (if (>= i n) i
      (let ((j (+ i 1)))
        (let ((k j))
          (deep-loop k n)))))
```

| shape | allocs/iteration, then | allocs/iteration, now |
|---|---|---|
| `tail-loop` — self-tail call, no `let` | 0 | 0 |
| `arg-loop` — `let` in argument position (call still at depth 0) | 3 | 0 |
| `nested-loop` — `let` wrapping the tail call | 3 | **0** |
| `deep-loop` — two `let`s wrapping the tail call | 6 | **0** |

The "then" column is this document's original measurement, and the last two rows
were 5 and 8 before that, when `OpSelfTailCall` could not yet unwind `let` frames.
The "now" column is measured at `a204912d`: both
`TestNestedLetSelfTailAllocations` and `TestDoublyNestedLetSelfTailAllocations`
(`pkg/wile/tail_call_alloc_test.go`) log **`slope=0.000 frames/iter`** — 14 allocs
at 10,000 trips and 14 at 30,000, i.e. a fixed startup cost and nothing per
iteration. Let-slot merging removed the frames entirely.

Both tests read the slope across two trip counts so a fixed cost cannot hide in
it, but note they assert a *ceiling* (`slope > 4.0` and `slope > 7.0` fail), not
equality, so they kept passing across the change, and their in-code comments still
describe the 3.0 and 6.0 let-frame floors that no longer exist. `tail-loop` and
`arg-loop`, which neither test covers, measure the same 14 allocs at both trip
counts under the same engine configuration.

## What a sound recovery would need

Not a runtime flag — for this case. The discipline this codebase uses for nearly
every other frame reclamation is a **compile-time proof**, and the same one
applies here: a `let` body that provably creates no escaping closure and
references no capture operator cannot make its parent chain reachable, so the
enclosing frame stays releasable across it. Those two predicates already exist
(`bodyCreatesEscapingClosure`,
`bodyReferencesCaptureOperator`) and are already composed for the self-tail-call
proof. The minimal pair is `leaky` above against:

```scheme
(define (safe i n)
  (cons (let ((j (+ i 1)))
          (* i j))            ; no lambda, no call/cc — nothing outlives the let
        n))
```

For a procedure-body `let` the question has moved: with the `let` merged there is
only the parameter frame, and the escape-gated `OpReleaseEnvFrame` gate
(no capture, no escaping closure, only capture-safe callees) decides on it.
Compiled today, `safe` emits `OpReleaseEnvFrame` before its tail call to `cons`
and `leaky` does not.

**The one class recovered without a proof, and why it does not generalize.** Until
`e28364f9`, every `call-with-values`, `call-with-exit`, prompt, barrier and
`force` call drained exactly one frame from the pool: the helper builds its
continuation frame over `mc.env` — inside a primitive, that is the pooled apply
frame holding the primitive's own arguments — and `NewMachineContinuation` left
`envPooled` false, so the restore dropped it to the GC instead of recycling it.
The fix is a runtime **ownership transfer** (`transferEnvOwnership`,
`run_body_under_frame.go`): the pooled status moves to the continuation frame and
`p.envPooled` is cleared, so there is exactly one owner and no double release. Its
licence is that no bytecode runs between building the frame and applying the body,
so nothing can parent on it in that window. That is a window argument, not an
escape analysis, and a `let` is the opposite case — arbitrary bytecode runs under
it, which is what forces the proof.

Two separate pieces would follow from the proof. **Both were sized against a tree
in which every `let` pushed a frame, and let-slot merging took that population
away** — they now reach only unmerged `let`s, i.e. the outermost `let` of a
top-level form. Kept because the reasoning is the same if the residual ever
matters:

- Re-arming the release at `OpPopEnv` behind that proof — reaches the share of
  unmerged `let` frames that have a pop (8% of the pre-merging population).
- Giving the tail-position case somewhere to release at all — the rest, and a
  design rather than a gate.

Independently, and needing no proof of any kind, an unmerged `let` frame's keys
map is a compile-time constant that `OpPushEnv` rebuilds every time. Interning the
compile-time frame as a template literal — exactly what `compileClosureBody`
already does for a lambda — would remove one of its three allocations. The same
caveat applies: merging already removed the sites where this would have paid.

**That argument was taken, and flat closures ship.** This section once framed it
as a standing alternative: rather than prove things about a capture path, delete
one. A **flat closure** holds the *values* of its free variables rather than a
pointer to the frame they live in. That is now the implementation —
`OpMakeClosure` links `env.TopLevel()` (`closureLink`), not `mc.env`, unless the
template retains its lexical env, and free variables travel by
value in a vector (`pkg/machine/operations_free.go` is the read side), which is
why the `envPooled` clear at that site is narrowed to the retaining case.

The corollary it did not carry still does not: captured continuations,
sub-contexts, SRFI-18 thread starts and `dynamic-wind` winders reach frames by
other routes, so a tail release would need that set re-derived on its own — which
is exactly the inference that failed three times above. Flat closures removed one
capture path; they did not remove the need for the proof.

## See also

- [`system.md`](system.md) — environment architecture, phases, binding stores
- [`diagram.md`](diagram.md) — the type relationships behind these frames
- [`../continuations/optimizations.md`](../continuations/optimizations.md) —
  continuation-side performance, including the continuation and stack pools
