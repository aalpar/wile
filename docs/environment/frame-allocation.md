# Environment-Frame Allocation and Recovery

Every procedure call and every `let` creates an environment frame. Most of them
are recycled through a freelist; a specific, measurable subset cannot be, and the
reason is a load-bearing invariant rather than an oversight.

This document explains which frames are recovered, which are not, and why the
obvious fix for the second group is unsound. It exists because that fix has been
proposed — and reverted — repeatedly.

## Two kinds of frame, two very different costs

| | parameter frame | `let` frame |
|---|---|---|
| created by | `Apply` (the closure-call path) | `OpPushEnv` |
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
fresh frame struct on every execution of the same `let` — three allocations, none
recycled.

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

A procedure whose body contains a `let` builds a two-frame chain. The parameter
frame is the pooled one; the `let` frame hangs off it as a lexical child.

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

Four sites make the current frame a **parent**, and each clears the flag in the
same step:

| site | why it clears |
|---|---|
| `OpPushEnv` | the new `let` frame points at `mc.env` as its parent |
| `OpMakeClosure` | the closure holds `mc.env` as its captured parent |
| `OperationBindPatternVars` | `syntax-case` binds its pattern variables in a child frame (`compilation/operation_syntax_case.go:292`, cleared at `:350`) |
| `ClosureEnv`'s detached-env fallback | a `ForeignClosure` outliving its call is parented on `mc.env` when that frame has no mutable runtime (`machine_context.go:341`) |

The first two are the ones a Scheme program reaches on every call; the other two
are narrow, and each carries its own clear. All four run under `Run` — being
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

Once the `let` executes, the enclosing procedure's parameter frame is no longer
recoverable on return — and the mechanism differs by where the `let` sits.

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

The two shapes, and what each compiles to:

```scheme
;; TAIL — the let is the last thing the body evaluates.
;; Emits: PushEnv=1  PopEnv=0
(define (tail-let i n)
  (if (>= i n)
      i
      (let ((j (+ i 1)))
        (helper j n))))         ; mc.env is still the LET frame at return

;; NON-TAIL — the let is an argument, so its value must come back.
;; Emits: PushEnv=1  PopEnv=1
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

A program that walks that sequence:

```scheme
;; Emits: PushEnv=1  MakeClosure=1  PopEnv=1 — all three of the diagram's ops.
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

**Read the counters below as PARAMETER frames only.** A `let` frame never enters
the pool in either direction — `OpPushEnv` allocates one directly and nothing ever
calls `releaseEnvFrame` on it — so `let` frames are invisible to these numbers and
their recovery rate is **0%, in every row**. What the hit rate measures is how
often `Apply`'s acquire found a recycled *parameter* frame waiting instead of
minting a new one.

That is the shape of the cost: a `let` allocates three frames' worth of objects
that are never recycled, **and** it knocks out recovery of the enclosing
procedure's parameter frame, which is a different frame and the one the pool
exists for.

The freelist's own counters, whole-program:

| benchmark | parameter frames acquired | released back | misses | recovered |
|---|---|---|---|---|
| `fib` (no `let`) | 2,692,550 | 2,692,549 | 27 | **100.0%** |
| `nqueens` | 8,503,623 | 5,623,346 | 2,880,278 | 66.1% |
| `sieve` | 1,040,948 | 694,591 | 346,358 | 66.7% |
| `peval` | 900,031 | 400,020 | 500,012 | 44.4% |

`fib` is the control: 27 misses across 2.7M acquires is warmup, and it shows the
pool works perfectly when frames come back. Everywhere else
`misses == in-flight-at-exit` exactly — every miss is one frame acquired and never
returned.

Re-measured at `e28364f9` (2026-08-19): all four recovery rates are unchanged and
every release count is identical. Acquires differ by 9 on `fib` and 1 on
`nqueens`, unattributed. The misses column was not re-read — it is the freelist's
own factory counter, not `VMCounters`.

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

Driven by `(fib 22)`, whose 57,313 activations are the whole of column one in row
1 — nothing else in the program applies a closure:

| body | parameter frames acquired | released back | recovered |
|---|---|---|---|
| no `let` | 57,313 | 57,313 | **100.0%** |
| `let` in non-tail position | 85,969 | 57,313 | 66.7% |
| `let` wrapping **only the base branch** | 57,313 | **28,656** | **50.0%** |

The third row is the sharpest: that `let` is off the recursive path and changes
no acquire count at all — and half the *parameter* frames stop coming back. The
28,657 activations that reach the base branch are exactly the ones that stop
releasing. It is a *tail* `let` — the last form the body evaluates on that branch
— so it compiles to `PushEnv` with no `PopEnv`, same as `tail-let` above. The
`let` frames it created are additional, and are not counted here because none of
them was ever poolable.

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

| shape | allocs/iteration |
|---|---|
| `tail-loop` — self-tail call, no `let` | 0 |
| `arg-loop` — `let` in argument position (call still at depth 0) | 3 |
| `nested-loop` — `let` wrapping the tail call | 3 |
| `deep-loop` — two `let`s wrapping the tail call | 6 |

The last two rows were 5 and 8 before `OpSelfTailCall` learned to unwind `let`
frames; what remains in each is 3 per `let` frame, which is the subject of this
document. Both are pinned as regression floors by
`TestNestedLetSelfTailAllocations` and `TestDoublyNestedLetSelfTailAllocations`
(`pkg/wile/tail_call_alloc_test.go`), which read the slope across two trip counts
so a fixed startup cost cannot hide in it.

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

Two separate pieces would follow from the proof, and they are different sizes:

- Re-arming the release at `OpPopEnv` behind that proof — reaches the 8% of `let`
  frames that have a pop.
- Giving the tail-position case somewhere to release at all — the other 92%, and
  a design rather than a gate.

Independently, and needing no proof of any kind, the `let` frame's own keys map is
a compile-time constant that `OpPushEnv` rebuilds every time. Interning the
compile-time frame as a template literal — exactly what `compileClosureBody`
already does for a lambda — would remove one of the three allocations at every
site.

There is also a standing argument that the premise of this section is the problem.
`plans/2026-08-18-flat-closure-conversion-design.local.md` (design only, nothing
implemented or measured) observes that every lever here needs that same proof, and
proposes deleting one of the two capture paths instead: a flat closure holds the
*values* of its free variables, so no closure ever points at a frame and the
question stops being asked. It declines the corollary — captured continuations,
sub-contexts, SRFI-18 thread starts and `dynamic-wind` winders still reach frames,
so a tail release would need that set re-derived on its own, which is exactly the
inference that failed before.

## See also

- [`system.md`](system.md) — environment architecture, phases, binding stores
- [`diagram.md`](diagram.md) — the type relationships behind these frames
- [`../continuations/optimizations.md`](../continuations/optimizations.md) —
  continuation-side performance, including the continuation and stack pools
