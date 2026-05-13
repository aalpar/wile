# Machine package structural reduction

**Date**: 2026-05-06
**Source**: `/structural-reduction ./machine` analysis
**Status**: Findings recorded; not yet scheduled for implementation
**Priority**: High (Tier 5 tech debt)

## Scope analyzed

`machine/` (139 Go files, 56 production .go files, ~36K LOC including tests).
Core hot-path files inspected: `machine_context.go` (1393 lines), `vm_state.go`,
`machine_continuation.go`, `opcode.go`, `instruction.go`, `operation.go`,
`call_context.go`, `call_promoted.go`, `multiple_values.go`, `expander_ctx.go`.
Companion `machine/compilation/` treated as a peer for dependency direction only.

## Dependency map

```
                      ┌────────────────┐
                      │  environment   │  I≈0.10  (stable: many depend, depends on values only)
                      └────────────────┘
                              ▲
            ┌─────────────────┼──────────────────┐
            │                 │                  │
            │            ┌────┴───────┐          │
            │            │   values   │  I≈0.0  (sink)
            │            └────┬───────┘          │
            │                 ▲                  │
            │                 │                  │
   ┌────────┴────────┐   ┌────┴───────┐    ┌─────┴────────┐
   │      werr       │   │  security  │    │internal/syntax│
   └─────────────────┘   └────────────┘    └──────────────┘
            ▲                 ▲                  ▲
            └─────────────────┼──────────────────┘
                              │
                       ┌──────┴───────┐
                       │   machine/   │  I≈0.55
                       │              │  defines: CallContext, ExpanderCtx,
                       │              │           Operation, InlinedOperation
                       └──────┬───────┘
                              ▲
                              │  back-edge: implements ExpanderCtx,
                              │  consumes machine.MachineContext
                              │  via type-assertion on `any`
                       ┌──────┴────────────┐
                       │ machine/compilation│  I≈0.85
                       │ (compiler+expander)│
                       └───────────────────┘
```

**Observations on the graph**

- `machine/` and `machine/compilation/` are a clean DAG: `compilation → machine`,
  no cycle. The split was achieved by extracting the `ExpanderCtx` interface
  (`expander_ctx.go`) and the `syntaxCase any` field — both are
  dependency-direction inverters in the spirit of Parnas (1972).
- `CallContext` (`call_context.go:39`) is a textbook **Interface Segregation**
  win: 7 methods exposed to extensions versus ~75 on the concrete type. Worth
  preserving as positive prior art.
- The back-edge from `compilation/` to `machine.MachineContext` via
  `mc.SyntaxCaseState() any` is the **one structural crack** in the otherwise
  clean separation — see Finding 1.

## Findings

### Finding 1 — `syntaxCase any` field: dependency inversion implemented as universe type

**Status**: **Considered and declined** (PR #731 review cycle, 2026-05-10).
A marker-interface prototype was implemented and then reverted after
multi-lens crosscheck review. The diff was retained as a documentation
update; the type narrowing was not.

**Principle**: Dependency / State Tightness
**Where**: `machine/machine_context.go:83`, `machine/machine_context.go:920-935`;
consumers in `machine/compilation/operation_syntax_case.go:51-289`
**Theory**: Universe types as dependency-inversion shims; **boolean blindness**
generalized to **type blindness** (Harper, *PFPL* §11). The `any` type has 2^∞
representable values; only one concrete type (`*compilation.syntaxCaseState`)
is semantically valid here, so the *type-system* precision = 1/∞.

**Why declined — practical precision is already 1/1 by encapsulation**:
- The field is **unexported** (`syntaxCase`), so the only way to write to it
  is through `SetSyntaxCaseState`.
- `SetSyntaxCaseState` has **exactly two production callers**, both in
  `machine/compilation/operation_syntax_case.go` (`ensureSyntaxCaseState`
  setting `&syntaxCaseState{}`, `OperationClearSyntaxCaseInput` clearing
  to nil).
- `SyntaxCaseState()` has **exactly four production callers**, all in the
  same file, all type-asserting to `*syntaxCaseState`.
- The "unauthorized type stored" failure mode the marker would prevent
  has zero call sites that could trigger it; the bug class is bounded
  to zero by package boundaries.

**Cross-package sealing limitation in Go**: An `interface{ isFoo() }`
sealed-by-unexported-method pattern only works when the implementer
lives in the **same package** as the interface. Here `machine/` cannot
import `machine/compilation/` (one-direction dependency). The marker
method must therefore be **exported** (`IsSyntaxCaseState()`), defeating
the strict "sealed" property and introducing a project-novel pattern
(no other `IsX()` empty-marker interfaces existed in the codebase).
The doc would have to honestly admit "marker interface, not literally
sealed" — adding clarity overhead without delivering the original
type-system promise.

**Crosscheck convergence on declining**:
- **Type-design lens**: rated marker 5/10 for "Invariant Usefulness" —
  the protected bug class has no observed instances; readers still
  type-assert at every call site; the marker narrows writes only.
- **Consistency lens**: flagged the `IsX()` empty-marker pattern as
  having no prior art in the workspace — the only such hit in
  `machine/`. Introducing a new convention for one type, especially
  with documented partial-seal semantics, increases cognitive overhead
  for future maintainers.
- **Code review lens**: the empty-body marker method violated the
  project's "NEVER write single-line function definitions" imperative;
  multi-line empty-body methods are syntactically awkward and provide
  no behavioral signal.

**Conclusion**: The `any`-typed field with strengthened doc comment is
the right shape for this code. The structural-reduction lens correctly
identified that *type-system precision* is suboptimal; the cost-benefit
analysis (one writer × one consumer-package × encapsulation already
enforces) does not justify a project-novel pattern that future
maintainers will copy without understanding the cross-package
limitation.

**Reopen criterion**: If the call-site count grows beyond `compilation/`
— e.g., a new package needs to write or read this field — revisit. The
encapsulation-as-defense argument depends on the single-consumer
property; if that property erodes, the type-system check becomes
load-bearing.

**Estimated size when reopened**: M — would require either (a) moving
`syntaxCaseState` to `machine/` (drags `internal/match` into machine's
import graph) or (b) defining a real interface with typed methods
matching the `ExpanderCtx` precedent.

### Finding 2 — Tail/non-tail opcode duplication: 28 cases that differ in one bit

**Status**: **Considered and declined** (2026-05-11, post-PR #737). The
encoding-driven collapse was prototyped on `feat/machine-sr-finding2`
(see closed PR #737) and bench-tested against `master` via the
project's pinned interleaved methodology
(`memory/finding5-bench-methodology.md`). All 16 Gabriel benchmarks
regressed; geomean **+2.5%**, 5× the parent-plan gate of ±0.5%. The
hypothesis that "encoding-driven dispatch retains the jump-table
compilation, costing only a predictable per-call-site branch on
`instr.Arg`'s sign bit" was wrong in practice — the dominant cost was
loss of compiler specialization: pre-collapse, the literal `false` /
`true` arguments at each `execPromoted` call site let the compiler
constant-fold the tail branch into separate code paths; post-collapse
the tail flag is decoded from `instr.Arg` at runtime, defeating that
specialization. The original author's hand-unroll was load-bearing for
performance. Full bench data and cost-model analysis in
`memory/finding2-collapse-revert.md`. This finding now joins Finding 1
and Finding 4(b) as a closed "considered and declined" item.

**Principle**: Composability
**Where**: `machine/opcode.go:93-126` (constants); `machine/machine_context.go:652-890`
(28 dispatch cases); `machine/call_promoted.go:184-207` (`execPromoted`);
14 entries × 2 in `promotedOpForName` at `call_promoted.go:238-269`
**Theory**: Canonical **hand-unrolled loop** anti-pattern — N blocks of code
differing only in one parameter value. By the **substitution principle**
(Strachey 1967), if I substitute `OpEqQ → OpEqQTail` everywhere, the only
differing observable is the `tail` bool passed to `execPromoted`. The pattern
is already factored at the *implementation* level (one `execPromoted`
function, one `inlineEq` per primitive); only the *dispatch* is unrolled.
**Current state**: 28 cases, each looking like:
```go
case OpEqQ:
    var err error
    mc, err = execPromoted(mc, instr, "eq?", 2, false, inlineEq)
    if err != nil { return err }

case OpEqQTail:
    var err error
    mc, err = execPromoted(mc, instr, "eq?", 2, true, inlineEq)
    if err != nil { return err }
```
The author has documented this is intentional ("Go compiles them to a jump
table; a table-driven approach was benchmarked and rejected — ~1.5% geo mean
regression. See plans/2026-04-05-structural-reduction.md").
**Problem**: The benchmark rejected *table-driven dispatch* (loading function
pointers from an array), not *encoding-driven dispatch*. The two are
different. The current encoding makes "tail-ness" a property of the *opcode*
— a 1-bit attribute consuming a whole opcode slot. Information-theoretically,
the 14 promoted ops carry 1 bit of tail-ness; the current scheme spends
`log₂(28)/log₂(14) ≈ 1.36×` the opcode entropy.

There is a representation that retains the per-case switch (preserving the
jump-table compilation) while halving the cases: encode tail-ness in the high
bit of `instr.Arg` (currently `int32` cached-binding index, plenty of room).
Then `case OpEqQ:` handles both, branching `if instr.Arg < 0` for tail. The
branch is predictable per call site (each call site is either always tail or
always non-tail at compile time), so prediction cost is zero. The `tail bool`
parameter to `execPromoted` already exists — only the case label and the
`bool` literal change.
**Proposed direction (testable)**: Move tail-ness from opcode to `instr.Arg`
sign bit; collapse 28 cases to 14. Re-run `make bench-extended` to confirm
the jump-table is still emitted (it should be — same cardinality regime). If
the regression is still >1%, the cost-benefit favors keeping the unroll and
this finding is stale; otherwise, eliminate the duplication.
**Decidable by measurement**, not aesthetics.
**Impact**: 28 dispatch cases → 14; opcode count `opCount` drops by 14;
`opcodeTable` shrinks; `promotedOpForName` returns a single op + a tail flag;
reduces edit-cost when adding a new promoted primitive (already documented in
`call_promoted.go:25-36` as a 3-file edit — with this change it becomes a
2-file edit).
**Estimated size**: M (small diff but requires re-benchmarking against the
previous experiment baseline).

### Finding 3 — Split value register: documented invariant, unenforced by types

**Principle**: State Tightness
**Where**: `machine/vm_state.go:97-113`; reads at `machine_context.go:209-235`;
writes at `machine_context.go:191-207` and `machine_continuation.go:140-146`
**Theory**: Sum type encoded as a product type. By type algebra:
```
Current shape:  singleValue: Value × multiValues: MultipleValues
                |representable| = (|Value|+1) × (|MV|+1)
                |valid|         = 1 + |Value| + |MV|     (empty | one-single | multi)
                precision       ≈ 0  for unbounded value spaces

Sum type shape: ValueRegister = Empty | Single Value | Multi MultipleValues
                |valid| = |representable|, precision = 100%
```
The author is explicit: "INVARIANT: at most one of the two fields is 'active'
at any time." That is a runtime obligation a sum type would discharge at
compile time.
**Current state**: A documented mutual exclusion between two fields, enforced
by every writer manually nilling the other.
**Problem**: Every write site is now responsible for an obligation that the
type doesn't track. By **Hoare logic**:
```
{singleValue = nil ∨ multiValues = nil}  SetValue(v)  {singleValue = v ∧ multiValues = nil}
```
The precondition has to be re-established by every consumer. The `PushValues`
method on `MachineContinuation` (`machine_continuation.go:140-146`) carries a
"promote-then-append" comment that exists *because* the invariant is
unenforced — promotion logic to keep the two fields consistent.
**Proposed direction**: Leave the split, but tighten encapsulation. The
author has measured: the split saved ~20% of allocations on call-heavy
benchmarks. A sum type using a `values.Value`-typed interface case would
re-introduce the very allocation the split was designed to remove.
**The optimizer chose to make illegal states representable in exchange for
measured throughput** — a defensible Pareto choice that should be documented
as such.

What *can* be tightened without re-introducing allocation is **encapsulation
discipline**: `singleValue` and `multiValues` are unexported, but the helper
methods (`SetValue`, `SetValues`, `GetValue`, `GetValues`, `PushValues`) are
scattered across two files. Co-locate them in a small set of methods on
`vmState` itself, with a hard rule that *no other code* reads or writes those
two fields directly. Then the unenforced invariant becomes locally
enforceable by code review. Run
`grep -n 'singleValue\|multiValues' machine/*.go | grep -v vm_state.go` —
every hit is a place the invariant could be violated.
**Impact**: Zero allocation change; reduced surface area for future invariant
violations; explicit documentation of an intentional precision/throughput
trade-off.
**Estimated size**: S (consolidate accessors; add lint or grep canary).

### Finding 4 — `maxCallDepth` / `callDepth` type mismatch and sentinel-encoded boolean

**Status**: **Partially shipped, partially declined** (2026-05-10).
- (a) Type-mismatch half: **shipped** as commit `7dc2511c` —
  `maxCallDepth` unified to `int`, hot-path `uint64(callDepth)` cast
  deleted. Public API broke: `WithMaxCallDepth(n int)` and
  `DefaultMaxCallDepth int` (Wile v1.x zero-consumers policy).
- (b) Sentinel-removal half: **considered and declined**. `maxCallDepth
  int → *int` plus `WithMaxCallDepthUnlimited()` was prototyped and
  bench-tested against the Gabriel suite. Gate criterion failed
  decisively: geo-mean +2.76% (gate ±0.5%, 5.5× over), worst case
  +7.87% on `sum` (gate 0.3%, 26× over); all 16 benchmarks
  regressed. PR #636's "cost is global (deref on every check)"
  prediction was confirmed line-for-line. Full bench data and
  cost-model analysis in
  `memory/maxcalldepth-nullable-revert.md`.

**Principle**: State Tightness
**Where**: `machine_context.go:84` (`maxCallDepth uint64`),
`vm_state.go:181` (`callDepth int`), check at `machine_context.go:1176`
**Theory**: Two related observations.

*(a) Type mismatch on a paired pair*: `vmState.callDepth` is `int`;
`MachineContext.maxCallDepth` is `uint64`. They appear together in the bound
check (`if uint64(callDepth) > maxCallDepth`). The compiler does the `uint64`
cast every check; the human reader has to mentally convert too.

*(b) Sentinel-encoded boolean*: `maxCallDepth uint64 // 0 = unlimited (default)`.
This is **Harper's boolean blindness in an integer disguise** — the boolean
`enabled` and the value `limit` are crammed into one field. Valid space is
`{0} ∪ {1, 2, …}` but the type admits all of `[0, 2^64)`.

**Current state**: Two intertwined sentinel fields (`maxCallDepth`,
`maxStackSize`) and a counterpart (`callDepth`) typed differently from its
bound.
**Problem**: Both pathologies are **information loss**. Six identical
instances of `if mc.maxStackSize > 0 { check }` in `Run()` (lines 368, 433,
529, 543, 557, 607) — itself a hand-unrolled idiom (Finding 5 below).
**Proposed direction**: Introduce an `Option[uint64]` or just an explicit
`*uint64` (nil = unlimited). For the type mismatch, decide a single type for
`callDepth` — `int` is fine since continuation chain length is bounded by
stack space anyway. The comparison becomes
`if mc.maxCallDepth != nil && callDepth > int(*mc.maxCallDepth)`.

**⚠ Historical context.** The `0 = unlimited` sentinel on `maxStackSize`
was chosen **deliberately** in PR #636 over a `(uint64, bool)` set/unset
pair: *"unlike `maxCallDepth` there is no default value, so zero-value =
not called = unlimited"* (`memory/2026-04-11-eval-stack-limit-design.md`).
A `*uint64` nullable replaces that `bool` flag with a pointer indirection
plus a heap allocation per setter. The "removes signed/unsigned juggling"
claim must be weighed against re-introducing exactly the kind of
indirection the original design avoided. **The win is local
(check-site readability); the cost is global (heap allocation +
deref on every check).**

**Bench-gating recommended**: Same criterion as Finding 5 — no Gabriel
regression > 0.3% per benchmark, geo-mean delta within ±0.5%. The check
sits in the same hot-path dispatch loop. If a measurable regression
appears, alternatives include keeping the sentinel and addressing only
the type mismatch (`callDepth int` ↔ `maxCallDepth uint64`) by
unifying both as `uint64` or both as `int`.
**Impact**: Removes the implicit `0 == off` convention; removes signed/unsigned
juggling at every check site; makes the "is this limit even on?" question a
property of the type rather than a runtime convention.
**Estimated size**: S (with bench-gate); type-only fix without sentinel
change is XS.

### Finding 5 — Repeated stack-size guard across opcode cases (hand-unrolled loop body)

**Status**: **Shipped via Option D** (2026-05-11, PR #734).
- (a) Option C-light (lift check to `Run()` loop head): **considered and
  declined**. Bench-tested against the Gabriel suite; gate failed
  decisively at +4.17% geo-mean regression (gate ±0.5%, 8× over). All 16
  benchmarks regressed; worst +6.6%. The cost is structural: shifting
  two field loads plus a branch from "only on push opcodes" to "every
  iteration" charges non-push opcodes (`Apply`, `LoadLocal`, `Branch`,
  `RestoreContinuation`, …) for a check they previously avoided.
- (b) Option D (extracted inlinable wrapper at the 6 push sites):
  **shipped**. New `checkStackSize` entry point delegates to
  `reportStackOverflow` only when `maxStackSize > 0`; the unlimited
  default returns immediately. Go inliner confirms `checkStackSize`
  inlines at cost 67/budget 80; the cold delegate stays a real call
  (cost 105) but only fires when bounded. Source-level dedup at the
  call sites; hot-path code generation unchanged from the hand-inlined
  status quo. Bench (pinned CPU, interleaved 3×3-block measurement):
  geo-mean +0.003%, 8 faster / 8 slower (balanced), worst per-bench
  regress deriv +0.99% offset by tak −1.37%. Full bench data and the
  measurement-methodology note are in
  `memory/finding5-bench-methodology.md`.

**Principle**: Composability
**Where**: `machine/machine_context.go:368-373, 433-438, 529-534, 543-548, 557-562, 607-612`
**Theory**: Six call sites of the *same five-line block*:
```go
if mc.maxStackSize > 0 {
    err := mc.checkStackSize()
    if err != nil {
        return err
    }
}
```
By **closure under refactoring** (Birds & de Moor, *Algebra of Programming*),
repeated identical blocks are an `applyToEach` waiting to be discovered. The
block has zero variation across its six occurrences — it is the **identity
morphism** on each opcode case, copied verbatim.
**Current state**: Each opcode that *grows* the eval stack (`OpPush`,
`OpUnpackListToStack`, `OpPushLiteral`, `OpPushGlobal`, `OpPushLocal`,
`OpPushCachedBinding`) inlines this block.

**⚠ Historical context (must read before implementing).** The duplication is
**deliberate perf scaffolding**, not accidental drift. Three pieces of
evidence converge:

1. **PR #636 commit 3 (`perf: guard checkStackSize calls with maxStackSize > 0`)**
   states: *"`checkStackSize` is not inlined by the Go compiler (cost 110,
   budget 80) and its format args escape to heap. Guard all 6 call sites so
   the default (unlimited) path has zero overhead in the VM hot loop."*
   The `if mc.maxStackSize > 0 { … }` guard exists **specifically** to keep
   the no-limit common case at one predicted-not-taken branch.
2. **Design doc `memory/2026-04-11-eval-stack-limit-design.md`** explicitly
   selected this shape over a Stack-internal alternative: *"No structural
   changes to `Stack`. Follows the `maxCallDepth` pattern exactly."*
   ("Approach B from brainstorming" — the Stack-internal alternative was
   weighed and rejected.)
3. **The "What Is NOT Checked" section of the same design doc** documents
   that pushes from foreign functions and complex operations are
   **deliberately unguarded**: *"these pushes are bounded by bytecode
   structure, not user input, and the next VM-loop push will catch any
   accumulated growth."* The 6-site coverage is the *intended* coverage,
   not a coverage gap waiting to be closed. The design treats `maxStackSize`
   as a **coarse-grained DoS cap for sandboxed embedders**, not a precise
   per-push correctness invariant. The "PushAll Multi-Value Behavior"
   section also documents accepted transient overshoots — cap-precision is
   not the goal.

**Reframed problem**: The *structural* observation (duplication) is
correct. But the *direction* the original analysis proposed —
"every `Push` everywhere is guarded uniformly" — inverts the design: it
**re-introduces the cost** the perf guard was added to eliminate, and
**extends coverage** to push sites the design intentionally exempted.

**Three options**:

| Option | Shape | Perf risk | Coverage change |
|---|---|---|---|
| A | `Stack.Push` returns `error`; bounds-check inside | Re-introduces the `checkStackSize` no-inline + heap-escape cost on every push regardless of limit; signature break across all push sites | Extends to all push sites (changes design intent) |
| B | `Stack.Push` panics on overflow (matches existing `Pull`/`Pop`/`PeekK` panic-on-underflow idiom) | Removes signature break; same per-push branch cost as A; needs panic recovery in `Run()` | Same as A |
| C | `Stack` owns `maxSize` field; `Run()` outer loop calls `s.CheckOverflow()` once per dispatch iteration when `s.maxSize != 0` | Single fast-path skip in the no-limit case; **same coverage as today** | Unchanged — preserves the design's intentional scope |
| D (status quo with extracted helper) | Replace 6 inline blocks with one call to `mc.checkStackSizeFast()` (an inlinable wrapper that does the `> 0` test and tail-calls `checkStackSize`); same coverage as today | Risk that the wrapper itself doesn't inline (cost budget); needs measurement | Unchanged |

**Recommended direction (revised)**: **Option C** — Stack owns its limit;
the dispatch loop checks once per opcode boundary when the limit is set.
Closes the duplication finding (`Run()` no longer has 6 inline guards) and
preserves the design's coverage scope and zero-overhead-when-unlimited
property.

**⚠ Bench-gating (mandatory)**: Any version of this phase must be
benchmark-gated against PR #636's status quo. **Acceptance criterion**:
no Gabriel-benchmark with `maxStackSize == 0` regresses by more than 0.3%,
and the geo-mean delta is within ±0.5%. Methodology mirrors
`memory/2026-04-05-structural-reduction.md` Phase 2: 6 runs of
`make bench-gabriel` averaged, against the immediate-prior commit on the
same machine. **Precedent**: that prior structural change was
**rejected** at 1.5% geo-mean regression — well within the perf-cliff
regime an unoptimized restructure of this code path could trigger.
**Impact**: 30 lines deleted from `Run()`; the duplication closes;
the optimization remains.
**Estimated size**: S (Option C); M with bench measurement.

### Finding 6 — `Operation` interface is the empty contract

**Principle**: Composability / Interface Segregation
**Where**: `machine/operation.go:26-36`
**Theory**:
```go
type Operation interface {
    values.Value     // base interface = anything that can be a Scheme value
}
type InlinedOperation interface {
    Operation
    Apply(mc *MachineContext) (*MachineContext, error)
}
```
`Operation` adds nothing to `values.Value`. It is a **trivial subtype** in
Pierce's sense (*TAPL* §15) — the type `Operation = Value` modulo a name. The
intended distinction (which the comment articulates) is "Operations
dispatched via OpComplex's side table need `Apply`; opcodes inlined directly
into Run() don't." But the inlined opcodes are no longer carried as
`Operation` values at runtime — they're carried as `OpCode` integers in
`Instruction`. So `Operation` exists only as a literal pool entry for
printing/equality.
**Current state**: An interface that adds zero methods over its parent and
exists primarily as a documentation device.
**Problem**: When `Operation` is just `values.Value`, the compiler cannot
help you. Code that takes an `Operation` parameter cannot do anything
operation-specific with it; it could equally take a `values.Value`. This is
the **identity morphism wrapped as a type**. By Wadler's "Theorems for Free!"
lens, a function `f: Operation → T` proves nothing more than `f: Value → T`
would.
**Proposed direction**: Either (a) delete `Operation` and have side-table
entries be `InlinedOperation`-only (rename `InlinedOperation` →
`Operation`), since literal-pool entries are already `values.Value`; or
(b) add a discriminator method `OpKind() OpKind` to `Operation` that
distinguishes the dispatch tier and gives the type a real contract.
**Impact**: Small, but eliminates a misleading hierarchy.
**Estimated size**: S.

### Finding 7 — `MachineContext`: 18 direct fields + 13 embedded; per-field getter/setter pairs

**Status (2026-05-12)**: Closed at 2/3 stages shipped, −40 bytes per
`MachineContext`. Stages executed bench-gated per the recommended
phasing below.

| Stage | Cluster | Outcome | Net |
|-------|---------|---------|-----|
| 1 | `ExpansionState{expanderCtx, syntaxCase}` | ✅ Shipped (PR #742) | −24 B; geomean +0.47% within ±0.5% gate |
| 2 | `TimerState{timerHandler, timerCancel}` | ✅ Shipped (PR #743) | −16 B; geomean −0.316% (branch faster) |
| 3 | `SubContextState{parentMC, escapeCont, barrierValid, isolatedMarks}` | ❌ Declined | Field-independence analysis showed no load-bearing co-variance |

Stage 3 was inspected and declined: the four fields don't co-vary,
they have independent writers, and `isolatedMarks` is a bare bool
that today reads in a single op — clustering would add a double
check (`mc.subCtx != nil && mc.subCtx.isolatedMarks`) and a pointer
deref for a −24 B struct shrink with no invariant payoff. The
parent plan's grouping was thematic ("all sub-context related")
rather than semantic; honest closure beats shipping a non-load-bearing
cluster. Detailed rationale recorded in
`memory/finding7-cluster-outcomes.md`.

**Principle**: State Tightness / Composability
**Where**: `machine/machine_context.go:66-91`; methods at lines 127-298,
912-967, 1148-1170
**Theory**: `MachineContext` has 18 directly-declared fields plus the
13-field `vmState` embedding ≈ 31 fields. Of those, ~17 are accessed
externally via getter/setter pairs (`Set*`/`*` for `Thread`, `TimerHandler`,
`TimerCancel`, `EscapeCont`, `BarrierValid`, `Debugger`, `ExceptionHandler`,
`ExpanderContext`, `SyntaxCaseState`, `EnvironmentFrame`, `EnvPooled`,
`MaxCallDepth`, `MaxStackSize`, `Context`, `PC`, plus several more). When
every field has a getter and a setter, encapsulation is zero — the methods
don't enforce invariants, they merely add a syntactic indirection.
**Parnas (1972)** distinguishes hiding *design decisions* from hiding *fields*;
this is the latter without the former.
**Current state**: Many fields are mutually correlated:
- `parentMC` and `escapeCont` co-vary (both relate to sub-context creation).
- `timerHandler` and `timerCancel` are a pair (one without the other is
  meaningless).
- `barrierValid` and `isolatedMarks` are both linked to
  `applyCapturedContinuation`.
- `expanderCtx` and `syntaxCase` are both expansion-time-only fields, nil at
  runtime.

By **Liskov & Guttag**'s representation invariant lens, these correlations
are a **representation invariant** that the type does not enforce:
```
INV(mc):  (mc.timerHandler == nil) ⇔ (mc.timerCancel == nil)
INV(mc):  (mc.parentMC == nil) ⇒ ¬is-sub-context
INV(mc):  (mc.expanderCtx == nil) ⇒ (mc.syntaxCase == nil)
```
**Problem**: The struct is doing the job of three or four sub-types — runtime
VM, sub-context VM, expander VM, debugger VM — fused into one product type.
By type algebra, the representable state-space is the *product* of all field
domains; the *valid* state-space is much smaller because most fields are nil
in most modes.
**Proposed direction**: This is a research project, not a one-PR change. The
pragmatic intermediate step:
1. Group correlated fields into named structs:
   `TimerState{handler, cancel}`,
   `ExpansionState{expanderCtx, syntaxCase}`,
   `SubContextState{parentMC, escapeCont, barrierValid, isolatedMarks}`.
   Each becomes a `*Foo` field on `MachineContext`, nil in the common case.
   The product type stays — but the correlations become enforced (you can't
   have a non-nil handler with a nil cancel).
2. Each of the three accessor blocks becomes a small interface that
   delegates: `mc.Timer().Handler()` / `mc.Timer().SetHandler(h)`. Invariants
   live on the small struct.
3. Then the giant-MachineContext-with-many-getters surface becomes
   "MachineContext is a coordinator for these named sub-states."

**Impact**: Encapsulation goes from 0 (raw fields with getters) to enforcing
the documented co-variation invariants; reduces effective field count from
~31 to ~10–12 atomic + 3–4 sub-records; many of the ~50 single-field accessor
methods can be deleted.
**Estimated size**: L (cross-cutting; sequence after the smaller findings).

## Opportunities (sort-package style)

### Opportunity: `Stack` with built-in size constraint

- **Replaces**: 6 inline `if mc.maxStackSize > 0 { mc.checkStackSize() }`
  blocks in `Run()`.
- **Core operation**: bounded push (push or refuse + report).
- **Algebraic structure**: `Stack` becomes a **partial monoid** — `Push` is
  closed only when `len < limit`. With a limit field on the stack, every push
  checks itself; the operation is no longer "push" + "check" composed
  manually.
- **Proposed shape**:
  ```go
  type Stack struct {
      items   []values.Value
      maxSize uint64    // 0 = unbounded
  }
  func (s *Stack) Push(v values.Value) error { ... }
  ```
- **Reuse sites**: Already used everywhere; the change concentrates the
  guard rather than spreading it.

### Opportunity: Sealed marker for opaque cross-package payloads

- **Replaces**: `syntaxCase any` in `MachineContext`; the same shape may
  surface again whenever `compilation/` adds another sub-system that needs
  to ride on the VM context.
- **Core operation**: "carry an opaque pointer that only its originating
  sub-package can use, with type-level proof of provenance."
- **Algebraic structure**: This is the **existential type** pattern — the
  field stores `∃T:HasMarker. T`, not `∀T. T`.
- **Proposed shape**:
  ```go
  // machine/marker.go
  type SyntaxCaseState interface{ isSyntaxCaseState() }
  // (one such marker per opaque sub-state)
  ```
- **Reuse sites**: Future expander sub-states, debugger sub-states, profiler
  annotations — anywhere the VM context needs to carry sub-package-private
  data.

### Opportunity: `OpFlag` bit (or `instr.Arg` sign bit) carrying tail-ness

- **Replaces**: 14 paired non-tail/tail opcodes (28 cases → 14).
- **Core operation**: "execute promoted primitive; tail or non-tail
  epilogue."
- **Algebraic structure**: Tail-ness is an **orthogonal coordinate** of the
  instruction. The current encoding makes it a property of the opcode label;
  the proposed encoding makes it a property of the operand. Multiplying
  coordinates separates, summing them entangles.
- **Proposed shape**:
  ```go
  // instr.Arg holds (tailFlag<<31) | bindingIdx ; bindingIdx is at most 24 bits.
  case OpEqQ:
      tail := instr.Arg < 0
      bIdx := instr.Arg & 0x7FFFFFFF
      mc, err = execPromoted(mc, bIdx, "eq?", 2, tail, inlineEq)
  ```
- **Reuse sites**: Every promoted op; eliminates table-doubling cost when
  adding a new promoted primitive.

## What's already done well (preserve)

Three pieces of architecture are textbook good and should be preserved or
imitated when addressing the findings:

1. **`vmState` save/restore field table** (`vm_state.go:78-93`): an ASCII
   matrix documenting per-field treatment under each save/restore method. A
   **typestate diagram** in commented form. Imitate this format for
   `MachineContext` field correlations (Finding 7).

2. **`envPooled` write-site table** (`vm_state.go:194-207`): explicitly
   enumerates every write site and the ownership semantics. **Linear typing
   discipline** without a linear type system — works by pinning the set of
   writers to a finite documented list.

3. **`CallContext` 7-method interface** (`call_context.go:39-61`): proper
   application of the **Interface Segregation Principle**. Extensions get
   exactly what they need; internal code that needs more type-asserts to
   `*MachineContext`. The `ExpanderCtx` interface (`expander_ctx.go`)
   follows the same discipline.

## Closing summary

**State-space**: Of the high-traffic types examined, `MachineContext` carries
~31 fields with combined representable states ≈ Π of field domains; the valid
state-space is constrained by 4+ documented co-variation invariants (timer
pair, sub-context cluster, expansion cluster). Type precision for the
`syntaxCase any` field alone is ≈1/∞ (one valid concrete type out of all
types). Tightening the four invariants Finding 7 lists into named sub-records
and the `syntaxCase any` field into a sealed interface gives a measurable
precision gain at no runtime cost.

**Dependency count**: `machine/` direct imports: 5 (environment,
internal/syntax, security, values, werr) — none could be eliminated without
re-architecting the VM. `machine/compilation/` direct imports into machine:
~28 references via the public API. The DAG is clean; the one structural
crack is the `any`-typed back-channel (Finding 1), which has a 5-line fix.

## Recommended phasing

Sequence from highest impact-per-effort to lowest:

| Phase | Finding | Size | Gating                      |
|-------|---------|------|-----------------------------|
| ~~1~~ | 1       | —    | **Declined** — encapsulation already enforces the constraint; cross-package sealing limitation in Go undermines the marker-interface pattern. See Finding 1 status block above. PR #731 retained the doc work and reverted the type narrowing. |
| 2     | 5       | M    | **Bench-gate**: no Gabriel regression > 0.3%/bench, geo-mean ±0.5%. Per PR #636 commit 3 (`memory/2026-04-11-eval-stack-limit-design.md`), the duplication is a **deliberate perf workaround** (`checkStackSize` non-inlinable, escapes args). Recommended Option C (Stack owns limit, outer-loop check) over Options A/B which extend coverage and re-introduce the cost the perf guard exists to avoid. |
| 3     | 4       | S    | **Bench-gate** (same criterion as Phase 2). The `0 = unlimited` sentinel was chosen deliberately in PR #636 over a `bool` flag; `*uint64` re-introduces an equivalent indirection. Type-only fix (unify `int`/`uint64` mismatch without nullable change) is XS with no perf risk and may be the right narrow scope. |
| 4     | 6       | S    | None — **next phase to ship** |
| 5     | 3       | S    | Decide on lint vs. discipline |
| 6     | 2       | M    | **Bench-gate** against `memory/2026-04-05-structural-reduction.md` Phase 2 baseline (which rejected a similar restructure at 1.5% geo-mean regression). |
| 7     | 7       | L    | After Findings 1, 4 — they shape sub-record boundaries (Finding 1 declined; Finding 4 still ahead) |

**Bench-gate methodology** (mirroring `memory/2026-04-05-structural-reduction.md`):
6 runs of `make bench-gabriel` averaged, against the immediate-prior commit
on the same machine, with `maxStackSize == 0` (the no-limit common-case
hot path). Acceptance: no individual benchmark regresses > 0.3%; geo-mean
delta within ±0.5%. **Precedent for rejection**: prior structural-reduction
Phase 2 rejected at 1.5% geo-mean regression, 15/16 benchmarks slower —
this regime is real and recurring for VM hot-path restructures.

Phases 1, 4, and 5 are independent and can be picked off in any order
without measurement risk. Phases 2, 3, and 6 are bench-gated. Phase 7 is
the largest and should land last; it absorbs the small wins from prior
phases into a coherent sub-structure.

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — **gating**.
  Recommends Tier A analyses (`values/`, `environment/`, `registry/`) before
  this plan's implementation. Phase 7 (named sub-records for
  `MachineContext`) interacts with how `EnvironmentFrame` is held on
  `MachineContext`; sequence after `environment/` analysis.
- **`memory/2026-04-05-structural-reduction.md`** — earlier promoted-ops
  experiment that rejected table-driven dispatch at **~1.5% geo-mean
  regression, 15/16 benchmarks slower** (Apple M4 Max, 6-run average,
  `make bench-gabriel`). Concrete numerical baseline for the bench-gate
  threshold on Phases 2, 3, and 6. Finding 2 proposes a *different*
  encoding (sign-bit on operand) not tested there.
- **`memory/2026-04-11-eval-stack-limit-design.md`** — design doc for
  PR #636 (`maxStackSize` introduction). Documents (1) the deliberate
  choice to keep `Stack` unmodified ("Approach B from brainstorming",
  no structural changes to `Stack`), (2) the intentional coverage scope
  ("What Is NOT Checked" — foreign / complex-op pushes are
  bytecode-bounded, not user-input-bounded), (3) the deliberate
  approximate-cap behavior ("PushAll Multi-Value Behavior"), and (4) the
  rationale for `0 = unlimited` sentinel over a `bool` flag. **Findings 4
  and 5 must be reconciled with this prior art before implementation.**
- **PR #636 commit 3 message** (`perf: guard checkStackSize calls with
  maxStackSize > 0`) — explicitly states *"`checkStackSize` is not
  inlined by the Go compiler (cost 110, budget 80) and its format args
  escape to heap. Guard all 6 call sites so the default (unlimited) path
  has zero overhead in the VM hot loop."* This is the load-bearing
  rationale for the duplication Finding 5 surfaces.
- `memory/2026-04-11-eval-stack-limit-impl.md` — implementation tracker
  for PR #636.
- `machine/CLAUDE.md`, `machine/CLAUDE.local.md` — architectural reference
  used to validate findings against existing invariants.
- TODO.md Tier 5 "FCA-Derived" — `vmCore sub-struct extraction` is a
  complementary structural change in the same package; sequence Finding 7
  with that item.
