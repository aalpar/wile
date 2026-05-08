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

**Principle**: Dependency / State Tightness
**Where**: `machine/machine_context.go:83`, `machine/machine_context.go:925-932`;
consumers in `machine/compilation/operation_syntax_case.go:51-289`
**Theory**: Universe types as dependency-inversion shims; **boolean blindness**
generalized to **type blindness** (Harper, *PFPL* §11). The `any` type has 2^∞
representable values; only one concrete type (`*compilation.syntaxCaseState`)
is semantically valid here, so type precision = 1/∞ ≈ 0%.
**Current state**:
```go
syntaxCase    any              // *compilation.syntaxCaseState; nil when not in syntax-case

func (p *MachineContext) SyntaxCaseState() any { return p.syntaxCase }
func (p *MachineContext) SetSyntaxCaseState(v any) { p.syntaxCase = v }
```
Every consumer in `compilation/operation_syntax_case.go` re-runs a type
assertion: `sc, ok := mc.SyntaxCaseState().(*syntaxCaseState)`.
**Problem**: `any` is the **universe type** — every value of every type is a
valid inhabitant. The comment names the *one* type that is actually allowed,
but the type system enforces nothing. Worse, the price of preserving this
typing is paid on every read (a runtime type assertion). The **Curry-Howard**
view: the proposition this type asserts is "something exists" rather than
"this specific contract exists." Compilers verify the former trivially; only
the latter prevents bugs.

This shape is a familiar dependency-inversion shim: `machine/` wants to avoid
importing `compilation/`, so it accepts an opaque pointer it cannot speak
about. But the *better* shim is a 0-method tag interface in `machine/`:

```go
// machine/syntax_case.go
type SyntaxCaseState interface { isSyntaxCaseState() }  // sealed marker
```

**Proposed direction**: Define a sealed marker interface in `machine/` (one
file, ~5 lines). Have `compilation.syntaxCaseState` implement it via an
unexported method. The field becomes `syntaxCase SyntaxCaseState`, the
accessor returns the interface, and consumers still type-assert to the
concrete type — but unauthorized types are now compile-time prohibited from
being stored. This is the **existential type** pattern (Pierce, *TAPL* §24).
**Impact**: Type precision goes from 1/∞ to 1/1 for the field's own
assertions; unauthorized stores become impossible; readers' type assertions
become specifications rather than runtime risks.
**Estimated size**: S.

### Finding 2 — Tail/non-tail opcode duplication: 28 cases that differ in one bit

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
**Impact**: Removes the implicit `0 == off` convention; removes signed/unsigned
juggling at every check site; makes the "is this limit even on?" question a
property of the type rather than a runtime convention.
**Estimated size**: S.

### Finding 5 — Repeated stack-size guard across opcode cases (hand-unrolled loop body)

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
**Problem**: There are six call sites; over time, "the opcodes that grow the
stack" will drift from "the opcodes that include this guard." Already,
`OpPullApply` and the 14 promoted ops grow the stack via `mc.evals.Push(...)`
indirectly through `execPromoted`/inline functions and *don't* run the guard.
Whether that's intentional depends on whether the inline functions are
bounded — and that's invisible from the call-site reader's perspective.
**Proposed direction (least invasive)**: Push the guard into the eval stack
itself. `Stack.Push` already exists — give it a single internal max-size
constraint via a small `Stack` struct field, set once in `NewStack(maxSize)`.
Every `Push` either succeeds, fails with the same error, or is unconditional.
The opcode dispatch loses the six guard blocks; the guard logic moves to one
place where it cannot drift.
**Impact**: 30 lines deleted from `Run()`; one line added to `Stack.Push`;
**closure** ensures every `Push` everywhere is guarded uniformly; new opcodes
opt-in by pushing or not pushing, not by remembering the guard.
**Estimated size**: S.

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
| 1     | 1       | S    | None                        |
| 2     | 5       | S    | None                        |
| 3     | 4       | S    | None                        |
| 4     | 6       | S    | None                        |
| 5     | 3       | S    | Decide on lint vs. discipline |
| 6     | 2       | M    | Re-run bench against the previous experiment baseline |
| 7     | 7       | L    | After Findings 1, 4 — they shape sub-record boundaries |

Phases 1–5 are independent and can be picked off in any order. Phase 6 needs
benchmark validation before commit. Phase 7 is the largest and should land
last; it absorbs the small wins from prior phases into a coherent
sub-structure.

## Cross-references

- `plans/2026-05-07-structural-reduction-roadmap.md` — **gating**.
  Recommends Tier A analyses (`values/`, `environment/`, `registry/`) before
  this plan's implementation. Phase 7 (named sub-records for
  `MachineContext`) interacts with how `EnvironmentFrame` is held on
  `MachineContext`; sequence after `environment/` analysis.
- `memory/2026-04-05-structural-reduction.md` — earlier promoted-ops
  experiment that rejected table-driven dispatch (~1.5% regression).
  Finding 2 proposes a *different* encoding (sign-bit on operand) not tested
  there.
- `machine/CLAUDE.md`, `machine/CLAUDE.local.md` — architectural reference
  used to validate findings against existing invariants.
- TODO.md Tier 5 "FCA-Derived" — `vmCore sub-struct extraction` is a
  complementary structural change in the same package; sequence Finding 7
  with that item.
