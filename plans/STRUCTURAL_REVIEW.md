# Structural Review

Codebase-wide review applying two lenses: **structural reduction** (dependency
minimization, state tightness, composability) and **staff-level technical debt**
assessment. Conducted 2026-02-27 against master at `addcb06`.
Updated 2026-02-27 against working tree (Promise fix applied, counts refreshed).

## Dependency Map

```
Layer 0 -- Leaf packages (no intra-module deps)
  values (I=0.00, Ca~25)    security (I=0.00, Ca=5)    internal/forms (I=0.00, Ca=2)

Layer 1
  internal/syntax (I=0.08, Ca=12)

Layer 2
  environment (I=0.13, Ca=13)    internal/tokenizer (I=0.50, Ca=2)    internal/schemeutil (I=0.50, Ca=2)

Layer 3
  internal/parser (I=0.33, Ca=8)    internal/match (I=0.75, Ca=1)    internal/validate (I=0.80, Ca=1)

Layer 4
  machine (I=0.38, Ce=9, Ca~15)

Layer 5
  registry (I=0.29, Ca~10)    registry/helpers (I=0.17, Ca~10)

Layer 6
  extensions/* (I~0.6-0.8 each)

Layer 7-8
  internal/extensions/* (I~0.8-0.9 each)

Layer 9
  internal/bootstrap (I=0.94, Ce=16)    runtime (I=0.71, Ca=2)

Layer 10
  root wile/ (I=0.81)    internal/repl (I=0.88, Ca=1)

Layer 11
  cmd/scheme (I=1.00)
```

**Metrics**: I = Ce/(Ca+Ce) where Ce = efferent coupling, Ca = afferent coupling.

**No cycles.** The graph is a strict 12-layer DAG.

**SDP note:** `registry` (I=0.29, Ca=10) depends on `machine` (I=0.38, Ca=15).
Registry is more stable yet depends on the less-stable machine package. This
coupling means any change to machine's public API has blast radius through
registry into all extensions. Not immediately actionable -- the coupling is
inherent to the architecture (registry registers primitives that operate on the
VM) -- but it is the single highest-leverage interface boundary in the codebase.

---

## Findings

### [High] ~~Remove `Promise.Forced` discriminant field~~ DONE

**Principle**: State Tightness
**Where**: `values/promise.go`

**Completed.** The `Forced bool` field was removed. The struct now uses
`Thunk == nil` as the sole discriminant. `forcePromise` in
`internal/extensions/all/prim_all.go` uses `promise.Thunk == nil` checks,
and `SchemeString()` does the same. 2 representable states, 2 valid (100%
precision).

---

### [High] ~~Table-drive `char-ci`/`string-ci` comparisons~~ DONE

**Principle**: Composability (hand-unrolled loop)
**Where**: `internal/extensions/all/prim_characters.go`,
`internal/extensions/all/prim_strings.go`,
`internal/extensions/all/register.go`

**Completed.** The 5 `PrimCharCi*Variadic` and 5 `PrimStringCi*Variadic`
functions were replaced with `charCiCompareSpecs`/`stringCiCompareSpecs` tables
and `makeCharCiComparePrimitive`/`makeStringCiComparePrimitive` factories,
mirroring the existing `charCompareSpecs` pattern in `registry/core`.
Registration loops in `addMoreChars`/`addMoreStrings` iterate over the tables.
~60 lines reduced to ~20.

**Effort**: S

---

### [High] ~~33 compound if-assignments across 17 files~~ DONE

**Principle**: Consistency Debt (convention violation)

**Completed.** All 33 production-code compound if-assignments split into
separate assignment + `if`. 3 `gocritic ifElseChain` warnings (newly exposed
by the split) suppressed with `//nolint` since they are type-assertion +
value-check chains, not switch candidates.

---

### [Medium] ~~`SyntaxSymbol.ResolvedBinding` is `any` (type erasure)~~ DONE

**Principle**: State Tightness / Dependency
**Where**: `internal/syntax/syntax_symbol.go`

**Completed.** Introduced `type ResolvedRef interface { values.Value }` in
`internal/syntax/syntax_symbol.go`. `ResolvedBinding any` changed to
`ResolvedBinding ResolvedRef` and `WithResolvedBinding(binding any)` to
`WithResolvedBinding(binding ResolvedRef)`. `*environment.GlobalIndex` already
satisfies `values.Value` so no changes needed in `machine/` or `internal/match/`.
Test stand-in strings (`"some-binding"`) replaced with `values.FalseValue`.
**Effort**: M

---

### [Medium] ~~Unused `formName string` parameter in typed compiler path~~ DONE

**Principle**: Composability (phantom parameter)
**Where**: `machine/compile_validated.go`, `machine/register.go`

**Completed.** Removed `formName string` from all 10 `CompileValidated*` method
signatures and from the `registerTypedCompiler` generic's `fn` type. The 9
adapter closures in `register.go` updated accordingly. Form name remains
accessible via `ValidatedExpr.FormName()` for any future need.
**Effort**: S

---

### [Medium] ~~`ToFloat64` handles 3 numeric types; `ExtractReal` handles 5~~ DONE

**Principle**: Consistency Debt
**Where**: `registry/helpers/value_conv.go`

**Completed.** `ToFloat64` extended to cover the full real tower: `Integer`,
`BigInteger`, `Float`, `BigFloat`, `Rational` — matching `ExtractReal`'s
type coverage without the exactness tracking. Complex types remain excluded
(correct: `atan y x` is real-only per R7RS §6.2.6; use `ToComplex128` for
complex). The `To*`/`Extract*` naming split is now semantically clean: `To*`
converts to a Go type covering the full applicable Scheme tower; `Extract*`
adds R7RS metadata (exactness, int64-or-big representation). Tests updated:
6 new success cases for `BigInteger`/`BigFloat`, corresponding error cases
removed.
**Effort**: S

---

### [Medium] `vmState.callDepth` is `uint64` with documented underflow risk

**Principle**: State Tightness
**Where**: `machine/vm_state.go:82-86`

The field is unsigned but depth is conceptually non-negative. Subtraction can
wrap silently to 2^64-1. The comment says "all depth computation must use the
parent pointer rather than arithmetic on callDepth" -- this is a process-level
mitigation for a type-level problem.

**Proposed direction**: Change to `int`. Maximum realistic call depth is bounded
by memory, not `int` range. Underflow becomes a detectable negative value.
**Effort**: S

---

### [Medium] `mctx` naming in 3 operation files

**Principle**: Consistency Debt
**Where**: `machine/operation_syntax_case.go`,
`machine/operation_syntax_rules_transform.go`,
`machine/operation_build_syntax.go`

Convention is `mc` for `*MachineContext` parameters. These three files use
`mctx`.

**Proposed direction**: Rename `mctx` to `mc`.
**Effort**: S

---

### [Medium] Single-line function definitions in sentinel types

**Principle**: Consistency Debt (imperative violation)
**Where**: `values/values.go:41-43,51-52` (5 methods on voidType/eofType),
`internal/syntax/syntax.go:24-29` (6 methods on syntaxVoidType)

CLAUDE.md imperative: "NEVER write single-line function definitions."
11 methods violate this.

**Proposed direction**: Expand each to multi-line form. Mechanical.
**Effort**: S

---

### [Low] `SyntaxForEach` callback signature is too broad

**Principle**: Composability (Interface Segregation)
**Where**: `machine/compile_time_continuation_library.go` -- 15 call sites, of
which 13 blank `index` and 14 blank `hasNext`

The callback signature `func(context.Context, int, bool, SyntaxValue) error`
forces every caller to acknowledge 4 parameters when most only use 1.

**Proposed direction**: Add a `SyntaxWalk(ctx, v, func(SyntaxValue) error)`
convenience wrapper that delegates to `SyntaxForEach`.
**Effort**: S

---

### [Low] `NativeTemplate` parallel slices have unenforced length invariant

**Principle**: State Tightness (representation invariant)
**Where**: `machine/native_template.go:28-53`

`code []Instruction` and `sourceRefs []uint16` must always have the same length.
The struct-of-arrays layout is deliberate for cache performance. No structural
change needed, but the invariant should be enforced at construction time.

**Proposed direction**: Add an assertion in the template constructor:
`if len(code) != len(sourceRefs) { panic("...") }`.
**Effort**: S

---

## Structural Observations (not findings)

These are worth documenting but don't require action.

### `vmState` split value register

`singleValue` and `multiValues` can theoretically both be non-nil
simultaneously (precision 75%). This is a documented performance optimization --
avoiding `[]Value{v}` allocation on every single-value path. `SetValue` and
`SetValues` each nil the other field. The invariant is maintained by write
discipline, and the performance rationale is sound.

### `MachineContext.thread` vs `vmState.threadID`

Two fields encoding thread identity separately. The split exists because
`threadID` propagates into continuations (in `vmState`) while `thread` is the
full Scheme object (only on `MachineContext`). The invariant
`thread.ID() == threadID` when both non-nil is not type-enforced but is
maintained by construction.

### `Boolean` singleton correctness

`BooleanToBool` uses pointer equality (`b == TrueValue`). `NewBoolean` is
exported and could produce non-singleton booleans, but a codebase-wide search
confirms it is never called outside `values/` itself. `BoolToBoolean` (which
returns singletons) is the only public constructor used in practice. The risk is
theoretical, not active.

---

## Summary

**State of the code**: Structurally healthy. Clean DAG with no cycles, stable
packages at the bottom, volatile wiring packages at the top. All High findings
and the three Medium structural findings are resolved. Remaining work is
consistency-level debt (naming drift in 3 files, 11 single-line functions,
`ToFloat64` coverage gap, `callDepth` type) and two Low-priority additions.

**Remaining items**:

1. ~~**`ToFloat64` vs `ExtractReal` gap** -- clarify or unify~~ DONE
2. **`vmState.callDepth` `uint64` → `int`** -- detectable underflow (S)
3. **`mctx` → `mc` rename** -- 3 operation files (S)
4. **Single-line function definitions** -- 11 methods (S)
5. **`SyntaxWalk` convenience wrapper** -- reduces 13 blanked parameters (S)
6. **`NativeTemplate` length-invariant assertion** -- panic at construction (S)
