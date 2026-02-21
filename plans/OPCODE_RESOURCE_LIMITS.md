# Opcode Resource Limits Design

Date: 2026-02-15
Status: Design (not yet implemented)
Depends on: (raw complexity data embedded in this document)

## Problem

The VM loop checks `ctx.Done()` every 1024 operations (`contextCheckMask`), but several opcodes can block *inside* a single `Apply()` call for unbounded time before returning to the loop. A malicious or runaway Scheme program can exploit this to consume unbounded CPU within a single opcode execution.

## Threat Model

An untrusted Scheme program (or malicious input to a trusted program) triggers unbounded computation inside a single VM opcode, bypassing the context cancellation check in the main VM loop.

The attacker controls Scheme source code (or input data that drives macro expansion). The embedder controls engine configuration. The goal is to give embedders per-category knobs that cap the work any single opcode can do.

## Existing Limits

| Limit | Location | Default | Error Sentinel |
|-------|----------|---------|----------------|
| Call depth | `machine_context.go:69` | 0 (unlimited) | `ErrCallDepthExceeded` |
| read-string allocation | `prim_read_write.go:34` | 100 MB | `ErrAllocationLimitExceeded` |
| read-bytevector allocation | `prim_read_write.go:38` | 100 MB | `ErrAllocationLimitExceeded` |
| VM loop context check | `machine_context.go:38` | Every 1024 ops | (context error) |
| Match VM context check | `match.go:237` | Every 1024 iterations | (context error) |

## Complete Non-O(1) Operation Catalog

### Engine VM (14 non-O(1) operations)

#### Unbounded — Require Limits

| Operation | File | Complexity | Dominant Cost | Limit Category |
|-----------|------|-----------|---------------|----------------|
| `SyntaxCaseMatch` | `operation_syntax_case.go:65` | O(input) | Runs match VM internally | Match Steps |
| `SyntaxRulesTransform` | `operation_syntax_rules_transform.go:105` | O(clauses × (input + template × reps)) | Match per clause + Expand on first match | Match Steps + Expand Steps |
| `SyntaxTemplateExpand` | `operation_syntax_case.go:206` | O(template × reps) | Recursive `expandSyntaxValue` calls | Expand Steps |
| `Apply` (composable cont.) | `machine_context.go:486` (`applyComposableContinuation`) | O(d) + O(d) + O(w) | `DeepCopy` O(d) + `GraftContinuation` O(d) + winding O(w) | Continuation Copy Depth |
| `ForeignFunctionCall` | `operation_foreign_function_call.go:61` | O(?) unbounded | Arbitrary Go code | Embedder Responsibility (ctx) |

#### Bounded by Compile-Time Constants — No Limits Needed

| Operation | File | Complexity | Why Safe |
|-----------|------|-----------|----------|
| `BuildSyntaxList` | (operation_build_syntax.go) | O(n), n = stack pops | n is argument count at call site, set at compile time |
| `MakeCaseLambdaClosure` | (operation_case_lambda.go) | O(c), c = clauses | c is compile-time constant, typically 2–4 |
| `BindPatternVars` | `operation_syntax_case.go:131` | O(v), v = pattern vars | v is compile-time constant, typically < 20 |
| `StoreSyntaxCaseInput` | `operation_syntax_case.go:248` | O(input) for DatumToSyntax | One-time conversion; bounded by input already parsed |

#### Bounded by Other Limits — No Additional Limits Needed

| Operation | File | Complexity | Why Safe |
|-----------|------|-----------|----------|
| `RestoreContinuation` | (operation_restore.go) | O(stack) for evals copy | Stack depth bounded by `maxCallDepth` (when set) |
| `Push` (multiValues path) | (operation_push.go) | O(v), v = value count | Rare path (`call-with-values` only); v is result count |
| `Apply` (normal path) | (operation_apply.go) | O(args + params) | args = call-site arity; params typically 1–5 |
| `SaveContinuationOffsetImmediate` | (operation_save_continuation.go) | O(1) | Just captures pointers |
| `SyntaxCaseNoMatch` | `operation_syntax_case.go:181` | O(1) | Always errors immediately |

### Match VM (2 non-O(1) instructions)

| Instruction | File | Complexity | Status |
|-------------|------|-----------|--------|
| `ByteCodeDone` | `match.go` | O(1) in practice | Safe — checks immediate cdr + one lookahead |
| `ByteCodeSkipIfTailCount` | `match.go` | O(1) with cache | **Fixed in 766bce1** — tail count cached, decremented per iteration |

### Syntax Expansion Helpers (tree-walking)

| Function | File | Complexity | Called From |
|----------|------|-----------|------------|
| `addScopeToSyntax` | `operation_syntax_rules_transform.go:349` | O(tree) | `SyntaxPair.AddScope` |
| `addScopeToSyntaxSkipFreeIds` | `operation_syntax_rules_transform.go:249` | O(tree) | Template expansion |
| `mapSyntaxTree` | `internal/syntax/syntax_pair.go` | O(tree) | `SyntaxPair.AddScope` |
| `DatumToSyntaxValue` | `internal/schemeutil/` | O(tree) | `StoreSyntaxCaseInput` (conditional) |
| `expandSyntaxValue` | `internal/match/syntax_adapter.go:255` | O(template × reps) | `SyntaxMatcher.Expand` |

These are covered by the **Expand Steps** category — they are called as part of template expansion.

## Limit Categories

### Category 1: Match Steps

**What it caps:** Iterations of the match VM's bytecode dispatch loop.

**Enforcement point:** `Matcher.MatchSyntaxWithLiterals()` in `internal/match/match.go`, line 234+. The loop already has an `iterations` counter (line 234) and a batched `ctx.Done()` check every 1024 iterations (line 237). The limit check piggybacks on this existing counter, inside the same batch check:

```
if maxMatchSteps > 0 && iterations > maxMatchSteps {
    return ErrMatchStepsExceeded
}
```

**Opcodes covered:**
- `SyntaxCaseMatch.Apply()` → `matcher.Match()` → `matcher.MatchSyntaxWithLiterals()`
- `SyntaxRulesTransform.Apply()` → `clause.matcher.MatchWithBindingChecker()` → `matcher.MatchSyntaxWithLiterals()`

**Plumbing (Engine → Matcher):**

```
engineConfig.maxMatchSteps
  → Engine.maxMatchSteps
    → MachineContext.maxMatchSteps (new field, inherited by sub-contexts)
      → passed to SyntaxMatcher / Matcher at match call sites
```

The match package is in `internal/match/` and cannot import `machine/`. The limit must be passed as a parameter to `Match()` / `MatchSyntaxWithLiterals()`, or stored on the `Matcher` struct at construction time. Passing as a parameter to `MatchSyntaxWithLiterals()` is cleanest — it avoids storing engine config in the match package and keeps the Matcher stateless with respect to limits.

**API:**
```go
wile.WithMaxMatchSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrMatchStepsExceeded = NewStaticError("match step limit exceeded")`

### Category 2: Expand Steps

**What it caps:** Total recursive calls to `expandSyntaxValue()` (and the tree-walking helpers it calls) during a single template expansion.

**Enforcement point:** At the top of `SyntaxMatcher.expandSyntaxValue()` in `internal/match/syntax_adapter.go`, line 255+:

```
p.expandSteps++
if p.maxExpandSteps > 0 && p.expandSteps > p.maxExpandSteps {
    return nil, ErrExpandStepsExceeded
}
```

The counter resets at the start of each `Expand()` call (line 238).

**What counts as a step:** Each recursive call to `expandSyntaxValue`. This counts:
- Each node visited in the template tree
- Each iteration of ellipsis repetition
- Nested pair/vector expansion

This does NOT count steps in `addScopeToSyntax` / `addScopeToSyntaxSkipFreeIds` / `mapSyntaxTree`. These tree walks are proportional to the expansion output size, which is already bounded by the expand step count. If a tighter bound is needed later, scope-walking can be added to the same counter.

**Opcodes covered:**
- `SyntaxTemplateExpand.Apply()` → `sc.matcher.Expand()`
- `SyntaxRulesTransform.Apply()` → `clause.matcher.Expand()`

**Plumbing (Engine → SyntaxMatcher):**

```
engineConfig.maxExpandSteps
  → Engine.maxExpandSteps
    → MachineContext.maxExpandSteps (new field, inherited by sub-contexts)
      → set on SyntaxMatcher at construction / before Expand() call
```

Since `SyntaxMatcher` already holds state (`matcher`, `literalSyntax`, `bindingChecker`), adding `maxExpandSteps` and `expandSteps` fields is consistent.

**API:**
```go
wile.WithMaxExpandSteps(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrExpandStepsExceeded = NewStaticError("expand step limit exceeded")`

### Category 3: Continuation Copy Depth

**What it caps:** The number of continuation frames walked during `DeepCopy()` and `GraftContinuation()` when invoking a composable continuation.

**Enforcement point:** Inside `MachineContinuation.DeepCopy()` in `machine/machine_continuation.go`, line 164:

```
depth := 0
for current.parent != nil {
    depth++
    if maxContinuationCopyDepth > 0 && depth > maxContinuationCopyDepth {
        return nil, ErrContinuationCopyDepthExceeded
    }
    parentCopy := current.parent.Copy()
    current.parent = parentCopy
    current = parentCopy
}
```

`GraftContinuation()` (machine_context.go:1151) walks the same chain but does not copy — it just finds the bottom. Its cost is dominated by `DeepCopy`'s cost, so a single limit on `DeepCopy` suffices. If the deep copy succeeds, the graft is guaranteed to succeed within the same bound.

**This is distinct from `maxCallDepth`:**
- `maxCallDepth` limits how deep the *live* call stack grows during execution.
- `maxContinuationCopyDepth` limits how much work a single *continuation invocation* does when copying a captured continuation segment.

A captured composable continuation's segment length is bounded by the distance between two prompts, not by the total call depth. A program could have `maxCallDepth = 100` but capture a composable continuation spanning 50 frames, which then gets deep-copied on every invocation.

**Plumbing (Engine → DeepCopy):**

```
engineConfig.maxContinuationCopyDepth
  → Engine.maxContinuationCopyDepth
    → MachineContext.maxContinuationCopyDepth (new field, inherited by sub-contexts)
      → passed to DeepCopy() as parameter
```

`DeepCopy` is a method on `MachineContinuation`, which does not have access to `MachineContext`. The limit must be passed as a parameter. This changes the signature:

```go
func (p *MachineContinuation) DeepCopy(maxDepth uint64) (*MachineContinuation, error)
```

The call site in `applyComposableContinuation` (machine_context.go:486) passes `p.maxContinuationCopyDepth`.

**Note on winding:** `RestoreWithWindingFrom` (machine_context.go:821) calls `unwindStackTo` and `RewindTo`, which walk the winding stack. The winding stack depth is bounded by the number of `dynamic-wind` frames, which are pushed by user code. However, each unwind/rewind step *executes a thunk* (before/after procedure), which re-enters the VM loop — so the VM loop's own `ctx.Done()` check covers these. No additional limit needed for winding.

**API:**
```go
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

**Default:** 0 (unlimited).

**Error sentinel:** `values.ErrContinuationCopyDepthExceeded = NewStaticError("continuation copy depth exceeded")`

### ForeignFunctionCall: Embedder Responsibility

`ForeignFunctionCall` calls arbitrary Go code via the `ForeignFunction` signature:

```go
type ForeignFunction func(ctx context.Context, mc *MachineContext) error
```

The `ctx` parameter is already threaded through. The VM cannot impose a step limit on opaque Go code. This is documented as the embedder's responsibility:

- Use `context.WithTimeout` or `context.WithDeadline` to bound total execution time.
- Foreign functions that perform unbounded work (e.g., HTTP calls, file I/O, computation loops) must check `ctx.Done()` internally.
- The engine's built-in primitives (in `registry/core/`) already follow this contract — they receive `ctx` and delegate to Go standard library functions that respect context cancellation.

No new API surface is needed for this category.

## New Error Sentinels

Added to `values/foreign_error.go`, grouped with existing resource-exhaustion errors:

```go
// Resource-limit errors (grouped together)
ErrMatchStepsExceeded              = NewStaticError("match step limit exceeded")
ErrExpandStepsExceeded             = NewStaticError("expand step limit exceeded")
ErrContinuationCopyDepthExceeded   = NewStaticError("continuation copy depth exceeded")
```

All are `*StaticError` values, matchable via `errors.Is()`. All are wrapped with context at return sites via `values.WrapForeignErrorf()`.

## API Summary

```go
// Existing
wile.WithMaxCallDepth(n uint64) EngineOption

// New
wile.WithMaxMatchSteps(n uint64) EngineOption
wile.WithMaxExpandSteps(n uint64) EngineOption
wile.WithMaxContinuationCopyDepth(n uint64) EngineOption
```

All follow the same convention:
- 0 = unlimited (default)
- \> 0 = hard cap
- Exceeded → wrapped sentinel error, propagated as Scheme exception

## Plumbing Summary

All three new limits follow the same path as `maxCallDepth`:

```
EngineOption (options.go)
  → engineConfig field (options.go)
    → Engine field (engine.go)
      → MachineContext field (machine_context.go)
        → inherited by sub-contexts (NewSubContext, NewSubContextForThread)
          → passed to enforcement point (match/expand/deep-copy)
```

New fields on `MachineContext`:
- `maxMatchSteps uint64`
- `maxExpandSteps uint64`
- `maxContinuationCopyDepth uint64`

New methods on `MachineContext`:
- `MaxMatchSteps() uint64` / `SetMaxMatchSteps(n uint64)`
- `MaxExpandSteps() uint64` / `SetMaxExpandSteps(n uint64)`
- `MaxContinuationCopyDepth() uint64` / `SetMaxContinuationCopyDepth(n uint64)`

## Implementation Notes

### Match Steps: Parameter Passing

The `Matcher` is in `internal/match/`, which cannot import `machine/`. The cleanest approach is to add a `maxSteps` parameter to `MatchSyntaxWithLiterals()`:

```go
func (p *Matcher) MatchSyntaxWithLiterals(
    ctx context.Context,
    target *syntax.SyntaxPair,
    literalSyntax map[string]*syntax.SyntaxSymbol,
    literalMatcher LiteralMatcher,
    maxSteps uint64,  // 0 = unlimited
) error
```

The `SyntaxMatcher` wrapper passes the limit through.

### Expand Steps: Counter Lifecycle

The `expandSteps` counter on `SyntaxMatcher` resets at the start of each `Expand()` call. This means each template expansion gets a fresh budget. If a single `SyntaxRulesTransform` operation matches clause N and expands it, the expand budget applies to that one expansion.

### DeepCopy: Signature Change

`DeepCopy` gains a `maxDepth` parameter and returns `(*MachineContinuation, error)`. This is a breaking change to an internal API (the `machine` package is public but `DeepCopy` is only called from `applyComposableContinuation`).

### Testing Strategy

Each limit category needs:
1. A unit test that triggers the limit and verifies the correct error sentinel
2. A unit test that runs just under the limit and succeeds
3. An integration test via the Engine API (`WithMaxXxx`) with Scheme code that would otherwise run unbounded

The existing `TestWithMaxCallDepth` in `wile_test.go:904` is the template.

## Future Work

- **Default non-zero limits:** Once the limits are implemented and tested, consider changing defaults from 0 (unlimited) to sensible non-zero values. This is a separate decision that affects backward compatibility.
- **Scope-walking step counting:** If `addScopeToSyntax` tree walks prove to be a security concern independent of expand steps, they can be added to the expand step counter.
- **Macro expansion depth:** The expander (`ExpandExpression` in `expander_time_continuation.go:102`) recursively expands macro results without tracking depth. This is a *separate* concern from template expansion steps — it's about how many times a macro output is re-expanded, not how large a single expansion is. A `maxExpansionDepth` limit (tracking recursive re-expansion) may be needed as a fourth category.
