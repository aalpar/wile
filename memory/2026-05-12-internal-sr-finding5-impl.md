# `internal/` structural reduction — Finding 5: `WalkBindingRefs`

**Status**: ✅ **SHIPPED** — PR #740 (`feat/internal-sr-finding5`), merge commit `04d12140`.

Implementation plan for **Finding 5** of
`plans/2026-05-07-internal-structural-reduction.md`, deferred from PR #739's
Phases 1-5 batch.

## Decision

Per the parent plan: extract a **higher-order traversal** that walks a
`ValidatedExpr` tree yielding each `*syntax.SyntaxSymbol` reference along
with its structural role and closure-nesting depth. Both `markCaptured`
and `markEscaped` become small filters over the unified traversal; the
shared setup boilerplate (build idToIdx, optionally walk inits, walk body,
recurse with role/depth bookkeeping) lives in exactly one place.

The parent plan estimated S–M. Updated estimate: **S.** The two walkers
are 137 + 114 LOC; the unified traversal + thin wrappers should be ~150
LOC total, removing ~100 LOC of duplication.

## Existing state

Two walkers in `internal/validate/`:

- `validate_capture.go` (137 LOC): `markCapturedBindings` + `captureWalker`.
  Tracks closure depth. Marks `binding.Captured = true` when a tracked
  binding is referenced from inside a closure (depth > 0). Includes set!
  target references. Special-cases immediately-applied lambdas (lambda
  is the Proc of a Call) to NOT increment depth, since the closure
  doesn't escape.
- `validate_escape.go` (114 LOC): `markEscapedBindings` + `escapeWalker`.
  Marks `binding.Escapes = true` when a tracked binding is referenced
  outside the call-proc position. Does NOT track depth. Skips set!
  targets (mutation tracked by `Mutable`).

Both share:
- A setup phase: build `idToIdx map[BindingID]int`, return early if empty,
  optionally walk init expressions, walk body.
- A recursive walk over `WalkSubExprs` reacting to `ChildRole`
  (`RoleCallProc`, `RoleClosureBody`, `RoleNormal`).
- A binding-resolution lookup at each symbol leaf to map symbol → binding
  index → mutate `bindings[idx]`.

`markMutableBindings` (`validate_let.go:644-658`) is a sibling concept but
uses a different mechanism — it reads `result.isMutated` (populated
during validation) and never walks the tree. Per the parent plan, it
stays as-is.

## Design

### New file: `internal/validate/walk_binding_refs.go`

```go
// RefRole describes how a symbol reference is used.
type RefRole int

const (
    // RefInBody is a normal-position reference: argument, return value,
    // init expression, branch arm, sequence body, etc. The default.
    RefInBody RefRole = iota

    // RefInCallProc is the operator position of a ValidatedCall or
    // ValidatedApply (the "callee" slot).
    RefInCallProc

    // RefSetBangTarget is the target name of a ValidatedSetBang (the
    // symbol being mutated, not the value expression).
    RefSetBangTarget
)

// WalkBindingRefs walks expr recursively, calling visit for every
// *syntax.SyntaxSymbol reference encountered, with its role and closure-
// nesting depth.
//
// depth = number of escaping closure boundaries crossed (0 = same
// closure as expr). Immediately-applied lambdas (lambda as Proc of
// Call/Apply) do NOT increment depth — the closure does not escape.
//
// set!-target references: a ValidatedSetBang yields a synthetic
// RefSetBangTarget visit for its target name, then recurses into the
// value expression as a normal walk (RefInBody for the value's symbol
// references).
//
// ValidatedLambda / ValidatedCaseLambda / ValidatedDefine-function
// bodies increment depth by 1 unless the lambda is immediately applied.
func WalkBindingRefs(
    expr ValidatedExpr,
    visit func(sym *syntax.SyntaxSymbol, role RefRole, depth int),
)
```

`RefInClosureBody` from the parent plan's sketch is intentionally **omitted**:
it's redundant with `depth > 0`. Callers that want "is this inside a
closure?" check `depth > 0`; callers that want "what structural slot is
this?" check role. Conflating the two would force callers to consult
both fields for the same question.

### Rewritten consumers

`validate_capture.go`'s `markCapturedBindings` becomes:

```go
func markCapturedBindings(
    childEnv *environment.EnvironmentFrame,
    bindings []ValidatedLetBinding,
    body []ValidatedExpr,
    walkInits bool,
) {
    if childEnv == nil || len(bindings) == 0 {
        return
    }
    idToIdx := buildBindingIdxMap(childEnv, bindings)
    if len(idToIdx) == 0 {
        return
    }

    visit := func(sym *syntax.SyntaxSymbol, _ RefRole, depth int) {
        // Capture predicate: reference at depth > 0 captures (any role).
        if depth <= 0 {
            return
        }
        bid, ok := childEnv.ResolveBindingID(sym.Sym, sym.Scopes())
        if !ok {
            return
        }
        idx, found := idToIdx[bid]
        if !found {
            return
        }
        bindings[idx].Captured = true
    }

    if walkInits {
        for _, b := range bindings {
            WalkBindingRefs(b.Init, visit)
        }
    }
    for _, expr := range body {
        WalkBindingRefs(expr, visit)
    }
}
```

`validate_escape.go`'s `markEscapedBindings` becomes the analogous
filter, with predicate `role == RefInBody` (skip call-proc and set!-target).

A small shared helper `buildBindingIdxMap` (also in
`walk_binding_refs.go`) eliminates the last bit of duplication.

### Immediately-applied lambda optimization

Preserved inside `WalkBindingRefs`. When `WalkSubExprs` yields a child at
`RoleCallProc` and the child is a `*ValidatedLambda` or
`*ValidatedCaseLambda`, we walk its body at the **current** depth (not
depth+1). The escape consumer doesn't read depth, so this is invisible
to it; the capture consumer relies on it to avoid spuriously marking
captures in immediately-applied lambdas.

## Scope

| File | Change |
|------|--------|
| `internal/validate/walk_binding_refs.go` | NEW — `WalkBindingRefs`, `RefRole`, `buildBindingIdxMap` (~120 LOC) |
| `internal/validate/validate_capture.go` | Rewrite `markCapturedBindings`; delete `captureWalker` (137 → ~35 LOC) |
| `internal/validate/validate_escape.go` | Rewrite `markEscapedBindings`; delete `escapeWalker` (114 → ~30 LOC) |
| `internal/validate/walk_binding_refs_test.go` | NEW — small unit tests for the traversal (role + depth invariants, set!-target events, immediately-applied lambda depth handling) |

Net delta: ~100 LOC removed, ~120 LOC added including a new test file.

## Risk

- **Behavior preservation.** The capture and escape semantics must be
  exactly preserved. Mitigation: the existing integration tests in
  `validate_capture_test.go` and `validate_escape_test.go` (the only
  observable behavior of these walkers) cover the critical paths. If
  any test fails, the refactor failed.
- **Immediately-applied lambda corner cases.** The optimization is
  preserved as-is. Risk: subtle differences in how the walker reaches
  symbols inside immediately-applied lambdas. Mitigation: the existing
  capture test exercises this path.
- **set! target handling.** The synthetic `RefSetBangTarget` visit must
  fire before the walker recurses into the value expression. Mitigation:
  unit test that verifies a `(set! x y)` form yields exactly two visits:
  `(x, RefSetBangTarget, _)` and `(y, RefInBody, _)`.

## Phases

1. **Plan + branch.** Commit this plan file.
2. **New file: `walk_binding_refs.go`.** Define `RefRole`, `WalkBindingRefs`,
   `buildBindingIdxMap`. Build verifies (the new code is dead until
   consumers are migrated).
3. **Rewrite `validate_capture.go`.** Replace `captureWalker` with a
   visitor over `WalkBindingRefs`. Delete the obsolete struct + methods.
4. **Rewrite `validate_escape.go`.** Same for `escapeWalker`.
5. **New file: `walk_binding_refs_test.go`.** Unit tests pinning role
   + depth + set!-target invariants.
6. **Verify.** `make lint && make covercheck && make ci`.

## Commit cadence

1. `docs(plans): impl plan for internal/ SR finding 5 (WalkBindingRefs)`
2. `feat(validate): add WalkBindingRefs higher-order traversal`
3. `refactor(validate): rewrite markCapturedBindings on WalkBindingRefs`
4. `refactor(validate): rewrite markEscapedBindings on WalkBindingRefs`
5. `test(validate): unit-test WalkBindingRefs invariants`

Each commit builds and passes tests independently.
