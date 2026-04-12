# WalkSubExprs ChildRole Refinement

**Date**: 2026-04-05
**Status**: **Complete** — `ChildRole` enum and `WalkSubExprs` implemented.
**TODO.md ref**: Refactoring > High Priority > `WalkSubExprs for validated expression traversal`

## Problem

`WalkSubExprs` exists with `callPosition bool`. B2 escape analysis uses it.
B1 capture analysis duplicates the full 14-case type switch because it needs to
distinguish **closure bodies** (depth+1) from normal children (same depth).
The bool cannot express that distinction.

Two type switches walk the same tree with different concerns:
- B1 (`validate_capture.go`): 14 cases, tracks `depth int`, special-cases immediately-applied lambdas
- B2 (`validate_escape.go`): delegates to `WalkSubExprs`, only cares about call position

## Design

### ChildRole Enum

Replace `callPosition bool` with a `ChildRole` enum:

```go
type ChildRole int

const (
    RoleNormal      ChildRole = iota // default: args, inits, branches, body of begin/let
    RoleCallProc                     // operator of ValidatedCall / ValidatedApply
    RoleClosureBody                  // body of lambda, case-lambda, define-function
)
```

### Signature

```go
// Before
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, callPosition bool))

// After
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, role ChildRole))
```

### Role Assignments

| Form | Children | Role |
|------|----------|------|
| `ValidatedCall` | `Proc()` | `RoleCallProc` |
| `ValidatedCall` | args | `RoleNormal` |
| `ValidatedApply` | `Proc` | `RoleCallProc` |
| `ValidatedApply` | `PrefixArgs`, `FinalList` | `RoleNormal` |
| `ValidatedLambda` | `Body()` | `RoleClosureBody` |
| `ValidatedCaseLambda` | clause bodies | `RoleClosureBody` |
| `ValidatedDefine` (function) | `Body()` | `RoleClosureBody` |
| `ValidatedDefine` (variable) | `SubExp()` | `RoleNormal` |
| Everything else | all children | `RoleNormal` |

### Immediately-Applied Lambda

When a `ValidatedCall`'s proc is a lambda, the closure doesn't escape. B1 must
walk the lambda's body at the **current** depth, not depth+1.

This is a two-level concern (call position + lambda detection) in a one-level
walker. It stays as a **B1-specific post-check** in the `RoleCallProc` callback
rather than a 4th role, keeping the role set purely positional.

### B1 Capture Analysis Migration

The 14-case type switch (~90 lines) becomes a role-based callback (~30 lines):

- `RoleClosureBody` → recurse at `depth+1`
- `RoleCallProc` → check for immediately-applied lambda; if so, walk body at current depth; otherwise recurse normally
- `RoleNormal` → recurse at current depth
- `ValidatedSetBang` pre-check: `checkSymbol(Name, depth)` before WalkSubExprs (Name is `*SyntaxSymbol`, not `ValidatedExpr`)
- `ValidatedSymbol` leaf check: before WalkSubExprs (same as current)
- `walkBody` helper eliminated

### B2 Escape Analysis Migration

Minimal: replace `callPosition bool` with `role == RoleCallProc`.

### Testing

- Update `walk_sub_exprs_test.go`: assert `ChildRole` instead of `callPosition bool`
- Add tests for `RoleClosureBody` on lambda body, case-lambda clause body, define-function body
- Existing B1/B2 tests cover behavioral preservation (immediately-applied lambda, call-position escape suppression)
- No integration tests needed — internal refactoring only

## Decision: Immediately-Applied Lambda

**Considered**: Encoding it as a 4th role (`RoleImmediateCallBody`) or having WalkSubExprs "see through" immediately-applied lambdas.

**Rejected because**: The immediately-applied lambda optimization requires inspecting parent + child type together. WalkSubExprs is a one-level function. Encoding a two-level relationship either breaks the "direct children only" contract or makes the role of a child context-dependent (same lambda body reported differently depending on grandparent). The ~12-line post-check in B1 is the honest cost of a two-level concern.
