# WalkSubExprs ChildRole Refinement — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Replace `callPosition bool` with a `ChildRole` enum in `WalkSubExprs`, then migrate B1 capture analysis to use it — eliminating the duplicated 14-case type switch.

**Architecture:** Add `ChildRole` type with three values (`RoleNormal`, `RoleCallProc`, `RoleClosureBody`). Update `WalkSubExprs` signature and role assignments. Migrate B2 (trivial — bool→enum). Migrate B1 (14-case switch → role-based callback with immediately-applied lambda post-check).

**Tech Stack:** Go, internal `validate` package. No new dependencies.

**Design doc:** `plans/2026-04-05-walk-sub-exprs-design.md`

---

### Task 1: Add ChildRole type and update WalkSubExprs signature

**Files:**
- Modify: `internal/validate/walk_sub_exprs.go`

**Step 1: Add the ChildRole type above WalkSubExprs**

Add before the `WalkSubExprs` function:

```go
// ChildRole describes the structural position of a sub-expression within its
// parent validated form.
type ChildRole int

const (
	// RoleNormal is the default: arguments, init expressions, branch arms,
	// body of begin/let/dynamic-wind/with-continuation-mark, define-variable value.
	RoleNormal ChildRole = iota

	// RoleCallProc is the operator position of ValidatedCall and ValidatedApply.
	RoleCallProc

	// RoleClosureBody is a body expression inside a closure boundary:
	// ValidatedLambda, ValidatedCaseLambda clause, or ValidatedDefine (function form).
	RoleClosureBody
)
```

**Step 2: Update WalkSubExprs signature and role assignments**

Change signature from:
```go
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, callPosition bool))
```
to:
```go
func WalkSubExprs(expr ValidatedExpr, fn func(child ValidatedExpr, role ChildRole))
```

Update the body — every `fn(x, true)` becomes `fn(x, RoleCallProc)`, every `fn(x, false)` becomes either `RoleNormal` or `RoleClosureBody` per this mapping:

- `ValidatedCall`: `fn(e.Proc(), RoleCallProc)`, args → `fn(arg, RoleNormal)`
- `ValidatedApply`: `fn(e.Proc, RoleCallProc)`, prefix/final → `fn(x, RoleNormal)`
- `ValidatedLambda`: body → `fn(b, RoleClosureBody)`
- `ValidatedCaseLambda`: clause bodies → `fn(b, RoleClosureBody)`
- `ValidatedDefine` (IsFunction): body → `fn(b, RoleClosureBody)`
- `ValidatedDefine` (!IsFunction): `fn(e.SubExp(), RoleNormal)`
- All other forms: `fn(x, RoleNormal)`

Update the doc comment to describe roles instead of `callPosition`.

**Step 3: Verify it does not compile (callers use old signature)**

Run: `go build ./internal/validate/`
Expected: compile errors in `validate_escape.go` and `walk_sub_exprs_test.go`

---

### Task 2: Update B2 escape analysis

**Files:**
- Modify: `internal/validate/validate_escape.go`

**Step 1: Update the WalkSubExprs callback**

In `escapeWalker.walkExpr` (line ~98), change:
```go
WalkSubExprs(expr, func(child ValidatedExpr, callPosition bool) {
    if callPosition {
```
to:
```go
WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
    if role == RoleCallProc {
```

No other changes needed — B2 only cares about call position.

**Step 2: Verify escape analysis still doesn't compile alone (tests need updating)**

Run: `go build ./internal/validate/`
Expected: compile errors in `walk_sub_exprs_test.go` only

---

### Task 3: Update WalkSubExprs tests

**Files:**
- Modify: `internal/validate/walk_sub_exprs_test.go`

**Step 1: Update childEntry and collectChildren**

Change:
```go
type childEntry struct {
    expr         ValidatedExpr
    callPosition bool
}

func collectChildren(expr ValidatedExpr) []childEntry {
    var result []childEntry
    WalkSubExprs(expr, func(child ValidatedExpr, callPos bool) {
        result = append(result, childEntry{child, callPos})
    })
    return result
}
```
to:
```go
type childEntry struct {
    expr ValidatedExpr
    role ChildRole
}

func collectChildren(expr ValidatedExpr) []childEntry {
    var result []childEntry
    WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
        result = append(result, childEntry{child, role})
    })
    return result
}
```

**Step 2: Update all test assertions**

Replace all `callPosition` assertions with `role` assertions. The mapping:

| Test | Old assertion | New assertion |
|------|---------------|---------------|
| `TestWalkSubExprs_Call` | `children[0].callPosition, qt.IsTrue` | `children[0].role, qt.Equals, RoleCallProc` |
| `TestWalkSubExprs_Call` | `children[1].callPosition, qt.IsFalse` | `children[1].role, qt.Equals, RoleNormal` |
| `TestWalkSubExprs_Call` | `children[2].callPosition, qt.IsFalse` | `children[2].role, qt.Equals, RoleNormal` |
| `TestWalkSubExprs_Apply` | `children[0].callPosition, qt.IsTrue` | `children[0].role, qt.Equals, RoleCallProc` |
| `TestWalkSubExprs_Apply` | `children[1].callPosition, qt.IsFalse` | `children[1].role, qt.Equals, RoleNormal` |
| `TestWalkSubExprs_Apply` | `children[2].callPosition, qt.IsFalse` | `children[2].role, qt.Equals, RoleNormal` |
| `TestWalkSubExprs_Lambda` | `children[N].callPosition, qt.IsFalse` | `children[N].role, qt.Equals, RoleClosureBody` |
| `TestWalkSubExprs_CaseLambda` | `children[0].callPosition, qt.IsFalse` | `children[0].role, qt.Equals, RoleClosureBody` |
| `TestWalkSubExprs_DefineFunction` | `children[0].callPosition, qt.IsFalse` | `children[0].role, qt.Equals, RoleClosureBody` |
| `TestWalkSubExprs_DefineValue` | `children[0].callPosition, qt.IsFalse` | `children[0].role, qt.Equals, RoleNormal` |
| All `for _, ch := range` loops | `ch.callPosition, qt.IsFalse` | `ch.role, qt.Equals, RoleNormal` |
| `TestWalkSubExprs_Nil` | `func(child ValidatedExpr, callPos bool)` | `func(child ValidatedExpr, role ChildRole)` |

**Step 3: Run tests**

Run: `go test -v -run TestWalkSubExprs ./internal/validate/`
Expected: all 17 tests pass

**Step 4: Run full validate test suite**

Run: `go test -v ./internal/validate/`
Expected: all pass (B1 capture tests still use their own type switch, unchanged)

**Step 5: Commit**

```
refactor(validate): replace callPosition bool with ChildRole enum in WalkSubExprs

ChildRole has three values: RoleNormal (default), RoleCallProc (operator of
call/apply), RoleClosureBody (body of lambda/case-lambda/define-function).

B2 escape analysis updated to use role == RoleCallProc.
B1 capture analysis unchanged (migrated in next commit).
```

---

### Task 4: Migrate B1 capture analysis to use WalkSubExprs

**Files:**
- Modify: `internal/validate/validate_capture.go`

**Step 1: Replace captureWalker.walkExpr body**

Replace the current `walkExpr` method (lines 93-181, 14-case type switch) with:

```go
func (p *captureWalker) walkExpr(expr ValidatedExpr, depth int) {
	if expr == nil {
		return
	}
	// Leaf: check symbol reference.
	if sym, ok := expr.(*ValidatedSymbol); ok {
		p.checkSymbol(sym.Symbol, depth)
		return
	}
	// set! target: mutation from inside a closure also captures.
	if setBang, ok := expr.(*ValidatedSetBang); ok {
		p.checkSymbol(setBang.Name, depth)
	}

	WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
		switch role {
		case RoleClosureBody:
			p.walkExpr(child, depth+1)
		case RoleCallProc:
			// Immediately-applied lambda: walk body at current depth,
			// not depth+1, because the closure does not escape.
			switch proc := child.(type) {
			case *ValidatedLambda:
				for _, b := range proc.Body() {
					p.walkExpr(b, depth)
				}
			case *ValidatedCaseLambda:
				for _, clause := range proc.Clauses() {
					for _, b := range clause.Body() {
						p.walkExpr(b, depth)
					}
				}
			default:
				p.walkExpr(child, depth)
			}
		default:
			p.walkExpr(child, depth)
		}
	})
}
```

**Step 2: Delete the walkBody helper**

Remove:
```go
func (p *captureWalker) walkBody(body []ValidatedExpr, depth int) {
    for _, expr := range body {
        p.walkExpr(expr, depth)
    }
}
```

It is no longer called.

**Step 3: Run capture analysis tests**

Run: `go test -v -run TestMarkCaptured ./internal/validate/`
Expected: all pass

**Step 4: Run full validate test suite**

Run: `go test -v ./internal/validate/`
Expected: all pass

**Step 5: Run full project tests and lint**

Run: `make lint && make test`
Expected: clean

**Step 6: Commit**

```
refactor(validate): migrate B1 capture analysis to use WalkSubExprs

Replaces the 14-case type switch in captureWalker.walkExpr with a
role-based WalkSubExprs callback. Immediately-applied lambda detection
remains as a B1-specific post-check in the RoleCallProc handler.

90 lines → 30 lines. walkBody helper eliminated.
```

---

## Summary

| Task | What | Files changed |
|------|------|---------------|
| 1 | Add `ChildRole` enum, update `WalkSubExprs` signature + role assignments | `walk_sub_exprs.go` |
| 2 | Update B2 escape analysis (bool → role) | `validate_escape.go` |
| 3 | Update tests, verify, commit | `walk_sub_exprs_test.go` |
| 4 | Migrate B1 capture analysis, delete `walkBody`, verify, commit | `validate_capture.go` |

Two commits. Tasks 1-3 are one commit (signature change + all callers). Task 4 is the second commit (B1 migration).
