# Move Expansion Operations to compilation/ — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Move 4 expansion-support files (~720 source LOC) from `machine/` to `machine/compilation/`, completing the producer-consumer colocation described in the decomposition design.

**Architecture:** The expansion operations implement `machine.InlinedOperation` and are dispatched polymorphically by the VM through the side table. Moving them to compilation/ is safe because the VM never references their concrete types — only the interface. The bridge types (`SyntaxRulesClause`, etc.) are already created by compilation/ and consumed by these operations.

**Tech Stack:** Go 1.24, mechanical refactoring (package rename, export helpers, add accessors).

---

### Task 1: Export operation comparison helpers

The moving files use `sameType`, `fieldMatches`, and `sliceMatches` — unexported
generic helpers in `operation_helpers.go`. Export them so compilation/ can call them.

**Files:**
- Modify: `machine/operation_helpers.go`
- Modify: `machine/operation_helpers_test.go`

**Step 1: Rename the three helpers in operation_helpers.go**

In `machine/operation_helpers.go`, rename:
- `sameType` → `SameType` (line 28)
- `fieldMatches` → `FieldMatches` (line 47)
- `sliceMatches` → `SliceMatches` (line 87)

Update the doc comments to match (lines 19-27, 38-46, 78-86).

Leave `fieldMethodMatches` lowercase — not used by moving files.

**Step 2: Update all callers in machine/**

Mechanical rename across these files (26 callsites total):

`sameType` → `SameType` (16 callsites in staying files):
- `operation_cont_mark.go`: lines 66, 100, 132
- `operations_closure.go`: line 84
- `operations_control.go`: line 121
- `operations_winding.go`: lines 71, 104, 126
- `operations_stack.go`: lines 37, 54, 71, 91
- `operations_call.go`: lines 45, 104, 129
- `operations_load_store.go`: line 40

`FieldMatches` (10 callsites in staying files):
- `operations_control.go`: lines 44, 78, 104
- `operations_stack.go`: line 115
- `operations_closure.go`: line 128
- `operations_load_store.go`: lines 67, 94, 152, 209
- `operations_winding.go`: line 148

Also update the 3 callsites in moving files (they'll carry the new names):
- `operation_syntax_rules_transform.go`: line 225 (`SameType`)
- `operation_syntax_case.go`: lines 106, 192, 238, 269, 292 (`SameType`)
- `operation_syntax_case.go`: line 169 (`SliceMatches`)
- `operation_build_syntax.go`: line 72 (`FieldMatches`)

Update `operation_helpers_test.go` to use the new names: lines 77, 144, 200, 274.

**Step 3: Verify**

Run: `go build ./machine/...`
Expected: Clean build, no errors.

Run: `go test ./machine/ -run TestSameType -v && go test ./machine/ -run TestFieldMatches -v && go test ./machine/ -run TestSliceMatches -v`
Expected: All pass.

---

### Task 2: Add syntaxCaseState accessors to MachineContext

The `syntaxCaseState` type moves with `operation_syntax_case.go`. MachineContext
needs opaque accessors so compilation/ can store/retrieve it.

**Files:**
- Modify: `machine/machine_context.go`

**Step 1: Change the field type and add accessors**

In `machine/machine_context.go`, change line 83:
```go
// Before:
syntaxCase    *syntaxCaseState // per-context syntax-case expansion state; nil when not in syntax-case

// After:
syntaxCase    any // *compilation.syntaxCaseState; nil when not in syntax-case
```

Add accessor methods after the existing `ExpanderContext()` method (around line 872):
```go
// SyntaxCaseState returns the opaque syntax-case expansion state.
// Returns nil when not in a syntax-case expansion.
func (p *MachineContext) SyntaxCaseState() any {
	return p.syntaxCase
}

// SetSyntaxCaseState sets the opaque syntax-case expansion state.
func (p *MachineContext) SetSyntaxCaseState(v any) {
	p.syntaxCase = v
}
```

**Step 2: Update in-package references**

In `operation_syntax_case.go` (still in machine/ at this point), update
`ensureSyntaxCaseState` and all `mc.syntaxCase` direct accesses to use
the accessors:

- `ensureSyntaxCaseState`: change `mc.syntaxCase` reads/writes to
  `mc.SyntaxCaseState()` and `mc.SetSyntaxCaseState(state)`, with
  type assertion from `any`.
- `OperationSyntaxCaseMatch.Apply`: change `mc.syntaxCase` to
  accessor calls.
- `OperationBindPatternVars.Apply`: same.
- `OperationStoreSyntaxCaseInput.Apply`: same.
- `OperationClearSyntaxCaseInput.Apply`: same.

**Step 3: Verify**

Run: `go build ./machine/... && go test ./machine/ -run TestSyntaxCase -v`
Expected: Clean build, tests pass.

---

### Task 3: Move source files to compilation/

**Files:**
- Move: `machine/operation_syntax_rules_transform.go` → `machine/compilation/`
- Move: `machine/operation_syntax_case.go` → `machine/compilation/`
- Move: `machine/operation_build_syntax.go` → `machine/compilation/`
- Move: `machine/syntax_bridge_types.go` → `machine/compilation/`

**Step 1: Copy files and change package declaration**

```bash
cp machine/operation_syntax_rules_transform.go machine/compilation/
cp machine/operation_syntax_case.go machine/compilation/
cp machine/operation_build_syntax.go machine/compilation/
cp machine/syntax_bridge_types.go machine/compilation/
```

In each copied file, change `package machine` → `package compilation`.

**Step 2: Add machine import, qualify machine types**

In each moved file, add `"github.com/aalpar/wile/machine"` to imports and
prefix these references:
- `MachineContext` → `machine.MachineContext`
- `OperationBase` → `machine.OperationBase`
- `NewOperationBase(...)` → `machine.NewOperationBase(...)`
- `NewOperationBaseWithGoName(...)` → `machine.NewOperationBaseWithGoName(...)`
- `SameType(...)` → `machine.SameType(...)`
- `FieldMatches(...)` → `machine.FieldMatches(...)`
- `SliceMatches(...)` → `machine.SliceMatches(...)`

In `operation_syntax_rules_transform.go` specifically:
- The `envBindingChecker` uses `mc.ExpanderContext()` — already returns
  `machine.ExpanderCtx` interface, which is fine from compilation/.

In `operation_syntax_case.go` specifically:
- `ensureSyntaxCaseState` calls `mc.SyntaxCaseState()` and
  `mc.SetSyntaxCaseState(state)` — returns `any`, type-assert to
  `*syntaxCaseState` (now local to compilation/).

**Step 3: Remove machine. prefix from now-local types**

In the moved files, these types are now in the same package (compilation/):
- `SyntaxRulesClause` — no prefix needed
- `SyntaxCaseClause` — no prefix needed
- `ClausesWrapper` — no prefix needed
- `FreeIdResolution` — no prefix needed

**Step 4: Delete original files from machine/**

```bash
rm machine/operation_syntax_rules_transform.go
rm machine/operation_syntax_case.go
rm machine/operation_build_syntax.go
rm machine/syntax_bridge_types.go
```

**Step 5: Update compilation/ files that referenced machine.SyntaxRulesClause etc.**

These compilation/ files currently use `machine.SyntaxRulesClause` etc. — remove
the `machine.` prefix since the types are now local:

- `compile_syntax_rules.go`: `machine.SyntaxRulesClause` → `SyntaxRulesClause`,
  `machine.FreeIdResolution` → `FreeIdResolution`,
  `machine.ClausesWrapper` → `ClausesWrapper`,
  `machine.NewOperationSyntaxRulesTransform()` → `NewOperationSyntaxRulesTransform()`
- `compile_syntax_case.go`: `machine.SyntaxCaseClause` → `SyntaxCaseClause`,
  `machine.NewOperationStoreSyntaxCaseInput()` → `NewOperationStoreSyntaxCaseInput()`,
  `machine.NewOperationSyntaxCaseMatch()` → `NewOperationSyntaxCaseMatch()`,
  `machine.NewOperationClearSyntaxCaseInput()` → `NewOperationClearSyntaxCaseInput()`,
  `machine.NewOperationBindPatternVars()` → `NewOperationBindPatternVars()`,
  `machine.NewOperationSyntaxCaseNoMatch()` → `NewOperationSyntaxCaseNoMatch()`
- `compile_syntax_form.go`: `machine.NewOperationSyntaxTemplateExpand()` →
  `NewOperationSyntaxTemplateExpand()`,
  `machine.NewOperationBuildSyntaxList()` → `NewOperationBuildSyntaxList()`

**Step 6: Verify build**

Run: `go build ./machine/... && go build ./machine/compilation/...`
Expected: Clean build.

---

### Task 4: Move test files

**Files:**
- Move: `machine/operation_syntax_case_test.go` → `machine/compilation/`
- Move: `machine/operation_build_syntax_test.go` → `machine/compilation/`

**Step 1: Copy and update package**

```bash
cp machine/operation_syntax_case_test.go machine/compilation/
cp machine/operation_build_syntax_test.go machine/compilation/
rm machine/operation_syntax_case_test.go
rm machine/operation_build_syntax_test.go
```

Change `package machine` → `package compilation` (or `package compilation_test`
if they use `_test` suffix — check each file).

Update imports: add `machine` import where tests reference `machine.MachineContext`
or test helpers from machine/. Remove `machine.` prefix from types now local
to compilation/ (`SyntaxCaseClause`, `OperationSyntaxCaseMatch`, etc.).

**Step 2: Update compilation/ test files that reference moved types**

- `compilation/coverage_improvement_test.go`: references `machine.ClausesWrapper`,
  `machine.SyntaxRulesClause` — remove `machine.` prefix.
- `compilation/compile_syntax_form_test.go`: references
  `machine.OperationSyntaxTemplateExpand` — remove `machine.` prefix.

**Step 3: Check for remaining references in machine/ test files**

These machine/ test files reference the moved types — update to use
`compilation.` prefix:
- `machine/operation_test.go`
- `machine/hygiene_test.go`
- `machine/coverage_fullruntime_test.go`
- `machine/disassemble_test.go`
- `machine/case_lambda_closure_test.go`

Add `"github.com/aalpar/wile/machine/compilation"` import and prefix:
`compilation.OperationSyntaxRulesTransform`, `compilation.SyntaxRulesClause`, etc.

**Step 4: Run full test suite**

Run: `go test ./machine/... && go test ./machine/compilation/...`
Expected: All pass.

---

### Task 5: Lint, covercheck, cleanup

**Files:**
- Modify: Various (goimports fixes)

**Step 1: Run goimports on all changed files**

```bash
goimports -w machine/ machine/compilation/
```

**Step 2: Run lint**

Run: `make lint`
Expected: Clean.

**Step 3: Run covercheck**

Run: `make covercheck`
Expected: Pass.

**Step 4: Verify full build and tests**

Run: `make build && make test`
Expected: All green.

---

## Summary

| Step | What | Files changed | Risk |
|------|------|---------------|------|
| Task 1 | Export SameType/FieldMatches/SliceMatches | 12 files (mechanical rename) | Low |
| Task 2 | Add SyntaxCaseState accessors | 2 files (machine_context.go, operation_syntax_case.go) | Low |
| Task 3 | Move 4 source files, update imports | ~10 files | Medium (import wiring) |
| Task 4 | Move 2 test files, update test imports | ~8 files | Medium (test wiring) |
| Task 5 | Lint and verify | 0 new changes | None |
