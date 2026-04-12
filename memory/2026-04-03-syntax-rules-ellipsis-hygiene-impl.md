# syntax-rules Ellipsis and Hygiene Bug Fixes — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Fix three syntax-rules bugs that block SRFI-42 and violate R7RS conformance: scope-aware duplicate binding detection (A), cross-group ellipsis zipping (B), and nested ellipsis depth tracking (C).

**Architecture:** Three independent fixes in order A → B → C. Bug A is in the validator (`validate_let.go`). Bugs B and C are in the match package (`match.go`, `syntax_expand.go`, `syntax_compiler.go`, `syntax_adapter.go`). Each fix is independently testable and shippable.

**Tech Stack:** Go, `go-quicktest` (`qt`) for assertions, Wile's `internal/match` and `internal/validate` packages.

---

### Task 1: Bug A — Failing tests for scope-aware duplicate binding detection

**Files:**
- Test: `internal/validate/validate_test.go` (append new test)

**Step 1: Write the failing tests**

Add `TestLetDuplicateBindingWithDifferentScopes` to `validate_test.go`. The test creates
a `let` form with two bindings that have the same string key ("x") but different scope
sets. The current code rejects this; the test asserts it should be accepted.

Also add a test confirming genuine duplicates (same key, same scopes) still error.

Build the syntax by hand:
1. Create two `SyntaxSymbol`s with key "x" but different `SourceContext.Scopes`
2. Build `(let ((x_scope1 1) (x_scope2 2)) x_scope1)` as syntax
3. Validate — expect `Ok() == true`
4. Build `(let ((x 1) (x 2)) x)` with identical scopes — expect error

Use the existing test patterns in `validate_test.go`. The validate tests use
`ValidateExpression(ctx, env, expr)` as the entry point.

Consult: `internal/validate/validate_test.go` for existing test structure and helpers.
Consult: `internal/syntax/syntax_value.go` for `NewSyntaxSymbol`, `NewScope`, `SourceContext`.
Consult: `environment/environment_frame.go` for `NewNamespaceFrame`, `NewEnvironmentFrameWithParent`.

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestLetDuplicateBindingWithDifferentScopes ./internal/validate/...`
Expected: FAIL — "duplicate binding name" error reported for scope-distinct bindings.

---

### Task 2: Bug A — Implement scope-aware duplicate detection

**Files:**
- Modify: `internal/validate/validate_let.go` — functions at lines 390-401 and `checkDuplicateBindingNames` at lines 547-568

**Step 1: Create a scope fingerprint helper**

Add a helper function in `validate_let.go`:

```go
func scopeFingerprint(scopes []*syntax.Scope) string {
    if len(scopes) == 0 {
        return ""
    }
    ids := make([]uint64, len(scopes))
    for i, s := range scopes {
        ids[i] = s.ID()
    }
    slices.Sort(ids)
    var buf strings.Builder
    for i, id := range ids {
        if i > 0 {
            buf.WriteByte(',')
        }
        fmt.Fprintf(&buf, "%d", id)
    }
    return buf.String()
}
```

Check: Does `syntax.Scope` have an `ID()` method? Verify in `internal/syntax/syntax_value.go`.
If not, use the scope pointer address or the `String()` method as fingerprint.

**Step 2: Create a binding identity type**

```go
type bindingIdentity struct {
    key      string
    scopeKey string
}
```

**Step 3: Update the inline duplicate check (lines 390-401)**

Change from:
```go
seen := make(map[string]bool, len(nameSyms))
for _, ns := range nameSyms {
    key := ns.Sym.Key
    if seen[key] { ...
```

To:
```go
seen := make(map[bindingIdentity]bool, len(nameSyms))
for _, ns := range nameSyms {
    id := bindingIdentity{key: ns.Sym.Key, scopeKey: scopeFingerprint(ns.Scopes())}
    if seen[id] { ...
```

**Step 4: Update `checkDuplicateBindingNames` (lines 547-568)**

Same pattern: change `map[string]bool` to `map[bindingIdentity]bool`, build identity
from `b.Name.Sym.Key` and `scopeFingerprint(b.Name.Scopes())`.

**Step 5: Run tests**

Run: `go test -v -run TestLetDuplicateBindingWithDifferentScopes ./internal/validate/...`
Expected: PASS

Run: `go test -v ./internal/validate/...`
Expected: All existing tests still pass.

**Step 6: Run lint**

Run: `make lint`
Expected: Clean.

---

### Task 3: Bug B — Failing tests for cross-group ellipsis expansion

**Files:**
- Test: `internal/match/syntax_expand_test.go` (append new test)

**Step 1: Write the failing tests**

Add `TestSyntaxExpandCrossGroupEllipsis` to `syntax_expand_test.go`. Follow the
pattern from `TestSyntaxExpandEllipsis` (line 406).

Test cases:
1. **Two groups zipped:** Pattern `(_ (a ...) (b ...))`, template `((cons a b) ...)`,
   input `(_ (1 2 3) (10 20 30))` → expect `((cons 1 10) (cons 2 20) (cons 3 30))`.
   Build pattern and input as syntax. Compile with `CompileSyntaxPattern`. Match. Expand.
   Walk the result list and verify each element is a 3-element list `(cons N M)`.

2. **Mismatched counts:** Pattern `(_ (a ...) (b ...))`, input `(_ (1 2) (10 20 30))`.
   Expansion should return an error (repetition counts differ).

3. **Three groups:** Pattern `(_ (a ...) (b ...) (c ...))`, template `((list a b c) ...)`,
   input `(_ (1 2) (10 20) (100 200))` → expect `((list 1 10 100) (list 2 20 200))`.

The variables map must include all pattern vars: `{"a": {}, "b": {}, "c": {}}`.
The compiled pattern's `EllipsisVars` will have separate IDs for each group.

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestSyntaxExpandCrossGroupEllipsis ./internal/match/...`
Expected: FAIL — `b` (and `c`) not recognized as pattern variables during expansion.

---

### Task 4: Bug B — Implement cross-group ellipsis zipping

**Files:**
- Modify: `internal/match/match.go` — add `findMatchingEllipsisIDs` (plural)
- Modify: `internal/match/syntax_expand.go` — update `expandSyntaxEllipsis`

**Step 1: Add `findMatchingEllipsisIDs` to `match.go`**

Below existing `findMatchingEllipsisID` (line 540), add:

```go
func (p *Matcher) findMatchingEllipsisIDs(vars map[string]struct{}) []int {
    if p.ellipsisVars == nil {
        return []int{0}
    }

    // Collect and sort IDs for deterministic order.
    ids := make([]int, 0, len(p.ellipsisVars))
    for id := range p.ellipsisVars {
        ids = append(ids, id)
    }
    sort.Ints(ids)

    // Try single-ID match first (common case).
    for _, id := range ids {
        ellipsisVars := p.ellipsisVars[id]
        allFound := true
        for v := range vars {
            if _, ok := ellipsisVars[v]; !ok {
                allFound = false
                break
            }
        }
        if allFound {
            return []int{id}
        }
    }

    // Multi-group case: collect all IDs that contribute at least one variable.
    var contributing []int
    for _, id := range ids {
        ellipsisVars := p.ellipsisVars[id]
        for v := range vars {
            if _, ok := ellipsisVars[v]; ok {
                contributing = append(contributing, id)
                break
            }
        }
    }
    if len(contributing) == 0 {
        return nil
    }
    return contributing
}
```

**Step 2: Update `expandSyntaxEllipsis` in `syntax_expand.go`**

Replace the call to `findMatchingEllipsisID` (line 382) with `findMatchingEllipsisIDs`.

When the result has length > 1, add the zipping logic:

```go
matchingIDs := p.matcher.findMatchingEllipsisIDs(patternVarsInTemplate)
if len(matchingIDs) == 0 {
    return p.expandSyntaxValue(rest, ctx, ellipsisVars, opts)
}

if len(matchingIDs) == 1 {
    // Existing single-group path (unchanged).
    children := ctx.children[matchingIDs[0]]
    // ... existing code ...
} else {
    // Multi-group: zip child contexts from all contributing IDs.
    // Verify equal iteration counts.
    count := len(ctx.children[matchingIDs[0]])
    for _, id := range matchingIDs[1:] {
        if len(ctx.children[id]) != count {
            return nil, werr.WrapForeignErrorf(werr.ErrSyntax,
                "expandSyntaxEllipsis: ellipsis groups have different repetition counts (%d vs %d)",
                count, len(ctx.children[id]))
        }
    }

    // Build merged contexts.
    var results []syntax.SyntaxValue
    for k := 0; k < count; k++ {
        merged := &captureContext{
            bindings: make(map[string]syntax.SyntaxValue),
            children: make(map[int][]*captureContext),
        }
        for _, id := range matchingIDs {
            child := ctx.children[id][k]
            maps.Copy(merged.bindings, child.bindings)
            for cid, cchildren := range child.children {
                merged.children[cid] = append(merged.children[cid], cchildren...)
            }
        }
        // ... expand pattern with merged context (same as single-group body) ...
    }
    // ... combine results with rest (same as single-group tail) ...
}
```

Factor the shared expansion loop body and result-combining tail into a helper to avoid
duplicating the single-group and multi-group paths. Keep the single-group fast path
allocation-free by not building merged contexts when only one ID is returned.

**Step 3: Run tests**

Run: `go test -v -run TestSyntaxExpandCrossGroupEllipsis ./internal/match/...`
Expected: PASS

Run: `go test -v ./internal/match/...`
Expected: All existing tests still pass.

**Step 4: Run lint**

Run: `make lint`
Expected: Clean.

---

### Task 5: Bug B — Integration test via Wile eval

**Files:**
- Test: `integration/srfi42_test.go` (create new file) OR add to an existing integration test file

**Step 1: Write integration test**

Define the SRFI-42 macros needed for the cross-group repro (`:do`, `do-ec`, `do-ec:do`,
`ec-simplify`, `:list`, `:range`, `:integers`, `:parallel`, `:parallel-1`) as a Scheme
string. Use `Engine.EvalMultiple` to load them, then test:

```scheme
(list-ec (:parallel (:range i 3) (:list x '(a b c))) (list i x))
;; => ((0 a) (1 b) (2 c))
```

Check: look at existing integration tests in `integration/` for the pattern. If no
integration test directory exists, add a test in `stdlib/stdlib_test.go` or
`wile_test.go` instead.

**Step 2: Run test**

Run: `go test -v -run TestSRFI42CrossGroup ./integration/...`
Expected: PASS

---

### Task 6: Bug C — Failing tests for nested ellipsis expansion

**Files:**
- Test: `internal/match/syntax_expand_test.go` (append new test)

**Step 1: Write the failing tests**

Add `TestSyntaxExpandNestedEllipsis` to `syntax_expand_test.go`.

Test cases:
1. **Basic nested:** Pattern `(_ (a ...) ...)`, template `((list a ...) ...)`,
   input `(_ (1 2 3) (4 5))` → expect `((list 1 2 3) (list 4 5))`.

2. **Wrapped nested:** Pattern `(_ ((a b) ...) ...)`, template `((list (cons a b) ...) ...)`,
   input `(_ ((1 10) (2 20)) ((3 30)))` → expect
   `((list (cons 1 10) (cons 2 20)) (list (cons 3 30)))`.

3. **Empty outer:** Pattern `(_ (a ...) ...)`, input `(_)` → expect `()`.

**Step 2: Run tests to verify they fail**

Run: `go test -v -run TestSyntaxExpandNestedEllipsis ./internal/match/...`
Expected: FAIL — outer ellipsis produces empty list.

---

### Task 7: Bug C — Add depth tracking to pattern compiler

**Files:**
- Modify: `internal/match/syntax_compiler.go` — add `ellipsisDepth` field, track in `compileEllipsis`
- Modify: `internal/match/syntax_adapter.go` — add `EllipsisDepths` to `CompiledPattern` and `SyntaxMatcherOpts`
- Modify: `internal/match/match.go` — add `ellipsisDepths` field to `Matcher`

**Step 1: Add depth tracking to `SyntaxCompiler`**

In `SyntaxCompiler` struct (line 83), add:

```go
ellipsisDepth  int            // current nesting depth during compilation
ellipsisDepths map[int]int    // ellipsisID -> nesting depth
```

Initialize `ellipsisDepths: map[int]int{}` in `NewSyntaxCompilerWithEllipsis`.

**Step 2: Record depth in `compileEllipsis`**

In `compileEllipsis` (line 334), after assigning `ellipsisID`, record depth and
manage the depth counter:

```go
ellipsisID := vis.nextEllipsisID
vis.nextEllipsisID++
vis.ellipsisDepths[ellipsisID] = vis.ellipsisDepth
vis.ellipsisVars[ellipsisID] = collectCapturedVariables(vis, entry)

// The inner pattern is compiled with incremented depth.
vis.ellipsisDepth++
// ... existing code that compiles the inner pattern ...
vis.ellipsisDepth--
```

The exact placement depends on where `extractPatternBytecode` is called — the depth
increment must wrap the compilation of the *inner* pattern, not the loop structure.
Read `extractPatternBytecode` and `emitEllipsisLoop` to find the right insertion point.

**Step 3: Thread depth through `CompiledPattern` and `SyntaxMatcherOpts`**

Add `EllipsisDepths map[int]int` to `CompiledPattern` (line 183) and
`SyntaxMatcherOpts` (line 105). Update `CompileSyntaxPattern` (line 201) to populate
`EllipsisDepths` from the compiler.

**Step 4: Thread depth into `Matcher`**

Add `ellipsisDepths map[int]int` to `Matcher` struct (line 58). Populate in
`NewMatcherFull` or equivalent constructor. Update `NewSyntaxMatcher` to pass it
through.

**Step 5: Write a unit test for depth assignment**

```go
func TestEllipsisDepthTracking(t *testing.T) {
    // Pattern: (_ (a ...) ...) — inner depth 0, outer depth 1
    // Compile and check ellipsisDepths map
}
```

**Step 6: Run tests**

Run: `go test -v ./internal/match/...`
Expected: All existing tests still pass. Depth test passes.

---

### Task 8: Bug C — Depth-aware ID selection in template expansion

**Files:**
- Modify: `internal/match/match.go` — update `findMatchingEllipsisID` for depth awareness
- Modify: `internal/match/syntax_expand.go` — pass depth context through expansion

**Step 1: Update `findMatchingEllipsisID` for depth awareness**

The existing function selects the first ID (sorted, lowest) that contains all
variables. For nested ellipsis, multiple IDs may contain the same variable at
different depths. The function should prefer the **highest-depth** ID (outermost
in the pattern, which corresponds to the outermost `...` in the template being
processed first).

Add a depth-aware variant or modify the existing function:

```go
func (p *Matcher) findMatchingEllipsisID(vars map[string]struct{}) int {
    // ... existing single-group-all-vars check ...

    // When multiple IDs match all vars (nested case), prefer highest depth.
    // ... collect all matching IDs, return the one with max depth ...
}
```

The key insight: for nested ellipsis, the outer ID (depth 1) contains the same
variables as the inner ID (depth 0), because the outer ellipsis captures the entire
nested structure. `findMatchingEllipsisID` already finds ALL-vars matches. Among
multiple ALL-vars matches, pick the one with highest depth.

**Step 2: Track consumed ellipsis IDs during expansion**

In `expandSyntaxEllipsis`, after selecting an ID and iterating its children, the
recursive call into each child context must know that this ID has been "consumed"
at this template level. Pass the consumed ID set through `ellipsisVars` or a new
parameter so the inner `...` doesn't re-select the same ID.

This may require extending the `ellipsisVars` map to also track consumed IDs, or
adding a separate `consumedIDs` set to the expansion state.

**Step 3: Run tests**

Run: `go test -v -run TestSyntaxExpandNestedEllipsis ./internal/match/...`
Expected: PASS

Run: `go test -v ./internal/match/...`
Expected: All existing tests still pass.

**Step 4: Run lint**

Run: `make lint`
Expected: Clean.

---

### Task 9: Full regression — all tests and SRFI-42 end-to-end

**Files:**
- Test: integration test for SRFI-42 dispatch generator

**Step 1: Run full test suite**

Run: `make test`
Expected: All pass.

Run: `make lint && make covercheck`
Expected: Clean.

**Step 2: SRFI-42 end-to-end test**

Load the full SRFI-42 reference implementation (all macros + runtime) and run:

```scheme
(list-ec (: i 5) (* i i))
;; => (0 1 4 9 16)

(list-ec (:parallel (:range i 3) (:string ch "abc")) (list i ch))
;; => ((0 #\a) (1 #\b) (2 #\c))

(vector-of-length-ec 5 (:range i 5) (* i i))
;; => #(0 1 4 9 16)
```

This exercises all three bug fixes working together.

**Step 3: Run benchmarks**

Run: `make bench-gabriel`
Expected: No regressions. The common single-group path is unchanged.

---

### Task 10: Update plans and documentation

**Files:**
- Modify: `plans/SRFI-42-SYNTAX-BUGS.md` — mark bugs as fixed
- Modify: `plans/2026-04-03-syntax-rules-ellipsis-hygiene-design.md` — mark as complete
- Modify: `internal/match/CLAUDE.local.md` — update gotchas for cross-group and depth-aware expansion
