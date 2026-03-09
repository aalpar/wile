# Machine Package Technical Debt Reduction

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Eliminate confirmed structural debt in the `machine/` package — duplicated logic, oversized files, dead aliases, stale comments, divergent code paths.

**Architecture:** Bottom-up by dependency and risk. Quick wins first (zero behavioral risk), then localized extractions, then cross-cutting unifications that depend on earlier phases having settled.

**Tech Stack:** Go 1.23, project error conventions (`werr`), table-driven tests.

**Validated findings only.** All items below are confirmed against actual code via the staff-engineer assessment (2026-03-08). No speculative or hypothetical items.

---

## Phase 1: Quick Wins (S effort, zero risk) ✅

Isolated cleanups with no behavioral changes and no cross-file dependencies.

### Task 1.1: Remove `EffectiveOperations` backward-compat alias ✅

`EffectiveOperations()` is a one-line alias for `Operations()`. The comment says "backward compatibility." Every call site is in test files. Two names for the same thing violates "different names = different concepts."

**Files:**
- Modify: `machine/native_template.go` (delete lines 117-121)
- Modify: all test files calling `EffectiveOperations()` (rename to `Operations()`)

**Step 1: Find all call sites**

Run: `grep -rn 'EffectiveOperations' machine/ --include='*.go'`

**Step 2: Replace all calls**

Rename `EffectiveOperations()` to `Operations()` in every call site.

**Step 3: Delete the alias**

Remove the `EffectiveOperations` method and its comment from `native_template.go`.

**Step 4: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint`
Expected: PASS

---

### Task 1.2: Clean up stale TODO comments in syntax_rules_test.go ✅

Three identical TODOs at lines 99, 138, 176: "TODO: Add test for actually invoking the transformer once the API supports it." The transformer API works — syntax-rules is fully functional with macro expansion exercised across many test files.

**Files:**
- Modify: `machine/syntax_rules_test.go`

**Step 1: Read the test file around those lines**

Verify each TODO is genuinely stale (the API it references exists and works).

**Step 2: Either delete the TODOs or write the tests**

If the tests are trivial to add (the API clearly supports invocation), add them. If not, delete the TODOs with a comment referencing where transformer invocation is already tested (e.g., `hygiene_test.go`, `coverage_fullruntime_test.go`).

**Step 3: Verify**

Run: `go test -v ./machine/... -count=1 -run TestCompileSyntaxRules`
Expected: PASS

---

## Phase 2: Arity-Checking Deduplication (S effort, high value) ✅

The same arity check + argument binding logic appears in three places with identical structure. The binding loops have already started to diverge (`values.List` vs `mc.buildRestArg`).

**Locations:**
- `machine_context.go:Apply()` (lines ~431-482)
- `machine_context.go:applyForeign()` (lines ~497-526)
- `call_foreign_cached.go:callForeignCached()` (lines ~48-78)

### Task 2.1: Extract `checkArity` helper ✅

**Files:**
- Create: `machine/arity.go`
- Modify: `machine/machine_context.go` (Apply, applyForeign)
- Modify: `machine/call_foreign_cached.go` (callForeignCached)

**Step 1: Read all three arity-check blocks**

Verify they are structurally identical. Note any differences.

**Step 2: Define the helper**

```go
// checkArity validates that argCount satisfies the arity requirements.
// For non-variadic: argCount must equal paramCount.
// For variadic: argCount must be >= paramCount-1.
func checkArity(paramCount int, isVariadic bool, argCount int) error {
    if !isVariadic {
        if argCount != paramCount {
            return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
                "expected %d arguments, got %d", paramCount, argCount)
        }
        return nil
    }
    if argCount < paramCount-1 {
        return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
            "expected at least %d arguments, got %d", paramCount-1, argCount)
    }
    return nil
}
```

**Step 3: Extract `bindArgs` helper**

The argument binding loops also repeat. Extract:

```go
// bindArgs binds arguments to environment bindings.
// For variadic closures, the rest args (from index paramCount-1 onward)
// are collected using the provided restArgFn. If restArgFn is nil,
// values.List is used (the default for MachineClosure.Apply).
func bindArgs(
    bnds []environment.Binding,
    vs []values.Value,
    paramCount int,
    isVariadic bool,
    restArgFn func(vs []values.Value, start int) values.Tuple,
) {
```

The three call sites differ only in how rest args are constructed:
- `Apply`: `values.List(vs[l-1:]...)`
- `applyForeign`: `mc.buildRestArg(vs, l-1)`
- `callForeignCached`: `mc.buildRestArg(vs, l-1)`

The `restArgFn` callback captures this difference.

**Step 4: Replace all three sites**

Each site becomes:
```go
if err := checkArity(l, tpl.IsVariadic(), len(vs)); err != nil {
    return nil, werr.WrapForeignErrorf(err, ...)
}
bindArgs(bnds, vs, l, tpl.IsVariadic(), restArgFn)
```

**Step 5: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint && make covercheck`
Expected: PASS

**Caution:** The `applyCallableError` wrapping differs between sites. `Apply` does NOT wrap (returns raw error), while `callForeignCached` wraps via `applyCallableError`. Preserve this difference — the wrapping happens at a different layer.

---

## Phase 3: Extract Closure Compilation (S effort, medium value) ✅

`compile_validated.go` (980 lines) mixes core form compilation with closure/body infrastructure. Extract closure-related helpers to a dedicated file.

### Task 3.1: Create `compile_closure.go` ✅

**Files:**
- Create: `machine/compile_closure.go`
- Modify: `machine/compile_validated.go` (remove extracted functions)

**Step 1: Read compile_validated.go**

Identify the closure-related functions to extract:
- `compileClosureBody` (~59 lines) — shared closure compilation
- `compileClosure` (~16 lines) — emit MakeClosure bytecode
- `compileBody` (~19 lines) — lambda body with letrec* semantics
- `predeclareDefineBindingFromValidated` (~12 lines) — pre-declare define in body
- `bindRestParameter` (~35 lines) — bind variadic rest parameter
- `setScopesOnLastBinding` (~10 lines) — attach hygiene scopes

**Step 2: Move the functions to `compile_closure.go`**

All functions are methods on `*CompileTimeContinuation`. Move them as-is with their doc comments. Keep imports minimal.

**Step 3: Verify nothing in compile_validated.go references the moved functions internally**

All moved functions are either called from `CompileValidatedLambda`, `CompileValidatedCaseLambda`, or `CompileValidatedBegin` — these remain in `compile_validated.go` and call the moved methods through the receiver.

**Step 4: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint`
Expected: PASS

**Result:** `compile_validated.go` drops from ~980 to ~830 lines.

---

## Phase 4: Expander Decomposition (M effort, medium value) ✅

`expander_time_continuation.go` (1493 lines) is the second-largest production file. Extract body processing into a dedicated file.

### Task 4.1: Extract `expander_body.go` ✅

**Files:**
- Create: `machine/expander_body.go`
- Modify: `machine/expander_time_continuation.go` (remove extracted functions)

**Step 1: Read expander_time_continuation.go**

Identify the body-processing methods to extract:
- `ExpandBodyWithDefineSyntax` — top-level body expansion with define-syntax support
- `extractDefineName` — extract variable name from define forms
- `isDefineForm` (if it exists as a standalone method) — detect define forms
- Any private helpers called exclusively by the above

**Step 2: Verify extraction boundaries**

Check that the methods to be extracted don't have tight coupling to other methods in the file (e.g., shared local variables, closures over file-level state). They should be pure methods on `*ExpanderTimeContinuation`.

**Step 3: Move to `expander_body.go`**

Move with doc comments. Keep the file focused on body processing only.

**Step 4: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint`
Expected: PASS

**Result:** `expander_time_continuation.go` drops from ~1493 to ~1200 lines.

---

## Phase 5: Letrec* Body Processing Unification (M effort, high value) ✅

The expander and compiler both implement the same R7RS 5.3.2 letrec* algorithm independently. This is the most architecturally concerning debt — divergence here produces "works in the REPL but not in a library" bugs.

**Duplicate locations:**
- Expander: `ExpandBodyWithDefineSyntax` (expander_body.go after Phase 4)
- Compiler: `processFormsWithLetrecSemantics` (compile_time_continuation_include.go:186)
- Compiler: `compileBody` (compile_closure.go after Phase 3)

### Task 5.1: Design the unified pre-scan abstraction ✅

**Step 1: Read all three implementations side by side**

Document the exact algorithm each implements:
1. Pre-scan: walk forms, detect `define`/`define-syntax`, register placeholder bindings
2. Process: expand/compile each form sequentially
3. Handle define-syntax eagerly

**Step 2: Identify the variation points**

| Aspect | Expander | Compiler (include) | Compiler (body) |
|--------|----------|-------------------|-----------------|
| Input type | `syntax.SyntaxValue` | `syntax.SyntaxValue` | `validate.ValidatedExpr` |
| Name extraction | `extractDefineName` | `predeclareDefineBinding` | `predeclareDefineBindingFromValidated` |
| Binding creation | Syntax-scoped | Environment-scoped | Environment-scoped |
| Processing | `p.ExpandOnce()` | `p.CompileExpression()` | `p.compileValidated()` |

**Step 3: Design callback-based abstraction**

Create a generic pre-scan function that accepts callbacks:

```go
// LetrecPreScanner walks a sequence of forms, extracts define names via
// extractName, registers placeholder bindings via registerBinding, then
// processes all forms via processForm. This is the shared R7RS 5.3.2
// letrec* semantics used by lambda bodies, library bodies, and include files.
type LetrecPreScanner[Form any] struct {
    ExtractName     func(form Form) (*values.Symbol, bool)
    RegisterBinding func(sym *values.Symbol)
    ProcessForm     func(form Form, isTail bool) error
}

func (s *LetrecPreScanner[Form]) Process(forms []Form) error {
    // Pass 1: pre-declare
    for _, form := range forms {
        sym, ok := s.ExtractName(form)
        if ok {
            s.RegisterBinding(sym)
        }
    }
    // Pass 2: process
    for i, form := range forms {
        isTail := i == len(forms)-1
        if err := s.ProcessForm(form, isTail); err != nil {
            return err
        }
    }
    return nil
}
```

**Step 4: Evaluate whether the abstraction is worth it**

This is the critical step. If the generics introduce more complexity than they save, document the pattern in a shared comment block instead. The assessment's compile-file agent noted: "Type heterogeneity (ValidatedExpr vs. SyntaxValue) and environmental differences (local vs. global) make abstraction more complex than duplication." Verify this claim against actual code.

**Decision criteria:**
- If the unified function is < 30 lines and each call site is < 5 lines: unify
- If the unified function requires > 3 type parameters or > 2 interface abstractions: document instead

**Step 5: Implement (or document)**

If unifying: create `machine/letrec_semantics.go` with the scanner.
If documenting: add a shared comment block referencing all three locations.

**Step 6: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint && make covercheck`
Expected: PASS

---

## Phase 6: Library Import Path Unification (M effort, medium value) ✅

After shared import resolution, binding installation diverges into two paths:
- `CopyLibraryBindingsToEnvAtPhase` (top-level, routes through `env.AtPhase()`)
- `copyLibraryBindingsDirect` (library-internal, bypasses `AtPhase()`)

Both contain duplicated binding search logic (~20 lines each).

### Task 6.1: Extract `findLibraryBinding` ✅

**Files:**
- Modify: `machine/library_bindings.go`

**Step 1: Read both functions**

Compare `CopyLibraryBindingsToEnvAtPhase` (lines ~210-275) and `copyLibraryBindingsDirect` (lines ~277-333). Identify the shared binding search pattern:

```go
libSym := values.NewSymbol(internalName)
libBinding := lib.Env.GetBinding(libSym)
if libBinding == nil {
    expandEnv := lib.Env.Expand()
    if expandEnv != nil {
        libBinding = expandEnv.GetBinding(libSym)
    }
}
if libBinding == nil {
    compileEnv := lib.Env.Compile()
    if compileEnv != nil {
        libBinding = compileEnv.GetBinding(libSym)
    }
}
```

**Step 2: Extract `findLibraryBinding`**

```go
// findLibraryBinding searches the library's runtime, expand, and compile
// environments for a binding with the given internal name. Returns nil if
// no binding is found in any phase environment.
func findLibraryBinding(lib *CompiledLibrary, internalName string) *environment.Binding {
```

**Step 3: Replace both call sites**

Both `CopyLibraryBindingsToEnvAtPhase` and `copyLibraryBindingsDirect` call `findLibraryBinding` instead of inlining the search.

**Step 4: Verify**

Run: `go test -v ./machine/... -count=1`
Expected: PASS

Run: `make lint && make covercheck`
Expected: PASS

---

## Items NOT Included (Assessed and Deferred)

These were identified in the assessment but deferred for documented reasons:

| Finding | Why Deferred |
|---------|-------------|
| **Operation struct boilerplate** (272 lines across 8 zero-field ops) | Type safety and IDE navigability outweigh the boilerplate cost. The helper functions (`sameType`, `fieldMatches`) already consolidate comparison logic. Code generation is an option if more operations are added, but the current count (8) doesn't justify it. |
| **VMCounters.String() hand-unrolled** (25 format args) | Changes rarely. Reflection-based alternatives would be slower and harder to read. |
| **NativeTemplate literal dedup O(n) fallback** | Theoretical scaling wall for non-hashable values. No evidence of real-world impact. |
| **Operation naming verbosity** (e.g., `OperationLoadGlobalByGlobalIndexLiteralIndexImmediate`) | Names encode instruction format. Renaming would touch many test files for cosmetic benefit. |
| **MachineContext decomposition** (1669 lines) | Already tracked in TODO.md as "F10: MachineContext decomposition [Medium, Postponed]". Depends on other refactorings settling. |
| **CompileDefineLibrary callback pattern** | The `SetLibraryCallback` side-channel is a kludge but changing the return type requires coordinating loader and compiler signatures. Medium risk, low urgency. |

---

## Verification Checklist

After all phases:

```bash
make lint && make covercheck   # Must both pass
go test ./machine/... -count=1 # All machine tests pass
go test ./... -count=1         # Full suite passes
```

No behavioral changes in any phase. Every commit should be independently revertible.

---

## Phase Dependencies

```
Phase 1 (quick wins) ── no dependencies
Phase 2 (arity)      ── no dependencies
Phase 3 (closure)    ── no dependencies
Phase 4 (expander)   ── no dependencies
Phase 5 (letrec*)    ── depends on Phase 3 + Phase 4 (extraction sites must be stable)
Phase 6 (library)    ── no dependencies
```

Phases 1-4 and 6 can be executed in any order or in parallel. Phase 5 should wait for Phases 3 and 4 to land.
