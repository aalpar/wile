# Tech Debt April 2026 — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** 24/27 tasks complete

> **Completed:** Phases 1-5, 6.1, 6.3, 7.1, 8.1, 8.3, 8.5.
> **Incomplete:** Task 6.2 (context.TODO() → real contexts in 39 test files), Task 6.4 (add typeswitchlint to values/values.go guide comment).
> **Opportunistic:** Tasks 8.2, 8.4 (deferred — low priority).

**Goal:** Systematically resolve 22 tech debt items from `TECH-DEBT-2026-04.md` (Task 4.2 already complete).

**Architecture:** Each task is self-contained with TDD steps. Tasks within a phase can be done in any order unless noted. Phases follow the recommended execution order from the assessment: 1 → 6 → 2 → 4 → 3 → 5 → 7 → 8.

**Tech Stack:** Go 1.24, `go test`, `make lint`, `make covercheck`

**Corrections to assessment:** Task 5.1 states "All three concrete types already have these methods" for `Name()`/`Doc()` — **incorrect**. Only `ForeignClosure` has them. `MachineClosure` uses `Template().Name()`/`Template().Doc()`. `CaseLambdaClosure` gets name from first clause. The fix must ADD methods to `MachineClosure` and `CaseLambdaClosure`, not just promote existing ones.

---

## Phase 1: Silent Limits & Safety (S each)

### Task 1.1: Fix `uint16` source table index overflow

**Files:**
- Modify: `machine/native_template.go:34` (`sourceRefs []uint16`), `:245` (`internSource`)
- Modify: `machine/edit_plan.go:217` (`rewriteCode`)
- Test: `machine/native_template_test.go`

**Step 1: Write the failing test**

```go
func TestInternSourceBeyond65536(t *testing.T) {
	tpl := NewNativeTemplate()
	// Intern 65,537 distinct source contexts (index 0 is reserved for nil).
	for i := 1; i <= 65537; i++ {
		src := &syntax.SourceContext{
			File:   fmt.Sprintf("file%d.scm", i),
			Line:   i,
			Column: 1,
		}
		idx := tpl.internSource(src)
		qt.Assert(t, idx, qt.Equals, uint32(i))
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run TestInternSourceBeyond65536 ./machine/...`
Expected: FAIL — `uint16` truncation or compile error after type change

**Step 3: Change types**

In `machine/native_template.go`:
- Line 34: `sourceRefs []uint16` → `sourceRefs []uint32`
- Line 245: return type `uint16` → `uint32`
- Line 254: `idx := uint16(...)` → `idx := uint32(...)`

In `machine/edit_plan.go`:
- Line 217: `rewriteCode(code []Instruction, sourceRefs []uint16, edits []edit) ([]Instruction, []uint16)` → change both `[]uint16` to `[]uint32`
- Line 225: `newRefs := make([]uint16, 0, newLen)` → `[]uint32`

**Step 4: Run test to verify it passes**

Run: `go test -run TestInternSourceBeyond65536 ./machine/...`
Expected: PASS

**Step 5: Run full suite**

Run: `make lint && make test ./machine/...`

**Step 6: Commit**

```
fix: widen sourceRefs from uint16 to uint32 to prevent silent overflow
```

---

### Task 1.2: Add opcode round-trip exhaustiveness test

**Files:**
- Test: `machine/native_template_test.go`

The existing `instructionToOperation` is missing cases for 28 promoted opcodes (OpNullQ through OpDivTail, plus OpCar/Cdr/Add/Sub/etc.) — they fall through to `default: return nil`. The existing `operationToInstruction` only handles base operations, not peephole-emitted opcodes. The round-trip test should verify `instructionToOperation` returns non-nil for all opcodes that appear in compiled code.

**Step 1: Write the failing test**

```go
func TestOpcodeRoundTrip(t *testing.T) {
	// Every opcode value from OpInvalid+1 to opCount-1 must produce a
	// non-nil result from instructionToOperation, EXCEPT OpComplex
	// (which requires a side table entry and is not round-trippable)
	// and OpInvalid.
	for op := OpCode(1); op < opCount; op++ {
		if op == OpComplex {
			continue
		}
		name := opcodeTable[op].name
		if name == "" {
			t.Errorf("opcode %d has no name in opcodeTable", op)
			continue
		}
		instr := Instruction{Op: op, Arg: 0}
		result := instructionToOperation(instr)
		if result == nil {
			t.Errorf("instructionToOperation returned nil for %s (opcode %d)", name, op)
		}
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -run TestOpcodeRoundTrip ./machine/...`
Expected: FAIL — missing cases for promoted arithmetic/list opcodes

**Step 3: Add missing cases to `instructionToOperation`**

In `machine/native_template.go`, after the existing Wave 9 cases (line ~221), add cases for all remaining promoted opcodes. They decompose to `LoadCachedBinding` for test assertions, same as the existing EqQ/VectorQ/VectorRef cases:

```go
case OpNullQ, OpNullQTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpPairQ, OpPairQTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpCar, OpCarTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpCdr, OpCdrTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpAdd, OpAddTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpSub, OpSubTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpMul, OpMulTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpDiv, OpDivTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpNumLt, OpNumLtTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpNumLe, OpNumLeTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpNumGt, OpNumGtTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpNumGe, OpNumGeTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpNumEq, OpNumEqTail:
	return NewOperationLoadCachedBinding(instr.Arg)
case OpCons, OpConsTail:
	return NewOperationLoadCachedBinding(instr.Arg)
```

**Step 4: Run test to verify it passes**

Run: `go test -run TestOpcodeRoundTrip ./machine/...`
Expected: PASS

**Step 5: Run full suite**

Run: `make lint && make test ./machine/...`

**Step 6: Commit**

```
test: add opcode round-trip exhaustiveness test

Catches opcodes added to the const block but missing from
instructionToOperation. Also adds 28 missing promoted opcode
cases (NullQ through Div, plus tail variants).
```

---

### Task 1.3: Add extension list consistency test

**Files:**
- Test: new file `extension_consistency_test.go` (root package)

The two lists are in different packages (`options.go` in root, `internal/bootstrap/environment_tiny.go` in internal), so we test at the root level where both are accessible indirectly. `AllExtensions()` returns `[]EngineOption`; we need to extract the extension names.

**Step 1: Determine how to extract names**

Read the `AllExtensions()` function and the `WithExtension` option to understand how names are accessible. The test needs to compare names from both lists.

**Implementation note:** Since `allExtensions` in `internal/bootstrap` is not exported, the test may need to create an engine with `AllExtensions()` and check `eng.AvailableLibraries()` against the known list. Alternatively, add an exported `AllExtensionNames()` to bootstrap. The simpler approach: test that an engine built with `WithAllExtensions()` has all expected libraries, and that a bootstrap-initialized env has the same set.

**Step 2: Write the test**

```go
func TestExtensionListConsistency(t *testing.T) {
	// Build engine with AllExtensions and verify the set matches
	// what initializeEnvironment provides.
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithAllExtensions())
	qt.Assert(t, err, qt.IsNil)

	libs := eng.AvailableLibraries()
	// Extract (wile <name>) libraries
	wileLibs := make(map[string]bool)
	for _, lib := range libs {
		if len(lib) == 2 && lib[0] == "wile" {
			wileLibs[lib[1]] = true
		}
	}

	// These must match the extensions from AllExtensions().
	// If this test fails, one list has an extension the other doesn't.
	expected := []string{
		"io", "files", "math", "introspection", "eval",
		"namespace", "threads", "gointerop", "all", "system", "process",
	}
	for _, name := range expected {
		qt.Assert(t, wileLibs[name], qt.IsTrue,
			qt.Commentf("missing (wile %s) library", name))
	}
}
```

**Step 3: Run test to verify it passes** (this is a consistency guard, not TDD red-green)

Run: `go test -run TestExtensionListConsistency ./...`
Expected: PASS — lists are currently consistent

**Step 4: Run full suite**

Run: `make lint && make test ./...`

**Step 5: Commit**

```
test: add extension list consistency guard

Ensures AllExtensions() and bootstrap's allExtensions stay in sync.
```

---

### Task 1.4: Add eval stack size limit — COMPLETE (PR #636)

**Files:**
- Modify: `werr/werr.go` (new `ErrStackOverflow` sentinel)
- Modify: `options.go` (new `WithMaxStackSize(n uint64)` engine option)
- Modify: `machine/machine_context.go` (`checkStackSize()` helper, `maxStackSize` field)
- Modify: `machine/machine_context_subcontext.go` (propagate through `NewSubContext`, `NewThreadSubContext`)
- Modify: `internal/extensions/eval/prim_eval.go` (propagate through `PrimEval`/`PrimLoad`)

**Design:** Follows the `maxCallDepth` pattern exactly. `checkStackSize()` is called at 6 push opcodes (`OpPush`, `OpPushLiteral`, `OpPushGlobal`, `OpPushLocal`, `OpPushCachedBinding`, `OpUnpackListToStack`) in `Run()`. Opt-in only — zero = unlimited.

**Status:** Complete. PR #636. Design: `plans/2026-04-11-eval-stack-limit-design.md`.

---

## Phase 6: Dead Code & Cleanup (S each)

### Task 6.1: Delete `runtime/` package

**Files:**
- Delete: `runtime/runtime.go`, `runtime/doc.go`, `runtime/runtime_test.go`

**Step 1: Verify no imports**

Run: `grep -r '"github.com/aalpar/wile/runtime"' --include='*.go' .`
Expected: no matches (or only within `runtime/` itself)

**Step 2: Delete**

```bash
rm -rf runtime/
```

**Step 3: Run full suite**

Run: `make lint && make test ./...`

**Step 4: Commit**

```
chore: delete unused runtime/ package

Duplicated Engine API. Imported by zero packages.
```

---

### Task 6.2: Replace `context.TODO()` in test files

**Original scope (7 sites in test helpers):** COMPLETE — replaced in `machine/testutil/testutil.go`, `registry/testhelpers/pipeline_helpers.go`, `registry/testhelpers/helpers.go`. (Note: `machine/testutil` subsequently eliminated by Task 7.1.)

**Expanded scope (431 sites across 39 test files):** OPEN

The assessment expanded this from 7 helper sites to 431 test-file sites. Largest concentrations:
- `internal/parser/parser_coverage_test.go` (104)
- `internal/parser/parser_test.go` (62)
- `internal/validate/validate_test.go` (44)
- `internal/bootstrap/multithreading_test.go` (30)
- `registry/core/prim_io_test.go` (27)
- `internal/match/syntax_expand_test.go` (23)

**Step 1: Project-wide replacement**

```bash
find . -name '*_test.go' -exec sed -i '' 's/context\.TODO()/context.Background()/g' {} +
```

**Step 2: Run full suite**

Run: `make lint && make test ./...`

**Step 3: Commit**

```
chore: replace context.TODO() with context.Background() in test files

431 occurrences across 39 test files. context.TODO() signals "haven't
decided yet" — test code should use context.Background().
```

---

### Task 6.3: Fix receiver naming on production types

**Files:**
- Modify: `machine/pool_generic.go:146` (`m`→`p`)
- Modify: `machine/counters.go:92,99` (`c`→`p`)
- Modify: `environment/load_path_stack.go:52-103` (`s`→`p`)
- Modify: `ffi_wrapper.go:29` (`s`→`p`)
- Modify: `values/port_base.go:56-116` (`b`→`p`)
- Modify: `internal/syntax/syntax_value.go:110` (`b`→`p`)

**Step 1: Rename receivers one file at a time**

For each file, use editor rename (or careful find-replace within the method body only) to change the receiver name to `p`. Verify each file compiles before moving to the next.

**Step 2: Run full suite after each file**

Run: `make lint && make test ./...`

**Step 3: Commit**

```
style: normalize receiver names to p per project convention
```

---

### Task 6.4: Add `typeswitchlint` to value type guide (S)

**Files:**
- Modify: `values/values.go:86` (guide comment)

**Step 1: Add step 8 to the guide comment**

The current guide lists 7 steps for "ADDING A NEW VALUE TYPE". Add step 8:

```go
//  8. cmd/typeswitchlint/main.go — add to knownValueTypes (lint coverage)
```

Insert after the existing step 7 (`machine/native_template.go — if it can appear as a compile-time literal`).

**Step 2: Verify accuracy**

Read `cmd/typeswitchlint/main.go:46` and confirm `knownValueTypes` matches the current set of value types in the codebase.

**Step 3: Run lint**

Run: `make lint`

**Step 4: Commit**

```
docs: add typeswitchlint to "ADDING A NEW VALUE TYPE" guide

Step 8 was missing — a new value type added by following the guide
would silently escape lint coverage.
```

---

## Phase 2: File Resolution Unification (S-M) — COMPLETE

All three tasks resolved:
- **2.3**: Done in commit `19d14d39` — bootstrap preserves pre-configured resolver.
- **2.2**: Done in commit `c8cbdf57` — dead `FindLibraryFile` with `os.Stat` bypass deleted.
- **2.1**: Resolved without code change — stated problem was incorrect. `ResolveFile` is only called by `OSFileResolver`, and `FSFileResolver` has its own resolution logic. See assessment in `TECH-DEBT-2026-04.md`.

~~### Task 2.3: Fix `initializeEnvironment` resolver overwrite (S)~~

**Do this first — it's the simplest and unblocks testing for 2.1/2.2.**

**Files:**
- Modify: `internal/bootstrap/environment_tiny.go:136`

**Step 1: Write the failing test**

```go
func TestInitializeEnvironment_PreservesExistingResolver(t *testing.T) {
	// Set up a namespace with a custom FileResolver before bootstrap.
	// After initializeEnvironment, the custom resolver should still be set.
	ctx := context.Background()
	env, err := bootstrap.NewNamespaceFrameTiny(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Set a custom resolver
	customResolver := compilation.NewOSFileResolver(env)
	env.SetFileResolver(customResolver)

	// Re-initialize (simulating the overwrite scenario)
	// After fix, the resolver should remain the custom one.
	qt.Assert(t, env.Namespace().FileResolver(), qt.Equals, customResolver)
}
```

Note: The exact test structure depends on whether `initializeEnvironment` is exported or only called through `NewNamespaceFrameTiny`. Read the code to determine the right test approach. The key assertion is: if a resolver is already set, it should not be overwritten.

**Step 2: Apply the fix**

In `internal/bootstrap/environment_tiny.go`, change line 136 from:

```go
env.SetFileResolver(compilation.NewOSFileResolver(env))
```

to:

```go
if env.Namespace().FileResolver() == nil {
	env.SetFileResolver(compilation.NewOSFileResolver(env))
}
```

**Step 3: Check if the testutil workaround can be removed**

After the fix, the workaround at `machine/testutil/testutil.go:140-151` (the `SetLibraryEnvFactory` wrapper that restores the chain resolver) may be removable. Test with it removed. If tests pass, delete it. If not, investigate why it's still needed.

**Step 4: Run full suite**

Run: `make lint && make test ./machine/... ./internal/bootstrap/...`

**Step 5: Commit**

```
fix: only set OSFileResolver when no resolver is configured

Prevents initializeEnvironment from overwriting a previously
configured ChainFileResolver. Removes the testutil workaround.
```

---

### Task 2.1: Make `ResolveFile` delegate to `FileResolver` (M)

**Files:**
- Modify: `environment/resolve.go` (lines 26, 40, 58 — `os.Stat` calls)
- Test: `environment/resolve_test.go`

**Step 1: Read the current `ResolveFile` signature and callers**

Understand how `ResolveFile` is called and what parameters it receives. The fix needs to thread a `FileResolver` through without breaking callers.

**Design decision:** Add an optional `FileResolver` parameter, or look it up from the `LoadPathStack`'s namespace. The tech debt doc suggests both options. The cleaner approach is to add `FileResolver` as a parameter since `ResolveFile` is a pure function (easier to test, no hidden coupling).

**Step 2: Write a failing test using an `fs.FS`-backed resolver**

Create a test that:
1. Sets up an `fstest.MapFS` with a file
2. Creates an `FSFileResolver` from it
3. Calls `ResolveFile` with a path that exists in the virtual FS but NOT on disk
4. Asserts the file is found

Currently this test would fail because `ResolveFile` uses `os.Stat`.

**Step 3: Add FileResolver parameter to ResolveFile**

Replace the three `os.Stat` calls with `resolver.ResolveAndOpen` when a resolver is provided. Fall back to `os.Stat` when resolver is nil (backward compatibility for callers that don't have a resolver).

**Step 4: Update all callers**

Search for all call sites of `ResolveFile` and pass the resolver where available.

**Step 5: Run full suite**

Run: `make lint && make test ./environment/... ./machine/...`

**Step 6: Commit**

```
feat: make ResolveFile delegate to FileResolver

Embedders using WithSourceFS() without WithSourceOS() now resolve
files correctly through all code paths.
```

---

### Task 2.2: Fix library search `os.Stat` bypass (M, depends on 2.1)

**Files:**
- Modify: `machine/compilation/library_registry.go:293,303`

**Step 1: Write a failing test**

Create a test that:
1. Sets up an `fstest.MapFS` containing a `.sld` library file
2. Configures an engine with `WithSourceFS()` only (no `WithSourceOS()`)
3. Attempts to import the library
4. Asserts it succeeds

Currently fails because `FindLibraryFile` uses `os.Stat`.

**Step 2: Thread FileResolver to FindLibraryFile**

The `FindLibraryFile` method needs access to the namespace's `FileResolver`. Either pass it as a parameter or retrieve it from the namespace that the `LibraryRegistry` already references.

Replace the two `os.Stat` calls (lines 293, 303) with resolver-based stat checks.

**Step 3: Run full suite**

Run: `make lint && make test ./machine/...`

**Step 4: Commit**

```
fix: route library file discovery through FileResolver

FindLibraryFile no longer bypasses the resolver chain with os.Stat.
```

---

## Phase 4: Test Discipline (S-M)

### Task 4.1: Migrate error-string assertions to `errors.Is` (M)

**Files (prioritized):**
- `registry/helpers/args_test.go:72,81,90` (regex on Go type names)
- `machine/compilation/import_set_datum_test.go` (~16 sites)
- `environment/resolve_test.go:36-37`
- `ffi_test.go:348,1056,1085`
- `machine/continuation_winding_coverage_test.go:563,604,646,691`
- `machine/exception_escape_test.go:35-68`

**Approach:** Work file by file. For each assertion:

1. Identify what error condition is being tested
2. Find the matching sentinel in `werr/werr.go`
3. Replace the string assertion with `errors.Is(err, werr.ErrXxx)` or `errors.As`
4. Run the test to verify it still passes

**Priority order:**
1. `args_test.go` — 3 sites, type-name regexes most fragile
2. `import_set_datum_test.go` — 16 sites, highest volume
3. Remaining files — 9 sites total

**Step 1: Fix `args_test.go`**

Replace:
```go
c.Assert(err.Error(), qt.Matches, `vector-length: expected a vector but got \*values\.Integer.*`)
```
With:
```go
qt.Assert(t, errors.Is(err, werr.ErrNotAVector), qt.IsTrue)
```

Repeat for all 3 sites, using the appropriate sentinel.

**Step 2: Run test**

Run: `go test -run TestArg ./registry/helpers/...`

**Step 3: Fix `import_set_datum_test.go`**

For each of the 16 `qt.Contains` assertions on `.Error()`, identify the sentinel and replace. Many will use `werr.ErrSyntaxError` or similar.

**Step 4: Run test**

Run: `go test ./machine/compilation/...`

**Step 5: Fix remaining files** (`resolve_test.go`, `ffi_test.go`, `continuation_winding_coverage_test.go`, `exception_escape_test.go`)

**Step 6: Run full suite**

Run: `make lint && make test ./...`

**Step 7: Commit**

```
test: migrate error-string assertions to errors.Is

Replaces fragile .Error() string matching with sentinel checks
across 6 test files (~28 sites).
```

---

### Task 4.2: ALREADY COMPLETE

`engine_sandbox_test.go` already tests `DenyAll()` authorizer end-to-end for `(exit)`, `(get-environment-variable)`, `(open-input-file)`, and many more. 15+ test functions covering extension-level and fine-grained authorization.

---

### Task 4.3: Add rest-arg buffer aliasing regression test (S)

**Files:**
- Test: `machine/machine_context_apply_test.go` (or appropriate existing test file)

**Step 1: Write the regression test**

The test calls a variadic primitive that stores its rest-arg list, then calls it again with different args, and verifies the first stored list is intact.

```go
func TestRestArgBufferAliasing(t *testing.T) {
	// list is variadic and returns its rest-arg list directly.
	// Call it twice and verify the first result isn't corrupted.
	code := `
		(let ((first (list 1 2 3))
		      (second (list 4 5 6)))
		  (list first second))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.SchemeString(), qt.Equals, "((1 2 3) (4 5 6))")
}
```

Note: `PrimList` in `registry/core/prim_lists.go` copies the rest-arg spine. The test verifies this copy is correct. If `list` already copies, consider testing a primitive that might NOT copy. Read `prim_lists.go:29` to check.

**Step 2: Run test**

Run: `go test -run TestRestArgBufferAliasing ./...`
Expected: PASS (if `list` copies correctly) — this is a regression guard

**Step 3: Commit**

```
test: add rest-arg buffer aliasing regression test
```

---

### Task 4.4: Add `validate/` clause-level syntax-rules error tests (S)

**Files:**
- Modify: `internal/validate/validate_macro_test.go`

**Step 1: Read the validator code to identify error conditions**

Read `internal/validate/validate_macro.go:88-101` to see what clause-shape errors exist.

**Step 2: Add table-driven test cases**

```go
// Add to existing TestSyntaxRules_Errors or create new test
tcs := []struct {
	name string
	code string
}{
	{"non-list clause", `(define-syntax bad (syntax-rules () not-a-list))`},
	{"improper clause", `(define-syntax bad (syntax-rules () (pattern . template)))`},
	{"clause wrong element count", `(define-syntax bad (syntax-rules () ((a))))`},
	{"clause too many elements", `(define-syntax bad (syntax-rules () ((a) b c d)))`},
}
```

**Step 3: Run tests**

Run: `go test -run TestSyntaxRules ./internal/validate/...`

**Step 4: Commit**

```
test: add clause-shape error cases for syntax-rules validation
```

---

## Phase 3: Registration Unification (M each)

### Task 3.1: Unify syntax compiler dual registration (M)

**Files:**
- Modify: `machine/compilation/register.go:23-50`
- Modify: `machine/compilation/syntax_compilers_registry.go:38-65`

**Step 1: Identify all 19 syntax compiler entries**

Read both files and build a side-by-side comparison. They should have the same names mapping to the same functions.

**Step 2: Create a single source-of-truth slice**

```go
// syntaxCompilerEntries is the single source of truth for all syntax
// compiler registrations. Both the compilerRegistry (dispatch) and
// RegisterSyntaxCompilers (compile-time environment) derive from this.
var syntaxCompilerEntries = []struct {
	name string
	fn   SyntaxCompilerFunc
}{
	{"syntax", CompileSyntaxRules},
	{"syntax-case", CompileSyntaxCase},
	// ... all 19 entries
}
```

**Step 3: Rewrite both registration paths to use the shared slice**

The `init()` in `register.go` iterates `syntaxCompilerEntries` and wraps with `syntaxCompiler()`. `RegisterSyntaxCompilers` iterates the same slice and wraps with `NewSyntaxCompiler()`.

**Step 4: Add a test verifying both registrations produce the same set**

```go
func TestSyntaxCompilerRegistrationConsistency(t *testing.T) {
	// Verify the compilerRegistry and RegisterSyntaxCompilers
	// derive from the same entries.
	// ... compare names from both paths
}
```

**Step 5: Run full suite**

Run: `make lint && make test ./machine/...`

**Step 6: Commit**

```
refactor: unify syntax compiler dual registration into single source

Both compilerRegistry and RegisterSyntaxCompilers now derive from
syntaxCompilerEntries, eliminating drift risk.
```

---

### Task 3.2: Consolidate phase registration calls (M, depends on 3.1)

**Files:**
- Create: function in `machine/compilation/register.go`
- Modify: `engine.go:623`
- Modify: `internal/bootstrap/environment_tiny.go:116`
- Modify: `machine/testutil/testutil.go:236`

**Step 1: Create `RegisterAllPhaseHandlers`**

```go
// RegisterAllPhaseHandlers registers both syntax compilers and
// primitive expanders in the correct order.
func RegisterAllPhaseHandlers(env *environment.EnvironmentFrame) error {
	if err := RegisterSyntaxCompilers(env); err != nil {
		return err
	}
	return RegisterPrimitiveExpanders(env)
}
```

**Step 2: Replace all 3 call sites**

At each site, replace the two separate calls with one `RegisterAllPhaseHandlers(env)` call.

**Step 3: Run full suite**

Run: `make lint && make test ./...`

**Step 4: Commit**

```
refactor: consolidate phase registration into RegisterAllPhaseHandlers
```

---

## Phase 5: Missing Abstractions (S-M)

### Task 5.1: Add `Name()`/`Doc()` to `Closure` interface (M)

**Files:**
- Modify: `machine/closure.go` (interface)
- Modify: `machine/machine_closure.go` (add `Name()`, `Doc()` methods)
- Modify: `machine/case_lambda_closure.go` (add `Name()`, `Doc()` methods)
- Modify: `registry/core/prim_reflection.go` (simplify 6+ type switches)
- Modify: `internal/repl/meta.go` (simplify 3 type switches)
- Modify: `extensions/introspection/prim_disassemble.go` (simplify 1 type switch)

**Step 1: Add methods to `MachineClosure`**

```go
func (p *MachineClosure) Name() string {
	return p.template.Name()
}

func (p *MachineClosure) Doc() string {
	return p.template.Doc()
}
```

**Step 2: Add methods to `CaseLambdaClosure`**

```go
func (p *CaseLambdaClosure) Name() string {
	if len(p.clauses) > 0 {
		return p.clauses[0].Name()
	}
	return ""
}

func (p *CaseLambdaClosure) Doc() string {
	if len(p.clauses) > 0 {
		return p.clauses[0].Doc()
	}
	return ""
}
```

**Step 3: Extend the Closure interface**

```go
type Closure interface {
	values.Callable
	closureMarker()
	Name() string
	Doc() string
}
```

**Step 4: Verify compilation**

Run: `go build ./machine/...`

The interface change will cause compile errors if any type claims to implement `Closure` but lacks the new methods.

**Step 5: Simplify `PrimProcedureName` in `prim_reflection.go`**

Replace the 3-way type switch for name extraction:

```go
// Before: 3 cases extracting name differently
switch v := callable.(type) {
case *machine.MachineClosure: ...
case *machine.ForeignClosure: ...
case *machine.CaseLambdaClosure: ...
}

// After: single Closure interface check for name
if cls, ok := callable.(machine.Closure); ok {
	name := cls.Name()
	if name == "" {
		mc.SetValue(values.FalseValue)
	} else {
		mc.SetValue(values.NewString(name))
	}
	return nil
}
```

Apply the same pattern to `PrimProcedureDocumentation` and other name/doc switches.

**Keep type switches** where behavior genuinely differs between types (e.g., `PrimProcedureArity` needs clause access for `CaseLambdaClosure`).

**Step 6: Run full suite**

Run: `make lint && make test ./machine/... ./registry/core/... ./internal/repl/... ./extensions/introspection/...`

**Step 7: Commit**

```
refactor: add Name()/Doc() to Closure interface

Eliminates 9+ type switches that extracted name/doc from the
three closure types. CaseLambdaClosure delegates to first clause.
```

---

### Task 5.2: Add `SetStringOrFalse` helper (S)

**Files:**
- Modify: `registry/helpers/` (add helper, likely in an existing file like `value.go` or `args.go`)
- Modify: `registry/core/prim_reflection.go` (6 sites)
- Modify: `internal/extensions/eval/prim_eval.go` (2 sites)
- Modify: `registry/core/prim_syntax_loc.go` (5 sites)

**Step 1: Find the right file for the helper**

Look for where `BoolToBoolean` is defined — the new helper follows the same pattern.

**Step 2: Add the helper**

```go
// SetStringOrFalse sets the value register to a Scheme string if s is
// non-empty, or #f if s is empty. Follows the BoolToBoolean precedent
// for eliminating repeated if/else patterns.
func SetStringOrFalse(mc *machine.MachineContext, s string) {
	if s == "" {
		mc.SetValue(values.FalseValue)
	} else {
		mc.SetValue(values.NewString(s))
	}
}
```

**Step 3: Replace all sites**

Search for the pattern `if s == "" { mc.SetValue(values.FalseValue) } else { mc.SetValue(values.NewString(s)) }` and replace with `helpers.SetStringOrFalse(mc, s)`.

Note: After Task 5.1, some of these sites in `prim_reflection.go` may already be simplified. Apply `SetStringOrFalse` to whatever remains.

**Step 4: Run full suite**

Run: `make lint && make test ./registry/...`

**Step 5: Commit**

```
refactor: add SetStringOrFalse helper, replace 9+ sites
```

---

### Task 5.3: Replace raw `ForEach` with `MustList` (S)

**Files:**
- Modify: `registry/core/prim_exceptions.go`
- Modify: `registry/core/prim_control.go`
- Modify: `registry/core/prim_strings.go`

**Step 1: Read each file to find the raw `ForEach` calls**

Identify the exact pattern at each site.

**Step 2: Write failing tests**

For each site, write a test that passes an improper list and expects an error. Currently these silently accept improper lists.

```go
{"with-exception-handler improper list", `(with-exception-handler (lambda (e) e) '(1 . 2))`},
```

**Step 3: Replace `ForEach` with `MustList`**

At each site, replace `t.ForEach(ctx, fn)` with `helpers.MustList(t, fn, name)` (or whatever the exact `MustList` API is — read `registry/helpers/list.go:29` first).

**Step 4: Run tests to verify improper lists now error**

Run: `go test -run TestImproperList ./registry/core/...`

**Step 5: Run full suite**

Run: `make lint && make test ./registry/core/...`

**Step 6: Commit**

```
fix: enforce proper-list requirement per R7RS via MustList

Three primitives previously accepted improper lists silently.
```

---

### Task 5.4: Extract `requireSourceContext` helper (S)

**Files:**
- Modify: `registry/core/prim_syntax_loc.go`

**Step 1: Read the repeated pattern**

Lines 43-44, 60-61, 77-78, 94-95, 111-112 all share:
1. Extract syntax value from arg
2. Get source context
3. If nil → set `#f` and return

**Step 2: Extract the helper**

```go
// requireSourceContext extracts the SourceContext from mc.Arg(0).
// Returns (nil, true) if the source context is nil or empty, having
// already set mc to #f. Returns (sctx, false) on success.
func requireSourceContext(mc *machine.MachineContext, name string) (*syntax.SourceContext, bool) {
	sv, ok := mc.Arg(0).(*syntax.SyntaxValue)
	if !ok {
		// ... error handling
	}
	sctx := sv.SourceContext()
	if sctx == nil {
		mc.SetValue(values.FalseValue)
		return nil, true
	}
	return sctx, false
}
```

**Step 3: Rewrite all 5 functions to use the helper**

Each function becomes ~3-4 lines: call helper, check done, extract the specific field.

**Step 4: Run tests**

Run: `go test -run TestSyntax ./registry/core/...`

**Step 5: Commit**

```
refactor: extract requireSourceContext helper in prim_syntax_loc.go

Eliminates 5x copy-pasted guard pattern.
```

---

### Task 5.5: Complete `RequireArg[T]` migration (S)

**Files (8 remaining sites):**
- `registry/core/prim_syntax_loc.go:26` — `mc.Arg(0).(syntax.SyntaxValue)` — used inside `requireSourceContext` helper
- `registry/core/prim_reflection.go:271,340` — `mc.Arg(0).(*values.String)` — `doc-topic`, `apropos`
- `registry/core/prim_exceptions.go:32,37` — `mc.Arg(0/1).(values.Callable)` — `with-exception-handler` args
- `registry/core/prim_predicates.go:120,152` — `mc.Arg(0).(values.Number)` — `exact?`, `inexact?`
- `registry/core/prim_opaque.go:34` — `mc.Arg(0).(values.Opaque)` — `opaque-tag`

**Assessment:** Original count was 16 sites; 8 have been migrated. The remaining 8 fall into two categories:

1. **Predicate-style branches (leave as-is):** `prim_predicates.go` sites branch on `ok` for non-error logic (`exact?`/`inexact?` return `#f` for non-numbers). These aren't error paths — `RequireArg[T]` would be wrong here.

2. **Migratable:** `prim_reflection.go` (2), `prim_exceptions.go` (2), `prim_opaque.go` (1) follow the standard `if !ok { return error }` pattern compatible with `RequireArg[T]`.

3. **Edge case:** `prim_syntax_loc.go:26` asserts to `syntax.SyntaxValue` (interface, not pointer), which is inside the shared `requireSourceContext` helper. Could use `RequireArg[syntax.SyntaxValue]` if the generic constraint permits interfaces.

**Step 1: Migrate `prim_reflection.go` sites**

Replace lines 271 and 340:
```go
// Before:
s, ok := mc.Arg(0).(*values.String)
if !ok {
    return werr.WrapForeignErrorf(werr.ErrNotAString, ...)
}

// After:
s, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "doc-topic")
if err != nil {
    return err
}
```

**Step 2: Migrate `prim_exceptions.go` sites**

Replace lines 32 and 37 with `RequireArg[values.Callable]`.

**Step 3: Migrate `prim_opaque.go` site**

Replace line 34 with `RequireArg[values.Opaque]`.

**Step 4: Leave predicate sites**

`prim_predicates.go:120,152` — intentional non-error branching. Document as accepted deviation.

**Step 5: Evaluate `prim_syntax_loc.go` site**

Check if `RequireArg[syntax.SyntaxValue]` works (interface constraint). If yes, migrate. If not, leave with comment.

**Step 6: Run full suite**

Run: `make lint && make test ./registry/core/...`

**Step 7: Commit**

```
refactor: migrate remaining RequireArg[T] sites in registry/core

5 manual type-assertion sites migrated. 3 intentional predicate-style
branches left as accepted deviations (prim_predicates.go, prim_syntax_loc.go).
```

---

## Phase 7: Unify Test Helpers (L)

### Task 7.1: Unify `machine/testutil` and `registry/testhelpers`

**Files:**
- Modify: `machine/testutil/testutil.go`
- Modify: `registry/testhelpers/helpers.go`
- Modify: `registry/testhelpers/pipeline_helpers.go`
- Modify: many test files that import either package

**Key behavioral difference:** `testutil` uses `mc.Run()` (no escape handling), `testhelpers` uses `mc.RunWithEscapeHandling()`. Production code uses `RunWithEscapeHandling()`.

**This is the largest task. Break into sub-steps:**

**Step 1: Make `testutil` use `RunWithEscapeHandling()`**

Change `machine/testutil/testutil.go:183` from `mc.Run()` to `mc.RunWithEscapeHandling()`. Run all tests to see what breaks. This is the critical behavioral change.

Run: `make test ./...`

Fix any test failures caused by the escape handling change (these represent tests that were passing under non-production behavior).

**Step 2: Inventory both packages**

List all exported functions from both packages. Identify:
- Identical functions (merge)
- Unique to one package (keep)
- Similar but different (decide which to keep)

**Step 3: Consolidate into `testutil`**

Move unique functions from `testhelpers` to `testutil`. `testutil` is the better home because it handles library setup. Update all imports.

**Step 4: Delete `registry/testhelpers/`**

Once all imports are migrated, delete the package.

**Step 5: Run full suite**

Run: `make lint && make test ./...`

**Step 6: Commit**

```
refactor: unify machine/testutil and registry/testhelpers

All test eval paths now use RunWithEscapeHandling to match
production behavior.
```

---

## Phase 8: Architectural Improvements (M each, opportunistic)

These are lower priority. Include here for completeness but expect them to be picked up opportunistically.

### Task 8.1: Extract `machine/compilation/resolver/`

**Goal:** Move the 4 `FileResolver` implementations out of the 23K-line `compilation` package.

**Step 1:** Create `machine/compilation/resolver/` package.
**Step 2:** Move `OSFileResolver`, `FSFileResolver`, `EmbedFileResolver`, `ChainFileResolver` and their shared helpers.
**Step 3:** Update all imports (type alias in `compilation` for backward compat if needed).
**Step 4:** Run `make lint && make test ./machine/...`

---

### Task 8.2: Evaluate `wile.Value` wrapper utility

**This is a design decision, not a code change.** Two options:

**(a) Remove wrapper:** Expose `values.Value` directly. Breaking change but v1.x with zero consumers.
**(b) Enrich wrapper:** Add `AsInt64() (int64, bool)`, `AsList() []Value`, `AsString() (string, bool)`, etc.

**Recommendation:** Defer until there's a real embedder to inform the decision. If the user wants to proceed, option (a) is simpler.

---

### Task 8.3: ~~Fix `internal/repl` importing `machine/compilation`~~ [Done]

Resolved by PR #617 (public REPL API for embedders). `internal/repl` deleted; consumers migrated to public `repl/` package using Engine-level APIs.

---

### Task 8.4: Make `DefaultBigFloatPrecision` configurable (M)

**Files:**
- Modify: `values/big_float.go:32` (`DefaultBigFloatPrecision = 256`)
- Modify: `options.go` (new `WithBigFloatPrecision(bits uint)` engine option)
- Modify: 12 call sites across `values/big_float.go`, `values/big_complex.go`, `values/promotion.go`

**Design challenge:** `values/` is below `machine/` in the package layering. Threading config requires either:
- **(a)** A context-local value readable during promotion/construction
- **(b)** A field on `MachineContext` propagated to arithmetic helpers
- **(c)** A package-level default settable at engine init (simplest but not goroutine-safe)

**Step 1: Decide threading approach**

Read the 12 call sites to understand how precision flows. Most are in `values/` which cannot import `machine/`. Option (a) using `context.Context` is the most layering-friendly but adds overhead per allocation.

**Step 2: Add engine option**

```go
func WithBigFloatPrecision(bits uint) EngineOption {
    return func(cfg *engineConfig) error {
        cfg.bigFloatPrecision = bits
        return nil
    }
}
```

**Step 3: Thread precision to call sites**

Replace `DefaultBigFloatPrecision` references with the configured value at all 12 sites.

**Step 4: Run full suite**

Run: `make lint && make test ./values/... ./machine/...`

**Step 5: Commit**

```
feat: add WithBigFloatPrecision engine option

Allows embedders to configure BigFloat precision (default 256 bits).
```

---

### Task 8.5: Funnel `prim_eval.go` through `NewSubContext` — COMPLETE (PR #637)

**Files:**
- Modify: `machine/machine_context_subcontext.go` (new `NewSubContextWithTemplate` method)
- Modify: `internal/extensions/eval/prim_eval.go` (`PrimEval`, `PrimLoad`)
- Test: `machine/machine_context_test.go` (`TestNewSubContextWithTemplate`)

**Design:** `NewSubContextWithTemplate(tpl, env)` delegates to `NewSubContext()` then overrides `template` and `env`. Replaces 6-line manual `NewMachineContext` + field propagation in both `PrimEval` and `PrimLoad`. Pool-backed with explicit `ReleaseSubContext`. Eliminates the "forgotten field" bug class — `windingStack`, `parentMC`, `escapeCont`, `barrierValid` now propagate automatically.

**Status:** Complete. PR #637. Design: `plans/2026-04-11-eval-subcontext-design.md`.

---

## Execution Summary

| Order | Phase | Tasks | Effort | Status |
|-------|-------|-------|--------|--------|
| 1st | Phase 1 | 1.1, 1.2, 1.3, 1.4 | S each | **DONE** (1.1-1.3: ffa7b90a; 1.4: PR #636) |
| 2nd | Phase 6 | 6.1, 6.2, 6.3, 6.4 | S each | 6.1, 6.3 **DONE**; 6.2 partial (7/431 sites), 6.4 open |
| 3rd | Phase 2 | 2.3, 2.2, 2.1 | S, S, — | **DONE** (19d14d39, c8cbdf57; 2.1 N/A) |
| 4th | Phase 4 | 4.1, 4.3, 4.4 | M, S, S | **DONE** (25 sentinel migrations + 2 regression tests) |
| 5th | Phase 3 | 3.1 → 3.2 | M, M | **DONE** (8e7ef892, 69fdbd5f) |
| 6th | Phase 5 | 5.1, 5.2, 5.3, 5.4, 5.5 | M, S, S, S, S | **DONE** (5.5: 5 migrated, 3 intentional deviations) |
| 7th | Phase 7 | 7.1 | L | **DONE** (c82bbd5e) |
| 8th | Phase 8 | 8.1, 8.2, 8.3, 8.4, 8.5 | M each | 8.1, 8.3, 8.5 **DONE**; 8.2, 8.4 opportunistic |

**Total: 27 tasks** (original 22 + 5 added from reassessment: 1.4, 5.5, 6.4, 8.4, 8.5).

| Status | Count | Tasks |
|--------|-------|-------|
| Complete | 24 | 1.1-1.4, 2.1-2.3, 3.1-3.2, 4.1-4.4, 5.1-5.5, 6.1, 6.3, 7.1, 8.1, 8.3, 8.5 |
| Open | 2 | 6.2 (expanded), 6.4 |
| Opportunistic | 2 | 8.2, 8.4 |
