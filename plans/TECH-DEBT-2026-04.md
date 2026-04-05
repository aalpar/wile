# Technical Debt Assessment — April 2026

> Full-codebase assessment: 437K lines Go, 1695 files, 39 packages.
> All tests passing, median coverage ~87%.

**Themes:** (1) dual-path legacy from incremental evolution, (2) missing interfaces
forcing type-switch proliferation, (3) silent limits with no enforcement.

---

## Phase 1: Silent Limits & Safety (High priority, low effort)

Items that fail silently — wrong results, not crashes.

### Task 1.1: Fix `uint16` source table index overflow

**Files:** `machine/native_template.go` (lines 245–257, `internSource`; `sourceRefs []uint16`), `machine/edit_plan.go:217`
**Problem:** `internSource()` returns `uint16`. After 65,536 distinct source locations per template the index wraps to 0 silently — no bounds check, no panic, no error. Stack traces and error messages silently point to wrong source lines.
**Fix:** Change `sourceRefs []uint16` to `[]uint32` and return type of `internSource` to `uint32`. Update `edit_plan.go:217` `rewriteCode` which also uses `[]uint16`. Add a test that interns >65536 sources and verifies the index is correct.
**Effort:** S
**Verify:** `make lint && make test ./machine/...`

### Task 1.2: Add opcode round-trip exhaustiveness test

**Files:** `machine/native_template.go:126-229` (`instructionToOperation`), `machine/native_template.go:284-341` (`operationToInstruction`), `machine/opcode.go`
**Problem:** Two parallel hand-maintained switches over all opcodes. A new opcode added to the `const` block but missing from either switch returns `nil` / `(Instruction{}, false)` silently. No compile-time check for exhaustiveness.
**Fix:** Add `TestOpcodeRoundTrip` in `machine/native_template_test.go` that creates an `Instruction` for every opcode value `< opCount` and asserts `instructionToOperation` returns non-nil for all non-side-table opcodes.
**Effort:** S
**Verify:** `go test -run TestOpcodeRoundTrip ./machine/...`

### Task 1.3: Add extension list consistency test

**Files:** `options.go:311-325` (`AllExtensions()`), `internal/bootstrap/environment_tiny.go:67-79` (`allExtensions`)
**Problem:** Two lists of extensions — one for the public API, one for bootstrap — not derived from each other. A new extension added to one but not the other causes divergence between CLI and embedded engine behavior.
**Fix:** Add `TestExtensionListConsistency` that compares sorted extension names from both lists.
**Effort:** S
**Verify:** `go test -run TestExtensionListConsistency ./...`

---

## Phase 2: File Resolution Unification (High priority, medium effort)

The `FileResolver` abstraction was introduced for embedded filesystems but legacy `os.Stat`
paths were never migrated.

### Task 2.1: Make `ResolveFile` delegate to `FileResolver`

**Files:** `environment/resolve.go` (3 `os.Stat` calls at lines 26, 40, 58), `environment/file_resolver.go`
**Problem:** `ResolveFile()` uses `os.Stat` directly, bypassing `FileResolver`. Embedders using `WithSourceFS()` without `WithSourceOS()` silently fail to resolve files through this path.
**Fix:** Add a `FileResolver` parameter to `ResolveFile` (or retrieve it from the `LoadPathStack`'s owning namespace). When non-nil, delegate to `FileResolver.ResolveAndOpen` instead of `os.Stat`. Fall back to `os.Stat` only when no resolver is configured. Update all callers.
**Effort:** M
**Verify:** `make lint && make test ./environment/... ./machine/...`

### Task 2.2: Fix library search `os.Stat` bypass

**Files:** `machine/compilation/library_registry.go:293,303`
**Problem:** Library file discovery uses `os.Stat` directly instead of `FileResolver`.
**Fix:** Thread the `FileResolver` from the namespace through to `findLibraryFile`. Use `ResolveAndOpen` (from Task 2.1) or a stat-check wrapper.
**Effort:** M (depends on 2.1)
**Verify:** `make lint && make test ./machine/...`

### Task 2.3: Fix `initializeEnvironment` resolver overwrite

**Files:** `internal/bootstrap/environment_tiny.go` (in `initializeEnvironment`)
**Problem:** `initializeEnvironment` unconditionally sets an `OSFileResolver` on the namespace, overwriting any previously configured `ChainFileResolver`. The workaround in `machine/testutil/testutil.go:144-152` confirms this bites in practice.
**Fix:** Only set `OSFileResolver` when `namespace.FileResolver()` is nil.
**Effort:** S
**Verify:** `make lint && make test ./machine/... ./internal/bootstrap/...`

---

## Phase 3: Registration Unification (High priority, medium effort)

Three forms of registration drift that can cause silent bugs during feature work.

### Task 3.1: Unify syntax compiler dual registration

**Files:** `machine/compilation/register.go:23-50` (`init()` → `compilerRegistry`), `machine/compilation/syntax_compilers_registry.go:38-65` (`RegisterSyntaxCompilers()` → compile-time environment)
**Problem:** The same 19 syntax compiler entries appear in two separate registration mechanisms. Both lists must stay in sync but nothing cross-checks them. A form added to one but missed in the other silently breaks either dispatch or library export.
**Fix:** Build a single `[]struct{ name string; fn SyntaxCompilerFunc }` slice that feeds both registration paths. The `compilerRegistry` wraps each entry with the `syntaxCompiler()` adapter; `RegisterSyntaxCompilers` wraps with `NewSyntaxCompiler()`. Eliminate the `SyntaxCompilerFunc` / `CompilerFunc` adapter boilerplate.
**Effort:** M
**Verify:** `make lint && make test ./machine/...`

### Task 3.2: Consolidate phase registration calls

**Files:** `engine.go:623`, `internal/bootstrap/environment_tiny.go:116`, `machine/testutil/testutil.go:236`
**Problem:** `RegisterSyntaxCompilers` and `RegisterPrimitiveExpanders` are called independently at 3+ engine-init sites. Tests can miss one registration call and still partially work.
**Fix:** Create a single `RegisterAllPhaseHandlers(env)` that calls both in the correct order. Replace all call sites. Add `VerifyAllPhaseHandlers()` that cross-checks expanders, syntax compilers, and form validators.
**Effort:** M (depends on 3.1)
**Verify:** `make lint && make test ./...`

---

## Phase 4: Test Discipline (Medium priority, medium effort)

Tests that assert on error message strings rather than sentinels, and missing
integration tests.

### Task 4.1: Migrate error-string assertions to `errors.Is`

**Files:** `registry/helpers/args_test.go:72,81,90`, `machine/compilation/import_set_datum_test.go` (~15 sites), `environment/resolve_test.go:36-37`, `ffi_test.go:348,1056,1085`, `machine/continuation_winding_coverage_test.go:563,604,646,691`, `machine/exception_escape_test.go:35-68`
**Problem:** Tests assert on `.Error()` string content or regex containing Go type names. Reformatting error messages (cosmetic) breaks tests; genuinely incorrect sentinels go undetected.
**Fix:** Replace `err.Error()` contains/matches checks with `errors.Is(err, werr.ErrSomeSentinel)`. Prioritize `args_test.go` (type-name regexes) and `import_set_datum_test.go` (most sites).
**Effort:** M
**Verify:** `make test ./...`

### Task 4.2: Add security gate integration tests

**Files:** `extensions/system/prim_system.go:63-69,108-114`, `extensions/files/prim_files.go`, `engine_sandbox_test.go`
**Problem:** No test creates an engine with `DenyAll()` authorizer and verifies that system/file extension primitives are actually blocked end-to-end.
**Fix:** Add tests in `engine_sandbox_test.go` calling `(exit)`, `(get-environment-variable "PATH")`, and `(open-input-file "/etc/passwd")` with a `DenyAll()` authorizer, verifying `ErrAccessDenied`.
**Effort:** S
**Verify:** `go test -run TestSandbox ./...`

### Task 4.3: Add rest-arg buffer aliasing regression test

**Files:** `machine/machine_context_apply.go:165-181`, `registry/core/prim_lists.go:29`
**Problem:** The rest-arg list is backed by a reusable `PairBlock`. No test verifies that storing a rest-arg list across calls doesn't corrupt on the next variadic call.
**Fix:** Write a test that calls a variadic primitive storing `Arg(N)`, calls it again with different arguments, and verifies the first stored list is intact.
**Effort:** S
**Verify:** `go test -run TestRestArgBuffer ./machine/...`

### Task 4.4: Add `validate/` clause-level syntax-rules error tests

**Files:** `internal/validate/validate_macro.go:88-101`, `internal/validate/validate_macro_test.go`
**Problem:** `TestSyntaxRules_Errors` has only one case ("missing literals list"). The clause-shape errors (non-list clause, improper clause, wrong element count) have zero test coverage.
**Fix:** Add 3-4 table-driven test cases for clause-shape errors.
**Effort:** S
**Verify:** `go test -run TestSyntaxRules ./internal/validate/...`

---

## Phase 5: Missing Abstractions (Medium priority, medium effort)

Interfaces and helpers that would eliminate type-switch proliferation and
hand-unrolled patterns.

### Task 5.1: Add `Name()`/`Doc()` to `Closure` interface

**Files:** `machine/closure.go` (interface), `machine/machine_closure.go`, `machine/foreign_closure.go`, `machine/case_lambda_closure.go`, `registry/core/prim_reflection.go` (6 switches), `internal/repl/meta.go` (3 switches), `extensions/introspection/prim_disassemble.go` (1 switch)
**Problem:** Nine type switches over the same three closure types extracting name and documentation. Every new callable type requires updating all 9+ sites.
**Fix:** Add `Name() string` and `Doc() string` to the `Closure` interface. All three concrete types already have these methods. Update `prim_reflection.go`, `meta.go`, and `prim_disassemble.go` to use the interface methods instead of type switches. Keep type switches only where type-specific behavior differs (e.g., `CaseLambdaClosure` template access).
**Effort:** M
**Verify:** `make lint && make test ./machine/... ./registry/core/... ./internal/repl/... ./extensions/introspection/...`

### Task 5.2: Add `SetStringOrFalse` helper

**Files:** `registry/helpers/` (new helper), `registry/core/prim_reflection.go` (6 sites), `internal/extensions/eval/prim_eval.go` (2 sites), `registry/core/prim_syntax_loc.go` (5 sites)
**Problem:** `if s == "" { SetValue(FalseValue) } else { SetValue(NewString(s)) }` appears 9+ times. `BoolToBoolean` established the precedent for this kind of helper.
**Fix:** Add `func SetStringOrFalse(mc *machine.MachineContext, s string)` to `registry/helpers/`. Replace all applicable sites.
**Effort:** S
**Verify:** `make lint && make test ./registry/...`

### Task 5.3: Replace raw `ForEach` with `MustList` for proper-list enforcement

**Files:** `registry/core/prim_exceptions.go`, `registry/core/prim_control.go`, `registry/core/prim_strings.go`, `registry/helpers/list.go:29` (`MustList`)
**Problem:** These primitives call `t.ForEach(ctx, fn)` directly without the improper-list guard that `MustList` provides. They silently accept improper lists where R7RS requires errors.
**Fix:** Replace raw `ForEach` with `MustList` at the 3 affected sites.
**Effort:** S
**Verify:** `make lint && make test ./registry/core/...`

### Task 5.4: Extract `requireSourceContext` helper in `prim_syntax_loc.go`

**Files:** `registry/core/prim_syntax_loc.go` (5 functions with identical guard)
**Problem:** Five functions share identical steps 1 and 2 (require syntax value, check source context nil → return `#f`). Step 2 is copy-pasted in each.
**Fix:** Extract `requireSourceContext(mc, sv, name) (*SourceContext, bool)` returning `(nil, true)` when source context is nil (having already set `FalseValue`). Each function then calls this helper and dispatches on the accessor.
**Effort:** S
**Verify:** `make lint && make test ./registry/core/...`

---

## Phase 6: Dead Code & Cleanup (Medium priority, low effort)

### Task 6.1: Delete `runtime/` package

**Files:** `runtime/runtime.go`, `runtime/doc.go`, `runtime/runtime_test.go`
**Problem:** Exports `Compile`, `Run`, and `Load` duplicating Engine API. Imported by zero packages. Doc comments reference internal API.
**Fix:** Delete the entire `runtime/` directory.
**Effort:** S
**Verify:** `make lint && make test ./...`

### Task 6.2: Replace `context.TODO()` in test helpers

**Files:** `machine/testutil/testutil.go` (5 sites), `registry/testhelpers/pipeline_helpers.go` (1 site), `registry/testhelpers/helpers.go` (1 site)
**Problem:** `context.TODO()` signals "haven't decided yet" — the correct answer in test helpers is `context.Background()`.
**Fix:** Replace all 7 `context.TODO()` with `context.Background()`.
**Effort:** S
**Verify:** `make lint && make test ./machine/testutil/... ./registry/testhelpers/...`

### Task 6.3: Fix receiver naming on production types

**Files:** `machine/pool_generic.go:146` (`m`→`p`), `machine/counters.go:92,99` (`c`→`p`), `environment/load_path_stack.go:52-103` (`s`→`p`), `ffi_wrapper.go:29` (`s`→`p`), `values/port_base.go:56-116` (`b`→`p`), `internal/syntax/syntax_value.go:110` (`b`→`p`)
**Problem:** Six production types use non-`p` receivers, violating the project convention.
**Fix:** Rename receivers to `p`. Use `gofmt` or editor rename to avoid mistakes.
**Effort:** S
**Verify:** `make lint && make test ./...`

---

## Phase 7: Unify Test Helpers (Medium priority, medium effort)

### Task 7.1: Unify `machine/testutil` and `registry/testhelpers`

**Files:** `machine/testutil/testutil.go`, `registry/testhelpers/helpers.go`, `registry/testhelpers/pipeline_helpers.go`
**Problem:** Two test helper packages provide near-identical pipelines. Key behavioral difference: `testutil` uses `mc.Run()` (no escape handling) while `testhelpers` uses `mc.RunWithEscapeHandling()`. This means tests using `testutil` don't exercise production escape handling.
**Fix:** Consolidate to one package (probably `testutil` since it handles library setup). Ensure all evaluation paths use `RunWithEscapeHandling()` to match production. Update all imports.
**Effort:** L (many files import these)
**Verify:** `make lint && make test ./...`

---

## Phase 8: Architectural Improvements (Low priority, large effort)

These are longer-term items to tackle opportunistically when working in the area.

### Task 8.1: Extract `machine/compilation/resolver/`

**Files:** `machine/compilation/file_resolver.go` (500 lines, 4 implementations)
**Problem:** `FileResolver` implementations are pure I/O infrastructure with no compilation logic, but live inside the 23K-line `compilation` package.
**Fix:** Extract `OSFileResolver`, `FSFileResolver`, `EmbedFileResolver`, `ChainFileResolver` into `machine/compilation/resolver/` sub-package.
**Effort:** M
**Verify:** `make lint && make test ./machine/...`

### Task 8.2: Evaluate `wile.Value` wrapper utility

**Files:** `value.go`
**Problem:** The public API wraps `values.Value` in `wile.Value` (`wrappedValue` struct). The wrapper provides no methods beyond `SchemeString()`, `IsVoid()`, and `Internal()`. The `Internal()` escape hatch shows the wrapper isn't pulling its weight.
**Decision:** Either (a) remove the wrapper and expose `values.Value` directly (breaking, but v1.x with zero consumers), or (b) add meaningful methods (`AsInt64() (int64, bool)`, `AsList() []Value`, etc.) that justify the wrapper's existence.
**Effort:** M
**Verify:** `make lint && make test ./...`

### Task 8.3: Fix `internal/repl` importing `machine/compilation`

**Files:** `internal/repl/meta.go:17-18`
**Problem:** REPL (presentation layer) imports `machine/compilation` (deep internal) to type-assert `LibraryRegistry` and access `CompiledLibrary` fields.
**Fix:** Widen `LibrarySearcher` interface to expose operations used at each call site (`Lookup`, `All`, `IsLoading`), or extract a `LibraryInfo` value type that `meta.go` consumes without importing `compilation`.
**Effort:** M
**Verify:** `make lint && make test ./internal/repl/... ./machine/...`

---

## Summary

| Phase | Items | Effort | Theme |
|-------|-------|--------|-------|
| 1 | 3 tasks | S each | Silent limits — add overflow guards, exhaustiveness tests |
| 2 | 3 tasks | S-M | File resolution — unify `os.Stat` paths with `FileResolver` |
| 3 | 2 tasks | M each | Registration — eliminate dual-source-of-truth patterns |
| 4 | 4 tasks | S-M | Test discipline — sentinels over strings, integration coverage |
| 5 | 4 tasks | S-M | Missing abstractions — interfaces, helpers, convention enforcement |
| 6 | 3 tasks | S each | Dead code and style cleanup |
| 7 | 1 task | L | Test helper unification |
| 8 | 3 tasks | M each | Architectural improvements (opportunistic) |

**Recommended execution order:** Phase 1 (cheap safety wins) → Phase 6 (cheap cleanup) → Phase 2 (file resolution, high impact for embedding) → Phase 4 (test discipline) → Phase 3 (registration) → Phase 5 (abstractions) → Phase 7 → Phase 8.
