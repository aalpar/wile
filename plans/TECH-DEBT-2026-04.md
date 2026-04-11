# Technical Debt Assessment — April 2026

> Full-codebase assessment: 437K lines Go, 1695 files, 39 packages.
> All tests passing, median coverage ~87%.

**Themes:** (1) dual-path legacy from incremental evolution, (2) missing interfaces
forcing type-switch proliferation, (3) silent limits with no enforcement.

---

## Phase 1: Silent Limits & Safety (High priority, low effort) — Mostly Complete

Items that fail silently — wrong results, not crashes.

### Task 1.1: ~~Fix `uint16` source table index overflow~~ [Done]

Changed `sourceRefs []uint16` → `[]uint32` and `internSource` return type to `uint32` across `native_template.go`, `edit_plan.go`, `peephole.go`, and all test files. Overflow guard updated from `MaxUint16` to `MaxUint32`. Test `TestInternSource_LargeIndex` verifies 70K source entries work correctly.

### Task 1.2: ~~Add opcode round-trip exhaustiveness test~~ [Done — already existed]

`TestOpcodeRoundTrip` at `machine/native_template_test.go:425`. Iterates all opcodes, skips `OpComplex`, verifies `instructionToOperation` returns non-nil.

### Task 1.3: ~~Add extension list consistency test~~ [Done — already existed]

`TestExtensionListConsistency` at `extension_consistency_test.go:29`. Verifies `(wile <name>)` library set matches expected extensions via `AvailableLibraries`.

### Task 1.4: Add eval stack size limit for sandboxed embedders

**Files:** `machine/stack.go`, `machine/machine_context.go`, `engine.go` (new option)
**Problem:** The eval stack (`[]values.Value`) grows without bound. A program like `(f a1 a2 ... a1000000)` allocates a million-element slice. Call depth is configurable (`WithMaxCallDepth`), but stack size is not. Sandboxed embedders running untrusted code have no protection against OOM via huge argument lists.
**Fix:** Add `WithMaxStackSize(n int)` engine option. Check on push only when limit is set (zero = unlimited, matching `WithMaxCallDepth` convention).
**Effort:** S
**Verify:** `make lint && make test ./machine/... && go test -run TestStackLimit ./...`

---

## Phase 2: File Resolution Unification (High priority, medium effort) — COMPLETE

### Task 2.1: ~~Make `ResolveFile` delegate to `FileResolver`~~ [Resolved — no change needed]

**Assessment (2026-04-05):** The stated problem was incorrect. `ResolveFile` has exactly one caller: `OSFileResolver.ResolveAndOpen` (`machine/compilation/file_resolver.go:194`). `FSFileResolver` has its own resolution logic (`file_resolver.go:310-357`) using `fs.Stat(p.fsys, candidate)` — it never calls `ResolveFile`. Embedders with `WithSourceFS()` get `FSFileResolver`, which never touches this code path. The `os.Stat` calls in `ResolveFile` are correct behavior for the OS filesystem resolver's internal implementation. Adding a `FileResolver` parameter would be complexity for no practical benefit since the two resolution strategies (OS absolute paths vs FS-relative paths) are fundamentally different.

### Task 2.2: ~~Fix library search `os.Stat` bypass~~ [Done, c8cbdf57]

`FindLibraryFile` (the method with `os.Stat` bypasses) was deleted as dead code. `LoadLibrary` already resolves via `FileResolver` correctly, making `FindLibraryFile` unreachable. `ToFilePath` (only used by `FindLibraryFile`) was also deleted.

### Task 2.3: Fix `initializeEnvironment` resolver overwrite [Done, 19d14d39]

Bootstrap now only sets `OSFileResolver` when `namespace.FileResolver()` is nil, preserving any previously configured `ChainFileResolver`.

---

## Phase 3: Registration Unification (High priority, medium effort) — COMPLETE

Three forms of registration drift that can cause silent bugs during feature work.

### Task 3.1: ~~Unify syntax compiler dual registration~~ [Done]

Single `syntaxCompilerEntries` slice feeds both `compilerRegistry` (dispatch) and `RegisterSyntaxCompilers` (compile-time environment). `TestSyntaxCompilerRegistrationConsistency` guards against out-of-band registration.

### Task 3.2: ~~Consolidate phase registration calls~~ [Done]

`RegisterAllPhaseHandlers(env)` replaces separate calls at all init sites. `VerifyAllPhaseHandlers()` cross-checks all three registries: `forms.Verify()` (validators), `VerifyCompilers()` (Tier 1 + Tier 2), `VerifyExpanders()` (syntax compiler ↔ expander consistency). Stale `RegisterPrimitiveExpanders` call in `machine/hygiene_test.go` migrated. `primitiveExpanderEntries` extracted to package-level var (parallels `syntaxCompilerEntries`). Verification immediately caught 6 missing expander entries (`library`, `export`, `meta`, `define-for-syntax`, `begin-for-syntax`, `eval-when`) — added as `expandUnchanged`.

---

## Phase 4: Test Discipline (Medium priority, medium effort) — Mostly Complete

### Task 4.1: ~~Migrate error-string assertions to `errors.Is`~~ [Done]

Migrated 25 sites across 4 files:
- `registry/helpers/args_test.go` (3 sites): regex on Go type names → `errors.Is`
- `machine/compilation/import_set_datum_test.go` (16 sites): `qt.Contains` → `errors.Is`
- `environment/resolve_test.go` (2 sites): string check → `errors.Is(err, werr.ErrFileNotFound)`
- `machine/continuation_winding_coverage_test.go` (4 sites): "arguments" → `errors.Is(err, werr.ErrWrongNumberOfArguments)`

Skipped (valid string tests): `ffi_test.go` (tests error propagation quality, sentinels already present), `exception_escape_test.go` (tests `Error()` formatting output).

### Task 4.2: ~~Add security gate integration tests~~ [Done — already existed]

**Assessment (2026-04-11):** `engine_sandbox_test.go` already contains 12+ tests covering `DenyAll()`, `ReadOnly()`, `FilesystemRoot()`, and selective authorization policies against files, system, eval, and import primitives. Tests: `TestAuthorizer_DenyBlocksFileRead`, `TestAuthorizer_DenyBlocksFileWrite`, `TestAuthorizer_DenyBlocksDelete`, `TestAuthorizer_DenyBlocksEnvVar`, `TestAuthorizer_DenyBlocksExit`, `TestAuthorizer_DenyBlocksLoad`, `TestAuthorizer_DenyBlocksImport`, etc.

### Task 4.3: ~~Add rest-arg buffer aliasing regression test~~ [Done]

Added `TestRestArgBufferAliasing` in `registry/core/prim_list_test.go`. Verifies `PrimList`'s spine copy prevents buffer aliasing across variadic calls.

### Task 4.4: ~~Add `validate/` clause-level syntax-rules error tests~~ [Done]

Added 4 cases to `TestSyntaxRules_Errors`: non-list clause, one-element clause, improper clause, non-symbol in literals. Note: three-element clause is silently accepted (compiler uses first two elements) — not an error condition.

---

## Phase 5: Missing Abstractions (Medium priority, medium effort)

Interfaces and helpers that would eliminate type-switch proliferation and
hand-unrolled patterns.

### Task 5.1: ~~Add `Name()`/`Doc()` to `Closure` interface~~ [Done]

Added `NamedCallable` interface (`Name() string`, `Doc() string`) to `machine/closure.go`. Embedded in `Closure`. Compile-time checks for `MachineClosure` and `CaseLambdaClosure`. Replaced anonymous interfaces in `prim_reflection.go` with `machine.NamedCallable`.

**Assessment (2026-04-11):** `PrimProcedureName` and `PrimProcedureDocumentation` already used anonymous interfaces (`interface{ Name() string }`). The remaining type switches in prim_reflection.go, meta.go, and prim_disassemble.go extract type-specific data (arity, template, source location, disassembly) that genuinely differs per type — Name()/Doc() cannot simplify those further.

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

### Task 5.5: Complete `RequireArg[T]` migration

**Files:** 16 manual `mc.Arg(n).(Type)` + `if !ok` sites: `registry/core/prim_reflection.go` (2), `prim_exceptions.go` (2), `prim_predicates.go` (2), `prim_syntax_loc.go` (1), `prim_opaque.go` (1), `extensions/math/prim_rounding.go` (3), `extensions/threads/prim_threads.go` (1), `extensions/process/prim_process.go` (1), `internal/extensions/namespace/prim_namespace.go` (1), `internal/extensions/all/prim_all.go` (1), `internal/extensions/io/prim_ports.go` (1)
**Problem:** `helpers.RequireArg[T]` (130 usages across 25 files) is the standard pattern. 16 sites still use manual 3-line assertion. Two ways to do the same thing confuses contributors.
**Fix:** Migrate sites where semantics match. Leave predicate-style sites that branch on `ok` for non-error paths (e.g., `prim_predicates.go`).
**Effort:** S
**Verify:** `make lint && make test ./registry/... ./extensions/... ./internal/extensions/...`

---

## Phase 6: Dead Code & Cleanup (Medium priority, low effort)

### Task 6.1: ~~Delete `runtime/` package~~ [Done]

Already deleted. Package no longer exists.

### Task 6.2: Replace `context.TODO()` in test files

**Files:** 431 occurrences across 39 test files. Largest concentrations: `internal/parser/parser_coverage_test.go` (104), `internal/parser/parser_test.go` (62), `internal/validate/validate_test.go` (44), `internal/bootstrap/multithreading_test.go` (30), `registry/core/prim_io_test.go` (27), `internal/match/syntax_expand_test.go` (23).
**Problem:** `context.TODO()` signals "haven't decided yet" — the correct answer in test code is `context.Background()`. Originally scoped to 7 sites in test helpers; actual scope is 431 sites across the full test suite.
**Fix:** Project-wide `context.TODO()` → `context.Background()` replacement in `*_test.go` files.
**Effort:** S (mechanical)
**Verify:** `make lint && make test ./...`

### Task 6.3: Fix receiver naming on production types

**Files:** `machine/pool_generic.go:146` (`m`→`p`), `machine/counters.go:92,99` (`c`→`p`), `environment/load_path_stack.go:52-103` (`s`→`p`), `ffi_wrapper.go:29` (`s`→`p`), `values/port_base.go:56-116` (`b`→`p`), `internal/syntax/syntax_value.go:110` (`b`→`p`)
**Problem:** Six production types use non-`p` receivers, violating the project convention.
**Fix:** Rename receivers to `p`. Use `gofmt` or editor rename to avoid mistakes.
**Effort:** S
**Verify:** `make lint && make test ./...`

### Task 6.4: Add `typeswitchlint` to "ADDING A NEW VALUE TYPE" guide

**Files:** `values/values.go:86` (guide comment), `cmd/typeswitchlint/main.go:46` (`knownValueTypes`)
**Problem:** The guide comment lists 7 steps for adding a new value type. Step 8 — updating `cmd/typeswitchlint/main.go:knownValueTypes` — is missing. A new value type added by following the guide will silently escape lint coverage.
**Fix:** Add "8. cmd/typeswitchlint/main.go — add to knownValueTypes" to the guide comment.
**Effort:** S
**Verify:** Read the guide comment, verify all 8 steps match reality.

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
**Problem:** REPL (presentation layer) imports `machine/compilation` (deep internal) to type-assert `LibraryRegistry` and access `CompiledLibrary` fields. Also `registry/core/prim_reflection.go` imports for `ParseLibraryNameFromDatum`.
**Fix:** Widen `LibrarySearcher` interface to expose operations used at each call site (`Lookup`, `All`, `IsLoading`), or extract a `LibraryInfo` value type that `meta.go` consumes without importing `compilation`.
**Effort:** M
**Verify:** `make lint && make test ./internal/repl/... ./machine/...`

### Task 8.4: Make `DefaultBigFloatPrecision` configurable

**Files:** `values/big_float.go:32` (`DefaultBigFloatPrecision = 256`), 12 call sites across `values/big_float.go`, `values/big_complex.go`, `values/promotion.go`
**Problem:** All BigFloat arithmetic uses a hardcoded 256-bit precision. No engine option exists to change it. Users doing high-precision numerical work (financial, scientific) have no recourse without forking. Architecturally awkward because `values/` is below `machine/`, so threading config requires either a context-local value or a field on MachineContext propagated during arithmetic.
**Fix:** Add `WithBigFloatPrecision(bits uint)` engine option. Thread precision through a field readable during promotion/construction. The 12 call sites would accept a precision parameter or read from a context.
**Effort:** M
**Verify:** `make lint && make test ./values/... ./machine/...`

---

## Summary

| Phase | Items | Effort | Theme |
|-------|-------|--------|-------|
| 1 | 4 tasks (1.1-1.4) | S each | Silent limits — overflow guards, exhaustiveness tests, stack limit |
| 2 | 3 tasks | S-M | File resolution — ~~COMPLETE~~ (2.1 premise incorrect, 2.2+2.3 done) |
| 3 | 2 tasks | M each | Registration — ~~COMPLETE~~ (3.1-3.2 done, found+fixed 6 missing expander entries) |
| 4 | 4 tasks | S-M | Test discipline — Mostly complete (4.1, 4.3, 4.4 done; 4.2 open) |
| 5 | 5 tasks (5.1-5.5) | S-M | Missing abstractions — interfaces, helpers, convention enforcement |
| 6 | 4 tasks (6.1-6.4) | S each | Dead code and style cleanup (6.1 done) |
| 7 | 1 task | L | Test helper unification |
| 8 | 4 tasks (8.1-8.4) | M each | Architectural improvements (opportunistic) |

**Recommended execution order:** Phase 1 (cheap safety wins) → Phase 6 (cheap cleanup) → Phase 5 (abstractions) → Phase 7 (test helpers) → Phase 8 (architecture).

**Last updated:** 2026-04-11 (added Tasks 1.4, 5.5, 6.4, 8.4 from full-codebase reassessment; marked 6.1 done; expanded 6.2 scope from 7 to 431 sites).
