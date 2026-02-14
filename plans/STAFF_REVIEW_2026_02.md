# Staff Software Engineer — Codebase Review (February 2026)

**Scope:** Full codebase review of Wile (R7RS Scheme in Go)
**Codebase:** 134K lines Go, 605 files (292 test), 33 packages
**Test Status:** All passing (Go + Scheme), zero failures
**Overall Grade:** A-

---

## Executive Summary

Wile is a well-engineered R7RS Scheme interpreter/compiler suitable for production embedding. The architecture is sound, the code is consistent, and the test coverage is thorough. The main areas for improvement are a handful of compile-time panics, some documentation gaps in the public API, and minor test coverage gaps in the hygiene algorithm.

---

## 1. Architecture (Grade: A)

The pipeline (`Source → Tokenizer → Parser → SyntaxValue → Expander → Compiler → NativeTemplate → VM → Value`) is clean and well-layered.

**Strengths:**
- PC-based bytecode VM with straightforward instruction dispatch
- Immutable `NativeTemplate` operations (never modified after creation)
- 36 operation types, each implementing `Apply(ctx, mc) (*MachineContext, error)`
- Clean separation: `values/` → `environment/` → `internal/*` → `machine/` → `registry/` → `wile/`
- Public API is minimal: `wile/`, `values/`, `registry/`

**Package sizes:**
- `machine/` — ~131 files, 14.4K source + 21.5K test (1.49x test ratio)
- `registry/core/` — ~145 files
- `values/` — ~94 files

**Compiler organization:**
- 25 `compile_*.go` files, one per special form — well-justified despite file count
- `SyntaxCompiler` dispatcher with registry-based form dispatch
- Low coupling between compiler files; no circular dependencies

---

## 2. VM & Continuations (Grade: A)

**MachineContext** (`machine_context.go`) is the execution hub:
- `Run()` loop checks `ctx.Done()` every iteration (proper preemption)
- Context cancellation properly threaded through (fixed in commit `4fcf675`)
- Debugger nullable and properly guarded

**Continuations are exemplary:**
- `MachineContinuation` captures immutable state `(env, template, value, evals, pc, parent)`
- `DeepCopy()` for re-invocation is correctly implemented
- Critical invariant maintained: `Restore()` copies evals stack, `PopContinuation()` does not (single-use)
- Composable continuations validate thread-ID to prevent cross-thread invocation

**Dynamic-wind (R7RS §6.10):**
- Thunks always called (even on exception/continuation escape)
- Sub-contexts isolate thunk execution
- Proper stack slicing with capacity limits to prevent aliasing
- Textbook correct continuation-safe unwinding

---

## 3. Concurrency (Grade: A)

**Production-ready.** All critical paths verified:

| Component | Mechanism | Status |
|---|---|---|
| Symbol interning | Per-TopLevelEnvironment, RWMutex + double-checked locking | Correct |
| Global environment | RWMutex with copy semantics | Correct |
| Thread state (SRFI-18) | sync.Mutex for all state transitions | Correct |
| Thread context | `context.WithCancel(parentCtx)` derivation | Correct (fixed 4fcf675) |
| Thread sub-context | `CaptureSubContextParams()` avoids goroutine data races | Correct |
| Mutex ownership tracking | Separate `mutexMu` (no lock ordering issues) | Correct |
| Condition variables | `sync.Cond` with waiter count tracking | Correct |
| Registry loading | RWMutex; primitives installed once at Engine creation | Correct |
| VM loop | Context cancellation checked every iteration | Correct |

**No goroutine leaks detected.** Proper cleanup on thread exit via defer.

**Documented limitation:** `LoadPathStack` is per-VM not per-thread; concurrent `(load ...)` calls could corrupt LIFO ordering. Low impact — concurrent loading is uncommon.

**Deferred items (low priority per architectural review criteria):**
- L15: `thread-sleep!` ignores context cancellation
- L11: `eval` doesn't inherit dynamic context
- L3: `ChannelSelect` busy-spins (could use `reflect.Select`)

---

## 4. Error Handling (Grade: B+)

**Two-layer convention (sentinel + wrap) well established:**
- 90+ static sentinels in `values/foreign_error.go`
- 59 files use `WrapForeignErrorf()` for context-aware wrapping
- Zero instances of `err == X` or `err != X` comparisons
- 79 documented uses of `errors.Is()`/`errors.As()` across 28 files

**Recovery boundaries are correct:**
- FFI boundary (`ffi.go:876-891`): Re-panics non-ForeignErrors
- VM ForeignFunction call (`operation_foreign_function_call.go:68-82`): Wraps with `ErrPanicRecovery`
- Thread execution (`values/thread.go:236-248`): Captures as `ErrThreadPanic`

### Panic Issues

**P0 — FIXED in this review session:**
- `compile_time_continuation.go:838` — `panic(values.ErrNotAList)` on quasiquoted improper lists with unquotes. Reachable via e.g. `` `(a ,b . c) ``. Fixed by removing the improper-list panic from the splice-check phase (the expansion code below already handles improper lists correctly).

**P0 — Unreachable but poor pattern:**
- `compile_time_continuation.go:836` — `panic(err)` where callback never returns unexpected errors
- `compile_quasisyntax.go:274` — `panic(err)` where callback always returns nil

**P1 — Intentional invariant checks (stack):**
- `stack.go:39,50,91` — Stack underflow panics. Only fire on compiler bugs (mismatched push/pop). Internal-only, not user-facing.

**P1 — FIXED (commit `b5c0ece`):**
- `environment/load_path_stack.go:51` — `panic("LoadPathStack.Push: ...")` → returns `ErrInvalidLoadPath` error
- `values/big_complex.go` — 10 string panics → typed sentinel panics (`ErrNotANumber`, `ErrExactnessConversion`)

### Ignored Errors (acceptable)

- File close in defer (`cmd/scheme/main.go:171`): Can't propagate from defer
- Port closure in `call-with-output-file`: Must close even if procedure failed
- `big.Float` accuracy (not an error, just precision info)
- Debug column parsing: Optional, defaults to 0

---

## 5. Public API & Embedding (Grade: A-)

**API design is minimal and well-focused:**
- `NewEngine()` with functional options (`WithExtension`, `WithRegistry`)
- `Eval`/`Compile`/`Run` pipeline with proper context threading
- `RegisterFunc` FFI is impressive: pre-computed converters for int64, float64, string, bool, []byte, []T, map[K]V, structs, func callbacks, wile.Value, context.Context
- Three-level error hierarchy: `Error` (init), `CompilationError` (parse/expand), `RuntimeError` (execution with Condition, Source, StackTrace)

### Issues

**P1 — Missing documentation (FIXED — commit `b5c0ece`):**
- ~~No goroutine-safety documentation on `Engine`~~ → Added: NOT safe for concurrent use; SRFI-18 threads within same Engine are safe
- ~~`CompiledCode` bound to its Engine — not documented~~ → Added: must run on same Engine, concurrent execution not safe
- `RuntimeError.Condition` can be nil — not documented (addressed via `IsSchemeException()` method)
- `RegisterFunc` callback synchronicity requirement needs clearer docs
- No API stability guarantee document

**P1 — Internal types in public API:**
- `Engine.Environment()` and `Engine.TopLevelEnvironment()` return `environment.*` types marked "advanced use" — could leak internal API changes to embedders

**P2 — API completeness:**
- No `Registry.FindPrimitive(name)` or `HasPrimitive(name)` query methods
- ~~`PushLoadPath` panics on relative paths — should return error~~ (FIXED in P1)
- `PrimitiveSpec` lacks metadata (Doc, ParamNames, Category) for auto-generated docs
- No `Engine.Close()` method (acceptable since Engine holds no OS resources, but extensions may spawn goroutines)

**P2 — Error type improvements:**
- No distinction between Scheme exceptions (Condition != nil) and VM/primitive errors
- `RuntimeError.Cause` can contain internal `machine.*` types — document as logging-only

---

## 6. Values & Numeric Tower (Grade: A-)

**Comprehensive 7-type system:**

| Type | File | Precision | Exact? |
|---|---|---|---|
| Integer | integer.go | int64 | yes |
| BigInteger | big_integer.go | arbitrary | yes |
| Rational | rational.go | math/big.Rat | yes |
| Float | float.go | float64 | no |
| BigFloat | big_float.go | arbitrary | no |
| Complex | complex.go | complex128 | no |
| BigComplex | big_complex.go | arbitrary | no |

**R7RS exactness contagion correctly implemented:**
- All 7 Multiply methods handle exact zero exception consistently
- `multiplyResultForZero()` helper used across all types
- `(* 0 +inf.0)` is implementation-defined per R7RS §6.2.2

**Panic convention for arithmetic is correct by design:**
- `Number` interface methods return `Number`, not `(Number, error)`
- Panics are recovered at the VM boundary (`operation_foreign_function_call.go`)
- This is the established contract, matching R7RS exception semantics

**Minor:**
- `Complex.Multiply` has a minor inconsistency (skips `multiplyResultForZero` helper) but is semantically correct since Complex is always inexact

---

## 7. Environment & Scoping (Grade: A)

**Binding model is sound:**
- Two-phase lookup: local first (O(1) map per frame), then global (RWMutex-protected)
- `LocalIndex` uses `[slot, depth]` pairs for correct parent-chain traversal
- No shadowing bugs: locals searched before globals

**Symbol interning (R7RS §6.5 compliant):**
- Per-TopLevelEnvironment (enables multiple isolated VMs)
- Child environments delegate to parent (preserves symbol identity across boundaries)
- Double-checked locking: RLock fast path, Lock slow path with double-check

**Phase separation (clean and correct):**
- `PhaseRegistry` with `map[int]*EnvironmentFrame`
- Each phase gets isolated global bindings but shares symbol interning
- Phase environments parent to runtime frame (siblings, not children)

**Scope sets (Flatt 2016 — correctly integrated):**
- Bindings carry `scopes []*syntax.Scope`
- `scopesCompatible()` checks subset relationship
- `GetLocalIndexWithScopes()` implements maximal resolution (most specific binding wins)
- Pre-hygiene bindings (no scopes) treated as universally compatible

**Test gap:** No explicit test for the "maximal" algorithm with multiple scope-bearing candidates. Low risk since hygiene is well-tested at the macro level.

---

## 8. Test Infrastructure (Grade: A-)

- Test-to-code ratio: 1.49x in machine/ (21.5K test : 14.4K source)
- Table-driven tests with quicktest (`qt`) + `SchemeEquals` throughout
- Full bootstrap integration tests (library_scheme_test.go)
- Comprehensive concurrency tests (global_environment_frame_concurrency_test.go)
- Coverage improvement tests exist as explicit files

**Gaps:**
- No test for scope-maximality algorithm with competing candidates
- No stress test for symbol interning with many symbols
- No integration test for context cancellation during thread execution
- Limited tests for `GetLocalIndexWithScopes()` edge cases

---

## Prioritized Recommendations

### P0 — Ship-blocking (DONE)

- [x] **Fix quasiquote panic on improper lists with unquotes** — `compile_time_continuation.go:838`. Fixed in this session. Regression test added.

### P1 — Should fix (DONE)

- [x] **Convert `LoadPathStack.Push` panic to error return** — `environment/load_path_stack.go:51`. Changed to return `ErrInvalidLoadPath`. Updated all callers: `engine.go` (`PushLoadPath`, `WithLoadPath`), `prim_eval.go` (`PrimLoad`), `compile_time_continuation.go` (`compileIncludeImpl`), `library_loader.go` (`loadLibraryFromFile`).
- [x] **Convert BigComplex string panics to sentinel errors** — `values/big_complex.go` (10 sites). Replaced string panics with typed sentinels (`ErrNotANumber`, `ErrExactnessConversion`). Panics remain (by design — numeric tower uses panic/recover at VM boundary) but are now recoverable via `errors.Is`.
- [x] **Document Engine goroutine-safety** — Added doc comment: Engine is NOT safe for concurrent use; each goroutine needs its own Engine or external synchronization. SRFI-18 threads within a single Engine are safe.
- [x] **Document CompiledCode binding to Engine** — Added doc comment: must only be run on the same Engine that compiled it; running on a different Engine is undefined behavior.
- [x] **Add `RuntimeError.IsSchemeException()` method** — `error.go`. Reports whether error originated from Scheme `raise`/`raise-continuable` (i.e., `Condition != nil`).

### P2 — Should improve (DONE)

- [x] **Add Registry query methods** — `FindPrimitive(name, phase)`, `HasPrimitive(name, phase)` added at `registry/registry.go:120-141`
- [x] **Add scope-maximality unit test** for `GetLocalIndexWithScopes()` — two new subtests: non-nested overlapping scope sets, scopeless-vs-scoped
- [x] **Structure long FFI docstrings** with Go doc subheadings — `RegisterFunc` restructured with `# Supported Types`, `# Variadic Functions`, `# Context Forwarding`, `# Callbacks`
- [x] **Add API stability statement** to README — added after Package Structure table
- [x] **Document RuntimeError.Condition nil semantics** — restructured doc comment with `# Condition`, `# Source and Stack Trace`, `# Cause` sections
- [x] **Document callback synchronicity requirement** in RegisterFunc — already documented at `ffi.go:69-73`

### P3 — Nice to have

- [ ] **Add Engine.Close()** for extensions that spawn goroutines
- [ ] **Add PrimitiveSpec metadata** (Doc, ParamNames, Category) for auto-generated docs
- [ ] **Extract machine/ subpackages** (operations/, compiler/, expander/) — optional organizational improvement
- [ ] **Add recursion depth limit** — configurable via Engine, prevents DoS
- [ ] **Deferred concurrency items**: thread-sleep context cancellation (L15), eval dynamic context (L11), ChannelSelect busy-spin (L3)

---

## Changes Made in This Review

### Fix: P1 staff review items — panics, docs, API

**Commit:** `b5c0ece`

**Files changed:**
- `environment/load_path_stack.go` — `Push` returns `error` instead of panicking
- `environment/load_path_stack_test.go` — Panic assertions → error assertions
- `environment/resolve_test.go` — Updated `Push` call sites
- `environment/top_level_environment_test.go` — Updated `Push` call sites
- `values/big_complex.go` — 10 string panics → typed sentinel panics (`ErrNotANumber`, `ErrExactnessConversion`)
- `values/foreign_error.go` — Added `ErrInvalidLoadPath` sentinel
- `engine.go` — `PushLoadPath` returns error; `WithLoadPath` propagates it; goroutine-safety docs on `Engine`
- `compiled.go` — Doc comment: `CompiledCode` bound to its Engine
- `error.go` — Added `RuntimeError.IsSchemeException()` method
- `internal/extensions/eval/prim_eval.go` — `PrimLoad` propagates `Push` error
- `machine/compile_time_continuation.go` — `compileIncludeImpl` propagates `Push` error
- `machine/library_loader.go` — `loadLibraryFromFile` propagates `Push` error

**Summary:** Converted the `LoadPathStack.Push` panic to a proper error return and propagated the new error through all 4 callers. Replaced 10 string panics in BigComplex with typed sentinels (the panics themselves are correct by design — the numeric tower uses panic/recover — but string panics can't be matched with `errors.Is`). Added goroutine-safety documentation to `Engine` and `CompiledCode`. Added `RuntimeError.IsSchemeException()` for embedders to distinguish Scheme exceptions from VM errors.

### Fix: quasiquote panic on improper lists with unquotes

**Files changed:**
- `machine/compile_time_continuation.go` — 6-line fix replacing 8-line panic block
- `machine/coverage_fullruntime_test.go` — 35-line regression test

**Bug:** `expandQuasiquoteList` panicked with `ErrNotAList` during the splice-checking phase when encountering an improper (dotted) list containing unquotes (e.g., `` `(a ,b . c) ``). The `SyntaxForEach` returns the dotted tail for improper lists, and line 838 treated any non-empty-list termination as an error — even though the expansion logic below (`expandQuasiquoteImproperList`) already handles improper lists correctly.

**Root cause:** The splice-check phase was stricter than the expansion phase. The quasisyntax version in `compile_quasisyntax.go` correctly ignores improper tails during splice-checking (line 276: "For improper lists, the tail won't be an empty list. That's fine"), but the quasiquote version panicked instead.

**Hidden by optimization:** `compileQuasiquoteDatum` checks `quasiquoteNeedsRuntime` first, and quasiquoted improper lists *without* unquotes take the literal-emission fast path, never reaching `expandQuasiquoteList`. Only improper lists *with* unquotes (which force runtime evaluation) trigger the bug.

**Fix:** Changed `v, err :=` to `_, err :=` (discard unused tail value), replaced nested panic block with `if err != nil && !errors.Is(err, values.ErrStopIteration) { panic(err) }`.

**Test:** Added `TestCoverageQuasiquoteImproperWithUnquote` with two subcases: single unquote before dotted tail, and multiple unquotes before dotted tail.

### Fix: P2 staff review items — docs, tests, API stability

**Files changed:**
- `error.go` — Restructured `RuntimeError` doc comment with `# Condition`, `# Source and Stack Trace`, `# Cause` subheadings. Documented nil Condition semantics (VM/primitive errors) and Cause opacity.
- `ffi.go` — Restructured `RegisterFunc` doc comment with `# Supported Types`, `# Variadic Functions`, `# Context Forwarding`, `# Callbacks` subheadings.
- `README.md` — Added "API Stability" subsection after Package Structure table listing public packages (`wile`, `values`, `registry`) and noting `machine/` and `internal/` are not covered by compatibility guarantees.
- `environment/environment_coverage_test.go` — Added two new subtests to `TestGetLocalIndexWithScopes_Maximality`: "non-nested overlapping scope sets resolved by count" (verifies scope count trumps position when candidates have disjoint scope subsets) and "scopeless candidate loses to scoped candidate" (verifies a binding with scopes beats a scopeless binding even when the scopeless one is innermost).

**Already documented (no changes needed):**
- Callback synchronicity requirement already documented at `ffi.go:69-73`.
