# Performance Refactoring Plan

**Status:** PLANNED — Not yet started

## Overview

Comprehensive performance refactoring plan covering the full interpreter pipeline: parsing → expansion → compilation → execution. Organized into dependency-ordered phases with clear success criteria.

This plan supersedes `plans/OPTIMIZATION_PLAN.md`, which covered only VM-level micro-optimizations. The new plan is broader — addressing macro expansion allocation, compiler optimizations, and caching alongside the original VM work.

### Relationship to OPTIMIZATION_PLAN.md

| OPTIMIZATION_PLAN Phase | This Plan Phase | Notes |
|---|---|---|
| Phase 0 (Profiling) | Phase 0 | Extended with per-stage benchmarks and GC pressure |
| Phase 1.1 (PopAll fix) | Phase 1.1 | Identical |
| Phase 1.2 (ctx.Done batching) | Phase 1.2 | Identical |
| Phase 2 (sync.Pool) | Phase 2 | Extended with sub-context pooling |
| Phase 3 (Switch dispatch) | Phase 6 | Moved later (higher risk, moderate impact) |
| Phase 4 (CoW environments) | Phase 3 | Promoted earlier (higher impact) |
| Phase 5 (Tagged integers) | Phase 7.1 | Kept as future work |

New in this plan: Phase 1.3–1.5 (character cache, error stack depth, single-value optimization), Phase 3.2–3.3 (binding copy avoidance, keys map sharing), Phase 4 (expander allocation), Phase 5 (compiler optimizations), Phase 7.2–7.3 (caching).

## Current Bottlenecks

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Allocation Hotspots by Pipeline Stage            │
├─────────────────┬───────────────────────────────────────────────────┤
│ VM Execution    │ PopAll clones stack (slices.Clone every Apply)   │
│                 │ Environment deep-copy on every non-tail call     │
│                 │ ctx.Done() select EVERY iteration                │
│                 │ Interface dispatch (2 ptr indirections per op)   │
│                 │ New Stack + MultipleValues per continuation save │
├─────────────────┼───────────────────────────────────────────────────┤
│ Macro Expansion │ New SyntaxPair/SyntaxSymbol per AddScope call    │
│                 │ New MachineContext per macro invocation           │
│                 │ New SourceContext per scope addition              │
│                 │ No expansion caching                             │
├─────────────────┼───────────────────────────────────────────────────┤
│ Compilation     │ Operations slice grows via append (no prealloc)  │
│                 │ No constant folding / dead code elimination      │
│                 │ No compilation caching across Engine instances    │
├─────────────────┼───────────────────────────────────────────────────┤
│ Values          │ Character: no cache (unlike Integer, Symbol)     │
│                 │ Pair: no pooling (highest-volume allocation, 24B) │
│                 │ ForeignError: 50-frame stack trace every error   │
│                 │ MultipleValues: slice alloc for single values    │
└─────────────────┴───────────────────────────────────────────────────┘
```

### Existing Infrastructure

| Component | Status |
|-----------|--------|
| `make bench` | Exists — end-to-end benchmarks (`wile_bench_test.go`) |
| `make profile-cpu` / `profile-mem` | Exists |
| `make bench-schelog` (Zebra puzzle) | Exists — real-world workload |
| Parser benchmarks | Exists (`internal/parser/parser_bench_test.go`) |
| Integer benchmarks | Exists (`values/integer_bench_test.go`) |
| VMCounters | Exists — OpsExecuted, ClosuresApplied, EnvsCopied, etc. |
| Integer cache | Exists — −32768..32767 |
| String interning | Exists — ≤64 bytes |
| Symbol interning | Exists — per TopLevel |
| Boolean singletons | Exists |

---

## Phase 0: Measurement Infrastructure

**Must complete first** — without stage-isolated benchmarks, optimizations are guesswork.

### Problem

The existing benchmarks measure end-to-end (`Eval`), but do not isolate pipeline stages. There are no benchmarks for macro expansion time, environment copy cost, continuation save/restore, or GC pressure from syntax object allocation.

### Approach

1. **Stage-isolated benchmarks:**
   - `BenchmarkExpandMacro` — expansion only (syntax-rules, nested macros, recursive macros like `and`/`or`/`let*`)
   - `BenchmarkCompileExpression` — compilation only (varying complexity)
   - `BenchmarkClosureApply` — isolated closure application cost (env copy)
   - `BenchmarkContinuationSaveRestore` — continuation save/restore cycle
   - `BenchmarkStackPopAll` — stack PopAll at various sizes
   - `BenchmarkEnvironmentCopy` — `LocalEnvironmentFrame.Copy` with varying binding counts
   - `BenchmarkPairAllocation` — cons cell allocation throughput
   - `BenchmarkSyntaxAddScope` — scope propagation through syntax trees

2. **GC pressure measurement:** `BenchmarkGCPressure` that runs `runtime.ReadMemStats` before/after workloads to measure `TotalAlloc`, `NumGC`, `PauseTotalNs`

3. **Live profiling:** Add `--pprof-port` flag to `cmd/main.go` for pprof HTTP server during REPL/file execution

4. **VMCounters expansion:** Add `SyntaxObjectsCreated`, `ScopeAdditions`, `EnvironmentLookups`, `ErrorsCreated`

### Files

| File | Action |
|------|--------|
| `wile_bench_test.go` | Add stage-isolated benchmarks |
| `machine/machine_context_bench_test.go` | New — VM internals benchmarks |
| `machine/expander_bench_test.go` | New — macro expansion benchmarks |
| `machine/counters.go` | Extend VMCounters |
| `cmd/main.go` | Add `--pprof-port` flag |

### Success Criteria

Every pipeline stage has at least one benchmark. `make bench` produces ns/op and allocs/op for all stages. `make profile-cpu PKG=./machine/...` produces actionable flame graphs.

### Risk

Low. Benchmarks are additive and test-only.

### Dependencies

None.

---

## Phase 1: Quick Wins — Zero-Allocation Fixes

Small, targeted fixes that reduce allocation without changing any API or architecture.

### 1.1 Fix PopAll Clone

**Problem:** `Stack.PopAll()` calls `p.Copy()` which calls `slices.Clone()`, allocating a new slice. The clone is unnecessary because the caller takes ownership of the returned slice.

**Fix:** Return the backing array directly and reset the stack to empty length.

**File:** `machine/stack.go`

**Success:** `BenchmarkStackPopAll` shows 0 allocs; `BenchmarkEvalFibonacci` shows measurable improvement.

**Risk:** Low.

### 1.2 Batch Context Cancellation Checks

**Problem:** `Run()` does `select { case <-ctx.Done(): ... default: }` on every iteration. The channel receive prevents certain compiler optimizations and adds a branch per instruction.

**Fix:** Split `Run()` into `runFast()` (no debugger) and `runWithDebugger()`. In `runFast()`, check `ctx.Done()` every 64 iterations using a bitmask: `if iter&63 == 0 { select ... }`.

**File:** `machine/machine_context.go`

**Success:** `BenchmarkEvalFibonacci` shows 10–15% improvement; REPL Ctrl+C still works within 1ms.

**Risk:** Low. Debugger path unchanged. Context cancellation latency increases by at most 64 instructions (microseconds).

### 1.3 Character Caching

**Problem:** `NewCharacter()` allocates a fresh `*Character` every time, even for common characters (`#\space`, `#\newline`, `#\0`–`#\127`). Character I/O reads create garbage.

**Fix:** Cache ASCII characters (0–127) in a global array, similar to the Integer cache.

**File:** `values/character.go`

**Risk:** None. Characters are immutable.

### 1.4 Reduce ForeignError Stack Trace Depth

**Problem:** Every `ForeignError` creation calls `runtime.Callers(1, pcs[:50])`, capturing up to 50 stack frames into a 400-byte array. Many errors are immediately caught and re-wrapped by the Scheme exception system.

**Fix:** Reduce the stack frame capture from 50 to 16 frames. Add a package-level `var ForeignErrorStackDepth = 16` for configurability. For errors in hot paths (type assertion failures in primitives), consider a separate `NewForeignErrorNoStack` that skips `runtime.Callers` entirely.

**File:** `values/foreign_error.go`

**Risk:** Low. Deep stack traces are rarely needed; 16 frames covers most useful context.

### 1.5 Single-Value MultipleValues Optimization

**Problem:** `NewMultipleValues(v)` allocates a `[]values.Value{v}` slice for every single-value return. The vast majority of Scheme operations return exactly one value.

**Fix:** For the common case of exactly 1 value, avoid the slice allocation. Pre-allocate a 1-element slice in MachineContext and reuse it: `p.value = p.value[:1]; p.value[0] = v`.

**Files:** `machine/multiple_values.go`, `machine/machine_context.go`

**Risk:** Low. Must ensure the slice is not shared across contexts (it is not — each mc has its own `value` field).

---

## Phase 2: Object Pooling with sync.Pool

**Problem:** High-frequency allocations with well-defined lifecycles: Stacks, MachineContinuations, and sub-contexts.

### 2.1 Stack Pool

Pool `*Stack` objects. `NewStack()` gets from pool; a new `Release()` method returns to pool after `Clear()`. Call `Release()` in `PopContinuation` (normal return) and when sub-contexts complete.

**Files:** `machine/stack.go`, `machine/machine_context.go`

### 2.2 Continuation Pool

Pool `*MachineContinuation` objects. `NewMachineContinuationFromMachineContext` gets from pool. `PopContinuation` returns consumed continuation to pool (after copying out its fields).

**Files:** `machine/machine_continuation.go`, `machine/machine_context.go`

### 2.3 Sub-Context Pool

Pool `*MachineContext` objects used for sub-contexts. `NewSubContext()` gets from pool and resets fields. Add a `release()` method called after `sub.Run()` completes.

**Files:** `machine/machine_context.go`

### 2.4 Pair Pool (Deferred/Experimental)

Pool `*Pair` objects for temporary list operations (map, for-each, list construction in primitives). Pairs returned to user space cannot be pooled (unknown lifetime). Only pool pairs used transiently during list construction where the final result is a vector or value extraction.

**Files:** `values/pair.go`

### Success Criteria

`BenchmarkEvalFibonacci` and `BenchmarkEvalTailRecursion` show 30–50% reduction in allocs/op. GC pressure benchmarks show reduced `NumGC` and `PauseTotalNs`.

### Risk

Medium. `sync.Pool` objects can be collected by GC; net benefit depends on allocation frequency vs GC timing. Continuation pooling must be carefully validated: a pooled continuation returned to pool must not be referenced by any `call/cc` capture.

### Dependencies

Phase 0 (benchmarks to measure improvement).

---

## Phase 3: Environment Copy Optimization

**Problem:** Every closure application in `Apply()` deep-copies the `LocalEnvironmentFrame`: the keys map is cloned, each `*Binding` is cloned (including its scopes slice). This is the single largest allocation cost in function-heavy workloads.

### 3.1 Copy-on-Write Environments

Add a `shared bool` flag to `LocalEnvironmentFrame`. When `Apply()` is called, set `shared = true` on the original and give the callee a shallow reference. Only when `SetLocalValue` is called does a copy happen (`copyOnWrite()`). Many closures (pure functions, accessors) never mutate their environment, avoiding the copy entirely.

**Files:** `environment/local_environment_frame.go`, `machine/machine_context.go`

### 3.2 Binding Copy Avoidance

**Problem:** `Binding.Copy()` clones the scopes slice even when scopes are nil (most runtime bindings have no scopes). The scopes are only needed for hygiene during expansion, not at runtime.

**Fix:** Skip scope cloning for bindings with nil scopes (the common case at runtime). Only clone scopes when `b.scopes != nil`.

**File:** `environment/binding.go`

### 3.3 Keys Map Sharing

**Problem:** The `keys map[values.Symbol]int` is cloned on every env copy, but it is never mutated after lambda compilation. Bindings are accessed by index, not by key lookup, at runtime.

**Fix:** Share the keys map between the original and copy. Add a `keysShared bool` flag; `EnsureLocalBinding` checks this flag and copies on write. Since `EnsureLocalBinding` is only called during compilation, not at runtime, the keys map is effectively immutable at runtime.

**File:** `environment/local_environment_frame.go`

### Success Criteria

`BenchmarkClosureApply` shows 60–80% reduction in allocs. `BenchmarkEvalFibonacci` (heavy function call) shows 20–40% total improvement.

### Risk

Medium. CoW environments require careful handling: any code path that mutates bindings must trigger the copy. Testing must cover recursive calls, `set!`, and mutation through closure capture.

### Dependencies

Phase 0 (benchmarks), Phase 1 (PopAll fix, to isolate env copy cost).

---

## Phase 4: Macro Expansion Allocation Reduction

**Problem:** The macro expander is the second largest source of allocation (after the VM). Every `AddScope` call on a syntax tree creates new `SyntaxPair`, `SyntaxSymbol`, and `SourceContext` objects. Each macro invocation creates a new `MachineContext`. Recursive macros (`and`, `or`, `let*`, `cond`) amplify this.

### 4.1 Scope Propagation Deferral

Instead of immediately propagating a scope through the entire syntax tree via `SyntaxPair.AddScope()`, record the pending scope on the pair and propagate lazily when a symbol is actually accessed. This changes `SyntaxPair` to carry an optional `pendingScopes []*Scope` field. `AddScope` appends to this list (O(1)) instead of walking the tree (O(n)). When `SyntaxCar()`/`SyntaxCdr()` is called, pending scopes are propagated to the child.

**Files:** `internal/syntax/syntax_pair.go`, `internal/syntax/syntax_symbol.go`

**Risk:** High. This is a structural change to the immutability model. Must be tested exhaustively against all hygiene tests, all `syntax-rules` tests, all `syntax-case` tests. The deferred propagation must be equivalent to eager propagation in all observable behavior.

### 4.2 SourceContext Interning

**Problem:** `WithScope` creates a new `SourceContext` per scope addition. Many SourceContexts share the same `Text`, `File`, `Start`, `End` fields and differ only in scopes.

**Fix:** Separate the location data (`Text`, `File`, `Start`, `End`) into an immutable `SourceLocation` struct shared by reference. `SourceContext` becomes `{ location *SourceLocation, Scopes []*Scope, Origin *OriginInfo }`. This reduces per-scope-addition allocation from copying 5 fields to creating a 3-field struct.

**Files:** `internal/syntax/source_context.go`, `internal/syntax/syntax_symbol.go`, `internal/syntax/syntax_pair.go`

### 4.3 Expander Sub-Context Reuse

**Problem:** `expandMacroInvocation` creates a new `MachineContext` + continuation + stack for every macro invocation. Bootstrap macros like `let`, `cond`, `and`, `or` are invoked hundreds of times during typical program compilation.

**Fix:** Maintain a per-expander reusable `MachineContext` that is reset between invocations. Instead of `NewMachineContextFromMachineClosure` each time, reset the existing context's fields (template, env, pc, value, evals).

**File:** `machine/expander_time_continuation.go`

### Success Criteria

`BenchmarkExpandMacro` shows 40–60% reduction in allocs. `BenchmarkCompile` shows measurable improvement. `BenchmarkEvalMacro` (end-to-end) shows improvement.

### Risk

High (4.1), Medium (4.2, 4.3).

### Dependencies

Phase 0 (expansion benchmarks). Independent of Phase 3.

---

## Phase 5: Compiler Optimizations

**Problem:** The compiler performs no optimization passes. Every expression is compiled directly to bytecode, including patterns that could be simplified at compile time.

### 5.1 Operations Slice Pre-allocation

**Problem:** `NativeTemplate.operations` and `sourceRefs` grow via `append`, causing multiple re-allocations during compilation.

**Fix:** Estimate operation count from the syntax tree size (heuristic: 2–3 ops per syntax node). Pre-allocate the operations and sourceRefs slices in `NewNativeTemplate`.

**Files:** `machine/native_template.go`, `machine/compile_time_continuation.go`

### 5.2 Constant Folding

Detect patterns like `(+ 1 2)` where all arguments are compile-time constants and replace with a single `LoadLiteral` operation. This applies to arithmetic, string operations, and boolean operations on literal arguments. Implement as a post-compilation pass over the operations slice.

**File:** New — `machine/optimize.go`

**Risk:** Medium. Must respect R7RS exactness semantics (exact arithmetic on exact operands must produce exact results).

### 5.3 Redundant Push/Pop Elimination

**Problem:** The compiler sometimes emits Push immediately followed by Pop (when a value is computed but unused). A peephole optimization pass can eliminate these.

**Fix:** Post-compilation peephole pass that scans for Push-Pop pairs with no intervening branches and eliminates them.

**File:** New — `machine/optimize.go`

### Success Criteria

`BenchmarkCompile` shows no regression (or improvement from pre-allocation). Simple expressions execute with fewer operations (visible in `VMCounters.OpsExecuted`).

### Dependencies

Phase 0. Independent of other phases.

---

## Phase 6: Switch-Based Dispatch

**Problem:** The VM dispatches operations via interface method call: `mc.template.operations[mc.pc].Apply(mc.ctx, mc)`. Interface dispatch in Go involves two pointer indirections (itab lookup + method pointer) and prevents inlining.

### Approach

Add an `Opcode uint8` enum, a compact `Instruction` struct, and a `switch`-based VM loop.

```
┌────────────────────────────────────────────────────┐
│                Migration Strategy                  │
├────────────────────────────────────────────────────┤
│ 1. Add Opcode to each Operation type               │
│ 2. Add NativeTemplate.instructions []Instruction   │
│    alongside existing operations                   │
│ 3. Populate instructions during compilation        │
│ 4. Switch dispatch in runFast(), interface          │
│    dispatch in runWithDebugger()                   │
│ 5. Eventually deprecate operations slice           │
└────────────────────────────────────────────────────┘
```

### Files

| File | Action |
|------|--------|
| `machine/opcode.go` | New — opcode enum (~37 opcodes) |
| `machine/instruction.go` | New — compact instruction struct |
| `machine/machine_context.go` | New switch-based `runSwitch()` loop |
| `machine/operation_*.go` (37+ files) | Add `Opcode()` method to each |

### Success Criteria

10–20% CPU reduction on `BenchmarkEvalFibonacci`. The switch loop should be visible as a single hot function in CPU profiles (vs scattered interface dispatch).

### Risk

High. Large refactor touching 37+ operation types. Must maintain perfect behavioral equivalence. Migration strategy (dual representation) allows incremental rollout.

### Dependencies

Phase 1 (runFast split). Phase 5 (peephole passes should run before instruction encoding).

---

## Phase 7: Advanced — Tagged Integers and Compilation Caching

Longer-term projects that require significant architectural changes.

### 7.1 Tagged Integers

Uses lower bits of pointers to represent small integers without heap allocation. Requires `unsafe` package. Deferred to last because it touches the `values.Value` interface — the most pervasive type in the codebase.

```go
type TaggedValue uintptr

const tagInteger = 0x1  // Low bit set = immediate integer

func (p TaggedValue) IsInteger() bool { return v&1 != 0 }
func (p TaggedValue) AsInteger() int64 { return int64(v) >> 1 }
```

**Benefits:**
- No allocation for integers −2^62 to 2^62
- Faster type checks (bit op vs interface assertion)
- Better cache locality

**Risk:** Major refactor of `values.Value` interface. Uses `unsafe`.

### 7.2 Compilation Caching

**Problem:** Each `Engine.Eval(ctx, code)` call parses, expands, and compiles from scratch. For repeated evaluation of the same code (e.g., REPL loop testing functions), this is wasteful.

**Fix:** Cache compiled `NativeTemplate` keyed by source string hash. The cache must be invalidated when definitions change (`define-syntax`, `define`). A simple LRU cache with a generation counter (incremented on any top-level definition) would work.

**Files:** `engine.go`, new `compilation_cache.go`

### 7.3 Library Pre-compilation

**Problem:** Standard libraries are re-expanded and re-compiled for each new `Engine` instance. The `LibraryRegistry` caches loaded libraries but not across engine instances.

**Fix:** Serialize compiled `NativeTemplate` to a binary format and cache on disk. Load pre-compiled libraries instead of re-parsing/expanding/compiling. This is a significant feature (bytecode serialization) that opens the door to ahead-of-time compilation.

**Dependencies:** Phase 5 (stable instruction encoding for serialization), Phase 6 (opcode-based format).

---

## Implementation Priority

| Phase | Expected Impact | Risk | Dependencies |
|---|---|---|---|
| **Phase 0** | Foundation | Low | None |
| **Phase 1** | 15–25% overall | Low | Phase 0 |
| **Phase 2** | 30–50% alloc reduction | Medium | Phase 0 |
| **Phase 3** | 20–40% on call-heavy workloads | Medium | Phase 0, Phase 1 |
| **Phase 4** | 40–60% macro expansion alloc reduction | High | Phase 0 |
| **Phase 5** | 5–15% execution improvement | Medium | Phase 0 |
| **Phase 6** | 10–20% CPU | High | Phase 1, Phase 5 |
| **Phase 7** | Variable | High | Phase 5, Phase 6 |

### Dependency Graph

```
Phase 0 ──→ Phase 1 ──→ Phase 3
   │            │
   │            └──→ Phase 6
   │
   ├──→ Phase 2
   │
   ├──→ Phase 4
   │
   └──→ Phase 5 ──→ Phase 6 ──→ Phase 7
```

---

## Out of Scope

| Item | Reason |
|------|--------|
| Custom allocator / arena allocation | Requires `unsafe`, low ROI for Go's already-optimized small-object allocator |
| JIT compilation | Out of scope for pure-Go project; would require `unsafe` for code generation |
| Parallel compilation | Compiler is single-threaded; parallelizing requires significant env synchronization |
| Alternative GC strategies | Project explicitly delegates GC to Go's runtime |
| Numeric tower optimization | Direct dispatch already in place; diminishing returns |
| Parser optimization | Parser benchmarks show it is not a bottleneck relative to expansion/compilation/execution |
| Tokenizer optimization | Simple state machine; not a bottleneck |

---

## Verification Protocol (All Phases)

After each phase:

1. `make test` — all existing tests pass
2. `make bench` — compare against Phase 0 baselines
3. `make profile-cpu` — verify hot functions shifted as expected
4. `make profile-mem` — verify allocation reduction
5. `make bench-schelog` — Zebra puzzle benchmark (real-world regression test)
6. Run REPL interactively — verify Ctrl+C still responsive (Phase 1.2 check)
7. Run hygiene test suite — verify macro correctness preserved (Phase 4 check)

---

## Critical Files

| File | Relevance |
|------|-----------|
| `machine/machine_context.go` | VM loop (`Run`, `Apply`, `NewSubContext`) — Phases 1, 2, 6 |
| `environment/local_environment_frame.go` | Environment copy hotspot — Phase 3 |
| `machine/stack.go` | Stack PopAll/pooling — Phases 1.1, 2.1 |
| `internal/syntax/syntax_pair.go` | AddScope allocation — Phase 4 |
| `internal/syntax/source_context.go` | SourceContext allocation — Phase 4.2 |
| `machine/expander_time_continuation.go` | Macro invocation sub-contexts — Phase 4.3 |
| `machine/machine_continuation.go` | Continuation save/restore — Phase 2.2 |
| `machine/native_template.go` | Operations slice growth — Phase 5.1 |
| `values/foreign_error.go` | Error stack trace allocation — Phase 1.4 |
| `values/character.go` | Character caching — Phase 1.3 |
| `machine/multiple_values.go` | Single-value optimization — Phase 1.5 |
| `engine.go` | Compilation caching — Phase 7.2 |
