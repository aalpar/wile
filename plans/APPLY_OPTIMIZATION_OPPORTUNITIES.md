# Apply Call Path Optimization Opportunities

This document tracks potential performance optimizations for the procedure application hot path in `machine/machine_context.go:Apply()` and related code.

## Completed

- ✅ **Early Arity Validation** — Check arity before `CopyForApply()` to fail fast on wrong-arity calls without paying environment copy cost

## "Load to Unload" Analysis

### Current Flow

```
1. Argument evaluation → Stack
   [arg0, arg1, arg2]

2. OperationApply.PopAll()
   vs := []values.Value{arg0, arg1, arg2}  // Stack slice

3. MachineContext.Apply() → Copy loop
   for i := range bnds[:l] {
       bnds[i].SetValue(vs[i])  // 16 bytes × N (interface copy)
   }

4. OperationLoadLocal → Read binding
   bd := mc.env.GetLocalBinding(index)
   value := bd.Value()
```

**Copy cost:** N × 16 bytes (interface value = type pointer + data pointer)

For 10 parameters: 160 bytes copied per call

### Optimization: Parameter Array Split

**Key insight:** At runtime, parameter bindings only need their `value` field. The metadata (`scopes`, `source`, `bindingType`) is compile-time only.

**Proposal:** Split `LocalEnvironmentFrame` storage:

```go
type LocalEnvironmentFrame struct {
    keys          map[values.Symbol]int
    paramValues   []values.Value  // Direct storage for parameters
    letBindings   []*Binding      // Full bindings for let-vars
    paramCount    int             // Split point
}
```

**Apply optimization:**
```go
// BEFORE: Copy loop (N interface assignments)
for i := range bnds[:l] {
    bnds[i].SetValue(vs[i])
}

// AFTER: Direct slice assignment (zero copies!)
localEnv.paramValues = vs[:l]
```

**LoadLocal changes:**
```go
func (p *LocalEnvironmentFrame) GetBinding(index int) *Binding {
    if index < p.paramCount {
        // Parameter: wrap value in temporary Binding
        return &Binding{value: p.paramValues[index]}
    } else {
        // Let-binding: return actual Binding
        return p.letBindings[index - p.paramCount]
    }
}
```

**Benefits:**
- Eliminates N interface copies on every call
- Reduces memory (no Binding allocations for parameters)
- PopAll slice can be reused directly as parameter storage

**Costs:**
- LoadLocal needs conditional dispatch (index < paramCount check)
- CopyForApply needs to handle two arrays
- Increased code complexity
- Temporary Binding allocation for parameter access (but only when binding metadata is actually needed)

**Estimated impact:**
- High for functions with many parameters (10+)
- Medium for typical functions (3-5 params)
- Zero for zero-parameter functions

**Risk level:** Medium (structural change to environment representation)

**Estimated effort:** 8-12 hours (refactor LocalEnvironmentFrame, update all accessors, comprehensive testing)

**Measurement Results** (Apple M4 Max, via `machine/instrumentation_demo_test.go`):

```
Params | Time/Call | Time/Param | Total Allocs
-------|-----------|------------|-------------
1      | 18.7 ns   | 18.7 ns    | 168 B (4 allocs)
2      | 13.7 ns   | 6.9 ns     | 224 B (4 allocs)
5      | 16.9 ns   | 3.4 ns     | 432 B (4 allocs)
10     | 23.0 ns   | 2.3 ns     | 752 B (4 allocs)
20     | 31.9 ns   | 1.6 ns     | 1408 B (4 allocs)
```

**Analysis:**
- Parameter copying scales linearly: ~2-4 ns per parameter
- Total overhead is negligible: 14-32 ns per call
- 4 allocations per call are dominated by environment copying (CopyForApply), not the SetValue loop
- The SetValue loop itself appears to be **nearly free** (sub-nanosecond per param after setup)

**Verdict:** The parameter copy loop is **NOT a bottleneck**. The overhead is dominated by:
1. Environment allocation (CopyForApply): ~150-280 B per call
2. Time measurement overhead in the benchmark (~5-10 ns)
3. The copy loop itself: <1 ns per parameter

**Recommendation:** **Do not implement the parameter array split**. The complexity cost far exceeds the ~20-30 ns per call savings. The early arity check optimization we already implemented (avoiding CopyForApply on wrong-arity calls) delivers far more value.

### Alternative: Compiler-Level Optimization

**Question:** Could the compiler arrange stack values to avoid copying entirely?

For example, instead of:
```
1. Push arg to stack
2. Apply pops and copies to binding
3. LoadLocal reads from binding
```

Could we:
```
1. Compiler detects argument position
2. Emits direct LoadIntoBinding operation
3. Apply skips copy (bindings pre-populated)
```

**Challenges:**
- Requires whole-program analysis or sophisticated compile-time tracking
- Stack is used for evaluation of nested expressions, not just arguments
- Breaks clean separation between compiler and runtime
- Complex for variadic functions

**Verdict:** Too complex for benefit. Current design cleanly separates compilation from execution.

## Investigation Needed

### Binding Array Length Analysis

**Question:** Does `localEnv.Bindings()` ever return more bindings than the template's parameter count?

**Current behavior:**
```go
bnds := localEnv.Bindings()
l := tpl.ParameterCount()
for i := range bnds[:l] {  // Only populate first l bindings
    bnds[i].SetValue(vs[i])
}
```

**Hypothesis:** `CopyForApply()` copies ALL bindings from the closure's environment, but we only populate the first `l` slots (parameter count). If closures carry extra bindings beyond their parameters, we're copying unused memory.

**Investigation steps:**
1. Add instrumentation to log `len(bnds)` vs `l` in production workloads
2. Check if let-bindings or internal defines create extra slots in closure environments
3. If `len(bnds) > l` is common, consider:
   - Pre-allocating exactly `l` bindings in `CopyForApply()` (requires template parameter count as argument)
   - Or skipping copy of bindings beyond index `l`

**Risk level:** Low — optimization would be invisible if hypothesis is wrong

**Expected impact:** Unknown until measured

## Future Optimizations

### Variadic Argument List Representation

**Current:** `values.List(vs[l-1:]...)` constructs a linked Scheme list (O(n) allocations)

**Proposal:** Use `*values.ArrayList` instead of linked lists for variadic args

**Requirements:**
- Verify R7RS spec allows non-proper-list for rest args (likely does via `Tuple` interface)
- Audit all variadic primitive implementations to ensure they handle `ArrayList` via `Tuple` interface
- Check macro system — does pattern matching require proper lists?

**Expected impact:** Reduced allocations and better cache locality for variadic calls

**Risk level:** Medium — requires careful verification that all consumers handle `ArrayList`

**Estimated effort:** 2-4 hours audit + testing

### Counter Instrumentation Overhead

**Current:** Four counter updates per `Apply()` call:
```go
p.counters.ClosuresApplied++
p.counters.EnvsCopied++
p.counters.BindingsCopied += uint64(len(bnds))
p.counters.KeysShared++
```

**Proposal:** Conditional compilation — compile out in release builds

**Implementation:**
```go
//go:build profile
// +build profile

func (p *MachineContext) recordApply(n int) {
    p.counters.ClosuresApplied++
    p.counters.EnvsCopied++
    p.counters.BindingsCopied += uint64(n)
    p.counters.KeysShared++
}
```

```go
//go:build !profile
// +build !profile

func (p *MachineContext) recordApply(int) {}
```

**Expected impact:** Minimal (modern CPUs handle increments well), but non-zero

**Risk level:** Low

**Estimated effort:** 30 minutes

### Environment Frame Pooling

**Current:** Stack and continuation pooling exists; environments are GC'd

**Proposal:** Pool `LocalEnvironmentFrame` or `EnvironmentFrame` objects

**Challenges:**
- Closures capture environments (indefinite lifetime)
- Environment chains are mutable (parent pointers)
- Need escape analysis to identify pool-safe environments
- Risk of use-after-release bugs

**Expected impact:** Reduced GC pressure, faster allocation

**Risk level:** High — complex lifetime analysis required

**Estimated effort:** 8-16 hours design + implementation + testing

**Recommendation:** Defer until profiling shows environment allocation is a bottleneck

## Benchmarking Strategy

Before implementing any optimization:

1. Establish baseline with existing benchmarks:
   - `BenchmarkLocalFrameCopyForApply` (environment/)
   - End-to-end interpreter benchmarks (integration/)

2. Add microbenchmarks for specific scenarios:
   - Wrong-arity calls (now fast-fail)
   - Variadic calls with varying rest-arg counts
   - Deeply recursive functions (environment copy churn)

3. Profile real-world Scheme code:
   - Macro-heavy codebases (R7RS library loading)
   - Numeric-heavy code (minimal environment overhead)
   - Higher-order functions (map, fold, etc.)

## References

- `environment/local_environment_frame.go:175-200` — `CopyForApply()` implementation and batch allocation optimization
- `environment/environment_bench_test.go` — Existing benchmarks for environment operations
- `machine/pool.go` — Stack/continuation pooling patterns
- `BIBLIOGRAPHY.md` — "Copy-on-Write" references for CoW semantics
