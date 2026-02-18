# VM Performance Instrumentation

This document describes the performance instrumentation added to measure parameter copying overhead during procedure application.

## What's Instrumented

The `MachineContext.Apply()` method now measures time spent copying parameters from the eval stack to bindings:

```go
// machine/machine_context.go:336-344
copyStart := time.Now()
if !tpl.IsVariadic() {
    for i := range bnds[:l] {
        bnds[i].SetValue(vs[i])
    }
} else {
    // ... variadic parameter handling
}
p.counters.ParamCopyTimeNanos += uint64(time.Since(copyStart).Nanoseconds())
```

## Viewing Results

The `VMCounters.String()` method includes parameter copy timing:

```
ops_executed:                 1234
closures_applied:             100
...
param_copy_time_ns:           3456 (0.003 ms)
```

## Running the Demo

```bash
# Run the instrumentation demo test
go test ./machine -run TestParameterCopyInstrumentation -v

# Run benchmarks with detailed metrics
go test ./machine -bench BenchmarkParameterCopy -benchmem
```

## Interpreting Results

**TestParameterCopyInstrumentation** shows:
- Total time spent in SetValue loops across all calls
- Average time per call
- Average time per parameter
- Percentage of total execution time

**BenchmarkParameterCopy** shows:
- ns/op: Total time per Apply call (including env copy, arity check, etc.)
- ns/call: Time in SetValue loop only (from instrumentation)
- ns/param: Time per individual parameter copy
- B/op: Bytes allocated per call
- allocs/op: Heap allocations per call

## Measured Performance (Apple M4 Max)

```
Parameters | SetValue Time | Total Apply Time | Allocations
-----------|---------------|------------------|------------
1          | 18.7 ns       | 98.9 ns          | 168 B (4 allocs)
2          | 13.7 ns       | 106 ns           | 224 B (4 allocs)
5          | 16.9 ns       | 144 ns           | 432 B (4 allocs)
10         | 23.0 ns       | 191 ns           | 752 B (4 allocs)
20         | 31.9 ns       | 302 ns           | 1408 B (4 allocs)
```

**Key findings:**
- Parameter copying is ~2-4 ns per parameter (sub-nanosecond after setup)
- Total overhead: 14-32 ns per call (negligible)
- Allocation cost dominates: CopyForApply allocates environment + bindings
- The SetValue loop itself is NOT a bottleneck

## Instrumentation Overhead

The `time.Now()` / `time.Since()` pair adds ~5-10 ns overhead per Apply call. This is acceptable for profiling but should not be used in production builds.

To disable instrumentation in production:
1. Remove the `copyStart := time.Now()` line
2. Remove the `p.counters.ParamCopyTimeNanos += ...` line
3. Or use build tags to conditionally compile instrumentation code

## Future Work

If parameter copying ever becomes a bottleneck (unlikely based on current measurements):

1. **Parameter Array Split** (see `plans/APPLY_OPTIMIZATION_OPPORTUNITIES.md`)
   - Split `LocalEnvironmentFrame` into `paramValues []values.Value` + `letBindings []*Binding`
   - Eliminates SetValue loop by direct slice assignment
   - Complexity cost: significant refactoring
   - Expected gain: ~20-30 ns per call (not worth it)

2. **Compiler-Level Optimization**
   - Emit bytecode that pre-populates bindings during argument evaluation
   - Avoids intermediate stack storage
   - Complexity cost: breaks compiler/runtime separation
   - Expected gain: unclear, likely small

Neither optimization is recommended based on current profiling.
