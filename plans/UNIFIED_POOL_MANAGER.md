# Unified Pool Manager

Date: 2026-02-24
Status: Implemented

## Problem Statement

The three `sync.Pool` instances in `machine/pool.go` (stack, sub-context, continuation) each have hand-written acquire/release functions with ad-hoc counter instrumentation. There is no unified way to observe pool behavior, drain pools, or disable pooling for debugging. The acquire/release boilerplate is repetitive across all three pools.

## Goals

1. **Observability**: Unified stats (acquires, releases, misses, in-flight) across all pools without per-pool ad-hoc counters
2. **Code deduplication**: Replace repetitive nil-check/reset/put patterns with a shared generic abstraction
3. **Management controls**: Drain, disable, and query pools uniformly (e.g., disable for race detection, drain between benchmark runs)

## Non-Goals

- Adding new pooled types (Pair, Environment, etc.) — this is about unifying what exists
- Changing per-MC execution counters (`VMCounters`) — those serve a different purpose (per-program profiling vs global pool tuning)
- Moving pools out of `machine/` package

## Design

### Core Type: `Pool[T]`

```go
// Pool[T] is a type-safe, observable object pool backed by sync.Pool.
type Pool[T any] struct {
    name    string
    inner   sync.Pool
    reset   func(*T)         // called on Release before returning to pool
    stats   poolStats
    enabled atomic.Bool       // false = bypass pool (fresh alloc, no recycle)
}

type poolStats struct {
    acquires atomic.Uint64   // total Acquire calls
    releases atomic.Uint64   // total Release calls
    misses   atomic.Uint64   // times sync.Pool.New was invoked (cache miss)
}
```

**Constructor:**

```go
func NewPool[T any](name string, newFn func() *T, resetFn func(*T)) *Pool[T]
```

- `newFn`: allocates a fresh `*T` (called on pool miss)
- `resetFn`: clears `*T` for reuse (called during `Release`, before `Put`)

**Methods:**

| Method | Behavior |
|--------|----------|
| `Acquire() *T` | `stats.acquires++`; if enabled, `inner.Get()` (tracks miss if New called); if disabled, calls `newFn` directly |
| `Release(v *T)` | `stats.releases++`; calls `resetFn(v)`; if enabled, `inner.Put(v)` |
| `Drain()` | Calls `inner.Get()` in a loop until nil (forces GC to reclaim cached objects) |
| `SetEnabled(b bool)` | Toggles pool bypass |
| `Name() string` | Returns pool name |
| `Stats() PoolSnapshot` | Returns point-in-time counter snapshot |

**Note on Drain:** `sync.Pool` has no public drain API. The only way to clear it is to let a GC cycle run. `Drain()` will call `runtime.GC()` to trigger pool cleanup. This is appropriate for test/debug scenarios, not production hot paths.

### PoolHandle Interface

```go
// PoolHandle is the non-generic interface for managing heterogeneous pools.
type PoolHandle interface {
    Name() string
    Stats() PoolSnapshot
    Drain()
    SetEnabled(bool)
}

// PoolSnapshot is a point-in-time copy of pool counters.
type PoolSnapshot struct {
    Name     string
    Acquires uint64
    Releases uint64
    Misses   uint64
    InFlight uint64   // Acquires - Releases (approximate under concurrency)
}
```

`Pool[T]` implements `PoolHandle` automatically.

### PoolManager

```go
type PoolManager struct {
    mu    sync.RWMutex
    pools []PoolHandle
}

func NewPoolManager() *PoolManager
func (m *PoolManager) Register(h PoolHandle)
func (m *PoolManager) AllStats() []PoolSnapshot
func (m *PoolManager) DrainAll()
func (m *PoolManager) SetAllEnabled(b bool)
func (m *PoolManager) String() string   // tabular summary
```

### Package-Level Initialization

```go
var (
    pools            = NewPoolManager()
    stackPool        = registerPool(pools, NewPool("stack", newStack, resetStack))
    subContextPool   = registerPool(pools, NewPool("sub_context", newSubCtx, resetSubCtx))
    continuationPool = registerPool(pools, NewPool("continuation", newCont, resetCont))
)

// registerPool registers and returns the pool (enables var init chain).
func registerPool[T any](mgr *PoolManager, p *Pool[T]) *Pool[T] {
    mgr.Register(p)
    return p
}
```

## Migration

### Stack Pool

**Reset callback:**
```go
func resetStack(s *Stack) {
    full := (*s)[:cap(*s)]
    for i := range full {
        full[i] = nil
    }
    *s = full[:0]
}
```

**Acquire/release become one-liners:**
```go
func acquireStack() *Stack          { return stackPool.Acquire() }
func releaseStack(s *Stack)         { if s != nil { stackPool.Release(s) } }
```

### Continuation Pool

**Reset callback (cascading release to stack pool):**
```go
func resetContinuation(cont *MachineContinuation) {
    releaseStack(cont.evals)
    *cont = MachineContinuation{}
}
```

**Acquire/release become one-liners:**
```go
func acquireContinuation() *MachineContinuation { return continuationPool.Acquire() }
func releaseContinuation(cont *MachineContinuation) {
    if cont != nil { continuationPool.Release(cont) }
}
```

### SubContext Pool

**Reset callback:**
```go
func resetSubContext(mc *MachineContext) {
    releaseStack(mc.evals)
    *mc = MachineContext{}
}
```

**`ReleaseSubContext` stays as a domain wrapper** — it handles per-MC counter increment before delegating to pool:
```go
func ReleaseSubContext(mc *MachineContext) {
    if mc == nil { return }
    if mc.parentMC != nil {
        mc.parentMC.counters.SubContextPoolReleases++
    }
    subContextPool.Release(mc)
}
```

**`acquireMacroContext` is unchanged** — it's a composite helper, not a pool.

### Counter Relationship

| Counter Layer | Scope | Purpose |
|---------------|-------|---------|
| `PoolSnapshot` (Pool[T]) | Global, atomic | Pool tuning: hit rate, capacity, overall throughput |
| `VMCounters.*PoolReleases` (MachineContext) | Per-execution, plain uint64 | Program profiling: "how many pool ops did THIS program do?" |

Both are kept. They answer different questions.

### Call Site Changes

No changes to code that calls `acquireStack()`, `releaseStack()`, `acquireContinuation()`, `releaseContinuation()`, `acquireSubContext()`, or `ReleaseSubContext()`. The function signatures are preserved. Only the internal implementation changes.

## File Plan

| File | Action |
|------|--------|
| `machine/pool_generic.go` | **New** — `Pool[T]`, `PoolHandle`, `PoolSnapshot`, `PoolManager`, `poolStats` |
| `machine/pool_generic_test.go` | **New** — tests for generic pool, manager, stats, drain, enable/disable |
| `machine/pool.go` | **Modify** — replace `sync.Pool` vars with `Pool[T]` instances, simplify acquire/release funcs |
| `machine/pool_test.go` | **Modify** — existing tests should pass unchanged (API preserved), add pool stats assertions |

## Risks

1. **`Drain()` via `runtime.GC()`**: Acceptable for debug/test but documents that it's not a surgical drain.
2. **Atomic counter overhead**: Each acquire/release adds two atomic ops (`Add(1)` + `enabled.Load()`). See benchmark results below.
3. **Reset callback ordering**: `resetContinuation` calls `releaseStack`, which calls `stackPool.Release`. If pools are disabled, `releaseStack` becomes a no-op for the Put but still runs the nil-clearing. This is correct — the object just won't be recycled.

## Benchmark Results

Gabriel benchmarks (3-run medians, `make bench-gabriel-compare`):

| Benchmark | master (e786b95) | Pool[T] (a182dcd) | Delta |
|-----------|-------------------|---------------------|-------|
| tak | 0.302s | 0.324s | +7.3% |
| fib | 1.001s | 1.062s | +6.1% |
| deriv | 0.182s | 0.191s | +4.9% |
| peval | 0.132s | 0.137s | +3.8% |

**Analysis**: The overhead comes from 4 extra atomic operations per pool acquire+release cycle (2 on acquire: `acquires.Add(1)` + `enabled.Load()`; 2 on release: `releases.Add(1)` + `enabled.Load()`). For fib(30), which creates ~2.7M continuation save/restore cycles, this is ~10.8M extra atomic ops. At ~5-10ns per uncontended atomic on x86-64, that's ~50-100ms on a ~1s benchmark, consistent with the observed ~60ms delta.

**Verdict**: This is the expected cost of observability. The counters can be compiled out or gated behind a build tag if the overhead becomes unacceptable for production use. For now, the observability benefit outweighs the cost.
