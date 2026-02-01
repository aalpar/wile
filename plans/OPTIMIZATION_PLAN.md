# Wile Optimization Plan

## Overview

Optimize the Wile Scheme interpreter for reduced CPU time, improved memory utilization, and better memory locality.

## Current Bottlenecks Identified

| Area | Issue | Impact |
|------|-------|--------|
| **VM dispatch** | Interface dispatch via `Operation.Apply()` on every instruction | CPU |
| **Per-iteration checks** | `ctx.Done()` select + debugger check every instruction | CPU |
| **PopAll clone** | `Stack.PopAll()` clones entire stack unnecessarily | Memory |
| **Environment copy** | Fresh environment copy on every function call | Memory |
| **No pair pooling** | Cons cells allocated fresh, never pooled | Memory/GC |
| **No benchmarks** | No profiling infrastructure to measure improvements | Foundation |

---

## Phase 0: Profiling Infrastructure (PREREQUISITE)

**Must complete first** - without benchmarks, optimizations are guesswork.

### 0.1 Create Benchmark Suite

**New file:** `go/machine/benchmark_test.go`

```go
func BenchmarkFibonacciRecursive(b *testing.B) // Function call overhead
func BenchmarkListReverse(b *testing.B)        // Cons cell allocation
func BenchmarkStackOperations(b *testing.B)    // Stack push/pop/popall
func BenchmarkEnvironmentCopy(b *testing.B)    // Closure application
```

### 0.2 Add pprof Integration

**File:** `go/cmd/main.go`
- Add `--profile-port` flag
- Start pprof HTTP server when enabled

### 0.3 Add Makefile Targets

**File:** `go/Makefile`
```makefile
bench:      $(GO_TEST) -bench=. -benchmem ./...
bench-cpu:  $(GO_TEST) -bench=. -cpuprofile=cpu.prof ./machine/...
bench-mem:  $(GO_TEST) -bench=. -memprofile=mem.prof ./machine/...
```

---

## Phase 1: Quick Wins (Low Effort)

### 1.1 Fix PopAll Clone

**File:** `go/machine/stack.go:77-81`

**Current:**
```go
func (p *Stack) PopAll() []values.Value {
    q := p.Copy()  // FULL CLONE via slices.Clone
    p.Clear()
    return *q
}
```

**Fixed:**
```go
func (p *Stack) PopAll() []values.Value {
    q := []values.Value(*p)
    *p = (*p)[:0]  // Clear in place, no allocation
    return q
}
```

### 1.2 Reduce Per-Iteration Overhead

**File:** `go/machine/machine_context.go:227-254`

Split into two loop variants:
```go
func (p *MachineContext) Run() error {
    if p.debugger != nil {
        return p.runWithDebugger()
    }
    return p.runFast()
}

func (p *MachineContext) runFast() error {
    // Check context every 64 iterations, not every one
    const checkInterval = 64
    iter := 0
    for mc.pc < len(mc.template.operations) {
        if iter&(checkInterval-1) == 0 {
            select {
            case <-mc.ctx.Done():
                return mc.ctx.Err()
            default:
            }
        }
        iter++
        mc, err = mc.template.operations[mc.pc].Apply(mc.ctx, mc)
        // ...
    }
}
```

---

## Phase 2: Object Pooling with sync.Pool

### 2.1 Stack Pool

**File:** `go/machine/stack.go`

```go
var stackPool = sync.Pool{
    New: func() interface{} {
        s := make(Stack, 0, 16)
        return &s
    },
}

func NewPooledStack() *Stack { return stackPool.Get().(*Stack) }
func (p *Stack) Release()    { p.Clear(); stackPool.Put(p) }
```

**Usage sites:**
- `machine_context.go`: `NewSubContext()`
- `machine_continuation.go`: continuation save/restore

### 2.2 Continuation Pool

**File:** `go/machine/machine_continuation.go`

```go
var continuationPool = sync.Pool{
    New: func() interface{} { return &MachineContinuation{} },
}
```

### 2.3 Pair Pool

**File:** `go/values/pair.go`

```go
var pairPool = sync.Pool{
    New: func() interface{} { return &Pair{} },
}

func NewPooledCons(car, cdr Value) *Pair {
    p := pairPool.Get().(*Pair)
    p[0], p[1] = car, cdr
    return p
}
```

**Note:** Pairs have variable lifetimes - need careful release strategy. Consider pooling only for temporary pairs in list operations.

---

## Phase 3: Switch-Based Dispatch (Moderate Refactor)

Replace interface dispatch with switch statement for ~10-20% CPU reduction.

### 3.1 Add Opcode Enum

**New file:** `go/machine/opcode.go`

```go
type Opcode uint8

const (
    OpPush Opcode = iota
    OpPop
    OpPopAll
    OpLoadVoid
    OpLoadLiteralInteger
    OpLoadLiteralByIndex
    OpLoadLocalByIndex
    OpStoreLocalByIndex
    OpLoadGlobalByIndex
    OpStoreGlobalByIndex
    OpBranchOffset
    OpBranchOnFalse
    OpBranchOnNotFalse
    OpSaveContinuation
    OpRestoreContinuation
    OpMakeClosure
    OpApply
    OpForeignFunctionCall
    // ... ~26 total opcodes
)
```

### 3.2 Compact Instruction Type

**New file:** `go/machine/instruction.go`

```go
type Instruction struct {
    opcode   Opcode
    operand1 int32
    operand2 int32
}
```

### 3.3 Switch-Based VM Loop

**File:** `go/machine/machine_context.go`

```go
func (p *MachineContext) runSwitch() error {
    for p.pc < len(p.template.instructions) {
        inst := p.template.instructions[p.pc]
        switch inst.opcode {
        case OpPush:
            p.evals.PushAll(p.value)
            p.pc++
        case OpPop:
            p.value = []values.Value{p.evals.Pop()}
            p.pc++
        case OpLoadLiteralByIndex:
            p.value = []values.Value{p.template.literals[inst.operand1]}
            p.pc++
        // ... other cases
        }
    }
    return nil
}
```

### 3.4 Migration Strategy

1. Add `NativeTemplate.instructions []Instruction` alongside existing `operations`
2. Populate instructions during compilation
3. Use switch dispatch in production, interface dispatch for debugging
4. Eventually deprecate `operations` slice

---

## Phase 4: Environment Optimization

### 4.1 Copy-on-Write Environments

**File:** `go/environment/local_environment_frame.go`

Add `shared` flag to avoid copying until mutation:

```go
type LocalEnvironmentFrame struct {
    keys     map[values.Symbol]int
    bindings []*Binding
    shared   bool  // NEW
}

func (p *LocalEnvironmentFrame) SetLocalValue(li *LocalIndex, v values.Value) error {
    if p.shared {
        p.copyOnWrite()
    }
    // ... existing code
}
```

**Benefit:** Many closures never mutate their environment - avoid copy entirely.

---

## Phase 5: Tagged Integers (Major Refactor - Future)

Use lower bits of pointers for type tags to avoid boxing small integers.

```go
type TaggedValue uintptr

const tagInteger = 0x1  // Low bit set = immediate integer

func (p TaggedValue) IsInteger() bool { return v&1 != 0 }
func (p TaggedValue) AsInteger() int64 { return int64(v) >> 1 }
```

**Benefits:**
- No allocation for integers -2^62 to 2^62
- Faster type checks (bit op vs interface assertion)
- Better cache locality

**Risks:**
- Major refactor of `values.Value` interface
- Uses `unsafe` package
- Deferred to later phase

---

## Implementation Priority

| Phase | Effort | Expected Impact | Priority |
|-------|--------|-----------------|----------|
| Phase 0 | 2-3 days | Foundation | **MUST DO FIRST** |
| Phase 1.1 | 1 day | Medium | Critical |
| Phase 1.2 | 1 day | Medium | High |
| Phase 2 | 5-7 days | High (30-50% fewer allocs) | High |
| Phase 3 | 7-10 days | Medium (10-20% CPU) | Medium |
| Phase 4 | 5-7 days | Medium | Medium |
| Phase 5 | 3-4 weeks | Very High | Future |

---

## Critical Files

| File | Changes |
|------|---------|
| `go/machine/machine_context.go:227-254` | VM loop, context checks |
| `go/machine/stack.go:77-81` | PopAll clone fix, pooling |
| `go/machine/machine_continuation.go` | Continuation pooling |
| `go/values/pair.go:36-39` | Pair pooling |
| `go/environment/local_environment_frame.go` | Copy-on-write |
| `go/machine/operation.go` | Switch dispatch conversion |
| `go/cmd/main.go` | pprof integration |
| `go/Makefile` | Benchmark targets |

---

## Verification

After each phase:

1. Run benchmarks: `make bench`
2. Compare allocation counts via `-benchmem`
3. Profile CPU: `make bench-cpu && go tool pprof cpu.prof`
4. Run existing tests: `make test`
5. Test REPL interactivity preserved (Ctrl+C still works after context check reduction)

**Success metrics:**
- Phase 1: 10-25% improvement on recursive Fibonacci
- Phase 2: 30-50% reduction in allocations
- Phase 3: Additional 10-20% CPU reduction
- Combined: 40-60% total improvement on function-heavy workloads
