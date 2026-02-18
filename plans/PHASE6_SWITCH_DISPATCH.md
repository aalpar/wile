# Phase 6: Switch Dispatch Implementation Plan

**Status:** IN PROGRESS — Phases 6.1–6.3 complete, Phases 6.4–6.6 remaining
**Date:** 2026-02-17 (updated 2026-02-18)
**Design Reference:** `INTEGER_OPCODE_DISPATCH.md`
**Parent Plan:** `PERFORMANCE_REFACTORING_PLAN.md`

## Overview

Replace interface-based operation dispatch with switch-based dispatch using integer opcodes and a compact instruction representation. Following **Option D (Hybrid)** from `INTEGER_OPCODE_DISPATCH.md`: migrate hot-path operations to integer dispatch while keeping complex operations as interface values in a side table.

**Expected impact:** 10–20% CPU improvement on tight numeric loops; moderate gains on general workloads.

## Operation Inventory (Verified from Code)

**30 operation types total** (excluding `OperationBase`). Categorized by operand count:

### Zero-Operand (8) — Wave 1

| Operation | File | Notes |
|-----------|------|-------|
| `OperationPush` | `operation_push.go` | Pushes value register to eval stack |
| `OperationPop` | `operation_pop.go` | Pops eval stack to value register |
| `OperationPull` | `operation_pull.go` | Dequeues bottom of eval stack |
| `OperationLoadVoid` | `operation_load_void.go` | Loads void value |
| `OperationDrop` | `operation_drop.go` | Discards top of eval stack (no count param) |
| `OperationPopEnv` | `operation_pop_env.go` | Restores parent environment |
| `OperationApply` | `operation_apply.go` | Dispatches to closures, case-lambda, parameters |
| `OperationRestoreContinuation` | `operation_restore_continuation.go` | Restores saved continuation |

### Single-Operand (7) — Wave 2

| Operation | Field | Type | File |
|-----------|-------|------|------|
| `OperationBranchOnFalseValueOffsetImmediate` | `Offset` | `int` | `operation_branch_on_false_value_offset_immediate.go` |
| `OperationBranchOffsetImmediate` | `Offset` | `int` | `operation_branch_offset_immediate.go` |
| `OperationSaveContinuationOffsetImmediate` | `Offset` | `int` | `operation_save_continuation_offset_immediate.go` |
| `OperationLoadLiteralByLiteralIndexImmediate` | `LiteralIndex` | `LiteralIndex` (`int`) | `operation_load_literal_by_literal_index_immediate.go` |
| `OperationLoadGlobalByGlobalIndexLiteralIndexImmediate` | `LiteralIndex` | `LiteralIndex` (`int`) | `operation_load_global_by_global_index_literal_index_immediate.go` |
| `OperationStoreGlobalByGlobalIndexLiteralIndexImmediate` | `LiteralIndex` | `LiteralIndex` (`int`) | `operation_store_global_by_global_index_literal_index_immediate.go` |
| `OperationPeekK` | `Depth` | `int` | `operation_peek_m.go` |

### Two-Operand (2) — Wave 3

| Operation | Field | Type | File |
|-----------|-------|------|------|
| `OperationLoadLocalByLocalIndexImmediate` | `LocalIndex` | `*environment.LocalIndex` | `operation_load_local_by_local_index_immediate.go` |
| `OperationStoreLocalByLocalIndexImmediate` | `LocalIndex` | `*environment.LocalIndex` | `operation_store_local_by_local_index_immediate.go` |

**Note:** `LocalIndex` is `[2]int` (see `environment/local_index.go:32`), where `[0]` = slot (over), `[1]` = depth (up). On 64-bit platforms, this is 16 bytes.

### Complex — Keep as Interface (13)

| Operation | Payload | File |
|-----------|---------|------|
| `OperationSyntaxRulesTransform` | clauses wrapper, scopes | `operation_syntax_rules_transform.go` |
| `OperationSyntaxCaseMatch` | matcher, pattern data | `operation_syntax_case.go` |
| `OperationBindPatternVars` | pattern bindings | `operation_syntax_case.go` |
| `OperationSyntaxCaseNoMatch` | error data | `operation_syntax_case.go` |
| `OperationSyntaxTemplateExpand` | template data | `operation_syntax_case.go` |
| `OperationStoreSyntaxCaseInput` | — | `operation_syntax_case.go` |
| `OperationClearSyntaxCaseInput` | — | `operation_syntax_case.go` |
| `OperationBuildSyntaxList` | syntax builder config | `operation_build_syntax.go` |
| `OperationMakeClosure` | template pointer | `operation_make_closure.go` |
| `OperationMakeCaseLambdaClosure` | multiple templates | `operation_make_case_lambda_closure.go` |
| `OperationPushWind` | thunk closures | `operation_push_wind.go` |
| `OperationPopWind` | thunk closures | `operation_pop_wind.go` |
| `OperationForeignFunctionCall` | Go function pointer | `operation_foreign_function_call.go` |

**Totals:** 17 migrated to integer dispatch + 13 remain as interface = 30.

## Design Summary (from INTEGER_OPCODE_DISPATCH.md)

```go
type OpCode uint16

type Instruction struct {
    Op  OpCode
    Arg int  // immediate operand OR side table index
}
// Size on 64-bit: 16 bytes (uint16 + 6 padding + int)

type NativeTemplate struct {
    code      []Instruction
    sideTable []Operation  // complex ops only
    literals  []values.Value
    // ...existing fields
}
```

VM loop becomes:
```go
for mc.pc < len(mc.template.code) {
    instr := mc.template.code[mc.pc]
    switch instr.Op {
    case OpPush:
        // inline implementation
        mc.pc++
    case OpLoadLocal:
        // inline implementation using instr.Arg
        mc.pc++
    // ...
    case OpComplex:
        mc, err = mc.template.sideTable[instr.Arg].Apply(ctx, mc)
    }
}
```

## Migration Strategy: Incremental by Priority

Migrate operations in **three waves**, benchmarking after each wave. Stop if gains plateau.

### Wave 1: Zero-Operand Hot Path

Simplest operations with highest execution frequency. No operand encoding needed.

| OpCode | Current Type | Frequency | Complexity |
|--------|--------------|-----------|------------|
| OpPush | OperationPush | Very High | Trivial |
| OpPop | OperationPop | Very High | Trivial |
| OpPull | OperationPull | High | Simple |
| OpLoadVoid | OperationLoadVoid | Medium | Trivial |
| OpDrop | OperationDrop | Medium | Simple |
| OpPopEnv | OperationPopEnv | Medium | Simple |
| OpApply | OperationApply | Very High | Complex body, simple dispatch |
| OpRestoreContinuation | OperationRestoreContinuation | High | Complex body, simple dispatch |

**Estimated LOC:** ~200 lines (opcode defs, switch cases, tests)
**Risk:** Low (no operand encoding, straightforward translation)

### Wave 2: Single-Operand Operations

Operations with one integer operand (offset, index, depth).

| OpCode | Current Type | Operand | Notes |
|--------|--------------|---------|-------|
| OpBranchOnFalseValue | OperationBranchOnFalseValueOffsetImmediate | int (offset) | Phase 5 peephole; reads value register directly |
| OpBranch | OperationBranchOffsetImmediate | int (offset) | |
| OpSaveContinuation | OperationSaveContinuationOffsetImmediate | int (offset) | |
| OpLoadLiteral | OperationLoadLiteralByLiteralIndexImmediate | LiteralIndex (int) | |
| OpLoadGlobal | OperationLoadGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex (int) | Indexes literals pool |
| OpStoreGlobal | OperationStoreGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex (int) | Indexes literals pool |
| OpPeekK | OperationPeekK | int (depth) | |

**Estimated LOC:** ~250 lines
**Risk:** Low (single operand, no struct encoding)

### Wave 3: Two-Operand Operations

Operations requiring two integer parameters: slot and depth from `LocalIndex`.

| OpCode | Current Type | Operand | Notes |
|--------|--------------|---------|-------|
| OpLoadLocal | OperationLoadLocalByLocalIndexImmediate | `*LocalIndex` (`[2]int`) | Most frequent |
| OpStoreLocal | OperationStoreLocalByLocalIndexImmediate | `*LocalIndex` (`[2]int`) | |

**LocalIndex encoding:**

`LocalIndex` is `[2]int` (`environment/local_index.go:32`), where `[0]` = slot (over), `[1]` = depth (up). On 64-bit platforms each `int` is 8 bytes.

The `Instruction{Op OpCode; Arg int}` struct already has a single `int` field. For two-operand ops, pack both values into `Arg`:

```go
// Encode: slot in low 32 bits, depth in high 32 bits
// Safe because slot/depth values never exceed 32-bit range in practice
arg := (depth << 32) | (slot & 0xFFFFFFFF)

// Decode:
slot := int(int32(instr.Arg))       // sign-extend low 32 bits
depth := int(int32(instr.Arg >> 32)) // sign-extend high 32 bits
```

This avoids changing the Instruction struct between waves. The bit-packing is confined to LoadLocal/StoreLocal encode/decode helpers.

**Estimated LOC:** ~100 lines (encode/decode helpers + switch cases)
**Risk:** Medium (bit-packing correctness, but easily tested)

## Implementation Phases

### Phase 6.1: Infrastructure

**Goal:** Add new types without breaking existing code.

1. **Define OpCode enum** (`machine/opcode.go`):
   ```go
   type OpCode uint16

   const (
       OpInvalid OpCode = iota
       // Wave 1: zero-operand
       OpPush
       OpPop
       OpPull
       OpLoadVoid
       OpDrop
       OpPopEnv
       OpApply
       OpRestoreContinuation
       // Wave 2: single-operand
       OpBranchOnFalseValue
       OpBranch
       OpSaveContinuation
       OpLoadLiteral
       OpLoadGlobal
       OpStoreGlobal
       OpPeekK
       // Wave 3: two-operand (bit-packed into Arg)
       OpLoadLocal
       OpStoreLocal
       // Fallback: dispatch to sideTable
       OpComplex
   )
   ```

2. **Define Instruction struct** (`machine/instruction.go`):
   ```go
   type Instruction struct {
       Op  OpCode  // uint16
       Arg int     // immediate operand, side table index, or bit-packed local index
   }
   // Size on 64-bit: 16 bytes (uint16 + 6 padding + int)
   ```

3. **Add new fields to NativeTemplate** (`machine/native_template.go`):
   ```go
   type NativeTemplate struct {
       // NEW: integer dispatch
       code      []Instruction
       sideTable []Operation

       // EXISTING: keep for backward compat during migration
       operations []Operation

       // ...existing fields (literals, sourceMap, etc.)
   }
   ```

4. **Add migration flag to CompileTimeContinuation**:
   ```go
   type CompileTimeContinuation struct {
       useIntegerOps bool  // flag to control which path to use
       // ...existing fields
   }
   ```

**Tests:**
- `TestOpcodeConstants` — verify OpCode enum values
- `TestInstructionSize` — verify sizeof(Instruction) == 16 bytes on 64-bit
- `TestNativeTemplateBackwardCompat` — operations field still works

### Phase 6.2: Dual-Mode VM Loop

**Goal:** VM can execute both old and new bytecode formats.

1. **Add Run() dispatch selector** (`machine/machine_context.go`):
   ```go
   func (p *MachineContext) Run() error {
       if len(p.template.code) > 0 {
           return p.runIntegerDispatch()
       }
       return p.runInterfaceDispatch()  // existing loop
   }
   ```

2. **Implement runIntegerDispatch()** (initially empty switch):
   ```go
   func (p *MachineContext) runIntegerDispatch() error {
       for p.pc < len(p.template.code) {
           // Context cancellation check (same as existing)
           if p.counters.OpsExecuted&contextCheckMask == 0 {
               select {
               case <-p.ctx.Done():
                   return p.ctx.Err()
               default:
               }
           }

           // Debugger check (same as existing)
           if p.debugger != nil {
               // ...
           }

           instr := p.template.code[p.pc]
           p.counters.OpsExecuted++

           var err error
           switch instr.Op {
           case OpComplex:
               p, err = p.template.sideTable[instr.Arg].Apply(p.ctx, p)
           default:
               return fmt.Errorf("unimplemented opcode: %d", instr.Op)
           }

           if err != nil {
               if errors.Is(err, errHalt) {
                   return nil
               }
               return err
           }
       }
       return nil
   }
   ```

3. **Rename existing Run() to runInterfaceDispatch()**:
   ```go
   func (p *MachineContext) runInterfaceDispatch() error {
       // Existing implementation unchanged
   }
   ```

**Tests:**
- `TestDualModeDispatch` — both paths execute correctly
- `TestBackwardCompatibility` — existing templates still work

### Phase 6.3: Wave 1 Migration

**Goal:** Migrate zero-operand hot-path operations.

For each operation in Wave 1:

1. **Add switch case to runIntegerDispatch()**:
   ```go
   case OpPush:
       if p.multiValues != nil {
           p.evals.PushAll(p.multiValues)
       } else if p.singleValue != nil {
           p.evals.Push(p.singleValue)
       }
       p.pc++
   ```
2. **Update compiler to emit Instruction** (`machine/compile_*.go`):
   ```go
   // OLD:
   tmpl.AppendOperations(NewOperationPush())

   // NEW (with flag check):
   if ctc.useIntegerOps {
       tmpl.AppendInstruction(Instruction{Op: OpPush})
   } else {
       tmpl.AppendOperations(NewOperationPush())
   }
   ```

3. **Add unit test** for the opcode case
4. **Add integration test** comparing old vs new output

**Compiler changes:**
- `compile_validated.go` — most operations emitted here (~30 emit sites for migrable ops)
- `compile_syntax_case.go` — syntax-case branches
- `compile_time_continuation.go` — symbol resolution emit sites
- Helper: `CompileTimeContinuation.emitOp()` wrapper

**Tests per operation:**
- Unit: `TestOp<Name>` — switch case logic
- Integration: `TestCompile<Form>IntegerOps` — full compilation

**Benchmark after Wave 1:**
```bash
make bench > bench-baseline.txt  # before
# ... apply Wave 1 changes, set useIntegerOps=true
make bench > bench-wave1.txt     # after
benchstat bench-baseline.txt bench-wave1.txt
```

**Decision gate:** If gains < 5%, stop here and document findings.

### Phase 6.4: Wave 2 Migration

Same process as 6.3, but for single-operand operations. Operand encoding:

```go
case OpBranchOnFalseValue:
    // Reads value register directly (no stack pop).
    // This is the Phase 5 peephole optimization that replaced
    // the Push+BranchOnFalse+Pop pattern.
    if !values.ValueToBool(p.GetValue()) {
        p.pc += instr.Arg  // offset from instruction
    } else {
        p.pc++
    }
```

Compiler emits offset:
```go
instr := Instruction{Op: OpBranchOnFalseValue, Arg: offset}
tmpl.AppendInstruction(instr)
```

**Benchmark after Wave 2:** Same process as 6.3.

### Phase 6.5: Wave 3 Migration

Two-operand operations use bit-packing into the existing `Arg int` field:

```go
// Encode helper:
func EncodeLocalIndex(li *environment.LocalIndex) int {
    return (li.Up() << 32) | (li.Over() & 0xFFFFFFFF)
}

// Decode in switch case:
case OpLoadLocal:
    slot := int(int32(instr.Arg))
    depth := int(int32(instr.Arg >> 32))
    li := environment.NewLocalIndex(slot, depth)
    bd := p.env.GetLocalBinding(li)
    if bd == nil {
        return p.Error(fmt.Sprintf("no such local binding %s", li))
    }
    p.SetValue(bd.Value())
    p.pc++
```

No Instruction struct change required — same `{Op OpCode; Arg int}` used throughout all waves.

**Tests:**
- `TestLocalIndexBitPacking` — round-trip encode/decode
- `TestLocalIndexBitPackingEdgeCases` — zero values, max values
- `TestLoadLocalIntegerOp` — full integration

**Benchmark after Wave 3:** Same process.

### Phase 6.6: Cleanup and Stabilization

1. **Remove migration flag:** Set `useIntegerOps = true` permanently
2. **Delete old code paths:**
   - Remove `operations []Operation` field (keep `sideTable` only)
   - Remove `runInterfaceDispatch()`
   - Remove old operation emit paths from compiler
3. **Delete unused operation struct files** (Wave 1-3 operations only):
   ```
   machine/operation_push.go
   machine/operation_pop.go
   machine/operation_pull.go
   machine/operation_load_void.go
   machine/operation_drop.go
   machine/operation_pop_env.go
   machine/operation_apply.go
   machine/operation_restore_continuation.go
   machine/operation_branch_on_false_value_offset_immediate.go
   machine/operation_branch_offset_immediate.go
   machine/operation_save_continuation_offset_immediate.go
   machine/operation_load_literal_by_literal_index_immediate.go
   machine/operation_load_global_by_global_index_literal_index_immediate.go
   machine/operation_store_global_by_global_index_literal_index_immediate.go
   machine/operation_peek_m.go
   machine/operation_load_local_by_local_index_immediate.go
   machine/operation_store_local_by_local_index_immediate.go
   ```
   **17 files deleted.**

4. **Update documentation:**
   - `machine/CLAUDE.local.md` — document new dispatch architecture
   - `PERFORMANCE_REFACTORING_PLAN.md` — mark Phase 6 complete
   - Add gains summary to `INTEGER_OPCODE_DISPATCH.md`

**Tests:**
- Full test suite: `make test`
- Benchmarks: `make bench`
- Integration: `make bench-schelog`
- Hygiene: run macro test suite

### Phase 6.7: Optional — ForeignFunctionCall Optimization

If benchmarks show FFI dispatch is still hot, consider dedicated function pointer table:

```go
type NativeTemplate struct {
    code      []Instruction
    sideTable []Operation
    fnTable   []ForeignFunc  // NEW: dedicated table for primitives
    // ...
}

case OpForeignCall:
    fn := tmpl.fnTable[instr.Arg]
    mc, err = fn(ctx, mc)
```

**Note:** Every primitive call (`+`, `-`, `car`, `cdr`, `cons`, `null?`, ...) dispatches through `OperationForeignFunctionCall`. For scripting workloads — the project's stated target — this is the dominant hot-path operation. If Wave 1-3 gains are smaller than expected on realistic workloads, FFI dispatch is likely the reason.

## Critical Files

### New Files

| File | Purpose | LOC |
|------|---------|-----|
| `machine/opcode.go` | OpCode enum, String() methods | 100 |
| `machine/instruction.go` | Instruction struct, bit-packing helpers | 80 |

### Modified Files

| File | Changes | LOC Delta |
|------|---------|-----------|
| `machine/machine_context.go` | Add runIntegerDispatch(), switch statement | +300 |
| `machine/native_template.go` | Add code/sideTable fields, AppendInstruction() | +50 |
| `machine/compile_validated.go` | Emit Instruction instead of Operation (~30 sites) | +100 |
| `machine/compile_syntax_case.go` | Emit Instruction for branches | +50 |
| `machine/compile_time_continuation.go` | Add emitOp() helper (~5 sites) | +30 |

### Deleted Files (Phase 6.6)

17 operation files for migrated operations (see Phase 6.6 list above).

**Net LOC:** ~+710 new, ~-935 deleted = **~-225 LOC**

## Verification Strategy

### After Each Wave

1. **Unit tests pass:** `go test -v ./machine/...`
2. **Benchmarks captured:** `make bench > bench-wave<N>.txt`
3. **Performance comparison:**
   ```bash
   benchstat bench-baseline.txt bench-wave<N>.txt
   ```
4. **Integration tests pass:** `make test`
5. **Hygiene tests pass:** macro expansion correctness
6. **REPL responsive:** Ctrl+C cancellation works
7. **Debugger works:** breakpoints trigger correctly

### Before Final Cleanup (Phase 6.6)

1. **Performance validation:** Gains match or exceed 10% target
2. **No regressions:** Check Schelog benchmark, Gabriel benchmarks
3. **Memory profile:** `make profile-mem` — verify no allocation increase
4. **CPU profile:** `make profile-cpu` — verify dispatch improvement visible

## Risk Mitigation

### High Risk: VM Loop Correctness

**Risk:** Switch statement bugs break execution.

**Mitigation:**
- Dual-mode execution during migration (both paths testable)
- Incremental migration (one operation at a time)
- Comprehensive test coverage before deleting old code
- Property-based testing: old and new paths must produce identical output

### Medium Risk: Debugger Breakpoints

**Risk:** PC semantics change breaks breakpoint resolution.

**Mitigation:**
- Keep PC as instruction index (same semantics)
- SourceMap remains 1:1 with PC
- Test debugger after each wave

### Medium Risk: Continuation Capture

**Risk:** PC saved in continuations now points to Instruction, not Operation.

**Mitigation:**
- PC is already an integer index (no semantic change)
- Test call/cc extensively after each wave
- Test dynamic-wind escape paths

### Low Risk: Performance Regression

**Risk:** Switch dispatch slower than interface dispatch on some platforms.

**Mitigation:**
- Benchmark on multiple platforms (M1, x86_64, Linux)
- Stop migration if gains don't materialize
- Keep dual-mode code if needed for A/B testing

## Open Questions

1. **LocalIndex bit-packing:** `LocalIndex` is `[2]int` — pack slot in low 32 bits and depth in high 32 bits of `Arg`. Safe because slot/depth values are small. Verify with edge case tests.

2. **ForeignFunctionCall:** Dedicated table or side table? → Defer to Phase 6.7 unless Wave 1-3 gains disappoint on scripting workloads.

3. **Complex operation threshold:** What defines "complex"? → Anything requiring heap-allocated payload (closures, matchers, syntax objects).

4. **Source map impact:** Does Instruction change source mapping? → No, PC remains 1:1 index.

5. **EqualTo for templates:** How does bytecode comparison work? → Compare `code` slices (simpler than operation-by-operation).

## Success Criteria

- [ ] 10–20% CPU improvement on tight numeric loops (measured via `make bench`)
- [ ] No regression on macro-heavy workloads (library loading, hygiene tests)
- [ ] Net negative LOC (code deletion > code addition)
- [ ] All tests pass (unit, integration, hygiene)
- [ ] Debugger and continuation capture work correctly
- [ ] REPL Ctrl+C responsive
- [ ] Documentation updated

## Timeline Estimate

| Phase | Ops | Risk |
|-------|-----|------|
| 6.1: Infrastructure | — | Low |
| 6.2: Dual-Mode VM | — | Medium |
| 6.3: Wave 1 | 8 zero-operand | Low |
| 6.4: Wave 2 | 7 single-operand | Low |
| 6.5: Wave 3 | 2 two-operand (bit-packed) | Medium |
| 6.6: Cleanup | 17 files deleted | Low |

## References

- `INTEGER_OPCODE_DISPATCH.md` — Design rationale and option comparison
- `PERFORMANCE_REFACTORING_PLAN.md` — Overall performance roadmap
- `machine/machine_context.go:516-554` — Current VM loop
- `machine/operation.go:22-25` — Operation interface
- `environment/local_index.go:32` — `LocalIndex [2]int` definition
- `machine/native_template.go:26` — `LiteralIndex int` definition
- Phase 5 (complete) — Compiler optimizations (ops prealloc, peephole, constant folding)
