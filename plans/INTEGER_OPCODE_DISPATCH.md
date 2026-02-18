# Integer Opcode Dispatch

**Status:** Discussion / Design
**Date:** 2026-02-17

## Problem

The VM dispatch loop (`machine_context.go:504`) uses Go interface method calls:

```go
for mc.pc < len(mc.template.operations) {
    mc, err = mc.template.operations[mc.pc].Apply(mc.ctx, mc)
}
```

Each cycle pays:
1. Index into `[]Operation` — slice of 16-byte interface values (`{itab_ptr, data_ptr}`)
2. Chase pointer to itab, load method address
3. Indirect call through vtable
4. The operation struct is a separate heap allocation (~48-64 bytes each)

~35 distinct operation types, each a separate struct implementing `Operation` (which embeds `values.Value`).

## Current Architecture

```
Operation interface {
    values.Value                                              // SchemeString, IsVoid, EqualTo
    Apply(ctx context.Context, mc *MachineContext) (*MachineContext, error)
}

NativeTemplate.operations  []Operation   // interface slice
```

Each operation struct: `OperationBase` (two string pointers) + type-specific fields (Offset, Index, etc.).

### Complication: Operations Are values.Value

Operations participate in Scheme equality and display. Some carry complex payloads:
- `OperationForeignFunctionCall` → Go function pointer
- `OperationSyntaxRulesTransform` → clauses wrapper, scope data
- `OperationSyntaxCaseMatch` → matcher, pattern data
- `OperationBuildSyntaxList` → syntax builder config

## Expected Performance Gains

| Source | Estimated gain | Mechanism |
|--------|---------------|-----------|
| Cache locality | 2-5x on dispatch | `[]uint64` or `[]Instruction` is contiguous; `[]Operation` chases two pointers per op through scattered heap |
| Switch vs itab | ~2-3ns/op | Interface dispatch (~3-5ns: load itab, indirect call) → jump table (~1-2ns) |
| Memory footprint | ~4-6x smaller | Push: 8 bytes vs ~48 bytes. BranchOnFalse: 16 bytes vs ~56 bytes |
| Allocation pressure | Moderate | No per-operation heap allocs during compilation |

### Where It Doesn't Help

Opcode dispatch is not the primary bottleneck for typical Scheme workloads. Most time goes to:
- `OperationApply` → `ApplyCallable` → environment copy + closure setup (100s of ns)
- GC pressure from cons cells, closures, environments
- Foreign function calls into Go primitives

Tight numeric loops benefit most. Scripting workloads (config, policy) benefit less.

## Options

### Option A: Flat `[]uint64` Stream

```
[]uint64 = [OpPush, OpLoadLocal, 3, OpBranchOnFalse, 7, OpForeignCall, 12, ...]
                                 ^                   ^                  ^
                              local idx           offset         side_table_idx
```

Simple ops: 1 word. Parameterized: 2+ words. Complex ops: opcode + index into `[]any` side table.

**Pros:** Maximum cache locality. Trivially serializable. Simple decoder.
**Cons:** Variable-width breaks 1:1 pc-to-instruction mapping (source maps become harder). Complex ops still need side table.

### Option B: Packed Instruction Word

```go
// 16-bit opcode | 48-bit operand
const opShift = 48
func encode(op OpCode, arg int) uint64 {
    return uint64(op)<<opShift | uint64(arg)&0xFFFFFFFFFFFF
}
```

One word per instruction for most ops. Multi-operand ops use pc+1 for second operand.

**Pros:** Single memory access per instruction. Fixed-width for common cases.
**Cons:** Encoding/decoding overhead (shifts + masks). Still needs side table for complex payloads.

### Option C: Fixed-Size Tagged Struct

```go
type Instruction struct {
    Op   OpCode  // uint16
    _    uint16  // padding
    Arg1 int32   // operand 1 (offset, index, side table ref)
}
// 8 bytes per instruction
```

Or wider for two-operand ops:

```go
type Instruction struct {
    Op   OpCode  // uint16
    Arg1 int32
    Arg2 int32
}
// 12 bytes per instruction
```

Dispatch: `switch instr.Op { case OpPush: ... case OpBranchOnFalse: pc += instr.Arg1 ... }`

**Pros:** Fixed-width keeps source maps 1:1 with pc. No variable-width decoding. No interface overhead.
**Cons:** Wastes Arg fields on zero-operand ops. Complex payloads still need side table. Full migration required (all 35 ops at once).

### Option D: Hybrid — Integer Dispatch + Interface Fallback (Recommended)

```go
type Instruction struct {
    Op  OpCode  // uint16
    Arg int     // immediate operand OR side table index
}

type NativeTemplate struct {
    code      []Instruction
    sideTable []Operation    // complex ops: syntax-rules, foreign-call, etc.
    // ...
}
```

Hot loop becomes:

```go
switch instr.Op {
case OpPush:
    mc.evals.Push(mc.singleValue)
    mc.pc++
case OpPop:
    mc.singleValue = mc.evals.Pop()
    mc.pc++
case OpLoadLocal:
    mc.singleValue = mc.env.GetByIndex(instr.Arg)
    mc.pc++
case OpBranchOnFalse:
    v := mc.evals.Pop()
    if !values.ValueToBool(v) {
        mc.pc += instr.Arg
    } else {
        mc.pc++
    }
// ...~15 more simple ops inlined...
case OpComplex:
    mc, err = tmpl.sideTable[instr.Arg].Apply(ctx, mc)
}
```

**Pros:**
- Gets cache/dispatch wins for the ~20 simple ops dominating execution
- Complex ops keep current representation unchanged
- Incremental migration — one op at a time, benchmark at each step
- No breakage to values.Value semantics for complex ops

**Cons:**
- Two dispatch mechanisms to maintain
- Simple ops lose their values.Value identity (rarely needed)

## Multi-Parameter Instruction Encoding

For operations with two int32 parameters (e.g., LoadLocal with slot + depth):

**struct{int32, int32} vs int64 analysis:** Both are 8 bytes, but struct has 4-byte alignment (int64 has 8-byte), direct field access (no shift/mask), and clearer semantics. Micro-benchmarks show int64 is ~25% faster (0.28ns vs 0.37ns) but this is negligible compared to VM dispatch overhead. Atomicity is irrelevant (operations are immutable). Debugger displays struct as `{Slot: 5, Depth: 2}` vs `0x0000000500000002`.

**Recommendation:** Use struct for readability, debuggability, and standard Go idioms. The performance difference is unmeasurable in real workloads.

## Recommendation: Option D

### Rationale

1. **80/20 rule.** ~8 operation types dominate the hot path: Push, Pop, LoadLocal, StoreLocal, Branch, BranchOnFalse, Apply, RestoreContinuation. Making these integer-dispatched captures most of the win.

2. **Complex ops are rare per-execution.** SyntaxRulesTransform fires once per macro expansion, not in tight loops. Keeping them as interface objects costs nothing.

3. **Incremental.** No big-bang migration. Port hot-path ops, benchmark, stop when gains plateau.

4. **Operations-as-values.** Complex ops that need values.Value semantics keep it. Simple ops rarely need it.

5. **Aligns with project vision.** Performance is deprioritized but "free" wins from better cache behavior are worth taking when the migration path is incremental.

### If Maximum Simplicity Is Preferred

Option C (fixed-size struct, no interface) is the cleanest final state but requires porting all ~35 operations and solving the values.Value requirement for all of them simultaneously.

## Hot-Path Operations (Migration Priority)

These ops should be converted first — they execute most frequently:

| Priority | Operation | Fields | Notes |
|----------|-----------|--------|-------|
| 1 | Push | none | |
| 1 | Pop | none | |
| 1 | LoadLocalByLocalIndexImmediate | LocalIndex struct{Slot, Depth int32} | Two-operand: struct is clearer than bit-packing, same 8-byte size |
| 1 | StoreLocalByLocalIndexImmediate | LocalIndex struct{Slot, Depth int32} | Two-operand: same struct as LoadLocal |
| 1 | BranchOnFalseOffsetImmediate | Offset int | |
| 1 | BranchOnNotFalseOffsetImmediate | Offset int | |
| 1 | BranchOffsetImmediate | Offset int | |
| 2 | LoadLiteralByLiteralIndexImmediate | Index LiteralIndex | |
| 2 | LoadLiteralInteger | Value int | |
| 2 | LoadVoid | none | |
| 2 | Apply | none | Body is complex but opcode dispatch itself is trivial |
| 2 | RestoreContinuation | none | |
| 2 | SaveContinuationOffsetImmediate | Offset int | |
| 3 | Pull | none | |
| 3 | PeekK | K int | |
| 3 | Drop | Count int | |
| 3 | PopAll | none | |
| 3 | PopEnv | none | |
| 3 | ForeignFunctionCall | fn pointer | Needs side table |
| 3 | LoadGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex int | Single-operand: indexes into literals pool to find GlobalIndex |
| 3 | StoreGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex int | Single-operand: same as LoadGlobal |

### Complex Ops (Keep as Interface)

These carry payloads that don't reduce to integers:

- OperationSyntaxRulesTransform
- OperationSyntaxCaseMatch
- OperationBindPatternVars
- OperationSyntaxCaseNoMatch
- OperationSyntaxTemplateExpand
- OperationStoreSyntaxCaseInput
- OperationClearSyntaxCaseInput
- OperationBuildSyntaxList
- OperationMakeClosure (template pointer)
- OperationMakeCaseLambdaClosure (template pointers)
- OperationPushWind / OperationPopWind
- OperationBrk (debugger)

## Open Questions

1. **Benchmarking baseline.** Need microbenchmarks isolating dispatch overhead before starting. Tight loop like `(do ((i 0 (+ i 1))) ((= i 1000000)))` and a cons-heavy workload like `(map (lambda (x) (+ x 1)) long-list)`.

2. **Two-operand ops.** LoadLocal and StoreLocal carry `LocalIndex struct{Slot, Depth int32}` (slot + depth). **Recommendation: use struct fields, not bit-packing.** Both `struct{int32, int32}` and `int64` are 8 bytes, but the struct has clearer semantics, direct field access (no shift/mask), and better debugger display. Go packs the struct with no padding (4-byte aligned). Bit-packing `up<<16|over` saves nothing and adds encoding overhead. LoadGlobal/StoreGlobal are single-operand (one LiteralIndex) despite the misleading name.

3. **ForeignFunctionCall.** Currently carries a Go function pointer. Side table reference is straightforward but adds an indirection. Alternative: dedicate an `[]ForeignFunc` table separate from the general side table.

4. **Source map impact.** If instructions stay fixed-width (Options C/D), source maps remain 1:1. If variable-width (Option A), source maps need a mapping layer.

5. **EqualTo for bytecode.** `Operations.EqualTo` compares operation-by-operation. With integer opcodes, this becomes comparing `[]Instruction` slices (simpler). Complex ops in side table still use interface equality. Is there code that depends on individual operation identity?
