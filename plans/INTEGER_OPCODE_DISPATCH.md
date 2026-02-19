# Integer Opcode Dispatch

**Status:** COMPLETE — implemented as Phase 6 (see `PHASE6_SWITCH_DISPATCH.md`)
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

30 distinct operation types, each a separate struct implementing `Operation` (which embeds `values.Value`).

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
| Memory footprint | ~3x smaller | Push: 16 bytes (`Instruction{OpCode, int}`) vs ~48 bytes. BranchOnFalseValue: 16 bytes vs ~56 bytes |
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
**Cons:** Wastes Arg fields on zero-operand ops. Complex payloads still need side table. Full migration required (all 30 ops at once).

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
case OpBranchOnFalseValue:
    // Phase 5 peephole: reads value register directly (no stack pop)
    if !values.ValueToBool(mc.GetValue()) {
        mc.pc += instr.Arg
    } else {
        mc.pc++
    }
// ...~14 more simple ops inlined...
case OpComplex:
    mc, err = tmpl.sideTable[instr.Arg].Apply(ctx, mc)
}
```

**Pros:**
- Gets cache/dispatch wins for the 17 simple ops dominating execution
- Complex ops keep current representation unchanged
- Incremental migration — one op at a time, benchmark at each step
- No breakage to values.Value semantics for complex ops

**Cons:**
- Two dispatch mechanisms to maintain
- Simple ops lose their values.Value identity (rarely needed)

## Multi-Parameter Instruction Encoding

For operations with two parameters (e.g., LoadLocal with slot + depth):

**Actual type:** `LocalIndex` is `[2]int` (see `environment/local_index.go:32`), where `[0]` = slot (over), `[1]` = depth (up). On 64-bit platforms, each `int` is 8 bytes, so `LocalIndex` is 16 bytes total.

**Encoding into `Instruction{Op OpCode; Arg int}`:** Since the `Arg` field is a single `int` (8 bytes on 64-bit), pack both values: slot in low 32 bits, depth in high 32 bits. Safe because slot/depth values are small integers in practice (never exceed 32-bit range).

```go
// Encode:
arg := (depth << 32) | (slot & 0xFFFFFFFF)
// Decode:
slot := int(int32(instr.Arg))        // sign-extend low 32 bits
depth := int(int32(instr.Arg >> 32)) // sign-extend high 32 bits
```

**Recommendation:** Bit-packing into the existing `Arg int` field avoids changing the Instruction struct for two-operand ops. The encode/decode helpers are confined to LoadLocal/StoreLocal emission and dispatch.

## Recommendation: Option D

### Rationale

1. **80/20 rule.** ~8 operation types dominate the hot path: Push, Pop, LoadLocal, StoreLocal, Branch, BranchOnFalseValue, Apply, RestoreContinuation. Making these integer-dispatched captures most of the win.

2. **Complex ops are rare per-execution.** SyntaxRulesTransform fires once per macro expansion, not in tight loops. Keeping them as interface objects costs nothing.

3. **Incremental.** No big-bang migration. Port hot-path ops, benchmark, stop when gains plateau.

4. **Operations-as-values.** Complex ops that need values.Value semantics keep it. Simple ops rarely need it.

5. **Aligns with project vision.** Performance is deprioritized but "free" wins from better cache behavior are worth taking when the migration path is incremental.

### If Maximum Simplicity Is Preferred

Option C (fixed-size struct, no interface) is the cleanest final state but requires porting all 30 operations and solving the values.Value requirement for all of them simultaneously.

## Hot-Path Operations (Migration Priority)

These ops should be converted first — they execute most frequently.

**Verified inventory: 30 total operations, 17 migrable, 13 complex (keep as interface).**

| Priority | Operation | Fields | Notes |
|----------|-----------|--------|-------|
| 1 | Push | none | |
| 1 | Pop | none | |
| 1 | Apply | none | Body is complex but opcode dispatch itself is trivial |
| 1 | RestoreContinuation | none | |
| 1 | LoadLocalByLocalIndexImmediate | `*LocalIndex` (`[2]int`) | Two-operand: bit-pack slot/depth into single `int` Arg |
| 1 | StoreLocalByLocalIndexImmediate | `*LocalIndex` (`[2]int`) | Two-operand: same encoding as LoadLocal |
| 1 | BranchOnFalseValueOffsetImmediate | Offset int | Phase 5 peephole; reads value register directly (no stack pop) |
| 1 | BranchOffsetImmediate | Offset int | |
| 2 | LoadLiteralByLiteralIndexImmediate | LiteralIndex (int) | |
| 2 | LoadVoid | none | |
| 2 | SaveContinuationOffsetImmediate | Offset int | |
| 2 | LoadGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex int | Single-operand: indexes into literals pool to find GlobalIndex |
| 2 | StoreGlobalByGlobalIndexLiteralIndexImmediate | LiteralIndex int | Single-operand: same as LoadGlobal |
| 3 | Pull | none | |
| 3 | PeekK | Depth int | |
| 3 | Drop | none | Discards top of eval stack (no count param) |
| 3 | PopEnv | none | |

**Note:** `ForeignFunctionCall` is kept as interface (complex payload) but is the dominant hot-path for scripting workloads. See Phase 6.7 in `PHASE6_SWITCH_DISPATCH.md` for dedicated function table optimization.

### Complex Ops (Keep as Interface — 13 total)

These carry payloads that don't reduce to integers:

- OperationSyntaxRulesTransform (clauses wrapper, scopes)
- OperationSyntaxCaseMatch (matcher, pattern data)
- OperationBindPatternVars (pattern bindings)
- OperationSyntaxCaseNoMatch (error data)
- OperationSyntaxTemplateExpand (template data)
- OperationStoreSyntaxCaseInput
- OperationClearSyntaxCaseInput
- OperationBuildSyntaxList (syntax builder config)
- OperationMakeClosure (template pointer)
- OperationMakeCaseLambdaClosure (multiple template pointers)
- OperationPushWind (thunk closures)
- OperationPopWind (thunk closures)
- OperationForeignFunctionCall (Go function pointer)

## Open Questions

1. **Benchmarking baseline.** Need microbenchmarks isolating dispatch overhead before starting. Tight loop like `(do ((i 0 (+ i 1))) ((= i 1000000)))` and a cons-heavy workload like `(map (lambda (x) (+ x 1)) long-list)`.

2. **Two-operand ops.** LoadLocal and StoreLocal carry `*environment.LocalIndex` (which is `[2]int`, see `environment/local_index.go:32`). Pack slot in low 32 bits and depth in high 32 bits of the single `int` Arg field. Safe because slot/depth values are small. LoadGlobal/StoreGlobal are single-operand (one LiteralIndex) despite the misleading name.

3. **ForeignFunctionCall.** Currently carries a Go function pointer. Side table reference is straightforward but adds an indirection. Alternative: dedicate an `[]ForeignFunc` table separate from the general side table.

4. **Source map impact.** If instructions stay fixed-width (Options C/D), source maps remain 1:1. If variable-width (Option A), source maps need a mapping layer.

5. **EqualTo for bytecode.** `Operations.EqualTo` compares operation-by-operation. With integer opcodes, this becomes comparing `[]Instruction` slices (simpler). Complex ops in side table still use interface equality. Is there code that depends on individual operation identity?
