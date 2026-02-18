# Performance Refactoring Plan

**Status:** IN PROGRESS — Phases 0–6 complete, Phase 7 remaining

## Overview

Full-pipeline performance refactoring: parsing → expansion → compilation → execution. Organized into dependency-ordered phases. Phases 0–4 delivered quick wins, sync.Pool, environment CoW, and expansion allocation optimization. Phase 5 added compiler-level optimizations.

## Current Bottlenecks (Remaining)

```
┌─────────────┬──────────────────────────────────────────────┐
│ VM          │ interface dispatch (2 ptr indirections/op)   │
├─────────────┼──────────────────────────────────────────────┤
│ Values      │ ForeignError: 50-frame stack trace every err │
└─────────────┴──────────────────────────────────────────────┘
```

**Existing infrastructure**: `make bench`, `make profile-cpu/mem`, `make bench-schelog`, parser benchmarks, VMCounters, integer/string/symbol/boolean caching.

## Completed Phases

| Phase | Description | Status |
|-------|-------------|--------|
| **0** | Quick wins — integer/string/symbol/boolean caching | Complete |
| **1** | sync.Pool for MachineContext | Complete |
| **2** | Environment copy-on-write | Complete |
| **3** | Expansion allocation optimization | Complete |
| **4** | WithScope/AddScope idempotency | Complete |
| **5** | Compiler optimizations — ops prealloc, constant folding, peephole | Complete |
| **6** | Switch dispatch — opcode enum, compact instruction struct, switch-based VM loop | Complete |

### Phase 5 Details

Three optimizations applied to the compiler:

1. **Operations prealloc**: `NativeTemplate` pre-allocates operations and sourceRefs slices with capacity 8 when created for compilation (no initial operations). Reduces append-driven slice growth for typical lambda bodies.

2. **BranchOnFalseValue peephole**: New `OperationBranchOnFalseValueOffsetImmediate` reads the value register directly instead of popping from the eval stack. Eliminates the preceding `Push` instruction in:
   - `CompileValidatedIf` — saves 1 operation per `if` form
   - `compileSyntaxCaseClause` — saves 1 operation per pattern match and per fender check

3. **Constant folding for `if`**: When the test expression is a compile-time-known literal:
   - `(if #f X Y)` → compiles only Y (or void if no alternative)
   - `(if <truthy-literal> X Y)` → compiles only X
   - Per R7RS, only `#f` is false; all other values are truthy

### Phase 6 Details

Replaced interface-based operation dispatch with switch-based dispatch using integer opcodes. Migrated 17 hot-path operations to integer dispatch in 3 waves:

**Wave 1 (Zero-operand):** Push, Pop, Pull, LoadVoid, Drop, PopEnv, Apply, RestoreContinuation
**Wave 2 (Single-operand):** BranchOnFalseValue, Branch, SaveContinuation, LoadLiteral, LoadGlobal, StoreGlobal, PeekK
**Wave 3 (Two-operand):** LoadLocal, StoreLocal (bit-packed LocalIndex)

See `PHASE6_SWITCH_DISPATCH.md` for full implementation details.

## Remaining Phases

| Phase | Description | Impact | Risk | Deps | Plan |
|-------|-------------|--------|------|------|------|
| **7** | Advanced — tagged integers (unsafe), compilation caching, library pre-compilation | Variable | High | 5, 6 | TBD |

## Critical Files

| File | Phases |
|------|--------|
| `machine/machine_context.go` | 6 (runIntegerDispatch) |
| `machine/opcode.go` | 6 (new) |
| `machine/instruction.go` | 6 (new) |
| `machine/native_template.go` | 5 (done), 6 (code/sideTable fields) |
| `machine/compile_validated.go` | 5 (done), 6 (emit Instructions) |
| `machine/compile_syntax_case.go` | 5 (done), 6 (emit Instructions) |
| `machine/operation_branch_on_false_value_offset_immediate.go` | 5 (new) |
| `values/foreign_error.go` | (ForeignError stack depth — deferred from Phase 1) |

## Out of Scope

Custom allocator/arena, JIT, parallel compilation, alternative GC, numeric tower optimization (direct dispatch already in place), parser/tokenizer optimization (not bottlenecks).

## Verification (All Phases)

`make test` → `make bench` (compare baselines) → `make profile-cpu/mem` → `make bench-schelog` → REPL Ctrl+C responsive → hygiene test suite.
