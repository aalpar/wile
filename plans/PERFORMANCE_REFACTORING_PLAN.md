# Performance Refactoring Plan

**Status:** PLANNED — Not started

## Overview

Full-pipeline performance refactoring: parsing → expansion → compilation → execution. Organized into dependency-ordered phases.

## Current Bottlenecks

```
┌─────────────┬──────────────────────────────────────────────┐
│ VM          │ PopAll clones stack, env deep-copy every     │
│             │ non-tail call, ctx.Done() every iteration,   │
│             │ interface dispatch (2 ptr indirections/op)   │
├─────────────┼──────────────────────────────────────────────┤
│ Expansion   │ New SyntaxPair/Symbol per AddScope,          │
│             │ new MachineContext per macro invocation,      │
│             │ no expansion caching                         │
├─────────────┼──────────────────────────────────────────────┤
│ Compilation │ Operations slice grows via append,            │
│             │ no constant folding / dead code elimination  │
├─────────────┼──────────────────────────────────────────────┤
│ Values      │ Character: no cache, Pair: no pooling,       │
│             │ ForeignError: 50-frame stack trace every err │
└─────────────┴──────────────────────────────────────────────┘
```

**Existing infrastructure**: `make bench`, `make profile-cpu/mem`, `make bench-schelog`, parser benchmarks, VMCounters, integer/string/symbol/boolean caching.

## Phases

| Phase | Description | Impact | Risk | Deps |
|-------|-------------|--------|------|------|
| **0** | Measurement — stage-isolated benchmarks, GC pressure, pprof | Foundation | Low | None |
| **1** | Quick wins — PopAll fix, ctx.Done batching, char cache, ForeignError depth, single-value MV | 15–25% | Low | 0 |
| **2** | sync.Pool — Stack, Continuation, sub-context pooling | 30–50% alloc reduction | Medium | 0 |
| **3** | Environment CoW — shared flag, binding copy avoidance, keys map sharing | 20–40% call-heavy | Medium | 0, 1 |
| **4** | Expansion — lazy scope propagation, SourceContext interning, expander sub-context reuse | 40–60% expansion alloc | High | 0 |
| **5** | Compiler — operations prealloc, constant folding, peephole (push/pop elimination) | 5–15% | Medium | 0 |
| **6** | Switch dispatch — opcode enum, compact instruction struct, switch-based VM loop | 10–20% CPU | High | 1, 5 |
| **7** | Advanced — tagged integers (unsafe), compilation caching, library pre-compilation | Variable | High | 5, 6 |

```
Phase 0 ──→ Phase 1 ──→ Phase 3
   │            └──→ Phase 6
   ├──→ Phase 2
   ├──→ Phase 4
   └──→ Phase 5 ──→ Phase 6 ──→ Phase 7
```

## Critical Files

| File | Phases |
|------|--------|
| `machine/machine_context.go` | 1, 2, 6 |
| `environment/local_environment_frame.go` | 3 |
| `machine/stack.go` | 1.1, 2.1 |
| `internal/syntax/syntax_pair.go` | 4 |
| `machine/expander_time_continuation.go` | 4.3 |
| `values/foreign_error.go` | 1.4 |
| `values/character.go` | 1.3 |
| `machine/native_template.go` | 5.1 |

## Out of Scope

Custom allocator/arena, JIT, parallel compilation, alternative GC, numeric tower optimization (direct dispatch already in place), parser/tokenizer optimization (not bottlenecks).

## Verification (All Phases)

`make test` → `make bench` (compare baselines) → `make profile-cpu/mem` → `make bench-schelog` → REPL Ctrl+C responsive → hygiene test suite.
