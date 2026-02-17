# Performance Refactoring Plan

**Status:** IN PROGRESS — Phases 0–4 complete, Phases 5–7 remaining

## Overview

Full-pipeline performance refactoring: parsing → expansion → compilation → execution. Organized into dependency-ordered phases. Phases 0–4 delivered quick wins, sync.Pool, environment CoW, and expansion allocation optimization.

## Current Bottlenecks (Remaining)

```
┌─────────────┬──────────────────────────────────────────────┐
│ VM          │ interface dispatch (2 ptr indirections/op)   │
├─────────────┼──────────────────────────────────────────────┤
│ Compilation │ Operations slice grows via append,            │
│             │ no constant folding / dead code elimination  │
├─────────────┼──────────────────────────────────────────────┤
│ Values      │ ForeignError: 50-frame stack trace every err │
└─────────────┴──────────────────────────────────────────────┘
```

**Existing infrastructure**: `make bench`, `make profile-cpu/mem`, `make bench-schelog`, parser benchmarks, VMCounters, integer/string/symbol/boolean caching.

## Remaining Phases

| Phase | Description | Impact | Risk | Deps |
|-------|-------------|--------|------|------|
| **5** | Compiler — operations prealloc, constant folding, peephole (push/pop elimination) | 5–15% | Medium | — |
| **6** | Switch dispatch — opcode enum, compact instruction struct, switch-based VM loop | 10–20% CPU | High | 5 |
| **7** | Advanced — tagged integers (unsafe), compilation caching, library pre-compilation | Variable | High | 5, 6 |

```
Phase 5 ──→ Phase 6 ──→ Phase 7
```

## Critical Files

| File | Phases |
|------|--------|
| `machine/machine_context.go` | 6 |
| `machine/native_template.go` | 5 |
| `values/foreign_error.go` | (ForeignError stack depth — deferred from Phase 1) |

## Out of Scope

Custom allocator/arena, JIT, parallel compilation, alternative GC, numeric tower optimization (direct dispatch already in place), parser/tokenizer optimization (not bottlenecks).

## Verification (All Phases)

`make test` → `make bench` (compare baselines) → `make profile-cpu/mem` → `make bench-schelog` → REPL Ctrl+C responsive → hygiene test suite.
