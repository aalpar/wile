# Performance Refactoring Plan

**Status:** IN PROGRESS — Phase 1 complete, Phase 2 complete

## Overview

Full-pipeline performance refactoring: parsing → expansion → compilation → execution. Organized into dependency-ordered phases.

## Current Bottlenecks

```
┌─────────────┬──────────────────────────────────────────────┐
│ VM          │ ✓PopAll fixed, env deep-copy every non-tail  │
│             │ call, ✓ctx.Done() batched every 1024 ops,    │
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
| **1** | Quick wins — ~~PopAll fix~~, ~~ctx.Done batching~~, char cache, ForeignError depth, ~~single-value MV~~ | 15–25% | Low | 0 |
| **2** | sync.Pool — Stack, Continuation, sub-context pooling | **Measured: 17% alloc ↓, 10% speed ↑** | Medium | 0 |
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

## Phase 1 Progress

| Item | Description | Status | Commit |
|------|-------------|--------|--------|
| 1.1 | PopAll — eliminate clone by swapping backing array ownership | **Complete** | `8f53bc7` |
| 1.2 | ctx.Done() batching — check every 1024 ops instead of every op | **Complete** | `7f694c1` |
| 1.3 | Character cache — intern ASCII characters (0–127) like integers/booleans | **Complete** | `80f1db7` |
| 1.4 | ForeignError stack removal | **Deferred** | Useful debug info on cold path; not worth removing |
| 1.5 | Single-value MV — split value register to avoid slice allocation | **Complete** | `d4fb408` |

## Phase 2 Progress

| Item | Description | Status | Notes |
|------|-------------|--------|-------|
| 2.1 | Stack pool (`acquireStack`/`releaseStack`) | **Complete** | Phase 1 prerequisite; already merged |
| 2.2 | SubContext pool (`acquireSubContext`/`ReleaseSubContext`) | **Complete** | Phase 1 prerequisite; already merged |
| 2.3 | Continuation pool (`acquireContinuation`/`releaseContinuation`) | **Complete** | `machine/pool.go` |
| 2.4 | Pool-backed `NewMachineContinuationFromMachineContext` | **Complete** | Uses `acquireContinuation()` instead of struct literal |
| 2.5 | `RestoreAndRelease` — transfer evals, pool consumed frame | **Complete** | Normal return path; no evals.Copy() |
| 2.6 | Switch `OperationRestoreContinuation` + `returnImmediate` | **Complete** | `Restore` → `RestoreAndRelease` |
| 2.7 | DeepCopy at continuation capture sites | **Complete** | Required for pool safety; see below |
| 2.8 | `ContinuationPoolReleases` counter | **Complete** | `machine/counters.go` |

### Pool safety: capture-site deep copies

`RestoreAndRelease` pools continuation frames after consuming them. Because `MachineContinuation.Copy()` is shallow (shares parent chain), captured continuations (call/cc, raise-continuable) would be corrupted when parent frames are pooled. Fix: all capture sites now use `DeepCopy()` to create fully independent chains.

```
Capture-time DeepCopy (ensures captured chain is intact):
  PrimCallCC inline:    mc.Parent().DeepCopy()
  PrimCallCC sub-ctx:   mc.EscapeCont().DeepCopy()
  PrimRaiseContinuable: cont.DeepCopy()

Use-time DeepCopy (each re-invocation gets a disposable chain):
  RestoreWithWindingFrom: cont.DeepCopy()
  RunWithEscapeHandling:  escapeCont.DeepCopy()
```

### Benchmark results (Phase 2 vs master)

```
                     │    master    │      pooling       │
                     │    sec/op    │  sec/op   vs base  │
Run/Lambda-16              787.7n      742.2n    -5.78%
Run/Let-16                 876.7n      835.8n    -4.67%
Run/Fibonacci-16           242.2µ      206.5µ   -14.73%
Run/TailRecursion-16       159.9µ      139.3µ   -12.93%
geomean                    12.79µ      11.56µ    -9.63%

                     │    master    │      pooling       │
                     │     B/op     │   B/op    vs base  │
Run/Lambda-16             1.493Ki     1.330Ki   -10.92%
Run/Let-16                1.649Ki     1.487Ki    -9.83%
Run/Fibonacci-16          390.7Ki     297.1Ki   -23.94%
Run/TailRecursion-16      259.9Ki     205.9Ki   -20.80%
geomean                   22.36Ki     18.65Ki   -16.60%

                     │    master    │      pooling       │
                     │  allocs/op   │ allocs/op vs base  │
Run/Lambda-16               29         27        -6.90%
Run/Let-16                  32         30        -6.25%
Run/Fibonacci-16           8566       7155      -16.47%
Run/TailRecursion-16       5731       4928      -14.01%
geomean                     462        411      -11.02%
```

p=0.000, n=8 for all comparisons. Apple M4 Max, darwin/arm64.

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
