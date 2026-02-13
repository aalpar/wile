# Wile Plugin Architecture Proposal

**Status:** PROPOSED — Design document for future consideration

> **Cross-reference**: See `EXTERNAL_EXTENSIONS_PLAN.md` for `EnvironmentAccess` interface design.

## Overview

Three-layer design separating minimal core from optional extensions:

```
┌───────────────────────────────────────────┐
│           User Applications               │
└───────────────┬───────────────────────────┘
                ▼
┌───────────────────────────────────────────┐
│  wile.Engine (Public API)                 │
│  NewEngine(options) / Eval / Compile / Run│
│  RegisterPrimitive / Define / Call / Get  │
└───────────┬───────────────────────────────┘
            │
     ┌──────┴──────┐
     ▼             ▼
┌──────────┐ ┌────────────────────┐
│ core     │ │ Optional Extensions│
│ (~85     │ │ io, files, system, │
│ prims)   │ │ math, threads,     │
│          │ │ gointerop, eval... │
└──────────┘ └────────────────────┘
```

## Design Goals

1. **Embeddable** — Clean, stable Go API for embedding
2. **Minimal core** — Only ~85 primitives required for Scheme semantics
3. **Optional extensions** — I/O, threading, system calls are opt-in
4. **No I/O by default** — Safe for sandboxed environments
5. **User extensibility** — Custom primitives without modifying Wile
6. **Backward compatible** — Existing code unchanged

## Core vs Extension Split

**Core (~85 primitives)**: Type predicates, equality, pairs/lists, CxR, arithmetic, numeric conversion, control (apply, call/cc), vectors, strings, characters, bytevectors, syntax, parameters.

**Extensions**: io (~25), files (~10), system (~10), math (~15), threads (~25), gointerop (~35), records (~10), exceptions (~8), eval (~6), syntax (~5).

## Key Components

| Component | Package | Purpose |
|-----------|---------|---------|
| `Registry` | `registry/` | Central primitive registration with phase awareness |
| `RegistryBuilder` | `registry/` | Collects registration functions (K8s SchemeBuilder pattern) |
| `Extension` interface | `registry/` | `Name() string` + `AddToRegistry(*Registry) error` |
| `Engine` | `wile/` | Public embedding API |
| REPL | `repl/` | Independent consumer of Engine API |

## Dependency Rules

| Package | Can Import | Cannot Import |
|---------|------------|---------------|
| `wile/` | `registry/`, `machine/`, `environment/`, `values/` | `repl/`, `cmd/`, `extensions/*` |
| `registry/` | `machine/`, `environment/`, `values/` | `wile/`, `repl/`, `extensions/*` |
| `repl/` | `wile/` | `registry/`, `machine/`, `extensions/*` |
| `extensions/*` | `registry/`, `machine/`, `values/` | `wile/`, `repl/` |

## Migration Phases

| Phase | Description |
|-------|-------------|
| 1 | Create registry infrastructure (Registry, Builder, Extension interface) |
| 2 | Create core primitives package (`registry/core/`) |
| 3 | Create public API package (`wile/engine.go`) |
| 4 | Extract extension packages (`extensions/*`) |
| 5 | Create independent REPL package |
| 6 | Update entry points (`cmd/main.go`) |
| 7 | Documentation and examples |
