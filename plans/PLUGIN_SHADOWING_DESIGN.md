# Plugin Shadowing Design

**Status:** Proposed — Not implemented

**Dependencies:** Public extension system (see `MODULE_DECOMPOSITION.md` Phase 2)

## Problem

Extensions need to shadow core primitives (e.g., R6RS compat needs to shadow `error`). Current architecture has globally registered primitives with no override mechanism.

## Three-Tier Lookup Model

```
┌─────────────────────────────────┐
│ User Environment                │  ← Highest priority
│ (user bindings, plugin overrides)│
├─────────────────────────────────┤
│ Extension Layer                 │  ← Middle priority
│ (extension-provided bindings)   │
├─────────────────────────────────┤
│ Core Registry                   │  ← Lowest priority (fallback)
│ (built-in primitives)           │
└─────────────────────────────────┘
```

## Design

Add optional `primitiveOverrides map[string]values.Value` to `Environment`. Lookup checks: local bindings → primitive overrides (nil check fast path) → parent → registry fallback.

Extensions declare overrides via `PrimitiveOverrides() map[string]PrimitiveSpec` method on Extension interface.

Scheme-level shadowing already works today via `(define error (let ((orig error)) (lambda args ...)))`.

## Use Cases

1. **R6RS compatibility** — shadow `error` to accept both R6RS and R7RS signatures
2. **Debug/trace mode** — shadow primitives to log calls
3. **Security/sandboxing** — intercept dangerous operations
4. **Performance instrumentation** — measure primitive call counts

## Implementation Phases

| Phase | Description |
|-------|-------------|
| 1 | Add `primitiveOverrides` to Environment, modify `Get()` |
| 2 | Add `PrimitiveOverrides()` to Extension interface |
| 3 | Scheme API: `define-primitive-override` (optional) |
| 4 | Performance profiling, error messages, debugging support |

## Design Decisions

- **Environment-scoped** (not global) — isolates extensions
- **Explicit opt-in** — no accidental shadowing
- **Immutable core** — core registry primitives can't be modified, only shadowed
- **Child inheritance** — overrides propagate via parent chain
- **No removal** — create new environment for clean state
- **Fast path**: nil pointer check (<1% overhead when no overrides)

## Alternatives Rejected

- Global override registry — no scoping flexibility
- Middleware chain — complex, high overhead
- Macro-based rewriting — breaks compiled code caching
