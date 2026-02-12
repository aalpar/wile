# Plugin Shadowing Design

## Problem Statement

Plugins/extensions need the ability to **shadow core primitives** to provide alternative implementations or compatibility layers. Current architecture does not support this—primitives are globally registered and cannot be overridden at the environment level.

**Concrete use case:** R6RS compatibility layer needs to shadow `error` to accept both R6RS `(error who message ...)` and R7RS `(error message ...)` signatures.

**Related documents:**
- `plans/EXTERNAL_EXTENSIONS_PLAN.md` - Public extension system
- `plans/PLUGIN_ARCHITECTURE_PROPOSAL.md` - Plugin architecture design
- `plans/AUTHORIZATION_FRAMEWORK.md` - K8s-style verb+resource authorization

## Current Architecture Limitations

### Primitive Registration (registry/registry.go)

```
Engine
  └─ Registry
       └─ primitives map[string]Primitive (global, no shadowing)
```

Primitives are registered globally via `AddPrimitive(spec PrimitiveSpec, phase Phase)`. The name is the key. Last registration wins, but there's no scoping—once registered, the primitive is visible everywhere.

### Lookup Flow

```
Symbol lookup
  → Environment.Get(symbol)
    → If not in environment, check registry.primitives[symbol.Name]
      → Returns single global implementation
```

No layering, no override mechanism, no way to "fork" the primitive set per environment.

## Design Goals

1. **Backward compatible** - Existing code works unchanged
2. **Explicit shadowing** - Users/plugins opt-in to overrides
3. **Environment-scoped** - Shadowing applies to specific environments, not global
4. **No performance regression** - Fast path for common case (no shadowing)
5. **Clear semantics** - Shadowing precedence is obvious and documented

## Proposed Architecture

### Three-Tier Lookup Model

```
┌─────────────────────────────────────────┐
│  User Environment                       │  ← Highest priority
│  - User-defined bindings                │
│  - Plugin overrides (optional)          │
├─────────────────────────────────────────┤
│  Extension Layer                        │  ← Middle priority
│  - Extension-provided bindings          │
│  - Can shadow core primitives           │
├─────────────────────────────────────────┤
│  Core Registry                          │  ← Lowest priority (fallback)
│  - Built-in primitives                  │
│  - Always available                     │
└─────────────────────────────────────────┘
```

### Environment Extension

```go
// environment/environment.go

type Environment struct {
    parent   *Environment
    bindings map[*values.Symbol]*Binding

    // NEW: Optional override layer for primitives
    primitiveOverrides map[string]values.Value  // symbol name → closure/primitive
}

// Get checks: local bindings → primitive overrides → parent → registry fallback
func (e *Environment) Get(symbol *values.Symbol) (values.Value, bool) {
    // 1. Check local bindings (user definitions, let-bindings, etc.)
    if b, ok := e.bindings[symbol]; ok {
        return b.Value, true
    }

    // 2. NEW: Check primitive overrides (extension/plugin shadows)
    if e.primitiveOverrides != nil {
        if v, ok := e.primitiveOverrides[symbol.Name]; ok {
            return v, true
        }
    }

    // 3. Check parent environment (lexical scope chain)
    if e.parent != nil {
        return e.parent.Get(symbol)
    }

    // 4. Fallback: check global registry (core primitives)
    return e.checkRegistry(symbol)
}
```

### Shadowing API

#### Extension-Level Shadowing

Extensions declare overrides during registration:

```go
// registry/extension.go (NEW)

type Extension interface {
    Name() string
    AddToRegistry(*Registry) error

    // NEW: Optional method for shadowing
    PrimitiveOverrides() map[string]PrimitiveSpec
}

// When loading extension, install overrides into environment
func (r *Registry) LoadExtensionWithOverrides(ext Extension, env *environment.Environment) error {
    if err := ext.AddToRegistry(r); err != nil {
        return err
    }

    // Install overrides if provided
    if overrides := ext.PrimitiveOverrides(); len(overrides) > 0 {
        for name, spec := range overrides {
            env.OverridePrimitive(name, spec)
        }
    }
    return nil
}
```

#### Scheme-Level Shadowing

Users can shadow primitives from Scheme code:

```scheme
;; Proposed syntax (requires new primitive)
(define-primitive-override 'error
  (lambda args
    ;; Custom error implementation
    ...))

;; Or using existing define (simpler, works today)
(define error
  (let ((r7rs-error error))  ; Capture original
    (lambda args
      ;; Custom wrapper
      ...)))
```

The second form (using `define`) works **today** without changes. The first form would be syntactic sugar.

### Implementation Phases

#### Phase 1: Foundation (Week 1)
- Add `primitiveOverrides` field to `Environment`
- Modify `Environment.Get()` to check overrides
- Add `Environment.OverridePrimitive(name, value)`
- Add tests for override precedence

#### Phase 2: Extension API (Week 2)
- Add `PrimitiveOverrides()` to `Extension` interface
- Modify `Registry.LoadExtension()` to install overrides
- Update extension documentation
- Create example extension using shadowing

#### Phase 3: Scheme API (Week 3)
- Add `define-primitive-override` primitive (optional)
- Add REPL support for inspecting overrides
- Document shadowing in user manual

#### Phase 4: Polish (Week 4)
- Performance profiling (ensure fast path is unchanged)
- Error messages when shadowed primitives fail
- Debugging support (show override chain)

## Use Cases

### 1. R6RS Compatibility Layer

```scheme
;; examples/lib/r6rs-compat.scm (using current Scheme-level approach)
(define r7rs:error error)

(define error
  (lambda args
    (if (string? (car args))
        (apply r7rs:error args)              ; R7RS form
        (apply r7rs:error                    ; R6RS form (who, message, ...)
               (string-append (symbol->string (car args)) ": " (cadr args))
               (cddr args)))))
```

With shadowing API:

```go
// internal/extensions/r6rs/r6rs.go
func (r *R6RSExtension) PrimitiveOverrides() map[string]PrimitiveSpec {
    return map[string]PrimitiveSpec{
        "error": {Name: "error", Impl: primR6RSError, ...},
    }
}
```

### 2. Debug/Trace Mode

Shadow primitives to log calls:

```scheme
(define +
  (let ((orig-+ +))
    (lambda args
      (display "calling + with: ") (display args) (newline)
      (apply orig-+ args))))
```

### 3. Security/Sandboxing

Restrict or intercept dangerous operations:

```go
func (s *SandboxExtension) PrimitiveOverrides() map[string]PrimitiveSpec {
    return map[string]PrimitiveSpec{
        "open-input-file": {Impl: primCheckedOpenInputFile, ...},
        "system":          {Impl: primDeniedSystem, ...},
    }
}
```

### 4. Performance Instrumentation

Measure primitive call counts:

```scheme
(define map
  (let ((orig-map map)
        (count 0))
    (lambda args
      (set! count (+ count 1))
      (apply orig-map args))))
```

## Security Considerations

**Risk:** Malicious code could shadow primitives to intercept/modify behavior.

**Mitigations:**
1. **Explicit loading** - Shadowing only happens when user loads extension/code
2. **Authorization framework** - Future work (see `plans/AUTHORIZATION_FRAMEWORK.md`)
3. **Immutable core** - Core registry primitives cannot be modified, only shadowed
4. **Inspection API** - Users can query active overrides

**Not a concern for current use case:** R6RS compat is user-controlled, non-malicious.

## Performance Impact

### Fast Path (No Overrides)

```go
// Current: 2 map lookups
bindings[symbol] → parent.Get() → registry[symbol]

// Proposed: +1 nil check
bindings[symbol] → (primitiveOverrides == nil) → parent.Get() → registry[symbol]
```

**Expected overhead:** <1% for typical workloads (nil pointer check is fast).

### Slow Path (With Overrides)

```go
bindings[symbol] → primitiveOverrides[symbol.Name] → parent.Get() → registry[symbol]
```

**Expected overhead:** One additional map lookup per shadowed primitive. Only affects environments with active overrides.

### Optimization: Override Cache

If profiling shows overhead, add per-environment cache:

```go
type Environment struct {
    ...
    lookupCache map[*values.Symbol]values.Value  // Invalidated on define/override
}
```

## Alternative Designs Considered

### A. Global Override Registry

Register overrides globally, not per-environment.

**Rejected:** No scoping flexibility. All environments affected, can't isolate extensions.

### B. Middleware Chain

Each primitive becomes a chain of handlers.

**Rejected:** Complex, high overhead, unclear semantics.

### C. First-Class Environments with Imports

R6RS-style library system with explicit imports.

**Deferred:** Larger change, not needed for immediate use case. Consider for R7RS-large.

### D. Macro-Based Shadowing

Use macros to rewrite symbol references.

**Rejected:** Doesn't work for runtime primitive dispatch, breaks compiled code caching.

## Open Questions

1. **Override introspection:** Should users be able to query `(primitive-overrides)` at runtime?
   - **Answer:** Yes, for debugging. Add `(primitive-overrides)` → list of (name . override) pairs.

2. **Override removal:** Should overrides be removable once installed?
   - **Answer:** No, for simplicity. Create new environment if you need clean state.

3. **Override inheritance:** Do child environments inherit parent's overrides?
   - **Answer:** Yes, via normal parent chain lookup.

4. **Multiple overrides:** Can you shadow the same primitive twice in nested environments?
   - **Answer:** Yes. Inner environment's override takes precedence.

5. **Compiled code caching:** How do overrides interact with bytecode caching?
   - **Answer:** Symbol lookup happens at runtime, so overrides apply correctly. No cache invalidation needed.

## Testing Strategy

```
tests/shadowing/
├── basic_override-test.scm         # Simple shadowing
├── nested_override-test.scm        # Override in child env
├── r6rs_compat-test.scm           # R6RS error compatibility
├── override_precedence-test.scm    # Multiple overrides
└── performance-test.scm            # Benchmark lookup overhead
```

## Documentation Requirements

1. **User manual section:** "Extending Wile: Primitive Shadowing"
2. **Extension guide:** "Creating Extensions with Overrides"
3. **API reference:** `Environment.OverridePrimitive()` documentation
4. **Security note:** Implications of shadowing for sandboxing

## Migration Path

### Immediate (No Changes Needed)

Scheme-level shadowing works today:

```scheme
(define + (let ((orig +)) (lambda args (apply orig args))))
```

### Short Term (Phase 1-2)

Environment-level shadowing via Go API for extensions.

### Long Term (Phase 3-4)

Scheme-level `define-primitive-override` for convenience.

## Success Metrics

- R6RS compatibility shim works without core changes (done via Scheme)
- Extension shadowing API is <50 LOC
- Lookup overhead is <1% in benchmarks
- No existing tests break
- New shadowing tests cover edge cases

## References

- R6RS §5.95: Error procedure signature `(error who message irritant ...)`
- R7RS §6.11: Error procedure signature `(error message irritant ...)`
- Racket: Parameterized first-class environments
- Guile: Module system with selective imports
- Chez Scheme: Library system with renaming

## Status

**Status:** Proposed
**Priority:** Medium (enables plugin flexibility)
**Effort:** 2-3 weeks
**Dependencies:** Public extension system (`EXTERNAL_EXTENSIONS_PLAN.md`)
**Blocks:** R6RS compatibility extension

## Appendix: Implementation Sketch

```go
// environment/environment.go

func (e *Environment) OverridePrimitive(name string, value values.Value) {
    if e.primitiveOverrides == nil {
        e.primitiveOverrides = make(map[string]values.Value)
    }
    e.primitiveOverrides[name] = value
}

func (e *Environment) GetPrimitiveOverrides() map[string]values.Value {
    if e.primitiveOverrides == nil {
        return nil
    }
    result := make(map[string]values.Value, len(e.primitiveOverrides))
    for k, v := range e.primitiveOverrides {
        result[k] = v
    }
    return result
}

func (e *Environment) HasPrimitiveOverride(name string) bool {
    if e.primitiveOverrides == nil {
        return false
    }
    _, ok := e.primitiveOverrides[name]
    return ok
}
```

```go
// registry/extension.go

type ExtensionWithOverrides interface {
    Extension
    PrimitiveOverrides() map[string]PrimitiveSpec
}

func (r *Registry) LoadExtensionWithShadowing(ext Extension, env *environment.Environment) error {
    if err := ext.AddToRegistry(r); err != nil {
        return err
    }

    if extWithOverrides, ok := ext.(ExtensionWithOverrides); ok {
        for name, spec := range extWithOverrides.PrimitiveOverrides() {
            primitive, err := r.createPrimitive(spec)
            if err != nil {
                return fmt.Errorf("failed to create override for %s: %w", name, err)
            }
            env.OverridePrimitive(name, primitive)
        }
    }

    return nil
}
```
