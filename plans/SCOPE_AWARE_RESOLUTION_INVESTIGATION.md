# Scope-Aware Symbol Resolution Investigation

**Status**: Deferred (like numeric tower dispatch)

**Related**: `ALGEBRAIC_REDUCTIONS.md` Item VI

---

## Summary

Investigation into potential consolidation of scope-checking logic duplicated between the compiler and expander. **Conclusion**: The duplication serves distinct architectural purposes and cannot be trivially consolidated without obscuring semantic differences.

---

## The Duplication Pattern

Both `machine/compile_time_continuation.go` and `machine/expander_time_continuation.go` contain superficially identical scope-checking branches:

```go
if len(scopes) == 0 {
    // Simple path: no scope checking
} else {
    // Complex path: scope-aware lookup/checking
}
```

This pattern implements Flatt's hygiene rule: a binding matches a reference only if `bindingScopes ⊆ useScopes`.

---

## Detailed Analysis

### Compiler (`compile_time_continuation.go:115-169`)

**Purpose**: Pre-lookup dispatch optimization

**Checks**: Reference symbol's scopes (from `expr.Scopes()`)

**Decision tree**:
```
Symbol reference
    │
    ├─ len(symbolScopes) == 0 → GetLocalIndex / GetGlobalIndex
    │                           (fast path: no scope checking)
    │
    └─ len(symbolScopes) > 0  → GetLocalIndexWithScopes / GetBindingWithScopes
                                (slow path: check scope compatibility)
```

**Rationale**: Scope-aware lookup is expensive (must check subset relationships). User-written code outside macros typically has no scopes, so this optimization avoids overhead in the common case.

**Output**: Emits different bytecode operations (`LoadLocalByLocalIndexImmediate` vs `LoadGlobalByGlobalIndexLiteralIndexImmediate`)

### Expander (`expander_time_continuation.go:88-94`)

**Purpose**: Post-lookup correctness check

**Checks**: Binding's scopes (from `binding.Scopes()` after lookup)

**Decision tree**:
```
Binding lookup complete
    │
    ├─ len(bindingScopes) == 0 → return true
    │                             (top-level binding matches any use)
    │
    └─ len(bindingScopes) > 0  → ScopesMatch(useScopes, bindingScopes)
                                 (check subset relationship)
```

**Rationale**: R7RS 4.2.2 requires `let`-bindings to shadow outer macro definitions. A binding with no scopes (top-level) is universally visible; a binding with scopes only matches if it's compatible with the reference's scopes.

**Output**: Boolean — does this variable binding shadow a macro?

---

## Semantic Differences

| Aspect | Compiler | Expander |
|--------|----------|----------|
| **Timing** | Before lookup (dispatch) | After lookup (validation) |
| **Input source** | `expr.Scopes()` — reference symbol | `binding.Scopes()` — definition |
| **Concern** | Performance | Correctness |
| **Effect** | Chooses which lookup method to call | Controls macro shadowing behavior |
| **Context** | Bytecode generation | Macro expansion |

Both implement the same hygiene rule but at different points in the processing pipeline for different purposes.

---

## Why Consolidation is Deferred

### Extraction Would Obscure Semantics

A naive extraction:

```go
// WRONG: Hides the semantic difference
func scopeCheckHelper(scopes []Scope, onNoScopes, onHasScopes func()) {
    if len(scopes) == 0 {
        onNoScopes()
    } else {
        onHasScopes()
    }
}
```

This saves ~5 lines but obscures:
- **Pre-lookup vs post-lookup** timing
- **Dispatch optimization vs correctness check** intent
- **Different data sources** (reference vs binding scopes)

### Structural vs Accidental Duplication

The duplication is **structural** (both implement Flatt's hygiene model) rather than **accidental** (copy-paste). The shared logic is the hygiene rule itself (`bindingScopes ⊆ useScopes`), which is fundamental to the macro system.

### Similar to Numeric Tower Deferral

Like the numeric tower dispatch (Item I), this is a case where:
- The surface-level pattern is repeated
- The underlying operation (scope checking) is the same
- But the contexts and purposes differ enough that consolidation would create a leaky abstraction
- The duplication is accepted as the clearer representation of distinct concerns

---

## Potential Future Consolidation

If consolidation is attempted in the future, it should:

1. **Preserve the semantic distinction**: Make clear that one is pre-lookup dispatch and the other is post-lookup validation
2. **Keep context visible**: Don't hide whether we're checking reference scopes vs binding scopes
3. **Maintain performance**: The compiler optimization (avoiding scope-aware lookup) must be preserved
4. **Ensure correctness**: The expander's shadowing check is critical for R7RS 4.2.2 compliance

A better abstraction might be:

```go
// Compiler context
type ScopeLookupStrategy interface {
    GetLocalIndex(sym) LocalIndex
    GetGlobalIndex(sym) GlobalIndex
}

// Returns FastLookup or ScopedLookup based on len(scopes)
strategy := SelectLookupStrategy(symbolScopes)
```

But even this requires careful design to not regress performance or complicate the code.

---

## Decision

**Defer consolidation** until:
1. A clear abstraction emerges that preserves both semantic clarity and performance
2. Additional duplication is discovered that would make the abstraction worthwhile
3. The hygiene system is refactored for other reasons (e.g., debugging tooling)

The current duplication is **intentional and understood**, not technical debt.

---

## References

- Flatt 2016: "Binding as Sets of Scopes" — hygiene rule formalization
- R7RS §4.2.2: Let-binding macro shadowing semantics
- R7RS §4.3: Macros and hygiene
- `docs/dev/ENVIRONMENT_SYSTEM.md`: Scope-aware binding resolution
- `machine/CLAUDE.local.md`: Macro system architecture
