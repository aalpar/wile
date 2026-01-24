# Plan: Fix let-syntax Hygiene for Local Bindings

## Problem Statement

Macros defined with `let-syntax` that reference local variables don't correctly capture those bindings:

```scheme
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))
; Returns 'inner (WRONG), should return 'outer
```

The macro template `x` should refer to the `x` bound by the outer `let`, not the inner `let`.

## Current State

- **Test 1 (local hygiene)**: FAILS - returns `inner` instead of `outer`
- **Test 2 (global hygiene)**: PASSES - global macros work via `freeIds`/`GlobalIndex` pre-resolution
- **Bootstrap macros**: PASS - `parameterize`, `and`, `or`, etc. all work

## Root Cause Analysis

### How the current `freeIds` mechanism works

1. At macro definition time, `collectFreeIdentifiersWithEllipsis` finds free identifiers in the template
2. For each free identifier, if there's a global binding, it stores the `GlobalIndex`
3. At expansion time, free identifiers with `GlobalIndex` get their `ResolvedBinding` set
4. At compile time, `CompileSymbol` checks `ResolvedBinding` first, using it directly

### Why it fails for local bindings

The `freeIds` mechanism ONLY handles global bindings (`GlobalIndex`). For local bindings:
- There's no `GlobalIndex` to store
- The template `x` ends up with NO pre-resolution
- At compile time, `CompileSymbol` has this logic:

```go
if len(symbolScopes) == 0 {
    // Try local binding first
    li := p.env.GetLocalIndex(sym)
    if li != nil {
        // Uses first local binding found, ignoring scopes!
        return nil
    }
}
```

When the template `x` (with no scopes) is compiled, `GetLocalIndex` finds the INNER `x` because it's the most recent local binding with that name.

## Flatt's Hygiene Model

Per "Binding as Sets of Scopes" (Flatt 2016):

1. Each binding form (lambda, let) introduces a fresh **scope**
2. **Bindings** are tagged with scopes from their definition site
3. **References** are tagged with scopes from their reference site
4. Resolution: `bindingScopes ⊆ referenceScopes`

### Expected behavior for Test 1

```
outer let expands to lambda, adds lambdaScope1:
  - outer binding x: scopes = [lambdaScope1]
  - body (including let-syntax): scopes += [lambdaScope1]

let-syntax compiles macro m:
  - template x should capture scopes [lambdaScope1]

inner let expands to lambda, adds lambdaScope2:
  - inner binding x: scopes = [lambdaScope1, lambdaScope2]

macro m expands, template x has scopes [lambdaScope1]:
  - ScopesMatch([lambdaScope1], [lambdaScope1]) = true (outer matches)
  - ScopesMatch([lambdaScope1], [lambdaScope1, lambdaScope2]) = false (inner doesn't match)
```

## Solution Options

### Option A: Extend freeIds to store LocalIndex scope sets

Store the binding's scope set (not index) for local free identifiers:

```go
type FreeIdInfo struct {
    GlobalIndex *GlobalIndex     // For global bindings
    Scopes      []*syntax.Scope  // For local bindings
}
```

At expansion time, create the symbol with the stored scopes.

**Pros**: Minimal change to existing architecture
**Cons**: Two different mechanisms (GlobalIndex vs scopes), more complexity

### Option B: Store scopes for ALL free identifiers (remove GlobalIndex special case)

Replace `GlobalIndex` pre-resolution with scope-based resolution for everything:

1. At definition time, store the binding's scope set for all free identifiers
2. At expansion time, attach these scopes to the free identifier symbols
3. At compile time, use scope-aware resolution uniformly

**Pros**: Simpler model, uniform handling
**Cons**: Need to verify global bindings still work, larger change

### Option C: Fix CompileSymbol to always use scope-aware resolution

Change the `len(symbolScopes) == 0` shortcut to use scope-aware resolution:

```go
// OLD: if len(symbolScopes) == 0 { use GetLocalIndex... }
// NEW: always use GetBindingAndLocalIndexWithScopes
```

**Pros**: Simple code change
**Cons**: Doesn't address the core issue (template scopes not captured)

## Recommended Approach: Option A (Incremental)

The safest approach is Option A - extend the existing mechanism rather than replace it.

### Implementation Steps

#### Step 1: Add scope storage for local free identifiers

In `compile_syntax_rules.go`, modify `collectFreeIdentifiersWithEllipsis`:

```go
// Current: freeIds map[string]*GlobalIndex
// Change to: freeIds map[string]*FreeIdResolution

type FreeIdResolution struct {
    Kind   FreeIdKind
    Global *GlobalIndex      // if Kind == FreeIdGlobal
    Scopes []*syntax.Scope   // if Kind == FreeIdLocal
}

func collectFreeIdentifiersWithEllipsis(...) map[string]*FreeIdResolution {
    // For each free identifier:
    // 1. Try to find local binding first
    // 2. If local, store its scope set
    // 3. If global, store GlobalIndex (existing behavior)
}
```

#### Step 2: Apply scopes during expansion

In `operation_syntax_rules_transform.go`, when building `freeIdsAny`:

```go
for k, v := range clause.freeIds {
    if v.Kind == FreeIdGlobal {
        freeIdsAny[k] = v.Global  // existing
    } else {
        freeIdsAny[k] = v.Scopes  // NEW: pass scopes
    }
}
```

#### Step 3: Handle scopes in valueToSyntaxWithOrigin

In `syntax_adapter.go`, modify the free identifier handling:

```go
case *values.Symbol:
    if freeIds != nil {
        if info, isFree := freeIds[v.Key]; isFree {
            switch resolved := info.(type) {
            case *GlobalIndex:
                // Existing: use ResolvedBinding
                sym := syntax.NewSyntaxSymbol(v.Key, symCtx)
                sym = sym.WithResolvedBinding(resolved)
                return sym
            case []*syntax.Scope:
                // NEW: create symbol with definition-site scopes
                scopedCtx := srcCtx.WithScopes(resolved)
                sym := syntax.NewSyntaxSymbol(v.Key, scopedCtx)
                return sym
            }
        }
    }
```

#### Step 4: Update CompileSymbol (if needed)

If step 3 correctly sets scopes, CompileSymbol should work. But verify the `len(symbolScopes) == 0` path is not taken for free identifiers with stored scopes.

### Verification Tests

1. **Test 1 - Local hygiene**:
   ```scheme
   (let ((x 'outer))
     (let-syntax ((m (syntax-rules () ((m) x))))
       (let ((x 'inner)) (m))))
   ; Expected: 'outer
   ```

2. **Test 2 - Mutation visibility**:
   ```scheme
   (let ((x 'outer))
     (let-syntax ((m (syntax-rules () ((m) x))))
       (set! x 'mutated)
       (let ((x 'inner)) (m))))
   ; Expected: 'mutated
   ```

3. **Test 3 - Global hygiene** (regression):
   ```scheme
   (define outer-x 'outer-global)
   (define-syntax m-global (syntax-rules () ((m-global) outer-x)))
   (let ((outer-x 'inner)) (m-global))
   ; Expected: 'outer-global
   ```

4. **Test 4 - Bootstrap macros** (regression):
   ```scheme
   (let ((p (make-parameter 1)))
     (parameterize ((p 2)) (p)))
   ; Expected: 2
   ```

5. **Full test suite**: `make test` must pass

## Risk Assessment

| Risk | Mitigation |
|------|------------|
| Breaking global macro hygiene | Test 3 verifies existing behavior |
| Breaking bootstrap macros | Test 4 verifies parameterize works |
| Performance regression | Scope lookup is O(n) but n is small |
| Subtle hygiene bugs | Run r7rs-tests.scm hygiene tests |

## Open Questions

1. **What scopes should be stored?** The binding's scopes at macro definition time. But need to verify this is accessible in `collectFreeIdentifiersWithEllipsis`.

2. **How does this interact with nested macros?** When a macro's output contains another macro invocation, the inner macro's template identifiers should also be resolved correctly.

3. **What about `define-syntax` at top level?** No scopes to store, falls back to GlobalIndex (existing behavior).

## Success Criteria

- [ ] Test 1 returns `'outer`
- [ ] Test 2 returns `'mutated`
- [ ] Test 3 returns `'outer-global`
- [ ] Test 4 returns `2`
- [ ] `make test` passes
- [ ] r7rs-tests.scm hygiene test passes
