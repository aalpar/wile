# Symbol Interning and Macro Hygiene Fix

## Problem Summary

Symbols are globally interned by string key, causing hygiene failures when:
1. A user's identifier (e.g., `p`) is captured as a pattern variable
2. The macro template also uses that same identifier name (e.g., `(let ((p param)) ...)`)

Both resolve to the same `*values.Symbol` pointer, so `syntaxMap` can only hold ONE mapping.

## Detailed Analysis

### The Architecture

```
*values.Symbol     - Just the string key, globally interned by InternSymbol()
*syntax.SyntaxSymbol - Wraps Symbol + SourceContext (which includes Scopes)
```

In Flatt's "Binding as Sets of Scopes" model, two occurrences of "p" in different scopes ARE different identifiers. But the current implementation loses this distinction when unwrapping syntax to values.

### The Flow That Fails

Example: `(let ((p (make-parameter 1))) (parameterize ((p 2)) (p)))`

The `parameterize` macro is defined as:
```scheme
(define-syntax parameterize
  (syntax-rules ()
    ((parameterize ((param val) rest ...) body ...)
     (let ((p param)        ; <-- template's 'p' is bound here
           (new val)
           (old (param)))
       (dynamic-wind
         (lambda () (p new))  ; <-- template's 'p' used here
         ...)))))
```

1. User's `p` has scopes from outer `let` (let-scope)
2. `parameterize` pattern matching:
   - `param` captures user's `p` symbol
   - `syntaxToValue` unwraps SyntaxSymbol to `*values.Symbol` (interned!)
   - `syntaxMap[interned_p] = user's SyntaxSymbol`
3. Template expansion:
   - Template's `p` is ALSO the same interned `*values.Symbol`
   - `syntaxMap[interned_p]` returns USER's SyntaxSymbol (wrong!)
   - Template's `p` should get intro-scope, but instead gets user's scopes

### Why Previous Fixes Failed

**Attempt 1: Skip syntaxMap for all symbols**
- Template's `if` worked correctly (got intro-scope)
- But pattern variable substitutions lost their original scopes
- User's `if` in `(let ((if even?)) (or #f (if 7)))` incorrectly got intro-scope

**Attempt 2: Use syntaxMap only for non-freeId symbols**
- Logic: freeIds = template identifiers, non-freeIds = pattern variables
- Failed because template-BOUND identifiers (like `p` in `(let ((p ...)))`) are NOT in freeIds
- Both pattern variables and template-bound identifiers are "not in freeIds"
- Can't distinguish them by freeIds membership

### The Root Cause

The `syntaxMap` uses `*values.Symbol` pointers as keys. Since symbols are globally interned:
- User's `p` and template's `p` → same pointer
- syntaxMap can only store one mapping
- Last write wins, corrupting the mapping

## The Solution

**Create fresh `*values.Symbol` instances during input unwrapping.**

In `syntaxToValue`, when processing a `SyntaxSymbol`:
1. Create a NEW `*values.Symbol` with the same Key (don't use `s.Sym`)
2. This fresh pointer is unique to this input position
3. Store in syntaxMap: `fresh_symbol → original_SyntaxSymbol`

Template symbols (from macro definition parse time) remain the original interned pointers. They won't be in syntaxMap, so they correctly go through the intro-scope path.

### Why This Works

```
Input Processing:
  User's 'p' SyntaxSymbol → fresh Symbol P1 → syntaxMap[P1] = SyntaxSymbol(p, user_scopes)

Template Expansion:
  Template's 'p' → interned Symbol P_global → NOT in syntaxMap → gets intro-scope
  Pattern var substitution → P1 → found in syntaxMap → returns SyntaxSymbol(p, user_scopes)
```

### Implementation Location

File: `match/syntax_adapter.go`, function `syntaxToValue`

Change the `SyntaxSymbol` case from:
```go
case *syntax.SyntaxSymbol:
    result = s.Sym  // Returns the globally interned symbol
```

To:
```go
case *syntax.SyntaxSymbol:
    // Create fresh symbol to avoid interning collision in syntaxMap
    result = values.NewSymbol(s.Sym.Key)  // Fresh pointer, same key
```

### Impact

- Pattern variable substitutions: Correctly preserve original scopes
- Template identifiers: Correctly get intro-scope (not in syntaxMap)
- Template-bound identifiers: Correctly get intro-scope (not in syntaxMap)
- Free identifiers: Correctly handled by freeIds logic

### Test Cases

1. `(let ((if even?)) (or #f (if 7)))` → should return `#f`
2. `(let ((p (make-parameter 1))) (parameterize ((p 2)) (p)))` → should return `2`

## Testing Log (2026-01-23)

### Tests That Pass with Fresh Symbol Approach

1. **Simple if shadowing**: `(let ((if even?)) (or #f (if 7)))` → returns `#f` ✓
   - Template's `if` correctly identified as free identifier
   - Scopes stripped, doesn't match user's `if` binding

2. **Parameterize**: `(let ((p (make-parameter 1))) (parameterize ((p 2)) (p)))` → returns `2` ✓
   - Template's `p` (bound in template's let) correctly gets intro scope
   - User's `p` (captured as pattern var `param`) correctly preserves original scopes

### Tests That FAIL with Fresh Symbol Approach

3. **Recursive or with x capture**: `(let ((x 8)) (or #f (or #f x)))` → returns `#f` (WRONG, should be `8`)
   - The `or` macro uses `x` as a local variable in template: `(let ((x test1)) ...)`
   - User's `x` should NOT be captured by template's `x`

   **Debug output analysis**:
   - User's `x` gets binding scope `0x14000012b10`
   - Template's `x` is NOT in syntaxMap (correct - fresh symbol approach works)
   - BUT template's `x` IS in freeIds: `freeIds=map[...x:0x14000367b60]`
   - This is WRONG: `x` is BOUND by the template's `let`, not free

   **Root cause hypothesis**: `collectFreeIdentifiers` walks the entire template and marks
   ALL non-pattern-variable symbols as "free", including symbols that are BOUND within
   the template itself. It doesn't account for template-introduced bindings.

### Approaches NOT YET Tried

1. **Modify collectFreeIdentifiers** to exclude template-bound identifiers
   - Would need to track binding forms (let, lambda, etc.) during template walk
   - Complexity: need to understand all binding forms

2. **Use SyntaxSymbol identity (with scopes) as map key** (user suggestion)
   - Change syntaxMap from `map[values.Value]` to key that includes scope identity
   - Would preserve full identity through recursive expansions

3. **Separate freeIds handling from syntaxMap lookup**
   - Currently freeIds affects valueToSyntaxWithOrigin behavior
   - Maybe the logic for "is this a free identifier" vs "should this get intro scope" is conflated

### Current State of Code

File: `match/syntax_adapter.go`
- `syntaxToValueWithMap`: Creates fresh symbols for ALL input SyntaxSymbols
- `valueToSyntaxWithOrigin`:
  - First checks syntaxMap (returns original if found - preserves input scopes)
  - If not in syntaxMap, checks if symbol is in freeIds
  - If free: strips scopes, doesn't add intro scope
  - If not free: strips scopes, adds intro scope

The issue: Template's `x` is in freeIds (incorrectly), so it gets "free identifier" treatment
instead of "template-introduced identifier" treatment. Both skip intro scope, but the
distinction matters for recursive expansion.

---

### NEW FINDING (later in session)

**Actual root cause identified**: The problem is NOT just freeIds. It's that `with-binding-scope`
adds scopes to EVERYTHING, including pattern variable substitutions.

**Trace for `(let ((x 8)) (or #f (or #f x)))`**:

1. User's `let` creates binding scope S1=0x14000012b10
2. User's `x` binding: scopes=[S1]
3. User's `x` reference (in body): scopes=[S1]
4. `or` macro expands, captures user's `x`
5. Template `(let ((x test1)) ...)` outputs
6. This `let` expands, `with-binding-scope` creates S2=0x14000012fc0
7. S2 is added to ENTIRE body, including substituted `(or #f x)` from pattern
8. User's `x` reference now has scopes=[S2, S1]
9. Second `or` expands, its `let` creates S3=0x14000013640
10. User's `x` reference now has scopes=[S3, S2, S1]

**Compilation debug output**:
```
DEBUG CompileSymbol: sym='x', expr.Scopes()=[0x14000013640 0x14000012fc0 0x14000012b10]
DEBUG CompileSymbol: scopes-aware path, li=0:0
DEBUG CompileSymbol: found binding with scopes=[0x14000013640]
```

**The problem**: `GetLocalIndexWithScopes` finds the FIRST binding whose scopes are a subset
of the reference's scopes. With scopes=[S3, S2, S1], ALL THREE bindings match:
- User's binding [S1] ⊆ [S3, S2, S1] ✓
- First macro's binding [S2] ⊆ [S3, S2, S1] ✓
- Second macro's binding [S3] ⊆ [S3, S2, S1] ✓

It finds the innermost (macro's) binding, which has value `#f`.

### Approaches NOT YET Tried (updated)

1. **Protect pattern variable substitutions from scope addition**
   - When substituting pattern vars, mark them as "frozen" so `with-binding-scope` doesn't add scopes
   - This is what Flatt's model calls "protecting" substituted content

2. **Use scope set arithmetic differently**
   - Current: `bindingScopes ⊆ referenceScopes`
   - Maybe need: exact match, or different algorithm for ambiguous cases

3. **Find the MOST SPECIFIC matching binding** (most scopes in common)
   - When multiple bindings match, prefer the one with most scope overlap
   - This would find user's binding because [S1] ⊆ [S3, S2, S1] with S1 in common

4. **Track substitution provenance**
   - Mark which identifiers came from pattern substitution vs template
   - Pattern substitution content shouldn't receive template's binding scopes

## Debug Path Tracking

### Test Case: `(let ((x 8)) (or #f (or #f x)))`

#### Path 1: syntaxToValue - VERIFIED
- [x] User's `x` creates fresh symbol, stored with `scopes=[]` initially
- [x] After user's `let` expands, user's `x` stored with `scopes=[S1]`
- [x] Template's `x` (from `or` macro) is NOT in syntaxMap initially (correct)
- [x] Template's `x` goes through freeIds path, gets NEW underlying symbol

#### Path 2: valueToSyntaxWithOrigin - PARTIAL
- [x] Template's `x` NOT in syntaxMap (interned pointer)
- [x] Template's `x` IS in freeIds (`isFree=true`)
- [x] `resolution` is non-nil pointer to `&{Global: nil, LocalScopes: []}`
- [x] `localScopes` check: `len([]) == 0`, should NOT enter (fixed from `!= nil`)
- [x] `globalBinding` check: `nil`, should NOT enter
- [ ] **NEEDS VERIFICATION**: Does code reach "template-bound path" after both checks?
- [ ] **NEEDS VERIFICATION**: Is intro scope being added?

#### Path 3: Scope accumulation - VERIFIED PROBLEMATIC
- [x] User's `x` reference accumulates scopes: [S3, S2, S1] after nested expansion
- [x] All three bindings match because [S1], [S2], [S3] ⊆ [S3, S2, S1]
- [x] Resolution picks innermost (macro's) binding

### Current Debug Point
Added debug at line 422: `'x' passed both checks, going to template-bound path`
**Result**: No output seen - need to verify if this line is reached

### Remaining Debug Branches
1. [ ] Verify code reaches line 422 after globalBinding check
2. [ ] If not reached, trace where code is exiting early
3. [ ] If reached, verify introScope is non-nil
4. [ ] Verify the created symbol actually has intro scope
5. [ ] Trace what happens when this symbol becomes input to `let` macro

### Code Structure Verification Needed
```
if isFree && resolution != nil {           // Line 361
    if lsp, ok := ...; ok {                // Line 366
        if len(localScopes) > 0 { ... }    // Line 372 - returns if true
    }                                       // Line 395
    if gbp, ok := ...; ok {                // Line 398
        if globalBinding != nil { ... }    // Line 403 - returns if true
    }                                       // Line 420
    // Template-bound path                  // Line 422+
}
```

## RESOLUTION (2026-01-23)

**Root cause found and fixed**: Go interface nil check bug.

The `globalBindingProvider.GetGlobal()` returned a typed nil pointer (e.g., `(*environment.GlobalIndex)(nil)`),
which is NOT equal to `nil` in Go because Go interfaces are only `nil` if both type AND value are nil.

### The Bug

```go
globalBinding := gbp.GetGlobal()
if globalBinding != nil {  // WRONG: returns true for typed-nil pointer!
    // This branch was taken incorrectly
}
```

Debug output showed: `globalBinding=<nil>, isNil=false`

### The Fix

Added reflection-based nil check in `match/syntax_adapter.go`:

```go
isActuallyNil := globalBinding == nil
if !isActuallyNil {
    // Use reflection to check if the underlying value is nil
    if rv := reflect.ValueOf(globalBinding); !rv.IsValid() || rv.IsNil() {
        isActuallyNil = true
    }
}
if !isActuallyNil {
    // Global binding path
}
// Template-bound path now correctly reached
```

### Verification

All three hygiene test cases now pass:

1. `(let ((if even?)) (or #f (if 7)))` → `#f` ✓
2. `(let ((p (make-parameter 1))) (parameterize ((p 2)) (p)))` → `2` ✓
3. `(let ((x 8)) (or #f (or #f x)))` → `8` ✓

Full test suite passes.

### Explanation

With the fix, template-bound identifiers like `x` in `(let ((x test1)) ...)` now correctly:
1. Pass the `len(localScopes) > 0` check (localScopes is empty)
2. Pass the `isActuallyNil` check (globalBinding contains nil)
3. Reach the "template-bound path"
4. Get intro scope added

The intro scope distinguishes template's `x` binding from user's `x` binding, fixing the hygiene violation.

## References

- Flatt 2016: "Binding as Sets of Scopes"
- R7RS §4.3: Macros
- `plans/FIX_TEMPLATE_SCOPE_INHERITANCE.md` - Earlier analysis
- `plans/SCOPE_DEBUGGING_NOTES.md` - Debugging session notes
