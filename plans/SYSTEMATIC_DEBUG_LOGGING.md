# Systematic Debug Logging Methodology

A disciplined approach to debugging complex issues using targeted debug logging, hypothesis testing, and iterative refinement.

## Overview

When facing a bug that isn't immediately obvious from code inspection, systematic debug logging provides a structured way to:
1. Understand actual runtime behavior vs expected behavior
2. Narrow down the location of the bug through successive refinement
3. Avoid wasted effort by tracking what's been tested
4. Document the debugging process for future reference

## The Process

### Phase 1: Problem Statement

**Required before any debugging:**

```markdown
## Problem Statement

**Symptom**: [What is happening]
**Expected**: [What should happen]
**Minimal reproduction**: [Smallest test case that exhibits the bug]
```

Example:
```markdown
## Problem Statement

**Symptom**: `(let ((x 8)) (or #f (or #f x)))` returns `#f`
**Expected**: Should return `8` (user's `x` value)
**Minimal reproduction**: The expression above in the REPL
```

### Phase 2: Hypothesis Formation

Before inserting debug logging, form a hypothesis about where the bug might be:

```markdown
## Current Hypothesis

**Location**: [File and function/area suspected]
**Theory**: [Why this location might contain the bug]
**What debug output would confirm/refute**: [Expected debug output if theory is correct]
```

Example:
```markdown
## Current Hypothesis

**Location**: `match/syntax_adapter.go`, `valueToSyntaxWithOrigin()`
**Theory**: Template symbols aren't getting intro scope because they're
           incorrectly identified as having global bindings
**What debug output would confirm/refute**:
  - If `globalBinding != nil` branch is taken when globalBinding is actually nil,
    the theory is confirmed
  - Debug output showing globalBinding value and nil check result
```

### Phase 3: Debug Logging Insertion

Insert **targeted** debug logging at the hypothesized location:

```go
// Pattern: Conditional debug for specific symbols/values of interest
if v.Key == "x" {
    fmt.Printf("DEBUG functionName: variable=%v, state=%v (LINE %d)\n",
        variable, state, lineNumber)
}
```

**Guidelines:**
- Include function name in debug prefix
- Include line number for easy location
- Filter to specific values of interest (don't flood output)
- Log both the value AND any derived boolean/decision
- Log entry AND exit of conditional branches

Example:
```go
if v.Key == "x" {
    fmt.Printf("DEBUG valueToSyntaxWithOrigin: 'x' globalBinding=%v, isNil=%v (LINE 403)\n",
        globalBinding, globalBinding == nil)
}
```

### Phase 4: Execute and Record Results

Run the test and capture debug output:

```markdown
## Debug Run #N

**Hypothesis**: [What we're testing]
**Debug locations**: [Where logging was added]

**Output**:
```
[paste relevant debug output]
```

**Analysis**:
- [What the output tells us]
- [Does it confirm or refute hypothesis?]
- [What new questions arise?]
```

Example:
```markdown
## Debug Run #3

**Hypothesis**: globalBinding nil check is failing
**Debug locations**: Line 403 in valueToSyntaxWithOrigin

**Output**:
```
DEBUG valueToSyntaxWithOrigin: 'x' globalBinding=<nil>, isNil=false (LINE 403)
```

**Analysis**:
- globalBinding prints as `<nil>` but `isNil=false`
- This confirms a Go interface nil bug: typed nil pointer != nil
- The `globalBinding != nil` branch is incorrectly taken
```

### Phase 5: Decision Point

After analyzing debug output, choose one of:

#### Option A: Formulate Fix
If the bug is identified:
```markdown
## Proposed Fix

**Root cause**: [Precise description of the bug]
**Fix location**: [File:line]
**Fix description**: [What change to make]

**Before**:
```go
[original code]
```

**After**:
```go
[fixed code]
```
```

#### Option B: Refine Hypothesis
If more information needed:
```markdown
## Refined Hypothesis

**Previous hypothesis**: [What we thought]
**What we learned**: [From debug output]
**New hypothesis**: [Updated theory]
**New debug locations**: [Where to add logging next]
**Rationale**: [Why these locations will help]
```

### Phase 6: Iterate or Verify

**If fix was proposed:**
1. Implement the fix
2. Remove debug logging
3. Run tests
4. If tests pass → Document and complete
5. If tests fail → Return to Phase 2 with new hypothesis

**If hypothesis was refined:**
1. Add new debug logging
2. Return to Phase 4

## Debug Path Tracking Template

Maintain a living document of what's been tested:

```markdown
## Debug Path Tracking

### Test Case: [description]

#### Path 1: [Area name] - [STATUS: VERIFIED/PARTIAL/UNTESTED]
- [x] [Checkpoint 1 - what was verified]
- [x] [Checkpoint 2 - what was verified]
- [ ] [Checkpoint 3 - still needs verification]

#### Path 2: [Area name] - [STATUS]
- [x] [Checkpoint 1]
- [ ] [Checkpoint 2]

### Approaches Tried
1. [Approach 1] - [Result: worked/failed/partial]
2. [Approach 2] - [Result]

### Approaches NOT YET Tried
1. [Approach 3] - [Why it might help]
2. [Approach 4] - [Why it might help]
```

## Common Debug Patterns

### Pattern 1: Branch Tracing
Log entry to each branch of a conditional:

```go
if condition1 {
    fmt.Printf("DEBUG func: took branch 1\n")
    // ...
} else if condition2 {
    fmt.Printf("DEBUG func: took branch 2\n")
    // ...
} else {
    fmt.Printf("DEBUG func: took else branch\n")
    // ...
}
```

### Pattern 2: State Inspection
Log key variables at decision points:

```go
fmt.Printf("DEBUG func: key=%s, scopes=%v, found=%v\n",
    sym.Key, scopes, found)
```

### Pattern 3: Before/After Comparison
Log state before and after a transformation:

```go
fmt.Printf("DEBUG func: BEFORE transform, x=%v\n", x)
result := transform(x)
fmt.Printf("DEBUG func: AFTER transform, result=%v\n", result)
```

### Pattern 4: Loop Iteration Tracking
Track what happens in each iteration:

```go
for i, item := range items {
    fmt.Printf("DEBUG func: iteration %d, item=%v\n", i, item)
    // ...
}
```

### Pattern 5: Pointer/Identity Debugging
When pointer identity matters:

```go
fmt.Printf("DEBUG func: sym=%s, ptr=%p, interned=%p\n",
    sym.Key, sym, internedSym)
```

## Go-Specific Gotchas to Check

### Interface Nil Bug
```go
// WRONG: typed nil pointer is NOT equal to nil
var gi *GlobalIndex = nil
var any interface{} = gi
fmt.Println(any == nil)  // false!

// FIX: use reflection
if any != nil {
    if rv := reflect.ValueOf(any); !rv.IsValid() || rv.IsNil() {
        // actually nil
    }
}
```

### Slice vs Nil
```go
var s []int = nil
var s2 []int = []int{}
fmt.Println(s == nil)   // true
fmt.Println(s2 == nil)  // false
fmt.Println(len(s2))    // 0 - but not nil!
```

### Map Key Identity
```go
// Maps use pointer identity for pointer keys
m := make(map[*Symbol]int)
sym1 := &Symbol{Key: "x"}
sym2 := &Symbol{Key: "x"}
m[sym1] = 1
fmt.Println(m[sym2])  // 0, not 1! Different pointers
```

## Cleanup Checklist

After fixing the bug:

- [ ] Remove ALL debug print statements
- [ ] Remove any debug-only imports (fmt if no longer needed)
- [ ] Run full test suite
- [ ] Run linter
- [ ] Document the fix in appropriate plan file
- [ ] Update any affected CLAUDE.md files with gotchas learned

## Example: Complete Debug Session

See `plans/SYMBOL_INTERNING_HYGIENE_FIX.md` for a real example of this methodology applied to debug a macro hygiene bug, including:
- Problem statement
- Multiple hypothesis iterations
- Debug path tracking
- Final resolution documentation
