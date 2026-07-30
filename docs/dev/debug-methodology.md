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

### Phase 4: Execute and Record Results

Run the test and capture debug output. Record what was tested, the output, and analysis of whether it confirms or refutes the hypothesis.

### Phase 5: Decision Point

After analyzing debug output, either:
- **Formulate fix** if the bug is identified (describe root cause, fix location, before/after code)
- **Refine hypothesis** if more information is needed (what was learned, new theory, new debug locations)

### Phase 6: Iterate or Verify

**If fix was proposed:** Implement, remove debug logging, run tests. If tests fail, return to Phase 2.

**If hypothesis was refined:** Add new debug logging, return to Phase 4.

## Common Debug Patterns

### Branch Tracing
Log entry to each branch of a conditional to see which path is taken.

### State Inspection
Log key variables at decision points:
```go
fmt.Printf("DEBUG func: key=%s, scopes=%v, found=%v\n", sym.Key, scopes, found)
```

### Before/After Comparison
Log state before and after a transformation.

### Pointer/Identity Debugging
When pointer identity matters:
```go
fmt.Printf("DEBUG func: sym=%s, ptr=%p, interned=%p\n", sym.Key, sym, internedSym)
```

## Go-Specific Gotchas

### Interface Nil Bug
```go
// WRONG: typed nil pointer is NOT equal to nil
var gi *GlobalIndex = nil
var any interface{} = gi
fmt.Println(any == nil)  // false!

// FIX: use reflection or return concrete nil
```

### Slice vs Nil
```go
var s []int = nil
var s2 []int = []int{}
fmt.Println(s == nil)   // true
fmt.Println(s2 == nil)  // false — but len(s2) == 0
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
- [ ] Update any affected CLAUDE.md files with gotchas learned

## Example: Complete Debug Session

The Go interface nil check bug in `pkg/internal/match/syntax_adapter.go` (commit history) demonstrates this methodology. The session is historical (the reflection-based check has since been replaced), but the process is the point:

1. **Problem**: `(let ((x 8)) (or #f (or #f x)))` returned `#f` instead of `8`
2. **Hypothesis**: Template symbols not getting intro scope
3. **Debug logging**: Added prints at nil check branch points
4. **Discovery**: `globalBinding=<nil>, isNil=false` — Go interface nil gotcha
5. **Fix**: Added reflection-based nil check
6. **Verification**: All three hygiene test cases pass
