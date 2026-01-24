# R7RS Test Bisection Log

## Objective
Identify the test(s) in r7rs-tests.scm causing infinite loop/hang.

## Test Sections Identified

| Section | Lines | Description |
|---------|-------|-------------|
| 4.1 | 43-92 | Primitive expression types |
| 4.2 | 94-394 | Derived expression types |
| 4.3 | 396-623 | Macros |
| 5 | 625-689 | Program structure |
| 6.1 | 694-752 | Equivalence Predicates |
| 6.2 | 754-1050 | Numbers |
| 6.3 | 1052-1076 | Booleans |
| 6.4 | 1078-1177 | Lists |
| 6.5 | 1179-1204 | Symbols |
| 6.6 | 1206-1296 | Characters |
| 6.7 | 1298-1480 | Strings |
| 6.8 | 1482-1559 | Vectors |
| 6.9 | 1561-1635 | Bytevectors |
| 6.10 | 1637-1788 | Control Features |
| 6.11 | 1790-1938 | Exceptions |
| 6.12 | 1940-1955 | Environments and evaluation |
| 6.13 | 1957-2477 | Input and output (includes nested Read syntax, Numeric syntax) |
| 6.14 | 2479-2514 | System interface |

## Bisection Strategy
1. First split: Run sections 4.1-6.2 (first half)
2. If hangs, narrow down within that half
3. If completes, run second half (6.3-6.14)
4. Repeat until specific test identified

---

## Test Log

### Round 1: First Half (Sections 4.1-6.2, Lines 1-1050)

**Status**: COMPLETED (no hang)
**Code range**: Lines 1-1050 (imports + sections 4.1, 4.2, 4.3, 5, 6.1, 6.2)
**Result**: Completed with errors (scope binding issue in 4.3), but NO INFINITE LOOP

---

### Round 2: Second Half (Sections 6.3-6.14, Lines 1051-end)

**Status**: HANG DETECTED
**Code range**: Lines 1051-end (sections 6.3 through 6.14)
**Result**: 6.3 Booleans completed, HUNG during 6.4 Lists

---

### Round 3: Section 6.4 Lists (Lines 1078-1177)

**Status**: HANG CONFIRMED
**Code range**: Lines 1078-1177 (section 6.4 Lists only)
**Result**: Hang confirmed in this section

---

### Round 4: First half of 6.4 (Lines 1078-1127)

**Status**: HANG DETECTED
**Code range**: Lines 1078-1127 (first half of 6.4 Lists)
**Result**: Hang in this range

---

### Round 5: First quarter of 6.4 (Lines 1080-1103)

**Status**: HANG DETECTED
**Code range**: Lines 1080-1103
**Result**: Hang in this range

---

### Round 6: Lines 1080-1091

**Status**: HANG DETECTED
**Code range**: Lines 1080-1091 (first 12 lines of section 6.4)
**Result**: Hang in this range

---

### Round 7: Isolate circular list test

**Status**: HANG CONFIRMED - ROOT CAUSE FOUND
**Code**: Minimal reproduction of the circular list test
```scheme
(let ((x (list 'a)))
  (set-cdr! x x)
  (list? x))
```
**Result**: CONFIRMED - `list?` hangs on circular lists

---

## ROOT CAUSE IDENTIFIED

**Problem**: The `list?` primitive does not detect circular lists and loops infinitely.

**Minimal Reproduction**:
```scheme
(import (scheme base))
(let ((x (list 'a)))
  (set-cdr! x x)
  (list? x))  ; <-- HANGS
```

**Expected behavior (R7RS)**: `list?` should return `#f` for circular lists.

**Location in r7rs-tests.scm**: Line 1089 `(test #f (list? x))` where `x` is made circular by line 1088 `(set-cdr! x x)`.

**Note**: There is another similar test at line 1113:
```scheme
(test #f (let ((x (list 'a))) (set-cdr! x x) (list? x)))
```

**Fix needed**: Implement tortoise-and-hare (Floyd's cycle detection) algorithm in `list?` implementation.

---

## Technical Analysis

### Code Path

1. `PrimListQ` in `go/registry/core/prim_predicates.go:114-127`
2. Calls `pr.IsList()` on the Pair
3. `Pair.IsList()` in `go/values/pair.go:67-76` calls `ForEach`
4. `Pair.ForEach()` in `go/values/pair.go:135-157` iterates via:
   ```go
   for pr != nil && !pr.IsEmptyList() {
       ...
       pr = pr0  // follows cdr pointer
   }
   ```

### Bug Location

`go/values/pair.go:143-156` - The `ForEach` loop follows cdr pointers without cycle detection. When a circular list is encountered, `pr` never becomes `nil` or empty, so the loop runs forever.

### Fix Options

**Option 1**: Add cycle detection to `IsList()` directly (simplest, most targeted)

**Option 2**: Add cycle detection to `ForEach()` (fixes the underlying iterator, but may have performance implications for all callers)

**Option 3**: Create separate `IsProperList()` with cycle detection (R7RS-specific)

**Recommended**: Option 1 - Modify `IsList()` to use Floyd's tortoise-and-hare algorithm.

---

## Fix Applied

**File**: `go/values/pair.go`

**Change**: Replaced `ForEach`-based implementation with Floyd's cycle detection algorithm.

**Verification**:
- `go test ./values/...` - PASS
- `go test ./registry/core/...` - PASS
- Circular list test `(let ((x (list 'a))) (set-cdr! x x) (list? x))` returns `#f` (no hang)
- Section 6.4 Lists no longer hangs (fails on separate `list-ref` bug)

**Note**: The full r7rs-tests.scm now progresses past the `list?` hang but encounters other unrelated errors.

