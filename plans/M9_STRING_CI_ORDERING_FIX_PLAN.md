# M9: String-CI Ordering Uses ToLower Instead of Case Folding - Fix Plan

**Bug ID:** M9 (Architectural Review - MEDIUM Priority)
**Status:** Planning
**Date:** 2026-02-12
**Branch:** `fix/architectural-review-findings-2`

## Context

The case-insensitive string ordering predicates (`string-ci<?`, `string-ci>?`, `string-ci<=?`, `string-ci>=?`) and character ordering predicates (`char-ci<?`, `char-ci>?`, `char-ci<=?`, `char-ci>=?`) use simple case conversion (`strings.ToLower()` and `unicode.ToLower()`) instead of Unicode case folding.

This creates inconsistency with `string-ci=?` (which correctly uses `strings.EqualFold`) and violates R7RS semantics, which require case folding for all case-insensitive operations.

### The Bug

**String comparisons** (`internal/extensions/all/prim_strings.go:337-359`):
```go
// string-ci<? - INCORRECT
return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
    return strings.ToLower(a) < strings.ToLower(b)  // ❌ Simple lowercase, not case folding
})

// string-ci=? - CORRECT (for reference)
return helpers.StringCompareVariadic(mc, "string-ci=?", strings.EqualFold)  // ✅ Uses case folding
```

**Character comparisons** (`internal/extensions/all/prim_characters.go:30-62`):
```go
// char-ci<? - INCORRECT
return helpers.CharCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
    return unicode.ToLower(a) < unicode.ToLower(b)  // ❌ Simple lowercase, not case folding
})
```

### Why It Matters

**Case folding vs. simple lowercasing** differ for characters like German eszett:

| Input | `strings.ToLower()` | Case folding (`cases.Fold()`) |
|-------|---------------------|-------------------------------|
| `"ß"` | `"ß"` (unchanged) | `"ss"` |
| `"ẞ"` | `"ẞ"` (unchanged) | `"ss"` |
| `"SS"` | `"ss"` | `"ss"` |

With the current (incorrect) implementation:
- `(string-ci=? "ß" "SS")` → `#t` ✅ (uses `EqualFold`, correct)
- `(string-ci<? "ß" "SS")` → comparison of `"ß"` vs `"ss"` (lexicographic ordering) ❌ (incorrect)

After case folding, both `"ß"` and `"SS"` become `"ss"`, so:
- `(string-ci=? "ß" "SS")` → `#t` ✅
- `(string-ci<? "ß" "SS")` → `#f` ✅ (both are `"ss"`)

**R7RS §6.7 requirement:**
> "For case-insensitive comparisons, implementations should use the case-folding procedure... `(string-ci=? s1 s2)` is equivalent to `(string=? (string-foldcase s1) (string-foldcase s2))`"

This applies to ALL case-insensitive string predicates, not just equality.

### Edge Cases Affected

1. **German eszett (ß/ẞ)** — Most common case
   - Lowercase ß (U+00DF) folds to `"ss"`
   - Uppercase ẞ (U+1E9E) folds to `"ss"`

2. **Greek sigma (Σ/σ/ς)** — Final vs medial forms
   - Final sigma ς (U+03C2) and medial sigma σ (U+03C3) both fold to σ

3. **Ligatures** — fi, fl, etc.
   - Some implementations fold ligatures (though R7RS doesn't specify)

4. **Turkish dotted/dotless I** — i/İ/ı/I
   - Standard Unicode case folding uses non-Turkish rules

## Solution Design

Use the existing case folding functions that are already implemented in Wile:

### For String Comparisons

Replace `strings.ToLower()` with `cases.Fold()` (from `golang.org/x/text/cases`), which is already used by `PrimStringFoldcase`.

**Before:**
```go
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
        return strings.ToLower(a) < strings.ToLower(b)
    })
}
```

**After:**
```go
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    caser := cases.Fold()
    return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
        return caser.String(a) < caser.String(b)
    })
}
```

**Optimization concern:** Creating `cases.Fold()` inside each comparison closure is inefficient. Better to create it once at package level:

```go
var (
    caseFolderOnce sync.Once
    caseFolder     cases.Caser
)

func getCaseFolded(s string) string {
    caseFolderOnce.Do(func() {
        caseFolder = cases.Fold()
    })
    return caseFolder.String(s)
}

func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
        return getCaseFolded(a) < getCaseFolded(b)
    })
}
```

### For Character Comparisons

Replace `unicode.ToLower()` with `simpleCaseFold()`, which is already implemented in `prim_characters.go` and used by `PrimCharFoldcase`.

**Before:**
```go
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
        return unicode.ToLower(a) < unicode.ToLower(b)
    })
}
```

**After:**
```go
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
        return simpleCaseFold(a) < simpleCaseFold(b)
    })
}
```

This is a straightforward substitution with no performance concerns (simple case folding is a direct function call).

## Implementation

### Phase 1: Add Package-Level Case Folder (String Comparisons)

**File:** `internal/extensions/all/prim_strings.go`

Add at package level (after imports, before functions):

```go
var (
    // caseFolderOnce ensures the case folder is initialized exactly once
    caseFolderOnce sync.Once
    // caseFolder is the Unicode case folder for string-ci comparisons
    caseFolder cases.Caser
)

// getCaseFolded returns the case-folded version of a string.
// Uses lazy initialization of the case folder via sync.Once.
// R7RS §6.7: Case-insensitive comparisons should use case folding.
func getCaseFolded(s string) string {
    caseFolderOnce.Do(func() {
        caseFolder = cases.Fold()
    })
    return caseFolder.String(s)
}
```

**Imports required:** Add `"sync"` to imports (it's already imported, so no change needed).

### Phase 2: Update String Ordering Predicates

**File:** `internal/extensions/all/prim_strings.go`

Replace all four ordering predicates:

**Lines 336-340** (`string-ci<?`):
```go
func PrimStringCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci<?", func(a, b string) bool {
        return getCaseFolded(a) < getCaseFolded(b)
    })
}
```

**Lines 342-346** (`string-ci>?`):
```go
func PrimStringCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci>?", func(a, b string) bool {
        return getCaseFolded(a) > getCaseFolded(b)
    })
}
```

**Lines 349-353** (`string-ci<=?`):
```go
func PrimStringCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci<=?", func(a, b string) bool {
        return getCaseFolded(a) <= getCaseFolded(b)
    })
}
```

**Lines 356-360** (`string-ci>=?`):
```go
func PrimStringCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.StringCompareVariadic(mc, "string-ci>=?", func(a, b string) bool {
        return getCaseFolded(a) >= getCaseFolded(b)
    })
}
```

### Phase 3: Update Character Ordering Predicates

**File:** `internal/extensions/all/prim_characters.go`

Replace all four ordering predicates (and equality for consistency, though it already works):

**Lines 30-35** (`char-ci=?`):
```go
func PrimCharCiEqVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci=?", func(a, b rune) bool {
        return simpleCaseFold(a) == simpleCaseFold(b)
    })
}
```

**Lines 37-42** (`char-ci<?`):
```go
func PrimCharCiLtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci<?", func(a, b rune) bool {
        return simpleCaseFold(a) < simpleCaseFold(b)
    })
}
```

**Lines 44-49** (`char-ci>?`):
```go
func PrimCharCiGtVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci>?", func(a, b rune) bool {
        return simpleCaseFold(a) > simpleCaseFold(b)
    })
}
```

**Lines 51-56** (`char-ci<=?`):
```go
func PrimCharCiLeVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci<=?", func(a, b rune) bool {
        return simpleCaseFold(a) <= simpleCaseFold(b)
    })
}
```

**Lines 58-63** (`char-ci>=?`):
```go
func PrimCharCiGeVariadic(_ context.Context, mc *machine.MachineContext) error {
    return helpers.CharCompareVariadic(mc, "char-ci>=?", func(a, b rune) bool {
        return simpleCaseFold(a) >= simpleCaseFold(b)
    })
}
```

### Phase 4: Add Tests for Edge Cases

**File:** `internal/extensions/all/prim_strings_test.go`

Add a new test function `TestStringCiOrderingEdgeCases`:

```go
func TestStringCiOrderingEdgeCases(t *testing.T) {
    c := qt.New(t)
    engine := newEngine(t)

    // Test German eszett case folding
    // ß (U+00DF) and ẞ (U+1E9E) both fold to "ss"
    // So ß, ẞ, SS, and ss should all be equal under case-insensitive comparison
    t.Run("eszett equality", func(t *testing.T) {
        tcs := []struct {
            name string
            code string
        }{
            {"lowercase eszett = uppercase SS", `(string-ci=? "ß" "SS")`},
            {"capital eszett = uppercase SS", `(string-ci=? "ẞ" "SS")`},
            {"lowercase eszett = lowercase ss", `(string-ci=? "ß" "ss")`},
            {"capital eszett = lowercase ss", `(string-ci=? "ẞ" "ss")`},
        }
        for _, tc := range tcs {
            t.Run(tc.name, func(t *testing.T) {
                result := eval(t, engine, tc.code)
                c.Assert(result.Internal(), qt.Equals, values.TrueValue)
            })
        }
    })

    t.Run("eszett ordering", func(t *testing.T) {
        // After case folding: ß → "ss", ẞ → "ss", SS → "ss"
        // So these should NOT have any ordering relationship (all equal)
        tcs := []struct {
            name string
            code string
            want values.Value
        }{
            {"ß not less than SS", `(string-ci<? "ß" "SS")`, values.FalseValue},
            {"ß not greater than SS", `(string-ci>? "ß" "SS")`, values.FalseValue},
            {"ß less than or equal to SS", `(string-ci<=? "ß" "SS")`, values.TrueValue},
            {"ß greater than or equal to SS", `(string-ci>=? "ß" "SS")`, values.TrueValue},

            // But ß should be less than "st" (since "ss" < "st")
            {"ß less than st", `(string-ci<? "ß" "ST")`, values.TrueValue},
            {"ß less than st lowercase", `(string-ci<? "ß" "st")`, values.TrueValue},

            // And greater than "sr" (since "ss" > "sr")
            {"ß greater than sr", `(string-ci>? "ß" "SR")`, values.TrueValue},
            {"ß greater than sr lowercase", `(string-ci>? "ß" "sr")`, values.TrueValue},
        }
        for _, tc := range tcs {
            t.Run(tc.name, func(t *testing.T) {
                result := eval(t, engine, tc.code)
                c.Assert(result.Internal(), qt.Equals, tc.want)
            })
        }
    })

    t.Run("consistency with string-foldcase", func(t *testing.T) {
        // R7RS §6.7: (string-ci<? s1 s2) should be equivalent to
        // (string<? (string-foldcase s1) (string-foldcase s2))
        tcs := []struct {
            s1, s2 string
        }{
            {"ß", "SS"},
            {"ẞ", "ss"},
            {"Hello", "WORLD"},
            {"abc", "ABC"},
        }
        for _, tc := range tcs {
            t.Run(tc.s1+" vs "+tc.s2, func(t *testing.T) {
                // Test that string-ci<? gives same result as comparing folded strings
                code := fmt.Sprintf(`(eq? (string-ci<? "%s" "%s")
                                          (string<? (string-foldcase "%s")
                                                   (string-foldcase "%s")))`,
                    tc.s1, tc.s2, tc.s1, tc.s2)
                result := eval(t, engine, code)
                c.Assert(result.Internal(), qt.Equals, values.TrueValue)

                // Same for other ordering predicates
                for _, op := range []string{">?", "<=?", ">=?"} {
                    code := fmt.Sprintf(`(eq? (string-ci%s "%s" "%s")
                                              (string%s (string-foldcase "%s")
                                                       (string-foldcase "%s")))`,
                        op, tc.s1, tc.s2, op, tc.s1, tc.s2)
                    result := eval(t, engine, code)
                    c.Assert(result.Internal(), qt.Equals, values.TrueValue)
                }
            })
        }
    })
}
```

**File:** `internal/extensions/all/prim_characters_test.go`

Add a new test function `TestCharCiOrderingEdgeCases`:

```go
func TestCharCiOrderingEdgeCases(t *testing.T) {
    c := qt.New(t)
    engine := newEngine(t)

    // Test capital sharp S (ẞ U+1E9E) which folds to lowercase sharp s (ß U+00DF)
    t.Run("capital sharp S", func(t *testing.T) {
        tcs := []struct {
            name string
            code string
            want values.Value
        }{
            {"ẞ equals ß", `(char-ci=? #\ẞ #\ß)`, values.TrueValue},
            {"ẞ not less than ß", `(char-ci<? #\ẞ #\ß)`, values.FalseValue},
            {"ẞ not greater than ß", `(char-ci>? #\ẞ #\ß)`, values.FalseValue},
            {"ẞ <= ß", `(char-ci<=? #\ẞ #\ß)`, values.TrueValue},
            {"ẞ >= ß", `(char-ci>=? #\ẞ #\ß)`, values.TrueValue},
        }
        for _, tc := range tcs {
            t.Run(tc.name, func(t *testing.T) {
                result := eval(t, engine, tc.code)
                c.Assert(result.Internal(), qt.Equals, tc.want)
            })
        }
    })

    t.Run("consistency with char-foldcase", func(t *testing.T) {
        // R7RS: char-ci comparisons should use char-foldcase semantics
        tcs := []struct {
            c1, c2 rune
        }{
            {'A', 'a'},
            {'Z', 'z'},
            {'ẞ', 'ß'},
        }
        for _, tc := range tcs {
            t.Run(string(tc.c1)+" vs "+string(tc.c2), func(t *testing.T) {
                code := fmt.Sprintf(`(eq? (char-ci<? #\\%c #\\%c)
                                          (char<? (char-foldcase #\\%c)
                                                 (char-foldcase #\\%c)))`,
                    tc.c1, tc.c2, tc.c1, tc.c2)
                result := eval(t, engine, code)
                c.Assert(result.Internal(), qt.Equals, values.TrueValue)
            })
        }
    })
}
```

## Edge Cases and Considerations

### 1. Performance

**Question:** Does case folding impact performance significantly?

**Analysis:**
- String comparisons now fold both strings before comparing (2 allocations per comparison)
- Character comparisons are negligible (simple function call)
- For variadic comparisons with N arguments, there are N-1 pairwise comparisons

**Mitigation:**
- Use `sync.Once` to initialize the case folder once (avoid repeated `cases.Fold()` calls)
- The cost is acceptable for correctness

**Alternative considered:** Cache folded strings. Rejected because:
- Adds complexity
- Unlikely to have repeated comparisons of same strings
- Memory overhead

### 2. Backwards Compatibility

**Impact:** This is a behavior change for edge cases involving special Unicode characters.

**Scenarios:**
1. **Code that depends on incorrect behavior** — Unlikely, since:
   - Most code uses ASCII strings where `ToLower` ≡ case folding
   - Code using eszett or other special characters is rare
   - Incorrect behavior is a bug, not a feature

2. **Tests that verify incorrect behavior** — Would need to be updated (but there are none)

**Decision:** Accept the behavior change as a bug fix.

### 3. R7RS Compliance

**R7RS §6.7 states:**
> "For case-insensitive comparisons, implementations should use the case-folding procedure."

The current implementation violates this for ordering predicates. The fix brings Wile into compliance.

### 4. Consistency with string-ci=?

**Current state:** `string-ci=?` uses `EqualFold` (correct) while `string-ci<?` uses `ToLower` (incorrect).

This creates inconsistent behavior:
```scheme
(string-ci=? "ß" "SS")   ; #t (correct - both fold to "ss")
(string-ci<? "ß" "SS")   ; #t (WRONG - "ß" < "ss" lexicographically)
(string-ci<=? "ß" "SS")  ; #t (by transitivity should be true, but for wrong reasons)
```

After the fix:
```scheme
(string-ci=? "ß" "SS")   ; #t (both fold to "ss")
(string-ci<? "ß" "SS")   ; #f (both fold to "ss", "ss" is not < "ss")
(string-ci<=? "ß" "SS")  ; #t (both fold to "ss", "ss" <= "ss")
```

This is mathematically consistent and R7RS-compliant.

## Verification

### 1. Existing Tests

All existing tests should continue to pass:
- `TestStringCiEq` — Already correct (uses `EqualFold`)
- `TestCharCi*` — Should pass (ASCII characters unaffected)
- All other string/character tests

Expected: **All existing tests pass** (no behavior change for ASCII)

### 2. New Edge Case Tests

The new tests should pass:
- `TestStringCiOrderingEdgeCases` — Eszett and case folding consistency
- `TestCharCiOrderingEdgeCases` — Capital sharp S

Expected: **All new tests pass**

### 3. Full Test Suite

```bash
make test
```

Expected: All tests pass

### 4. Lint Check

```bash
make lint
```

Expected: 0 issues

## Risk Assessment

**LOW RISK** — Correctness fix with minimal impact:

**Why safe:**
1. Only affects Unicode edge cases (eszett, etc.) which are rare in practice
2. ASCII strings have identical behavior (ToLower ≡ case folding for ASCII)
3. Brings implementation into R7RS compliance
4. Fixes inconsistency between `string-ci=?` and ordering predicates
5. No API changes, no signature changes
6. All existing tests pass

**Potential issues:**
- **Performance:** Minimal (lazy initialization of case folder, simple function calls for chars)
- **Compatibility:** Only code relying on incorrect eszett behavior would break (unlikely to exist)

## Success Criteria

✅ All string-ci ordering predicates use `cases.Fold()`
✅ All char-ci ordering predicates use `simpleCaseFold()`
✅ New edge case tests pass (eszett, capital sharp S)
✅ All existing tests pass
✅ Full test suite passes
✅ Lint clean (0 issues)
✅ Consistent with R7RS §6.7 case folding requirements

## Files Changed

| File | Changes |
|------|---------|
| `internal/extensions/all/prim_strings.go` | Add `getCaseFolded()` helper; update 4 ordering predicates |
| `internal/extensions/all/prim_characters.go` | Update 5 comparison predicates to use `simpleCaseFold()` |
| `internal/extensions/all/prim_strings_test.go` | Add `TestStringCiOrderingEdgeCases` |
| `internal/extensions/all/prim_characters_test.go` | Add `TestCharCiOrderingEdgeCases` |
| `plans/ARCHITECTURAL_REVIEW.md` | Mark M9 as ✅ Fixed |
| `plans/ARCHITECTURAL_REVIEW_FIXES.md` | Add M9 fix documentation |

**Estimated changes:** 6 files, ~150 lines changed (~50 code, ~100 tests)

## References

- R7RS §6.7 (Strings) — Case-insensitive comparisons should use case folding
- Unicode Standard Annex #44 (Case Folding) — https://unicode.org/reports/tr44/#CaseFolding
- Go `golang.org/x/text/cases` documentation — https://pkg.go.dev/golang.org/x/text/cases
- `plans/ARCHITECTURAL_REVIEW.md:195-200` — Original M9 bug report
- `internal/extensions/all/prim_strings.go:395-409` — Existing `PrimStringFoldcase` implementation
- `internal/extensions/all/prim_characters.go:147-165` — Existing `simpleCaseFold` implementation
