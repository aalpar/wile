# Completed Features Reference

This document summarizes features and APIs that have been implemented, organized by category.

---

## Numeric Tower (NEW)

**Location:** `go/values/numeric_tower.go`
**Plan:** `plans/NUMERIC_TOWER_REFACTOR.md`
**Status:** Complete

### Summary

Unified dispatch system for cross-type numeric operations.

### API

```go
// Type ranking
func Rank(n Number) NumericRank
func Promote(n Number, target NumericRank) Number
func PromoteBoth(a, b Number) (Number, Number)
func Simplify(n Number) Number

// Exactness
func ExactnessOf(n Number) Exactness
func ResultExactness(a, b Number) Exactness

// High-level operations
func TowerAdd(a, b Number) Number
func TowerSubtract(a, b Number) Number
func TowerMultiply(a, b Number) Number
func TowerDivide(a, b Number) Number
func TowerCompare(a, b Number) int

// Custom dispatch
func BinaryOp(a, b Number, op func(Number, Number) Number) Number
```

### Details

See [NUMERIC_TOWER.md](NUMERIC_TOWER.md) for full documentation.

---

## R7RS Conformance

**Plan:** `plans/R7RS_CONFORMANCE_PLAN.md`
**Status:** ~90% complete for `(scheme base)`

### String Operations

| Procedure | Location | Notes |
|-----------|----------|-------|
| `string-copy` | `prim_string_copy.go` | With optional start/end |
| `string->list` | `prim_string_to_list.go` | With optional start/end |
| `string-set!` | `prim_strings.go` | Mutation |
| `string-fill!` | `prim_strings.go` | Mutation |
| `string-copy!` | `prim_strings.go` | Mutation |
| `string-map` | `extensions/all/prim_all.go` | R7RS §6.7 |
| `string-for-each` | `extensions/all/prim_all.go` | R7RS §6.7 |
| Case-insensitive comparisons | `prim_string_ci_variadic.go` | All 5 procedures |

### Vector Operations

| Procedure | Location | Notes |
|-----------|----------|-------|
| `vector->list` | `prim_vectors.go` | With optional start/end |
| `vector-copy` | `prim_vectors.go` | |
| `vector-copy!` | `prim_vectors.go` | Mutation |
| `vector-fill!` | `prim_vectors.go` | Mutation |
| `vector-append` | `prim_vectors.go` | |
| `vector-map` | `prim_vectors.go` | R7RS §6.7 |
| `vector-for-each` | `prim_vectors.go` | R7RS §6.7 |
| `vector->string` | `prim_vectors.go` | |
| `string->vector` | `prim_vectors.go` | |

### List Operations

| Procedure | Location | Notes |
|-----------|----------|-------|
| `member` | `prim_lists.go` | With optional compare procedure |
| `assoc` | `prim_lists.go` | With optional compare procedure |
| `list-copy` | `prim_lists.go` | |

### Character Operations

| Procedure | Location | Notes |
|-----------|----------|-------|
| Case-insensitive comparisons | `prim_char_ci_variadic.go` | All 5 char-ci procedures |
| `char-foldcase` | `extensions/all/prim_all.go` | Unicode simple case folding |
| `digit-value` | `extensions/all/prim_all.go` | All Unicode decimal digits |

### Equality Predicates

| Procedure | Location | Notes |
|-----------|----------|-------|
| `boolean=?` | `registry/core/prim_equality.go` | |
| `symbol=?` | `registry/core/prim_equality.go` | |

### Port Operations

| Procedure | Location | Notes |
|-----------|----------|-------|
| `textual-port?` | `extensions/io/prim_ports.go` | |
| `binary-port?` | `extensions/io/prim_ports.go` | |
| `call-with-port` | `extensions/io/prim_ports.go` | |
| `flush-output-port` | `extensions/io/prim_read_write.go` | |
| `read-char` | `extensions/io/prim_read_write.go` | |
| `peek-char` | `extensions/io/prim_read_write.go` | |
| `read-line` | `extensions/io/prim_read_write.go` | |
| `char-ready?` | `extensions/io/prim_read_write.go` | |
| `read-string` | `extensions/io/prim_read_write.go` | |
| `write-string` | `extensions/io/prim_read_write.go` | |

### Error Predicates

| Procedure | Location | Notes |
|-----------|----------|-------|
| `read-error?` | `extensions/exceptions/prim_exceptions.go` | |
| `file-error?` | `extensions/exceptions/prim_exceptions.go` | |

---

## Primitive Unit Tests

**Plan:** `plans/TESTING_PLAN.md`
**Status:** Complete

All 12 phases implemented across ~90 test files in `go/registry/core/`.

### Test Pattern

```go
func TestXxx(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        {"basic case", `(xxx 1 2)`, values.NewInteger(3)},
    }
    for _, tc := range tcs {
        t.Run(tc.name, func(t *testing.T) {
            result, err := runSchemeCode(t, tc.code)
            qt.Assert(t, err, qt.IsNil)
            qt.Assert(t, result, values.SchemeEquals, tc.out)
        })
    }
}
```

### Type Coverage

All numeric primitives tested with:

| Type | Constructor |
|------|-------------|
| Integer | `values.NewInteger(42)` |
| BigInteger | `values.NewBigIntegerFromString(...)` |
| Float | `values.NewFloat(3.14)` |
| BigFloat | `values.NewBigFloatFromString(...)` |
| Rational | `values.NewRational(3, 4)` |
| Complex | `values.NewComplexFromParts(1.0, 2.0)` |

---

## Unicode Support

### Case Folding

**Location:** `extensions/all/prim_all.go`

- `char-foldcase` - Unicode simple case folding (one-to-one mapping)
- `string-foldcase` - Unicode full case folding via `golang.org/x/text/cases.Fold()`
  - Correctly handles ß → "ss" expansion
  - Correctly handles ẞ (capital sharp S) → "ss"

### Digit Value

**Location:** `extensions/all/prim_all.go`

`digit-value` handles all Unicode decimal digits (Nd category):
- Arabic-Indic digits (U+0660-U+0669)
- Extended Arabic-Indic digits (U+06F0-U+06F9)
- Devanagari digits (U+0966-U+096F)
- Bengali, Thai, and all other Unicode decimal digit scripts

---

## Library System

**Status:** ~90% complete

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | ~90% | Missing: `case`, `letrec*`, `let-syntax`, `letrec-syntax`, `syntax-error`, `define-values` |
| `(scheme char)` | 100% | |
| `(scheme complex)` | 100% | |
| `(scheme cxr)` | 100% | |
| `(scheme eval)` | 100% | |
| `(scheme file)` | 100% | |
| `(scheme inexact)` | 100% | |
| `(scheme lazy)` | 100% | |
| `(scheme load)` | 100% | |
| `(scheme process-context)` | 100% | |
| `(scheme r5rs)` | 100% | |
| `(scheme read)` | 100% | |
| `(scheme repl)` | 100% | |
| `(scheme time)` | 100% | |
| `(scheme write)` | 100% | |
| `(scheme case-lambda)` | 100% | |
| `(chibi test)` | 100% | Minimal stub for R7RS tests |

---

## Implementation Notes

### `letrec*` Implementation

**Location:** `go/registry/core/bootstrap.go:72-75`

Wile's `letrec*` simply delegates to `letrec`:

```scheme
(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var init) ...) body ...)
     (letrec ((var init) ...) body ...))))
```

This works because Wile's `letrec` expands to sequential `set!` statements, guaranteeing left-to-right evaluation as required by R7RS §4.2.2.

See [IMPLEMENTATION_NOTES.md](IMPLEMENTATION_NOTES.md) for details.

---

## Running Tests

```bash
# All tests
cd go && make test

# Numeric tower tests
cd go && go test -v ./values/ -run "TestTower|TestNumericTower"

# Coverage check
cd go && go test -cover ./registry/core/...

# Unicode tests
cd go && go test -v -run "Unicode" ./registry/core/...

# R7RS conformance tests
./dist/scheme -f r7rs-tests.scm
```

---

## References

- `plans/NUMERIC_TOWER_REFACTOR.md` - Numeric tower design
- `plans/TESTING_PLAN.md` - Test implementation plan (complete)
- `plans/R7RS_CONFORMANCE_PLAN.md` - Conformance status
- [R7RS_SEMANTIC_DIFFERENCES.md](R7RS_SEMANTIC_DIFFERENCES.md) - Documented differences from R7RS
- [IMPLEMENTATION_NOTES.md](IMPLEMENTATION_NOTES.md) - Implementation choices
