# Completed Features Reference

This document summarizes features and APIs that have been implemented, organized by category.

---

## Numeric Tower

**Location:** `values/numeric_tower.go`
**Architecture:** See `docs/dev/NUMERIC_TOWER.md`
**Status:** Stable (direct dispatch architecture finalized 2026-02-05)

### Summary

Direct dispatch architecture for cross-type numeric operations. Each type's `Add`, `Subtract`, `Multiply`, `Divide`, and `Compare` methods handle all 49 type combinations via type switches.

### Current API

```go
// Simplification
func Simplify(n Number) Number

// Exactness
func ExactnessOf(n Number) Exactness
```

**Deleted (2026-02-05):** `NumericRank`, `Rank`, `Promote`, `PromoteBoth`, `CommonRank`, `BinaryOp`, `TowerAdd`, `TowerSubtract`, `TowerMultiply`, `TowerDivide`, `TowerCompare`. These were unused infrastructure with a latent exactness bug for complex numbers. `ResultExactness` was also removed — unused in production code (only called from tests and docs).

### Details

See [NUMERIC_TOWER.md](NUMERIC_TOWER.md) for full documentation.

---

## R7RS Conformance

**Status:** Complete
**Status:** Complete — full R7RS-small conformance achieved

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

**Status:** All phases complete
**Status:** Complete

All 12 phases implemented across ~90 test files in `registry/core/`.

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

**Status:** Complete

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | 100% | |
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

**Location:** `registry/core/bootstrap.go`

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
make test

# Numeric tower tests
go test -v ./values/ -run "TestTower|TestNumericTower"

# Coverage check
go test -cover ./registry/core/...

# Unicode tests
go test -v -run "Unicode" ./registry/core/...
```

---

## References

- [NUMERIC_TOWER.md](NUMERIC_TOWER.md) - Numeric tower architecture
- [R7RS_SEMANTIC_DIFFERENCES.md](R7RS_SEMANTIC_DIFFERENCES.md) - Documented differences from R7RS
- [IMPLEMENTATION_NOTES.md](IMPLEMENTATION_NOTES.md) - Implementation choices
