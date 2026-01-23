# R7RS Conformance Plan

This document outlines remaining non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

---

## Summary

| Category | Count | Priority |
|----------|-------|----------|
| Semantic issues | 3 | Medium |
| **Total** | **3** | |

---

## Completed Items (Removed from Plan)

The following items from the original plan have been implemented:

### String Operations (Original Phase 1)
- **Case-Insensitive Character Comparisons** - All 5 variadic char-ci procedures in `prim_char_ci_variadic.go`
- **Case-Insensitive String Comparisons** - All 5 variadic string-ci procedures in `prim_string_ci_variadic.go`
- **Min/Max Single-Argument** - Implementation supports single argument calls
- **string-copy with start/end** - Implemented in `prim_string_copy.go`
- **string->list with start/end** - Implemented in `prim_string_to_list.go`
- **string-map** - Implemented in `extensions/all/prim_all.go`
- **string-for-each** - Implemented in `extensions/all/prim_all.go`
- **String Mutation** - string-set!, string-fill!, string-copy! all implemented

### Vector Operations (Original Phase 1.1, Phase 2.1)
- **vector->list with start/end** - Implemented in `prim_vectors.go` with tests
- **vector-copy** - Implemented in `prim_vectors.go`
- **vector-copy!** - Implemented in `prim_vectors.go`
- **vector-fill!** - Implemented in `prim_vectors.go`
- **vector-append** - Implemented in `prim_vectors.go`
- **vector-map** - Implemented in `prim_vectors.go`
- **vector-for-each** - Implemented in `prim_vectors.go`
- **vector->string** - Implemented in `prim_vectors.go`
- **string->vector** - Implemented in `prim_vectors.go`

### List Operations (Original Phase 1.2, Phase 2.2)
- **member with compare** - Implemented in `prim_lists.go` with tests
- **assoc with compare** - Implemented in `prim_lists.go` with tests
- **list-copy** - Implemented in `prim_lists.go`

### Equality Predicates (Original Phase 2.3)
- **boolean=?** - Implemented in `registry/core/prim_equality.go`
- **symbol=?** - Implemented in `registry/core/prim_equality.go`

### Port Operations (Original Phase 2.4, 2.8)
- **textual-port?** - Implemented in `extensions/io/prim_ports.go`
- **binary-port?** - Implemented in `extensions/io/prim_ports.go`
- **call-with-port** - Implemented in `extensions/io/prim_ports.go`
- **flush-output-port** - Implemented in `extensions/io/prim_read_write.go`

### Error Predicates (Original Phase 2.5)
- **read-error?** - Implemented in `extensions/exceptions/prim_exceptions.go`
- **file-error?** - Implemented in `extensions/exceptions/prim_exceptions.go`

### Character I/O (Original Phase 2.6)
- **read-char** - Implemented in `extensions/io/prim_read_write.go`
- **peek-char** - Implemented in `extensions/io/prim_read_write.go`
- **read-line** - Implemented in `extensions/io/prim_read_write.go`
- **char-ready?** - Implemented in `extensions/io/prim_read_write.go`

### String I/O (Original Phase 2.7)
- **read-string** - Implemented in `extensions/io/prim_read_write.go`
- **write-string** - Implemented in `extensions/io/prim_read_write.go`

---

## Phase 3: Semantic Fixes (Priority: Medium)

### 3.1 Unicode Case Folding

| Procedure | Current Implementation | R7RS Requirement |
|-----------|------------------------|------------------|
| `char-foldcase` | `unicode.ToLower()` | Unicode SimpleCaseFolding |
| `string-foldcase` | `strings.ToLower()` | Unicode CaseFolding |

**Issue:** Case folding is not the same as lowercasing. For example:
- German ß (U+00DF) should fold to "ss"
- Turkish dotted I has special folding rules

**Implementation:**
- [ ] Use `golang.org/x/text/cases` package for proper case folding
- [ ] Update `prim_char_foldcase.go` to use `cases.Fold`
- [ ] Update `prim_string_foldcase.go` to use `cases.Fold`
- [ ] Add Unicode case folding tests

### 3.2 Unicode Digit Value

| Procedure | Current Implementation | R7RS Requirement |
|-----------|------------------------|------------------|
| `digit-value` | ASCII 0-9 only | All Unicode decimal digits |

**Issue:** Only handles ASCII digits. R7RS requires handling all Unicode decimal digits:
- Arabic-Indic: ٠١٢٣٤٥٦٧٨٩ (U+0660-U+0669)
- Devanagari: ०१२३४५६७८९ (U+0966-U+096F)
- And many more...

**Implementation:**
- [ ] Update `prim_digit_value.go` to use Unicode digit detection
- [ ] Use `unicode.IsDigit()` and calculate value from code point
- [ ] Add tests for non-ASCII digits

---

## Testing Strategy

For each fix:
1. Add conformance tests that verify R7RS behavior
2. Test edge cases (empty inputs, boundary values)
3. Test error conditions
4. Ensure backward compatibility

---

## Verification Checklist

After implementation:

```bash
# Run all tests
cd go && make test

# Check coverage
cd go && go test -cover ./registry/core/...
```

---

## Notes

### String Mutability
R7RS specifies `string-set!`, `string-fill!`, and `string-copy!` which mutate strings. These have been implemented.

### Case Folding Complexity
True Unicode case folding requires the `golang.org/x/text/cases` package. This adds a dependency but provides correct behavior.

### Unicode Digit Detection
Go's `unicode.IsDigit()` returns true for all Unicode decimal digits. The digit value can be calculated by subtracting the base digit (0) of that script's digit range.
