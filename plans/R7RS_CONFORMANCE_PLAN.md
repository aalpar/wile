# R7RS Conformance Plan

This document outlines remaining non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

---

## Summary

**All planned R7RS conformance items have been implemented.**

| Category | Count | Status |
|----------|-------|--------|
| Missing optional arguments | 0 | Complete |
| Missing R7RS base procedures | 0 | Complete |
| Semantic issues | 0 | Complete |
| **Total** | **0** | **Complete** |

---

## Completed Items

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

### Semantic Fixes (Original Phase 3)

#### Unicode Case Folding
- **char-foldcase** - Uses Unicode simple case folding in `extensions/all/prim_all.go`
- **string-foldcase** - Uses Unicode full case folding via `golang.org/x/text/cases` in `extensions/all/prim_all.go`
  - Correctly handles ß → "ss" expansion
  - Correctly handles ẞ (capital sharp S) → "ss"
  - Tests in `prim_string_test.go` and `prim_char_extra_test.go`

#### Unicode Digit Value
- **digit-value** - Handles all Unicode decimal digits (Nd category) in `extensions/all/prim_all.go`
  - Supports Arabic-Indic digits (U+0660-U+0669)
  - Supports Extended Arabic-Indic digits (U+06F0-U+06F9)
  - Supports Devanagari digits (U+0966-U+096F)
  - Supports Bengali, Thai, and all other Unicode decimal digit scripts
  - Tests in `prim_char_extra_test.go`

---

## Verification

All implementations have been verified with comprehensive tests:

```bash
# Run all tests
cd go && make test

# Run Unicode-specific tests
cd go && go test -v -run "Unicode" ./registry/core/...
```

---

## Notes

### String Mutability
R7RS specifies `string-set!`, `string-fill!`, and `string-copy!` which mutate strings. These have been implemented.

### Case Folding Implementation
- `char-foldcase` uses Unicode simple case folding (one-to-one character mapping)
- `string-foldcase` uses Unicode full case folding via `golang.org/x/text/cases.Fold()` which correctly handles expansions like ß → "ss"

### Unicode Digit Detection
Go's `unicode.IsDigit()` returns true for all Unicode decimal digits (Nd category). The digit value is calculated by finding the base '0' character of each script's digit range.
