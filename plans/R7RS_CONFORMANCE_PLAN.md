# R7RS Conformance Plan

This document outlines remaining non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

**Related:** [R7RS_TEST_BUGS.md](R7RS_TEST_BUGS.md) - Bugs discovered by running the R7RS test suite

---

## Summary

| Category | Count | Status |
|----------|-------|--------|
| Missing syntax/macros | 5 | Not started |
| Library system issues | 2 | Not started |
| Tokenizer issues | 1 | Not started |
| Completed items | 30+ | Complete |
| **Total remaining** | **8** | **In progress** |

---

## Outstanding Issues

### Phase 4: Missing Syntax/Macros (R7RS §4)

These derived expression types are specified in R7RS but not implemented.

| Item | R7RS Section | Priority | Notes |
|------|--------------|----------|-------|
| `case` | §4.2.1 | High | Conditional expression; commonly used |
| `letrec*` | §4.2.2 | Medium | Sequential letrec; less common |
| `let-syntax` | §4.3.1 | Medium | Local syntax definitions |
| `letrec-syntax` | §4.3.1 | Medium | Local recursive syntax definitions |
| `syntax-error` | §4.3.1 | Low | Macro error signaling |
| `define-values` | §5.3.3 | Medium | Multiple value definition |

**Implementation location:** `go/registry/core/bootstrap.go`

#### `case` macro implementation

```scheme
(define-syntax case
  (syntax-rules (else =>)
    ((case (key ...) clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key (else => result))
     (result key))
    ((case key (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((case key ((atoms ...) => result) clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (case key clause clauses ...)))
    ((case key ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key ((atoms ...) result1 result2 ...) clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))
```

### Phase 5: Library System Issues

| Item | Priority | Notes |
|------|----------|-------|
| Auxiliary syntax exports | High | R7RS requires `(scheme base)` to export `else`, `=>`, `...`, `_`. Currently cannot be exported because they aren't bound as values. |
| Macro hygiene with internal bindings | Medium | Macros in libraries that reference library-internal helpers fail at use site. Workaround: export helpers with `%` prefix. |

**Auxiliary syntax issue:**

R7RS §7.1.1 specifies that `(scheme base)` must export auxiliary syntax keywords used in `cond`, `case`, `syntax-rules`, etc. These are:
- `else` - used in `cond`, `case`, `guard`
- `=>` - used in `cond`, `case`
- `...` - used in `syntax-rules` patterns
- `_` - used in `syntax-rules` as wildcard

Currently these cannot be exported from libraries because they have no runtime binding. Need to implement an auxiliary syntax binding mechanism that marks identifiers as syntax keywords without runtime values.

**Macro hygiene issue:**

When a macro defined in a library references a helper function also defined in that library:

```scheme
;; In (my-lib):
(define (helper x) ...)
(define-syntax my-macro
  (syntax-rules ()
    ((my-macro x) (helper x))))  ;; 'helper' should resolve to library's binding
```

The expanded code at the use site fails with "no such binding: helper". The macro expander should preserve the original binding context for identifiers introduced by the macro template.

### Phase 6: Tokenizer Issues

| Item | Priority | Notes |
|------|----------|-------|
| Scientific notation in libraries | Medium | `1e-10` fails to parse in .sld files |

**Scientific notation issue:**

Numbers in scientific notation (e.g., `1e-10`, `3.14e5`) fail to parse when used in library definition files (.sld) with error:
```
strconv.ParseInt: parsing "1e-10": invalid syntax
```

The tokenizer handles scientific notation in the REPL but the library loader uses a different code path that doesn't properly handle exponent notation.

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

## Library Status

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | ~90% | Missing: `case`, `letrec*`, `let-syntax`, `letrec-syntax`, `syntax-error`, `define-values`, auxiliary syntax |
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
| `(chibi test)` | 100% | Minimal stub implementation for running R7RS tests |

---

## Testing

### R7RS Conformance Tests

The project includes `r7rs-tests.scm` which uses the `(chibi test)` library. A minimal compatible implementation of `(chibi test)` has been created at `lib/chibi/test.sld`.

To run conformance tests:
```bash
./dist/scheme -f r7rs-tests.scm
```

**Note:** The full chibi test library requires dependencies (`chibi diff`, `chibi term ansi`, `chibi optional`, `srfi 1`) that have complex requirements. The stub implementation provides the essential test interface without these dependencies.

### Unit Tests

All implementations have been verified with comprehensive Go tests:

```bash
# Run all tests
cd go && make test

# Run Unicode-specific tests
cd go && go test -v -run "Unicode" ./registry/core/...

# Run library import tests
cd go && go test -v -run "TestSchemeLibrary" ./machine/...
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

### Chibi Test Stub
The `(chibi test)` stub exports helper functions with `%` prefix (`%test-pass`, `%test-fail`, `%approx-equal?`) to work around the macro hygiene issue with library-internal bindings.
