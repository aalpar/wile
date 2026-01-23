# R7RS Conformance Plan

This document outlines remaining non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

**Related:**
- [R7RS_TEST_BUGS.md](R7RS_TEST_BUGS.md) - Bugs discovered by running the R7RS test suite (all fixed)
- [R7RS_SEMANTIC_DIFFERENCES.md](../docs/dev/R7RS_SEMANTIC_DIFFERENCES.md) - Semantic differences from R7RS

**Last Updated:** 2026-01-23

---

## Summary

| Category | Count | Status |
|----------|-------|--------|
| Missing syntax/macros | 2 | Not started |
| Library system issues | 2 | Partially complete |
| Tokenizer issues | 1 | Not started |
| Semantic differences | 2 | Not started |
| Completed items | 40+ | Complete |
| **Total remaining** | **7** | **In progress** |

---

## Outstanding Issues

### Phase 4: Missing Syntax/Macros (R7RS §4)

| Item | R7RS Section | Priority | Status |
|------|--------------|----------|--------|
| `case` | §4.2.1 | High | ✅ Implemented in `bootstrap.go` |
| `letrec*` | §4.2.2 | Medium | ✅ Implemented in `bootstrap.go` |
| `let-syntax` | §4.3.1 | Medium | ✅ Implemented as primitive expander |
| `letrec-syntax` | §4.3.1 | Medium | ✅ Implemented as primitive expander |
| `syntax-error` | §4.3.1 | Low | ❌ Not implemented |
| `define-values` | §5.3.3 | Medium | ❌ Not implemented |

**Remaining items:**

#### `syntax-error` (R7RS §4.3.1)

`syntax-error` is used in macro definitions to signal compile-time errors:

```scheme
(define-syntax must-be-even
  (syntax-rules ()
    ((must-be-even n)
     (if (odd? n)
         (syntax-error "must be even" n)
         n))))
```

**Implementation notes:**
- Must be recognized at macro expansion time, not runtime
- Should include the template arguments in the error message

#### `define-values` (R7RS §5.3.3)

`define-values` binds multiple variables to values returned by a multiple-value expression:

```scheme
(define-values (quotient remainder) (floor/ 10 3))
;; quotient => 3, remainder => 1
```

**Implementation notes:**
- Requires `call-with-values` support (already implemented)
- Can be implemented as a macro in `bootstrap.go`

---

### Phase 5: Library System Issues

| Item | Priority | Status |
|------|----------|--------|
| Auxiliary syntax exports (`...`, `_`) | Medium | ❌ Not exported |
| Macro hygiene with internal bindings | Medium | ⚠️ Workaround available |

#### Auxiliary Syntax Exports

R7RS §7.1.1 specifies that `(scheme base)` must export auxiliary syntax keywords:

| Keyword | Used in | Status |
|---------|---------|--------|
| `else` | `cond`, `case`, `guard` | ✅ Exported |
| `=>` | `cond`, `case` | ✅ Exported |
| `...` | `syntax-rules` patterns | ❌ Not exported |
| `_` | `syntax-rules` wildcard | ❌ Not exported |

**Current status:** `else` and `=>` are exported from `(scheme base)` and work correctly. The ellipsis `...` and underscore `_` are not exported because they have no runtime binding and the library system doesn't support exporting pure auxiliary syntax.

**Impact:** Low - these are only needed for macros that re-export or rename auxiliary syntax, which is rare.

#### Macro Hygiene with Internal Bindings

When a macro defined in a library references a helper function also defined in that library:

```scheme
;; In (my-lib):
(define (helper x) ...)
(define-syntax my-macro
  (syntax-rules ()
    ((my-macro x) (helper x))))  ;; 'helper' should resolve to library's binding
```

The expanded code at the use site fails with "no such binding: helper".

**Workaround:** Export helpers with `%` prefix convention. See `lib/chibi/test.sld` for an example.

---

### Phase 6: Tokenizer Issues

| Item | Priority | Notes |
|------|----------|-------|
| Scientific notation for bare integers | Medium | `1e-10` fails; `1.0e-10` works |

#### Scientific Notation Issue

Numbers in scientific notation without a decimal point fail to parse:

```scheme
1e-10      ; Error: strconv.ParseInt: parsing "1e-10": invalid syntax
1.0e-10    ; Works: 1e-10
+1e10      ; Error
1.5e10     ; Works: 15000000000
```

**Root cause:** The tokenizer correctly identifies the number, but the value conversion attempts to parse it as an integer when there's no decimal point.

**Impact:** Medium - requires users to include decimal points in scientific notation.

---

### Phase 7: Semantic Differences

| Item | R7RS Requirement | Current Implementation | Priority |
|------|------------------|------------------------|----------|
| `string-upcase` | Unicode full uppercasing | `strings.ToUpper()` | Low |
| `string-downcase` | Unicode full lowercasing | `strings.ToLower()` | Low |

#### Unicode Full Case Mapping

R7RS requires `string-upcase` and `string-downcase` to use Unicode full case mapping, which can change string length:

```scheme
;; R7RS behavior:
(string-upcase "straße")  ; Should return "STRASSE" (7 chars)

;; Current behavior:
(string-upcase "straße")  ; Returns "STRAßE" (6 chars) - ß unchanged
```

**Fix:** Use `golang.org/x/text/cases.Upper()` and `cases.Lower()` similar to how `string-foldcase` was fixed.

---

## Completed Items

The following items have been implemented and verified:

### Syntax/Macros (R7RS §4)
- ✅ `case` - Conditional expression with datum matching
- ✅ `letrec*` - Sequential letrec with left-to-right evaluation
- ✅ `let-syntax` - Local syntax definitions
- ✅ `letrec-syntax` - Local recursive syntax definitions
- ✅ `cond-expand` - Feature-based conditional expansion
- ✅ `guard` - Exception handling with condition clauses
- ✅ `parameterize` - Dynamic parameter binding
- ✅ `delay` / `delay-force` / `force` - Lazy evaluation
- ✅ `define-record-type` - Record type definitions

### String Operations
- ✅ Case-insensitive comparisons (`string-ci=?`, `string-ci<?`, etc.)
- ✅ `string-copy` with start/end parameters
- ✅ `string->list` with start/end parameters
- ✅ `string-map`, `string-for-each`
- ✅ String mutation (`string-set!`, `string-fill!`, `string-copy!`)
- ✅ `string-foldcase` - Unicode full case folding (ß → "ss")

### Character Operations
- ✅ Case-insensitive comparisons (`char-ci=?`, `char-ci<?`, etc.)
- ✅ `char-foldcase` - Unicode simple case folding
- ✅ `digit-value` - All Unicode decimal digits (Nd category)

### Vector Operations
- ✅ `vector->list` with start/end parameters
- ✅ `vector-copy`, `vector-copy!`, `vector-fill!`, `vector-append`
- ✅ `vector-map`, `vector-for-each`
- ✅ `vector->string`, `string->vector`

### List Operations
- ✅ `member` / `assoc` with custom comparator
- ✅ `list-copy`

### I/O Operations
- ✅ `read-char`, `peek-char`, `char-ready?`
- ✅ `read-line`, `read-string`, `write-string`
- ✅ `read-u8`, `peek-u8`, `write-u8`, `u8-ready?`
- ✅ `textual-port?`, `binary-port?`
- ✅ `call-with-port`, `flush-output-port`
- ✅ Circular structure handling (`write`, `write-shared`)

### Exception Handling
- ✅ `with-exception-handler`, `raise`, `raise-continuable`
- ✅ `guard` with `=>` syntax
- ✅ `error-object?`, `error-object-message`, `error-object-irritants`
- ✅ `read-error?`, `file-error?`

### Macro System
- ✅ `syntax-rules` with custom ellipsis identifier
- ✅ Ellipsis escape form `(... <template>)`
- ✅ `_` wildcard respects literals list
- ✅ `syntax-case` (R6RS-style procedural macros)

### Miscellaneous
- ✅ `boolean=?`, `symbol=?`
- ✅ Datum labels (`#n=` and `#n#`) for shared/circular structures
- ✅ `#!fold-case` / `#!no-fold-case` directives
- ✅ Case-insensitive number prefixes (`#I`, `#E`, `#B`, `#O`, `#D`, `#X`)

---

## Library Status

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | ~95% | Missing: `syntax-error`, `define-values`, `...`/`_` exports |
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

---

## Testing

### R7RS Conformance Tests

```bash
./dist/scheme -f r7rs-tests.scm
```

All 13 bugs from the R7RS test suite have been fixed. See [R7RS_TEST_BUGS.md](R7RS_TEST_BUGS.md) for details.

### Unit Tests

```bash
cd go && make test
```

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)
- [R7RS Corrected HTML](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)
- [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
