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
| Macro system bugs | 2 | In Progress |
| Scoping bugs | 1 | In Progress |
| Numeric comparison bugs | 1 | In Progress |
| Missing syntax/macros | 0 | Complete |
| Library system issues | 0 | Complete |
| Tokenizer issues | 0 | Complete |
| Semantic differences | 0 | Complete |
| Completed items | 46+ | Complete |
| **Total remaining** | **4** | **In Progress** |

---

## Outstanding Issues

### Macro System Bugs (R7RS §4.3)

| Item | R7RS Section | Priority | Status |
|------|--------------|----------|--------|
| `letrec-syntax` expansion failure | §4.3.1 | High | Open |
| `let-syntax` hygiene failure | §4.3.1 | High | Open |

#### Bug: `letrec-syntax` expansion failure

**Test case** (from r7rs-tests.scm line 413-430):
```scheme
(letrec-syntax
  ((my-or (syntax-rules ()
            ((my-or) #f)
            ((my-or e) e)
            ((my-or e1 e2 ...)
             (let ((temp e1))
               (if temp
                   temp
                   (my-or e2 ...)))))))
  (let ((x #f)
        (y 7)
        (temp 8)
        (let odd?)
        (if even?))
    (my-or x
           (let temp)
           (if y)
           y)))
```

**Expected:** `7`

**Actual:** Compilation error:
```
if: missing consequent: Cannot compile expression
```

**Analysis:** The recursive macro expansion of `my-or` eventually expands `(my-or)` to `#f`, but something in the nested `if` expansion chain loses track of the consequent expression.

---

#### Bug: `let-syntax` hygiene failure for local bindings

**Test case** (from r7rs-tests.scm line 408-411):
```scheme
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner))
      (m))))
```

**Expected:** `outer` (the macro `m` should capture the outer `x` at definition time)

**Actual:** `inner` (the macro is incorrectly using the inner `x`)

**Root cause analysis:**

The cross-library hygiene fix (commit 92ef270) implemented pre-resolved bindings for free identifiers in macro templates, but it only works for **global** bindings:

1. In `collectFreeIdentifiersWithEllipsis` (compile_syntax_rules.go:308), free identifiers are resolved via `env.GetGlobalIndex(sym)`, which only returns bindings from the global environment.

2. For local lexical bindings (from `let`, `lambda`, etc.), `GetGlobalIndex` returns `nil`, so no resolved binding is attached to the free identifier.

3. At expansion time, since no pre-resolved binding exists, the symbol `x` resolves via normal scoping rules to whatever is in scope at the use site (the inner `x`).

**Verification:**
```scheme
;; Global bindings work correctly:
(define outer-x 'outer-global)
(define-syntax m-global (syntax-rules () ((m-global) outer-x)))
(let ((outer-x 'inner)) (m-global))  ; => 'outer-global ✓

;; Local bindings fail:
(let ((x 'outer))
  (let-syntax ((m (syntax-rules () ((m) x))))
    (let ((x 'inner)) (m))))  ; => 'inner ✗ (should be 'outer)
```

**Fix required:** The macro compiler needs to capture the full lexical environment at macro definition time, not just global bindings. Free identifiers should be resolved against this captured environment, including local bindings from enclosing `let`/`lambda` forms.

---

### Scoping Bugs (R7RS §5.3)

| Item | R7RS Section | Priority | Status |
|------|--------------|----------|--------|
| `let*-values` internal define leaks | §5.3.2 | Medium | Open |

#### Bug: Internal define leaks through `let*-values`

**Test case** (from r7rs-tests.scm line 242-246):
```scheme
(let ((x 1))
  (let*-values ()
    (define x 2)
    #f)
  x)
```

**Expected:** `1` (the internal `define` should be local to `let*-values` body)

**Actual:** `2` (the internal define is leaking to the outer scope)

**Analysis:** Per R7RS §5.3.2, internal definitions at the beginning of a body are equivalent to `letrec*` and should not affect bindings outside that body.

---

### Numeric Comparison Bugs (R7RS §6.1)

| Item | R7RS Section | Priority | Status |
|------|--------------|----------|--------|
| `equal?` fails on large exact integers | §6.1 | Medium | Open |

#### Bug: `equal?` returns false for equal large integers

**Test case** (from r7rs-tests.scm line 215-225):
```scheme
(let*-values (((root rem) (exact-integer-sqrt (expt 2 119))))
  (list root rem))
```

**Expected:** `(815238614083298888 443242361398135744)` - and test passes

**Actual:** The computed value is correct, but `(equal? computed expected)` returns `#f`

**Analysis:** The chibi-test framework uses `equal?` for comparison. When comparing lists containing large exact integers (bignums), `equal?` appears to fail even when the values are identical. This may be an issue with bignum comparison in `equal?` or `eqv?`.

**Affected tests:**
- `(expt 2 119)` - root and remainder
- `(expt 2 120)` - root and remainder
- `(expt 2 121)` - root and remainder
- `(expt 2 140)` - remainder check

---

### Phase 4: Missing Syntax/Macros (R7RS §4) - COMPLETE

| Item | R7RS Section | Priority | Status |
|------|--------------|----------|--------|
| `case` | §4.2.1 | High | ✅ Implemented in `bootstrap.go` |
| `letrec*` | §4.2.2 | Medium | ✅ Implemented in `bootstrap.go` |
| `let-syntax` | §4.3.1 | Medium | ✅ Implemented as primitive expander |
| `letrec-syntax` | §4.3.1 | Medium | ✅ Implemented as primitive expander |
| `syntax-error` | §4.3.1 | Low | ✅ Implemented as primitive expander |
| `define-values` | §5.3.3 | Medium | ✅ Implemented in compiler |

All syntax/macros have been implemented.

---

### Phase 5: Library System Issues (Complete)

| Item | Priority | Status |
|------|----------|--------|
| Auxiliary syntax exports (`...`, `_`) | Medium | ✅ Exported |
| Macro hygiene with internal bindings | Medium | ✅ Fixed |

All library system issues have been resolved.

---

### Phase 6: Tokenizer Issues (Complete)

| Item | Priority | Status |
|------|----------|--------|
| Scientific notation for bare integers | Medium | ✅ Fixed |

All tokenizer issues have been resolved.

---

### Phase 7: Semantic Differences (Complete)

| Item | R7RS Requirement | Status |
|------|------------------|--------|
| `string-upcase` | Unicode full uppercasing | ✅ Fixed |
| `string-downcase` | Unicode full lowercasing | ✅ Fixed |

All semantic differences have been resolved.

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
- ✅ `define-values` - Multiple value definitions
- ✅ Scientific notation for bare integers (`1e-10`, `+1e10`)
- ✅ Auxiliary syntax exports (`...`, `_`) from `(scheme base)`
- ✅ `string-upcase` / `string-downcase` - Unicode full case mapping (ß → SS)
- ✅ `syntax-error` - Compile-time error signaling in macros

---

## Library Status

| Library | Status | Notes |
|---------|--------|-------|
| `(scheme base)` | 100% | Complete |
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

**Current Status:** The test suite runs but fails in section 4.3 (Macros) with a compilation error. The 4 outstanding issues documented above prevent full test suite completion.

**Previously Fixed:** 13 bugs from the R7RS test suite have been fixed. See [R7RS_TEST_BUGS.md](R7RS_TEST_BUGS.md) for details.

### Unit Tests

```bash
cd go && make test
```

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)
- [R7RS Corrected HTML](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)
- [Unicode CaseFolding.txt](https://www.unicode.org/Public/UCD/latest/ucd/CaseFolding.txt)
