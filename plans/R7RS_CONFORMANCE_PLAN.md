# R7RS Conformance Plan

This document outlines all non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

---

## Summary

| Category | Count | Priority |
|----------|-------|----------|
| Variadic conformance | 12 | High |
| Missing optional arguments | 8 | High |
| Missing R7RS base procedures | 35 | Medium |
| Semantic issues | 3 | Medium |
| **Total** | **58** | |

---

## Phase 1: Variadic Conformance (Priority: High)

These procedures are currently binary but R7RS requires them to accept 2+ arguments with chained comparison semantics.

### 1.1 Case-Insensitive Character Comparisons

| Procedure | Current | R7RS Required | File |
|-----------|---------|---------------|------|
| `char-ci=?` | binary (2 args) | variadic (2+) | `prim_char_ci_eq.go` |
| `char-ci<?` | binary (2 args) | variadic (2+) | `prim_char_ci_lt.go` |
| `char-ci>?` | binary (2 args) | variadic (2+) | `prim_char_ci_gt.go` |
| `char-ci<=?` | binary (2 args) | variadic (2+) | `prim_char_ci_le.go` |
| `char-ci>=?` | binary (2 args) | variadic (2+) | `prim_char_ci_ge.go` |

**Implementation:**
- [ ] Create `charCompareCiVariadic` helper in `char_compare.go`
- [ ] Update each `prim_char_ci_*.go` to use new helper
- [ ] Update registration in `environment_tiny.go`: change `{..., 2, false, ...}` to `{..., 2, true, ...}`
- [ ] Add variadic tests

### 1.2 Case-Insensitive String Comparisons

| Procedure | Current | R7RS Required | File |
|-----------|---------|---------------|------|
| `string-ci=?` | binary (2 args) | variadic (2+) | `prim_string_ci_eq.go` |
| `string-ci<?` | binary (2 args) | variadic (2+) | `prim_string_ci_lt.go` |
| `string-ci>?` | binary (2 args) | variadic (2+) | `prim_string_ci_gt.go` |
| `string-ci<=?` | binary (2 args) | variadic (2+) | `prim_string_ci_le.go` |
| `string-ci>=?` | binary (2 args) | variadic (2+) | `prim_string_ci_ge.go` |

**Implementation:**
- [ ] Create `stringCompareCiVariadic` helper in `string_compare.go`
- [ ] Update each `prim_string_ci_*.go` to use new helper
- [ ] Update registration in `environment_tiny.go`: change `{..., 2, false, ...}` to `{..., 2, true, ...}`
- [ ] Add variadic tests

### 1.3 Min/Max Minimum Argument Count

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `max` | min 2 args | min 1 arg |
| `min` | min 2 args | min 1 arg |

**Implementation:**
- [ ] Update `prim_max.go` to handle single argument case (return that argument)
- [ ] Update `prim_min.go` to handle single argument case (return that argument)
- [ ] Update registration: change `{"max", 2, true, ...}` to `{"max", 1, true, ...}`
- [ ] Update registration: change `{"min", 2, true, ...}` to `{"min", 1, true, ...}`
- [ ] Add single-argument tests

---

## Phase 2: Missing Optional Arguments (Priority: High)

These procedures exist but are missing optional `start`/`end` or other optional parameters per R7RS.

### 2.1 String Operations

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `string-copy` | `(string-copy string)` | `(string-copy string [start [end]])` |
| `string->list` | `(string->list string)` | `(string->list string [start [end]])` |

**Implementation:**
- [ ] Update `prim_string_copy.go` to accept optional start/end
- [ ] Update `prim_string_to_list.go` to accept optional start/end
- [ ] Update registration to variadic
- [ ] Add tests for optional arguments

### 2.2 Vector Operations

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `vector->list` | `(vector->list vector)` | `(vector->list vector [start [end]])` |

**Implementation:**
- [ ] Update `prim_vector_to_list.go` to accept optional start/end
- [ ] Update registration to variadic
- [ ] Add tests

### 2.3 List Operations

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `member` | `(member obj list)` | `(member obj list [compare])` |
| `assoc` | `(assoc obj alist)` | `(assoc obj alist [compare])` |

**Implementation:**
- [ ] Update `prim_member.go` to accept optional compare procedure
- [ ] Update `prim_assoc.go` to accept optional compare procedure
- [ ] Update registration to variadic
- [ ] Add tests with custom comparators

### 2.4 Other

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `load` | `(load filename)` | `(load filename [environment-specifier])` |

**Implementation:**
- [ ] Update `prim_load.go` to accept optional environment
- [ ] Add tests

---

## Phase 3: Missing R7RS Base Procedures (Priority: Medium)

### 3.1 String Mutation Procedures

| Procedure | Signature | Notes |
|-----------|-----------|-------|
| `string-set!` | `(string-set! string k char)` | Mutate character at position |
| `string-fill!` | `(string-fill! string fill [start [end]])` | Fill region with character |
| `string-copy!` | `(string-copy! to at from [start [end]])` | Copy between strings |

**Implementation:**
- [ ] Create `prim_string_set.go`
- [ ] Create `prim_string_fill.go`
- [ ] Create `prim_string_copy_bang.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

**Note:** Strings in Go are immutable. Implementation options:
1. Make `values.String` mutable (store as `[]rune`)
2. Return error for mutation operations
3. Create mutable string wrapper type

### 3.2 String Higher-Order Procedures

| Procedure | Signature |
|-----------|-----------|
| `string-map` | `(string-map proc string1 string2 ...)` |
| `string-for-each` | `(string-for-each proc string1 string2 ...)` |

**Implementation:**
- [ ] Create `prim_string_map.go`
- [ ] Create `prim_string_for_each.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.3 Vector Procedures

| Procedure | Signature |
|-----------|-----------|
| `vector-copy` | `(vector-copy vector [start [end]])` |
| `vector-copy!` | `(vector-copy! to at from [start [end]])` |
| `vector-fill!` | `(vector-fill! vector fill [start [end]])` |
| `vector-append` | `(vector-append vector ...)` |
| `vector-map` | `(vector-map proc vector1 vector2 ...)` |
| `vector-for-each` | `(vector-for-each proc vector1 vector2 ...)` |
| `vector->string` | `(vector->string vector [start [end]])` |
| `string->vector` | `(string->vector string [start [end]])` |

**Implementation:**
- [ ] Create `prim_vector_copy.go`
- [ ] Create `prim_vector_copy_bang.go`
- [ ] Create `prim_vector_fill.go`
- [ ] Create `prim_vector_append.go`
- [ ] Create `prim_vector_map.go`
- [ ] Create `prim_vector_for_each.go`
- [ ] Create `prim_vector_to_string.go`
- [ ] Create `prim_string_to_vector.go`
- [ ] Register all in `environment_tiny.go`
- [ ] Add tests

### 3.4 List Procedures

| Procedure | Signature |
|-----------|-----------|
| `list-copy` | `(list-copy obj)` |

**Implementation:**
- [ ] Create `prim_list_copy.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.5 Equality Predicates

| Procedure | Signature |
|-----------|-----------|
| `boolean=?` | `(boolean=? boolean1 boolean2 boolean3 ...)` |
| `symbol=?` | `(symbol=? symbol1 symbol2 symbol3 ...)` |

**Implementation:**
- [ ] Create `prim_boolean_eq.go` (variadic)
- [ ] Create `prim_symbol_eq.go` (variadic)
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.6 Port Predicates

| Procedure | Signature |
|-----------|-----------|
| `textual-port?` | `(textual-port? obj)` |
| `binary-port?` | `(binary-port? obj)` |
| `call-with-port` | `(call-with-port port proc)` |

**Implementation:**
- [ ] Create `prim_textual_port_q.go`
- [ ] Create `prim_binary_port_q.go`
- [ ] Create `prim_call_with_port.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.7 Error Predicates

| Procedure | Signature |
|-----------|-----------|
| `read-error?` | `(read-error? obj)` |
| `file-error?` | `(file-error? obj)` |

**Implementation:**
- [ ] Create `prim_read_error_q.go`
- [ ] Create `prim_file_error_q.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.8 Character I/O

| Procedure | Signature |
|-----------|-----------|
| `read-char` | `(read-char [port])` |
| `peek-char` | `(peek-char [port])` |
| `read-line` | `(read-line [port])` |
| `char-ready?` | `(char-ready? [port])` |

**Implementation:**
- [ ] Create `prim_read_char.go`
- [ ] Create `prim_peek_char.go`
- [ ] Create `prim_read_line.go`
- [ ] Create `prim_char_ready_q.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.9 String I/O

| Procedure | Signature |
|-----------|-----------|
| `read-string` | `(read-string k [port])` |
| `write-string` | `(write-string string [port [start [end]]])` |

**Implementation:**
- [ ] Create `prim_read_string.go`
- [ ] Create `prim_write_string.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.10 Binary I/O

| Procedure | Signature |
|-----------|-----------|
| `read-u8` | `(read-u8 [port])` |
| `peek-u8` | `(peek-u8 [port])` |
| `u8-ready?` | `(u8-ready? [port])` |
| `write-u8` | `(write-u8 byte [port])` |
| `read-bytevector` | `(read-bytevector k [port])` |
| `read-bytevector!` | `(read-bytevector! bytevector [port [start [end]]])` |
| `write-bytevector` | `(write-bytevector bytevector [port [start [end]]])` |

**Implementation:**
- [ ] Create `prim_read_u8.go`
- [ ] Create `prim_peek_u8.go`
- [ ] Create `prim_u8_ready_q.go`
- [ ] Create `prim_write_u8.go`
- [ ] Create `prim_read_bytevector.go`
- [ ] Create `prim_read_bytevector_bang.go`
- [ ] Create `prim_write_bytevector.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

### 3.11 Port Operations

| Procedure | Signature |
|-----------|-----------|
| `flush-output-port` | `(flush-output-port [port])` |

**Implementation:**
- [ ] Create `prim_flush_output_port.go`
- [ ] Register in `environment_tiny.go`
- [ ] Add tests

---

## Phase 4: Semantic Fixes (Priority: Medium)

### 4.1 Unicode Case Folding

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

### 4.2 Unicode Digit Value

| Procedure | Current Implementation | R7RS Requirement |
|-----------|------------------------|------------------|
| `digit-value` | ASCII 0-9 only | All Unicode decimal digits |

**Issue:** Only handles ASCII digits. R7RS requires handling all Unicode decimal digits:
- Arabic-Indic: ٠١٢٣٤٥٦٧٨٩ (U+0660-U+0669)
- Extended Arabic-Indic: ۰۱۲۳۴۵۶۷۸۹ (U+06F0-U+06F9)
- Devanagari: ०१२३४५६७८९ (U+0966-U+096F)
- And many more...

**Implementation:**
- [ ] Update `prim_digit_value.go` to use Unicode digit detection
- [ ] Use `unicode.IsDigit()` and calculate value from code point
- [ ] Add tests for non-ASCII digits

---

## Implementation Order

### Sprint 1: Variadic Fixes (High Priority)
1. char-ci comparisons (5 procedures)
2. string-ci comparisons (5 procedures)
3. max/min single-argument support (2 procedures)

**Estimated effort:** 1-2 days

### Sprint 2: Optional Arguments (High Priority)
1. string-copy with start/end
2. string->list with start/end
3. vector->list with start/end
4. member with compare
5. assoc with compare

**Estimated effort:** 1-2 days

### Sprint 3: Core Missing Procedures (Medium Priority)
1. boolean=?, symbol=?
2. list-copy
3. Port predicates (textual-port?, binary-port?, call-with-port)
4. Error predicates (read-error?, file-error?)

**Estimated effort:** 1 day

### Sprint 4: String/Vector Procedures (Medium Priority)
1. String mutation (string-set!, string-fill!, string-copy!)
2. String higher-order (string-map, string-for-each)
3. Vector procedures (8 total)

**Estimated effort:** 2-3 days

### Sprint 5: I/O Procedures (Medium Priority)
1. Character I/O (read-char, peek-char, read-line, char-ready?)
2. String I/O (read-string, write-string)
3. Binary I/O (7 procedures)
4. flush-output-port

**Estimated effort:** 2-3 days

### Sprint 6: Semantic Fixes (Medium Priority)
1. Unicode case folding
2. Unicode digit-value

**Estimated effort:** 1 day

---

## Testing Strategy

For each fix:
1. Add conformance tests that verify R7RS behavior
2. Test edge cases (empty inputs, boundary values)
3. Test error conditions
4. Ensure backward compatibility

### Test File Naming
```
prim_{procedure_name}_test.go
```

### Example Test Pattern
```go
func TestCharCiEqVariadic(t *testing.T) {
    tcs := []struct {
        name string
        code string
        out  values.Value
    }{
        // R7RS requires variadic (2+ args)
        {"two args equal", `(char-ci=? #\a #\A)`, values.TrueValue},
        {"three args equal", `(char-ci=? #\a #\A #\a)`, values.TrueValue},
        {"three args not equal", `(char-ci=? #\a #\A #\b)`, values.FalseValue},
        {"four args chain", `(char-ci=? #\A #\a #\A #\a)`, values.TrueValue},
    }
    // ...
}
```

---

## Verification Checklist

After implementation:

```bash
# Run all tests
cd go && make test

# Check coverage
cd go && go test -cover ./runtime/primitives/...

# Run specific category tests
cd go && go test -v -run "TestCharCi|TestStringCi" ./runtime/primitives/...
```

---

## Notes

### String Mutability
R7RS specifies `string-set!`, `string-fill!`, and `string-copy!` which mutate strings. Go strings are immutable. Options:
1. Change `values.String` to store `[]rune` internally (breaking change)
2. Create a separate `MutableString` type
3. Document as implementation limitation

**Recommendation:** Option 1 is cleanest for R7RS conformance.

### Case Folding Complexity
True Unicode case folding requires the `golang.org/x/text/cases` package. This adds a dependency but provides correct behavior.

### Unicode Digit Detection
Go's `unicode.IsDigit()` returns true for all Unicode decimal digits. The digit value can be calculated by subtracting the base digit (0) of that script's digit range.
