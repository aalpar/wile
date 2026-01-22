# R7RS Conformance Plan

This document outlines remaining non-conformance issues with R7RS-small and the plan to fix them.

**Reference:** [R7RS-small Specification](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)

---

## Summary

| Category | Count | Priority |
|----------|-------|----------|
| Missing optional arguments | 3 | High |
| Missing R7RS base procedures | 25 | Medium |
| Semantic issues | 3 | Medium |
| **Total** | **31** | |

---

## Completed Items (Removed from Plan)

The following items from the original plan have been implemented:

- **Phase 1.1 Case-Insensitive Character Comparisons** - All 5 variadic char-ci procedures implemented in `prim_char_ci_variadic.go`
- **Phase 1.2 Case-Insensitive String Comparisons** - All 5 variadic string-ci procedures implemented in `prim_string_ci_variadic.go`
- **Phase 1.3 Min/Max Single-Argument** - Implementation already supports single argument calls
- **Phase 2.1 string-copy with start/end** - Implemented in `prim_string_copy.go`
- **Phase 2.1 string->list with start/end** - Implemented in `prim_string_to_list.go`
- **Phase 3.2 string-map** - Implemented in `extensions/all/prim_all.go`
- **Phase 3.2 string-for-each** - Implemented in `extensions/all/prim_all.go`
- **Phase 3.1 String Mutation** - string-set!, string-fill!, string-copy! all implemented

---

## Phase 1: Missing Optional Arguments (Priority: High)

### 1.1 Vector Operations

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `vector->list` | `(vector->list vector)` | `(vector->list vector [start [end]])` |

**Implementation:**
- [ ] Update `prim_vector_to_list.go` to accept optional start/end
- [ ] Update registration to variadic
- [ ] Add tests

### 1.2 List Operations with Compare

| Procedure | Current | R7RS Required |
|-----------|---------|---------------|
| `member` | `(member obj list)` | `(member obj list [compare])` |
| `assoc` | `(assoc obj alist)` | `(assoc obj alist [compare])` |

**Implementation:**
- [ ] Update `prim_member.go` to accept optional compare procedure
- [ ] Update `prim_assoc.go` to accept optional compare procedure
- [ ] Update registration to variadic
- [ ] Add tests with custom comparators

---

## Phase 2: Missing R7RS Base Procedures (Priority: Medium)

### 2.1 Vector Procedures

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
- [ ] Register all in appropriate extension
- [ ] Add tests

### 2.2 List Procedures

| Procedure | Signature |
|-----------|-----------|
| `list-copy` | `(list-copy obj)` |

**Implementation:**
- [ ] Create `prim_list_copy.go`
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.3 Equality Predicates

| Procedure | Signature |
|-----------|-----------|
| `boolean=?` | `(boolean=? boolean1 boolean2 boolean3 ...)` |
| `symbol=?` | `(symbol=? symbol1 symbol2 symbol3 ...)` |

**Implementation:**
- [ ] Create `prim_boolean_eq.go` (variadic)
- [ ] Create `prim_symbol_eq.go` (variadic)
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.4 Port Predicates

| Procedure | Signature |
|-----------|-----------|
| `textual-port?` | `(textual-port? obj)` |
| `binary-port?` | `(binary-port? obj)` |
| `call-with-port` | `(call-with-port port proc)` |

**Implementation:**
- [ ] Create `prim_textual_port_q.go`
- [ ] Create `prim_binary_port_q.go`
- [ ] Create `prim_call_with_port.go`
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.5 Error Predicates

| Procedure | Signature |
|-----------|-----------|
| `read-error?` | `(read-error? obj)` |
| `file-error?` | `(file-error? obj)` |

**Implementation:**
- [ ] Create `prim_read_error_q.go`
- [ ] Create `prim_file_error_q.go`
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.6 Character I/O

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
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.7 String I/O

| Procedure | Signature |
|-----------|-----------|
| `read-string` | `(read-string k [port])` |
| `write-string` | `(write-string string [port [start [end]]])` |

**Implementation:**
- [ ] Create `prim_read_string.go`
- [ ] Create `prim_write_string.go`
- [ ] Register in appropriate extension
- [ ] Add tests

### 2.8 Port Operations

| Procedure | Signature |
|-----------|-----------|
| `flush-output-port` | `(flush-output-port [port])` |

**Implementation:**
- [ ] Create `prim_flush_output_port.go`
- [ ] Register in appropriate extension
- [ ] Add tests

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
