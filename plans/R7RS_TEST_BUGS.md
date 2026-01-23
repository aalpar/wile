# R7RS Test Suite Bugs

This document tracks bugs discovered when running `r7rs-tests.scm` against Wile.

**Test Source:** `r7rs-tests.scm` (based on Chibi Scheme's R7RS test suite)

---

## Summary

| Priority | Category | Count | Status |
|----------|----------|-------|--------|
| Critical | Infinite loop / hang | 1 | Not fixed |
| High | Core functionality | 6 | Not fixed |
| Medium | Advanced features | 5 | Not fixed |
| **Total** | | **12** | |

---

## Critical: Infinite Loop

### Bug #1: `write` and `display` hang on circular lists

**Priority:** Critical
**Status:** Not fixed
**Impact:** Any circular structure causes infinite loop

**Location:**
- `go/extensions/io/prim_read_write.go:206` (PrimWrite)
- `go/extensions/io/prim_read_write.go:283` (PrimDisplay)
- Root cause: `go/values/pair.go:201` - `SchemeString()` has no cycle detection

**Reproduction:**
```scheme
(let ((x (list 1)))
  (set-cdr! x x)
  (write x))  ; HANGS FOREVER
```

**R7RS Reference:** §6.13.3 - `write` and `write-shared` must handle circular structures

**Fix Plan:**

1. Add cycle detection to `SchemeString()` in `go/values/pair.go`:
   ```go
   func (p *Pair) SchemeStringWithSeen(seen map[*Pair]int, labelCounter *int) string {
       if idx, found := seen[p]; found {
           return fmt.Sprintf("#%d#", idx)  // Back-reference
       }
       // First occurrence - assign label if needed later
       seen[p] = *labelCounter
       *labelCounter++
       // ... rest of implementation
   }
   ```

2. Update `PrimWrite` and `PrimDisplay` to use cycle-aware printing

3. Implement `write-shared` properly with datum labels (`#n=` and `#n#`)

4. Keep `write-simple` as current behavior (no cycle detection, will still hang on circular structures - this is R7RS-compliant)

**Files to modify:**
- `go/values/pair.go` - Add `SchemeStringWithCycleDetection()`
- `go/extensions/io/prim_read_write.go` - Update write primitives
- `go/values/vector.go` - Also needs cycle detection for vectors

---

## High Priority Bugs

### Bug #2: `abs` doesn't support complex numbers

**Priority:** High
**Status:** Not fixed

**Location:** Primitive `abs` implementation

**Reproduction:**
```scheme
(abs 3+4i)  ; Error: expected a real number but got *values.Complex
```

**Expected:** `5` (the magnitude)

**R7RS Reference:** §6.2.6 - For complex numbers, `abs` returns the magnitude

**Fix Plan:**
1. Find the `abs` primitive implementation
2. Add case for `*values.Complex` that returns `(sqrt (+ (* real real) (* imag imag)))`

**Files to modify:**
- Likely `go/registry/core/prim_*.go` (need to locate abs primitive)

---

### Bug #3: `=>` not recognized in `cond` clauses

**Priority:** High
**Status:** Not fixed

**Location:** SRFI-1 library loading fails due to this

**Reproduction:**
```scheme
(cond ((find-tail pred ls) => car) (else #f))
; Error: no such local or global binding "=>"
```

**R7RS Reference:** §4.2.1 - `cond` clause syntax includes `(<test> => <expression>)`

**Fix Plan:**
1. The `cond` macro must recognize `=>` as auxiliary syntax
2. Check `go/registry/core/bootstrap.go` for `cond` implementation
3. Ensure `=>` is treated as a literal in the syntax-rules pattern

**Files to modify:**
- `go/registry/core/bootstrap.go` - Fix `cond` macro
- `lib/scheme/base.sld` - Ensure `=>` is properly exported as auxiliary syntax

---

### Bug #4: Datum labels don't work (`#n=` and `#n#`)

**Priority:** High
**Status:** Not fixed

**Location:** Parser/reader

**Reproduction:**
```scheme
(cadr (read (open-input-string "(#0=(1 2 3) #0#)")))
; Returns: 0 (should return: (1 2 3))
```

**R7RS Reference:** §2.4 - Datum labels for shared/circular structure

**Fix Plan:**
1. Tokenizer must recognize `#n=` as "define datum label" and `#n#` as "reference datum label"
2. Parser must maintain a label table during parsing
3. When `#n#` is encountered, look up and return the previously labeled datum

**Files to modify:**
- `go/tokenizer/tokenizer.go` - Recognize datum label tokens
- `go/parser/parser.go` - Maintain label table, handle references

---

### Bug #5: Vector quasiquote with unquote-splicing fails

**Priority:** High
**Status:** Not fixed

**Reproduction:**
```scheme
`#(10 5 ,(square 2) ,@(map square '(4 3)) 8)
; Error: unquote-splicing: not in quasiquote context
```

**R7RS Reference:** §4.2.8 - Quasiquotation works in vectors

**Fix Plan:**
1. Check quasiquote expansion for vector handling
2. Ensure `,@` inside vectors is properly recognized and expanded

**Files to modify:**
- `go/machine/expand_*.go` or `go/registry/core/bootstrap.go` - Quasiquote handling

---

### Bug #6: Hygiene bug - `let-syntax` doesn't isolate renamed keywords

**Priority:** High
**Status:** Not fixed

**Reproduction (R7RS §4.3.1 example):**
```scheme
(let-syntax
    ((when (syntax-rules ()
             ((when test stmt1 stmt2 ...)
              (if test (begin stmt1 stmt2 ...))))))
  (let ((if #t))
    (when if (set! if 'now))
    if))
; Error: syntax-rules: no matching clause for input
; Expected: 'now
```

**R7RS Reference:** §4.3 - Macros must be hygienic; the `if` in the macro template refers to the `if` from when the macro was defined, not the local binding

**Fix Plan:**
1. Review scope set implementation in `go/syntax/`
2. Ensure macro-introduced identifiers capture their definition-time bindings
3. The `if` keyword used in the macro template must resolve to the special form, not the local variable

**Files to modify:**
- `go/machine/expand_*.go` - Macro expansion
- `go/syntax/syntax.go` - Scope handling

---

### Bug #7: Uppercase inexact prefix `#I` not recognized

**Priority:** High
**Status:** Not fixed

**Reproduction:**
```scheme
(read (open-input-string "#I1"))
; Error: unknown token type: #I1
```

**R7RS Reference:** §7.1.1 - Number syntax is case-insensitive for prefixes

**Fix Plan:**
1. Update tokenizer to accept both `#i` and `#I` for inexact prefix
2. Also check `#E`/`#e`, `#B`/`#b`, `#O`/`#o`, `#D`/`#d`, `#X`/`#x`

**Files to modify:**
- `go/tokenizer/tokenizer.go` - Number prefix parsing

---

## Medium Priority Bugs

### Bug #8: Ellipsis escape in nested syntax-rules doesn't work

**Priority:** Medium
**Status:** Not fixed

**Reproduction:**
```scheme
(define-syntax be-like-begin
  (syntax-rules ()
    ((be-like-begin name)
     (define-syntax name
       (syntax-rules ()
         ((name expr (... ...))
          (begin expr (... ...))))))))
; Error: syntax-rules: no matching clause for input
```

**R7RS Reference:** §4.3.2 - `(... <template>)` escapes ellipsis in the template

**Fix Plan:**
1. Implement ellipsis escaping in syntax-rules pattern matching
2. When `(... ...)` is encountered, treat the inner `...` as a literal, not an ellipsis

**Files to modify:**
- `go/match/` or `go/define_syntax/` - Pattern matching engine

---

### Bug #9: Custom ellipsis identifier not supported

**Priority:** Medium
**Status:** Not fixed

**Reproduction:**
```scheme
(syntax-rules dots ()
  ((name expr dots)
   (begin expr dots)))
; Error: syntax-rules: missing pattern in clause
```

**R7RS Reference:** §4.3.2 - `(syntax-rules (<ellipsis>) (<literal> ...) <clause> ...)`

**Fix Plan:**
1. Parse optional ellipsis identifier in syntax-rules
2. Use specified identifier instead of `...` for ellipsis matching

**Files to modify:**
- `go/define_syntax/` - Syntax-rules parsing

---

### Bug #10: `#!fold-case` directive not processed

**Priority:** Medium
**Status:** Not fixed

**Reproduction:**
```scheme
(read (open-input-string "#!fold-case ABC"))
; Returns: fold-case (should return: abc)
```

**R7RS Reference:** §2.1 - `#!fold-case` and `#!no-fold-case` directives

**Fix Plan:**
1. Tokenizer should recognize `#!fold-case` and `#!no-fold-case`
2. Toggle case-folding mode for subsequent symbols

**Files to modify:**
- `go/tokenizer/tokenizer.go` - Directive handling

---

### Bug #11: Bignum overflow in `exact-integer-sqrt`

**Priority:** Medium
**Status:** Not fixed

**Reproduction:**
```scheme
(exact-integer-sqrt (expt 10 39))
; Error: strconv.ParseInt: parsing "31622776601683793319": value out of range
```

**Fix Plan:**
1. Find `exact-integer-sqrt` implementation
2. Ensure it uses BigInteger for large numbers
3. Verify result conversion doesn't overflow int64

**Files to modify:**
- Primitive implementation for `exact-integer-sqrt`

---

### Bug #12: `write-u8` not bound

**Priority:** Medium
**Status:** Not fixed

**Reproduction:**
```scheme
(write-u8 1 (open-output-bytevector))
; Error: no such local or global binding "write-u8"
```

**R7RS Reference:** §6.13.3 - Binary I/O procedures

**Fix Plan:**
1. Implement `write-u8` primitive
2. Should write a single byte to a binary output port

**Files to modify:**
- `go/extensions/io/` - Add binary I/O primitives
- `lib/scheme/base.sld` - Export `write-u8`

---

## Test Failures (Not Bugs)

These are differences in behavior that are documented but not necessarily bugs:

| Issue | Notes |
|-------|-------|
| Floating point precision | `9.728` vs `9.728000255822641` - acceptable precision difference |
| Unicode string handling | Various `string-ci<?` failures - may need review |

---

## Testing Commands

```bash
# Run full test suite (will hang on circular write test)
./dist/scheme r7rs-tests.scm

# Run with timeout to detect hangs
timeout 60 ./dist/scheme r7rs-tests.scm

# Create a modified test that skips circular structure tests
head -2127 r7rs-tests.scm > /tmp/safe-tests.scm
tail -n +2158 r7rs-tests.scm >> /tmp/safe-tests.scm
./dist/scheme /tmp/safe-tests.scm
```

---

## Fix Priority Order

1. **Bug #1** (Critical) - Circular structure hang - blocks running full test suite
2. **Bug #3** - `=>` in cond - blocks SRFI-1 loading
3. **Bug #6** - Hygiene - core macro system correctness
4. **Bug #7** - Case-insensitive prefixes - easy tokenizer fix
5. **Bug #2** - Complex abs - straightforward addition
6. **Bug #4** - Datum labels - enables circular structure reading
7. **Bug #5** - Vector quasiquote - macro system enhancement
8. **Bug #12** - write-u8 - missing primitive
9. **Bug #11** - Bignum overflow - numeric edge case
10. **Bug #8-10** - Advanced syntax-rules features

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)
- [R7RS Corrected HTML](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)
- Test suite: `r7rs-tests.scm` (Chibi Scheme R7RS tests)
