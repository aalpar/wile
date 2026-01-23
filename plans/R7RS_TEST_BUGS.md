# R7RS Test Suite Bugs

This document tracks bugs discovered when running `r7rs-tests.scm` against Wile.

**Test Source:** `r7rs-tests.scm` (based on Chibi Scheme's R7RS test suite)

---

## Summary

| Priority | Category | Count | Status |
|----------|----------|-------|--------|
| Critical | Infinite loop / hang | 1 | **Fixed** |
| High | Core functionality | 7 | 7 fixed |
| Medium | Advanced features | 5 | 3 fixed, 2 remaining |
| **Total** | | **13** | 11 fixed |

---

## Critical: Infinite Loop

### Bug #1: `write` and `display` hang on circular lists

**Priority:** Critical
**Status:** ✅ **Fixed**
**Fixed in:** `go/values/scheme_writer.go`, `go/extensions/io/prim_read_write.go`

**Impact:** Any circular structure causes infinite loop

**Previous Location:**
- `go/extensions/io/prim_read_write.go:206` (PrimWrite)
- `go/extensions/io/prim_read_write.go:283` (PrimDisplay)
- Root cause: `go/values/pair.go:201` - `SchemeString()` has no cycle detection

**Reproduction:**
```scheme
(let ((x (list 1)))
  (set-cdr! x x)
  (write x))  ; Previously HUNG FOREVER - now outputs: #0=(1 . #0#)
```

**R7RS Reference:** §6.13.3 - `write` and `write-shared` must handle circular structures

**Solution Implemented:**

1. Created `go/values/scheme_writer.go` with `SchemeWriter` type:
   - Two-pass algorithm: first pass identifies shared/circular structures
   - Second pass outputs with datum labels (`#n=` for definitions, `#n#` for references)
   - Handles both pairs and vectors
   - Supports both `write` mode (quoted strings) and `display` mode (raw strings)

2. Updated primitives to use cycle-aware printing:
   - `PrimWrite` uses `values.WriteValueToString()`
   - `PrimDisplay` uses `values.DisplayValueToString()`
   - `PrimWriteShared` uses `values.WriteValueToString()`

3. Added comprehensive tests in `go/registry/core/prim_io_test.go`:
   - `TestWriteCircularPair`
   - `TestWriteCircularList`
   - `TestDisplayCircularPair`
   - `TestWriteCircularVector`
   - `TestWriteSharedCircularPair`
   - `TestWriteNonCircularPair`
   - `TestWriteNonCircularVector`

**Files modified:**
- `go/values/scheme_writer.go` - New file with cycle-aware writing
- `go/extensions/io/prim_read_write.go` - Updated write primitives
- `go/registry/core/prim_io_test.go` - Added circular structure tests
- `go/registry/core/prim_write_shared_test.go` - Fixed expected output format

---

## High Priority Bugs

### Bug #2: `abs` doesn't support complex numbers

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/registry/core/prim_arithmetic.go`

**Location:** Primitive `abs` implementation

**Reproduction:**
```scheme
(abs 3+4i)  ; Previously: Error: expected a real number but got *values.Complex
            ; Now: 5.0 (the magnitude)
```

**Expected:** `5` (the magnitude)

**R7RS Reference:** §6.2.6 - For complex numbers, `abs` returns the magnitude

**Solution Implemented:**

Added cases for `*values.Complex` and `*values.BigComplex` to `PrimAbs`:
- For Complex: uses existing `Magnitude()` method which returns `cmplx.Abs(value)`
- For BigComplex: uses existing `Magnitude()` method which returns a BigFloat
- Also added BigFloat support using the IsNegative/Negate pattern

**Files modified:**
- `go/registry/core/prim_arithmetic.go` - Added Complex, BigComplex, BigFloat cases
- `go/registry/core/prim_abs_div_extra_test.go` - Added tests for complex abs

---

### Bug #3: `=>` not recognized in `cond` clauses

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/registry/core/specialforms.go`, `lib/scheme/base.sld`, `go/machine/library.go`, `go/machine/compile_time_continuation.go`

**Location:** SRFI-1 library loading fails due to this

**Reproduction:**
```scheme
(cond ((find-tail pred ls) => car) (else #f))
; Previously: Error: no such local or global binding "=>"
; Now: Works correctly
```

**R7RS Reference:** §4.2.1 - `cond` clause syntax includes `(<test> => <expression>)`

**Solution Implemented:**

The issue was that `=>` and `else` are auxiliary syntax that need to be:
1. Registered as compile-time bindings
2. Exported from `(scheme base)`
3. Found during library import (which wasn't checking the compile environment)

**Changes made:**

1. Added `else` and `=>` to compile-time bindings in `go/registry/core/specialforms.go`
2. Added `else`, `=>`, `case`, and `letrec*` exports to `lib/scheme/base.sld`
3. Updated library import code in three places to check the compile environment:
   - `go/machine/library.go:CopyLibraryBindingsToEnv`
   - `go/machine/compile_time_continuation.go` (two locations for define-library imports and top-level imports)

**Files modified:**
- `go/registry/core/specialforms.go` - Added `else` and `=>` to compileTimeBindings
- `lib/scheme/base.sld` - Added `else`, `=>`, `case`, `letrec*` exports
- `go/machine/library.go` - Check compile environment for auxiliary syntax
- `go/machine/compile_time_continuation.go` - Check compile environment in two import paths
- `go/machine/coverage_fullruntime_test.go` - Added tests for cond/case with =>

---

### Bug #3b: Forward references don't work in library bodies

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/machine/compile_time_continuation.go`

**Location:** Library body compilation

**Reproduction:**
```scheme
;; In SRFI-1, 'any' references 'every' before 'every' is defined:
(define-library (test lib)
  (begin
    (define (any pred ls)
      ...
      (not (every (lambda (x) (not (pred x))) ls)))  ; forward reference
    (define (every pred ls)
      ...)))
; Previously: Error: no such local or global binding "every"
; Now: Works correctly (SRFI-1 loads)
```

**R7RS Reference:** §5.3.2 - Internal definitions use `letrec*` semantics where all defined variables are in scope at the start of the body.

**Solution Implemented:**

The issue was that library `begin` bodies and `include` files were being compiled sequentially without pre-declaring bindings. R7RS requires `letrec*` semantics for library bodies.

**Changes made:**

1. Created `compileLibraryBegin` function that:
   - Pass 1: Pre-declares all `define` bindings by scanning the body
   - Pass 2: Compiles all expressions (with all bindings now visible)

2. Updated `compileIncludeImpl` to batch forms and use `processFormsWithLetrecSemantics`:
   - Reads all forms from included files
   - Pre-declares all define bindings before compilation
   - Compiles all forms with forward references resolved

3. Added reusable `predeclareDefineBinding` helper function

**Files modified:**
- `go/machine/compile_time_continuation.go`:
  - Added `compileLibraryBegin()` function
  - Added `processFormsWithLetrecSemantics()` function
  - Added `predeclareDefineBinding()` helper function
  - Updated `processLibraryDeclaration()` to use `compileLibraryBegin` for begin declarations
  - Updated `compileIncludeImpl()` to batch forms and pre-declare bindings
- `go/machine/library_test.go` - Added `TestLibraryForwardReferences` test

**Impact:** SRFI-1 library now loads successfully.

---

### Bug #4: Datum labels don't work (`#n=` and `#n#`)

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/parser/parser.go`, `go/syntax/syntax_value.go`, `go/extensions/io/prim_read_write.go`

**Previous behavior:**
```scheme
(cadr (read (open-input-string "(#0=(1 2 3) #0#)")))
; Returned: 0 (the label number, not the datum)
```

**R7RS Reference:** §2.4 - Datum labels for shared/circular structure

**Solution Implemented:**

1. **Parser label table** (`go/parser/parser.go`):
   - Added `datumLabels map[int]syntax.SyntaxValue` field to Parser struct
   - On `#n=<datum>`: Store the datum in the label table and return a `SyntaxDatumLabelAssignment`
   - On `#n#`: Look up the datum in the label table and return it directly
   - For circular structures (e.g., `#0=(1 . #0#)`): Pre-create a placeholder pair and register it in the label table before reading the list contents

2. **Shared structure unwrapping** (`go/syntax/syntax_value.go`):
   - Added `UnwrapAllShared(sv SyntaxValue, cache map[SyntaxValue]values.Value)` function
   - Tracks already-unwrapped syntax values to preserve object identity
   - Pre-registers placeholder pairs/vectors in the cache before recursing to handle circular structures
   - Ensures that `eq?` returns `#t` for datum label references to the same object

3. **Read primitive** (`go/extensions/io/prim_read_write.go`):
   - Updated `PrimRead` to use `syntax.UnwrapAllShared()` instead of `UnwrapAll()`
   - Passes a cache map to preserve object identity across datum label references

**Test cases verified:**
```scheme
;; Basic datum label reference
(cadr (read (open-input-string "(#0=(1 2 3) #0#)")))  ; => (1 2 3)

;; Circular structure
(cadr (read (open-input-string "#0=(1 . #0#)")))  ; => 1

;; Object identity preserved
(let ((result (read (open-input-string "(#0=(a b c) #0#)"))))
  (eq? (car result) (cadr result)))  ; => #t
```

**Files modified:**
- `go/parser/parser.go` - Added label table and circular reference handling
- `go/syntax/syntax_value.go` - Added `UnwrapAllShared` function
- `go/extensions/io/prim_read_write.go` - Updated `PrimRead` to use shared unwrapping

---

### Bug #5: Vector quasiquote with unquote-splicing fails

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/parser/parser.go`, `go/machine/compile_time_continuation.go`

**Previous behavior:**
```scheme
`#(10 5 ,(square 2) ,@(map square '(4 3)) 8)
; Returned: #(10 5) (truncated after encountering compound expressions)
```

**R7RS Reference:** §4.2.8 - Quasiquotation works in vectors

**Solution Implemented:**

Two separate issues were fixed:

1. **Vector parsing fix** (`go/parser/parser.go`):
   - The vector parser had a loop structure that incorrectly terminated when reading compound elements (lists)
   - After `readSyntax()` read a list like `(unquote (+ 2 3))`, the tokenizer was positioned at the `)` of that list
   - The loop condition checked for `)` and exited prematurely, thinking the vector was complete
   - Fixed by restructuring the loop to match the list parsing pattern: check token type BEFORE reading, advance AFTER reading

2. **Quasiquote expansion fix** (`go/machine/compile_time_continuation.go`):
   - The `expandQuasiquote` function for `SyntaxVector` didn't handle `unquote-splicing`
   - It always generated `(list->vector (list ...))` which doesn't splice elements
   - Added detection for `unquote-splicing` at depth 1 within vectors
   - When splicing is present, now generates `(list->vector (append ...))` with proper segmentation

**Test cases verified:**
```scheme
(define (square x) (* x x))

;; Unquote with expression
`#(1 ,(+ 2 3) 4)  ; => #(1 5 4)

;; Unquote-splicing
`#(1 ,@'(2 3) 4)  ; => #(1 2 3 4)

;; Original bug case
`#(10 5 ,(square 2) ,@(map square '(4 3)) 8)  ; => #(10 5 4 16 9 8)
```

**Files modified:**
- `go/parser/parser.go` - Fixed vector element parsing loop structure
- `go/machine/compile_time_continuation.go` - Added unquote-splicing support for vectors in `expandQuasiquote`

---

### Bug #6: Hygiene bug - `let-syntax` doesn't isolate renamed keywords

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/match/syntax_adapter.go`

**Reproduction (R7RS §4.3.1 example):**
```scheme
(let-syntax
    ((when (syntax-rules ()
             ((when test stmt1 stmt2 ...)
              (if test (begin stmt1 stmt2 ...))))))
  (let ((if #t))
    (when if (set! if 'now))
    if))
; Previously: Error - "if" not found
; Now: 'now (correct)
```

**R7RS Reference:** §4.3 - Macros must be hygienic; the `if` in the macro template refers to the `if` from when the macro was defined, not the local binding

**Root Cause:**
Free identifiers in macro templates (like `if`, `begin`) were inheriting use-site scopes from the source context. When the special form `if` has empty scopes but the macro-introduced `if` had use-site scopes, scope matching failed.

**Solution Implemented:**
In `valueToSyntaxWithOrigin` (the function that converts expanded template back to syntax), free identifiers now get a scope-free source context. This allows them to match global/compile-time bindings like special forms.

```go
// For free identifiers, use a scope-free source context so they can
// match global/compile-time bindings (like special forms 'if', 'begin').
// R7RS §4.3: macro-introduced identifiers refer to definition-time bindings.
if isFree && srcCtx != nil && len(srcCtx.Scopes) > 0 {
    symCtx = &syntax.SourceContext{
        Text:   srcCtx.Text,
        File:   srcCtx.File,
        Start:  srcCtx.Start,
        End:    srcCtx.End,
        Origin: srcCtx.Origin,
        // Scopes intentionally omitted for free identifiers
    }
}
```

**Files modified:**
- `go/match/syntax_adapter.go` - Strip scopes from free identifiers during template expansion

---

### Bug #7: Uppercase inexact prefix `#I` not recognized

**Priority:** High
**Status:** ✅ **Fixed**
**Fixed in:** `go/tokenizer/tokenizer.go`

**Previous behavior:**
```scheme
(read (open-input-string "#I1"))
; Error: unknown token type: #I1
```

**R7RS Reference:** §7.1.1 - Number syntax is case-insensitive for prefixes

**Solution Implemented:**

Updated `readTypedArrayOrExactnessOrRadixOrBooleanMarker` to accept uppercase variants for all number prefixes:
- `#I`/`#i` for inexact
- `#E`/`#e` for exact
- `#B`/`#b` for binary
- `#O`/`#o` for octal
- `#D`/`#d` for decimal
- `#X`/`#x` for hexadecimal
- `#U8(`/`#u8(` for bytevectors

**Test cases verified:**
```scheme
(read (open-input-string "#I1"))    ; => 1 (inexact)
(read (open-input-string "#E1"))    ; => 1 (exact)
(read (open-input-string "#B101"))  ; => 5
(read (open-input-string "#O77"))   ; => 63
(read (open-input-string "#D99"))   ; => 99
(read (open-input-string "#XFF"))   ; => 255
(read (open-input-string "#U8(1 2 3)")) ; => #u8(1 2 3)
(read (open-input-string "#I#XFF")) ; => 255 (mixed case)
```

**Files modified:**
- `go/tokenizer/tokenizer.go` - Added uppercase variants to switch cases
- `go/tokenizer/final_coverage_test.go` - Added tests for uppercase prefixes

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
**Status:** ✅ **Fixed**
**Fixed in:** `go/parser/parser.go`

**Previous behavior:**
```scheme
#!fold-case
(define FOO 42)
foo  ; Error: no such binding "foo"
```

**R7RS Reference:** §2.1 - `#!fold-case` and `#!no-fold-case` directives

**Solution Implemented:**

Added fold-case directive processing to the parser:

1. Added `foldCase bool` field to Parser struct to track directive state

2. Added `processFoldCaseDirective` method that:
   - Checks directive name using `strings.EqualFold` for case-insensitive matching
   - Sets `p.foldCase = true` for `#!fold-case`
   - Sets `p.foldCase = false` for `#!no-fold-case`

3. Updated `wrapSyntaxSymbol` to apply case folding:
   - When `p.foldCase` is true, converts symbol name to lowercase using `strings.ToLower`
   - This ensures symbols are interned with folded case

4. Updated `ReadSyntax` to handle `SyntaxDirective`:
   - Calls `processFoldCaseDirective` when a directive is encountered
   - Continues reading to skip directives (like comments)

**Test cases verified:**
```scheme
#!fold-case
(define FOO 42)
foo  ; => 42 (FOO and foo are the same symbol)

#!fold-case
(define FOO 42)
#!no-fold-case
(define foo 100)
foo  ; => 100 (distinct from folded FOO)

#!FOLD-CASE  ; directive is case-insensitive
HELLO  ; => hello (lowercased)
```

**Files modified:**
- `go/parser/parser.go` - Added foldCase field, processFoldCaseDirective method, updated wrapSyntaxSymbol
- `go/parser/parser_test.go` - Added TestParser_FoldCase tests

---

### Bug #11: Bignum overflow in `exact-integer-sqrt`

**Priority:** Medium
**Status:** ✅ **Fixed**
**Fixed in:** `go/extensions/math/prim_math.go`

**Previous behavior:**
```scheme
(exact-integer-sqrt (expt #z10 39))
; Error: strconv.ParseInt: parsing "31622776601683793319": value out of range
```

**R7RS Reference:** §6.2.6 - `exact-integer-sqrt` returns two non-negative exact integers s and r where n = s² + r and n < (s+1)².

**Solution Implemented:**

Updated `PrimExactIntegerSqrt` to handle both Integer and BigInteger inputs:

1. Changed from single-type handling to a type switch for `*values.Integer` and `*values.BigInteger`

2. For `*values.BigInteger` input:
   - Uses `big.Int.Sqrt()` which computes floor(sqrt(n))
   - Computes remainder as r = n - s²
   - Returns both values as BigInteger

3. Preserved existing Integer handling for efficiency with small numbers

**Test cases verified:**
```scheme
;; Perfect square BigInteger
(call-with-values
  (lambda () (exact-integer-sqrt #z100000000000000000000))
  list)
; => (10000000000 0)

;; BigInteger with remainder
(call-with-values
  (lambda () (exact-integer-sqrt #z100000000000000000001))
  list)
; => (10000000000 1)

;; Very large number (10^39)
(call-with-values
  (lambda () (exact-integer-sqrt (expt #z10 39)))
  list)
; => (31622776601683793319 62545769258890964239)
```

**Files modified:**
- `go/extensions/math/prim_math.go` - Added BigInteger case to PrimExactIntegerSqrt
- `go/registry/core/prim_numeric_extra_test.go` - Added BigInteger test cases

---

### Bug #12: `write-u8` not bound

**Priority:** Medium
**Status:** ✅ **Fixed**
**Fixed in:** `go/extensions/io/prim_read_write.go`, `go/extensions/io/register.go`, `lib/scheme/base.sld`

**Previous behavior:**
```scheme
(write-u8 1 (open-output-bytevector))
; Error: no such local or global binding "write-u8"
```

**R7RS Reference:** §6.13.3 - Binary I/O procedures

**Solution Implemented:**

Added complete binary I/O primitives:
- `write-u8` - Write a byte to a binary output port
- `read-u8` - Read a byte from a binary input port
- `peek-u8` - Peek at next byte without consuming
- `u8-ready?` - Check if byte is available

**Test cases verified:**
```scheme
;; write-u8
(let ((port (open-output-bytevector)))
  (write-u8 65 port)
  (write-u8 66 port)
  (write-u8 67 port)
  (get-output-bytevector port))  ; => #u8(65 66 67)

;; read-u8
(let ((port (open-input-bytevector #u8(72 101 108))))
  (list (read-u8 port) (read-u8 port) (read-u8 port)))  ; => (72 101 108)

;; peek-u8
(let ((port (open-input-bytevector #u8(42 43))))
  (let ((first (peek-u8 port)))
    (list first (read-u8 port))))  ; => (42 42)

;; EOF detection
(let ((port (open-input-bytevector #u8(1))))
  (list (read-u8 port) (eof-object? (read-u8 port))))  ; => (1 #t)
```

**Files modified:**
- `go/extensions/io/prim_read_write.go` - Added PrimWriteU8, PrimReadU8, PrimPeekU8, PrimU8ReadyQ
- `go/extensions/io/register.go` - Registered new primitives
- `lib/scheme/base.sld` - Exported write-u8, read-u8, peek-u8, u8-ready?

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

1. ~~**Bug #1** (Critical) - Circular structure hang - blocks running full test suite~~ ✅ **Fixed**
2. ~~**Bug #3** - `=>` in cond - blocks SRFI-1 loading~~ ✅ **Fixed**
3. ~~**Bug #3b** - Forward references - blocks SRFI-1 loading~~ ✅ **Fixed**
4. ~~**Bug #6** - Hygiene - core macro system correctness~~ ✅ **Fixed**
5. ~~**Bug #7** - Case-insensitive prefixes - easy tokenizer fix~~ ✅ **Fixed**
6. ~~**Bug #2** - Complex abs - straightforward addition~~ ✅ **Fixed**
7. ~~**Bug #4** - Datum labels - enables circular structure reading~~ ✅ **Fixed**
8. ~~**Bug #5** - Vector quasiquote - macro system enhancement~~ ✅ **Fixed**
9. ~~**Bug #12** - write-u8 - missing primitive~~ ✅ **Fixed**
10. ~~**Bug #10** - fold-case directive - case folding for identifiers~~ ✅ **Fixed**
11. ~~**Bug #11** - Bignum overflow - exact-integer-sqrt with BigInteger~~ ✅ **Fixed**
12. **Bug #8-9** - Advanced syntax-rules features (ellipsis escape, custom ellipsis)

---

## References

- [R7RS-small Specification](https://small.r7rs.org/attachment/r7rs.pdf)
- [R7RS Corrected HTML](https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html)
- Test suite: `r7rs-tests.scm` (Chibi Scheme R7RS tests)
