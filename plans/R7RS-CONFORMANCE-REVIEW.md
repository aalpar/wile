# R7RS-small Conformance Review

**Date:** 2026-02-28
**Status:** Reference (findings documented, fixes proposed)
**Scope:** Full R7RS-small spec (§3-6), tested against `./dist/scheme` at commit `1063ad8`

---

## Summary

| Section | Tests | Pass | Fail | Rate |
|---------|-------|------|------|------|
| §3.5 Tail Calls | 24 | 23 | 1 | 96% |
| §4.1-4.2 Program Structure | 42 | 42 | 0 | 100% |
| §4.3 Macros | 24 | 22 | 2 | 92% |
| §6.1 Equivalence | 39 | 37 | 2 | 95% |
| §6.2 Numbers | 90+ | 73 | 17 | ~81% |
| §6.3.1 Booleans | 10 | 10 | 0 | 100% |
| §6.3.3 Symbols | 8 | 8 | 0 | 100% |
| §6.3 Pairs/Lists | 27 | 27 | 0 | 100% |
| §6.3.4 Characters | 29 | 20 | 9 | 69% |
| §6.3.5 Strings | 16 | 16 | 0 | 100% |
| §6.3.6 Vectors | 15 | 15 | 0 | 100% |
| §6.4 Bytevectors | 10 | 10 | 0 | 100% |
| §6.5 Control | 32 | 29 | 3 | 91% |
| §6.6 Exceptions | 18 | 16 | 2 | 89% |
| §6.12 Eval/Environments | 19 | 15 | 4 | 79% |
| §6.13 Ports & I/O | 30+ | 24 | 6 | ~80% |

---

## Findings

### CRITICAL

#### C1. `parameterize` with converter double-applies converter on restore

**R7RS §4.2.6.** The `parameterize` macro saves the current value via `(old (param))` and restores via `(p old)`. When a parameter has a converter, `(param)` returns the already-converted value, but `(p old)` applies the converter again.

**Reproduction:**
```scheme
(define p (make-parameter 10 (lambda (x) (* x 2))))
;; p stores 20 (correct: 10 * 2)
(parameterize ((p 5))
  (p))  ;; returns 10 (correct: 5 * 2)
(p)    ;; returns 40 (BUG: should be 20, but 20 * 2 = 40)
```

**Location:** `registry/core/bootstrap.scm:141-152`

**Fix:** The restore thunk must bypass the converter. Either store the raw internal value, or provide a low-level set that skips conversion.

---

#### C2. `write` outputs raw bytes for named characters, not R7RS names

**R7RS §6.13.3.** `write` must produce readable output. For the 9 named characters (`alarm`, `backspace`, `delete`, `escape`, `newline`, `null`, `return`, `space`, `tab`), `write` outputs `#\` followed by the raw control byte instead of the character name.

**Reproduction:**
```scheme
(write #\newline)  ;; outputs #\ followed by literal 0x0A byte
                   ;; should output #\newline
```

**Location:** Character `SchemeString()` method in `values/`

**Fix:** Add a lookup table mapping the 9 control codepoints to their R7RS names in the character write path.

---

#### C3. Inexact integer-valued arithmetic results write/display as exact

**R7RS §6.13.3.** `write` must produce output that `read` can reconstruct as an equivalent value. `(+ 1 1.0)` is inexact, but writes as `2` (which reads back as exact `2`).

**Reproduction:**
```scheme
(write (+ 1 1.0))          ;; outputs 2, should output 2.0
(number->string (+ 1 1.0)) ;; returns "2", should return "2.0"
(inexact? (+ 1 1.0))       ;; #t (correctly marked inexact)
```

**Root cause:** Integer + Float arithmetic produces a BigFloat. BigFloat's `String()` method drops `.0` for integer-valued results.

**Location:** `values/big_float.go` — `String()` or `SchemeString()` method

**Fix:** Ensure BigFloat's string representation always includes a decimal point when the value is integer-valued, matching Float behavior.

---

#### C4. `scheme-report-environment` is identical to `interaction-environment`

**R7RS §6.12.** `scheme-report-environment` should return an immutable environment with only R7RS bindings. Currently it returns the same object as `interaction-environment`.

**Reproduction:**
```scheme
(eq? (interaction-environment) (scheme-report-environment 5)) ;; #t (wrong)
(define unique-test-var 777)
(eval 'unique-test-var (scheme-report-environment 5))         ;; 777 (wrong)
```

Also: `environment` and `scheme-report-environment` are mutable — `eval` can `define` new bindings in them.

**Location:** Environment creation in `machine/` or `registry/`

**Fix:** Create distinct environment objects. `scheme-report-environment` and `environment` should return frozen copies. `null-environment` already returns fresh objects correctly.

---

#### C5. `read-bytevector` / `read-bytevector!` short reads from `bufio.Reader`

**R7RS §6.13.3.** `read-bytevector` should read up to k bytes. A single `p.Read(buf)` call on a `bufio.Reader`-backed port returns only what's in the internal buffer (4096 bytes default).

**Reproduction:**
```scheme
;; Write 8192 bytes to a file, then:
(let ((p (open-binary-input-file "test.bin")))
  (let ((a (bytevector-length (read-bytevector 4000 p)))
        (b (bytevector-length (read-bytevector 4000 p))))
    (list a b)))
;; Returns (4000 96) — should return (4000 4000)
```

**Location:** `internal/extensions/io/prim_read_write.go` — `PrimReadBytevector` and `PrimReadBytevectorBang`

**Fix:** Replace `p.Read(buf)` with `io.ReadFull(p, buf)` or a read loop.

---

### HIGH

#### H1. `apply` in tail position grows Go stack

**R7RS §3.5.** Proper tail recursion requires unbounded tail calls. `apply` in tail position creates recursive Go stack frames via `PrimApply` → `ApplyCallable` → `Run()`.

**Reproduction:**
```scheme
(define (f n) (if (zero? n) 'done (apply f (list (- n 1)))))
(f 1000000)  ;; Go stack overflow at ~300K
```

**Location:** `registry/core/prim_control.go` — `PrimApply`

---

#### H2. `sqrt` does not preserve exactness for perfect squares

**R7RS §6.2.6.** "If z is exact, the result is exact (if possible)."

```scheme
(sqrt 4)    ;; returns 2.0 (should be exact 2)
(sqrt 9/4)  ;; returns 1.5 (should be exact 3/2)
(sqrt -4)   ;; returns 0.0+2.0i (should be exact 0+2i)
```

---

#### H3. Exact complex division returns inexact

**R7RS §6.2.6.** Exact op exact should be exact.

```scheme
(/ 3+4i 1+2i)  ;; returns 2.2-0.4i (should be 11/5-2/5i)
(+ 1+2i 3+4i)  ;; returns 4+6i (correctly exact)
(* 1+2i 3+4i)  ;; returns -5+10i (correctly exact)
```

Only division uses floating-point internally. Addition and multiplication preserve exactness correctly.

---

#### H4. Division by inexact zero errors instead of returning infinity

**R7RS §6.2.6 + IEEE 754.** Division by exact zero should error. Division by inexact zero should return `+inf.0`/`-inf.0`/`+nan.0`.

```scheme
(/ 1 0)    ;; error (correct)
(/ 1 0.0)  ;; error (should return +inf.0)
(/ -1 0.0) ;; error (should return -inf.0)
(/ 0.0 0.0);; error (should return +nan.0)
```

---

#### H5. `string->number` doesn't parse complex or special values

**R7RS §7.1.1.**

```scheme
(string->number "3+4i")   ;; #f (should be 3+4i)
(string->number "+inf.0") ;; #f (should be +inf.0)
(string->number "-inf.0") ;; #f (should be -inf.0)
(string->number "+nan.0") ;; #f (should be +nan.0)
(string->number "+i")     ;; #f (should be 0+1i)
```

---

#### H6. `current-input-port` parameterize crashes on non-CharacterInputPort

**R7RS §6.13.1.** `GetCurrentInputPort()` in `internal/extensions/io/state.go` does a hard type assertion to `*CharacterInputPort`. When `parameterize` sets `current-input-port` to a `StringInputPort`, this panics.

---

### MEDIUM

#### M1. `expt` with rational exponents doesn't return exact when possible

```scheme
(expt 4 1/2)   ;; 2.0 (should be exact 2)
(expt -1 1/2)  ;; +nan.0 (should be 0+1i)
```

---

#### M2. `real-part`/`imag-part`/`magnitude` lose exactness on non-complex reals

```scheme
(real-part 3)   ;; 3.0 (should be exact 3)
(imag-part 3)   ;; 0.0 (should be exact 0)
(magnitude 3)   ;; 3.0 (should be exact 3)
```

---

#### M3. `guard` re-raise fails with `with-exception-handler`

**R7RS §4.2.7.** When no `guard` clause matches, the re-raise via `(raise var)` in `guard-aux` executes inside the handler's dynamic extent.

```scheme
(with-exception-handler
  (lambda (e) (list 'caught e))
  (lambda ()
    (guard (inner ((symbol? inner) 'sym))
      (raise 42))))
;; Error: handler returned from non-continuable exception
;; Should return: (caught 42)
```

**Location:** `registry/core/bootstrap.scm:155-181`

---

#### M4. `dynamic-wind` double-fires after thunks at top level

Nested `dynamic-wind` + `call/cc` escape fires after-thunks twice at top level, but correctly inside `let` contexts.

---

#### M5. `eval` drops multiple return values

```scheme
(call-with-values
  (lambda () (eval '(values 1 2 3) (interaction-environment)))
  list)
;; Returns (1) — should return (1 2 3)
```

---

#### M6. Textual output ops accept binary ports

`write-char`, `write-string`, `display`, `newline`, `write`, `write-simple`, `write-shared` use `getOptionalOutputPort()` which accepts any `OutputPort`, including binary ports. R7RS says textual operations on binary ports are an error.

**Location:** `internal/extensions/io/prim_read_write.go`

---

#### M7. `syntax-rules` vector patterns with pattern variables don't match

```scheme
(define-syntax vec-first
  (syntax-rules ()
    ((vec-first #(x rest ...)) x)))
(vec-first #(1 2 3))  ;; ERROR: no matching clause
```

Empty vector patterns and literal-only vector patterns work. Only pattern variables inside vectors fail.

---

#### M8. Dotted pair patterns in `syntax-rules` don't work correctly

```scheme
(define-syntax rest-test
  (syntax-rules ()
    ((rest-test a . b) (list a b))))
(rest-test 1 2 3)  ;; ERROR
```

---

### LOW

#### L1. `number->string` ignores radix for BigInt

```scheme
(number->string (expt 2 64) 16)  ;; "18446744073709551616" (decimal, not hex)
```

---

#### L2. `positive?`/`negative?` reject real-valued complex

```scheme
(real? 3+0i)     ;; #t
(positive? 3+0i) ;; error (should accept since it's real)
```

---

#### L3. Number prefix ordering

```scheme
#x#e10  ;; error (R7RS allows radix-then-exactness ordering)
#e#x10  ;; 16 (exactness-then-radix works)
```

---

#### L4. `read-string 0` returns EOF instead of `""`

```scheme
(read-string 0 (open-input-string "hello"))  ;; #!eof (should be "")
```

---

#### L5. `close-input-port`/`close-output-port` don't validate direction

Both map to `PrimClosePort` which accepts any `Port`. R7RS says `close-input-port` should only accept input ports.

---

#### L6. Bytevector display format uses extra spaces

```scheme
(write #u8(1 2 3))  ;; #u8( 1 2 3 ) — conventional: #u8(1 2 3)
```

---

#### L7. `char-ready?`/`u8-ready?` always return `#t`

Documented in `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`. Conservative-safe behavior.

---

### LATENT / EDGE-CASE

#### E1. Datum-label circular literals crash compiler

```scheme
(equal? '#1=(a . #1#) '#2=(a . #2#))  ;; Go stack overflow
```

`internSymbolsInValue` in `machine/compile_time_continuation.go:351-365` has no cycle detection.

---

#### E2. `helpers.Eqv` returns `true` for BigFloat NaN

```go
// registry/helpers/equality.go:50-53
// BigFloat NaN stores zero-valued *big.Float, so Cmp returns 0
helpers.Eqv(BigFloatNaN, BigFloatNaN) // true (wrong)
```

Only reachable via BigFloat Inf arithmetic. Standard `+nan.0` (Float) works correctly.

---

#### E3. `Float.EqualTo` panics comparing Float NaN with BigFloat

`big.Float.SetFloat64(NaN)` panics. Reachable if `equal?` dispatches Float NaN against BigFloat.

**Location:** `values/float.go:283-292`

---

## bufio Assessment

**Question:** Does Go's `bufio.Reader`/`bufio.Writer` cause R7RS non-conformance in the port system?

**Answer:** One real bug (C5), otherwise sound.

| Concern | Status |
|---------|--------|
| `peek-char`/`peek-u8` with bufio | PASS — uses ReadRune+UnreadRune / ReadByte+UnreadByte |
| `read-bytevector` short reads | **FAIL (C5)** — `bufio.Reader.Read()` returns only buffered data |
| Flush-on-close | PASS — `flushThenClose` properly flushes `bufio.Writer` |
| `read-line` line endings | PASS — handles `\n`, `\r\n`, `\r` |
| String ports | N/A — bypass bufio entirely (use `bytes.Buffer`) |
| `read` then `read-char` interleaving | PASS — tokenizer shares the `bufio.Reader` |
| Binary vs textual separation | Partial — binary-on-textual enforced; textual-on-binary not (M6) |

---

## What Works Well

- **Proper tail recursion** — 23/24 tail positions pass at 1M iterations
- **Macro hygiene** — scope-set model correctly prevents capture
- **Numeric tower** — complete with correct exactness contagion, round-to-even, GCD/LCM, rationalize, bigint promotion
- **Data types** — pairs, lists, strings, vectors, bytevectors all 100% conformant
- **`equal?` on circular structures** — terminates correctly via visited-set
- **Promises/lazy evaluation** — iterative forcing works without stack overflow
- **`parameterize` + continuations** — parameter correctly restored on re-entry (excluding converter bug C1)
- **Port architecture** — clean interface hierarchy, correct string port bypass of bufio

---

## Suggested Fix Priority

| Priority | Fix | Effort | Impact |
|----------|-----|--------|--------|
| 1 | C2: Character write names | Low (lookup table) | Fixes 9 test failures + round-trip |
| 2 | C3: BigFloat integer display | Low (String method) | Fixes write/read round-trip |
| 3 | C5: `io.ReadFull` for read-bytevector | Low (one-line) | Fixes bufio short reads |
| 4 | C1: Parameterize converter bypass | Medium | Fixes silent data corruption |
| 5 | H5: string->number complex/inf/nan | Medium | Parser extension |
| 6 | H2: sqrt exactness for perfect squares | Medium | Special-case detection |
| 7 | C4: Environment separation | Medium | Security + sandboxing |
| 8 | H3: Exact complex division | Medium | Rational arithmetic path |
| 9 | H4: Inexact zero division | Low | IEEE 754 compliance |
| 10 | M7: Vector patterns | Medium | One new bytecode + 5 type-switch additions (M8 not a bug) |
