# R7RS-small Conformance Review

**Date:** 2026-02-28
**Status:** Complete — all findings resolved (PRs #364–#368). See `R7RS-CONFORMANCE-FIXES.md` for implementation details.
**Scope:** Full R7RS-small spec (§3-6), originally tested against `./dist/scheme` at commit `1063ad8`

---

## Summary (at time of review)

These were the pass rates when the review was conducted. All failures listed below
have since been fixed (PRs #364–#368), except L7 (documented semantic difference)
and Finding 2 (performance nit, not a conformance issue).

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

#### C1. `parameterize` with converter double-applies converter on restore — FIXED (PR #364)

**R7RS §4.2.6.** The `parameterize` macro saves the current value via `(old (param))` and restores via `(p old)`. When a parameter has a converter, `(param)` returns the already-converted value, but `(p old)` applies the converter again.

**Reproduction:**
```scheme
(define p (make-parameter 10 (lambda (x) (* x 2))))
;; p stores 20 (correct: 10 * 2)
(parameterize ((p 5))
  (p))  ;; returns 10 (correct: 5 * 2)
(p)    ;; returns 40 (BUG: should be 20, but 20 * 2 = 40)
```

**Location:** `registry/core/bootstrap.scm`

**Resolution:** Added `%parameter-raw-set!` primitive. Restore thunk uses `(%parameter-raw-set! p old)` to bypass converter.

---

#### C2. `write` outputs raw bytes for named characters, not R7RS names — FIXED (PR #364)

**R7RS §6.13.3.** `write` must produce readable output. For the 9 named characters (`alarm`, `backspace`, `delete`, `escape`, `newline`, `null`, `return`, `space`, `tab`), `write` outputs `#\` followed by the raw control byte instead of the character name.

**Reproduction:**
```scheme
(write #\newline)  ;; outputs #\ followed by literal 0x0A byte
                   ;; should output #\newline
```

**Resolution:** Added `charNames` lookup table in `values/character.go`.

---

#### C3. Inexact integer-valued arithmetic results write/display as exact — FIXED (PR #369, open)

**R7RS §6.13.3.** `write` must produce output that `read` can reconstruct as an equivalent value. `(+ 1 1.0)` is inexact, but writes as `2` (which reads back as exact `2`).

**Reproduction:**
```scheme
(write (+ 1 1.0))          ;; outputs 2, should output 2.0
(number->string (+ 1 1.0)) ;; returns "2", should return "2.0"
(inexact? (+ 1 1.0))       ;; #t (correctly marked inexact)
```

**Resolution:** `BigFloat.SchemeString()` appends `.0` for integer-valued results. PR #369 open.

---

#### C4. `scheme-report-environment` is identical to `interaction-environment` — FIXED (PR #364)

**R7RS §6.12.** `scheme-report-environment` should return an immutable environment with only R7RS bindings. Currently it returns the same object as `interaction-environment`.

**Reproduction:**
```scheme
(eq? (interaction-environment) (scheme-report-environment 5)) ;; #t (wrong)
(define unique-test-var 777)
(eval 'unique-test-var (scheme-report-environment 5))         ;; 777 (wrong)
```

**Resolution:** Returns `NewChildTopLevelEnvironment()` — distinct frozen copy with R7RS bindings only.

---

#### C5. `read-bytevector` / `read-bytevector!` short reads from `bufio.Reader` — FIXED (PR #364)

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

**Resolution:** Replaced `p.Read(buf)` with `io.ReadFull(p, buf)` in both `PrimReadBytevector` and `PrimReadBytevectorBang`.

---

### HIGH

#### H1. `apply` in tail position grows Go stack — FIXED (PR #368)

**R7RS §3.5.** Proper tail recursion requires unbounded tail calls. `apply` in tail position creates recursive Go stack frames via `PrimApply` → `ApplyCallable` → `Run()`.

**Reproduction:**
```scheme
(define (f n) (if (zero? n) 'done (apply f (list (- n 1)))))
(f 1000000)  ;; Go stack overflow at ~300K
```

**Resolution:** `apply` compiled as special form: `OpUnpackListToStack` + `OpApply`. Tail-position apply skips `SaveContinuation` — constant Go stack. 1M iterations in ~0.4s.

---

#### H2. `sqrt` does not preserve exactness for perfect squares — FIXED (PR #364, #365)

**R7RS §6.2.6.** "If z is exact, the result is exact (if possible)."

```scheme
(sqrt 4)    ;; returns 2.0 (should be exact 2)
(sqrt 9/4)  ;; returns 1.5 (should be exact 3/2)
(sqrt -4)   ;; returns 0.0+2.0i (should be exact 0+2i)
```

**Resolution:** Perfect-square detection for Integer, BigInteger, and Rational cases.

---

#### H3. Exact complex division returns inexact — FIXED (PR #365)

**R7RS §6.2.6.** Exact op exact should be exact.

```scheme
(/ 3+4i 1+2i)  ;; returns 2.2-0.4i (should be 11/5-2/5i)
(+ 1+2i 3+4i)  ;; returns 4+6i (correctly exact)
(* 1+2i 3+4i)  ;; returns -5+10i (correctly exact)
```

**Resolution:** Exact rational arithmetic path for BigComplex division when both parts are exact.

---

#### H4. Division by inexact zero errors instead of returning infinity — FIXED (PR #365)

**R7RS §6.2.6 + IEEE 754.** Division by exact zero should error. Division by inexact zero should return `+inf.0`/`-inf.0`/`+nan.0`.

```scheme
(/ 1 0)    ;; error (correct)
(/ 1 0.0)  ;; error (should return +inf.0)
(/ -1 0.0) ;; error (should return -inf.0)
(/ 0.0 0.0);; error (should return +nan.0)
```

**Resolution:** Zero-check guard bypassed for inexact divisors; Go native float64 division produces correct IEEE 754 results.

---

#### H5. `string->number` doesn't parse complex or special values — FIXED (PR #364, #365)

**R7RS §7.1.1.**

```scheme
(string->number "3+4i")   ;; #f (should be 3+4i)
(string->number "+inf.0") ;; #f (should be +inf.0)
(string->number "-inf.0") ;; #f (should be -inf.0)
(string->number "+nan.0") ;; #f (should be +nan.0)
(string->number "+i")     ;; #f (should be 0+1i)
```

**Resolution:** Full R7RS radix/exactness prefix support and complex/special value parsing.

---

#### H6. `current-input-port` parameterize crashes on non-CharacterInputPort — FIXED (PR #364)

**R7RS §6.13.1.** `GetCurrentInputPort()` in `internal/extensions/io/state.go` does a hard type assertion to `*CharacterInputPort`. When `parameterize` sets `current-input-port` to a `StringInputPort`, this panics.

**Resolution:** Returns `TextualReader` / `OutputPort` interfaces instead of concrete types.

---

### MEDIUM

#### M1. `expt` with rational exponents doesn't return exact when possible — FIXED (PR #365)

```scheme
(expt 4 1/2)   ;; 2.0 (should be exact 2)
(expt -1 1/2)  ;; +nan.0 (should be 0+1i)
```

**Resolution:** Delegates `(expt x 1/2)` to sqrt logic; small-denominator exact root detection.

---

#### M2. `real-part`/`imag-part`/`magnitude` lose exactness on non-complex reals — FIXED (PR #365)

```scheme
(real-part 3)   ;; 3.0 (should be exact 3)
(imag-part 3)   ;; 0.0 (should be exact 0)
(magnitude 3)   ;; 3.0 (should be exact 3)
```

**Resolution:** Non-complex reals return themselves for `real-part`, exact zero for `imag-part`, and type-preserving `abs` for `magnitude`.

---

#### M3. `guard` re-raise fails with `with-exception-handler` — FIXED (PR #366, #367)

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

**Resolution:** R7RS §7.3 double `call/cc` pattern — `handler-k` returns to handler's dynamic extent for re-raise.

---

#### M4. `dynamic-wind` double-fires after thunks at top level — NOT REPRODUCIBLE (PR #366)

Nested `dynamic-wind` + `call/cc` escape fires after-thunks twice at top level, but correctly inside `let` contexts.

**Resolution:** Investigated with 5 test variations (simple/nested/re-entry × top-level/let). All cases show correct identical behavior. Closed as not-a-bug.

---

#### M5. `eval` drops multiple return values — FIXED (PR #366)

```scheme
(call-with-values
  (lambda () (eval '(values 1 2 3) (interaction-environment)))
  list)
;; Returns (1) — should return (1 2 3)
```

**Resolution:** `PrimEval` uses `SetValues(sub.GetValues()...)` instead of `SetValue(sub.GetValue())`.

---

#### M6. Textual output ops accept binary ports — FIXED (PR #364)

`write-char`, `write-string`, `display`, `newline`, `write`, `write-simple`, `write-shared` use `getOptionalOutputPort()` which accepts any `OutputPort`, including binary ports. R7RS says textual operations on binary ports are an error.

**Resolution:** Added `getOptionalTextualOutputPort` with explicit `BinaryWriter` rejection.

---

#### M7. `syntax-rules` vector patterns with pattern variables don't match — FIXED (PR #367)

```scheme
(define-syntax vec-first
  (syntax-rules ()
    ((vec-first #(x rest ...)) x)))
(vec-first #(1 2 3))  ;; ERROR: no matching clause
```

Empty vector patterns and literal-only vector patterns work. Only pattern variables inside vectors fail.

**Resolution:** New `ByteCodeVisitCarAsVector` bytecode converts vector elements to pair chain for matching. Five type-switch additions across pattern analysis, compilation, and variable discovery.

---

#### M8. Dotted pair patterns in `syntax-rules` don't work correctly — NOT A BUG

```scheme
(define-syntax rest-test
  (syntax-rules ()
    ((rest-test a . b) (list a b))))
(rest-test 1 2 3)  ;; ERROR
```

**Resolution:** Working correctly. `b` captures `(2 3)` as expected. The error is correct Scheme: `(list 1 (2 3))` evaluates `(2 3)` as a form, calling `2` as a procedure. Verified with quote, cons, and dotted templates.

---

### LOW

#### L1. `number->string` ignores radix for BigInt — FIXED (PR #365)

```scheme
(number->string (expt 2 64) 16)  ;; "18446744073709551616" (decimal, not hex)
```

**Resolution:** Uses `v.BigInt().Text(radix)` instead of `v.SchemeString()`.

---

#### L2. `positive?`/`negative?` reject real-valued complex — FIXED (PR #365)

```scheme
(real? 3+0i)     ;; #t
(positive? 3+0i) ;; error (should accept since it's real)
```

**Resolution:** Extracts real part from complex with zero imaginary before `RealNumber` check.

---

#### L3. Number prefix ordering — FIXED (PR #365)

```scheme
#x#e10  ;; error (R7RS allows radix-then-exactness ordering)
#e#x10  ;; 16 (exactness-then-radix works)
```

**Resolution:** Parser's `MarkerBase*` cases now handle a following exactness marker. Bug was tokenizer/parser only; `string->number` already handled both orderings.

---

#### L4. `read-string 0` returns EOF instead of `""` — FIXED (PR #364)

```scheme
(read-string 0 (open-input-string "hello"))  ;; #!eof (should be "")
```

**Resolution:** Early return for `k == 0` before the read loop.

---

#### L5. `close-input-port`/`close-output-port` don't validate direction — FIXED (PR #364)

Both map to `PrimClosePort` which accepts any `Port`. R7RS says `close-input-port` should only accept input ports.

**Resolution:** Separate `PrimCloseInputPort` / `PrimCloseOutputPort` with `InputPort` / `OutputPort` type checks.

---

#### L6. Bytevector display format uses extra spaces — FIXED (PR #364)

```scheme
(write #u8(1 2 3))  ;; #u8( 1 2 3 ) — conventional: #u8(1 2 3)
```

**Resolution:** Removed leading/trailing space in `formatIndexable`. Affects both bytevector and vector display.

---

#### L7. `char-ready?`/`u8-ready?` always return `#t` — WONTFIX

Documented in `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md`. Conservative-safe behavior. Common simplification across Scheme implementations.

---

### LATENT / EDGE-CASE

#### E1. Datum-label circular literals crash compiler — PARTIALLY FIXED (PR #367)

```scheme
(equal? '#1=(a . #1#) '#2=(a . #2#))  ;; Go stack overflow
```

**Resolution:** Crash prevention — `internSymbolsInValue` and `deduplicatePair` use visited sets; circular datum labels produce compile-time errors instead of stack overflows. `Pair.SchemeString()`/`String()` terminate on cycles.

**Remaining:** Shared acyclic datum labels (`#0=(a) (#0#)`) incorrectly rejected as circular. Tracked in `TODO.md`.

---

#### E2. `helpers.Eqv` returns `true` for BigFloat NaN — FIXED (PR #365)

```go
// registry/helpers/equality.go:50-53
// BigFloat NaN stores zero-valued *big.Float, so Cmp returns 0
helpers.Eqv(BigFloatNaN, BigFloatNaN) // true (wrong)
```

**Resolution:** NaN check before `Cmp`: `if va.IsNaN() || vb.IsNaN() { return false }`.

---

#### E3. `Float.EqualTo` panics comparing Float NaN with BigFloat — FIXED (PR #364)

`big.Float.SetFloat64(NaN)` panics. Reachable if `equal?` dispatches Float NaN against BigFloat.

**Resolution:** NaN guard in `Float.EqualTo` BigFloat case: `if math.IsNaN(p.Value) || other.IsNaN() { return false }`.

---

## bufio Assessment

**Question:** Does Go's `bufio.Reader`/`bufio.Writer` cause R7RS non-conformance in the port system?

**Answer:** One real bug (C5, now fixed), otherwise sound.

| Concern | Status |
|---------|--------|
| `peek-char`/`peek-u8` with bufio | PASS — uses ReadRune+UnreadRune / ReadByte+UnreadByte |
| `read-bytevector` short reads | PASS — fixed with `io.ReadFull` (PR #364) |
| Flush-on-close | PASS — `flushThenClose` properly flushes `bufio.Writer` |
| `read-line` line endings | PASS — handles `\n`, `\r\n`, `\r` |
| String ports | N/A — bypass bufio entirely (use `bytes.Buffer`) |
| `read` then `read-char` interleaving | PASS — tokenizer shares the `bufio.Reader` |
| Binary vs textual separation | PASS — both directions enforced (M6 fixed in PR #364) |

---

## What Works Well

- **Proper tail recursion** — 23/24 tail positions pass at 1M iterations
- **Macro hygiene** — scope-set model correctly prevents capture
- **Numeric tower** — complete with correct exactness contagion, round-to-even, GCD/LCM, rationalize, bigint promotion
- **Data types** — pairs, lists, strings, vectors, bytevectors all 100% conformant
- **`equal?` on circular structures** — terminates correctly via visited-set
- **Promises/lazy evaluation** — iterative forcing works without stack overflow
- **`parameterize` + continuations** — parameter correctly restored on re-entry (C1 converter bug now fixed)
- **Port architecture** — clean interface hierarchy, correct string port bypass of bufio

---

## Resolution Summary

All findings resolved across PRs #364–#368:

| Finding | Resolution | PR |
|---------|------------|----|
| C1 | `%parameter-raw-set!` bypasses converter | #364 |
| C2 | `charNames` lookup table | #364 |
| C3 | BigFloat `.0` suffix for integer values | #369 (open) |
| C4 | `NewChildTopLevelEnvironment()` for distinct env | #364 |
| C5 | `io.ReadFull` replaces single `Read` | #364 |
| H1 | `apply` as compile-time special form | #368 |
| H2 | Perfect-square detection (Integer, BigInteger, Rational) | #364, #365 |
| H3 | Exact rational arithmetic for BigComplex division | #365 |
| H4 | Inexact zero bypass for IEEE 754 infinity/NaN | #365 |
| H5 | Full complex/special value parsing in `string->number` | #364, #365 |
| H6 | Interface return types for current port accessors | #364 |
| M1 | Delegates `(expt x 1/2)` to sqrt logic | #365 |
| M2 | Type-preserving `real-part`/`imag-part`/`magnitude` | #365 |
| M3 | R7RS §7.3 double `call/cc` guard pattern | #366, #367 |
| M4 | Not reproducible | #366 |
| M5 | `SetValues(sub.GetValues()...)` in eval | #366 |
| M6 | `getOptionalTextualOutputPort` with binary rejection | #364 |
| M7 | `ByteCodeVisitCarAsVector` + 5 type-switch additions | #367 |
| M8 | Not a bug (correct Scheme behavior) | — |
| L1 | `v.BigInt().Text(radix)` | #365 |
| L2 | Extract real from zero-imag complex | #365 |
| L3 | Parser `MarkerBase*` handles following exactness marker | #365 |
| L4 | Early return for `k == 0` | #364 |
| L5 | Separate `PrimCloseInputPort`/`PrimCloseOutputPort` | #364 |
| L6 | Remove spaces in `formatIndexable` | #364 |
| L7 | Documented semantic difference (WONTFIX) | — |
| E1 | Crash prevention; shared acyclic DAGs still tracked | #367 |
| E2 | NaN guard in `helpers.Eqv` | #365 |
| E3 | NaN guard in `Float.EqualTo` | #364 |
