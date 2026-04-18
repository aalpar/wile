(wile algebra polynomial) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.
>
> **Commit discipline (project-specific):** Wile's CLAUDE.md says "NEVER commit changes without asking first." Every task's commit step is **propose to user, await approval** — never auto-commit. The user structures commits themselves.

**Goal:** Add a first-class polynomial type and algebra library `(wile algebra polynomial)` with ring-parameterized arithmetic, Horner evaluation, formal derivative, and field-required division/GCD.

**Architecture:** Univariate polynomial over any coefficient ring. Record type `<polynomial>` wraps `(ring, coeffs)` where `coeffs` is a list in **ascending** power order, normalized (no trailing zero). Uses existing `make-ring`/`make-field` abstractions from `(wile algebra ring)` for coefficient arithmetic. Capstone `polynomial-ring` constructor packages the library *as* a ring, enabling recursive polynomial rings (i.e., multivariate via `R[x][y]`).

**Tech Stack:** R7RS Scheme, `define-record-type`, `syntax-rules`, chibi-test. No Go changes.

**Representation:**
- `coeffs = '(a₀ a₁ a₂ ...)` represents `a₀ + a₁x + a₂x² + ...`
- Zero polynomial: `coeffs = '()`
- Invariant (enforced by `make-poly`): if `coeffs ≠ '()`, then `(last coeffs) ≠ (ring-zero R)`
- Degree convention: `poly-degree` returns `-1` for the zero polynomial (documented; PARI/GP convention)

**Prior art to extend, not duplicate:** `differential.scm:75-142` has working internal helpers (`poly+`, `poly*`, `poly-neg`, `poly-scale`, `poly-shift`, `normalize`, `poly-deriv`). Task 11 replaces them with the new library.

**Test invocation convention:** `run-all.sh` auto-discovers all `*-test.scm` files — it does **not** accept per-file arguments. To run a single file during development, use:

```bash
WILE=./dist/$(go env GOOS)/$(go env GOARCH)/wile
$WILE --quiet -f test/wile/algebra-polynomial-test.scm
```

To run the full suite: `make test-scheme`. References to "run single file" below use the `$WILE --quiet -f <path>` form.

**Scope — in:** Items 1–7 from the original spec (construct, arithmetic, eval, degree, leading coeff, divmod, gcd, derivative).

**Scope — out:** Item 8 (`poly-factor`, `poly-roots`). Factoring over GF(p) needs Cantor–Zassenhaus or Berlekamp; root-finding over ℚ needs rational-root theorem + descent. These are separate work and earn their own plan.

---

## File Structure

**Create:**
- `stdlib/lib/wile/algebra/polynomial.sld` — library definition with exports
- `stdlib/lib/wile/algebra/polynomial.scm` — implementation
- `test/wile/algebra-polynomial-test.scm` — chibi-test suite

**Modify:**
- `stdlib/lib/wile/algebra/differential.scm:75-142` — refactor `polynomial-derivation` to use new library
- `stdlib/lib/wile/algebra/differential.sld` — add `(wile algebra polynomial)` import
- `stdlib/lib/wile/algebra.sld` — export new polynomial symbols, import new library

---

## Task 1: Library skeleton, record type, constructor, predicate

**Files:**
- Create: `stdlib/lib/wile/algebra/polynomial.sld`
- Create: `stdlib/lib/wile/algebra/polynomial.scm`
- Create: `test/wile/algebra-polynomial-test.scm`

- [ ] **Step 1: Write the failing test file**

Create `test/wile/algebra-polynomial-test.scm`:

```scheme
;;; algebra-polynomial-test.scm — Polynomial library tests

(import (scheme base)
        (chibi test)
        (wile algebra ring)
        (wile algebra polynomial))

(test-begin "polynomials")

;; -- Construction & predicate --

(test-group "make-poly and predicate"
  (let ((R (integer-ring)))
    (test #t (polynomial? (make-poly R '(1 2 3))))
    (test #f (polynomial? '(1 2 3)))
    (test #f (polynomial? 42))
    ;; Zero polynomial
    (test #t (polynomial? (make-poly R '())))))

(test-group "normalization strips trailing zeros"
  (let ((R (integer-ring)))
    ;; 1 + 2x + 0 + 0 → 1 + 2x
    (test '(1 2) (poly-coeffs (make-poly R '(1 2 0 0))))
    ;; All zeros → empty (zero poly)
    (test '() (poly-coeffs (make-poly R '(0 0 0))))
    ;; Already normalized
    (test '(1 2 3) (poly-coeffs (make-poly R '(1 2 3))))
    ;; Empty stays empty
    (test '() (poly-coeffs (make-poly R '())))))

(test-group "poly-ring accessor"
  (let* ((R (integer-ring))
         (p (make-poly R '(1 2 3))))
    (test #t (ring? (poly-ring p)))))

(test-end)
(test-exit)
```

- [ ] **Step 2: Create library skeleton `polynomial.sld`**

```scheme
(define-library (wile algebra polynomial)
  (description "Univariate polynomials over a coefficient ring. Ascending-order coefficient lists, normalized (no trailing zero). Supports arithmetic, Horner evaluation, formal derivative, and (field-required) Euclidean divmod/gcd.")
  (export make-poly polynomial?
          poly-ring poly-coeffs)
  (import (scheme base)
          (wile algebra ring))
  (include "polynomial.scm"))
```

- [ ] **Step 3: Create `polynomial.scm` with record type and normalizing constructor**

```scheme
;;; (wile algebra polynomial) — Univariate polynomials over a ring
;;;
;;; Representation: coeffs is a list in ascending power order.
;;;   '(a0 a1 a2 ...) represents a0 + a1*x + a2*x^2 + ...
;;; Zero polynomial: coeffs = '().
;;; Invariant (enforced by make-poly): if coeffs is non-empty,
;;; its last element is not (ring-zero R).

(define-record-type <polynomial>
  (make-poly* ring coeffs)
  polynomial?
  (ring   poly-ring)
  (coeffs poly-coeffs))

;; Strip trailing zeros from the ascending coefficient list.
;; Internal — exposed downstream only through make-poly.
(define (poly-normalize R coeffs)
  (let loop ((cs (reverse coeffs)))
    (cond
      ((null? cs) '())
      ((equal? (car cs) (ring-zero R)) (loop (cdr cs)))
      (else (reverse cs)))))

(define (make-poly R coeffs)
  "Construct a polynomial over ring R from coefficient list COEFFS.\nCOEFFS is in ascending power order: (a0 a1 a2 ...) represents\na0 + a1*x + a2*x^2 + .... Trailing zero coefficients are stripped\nto enforce the normal-form invariant. The empty list represents\nthe zero polynomial.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '(1 2 3))))     => (1 2 3)\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '(1 2 0 0))))   => (1 2)\n  (let ((R (integer-ring)))\n    (poly-coeffs (make-poly R '())))          => ()\n\nParameters:\n  R : any\n  coeffs : list\nReturns: any\nCategory: algebra\nKeywords: polynomial, univariate, constructor, coefficient list, normal form\n\nSee also: `polynomial?', `poly-ring', `poly-coeffs'."
  (make-poly* R (poly-normalize R coeffs)))
```

- [ ] **Step 4: Run tests to verify they pass**

Run: `cd /Users/aalpar/projects/wile-workspace/wile && make build && $WILE --quiet -f test/wile/algebra-polynomial-test.scm` (with `WILE` defined as in the header)
Expected: all "polynomials" tests pass.

- [ ] **Step 5: Propose commit to user — do not commit automatically**

Propose message: `feat(algebra): add (wile algebra polynomial) library skeleton with record type`

---

## Task 2: Constants and shape accessors

**Files:**
- Modify: `stdlib/lib/wile/algebra/polynomial.sld` (add exports)
- Modify: `stdlib/lib/wile/algebra/polynomial.scm`
- Modify: `test/wile/algebra-polynomial-test.scm` (add tests)

- [ ] **Step 1: Add failing tests**

Append to `test/wile/algebra-polynomial-test.scm` before `(test-end)`:

```scheme
(test-group "poly-zero and poly-one"
  (let ((R (integer-ring)))
    (test '()  (poly-coeffs (poly-zero R)))
    (test '(1) (poly-coeffs (poly-one R)))))

(test-group "poly-degree"
  (let ((R (integer-ring)))
    (test -1 (poly-degree (make-poly R '())))        ; zero poly: -1 by convention
    (test 0  (poly-degree (make-poly R '(5))))       ; constant
    (test 2  (poly-degree (make-poly R '(1 2 3))))   ; 1 + 2x + 3x^2
    (test 1  (poly-degree (make-poly R '(1 2 0)))))) ; normalized to (1 2)

(test-group "poly-leading-coeff"
  (let ((R (integer-ring)))
    (test 0 (poly-leading-coeff (make-poly R '())))   ; zero poly → ring-zero
    (test 5 (poly-leading-coeff (make-poly R '(5))))
    (test 3 (poly-leading-coeff (make-poly R '(1 2 3))))))
```

- [ ] **Step 2: Verify tests fail**

Run: `$WILE --quiet -f test/wile/algebra-polynomial-test.scm`
Expected: FAIL with undefined identifiers.

- [ ] **Step 3: Extend `polynomial.sld` exports**

Replace export list in `polynomial.sld` with:

```scheme
  (export make-poly polynomial?
          poly-ring poly-coeffs
          poly-zero poly-one
          poly-degree poly-leading-coeff)
```

- [ ] **Step 4: Implement in `polynomial.scm`**

Append to `polynomial.scm`:

```scheme
;; ─── Constants ───────────────────────────────

(define (poly-zero R)
  "Return the zero polynomial over ring R.\nRepresented internally by an empty coefficient list.\n\nExamples:\n  (poly-coeffs (poly-zero (integer-ring)))  => ()\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: zero polynomial, additive identity, empty polynomial\n\nSee also: `poly-one', `make-poly'."
  (make-poly* R '()))

(define (poly-one R)
  "Return the unit polynomial 1 over ring R.\n\nExamples:\n  (poly-coeffs (poly-one (integer-ring)))  => (1)\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: unit polynomial, multiplicative identity, constant one\n\nSee also: `poly-zero', `make-poly'."
  (make-poly* R (list (ring-one R))))

;; ─── Shape accessors ────────────────────────

(define (poly-degree p)
  "Return the degree of polynomial P.\nBy convention, the zero polynomial has degree -1 (PARI/GP\nconvention); this lets callers test for zero via negative\ndegree without special-casing.\n\nExamples:\n  (poly-degree (make-poly (integer-ring) '()))       => -1\n  (poly-degree (make-poly (integer-ring) '(5)))      => 0\n  (poly-degree (make-poly (integer-ring) '(1 2 3)))  => 2\n\nParameters:\n  p : any\nReturns: integer\nCategory: algebra\nKeywords: degree, polynomial degree, order, rank\n\nSee also: `poly-leading-coeff'."
  (let ((cs (poly-coeffs p)))
    (if (null? cs)
        -1
        (- (length cs) 1))))

(define (poly-leading-coeff p)
  "Return the leading (highest-power) coefficient of P.\nFor the zero polynomial, returns the ring's zero element.\n\nExamples:\n  (poly-leading-coeff (make-poly (integer-ring) '(1 2 3)))  => 3\n  (poly-leading-coeff (make-poly (integer-ring) '()))       => 0\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: leading coefficient, highest coefficient, top coefficient\n\nSee also: `poly-degree', `poly-coeffs'."
  (let ((cs (poly-coeffs p)))
    (if (null? cs)
        (ring-zero (poly-ring p))
        (let loop ((xs cs))
          (if (null? (cdr xs)) (car xs) (loop (cdr xs)))))))
```

- [ ] **Step 5: Verify tests pass**

Run: `$WILE --quiet -f test/wile/algebra-polynomial-test.scm`
Expected: all groups pass.

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-zero, poly-one, degree, leading-coeff`

---

## Task 3: Additive structure — plus, negate, minus

**Files:**
- Modify: `stdlib/lib/wile/algebra/polynomial.sld` (add exports)
- Modify: `stdlib/lib/wile/algebra/polynomial.scm`
- Modify: `test/wile/algebra-polynomial-test.scm`

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-plus"
  (let ((R (integer-ring)))
    ;; (1 + 2x) + (3 + 4x + 5x^2) = 4 + 6x + 5x^2
    (test '(4 6 5) (poly-coeffs (poly-plus (make-poly R '(1 2))
                                           (make-poly R '(3 4 5)))))
    ;; Zero identity
    (test '(1 2 3) (poly-coeffs (poly-plus (poly-zero R)
                                           (make-poly R '(1 2 3)))))
    ;; Cancellation normalizes: (1 + x) + (-1 - x) = 0
    (test '() (poly-coeffs (poly-plus (make-poly R '(1 1))
                                      (make-poly R '(-1 -1)))))))

(test-group "poly-negate"
  (let ((R (integer-ring)))
    (test '(-1 -2 -3) (poly-coeffs (poly-negate (make-poly R '(1 2 3)))))
    (test '() (poly-coeffs (poly-negate (poly-zero R))))))

(test-group "poly-minus"
  (let ((R (integer-ring)))
    ;; (3 + 4x) - (1 + 2x) = 2 + 2x
    (test '(2 2) (poly-coeffs (poly-minus (make-poly R '(3 4))
                                          (make-poly R '(1 2)))))))
```

- [ ] **Step 2: Verify tests fail**

Run: `$WILE --quiet -f test/wile/algebra-polynomial-test.scm`
Expected: FAIL with undefined identifiers.

- [ ] **Step 3: Update exports in `polynomial.sld`**

Add `poly-plus poly-negate poly-minus` to export list.

- [ ] **Step 4: Implement in `polynomial.scm`**

```scheme
;; ─── Additive structure ─────────────────────

(define (poly-plus p q)
  "Add polynomials P and Q. Both must share the same coefficient ring.\nResult is normalized (trailing zeros stripped after coefficient-wise add).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-plus (make-poly R '(1 2)) (make-poly R '(3 4 5)))))\n  => (4 6 5)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial addition, add, sum, plus\n\nSee also: `poly-minus', `poly-negate'."
  (let ((R (poly-ring p)))
    (make-poly R
      (let loop ((xs (poly-coeffs p)) (ys (poly-coeffs q)))
        (cond
          ((null? xs) ys)
          ((null? ys) xs)
          (else
            (cons (ring-plus R (car xs) (car ys))
                  (loop (cdr xs) (cdr ys)))))))))

(define (poly-negate p)
  "Return the additive inverse of polynomial P.\nNegates every coefficient under the coefficient ring's negation.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-negate (make-poly R '(1 2 3)))))  => (-1 -2 -3)\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: polynomial negation, additive inverse, unary minus\n\nSee also: `poly-plus', `poly-minus'."
  (let ((R (poly-ring p)))
    (make-poly* R
      (map (lambda (c) (ring-negate R c)) (poly-coeffs p)))))

(define (poly-minus p q)
  "Subtract polynomial Q from P. Computed as P plus negation of Q.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-minus (make-poly R '(3 4)) (make-poly R '(1 2)))))\n  => (2 2)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial subtraction, subtract, difference, minus\n\nSee also: `poly-plus', `poly-negate'."
  (poly-plus p (poly-negate q)))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add additive structure (plus, negate, minus)`

---

## Task 4: Multiplication (naive O(n·m))

**Files:** same as Task 3 plus test additions.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-times"
  (let ((R (integer-ring)))
    ;; (1 + x)(1 + x) = 1 + 2x + x^2
    (test '(1 2 1) (poly-coeffs (poly-times (make-poly R '(1 1))
                                            (make-poly R '(1 1)))))
    ;; (1 - x)(1 + x) = 1 - x^2
    (test '(1 0 -1) (poly-coeffs (poly-times (make-poly R '(1 -1))
                                             (make-poly R '(1 1)))))
    ;; Zero annihilates
    (test '() (poly-coeffs (poly-times (poly-zero R) (make-poly R '(1 2 3)))))
    ;; One is identity
    (test '(1 2 3) (poly-coeffs (poly-times (poly-one R) (make-poly R '(1 2 3)))))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `poly-times`**

- [ ] **Step 4: Implement**

Append to `polynomial.scm`:

```scheme
;; ─── Multiplication ─────────────────────────
;;
;; Naive O(n·m) schoolbook multiplication. Karatsuba/FFT would lower
;; this to O(n^1.58) / O(n log n) but are not warranted until a real
;; benchmark justifies the added complexity.

(define (poly-times p q)
  "Multiply polynomials P and Q. Both must share the same coefficient ring.\nComputed via schoolbook multiplication in O(n*m) coefficient operations.\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-times (make-poly R '(1 1)) (make-poly R '(1 1)))))\n  => (1 2 1)\n\nParameters:\n  p : any\n  q : any\nReturns: any\nCategory: algebra\nKeywords: polynomial multiplication, multiply, product, times, convolution\n\nSee also: `poly-plus', `poly-eval'."
  (let ((R  (poly-ring p))
        (xs (poly-coeffs p))
        (ys (poly-coeffs q)))
    (cond
      ((null? xs) (poly-zero R))
      ((null? ys) (poly-zero R))
      (else
        (let ((rz (ring-zero R))
              (n  (+ (length xs) (length ys) -1)))
          ;; Accumulate into a vector of length n, then convert.
          (let ((acc (make-vector n rz)))
            (let loop-i ((i 0) (xs xs))
              (if (null? xs)
                  (make-poly R (vector->list acc))
                  (begin
                    (let loop-j ((j 0) (ys ys))
                      (if (null? ys)
                          (if #f #f)  ; unspecified
                          (let ((k (+ i j)))
                            (vector-set! acc k
                              (ring-plus R (vector-ref acc k)
                                           (ring-times R (car xs) (car ys))))
                            (loop-j (+ j 1) (cdr ys)))))
                    (loop-i (+ i 1) (cdr xs)))))))))))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-times (schoolbook)`

---

## Task 5: Evaluation via Horner's method

**Files:** same pattern.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-eval (Horner)"
  (let ((R (integer-ring)))
    ;; p(x) = 1 + 2x + 3x^2; p(0) = 1, p(1) = 6, p(2) = 17
    (let ((p (make-poly R '(1 2 3))))
      (test 1  (poly-eval p 0))
      (test 6  (poly-eval p 1))
      (test 17 (poly-eval p 2)))
    ;; Zero polynomial evaluates to ring-zero
    (test 0 (poly-eval (poly-zero R) 42))
    ;; Constant polynomial evaluates to its constant
    (test 5 (poly-eval (make-poly R '(5)) 99))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `poly-eval`**

- [ ] **Step 4: Implement**

```scheme
;; ─── Evaluation (Horner's method) ──────────
;;
;; For coeffs in ascending order (a0 a1 ... an), Horner's scheme
;; computes p(x) = a0 + x(a1 + x(a2 + ... + x*an)) using n
;; multiplications and n additions — O(n) rather than the O(n^2)
;; of naive power-accumulation.

(define (poly-eval p x)
  "Evaluate polynomial P at point X via Horner's method.\nUses n multiplications and n additions under the coefficient\nring for a degree-n polynomial — O(n) rather than the O(n^2)\nof naive power accumulation. The point X must belong to the\ncoefficient ring (or a ring extension where ring-plus and\nring-times remain meaningful).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-eval (make-poly R '(1 2 3)) 2))  => 17\n  ; because 1 + 2*2 + 3*4 = 17\n\nParameters:\n  p : any\n  x : any\nReturns: any\nCategory: algebra\nKeywords: polynomial evaluation, Horner's method, Horner scheme, synthetic substitution\n\nSee also: `poly-plus', `poly-times'."
  (let ((R (poly-ring p)))
    (let loop ((cs (reverse (poly-coeffs p))) (acc (ring-zero R)))
      (if (null? cs)
          acc
          (loop (cdr cs) (ring-plus R (car cs) (ring-times R x acc)))))))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-eval via Horner's method`

---

## Task 6: Formal derivative

**Files:** same pattern.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-derivative"
  (let ((R (integer-ring)))
    ;; D(1 + 2x + 3x^2) = 2 + 6x
    (test '(2 6) (poly-coeffs (poly-derivative (make-poly R '(1 2 3)))))
    ;; D(constant) = 0
    (test '() (poly-coeffs (poly-derivative (make-poly R '(42)))))
    ;; D(0) = 0
    (test '() (poly-coeffs (poly-derivative (poly-zero R))))
    ;; D(x) = 1
    (test '(1) (poly-coeffs (poly-derivative (make-poly R '(0 1)))))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `poly-derivative`**

- [ ] **Step 4: Implement**

```scheme
;; ─── Formal derivative ──────────────────────
;;
;; The formal derivative is purely symbolic — no limits, no analysis.
;; For coeffs (a0 a1 a2 ... an) in ascending order, the derivative is
;; (a1 2*a2 3*a3 ... n*an). In rings without natural integer embedding
;; (e.g., GF(p)), the "k" factor is built by repeated addition of ring-one.

(define (poly-derivative p)
  "Return the formal derivative of polynomial P.\nPurely symbolic: for (a0 a1 a2 ... an), produces\n(a1 2*a2 3*a3 ... n*an). The integer multiplier k is realized\ninside the coefficient ring by summing ring-one k times, so this\nworks correctly over rings of positive characteristic (e.g., GF(p)).\n\nExamples:\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-derivative (make-poly R '(1 2 3)))))  => (2 6)\n  (let ((R (integer-ring)))\n    (poly-coeffs (poly-derivative (make-poly R '(42)))))     => ()\n\nParameters:\n  p : any\nReturns: any\nCategory: algebra\nKeywords: formal derivative, symbolic derivative, differentiation, polynomial derivative\n\nSee also: `poly-plus', `poly-eval'."
  (let ((R (poly-ring p)))
    (let ((cs (poly-coeffs p)))
      (if (or (null? cs) (null? (cdr cs)))
          (poly-zero R)
          (let ((rz (ring-zero R))
                (ro (ring-one R)))
            ;; Build ring-element representing natural number k.
            (define (ring-nat k)
              (let loop ((i 0) (acc rz))
                (if (>= i k)
                    acc
                    (loop (+ i 1) (ring-plus R acc ro)))))
            (make-poly R
              (let loop ((xs (cdr cs)) (k 1))
                (if (null? xs)
                    '()
                    (cons (ring-times R (ring-nat k) (car xs))
                          (loop (cdr xs) (+ k 1)))))))))))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-derivative (formal)`

---

## Task 7: Division with remainder (field required)

**Files:** same pattern. This is the first operation that requires a **field** rather than a ring — we need to invert the divisor's leading coefficient to eliminate the leading term on each step of long division.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-divmod (rational field)"
  (let ((F (rational-field)))
    ;; (x^2 - 1) / (x - 1) = (x + 1), remainder 0
    (let-values (((q r) (poly-divmod (make-poly F '(-1 0 1))
                                     (make-poly F '(-1 1))
                                     F)))
      (test '(1 1) (poly-coeffs q))
      (test '()    (poly-coeffs r)))
    ;; (x^2 + 1) / (x) = (x), remainder 1
    (let-values (((q r) (poly-divmod (make-poly F '(1 0 1))
                                     (make-poly F '(0 1))
                                     F)))
      (test '(0 1) (poly-coeffs q))
      (test '(1)   (poly-coeffs r)))
    ;; Dividing smaller by larger: quotient 0, remainder is dividend
    (let-values (((q r) (poly-divmod (make-poly F '(1 2))
                                     (make-poly F '(1 2 3))
                                     F)))
      (test '()    (poly-coeffs q))
      (test '(1 2) (poly-coeffs r)))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Update `polynomial.sld`**

Add `(wile algebra ring)` is already imported. `poly-divmod` needs field access — design decision: **pass the field explicitly** as the third argument so the polynomial record only carries the ring. This keeps the record type minimal and lets callers divide in a field even when the polynomial's declared ring is weaker.

Export `poly-divmod`.

- [ ] **Step 4: Implement**

```scheme
;; ─── Division with remainder ───────────────
;;
;; poly-divmod requires the coefficient structure to be a FIELD (not
;; merely a ring) because long division must divide each leading term
;; of the remainder by the divisor's leading coefficient. The divisor
;; must be non-zero. Returns (values quotient remainder) satisfying
;;   p = q*d + r,  (poly-degree r) < (poly-degree d).

(define (poly-divmod p d F)
  "Divide polynomial P by non-zero polynomial D over field F.\nReturns two values: the quotient Q and remainder R satisfying\nP = Q*D + R with (poly-degree R) < (poly-degree D).\nRequires F to be a field (for reciprocal of D's leading coefficient).\nThe polynomial records P and D carry their own coefficient ring,\nbut F provides the field structure needed for division; typically\nF is the field whose underlying ring matches (poly-ring P).\n\nExamples:\n  (let ((F (rational-field)))\n    (call-with-values\n      (lambda () (poly-divmod (make-poly F '(-1 0 1)) (make-poly F '(-1 1)) F))\n      (lambda (q r) (list (poly-coeffs q) (poly-coeffs r)))))\n  => ((1 1) ())   ; (x^2 - 1) / (x - 1) = x + 1\n\nParameters:\n  p : any\n  d : any\n  F : any\nReturns: (values any any)\nCategory: algebra\nKeywords: polynomial division, long division, divmod, quotient, remainder, Euclidean division\n\nSee also: `poly-gcd', `field-reciprocal'."
  (let ((R (poly-ring p)))
    (when (null? (poly-coeffs d))
      (error "poly-divmod: division by zero polynomial"))
    (let ((lead-d-inv (field-reciprocal F (poly-leading-coeff d)))
          (deg-d      (poly-degree d)))
      (let loop ((rem p)
                 (q-coeffs-rev '()))
        (let ((deg-r (poly-degree rem)))
          (if (< deg-r deg-d)
              (values (make-poly R (reverse-pad q-coeffs-rev R (+ deg-r 1)))
                      rem)
              (let* ((lead-r  (poly-leading-coeff rem))
                     (coeff   (ring-times R lead-r lead-d-inv))
                     (shift   (- deg-r deg-d))
                     ;; term = coeff * x^shift, as a polynomial
                     (term    (make-poly R
                                (append (make-list shift (ring-zero R))
                                        (list coeff))))
                     (rem*    (poly-minus rem (poly-times term d))))
                (loop rem* (cons (cons shift coeff) q-coeffs-rev)))))))))

;; Internal helper: given a reversed list of (shift . coeff) entries
;; produced by long division, materialize the quotient's ascending
;; coefficient list of length N, filling unassigned slots with ring-zero.
(define (reverse-pad entries R n)
  (let ((acc (make-vector n (ring-zero R))))
    (for-each
      (lambda (e) (vector-set! acc (car e) (cdr e)))
      entries)
    (vector->list acc)))
```

**Note:** `make-list` is R7RS. `call-with-values` is R7RS. `let-values` is R7RS (via `(scheme base)`).

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-divmod (field-required long division)`

---

## Task 8: GCD via Euclidean algorithm (field required)

**Files:** same pattern.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "poly-gcd (rational field)"
  (let ((F (rational-field)))
    ;; gcd(x^2 - 1, x - 1) = x - 1 (up to unit)
    ;; After monic-normalization: (x - 1), coeffs = (-1 1)
    (test '(-1 1) (poly-coeffs (poly-gcd (make-poly F '(-1 0 1))
                                         (make-poly F '(-1 1))
                                         F)))
    ;; gcd(x^2 - 1, x^2 - 2x + 1) = x - 1
    (test '(-1 1) (poly-coeffs (poly-gcd (make-poly F '(-1 0 1))
                                         (make-poly F '(1 -2 1))
                                         F)))
    ;; gcd with zero: gcd(p, 0) = p (monic-normalized)
    (test '(1 1) (poly-coeffs (poly-gcd (make-poly F '(2 2))  ; 2(1+x)
                                        (poly-zero F)
                                        F)))))
```

**Normalization choice:** `poly-gcd` returns a **monic** result (leading coefficient = 1). Without this, GCD is defined only up to a unit (any non-zero scalar), and two equally-valid implementations could return different answers. Monic normalization pins it down.

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `poly-gcd`**

- [ ] **Step 4: Implement**

```scheme
;; ─── GCD (Euclidean algorithm) ──────────────
;;
;; The Euclidean algorithm terminates because poly-divmod strictly
;; decreases the remainder's degree. We normalize the final result
;; to be monic so GCD is uniquely determined (otherwise it is defined
;; only up to multiplication by a unit).

(define (poly-monic p F)
  (let ((cs (poly-coeffs p)))
    (if (null? cs)
        p
        (let* ((R        (poly-ring p))
               (lead-inv (field-reciprocal F (poly-leading-coeff p))))
          (make-poly R (map (lambda (c) (ring-times R c lead-inv)) cs))))))

(define (poly-gcd p q F)
  "Return the monic greatest common divisor of polynomials P and Q over field F.\nComputed by the Euclidean algorithm: repeatedly replace (p, q) with\n(q, p mod q) until the second argument is zero. The final non-zero\nremainder is normalized to monic form (leading coefficient = 1) so\nthe result is unique rather than defined up to a unit.\n\nExamples:\n  (let ((F (rational-field)))\n    (poly-coeffs (poly-gcd (make-poly F '(-1 0 1))\n                           (make-poly F '(-1 1)) F)))\n  => (-1 1)   ; x - 1\n\nParameters:\n  p : any\n  q : any\n  F : any\nReturns: any\nCategory: algebra\nKeywords: polynomial GCD, greatest common divisor, Euclidean algorithm, monic\n\nSee also: `poly-divmod'."
  (let loop ((a p) (b q))
    (if (null? (poly-coeffs b))
        (poly-monic a F)
        (let-values (((_q r) (poly-divmod a b F)))
          (loop b r)))))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add poly-gcd via Euclidean algorithm`

---

## Task 9: Capstone — `polynomial-ring` constructor

**Files:** same pattern. This packages the polynomial library *as* a ring, so polynomials can themselves be coefficients (enabling recursive/multivariate use).

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "polynomial-ring constructor"
  (let* ((R   (integer-ring))
         (PR  (polynomial-ring R))
         (p   (make-poly R '(1 2)))    ; 1 + 2x
         (q   (make-poly R '(3 4))))   ; 3 + 4x
    (test #t (ring? PR))
    ;; PR's operations should agree with poly-plus/poly-times
    (test '(4 6)   (poly-coeffs (ring-plus PR p q)))
    (test '(3 10 8) (poly-coeffs (ring-times PR p q)))
    (test '() (poly-coeffs (ring-zero PR)))
    (test '(1) (poly-coeffs (ring-one PR)))
    (test '(-1 -2) (poly-coeffs (ring-negate PR p)))))

(test-group "polynomial-ring enables recursion (bivariate via R[x][y])"
  (let* ((R   (integer-ring))
         (Rx  (polynomial-ring R))           ; Z[x]
         (Rxy (polynomial-ring Rx)))         ; Z[x][y]
    (test #t (ring? Rxy))
    ;; Element of Z[x][y] has poly-over-R coefficients
    (let ((x (make-poly R '(0 1)))            ; x
          (y-plus-x (make-poly Rx
                      (list (make-poly R '(0 1))  ; x
                            (make-poly R '(1))))))  ; y
      (test #t (polynomial? y-plus-x)))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `polynomial-ring`**

- [ ] **Step 4: Implement**

```scheme
;; ─── Capstone: polynomial-ring as a ring ────
;;
;; polynomial-ring R packages the polynomial library as a ring whose
;; elements are <polynomial> records over R. This is what enables
;; recursive constructions: (polynomial-ring (polynomial-ring R))
;; is R[x][y], i.e., bivariate polynomials.

(define (polynomial-ring R)
  "Construct the ring of polynomials over coefficient ring R.\nElements of the resulting ring are <polynomial> records whose\ncoefficients live in R. Since the result is itself a ring,\niteration gives multivariate polynomial rings:\n(polynomial-ring (polynomial-ring R)) is R[x][y].\n\nExamples:\n  (let ((PR (polynomial-ring (integer-ring))))\n    (ring? PR))  => #t\n  (let* ((PR (polynomial-ring (integer-ring)))\n         (p  (make-poly (integer-ring) '(1 2))))\n    (poly-coeffs (ring-plus PR p p)))  => (2 4)\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: polynomial ring, R[x], formal power series ring, coefficient ring, multivariate recursion\n\nSee also: `make-poly', `make-ring'."
  (make-ring
    poly-plus
    poly-times
    (poly-zero R)
    (poly-one R)
    poly-negate))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add polynomial-ring capstone constructor`

---

## Task 10: `with-polynomial` macro and validation

**Files:** same pattern.

- [ ] **Step 1: Add failing tests**

```scheme
(test-group "with-polynomial macro"
  (let ((R (integer-ring)))
    (test '(2 4) (poly-coeffs
                   (with-polynomial R (plus times zero one negate)
                     (plus (make-poly R '(1 2)) (make-poly R '(1 2))))))))

(test-group "validate-polynomial-ring"
  ;; Over the rational field (a field is a ring), polynomial laws must hold
  ;; on a small sample of polynomials.
  (let* ((F  (rational-field))
         (R  (field->ring F))
         (PR (polynomial-ring R))
         (samples (list (make-poly R '())
                        (make-poly R '(1))
                        (make-poly R '(1 2))
                        (make-poly R '(0 1))
                        (make-poly R '(-1 0 1)))))
    (test #t (validate-ring PR samples))))
```

Note the validation test reuses `validate-ring` rather than needing a polynomial-specific validator — the capstone makes polynomials first-class ring elements, so `validate-ring` applies directly.

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Export `with-polynomial`**

- [ ] **Step 4: Implement**

```scheme
(define-syntax with-polynomial
  (syntax-rules ()
    ((with-polynomial R (plus times zero one negate) body ...)
     (let ((tmp R))
       (let ((plus   poly-plus)
             (times  poly-times)
             (zero   (poly-zero tmp))
             (one    (poly-one tmp))
             (negate poly-negate))
         body ...)))))
```

- [ ] **Step 5: Verify tests pass**

- [ ] **Step 6: Propose commit**

Propose message: `feat(algebra/polynomial): add with-polynomial macro`

---

## Task 11: Refactor `differential.scm` to use new library

**Files:**
- Modify: `stdlib/lib/wile/algebra/differential.sld:10-11` — add `(wile algebra polynomial)` to imports
- Modify: `stdlib/lib/wile/algebra/differential.scm:75-142` — replace `polynomial-derivation` internals with calls into the new library
- Modify: `test/wile/algebra-differential-test.scm` — update tests that depended on list-representation (if any)

**Background:** `differential.scm:77-142` reimplements polynomial addition, multiplication, negation, shift, scale, and derivative as local helpers. Now we have a real library. The refactor:
1. Changes element representation from raw coefficient lists to `<polynomial>` records.
2. Updates the returned differential-ring so its underlying ring operates on polynomials, and its derivation is `poly-derivative`.

**Breaking-change note:** Wile is pre-consumer per CLAUDE.md — "break freely in minor versions." Callers of `polynomial-derivation` will now see `<polynomial>` records instead of bare coefficient lists from `differential-deriv`. This is an *improvement* (type-safe, uses the proper abstraction) and the test file must be updated to match.

- [ ] **Step 1: Read existing differential test to understand impact**

Run: `cat test/wile/algebra-differential-test.scm | grep -n 'polynomial-derivation\|poly' | head -20`

Review which assertions about `polynomial-derivation` need updating.

- [ ] **Step 2: Add `(wile algebra polynomial)` import to `differential.sld`**

Replace:

```scheme
  (import (scheme base)
          (wile algebra ring))
  (include "differential.scm"))
```

with:

```scheme
  (import (scheme base)
          (wile algebra ring)
          (wile algebra polynomial))
  (include "differential.scm"))
```

- [ ] **Step 3: Replace `polynomial-derivation` body in `differential.scm:77-142`**

Replace the entire function (and its local helpers `normalize`, `poly+`, `poly-neg`, `poly-scale`, `poly-shift`, `poly*`, `ring-nat`, `poly-deriv`) with:

```scheme
(define (polynomial-derivation R)
  "Construct a differential ring of polynomials over ring R.\nElements are <polynomial> records (see (wile algebra polynomial)).\nThe underlying ring is (polynomial-ring R); the derivation is the\nformal derivative poly-derivative.\n\nExamples:\n  (let* ((D (polynomial-derivation (integer-ring)))\n         (p (make-poly (integer-ring) '(3 2 1))))\n    (poly-coeffs (differential-deriv D p)))  => (2 2)\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: polynomial, formal derivative, differential ring, polynomial derivation\n\nSee also: `dual-number-ring', `make-differential-ring', `polynomial-ring'."
  (make-differential-ring (polynomial-ring R) poly-derivative))
```

This is the whole refactor. 66 lines of duplicated logic collapse to one line that composes two library calls.

- [ ] **Step 4: Update `test/wile/algebra-differential-test.scm`**

Every assertion that expected a *list* from `differential-deriv D p` must be updated to expect a `<polynomial>` or to call `poly-coeffs` on the result. Walk each failing test case and update.

- [ ] **Step 5: Verify tests pass**

Run: `$WILE --quiet -f test/wile/algebra-differential-test.scm`
Expected: all groups pass.

Run: `$WILE --quiet -f test/wile/algebra-polynomial-test.scm`
Expected: all groups pass.

- [ ] **Step 6: Propose commit**

Propose message: `refactor(algebra/differential): use (wile algebra polynomial) for polynomial-derivation`

---

## Task 12: Export via `algebra.sld` aggregator + pre-built instances

**Files:**
- Modify: `stdlib/lib/wile/algebra.sld`
- Modify: `stdlib/lib/wile/algebra/polynomial.scm` (pre-built instances)
- Modify: `test/wile/algebra-polynomial-test.scm`

- [ ] **Step 1: Add failing tests for pre-built instances**

```scheme
(test-group "pre-built integer-polynomials"
  (let ((PR (integer-polynomials)))
    (test #t (ring? PR))
    ;; Elements must be polynomials over integer-ring
    (let ((p (make-poly (integer-ring) '(1 2)))
          (q (make-poly (integer-ring) '(3 4))))
      (test '(4 6) (poly-coeffs (ring-plus PR p q))))))

(test-group "pre-built rational-polynomials"
  (let ((PR (rational-polynomials)))
    (test #t (ring? PR))
    (let ((p (make-poly (field->ring (rational-field)) '(1/2 1/3))))
      (test '(1 2/3) (poly-coeffs (ring-plus PR p p))))))
```

- [ ] **Step 2: Verify fail**

- [ ] **Step 3: Add instances to `polynomial.scm`**

```scheme
;; ─── Pre-built polynomial-ring instances ────

(define (integer-polynomials)
  "Construct the polynomial ring Z[x] over the integers.\nShortcut for (polynomial-ring (integer-ring)).\n\nExamples:\n  (ring? (integer-polynomials))  => #t\n\nReturns: any\nCategory: algebra\nKeywords: Z[x], integer polynomials, polynomial ring over integers\n\nSee also: `polynomial-ring', `integer-ring', `rational-polynomials'."
  (polynomial-ring (integer-ring)))

(define (rational-polynomials)
  "Construct the polynomial ring Q[x] over the rationals.\nShortcut for (polynomial-ring (field->ring (rational-field))).\n\nExamples:\n  (ring? (rational-polynomials))  => #t\n\nReturns: any\nCategory: algebra\nKeywords: Q[x], rational polynomials, polynomial ring over rationals\n\nSee also: `polynomial-ring', `rational-field', `integer-polynomials'."
  (polynomial-ring (field->ring (rational-field))))
```

- [ ] **Step 4: Update `polynomial.sld` exports**

Final export list:

```scheme
  (export make-poly polynomial?
          poly-ring poly-coeffs
          poly-zero poly-one
          poly-degree poly-leading-coeff
          poly-plus poly-negate poly-minus
          poly-times
          poly-eval
          poly-derivative
          poly-divmod poly-gcd
          polynomial-ring
          integer-polynomials rational-polynomials
          with-polynomial)
```

- [ ] **Step 5: Update `stdlib/lib/wile/algebra.sld` aggregator**

Insert after the Rings exports (after line 69 `validate-ring with-ring`), before `;; Differential rings`:

```scheme
    ;; Polynomials
    make-poly polynomial?
    poly-ring poly-coeffs
    poly-zero poly-one
    poly-degree poly-leading-coeff
    poly-plus poly-negate poly-minus
    poly-times
    poly-eval
    poly-derivative
    poly-divmod poly-gcd
    polynomial-ring
    integer-polynomials rational-polynomials
    with-polynomial
```

Add `(wile algebra polynomial)` to the `(import ...)` clause — insert between `(wile algebra ring)` and `(wile algebra differential)` to keep topological order (differential depends on polynomial).

- [ ] **Step 6: Verify tests pass**

Run: `$WILE --quiet -f test/wile/algebra-polynomial-test.scm`
Run: `$WILE --quiet -f test/wile/algebra-differential-test.scm`
Run: `$WILE --quiet -f test/wile/algebra-ring-test.scm` (sanity — should still pass unchanged)

- [ ] **Step 7: Full Scheme test suite**

Run: `cd /Users/aalpar/projects/wile-workspace/wile && make test-scheme`
Expected: all tests pass. This catches unintended breakage anywhere in the algebra tree.

- [ ] **Step 8: Go lint clean (defensive — no Go changed, but verify)**

Run: `make lint`
Expected: pass.

- [ ] **Step 9: Propose final commit**

Propose message: `feat(algebra): export polynomial library via algebra.sld aggregator with pre-built instances`

---

## Self-Review

**Spec coverage check** (user's original items 1–7):

| Item | Covered in | Notes |
|------|-----------|-------|
| 1. `(make-poly ring coeffs)` | Task 1 | Normalizes on construction |
| 2. `poly-plus`, `poly-times`, `poly-minus`, `poly-negate` | Tasks 3, 4 | Uses `ring-plus`/`ring-times` internally |
| 3. `poly-eval` (Horner O(n)) | Task 5 | Explicitly Horner, not naive |
| 4. `poly-degree`, `poly-leading-coeff` | Task 2 | zero-poly convention documented |
| 5. `poly-divmod` (field required) | Task 7 | Field passed explicitly |
| 6. `poly-gcd` (Euclidean) | Task 8 | Monic normalization chosen |
| 7. `poly-derivative` (formal) | Task 6 | Works over positive-characteristic rings |
| 8. `poly-factor`, `poly-roots` | **out of scope** | Deferred; earns separate plan |

**Bonus coverage beyond spec:**
- `polynomial-ring` capstone (Task 9) — enables multivariate via recursion
- `with-polynomial` macro (Task 10) — matches pattern of `with-ring`, `with-field`, etc.
- Pre-built `integer-polynomials`, `rational-polynomials` (Task 12) — matches pattern of `integer-ring`, `rational-field`

**Placeholder scan:** No "TBD", "TODO", "handle appropriately", or "similar to Task N" — every step has concrete code.

**Type consistency check:**
- `make-poly` is used consistently (Tasks 1–12) — never `make-poly*` at call sites (that's the raw constructor, internal only).
- `poly-ring`/`poly-coeffs` accessors consistent throughout.
- `poly-divmod` signature `(p d F)` — 3-arg with field last, used that way in Task 8.
- `poly-gcd` signature `(p q F)` — same pattern.

**Design decisions locked in:**
1. **Ascending-order coefficients** — matches prior art in `differential.scm`, Horner evaluates right-to-left via reverse.
2. **Zero polynomial = `'()`** — matches prior art.
3. **`poly-degree (zero-poly) = -1`** — PARI/GP convention, documented.
4. **Normalization inside `make-poly`** — makes invariants uniform; `make-poly*` is the raw constructor for internal use only.
5. **Field passed explicitly to `poly-divmod`/`poly-gcd`** — polynomial record only carries a ring; caller supplies field when needed. Keeps record minimal.
6. **`poly-gcd` returns monic result** — uniquely defined (otherwise defined only up to a unit).
7. **`polynomial-ring R` capstone** — composes with itself to produce multivariate rings.

**Open question (user should flag before execution):** If you prefer `poly-degree (zero-poly) = #f` (signaling "undefined") over `-1`, say so — change is localized to Task 2 plus the test expectations for zero-poly degree.

---

## Execution Handoff

Plan complete and saved to `plans/2026-04-18-polynomial-library.md`. Two execution options:

1. **Subagent-Driven (recommended)** — I dispatch a fresh subagent per task, review between tasks, fast iteration. Subagents respect the "propose commit, await approval" rule — you still structure the commits.
2. **Inline Execution** — I execute tasks in this session with checkpoints for your review.

Which approach?
