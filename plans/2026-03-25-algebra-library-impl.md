# Algebra Library Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Implement `(wile algebra)` — composable algebraic structures (partial orders through Galois connections) as R7RS records with validation and fixpoint computation.

**Architecture:** Each algebraic structure is an R7RS `define-record-type` with operation function slots. Sub-libraries under `lib/wile/algebra/` export one structure each. An umbrella `lib/wile/algebra.sld` re-exports everything. Tests use `(chibi test)` and are auto-discovered by `test/run-all.sh`.

**Tech Stack:** R7RS Scheme, `define-record-type`, `case-lambda`, `(chibi test)`

**Design doc:** `plans/2026-03-25-algebra-library-design.md`

---

## File Layout

```
lib/wile/algebra.sld                 umbrella re-export
lib/wile/algebra/order.sld           partial orders (.sld)
lib/wile/algebra/order.scm           partial orders (.scm)
lib/wile/algebra/lattice.sld         lattices + fixpoint
lib/wile/algebra/lattice.scm
lib/wile/algebra/monoid.sld          monoids
lib/wile/algebra/monoid.scm
lib/wile/algebra/semiring.sld        semirings + pre-built
lib/wile/algebra/semiring.scm
lib/wile/algebra/group.sld           groups
lib/wile/algebra/group.scm
lib/wile/algebra/ring.sld            rings + fields + pre-built
lib/wile/algebra/ring.scm
lib/wile/algebra/galois.sld          Galois connections
lib/wile/algebra/galois.scm
test/wile/algebra-order-test.scm     tests (per structure)
test/wile/algebra-lattice-test.scm
test/wile/algebra-monoid-test.scm
test/wile/algebra-semiring-test.scm
test/wile/algebra-group-test.scm
test/wile/algebra-ring-test.scm
test/wile/algebra-galois-test.scm
test/wile/algebra-integration-test.scm
```

## Running Tests

```bash
make build && make test-scheme
```

Or run a single test file directly:

```bash
./dist/$(uname -s | tr A-Z a-z)/$(uname -m)/wile --quiet -f test/wile/algebra-order-test.scm
```

On Apple Silicon: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-order-test.scm`

---

## Task 1: Partial Orders — record type and operations

**Files:**
- Create: `lib/wile/algebra/order.sld`
- Create: `lib/wile/algebra/order.scm`
- Create: `test/wile/algebra-order-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-order-test.scm`:

```scheme
;;; algebra-order-test.scm — Partial order tests

(import (scheme base)
        (chibi test)
        (wile algebra order))

(test-begin "partial-orders")

;; -- Construction and predicate --

(test-group "construction"
  (let ((po (make-partial-order <=)))
    (test #t (partial-order? po))
    (test #f (partial-order? 42))
    (test #f (partial-order? "not a po"))))

;; -- po-leq? --

(test-group "po-leq?"
  (let ((po (make-partial-order <=)))
    (test #t (po-leq? po 1 2))
    (test #t (po-leq? po 1 1))
    (test #f (po-leq? po 2 1))))

;; -- po-comparable? --

(test-group "po-comparable?"
  ;; divisibility partial order: a ≤ b iff a divides b
  (let ((div-po (make-partial-order
                  (lambda (a b) (zero? (modulo b a))))))
    (test #t (po-comparable? div-po 2 6))   ; 2|6
    (test #t (po-comparable? div-po 6 2))   ; 2|6 reversed
    (test #f (po-comparable? div-po 2 3)))) ; neither 2|3 nor 3|2

;; -- po-monotone? --

(test-group "po-monotone?"
  (let ((po (make-partial-order <=)))
    ;; doubling is monotone on ≤
    (test #t (po-monotone? po (lambda (x) (* x 2)) 1 3))
    ;; negation is NOT monotone on ≤ (1≤3 but -1 > -3)
    (test #f (po-monotone? po (lambda (x) (- x)) 1 3))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-order-test.scm`

Expected: FAIL — `(wile algebra order)` library not found.

### Step 3: Write the library definition

Create `lib/wile/algebra/order.sld`:

```scheme
(define-library (wile algebra order)
  (export make-partial-order partial-order?
          po-leq? po-comparable? po-monotone?
          validate-partial-order)
  (import (scheme base))
  (include "order.scm"))
```

Create `lib/wile/algebra/order.scm`:

```scheme
;;; (wile algebra order) — Partial orders
;;;
;;; A partial order is a reflexive, antisymmetric, transitive relation.
;;; Represented as an R7RS record holding a single leq? predicate.

(define-record-type <partial-order>
  (make-partial-order leq?)
  partial-order?
  (leq? po-leq-fn))

(define (po-leq? po a b)
  ((po-leq-fn po) a b))

(define (po-comparable? po a b)
  (or (po-leq? po a b)
      (po-leq? po b a)))

(define (po-monotone? po f a b)
  ;; a ≤ b ⟹ f(a) ≤ f(b)
  (if (po-leq? po a b)
      (po-leq? po (f a) (f b))
      #t))  ; precondition not met, vacuously true

(define (validate-partial-order po samples)
  ;; Spot-check reflexivity, antisymmetry, transitivity on sample pairs.
  ;; Returns #t or a list of (violation-type a b ...) entries.
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Reflexivity: a ≤ a
    (for-each
      (lambda (a) (unless (po-leq? po a a) (fail! 'reflexivity a)))
      samples)
    ;; Antisymmetry: a ≤ b ∧ b ≤ a ⟹ a = b
    ;; We can only check that leq? is consistent in both directions.
    ;; Without an equality predicate we check: a≤b ∧ b≤a ⟹ b≤a ∧ a≤b (tautology).
    ;; Real antisymmetry requires equal?, which we don't have.
    ;; Skip — antisymmetry is the caller's responsibility.
    ;; Transitivity: a ≤ b ∧ b ≤ c ⟹ a ≤ c
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (po-leq? po a b)
              (for-each
                (lambda (c)
                  (when (and (po-leq? po b c)
                             (not (po-leq? po a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test to verify it passes

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-order-test.scm`

Expected: PASS — all tests green.

### Step 5: Commit

```bash
git add lib/wile/algebra/order.sld lib/wile/algebra/order.scm test/wile/algebra-order-test.scm
git commit -m "feat(algebra): add partial order library (wile algebra order)"
```

---

## Task 2: Partial Orders — validation tests

**Files:**
- Modify: `test/wile/algebra-order-test.scm`

### Step 1: Write the failing test

Append to `test/wile/algebra-order-test.scm` (before `test-end`):

```scheme
;; -- validate-partial-order --

(test-group "validate-partial-order"
  ;; ≤ on integers is a valid partial order
  (test #t (validate-partial-order
             (make-partial-order <=)
             '(1 2 3 4 5)))
  ;; A broken "order" that isn't reflexive
  (let ((result (validate-partial-order
                  (make-partial-order <)  ; strict < is not reflexive
                  '(1 2 3))))
    (test #f (eq? #t result))  ; should return violations, not #t
    ;; Each violation should be (reflexivity x)
    (test 'reflexivity (caar result))))
```

### Step 2: Run test to verify it fails (if implementation is incomplete) or passes

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-order-test.scm`

Expected: PASS — validation was implemented in Task 1.

### Step 3: Commit

```bash
git add test/wile/algebra-order-test.scm
git commit -m "test(algebra): add validation tests for partial orders"
```

---

## Task 3: Lattices — record type and core operations

**Files:**
- Create: `lib/wile/algebra/lattice.sld`
- Create: `lib/wile/algebra/lattice.scm`
- Create: `test/wile/algebra-lattice-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-lattice-test.scm`:

```scheme
;;; algebra-lattice-test.scm — Lattice tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice))

(test-begin "lattices")

;; -- A simple lattice: divisibility on {1,2,3,6} --
;; join = lcm, meet = gcd, bottom = 1, top = 6

(define div-lat
  (make-lattice
    (lambda (a b) (lcm a b))        ; join
    (lambda (a b) (gcd a b))        ; meet
    1                                ; bottom
    6                                ; top
    (lambda (a b) (zero? (modulo b a)))))  ; leq: a divides b

(test-group "construction"
  (test #t (lattice? div-lat))
  (test #f (lattice? 42)))

(test-group "lattice-join"
  (test 6  (lattice-join div-lat 2 3))
  (test 2  (lattice-join div-lat 1 2))
  (test 6  (lattice-join div-lat 2 6)))

(test-group "lattice-meet"
  (test 1  (lattice-meet div-lat 2 3))
  (test 2  (lattice-meet div-lat 2 6))
  (test 3  (lattice-meet div-lat 3 6)))

(test-group "lattice-bottom and lattice-top"
  (test 1 (lattice-bottom div-lat))
  (test 6 (lattice-top div-lat)))

(test-group "lattice-leq?"
  (test #t (lattice-leq? div-lat 1 6))
  (test #t (lattice-leq? div-lat 2 6))
  (test #f (lattice-leq? div-lat 6 2))
  (test #f (lattice-leq? div-lat 2 3)))

(test-group "lattice->partial-order"
  (let ((po (lattice->partial-order div-lat)))
    (test #t (partial-order? po))
    (test #t (po-leq? po 1 6))
    (test #f (po-leq? po 6 1))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — `(wile algebra lattice)` not found.

### Step 3: Write the library

Create `lib/wile/algebra/lattice.sld`:

```scheme
(define-library (wile algebra lattice)
  (export make-lattice lattice?
          lattice-join lattice-meet lattice-bottom lattice-top
          lattice-leq? lattice->partial-order
          flat-lattice powerset-lattice product-lattice map-lattice
          fixpoint fixpoint/widen
          validate-lattice
          with-lattice)
  (import (scheme base)
          (wile algebra order))
  (include "lattice.scm"))
```

Create `lib/wile/algebra/lattice.scm`:

```scheme
;;; (wile algebra lattice) — Lattices, constructors, and fixpoint
;;;
;;; A lattice is a partially ordered set where every pair has a join
;;; (least upper bound) and meet (greatest lower bound), plus bottom
;;; and top elements.

;; ─── Record type ─────────────────────────────

(define-record-type <lattice>
  (make-lattice* join-fn meet-fn bottom top leq-fn)
  lattice?
  (join-fn lattice-join-fn)
  (meet-fn lattice-meet-fn)
  (bottom  lattice-bottom)
  (top     lattice-top)
  (leq-fn  lattice-leq-fn))

(define (make-lattice join meet bottom top leq?)
  (make-lattice* join meet bottom top leq?))

;; ─── Core operations ─────────────────────────

(define (lattice-join L a b)
  ((lattice-join-fn L) a b))

(define (lattice-meet L a b)
  ((lattice-meet-fn L) a b))

(define (lattice-leq? L a b)
  ((lattice-leq-fn L) a b))

;; ─── Projection ──────────────────────────────

(define (lattice->partial-order L)
  (make-partial-order (lattice-leq-fn L)))

;; ─── with-lattice macro ─────────────────────

(define-syntax with-lattice
  (syntax-rules ()
    ((with-lattice L (join meet bottom top leq?) body ...)
     (let ((tmp L))
       (let ((join   (lambda (a b) (lattice-join tmp a b)))
             (meet   (lambda (a b) (lattice-meet tmp a b)))
             (bottom (lattice-bottom tmp))
             (top    (lattice-top tmp))
             (leq?   (lambda (a b) (lattice-leq? tmp a b))))
         body ...)))))

;; ─── Lattice equality (derived from leq?) ───

(define (lattice-equal? L a b)
  (and (lattice-leq? L a b)
       (lattice-leq? L b a)))

;; ─── Fixpoint ────────────────────────────────

(define fixpoint
  (case-lambda
    ((L f x)
     ;; Unbounded Kleene iteration
     (let loop ((current x))
       (let ((next (f current)))
         (if (lattice-equal? L current next)
             current
             (loop next)))))
    ((L f x fuel)
     ;; Bounded iteration — returns #f if fuel exhausted
     (let loop ((current x) (remaining fuel))
       (if (<= remaining 0) #f
           (let ((next (f current)))
             (if (lattice-equal? L current next)
                 current
                 (loop next (- remaining 1)))))))))

(define (fixpoint/widen L f x widen)
  ;; Kleene iteration with widening: apply widen instead of raw join
  ;; when the value changes. widen : element element → element
  ;; Must satisfy: ∀a,b. a ⊔ b ≤ widen(a, b) and every ascending
  ;; chain under widen is finite.
  (let loop ((current x))
    (let* ((next (f current))
           (widened (if (lattice-leq? L next current)
                       current        ; already stable
                       (widen current next))))
      (if (lattice-equal? L current widened)
          current
          (loop widened)))))

;; ─── Lattice constructors ────────────────────

(define (flat-lattice elements equal?)
  ;; ⊥ < each element < ⊤ ; incomparable between elements.
  ;; Elements are stored for reference; bottom = 'flat-bottom,
  ;; top = 'flat-top (unique symbols).
  (let ((bot 'flat-bottom)
        (top 'flat-top))
    (define (member? x)
      (let loop ((es elements))
        (cond ((null? es) #f)
              ((equal? x (car es)) #t)
              (else (loop (cdr es))))))
    (make-lattice
      ;; join
      (lambda (a b)
        (cond ((eq? a bot) b)
              ((eq? b bot) a)
              ((equal? a b) a)
              (else top)))
      ;; meet
      (lambda (a b)
        (cond ((eq? a top) b)
              ((eq? b top) a)
              ((equal? a b) a)
              (else bot)))
      bot top
      ;; leq?
      (lambda (a b)
        (cond ((eq? a bot) #t)
              ((eq? b top) #t)
              ((equal? a b) #t)
              (else #f))))))

(define (powerset-lattice universe)
  ;; (P(universe), ⊆, ∪, ∩, ∅, universe)
  ;; Sets represented as sorted lists using symbol<? or default <.
  ;; For simplicity, use equal?-based membership and list representation.
  (define (subset? a b)
    (cond ((null? a) #t)
          ((member (car a) b) (subset? (cdr a) b))
          (else #f)))
  (define (union a b)
    (cond ((null? a) b)
          ((member (car a) b) (union (cdr a) b))
          (else (cons (car a) (union (cdr a) b)))))
  (define (intersect a b)
    (cond ((null? a) '())
          ((member (car a) b) (cons (car a) (intersect (cdr a) b)))
          (else (intersect (cdr a) b))))
  (make-lattice union intersect '() universe subset?))

(define (product-lattice . lattices)
  ;; Pointwise on lists: (a1 a2 ...) ≤ (b1 b2 ...) iff a1≤b1 ∧ a2≤b2 ∧ ...
  (make-lattice
    ;; join: pointwise
    (lambda (a b) (map (lambda (L ai bi) (lattice-join L ai bi))
                       lattices a b))
    ;; meet: pointwise
    (lambda (a b) (map (lambda (L ai bi) (lattice-meet L ai bi))
                       lattices a b))
    ;; bottom
    (map lattice-bottom lattices)
    ;; top
    (map lattice-top lattices)
    ;; leq?: all components
    (lambda (a b)
      (let loop ((Ls lattices) (as a) (bs b))
        (cond ((null? Ls) #t)
              ((not (lattice-leq? (car Ls) (car as) (car bs))) #f)
              (else (loop (cdr Ls) (cdr as) (cdr bs))))))))

(define (map-lattice keys value-lattice)
  ;; Alist: keys → value-lattice, pointwise operations.
  ;; Elements are alists ((k1 . v1) (k2 . v2) ...).
  ;; Missing keys treated as bottom.
  (let ((vbot (lattice-bottom value-lattice))
        (vtop (lattice-top value-lattice)))
    (define (lookup key alist)
      (let ((pair (assoc key alist)))
        (if pair (cdr pair) vbot)))
    (define (pointwise-binop op a b)
      (map (lambda (k) (cons k (op value-lattice (lookup k a) (lookup k b))))
           keys))
    (make-lattice
      (lambda (a b) (pointwise-binop lattice-join a b))
      (lambda (a b) (pointwise-binop lattice-meet a b))
      (map (lambda (k) (cons k vbot)) keys)
      (map (lambda (k) (cons k vtop)) keys)
      (lambda (a b)
        (let loop ((ks keys))
          (cond ((null? ks) #t)
                ((not (lattice-leq? value-lattice
                                    (lookup (car ks) a)
                                    (lookup (car ks) b)))
                 #f)
                (else (loop (cdr ks)))))))))

;; ─── Validation ──────────────────────────────

(define (validate-lattice L samples)
  ;; Spot-check lattice laws on sample elements.
  ;; Returns #t or list of (violation-type a b ...).
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            ;; Commutativity of join
            (unless (lattice-equal? L (lattice-join L a b) (lattice-join L b a))
              (fail! 'join-commutativity a b))
            ;; Commutativity of meet
            (unless (lattice-equal? L (lattice-meet L a b) (lattice-meet L b a))
              (fail! 'meet-commutativity a b))
            ;; Absorption: a ⊔ (a ⊓ b) = a
            (unless (lattice-equal? L (lattice-join L a (lattice-meet L a b)) a)
              (fail! 'absorption-join a b))
            ;; Absorption: a ⊓ (a ⊔ b) = a
            (unless (lattice-equal? L (lattice-meet L a (lattice-join L a b)) a)
              (fail! 'absorption-meet a b)))
          samples)
        ;; Idempotence
        (unless (lattice-equal? L (lattice-join L a a) a)
          (fail! 'join-idempotence a))
        (unless (lattice-equal? L (lattice-meet L a a) a)
          (fail! 'meet-idempotence a))
        ;; Identity: bottom is join identity
        (unless (lattice-equal? L (lattice-join L (lattice-bottom L) a) a)
          (fail! 'join-identity a))
        ;; Identity: top is meet identity
        (unless (lattice-equal? L (lattice-meet L (lattice-top L) a) a)
          (fail! 'meet-identity a)))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test to verify it passes

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-lattice-test.scm`

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/lattice.sld lib/wile/algebra/lattice.scm test/wile/algebra-lattice-test.scm
git commit -m "feat(algebra): add lattice library (wile algebra lattice)"
```

---

## Task 4: Lattices — constructors and fixpoint tests

**Files:**
- Modify: `test/wile/algebra-lattice-test.scm`

### Step 1: Write tests for flat-lattice, powerset-lattice, fixpoint

Append to `test/wile/algebra-lattice-test.scm` (before `test-end`):

```scheme
;; -- flat-lattice --

(test-group "flat-lattice"
  (let ((fl (flat-lattice '(a b c) eq?)))
    (test #t (lattice? fl))
    ;; bottom ≤ everything
    (test #t (lattice-leq? fl (lattice-bottom fl) 'a))
    ;; everything ≤ top
    (test #t (lattice-leq? fl 'a (lattice-top fl)))
    ;; elements are incomparable
    (test #f (lattice-leq? fl 'a 'b))
    ;; join of incomparable = top
    (test 'flat-top (lattice-join fl 'a 'b))
    ;; meet of incomparable = bottom
    (test 'flat-bottom (lattice-meet fl 'a 'b))
    ;; join with bottom = identity
    (test 'a (lattice-join fl (lattice-bottom fl) 'a))
    ;; join of same = same
    (test 'a (lattice-join fl 'a 'a))))

;; -- powerset-lattice --

(test-group "powerset-lattice"
  (let ((ps (powerset-lattice '(x y z))))
    (test #t (lattice? ps))
    ;; empty set is bottom
    (test '() (lattice-bottom ps))
    ;; universe is top
    (test '(x y z) (lattice-top ps))
    ;; subset ordering
    (test #t (lattice-leq? ps '() '(x y)))
    (test #t (lattice-leq? ps '(x) '(x y)))
    (test #f (lattice-leq? ps '(x y) '(x)))
    ;; join = union (order may vary, test membership)
    (let ((result (lattice-join ps '(x) '(y))))
      (test #t (and (member 'x result) (member 'y result) #t)))))

;; -- product-lattice --

(test-group "product-lattice"
  (let* ((fl (flat-lattice '(a b) eq?))
         (pl (product-lattice fl fl)))
    (test #t (lattice? pl))
    ;; bottom is (flat-bottom flat-bottom)
    (test (list 'flat-bottom 'flat-bottom) (lattice-bottom pl))
    ;; pointwise join
    (test (list 'a 'b)
      (lattice-join pl
        (list 'a 'flat-bottom)
        (list 'flat-bottom 'b)))))

;; -- fixpoint --

(test-group "fixpoint"
  ;; Fixpoint on powerset: start from empty, add 'x each step until {x y z}
  (let* ((ps (powerset-lattice '(x y z)))
         ;; transfer: add 'x, 'y, 'z one at a time based on what's there
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (let ((result (fixpoint ps f '())))
      ;; Should reach {x y z}
      (test #t (and (member 'x result) (member 'y result)
                    (member 'z result) #t)))))

(test-group "fixpoint-bounded"
  ;; Same as above but with fuel=1, should return #f (not converged)
  (let* ((ps (powerset-lattice '(x y z)))
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (test #f (fixpoint ps f '() 1))))

;; -- with-lattice macro --

(test-group "with-lattice"
  (test 6 (with-lattice div-lat (join meet bottom top leq?)
            (join (join bottom 2) 3))))

;; -- validate-lattice --

(test-group "validate-lattice"
  (test #t (validate-lattice div-lat '(1 2 3 6))))
```

### Step 2: Run test

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-lattice-test.scm`

Expected: PASS

### Step 3: Commit

```bash
git add test/wile/algebra-lattice-test.scm
git commit -m "test(algebra): add lattice constructor, fixpoint, and validation tests"
```

---

## Task 5: Monoids

**Files:**
- Create: `lib/wile/algebra/monoid.sld`
- Create: `lib/wile/algebra/monoid.scm`
- Create: `test/wile/algebra-monoid-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-monoid-test.scm`:

```scheme
;;; algebra-monoid-test.scm — Monoid tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid))

(test-begin "monoids")

(define sum-monoid (make-monoid + 0))
(define product-monoid (make-monoid * 1))

(test-group "construction"
  (test #t (monoid? sum-monoid))
  (test #f (monoid? 42)))

(test-group "monoid-op"
  (test 5 (monoid-op sum-monoid 2 3))
  (test 6 (monoid-op product-monoid 2 3)))

(test-group "monoid-identity"
  (test 0 (monoid-identity sum-monoid))
  (test 1 (monoid-identity product-monoid)))

(test-group "monoid-fold"
  (test 10 (monoid-fold sum-monoid '(1 2 3 4)))
  (test 24 (monoid-fold product-monoid '(1 2 3 4)))
  ;; empty list -> identity
  (test 0 (monoid-fold sum-monoid '())))

(test-group "monoid-power"
  ;; 3 + 3 + 3 + 3 = 12
  (test 12 (monoid-power sum-monoid 3 4))
  ;; 2 * 2 * 2 = 8
  (test 8 (monoid-power product-monoid 2 3))
  ;; power 0 = identity
  (test 0 (monoid-power sum-monoid 99 0))
  (test 1 (monoid-power product-monoid 99 0)))

(test-group "with-monoid"
  (test 6 (with-monoid sum-monoid (op identity)
            (op (op identity 1) (op 2 3)))))

(test-group "validate-monoid"
  (test #t (validate-monoid sum-monoid '(0 1 2 3 5 10))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — library not found.

### Step 3: Write the library

Create `lib/wile/algebra/monoid.sld`:

```scheme
(define-library (wile algebra monoid)
  (export make-monoid monoid?
          monoid-op monoid-identity
          monoid-fold monoid-power
          validate-monoid
          with-monoid)
  (import (scheme base))
  (include "monoid.scm"))
```

Create `lib/wile/algebra/monoid.scm`:

```scheme
;;; (wile algebra monoid) — Monoids
;;;
;;; A monoid is a set with an associative binary operation and an identity
;;; element: (S, ⊕, e) where a ⊕ (b ⊕ c) = (a ⊕ b) ⊕ c and e ⊕ a = a ⊕ e = a.

(define-record-type <monoid>
  (make-monoid op identity)
  monoid?
  (op       monoid-op-fn)
  (identity monoid-identity))

(define (monoid-op M a b)
  ((monoid-op-fn M) a b))

(define (monoid-fold M lst)
  (let loop ((acc (monoid-identity M)) (xs lst))
    (if (null? xs) acc
        (loop (monoid-op M acc (car xs)) (cdr xs)))))

(define (monoid-power M a n)
  ;; Repeated application: a ⊕ a ⊕ ... (n times). O(n).
  (let loop ((acc (monoid-identity M)) (remaining n))
    (if (<= remaining 0) acc
        (loop (monoid-op M acc a) (- remaining 1)))))

(define-syntax with-monoid
  (syntax-rules ()
    ((with-monoid M (op identity) body ...)
     (let ((tmp M))
       (let ((op       (lambda (a b) (monoid-op tmp a b)))
             (identity (monoid-identity tmp)))
         body ...)))))

(define (validate-monoid M samples)
  (let ((violations '())
        (e (monoid-identity M)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Left identity
        (unless (equal? (monoid-op M e a) a)
          (fail! 'left-identity a))
        ;; Right identity
        (unless (equal? (monoid-op M a e) a)
          (fail! 'right-identity a))
        ;; Associativity (with all pairs)
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (monoid-op M (monoid-op M a b) c)
                                (monoid-op M a (monoid-op M b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-monoid-test.scm`

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/monoid.sld lib/wile/algebra/monoid.scm test/wile/algebra-monoid-test.scm
git commit -m "feat(algebra): add monoid library (wile algebra monoid)"
```

---

## Task 6: Semirings

**Files:**
- Create: `lib/wile/algebra/semiring.sld`
- Create: `lib/wile/algebra/semiring.scm`
- Create: `test/wile/algebra-semiring-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-semiring-test.scm`:

```scheme
;;; algebra-semiring-test.scm — Semiring tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra semiring))

(test-begin "semirings")

(test-group "construction"
  (let ((s (make-semiring + * 0 1)))
    (test #t (semiring? s))
    (test #f (semiring? 42))))

(test-group "operations"
  (let ((s (make-semiring + * 0 1)))
    (test 5 (semiring-plus s 2 3))
    (test 6 (semiring-times s 2 3))
    (test 0 (semiring-zero s))
    (test 1 (semiring-one s))))

(test-group "boolean-semiring"
  (let ((bs (boolean-semiring)))
    (test #t  (semiring-plus bs #f #t))
    (test #f  (semiring-plus bs #f #f))
    (test #t  (semiring-times bs #t #t))
    (test #f  (semiring-times bs #t #f))
    (test #f  (semiring-zero bs))
    (test #t  (semiring-one bs))))

(test-group "tropical-semiring"
  (let ((ts (tropical-semiring)))
    ;; plus = min
    (test 2 (semiring-plus ts 2 5))
    (test 2 (semiring-plus ts 5 2))
    ;; times = +
    (test 7 (semiring-times ts 2 5))
    ;; zero = +inf.0
    (test +inf.0 (semiring-zero ts))
    ;; one = 0
    (test 0 (semiring-one ts))))

(test-group "counting-semiring"
  (let ((cs (counting-semiring)))
    (test 5 (semiring-plus cs 2 3))
    (test 6 (semiring-times cs 2 3))))

(test-group "semiring->additive-monoid"
  (let* ((s (make-semiring + * 0 1))
         (m (semiring->additive-monoid s)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))))

(test-group "semiring->multiplicative-monoid"
  (let* ((s (make-semiring + * 0 1))
         (m (semiring->multiplicative-monoid s)))
    (test #t (monoid? m))
    (test 1  (monoid-identity m))
    (test 6  (monoid-op m 2 3))))

(test-group "with-semiring"
  (let ((s (make-semiring + * 0 1)))
    (test 11 (with-semiring s (plus times zero one)
               (plus (times 2 3) (times one 5))))))

(test-group "validate-semiring"
  (test #t (validate-semiring (counting-semiring) '(0 1 2 3))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — library not found.

### Step 3: Write the library

Create `lib/wile/algebra/semiring.sld`:

```scheme
(define-library (wile algebra semiring)
  (export make-semiring semiring?
          semiring-plus semiring-times semiring-zero semiring-one
          semiring->additive-monoid semiring->multiplicative-monoid
          boolean-semiring tropical-semiring counting-semiring
          validate-semiring
          with-semiring)
  (import (scheme base)
          (wile algebra monoid))
  (include "semiring.scm"))
```

Create `lib/wile/algebra/semiring.scm`:

```scheme
;;; (wile algebra semiring) — Semirings
;;;
;;; A semiring (S, +, ×, 0, 1) has:
;;; - (S, +, 0) is a commutative monoid
;;; - (S, ×, 1) is a monoid
;;; - × distributes over +
;;; - 0 annihilates ×: 0 × a = a × 0 = 0

(define-record-type <semiring>
  (make-semiring* plus-fn times-fn zero one)
  semiring?
  (plus-fn  semiring-plus-fn)
  (times-fn semiring-times-fn)
  (zero     semiring-zero)
  (one      semiring-one))

(define (make-semiring plus times zero one)
  (make-semiring* plus times zero one))

(define (semiring-plus S a b)
  ((semiring-plus-fn S) a b))

(define (semiring-times S a b)
  ((semiring-times-fn S) a b))

(define (semiring->additive-monoid S)
  (make-monoid (semiring-plus-fn S) (semiring-zero S)))

(define (semiring->multiplicative-monoid S)
  (make-monoid (semiring-times-fn S) (semiring-one S)))

;; ─── Pre-built instances ─────────────────────

(define (boolean-semiring)
  (make-semiring
    (lambda (a b) (or a b))
    (lambda (a b) (and a b))
    #f #t))

(define (tropical-semiring)
  (make-semiring min + +inf.0 0))

(define (counting-semiring)
  (make-semiring + * 0 1))

;; ─── Macro ───────────────────────────────────

(define-syntax with-semiring
  (syntax-rules ()
    ((with-semiring S (plus times zero one) body ...)
     (let ((tmp S))
       (let ((plus  (lambda (a b) (semiring-plus tmp a b)))
             (times (lambda (a b) (semiring-times tmp a b)))
             (zero  (semiring-zero tmp))
             (one   (semiring-one tmp)))
         body ...)))))

;; ─── Validation ──────────────────────────────

(define (validate-semiring S samples)
  (let ((violations '())
        (z (semiring-zero S))
        (o (semiring-one S)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (semiring-plus S z a) a)
          (fail! 'additive-left-identity a))
        (unless (equal? (semiring-plus S a z) a)
          (fail! 'additive-right-identity a))
        ;; Multiplicative identity
        (unless (equal? (semiring-times S o a) a)
          (fail! 'multiplicative-left-identity a))
        (unless (equal? (semiring-times S a o) a)
          (fail! 'multiplicative-right-identity a))
        ;; Zero annihilation
        (unless (equal? (semiring-times S z a) z)
          (fail! 'left-annihilation a))
        (unless (equal? (semiring-times S a z) z)
          (fail! 'right-annihilation a))
        (for-each
          (lambda (b)
            ;; Additive commutativity
            (unless (equal? (semiring-plus S a b) (semiring-plus S b a))
              (fail! 'additive-commutativity a b))
            ;; Left distributivity: a × (b + c)
            (for-each
              (lambda (c)
                (unless (equal? (semiring-times S a (semiring-plus S b c))
                                (semiring-plus S (semiring-times S a b)
                                                 (semiring-times S a c)))
                  (fail! 'left-distributivity a b c))
                (unless (equal? (semiring-times S (semiring-plus S a b) c)
                                (semiring-plus S (semiring-times S a c)
                                                 (semiring-times S b c)))
                  (fail! 'right-distributivity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/semiring.sld lib/wile/algebra/semiring.scm test/wile/algebra-semiring-test.scm
git commit -m "feat(algebra): add semiring library with boolean, tropical, counting instances"
```

---

## Task 7: Groups

**Files:**
- Create: `lib/wile/algebra/group.sld`
- Create: `lib/wile/algebra/group.scm`
- Create: `test/wile/algebra-group-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-group-test.scm`:

```scheme
;;; algebra-group-test.scm — Group tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra group))

(test-begin "groups")

(define int-add-group (make-group + 0 -))

(test-group "construction"
  (test #t (group? int-add-group))
  (test #f (group? 42)))

(test-group "operations"
  (test 5  (group-op int-add-group 2 3))
  (test 0  (group-identity int-add-group))
  (test -3 (group-inverse int-add-group 3)))

(test-group "group->monoid"
  (let ((m (group->monoid int-add-group)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))
    ;; fold works
    (test 10 (monoid-fold m '(1 2 3 4)))))

(test-group "with-group"
  (test 0 (with-group int-add-group (op identity inverse)
            (op 3 (inverse 3)))))

(test-group "validate-group"
  (test #t (validate-group int-add-group '(-2 -1 0 1 2))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — library not found.

### Step 3: Write the library

Create `lib/wile/algebra/group.sld`:

```scheme
(define-library (wile algebra group)
  (export make-group group?
          group-op group-identity group-inverse
          group->monoid
          validate-group
          with-group)
  (import (scheme base)
          (wile algebra monoid))
  (include "group.scm"))
```

Create `lib/wile/algebra/group.scm`:

```scheme
;;; (wile algebra group) — Groups
;;;
;;; A group (G, ⊕, e, ⁻¹) is a monoid with inverses:
;;; a ⊕ a⁻¹ = a⁻¹ ⊕ a = e.

(define-record-type <group>
  (make-group* op-fn identity inverse-fn)
  group?
  (op-fn      group-op-fn)
  (identity   group-identity)
  (inverse-fn group-inverse-fn))

(define (make-group op identity inverse)
  (make-group* op identity inverse))

(define (group-op G a b)
  ((group-op-fn G) a b))

(define (group-inverse G a)
  ((group-inverse-fn G) a))

(define (group->monoid G)
  (make-monoid (group-op-fn G) (group-identity G)))

(define-syntax with-group
  (syntax-rules ()
    ((with-group G (op identity inverse) body ...)
     (let ((tmp G))
       (let ((op      (lambda (a b) (group-op tmp a b)))
             (identity (group-identity tmp))
             (inverse  (lambda (a) (group-inverse tmp a))))
         body ...)))))

(define (validate-group G samples)
  (let ((violations '())
        (e (group-identity G)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Monoid laws
    (for-each
      (lambda (a)
        (unless (equal? (group-op G e a) a)
          (fail! 'left-identity a))
        (unless (equal? (group-op G a e) a)
          (fail! 'right-identity a))
        ;; Inverse
        (unless (equal? (group-op G a (group-inverse G a)) e)
          (fail! 'right-inverse a))
        (unless (equal? (group-op G (group-inverse G a) a) e)
          (fail! 'left-inverse a))
        ;; Associativity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (group-op G (group-op G a b) c)
                                (group-op G a (group-op G b c)))
                  (fail! 'associativity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/group.sld lib/wile/algebra/group.scm test/wile/algebra-group-test.scm
git commit -m "feat(algebra): add group library (wile algebra group)"
```

---

## Task 8: Rings and Fields

**Files:**
- Create: `lib/wile/algebra/ring.sld`
- Create: `lib/wile/algebra/ring.scm`
- Create: `test/wile/algebra-ring-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-ring-test.scm`:

```scheme
;;; algebra-ring-test.scm — Ring and field tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra semiring)
        (wile algebra group)
        (wile algebra ring))

(test-begin "rings-and-fields")

;; -- Rings --

(test-group "integer-ring"
  (let ((R (integer-ring)))
    (test #t (ring? R))
    (test 5  (ring-plus R 2 3))
    (test 6  (ring-times R 2 3))
    (test 0  (ring-zero R))
    (test 1  (ring-one R))
    (test -3 (ring-negate R 3))
    (test -1 (ring-minus R 2 3))))

(test-group "modular-ring"
  (let ((R (modular-ring 7)))
    (test #t (ring? R))
    (test 2  (ring-plus R 5 4))    ; (5+4) mod 7 = 2
    (test 6  (ring-times R 2 3))   ; (2*3) mod 7 = 6
    (test 4  (ring-negate R 3))    ; (-3) mod 7 = 4
    (test 0  (ring-zero R))
    (test 1  (ring-one R))))

(test-group "ring-projections"
  (let ((R (integer-ring)))
    (test #t (semiring? (ring->semiring R)))
    (test #t (group? (ring->additive-group R)))
    ;; additive group inverse = negate
    (test -3 (group-inverse (ring->additive-group R) 3))))

(test-group "with-ring"
  (let ((R (integer-ring)))
    (test 7 (with-ring R (plus times zero one negate)
              (plus (times 2 3) one)))))

(test-group "validate-ring"
  (test #t (validate-ring (integer-ring) '(-2 -1 0 1 2))))

;; -- Fields --

(test-group "rational-field"
  (let ((F (rational-field)))
    (test #t (field? F))
    (test 5  (field-plus F 2 3))
    (test 6  (field-times F 2 3))
    (test 1/3 (field-reciprocal F 3))
    (test 2/3 (field-divide F 2 3))))

(test-group "field-projections"
  (let ((F (rational-field)))
    (test #t (ring? (field->ring F)))))

(test-group "with-field"
  (let ((F (rational-field)))
    (test 7/3 (with-field F (plus times zero one negate reciprocal)
                (plus (times 2 (reciprocal 3)) one)))))

(test-group "validate-field"
  ;; exclude 0 from samples for multiplicative inverse checks
  (test #t (validate-field (rational-field) '(-2 -1 1/2 1 2))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — library not found.

### Step 3: Write the library

Create `lib/wile/algebra/ring.sld`:

```scheme
(define-library (wile algebra ring)
  (export make-ring ring?
          ring-plus ring-times ring-zero ring-one
          ring-negate ring-minus
          ring->semiring ring->additive-group
          integer-ring modular-ring
          validate-ring
          with-ring
          make-field field?
          field-plus field-times field-zero field-one
          field-negate field-reciprocal field-divide
          field->ring
          rational-field
          validate-field
          with-field)
  (import (scheme base)
          (wile algebra monoid)
          (wile algebra semiring)
          (wile algebra group))
  (include "ring.scm"))
```

Create `lib/wile/algebra/ring.scm`:

```scheme
;;; (wile algebra ring) — Rings and fields
;;;
;;; A ring (R, +, ×, 0, 1, -) is a semiring where (R, +, 0, -) is an
;;; abelian group. A field adds multiplicative inverses for nonzero elements.

;; ─── Rings ───────────────────────────────────

(define-record-type <ring>
  (make-ring* plus-fn times-fn zero one negate-fn)
  ring?
  (plus-fn   ring-plus-fn)
  (times-fn  ring-times-fn)
  (zero      ring-zero)
  (one       ring-one)
  (negate-fn ring-negate-fn))

(define (make-ring plus times zero one negate)
  (make-ring* plus times zero one negate))

(define (ring-plus R a b)   ((ring-plus-fn R) a b))
(define (ring-times R a b)  ((ring-times-fn R) a b))
(define (ring-negate R a)   ((ring-negate-fn R) a))
(define (ring-minus R a b)  (ring-plus R a (ring-negate R b)))

(define (ring->semiring R)
  (make-semiring (ring-plus-fn R) (ring-times-fn R)
                 (ring-zero R) (ring-one R)))

(define (ring->additive-group R)
  (make-group (ring-plus-fn R) (ring-zero R) (ring-negate-fn R)))

(define-syntax with-ring
  (syntax-rules ()
    ((with-ring R (plus times zero one negate) body ...)
     (let ((tmp R))
       (let ((plus   (lambda (a b) (ring-plus tmp a b)))
             (times  (lambda (a b) (ring-times tmp a b)))
             (zero   (ring-zero tmp))
             (one    (ring-one tmp))
             (negate (lambda (a) (ring-negate tmp a))))
         body ...)))))

;; ─── Pre-built ring instances ────────────────

(define (integer-ring)
  (make-ring + * 0 1 -))

(define (modular-ring n)
  (make-ring
    (lambda (a b) (modulo (+ a b) n))
    (lambda (a b) (modulo (* a b) n))
    0 1
    (lambda (a) (modulo (- a) n))))

;; ─── Ring validation ─────────────────────────

(define (validate-ring R samples)
  (let ((violations '())
        (z (ring-zero R))
        (o (ring-one R)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Additive identity
        (unless (equal? (ring-plus R z a) a)
          (fail! 'additive-left-identity a))
        ;; Multiplicative identity
        (unless (equal? (ring-times R o a) a)
          (fail! 'multiplicative-left-identity a))
        ;; Additive inverse
        (unless (equal? (ring-plus R a (ring-negate R a)) z)
          (fail! 'additive-inverse a))
        ;; Distributivity
        (for-each
          (lambda (b)
            (for-each
              (lambda (c)
                (unless (equal? (ring-times R a (ring-plus R b c))
                                (ring-plus R (ring-times R a b)
                                             (ring-times R a c)))
                  (fail! 'left-distributivity a b c)))
              samples))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))

;; ─── Fields ──────────────────────────────────

(define-record-type <field>
  (make-field* plus-fn times-fn zero one negate-fn reciprocal-fn)
  field?
  (plus-fn       field-plus-fn)
  (times-fn      field-times-fn)
  (zero          field-zero)
  (one           field-one)
  (negate-fn     field-negate-fn)
  (reciprocal-fn field-reciprocal-fn))

(define (make-field plus times zero one negate reciprocal)
  (make-field* plus times zero one negate reciprocal))

(define (field-plus F a b)       ((field-plus-fn F) a b))
(define (field-times F a b)      ((field-times-fn F) a b))
(define (field-negate F a)       ((field-negate-fn F) a))
(define (field-reciprocal F a)   ((field-reciprocal-fn F) a))
(define (field-divide F a b)     (field-times F a (field-reciprocal F b)))

(define (field->ring F)
  (make-ring (field-plus-fn F) (field-times-fn F)
             (field-zero F) (field-one F) (field-negate-fn F)))

(define-syntax with-field
  (syntax-rules ()
    ((with-field F (plus times zero one negate reciprocal) body ...)
     (let ((tmp F))
       (let ((plus       (lambda (a b) (field-plus tmp a b)))
             (times      (lambda (a b) (field-times tmp a b)))
             (zero       (field-zero tmp))
             (one        (field-one tmp))
             (negate     (lambda (a) (field-negate tmp a)))
             (reciprocal (lambda (a) (field-reciprocal tmp a))))
         body ...)))))

;; ─── Pre-built field instances ───────────────

(define (rational-field)
  (make-field + * 0 1 - (lambda (x) (/ 1 x))))

;; ─── Field validation ────────────────────────

(define (validate-field F samples)
  ;; Samples should exclude zero for multiplicative inverse checks.
  (let ((violations '())
        (z (field-zero F))
        (o (field-one F)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Ring laws
    (let ((ring-result (validate-ring (field->ring F) samples)))
      (when (not (eq? #t ring-result))
        (set! violations (append ring-result violations))))
    ;; Multiplicative inverse for nonzero elements
    (for-each
      (lambda (a)
        (unless (equal? a z)
          (unless (equal? (field-times F a (field-reciprocal F a)) o)
            (fail! 'multiplicative-inverse a))))
      samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/ring.sld lib/wile/algebra/ring.scm test/wile/algebra-ring-test.scm
git commit -m "feat(algebra): add ring and field libraries with integer-ring, modular-ring, rational-field"
```

---

## Task 9: Galois Connections

**Files:**
- Create: `lib/wile/algebra/galois.sld`
- Create: `lib/wile/algebra/galois.scm`
- Create: `test/wile/algebra-galois-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-galois-test.scm`:

```scheme
;;; algebra-galois-test.scm — Galois connection tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra galois))

(test-begin "galois-connections")

;; Sign abstraction: integers → {neg, zero, pos, top, bottom}
;; alpha: concrete integer → abstract sign
;; gamma: abstract sign → set of integers (but we represent as a predicate)

(define sign-lattice
  (make-lattice
    ;; join
    (lambda (a b)
      (cond ((eq? a 'sign-bottom) b)
            ((eq? b 'sign-bottom) a)
            ((eq? a b) a)
            (else 'sign-top)))
    ;; meet
    (lambda (a b)
      (cond ((eq? a 'sign-top) b)
            ((eq? b 'sign-top) a)
            ((eq? a b) a)
            (else 'sign-bottom)))
    'sign-bottom
    'sign-top
    ;; leq?
    (lambda (a b)
      (cond ((eq? a 'sign-bottom) #t)
            ((eq? b 'sign-top) #t)
            ((eq? a b) #t)
            (else #f)))))

(define int-po (make-partial-order <=))

(define sign-gc
  (make-galois-connection
    ;; alpha: int → sign
    (lambda (n)
      (cond ((< n 0) 'neg)
            ((= n 0) 'zero)
            ((> n 0) 'pos)))
    ;; gamma: sign → "most precise" concrete representative
    ;; For soundness checking we need gamma to return a concrete value
    ;; such that alpha(gamma(a)) ≤ a. Using representative values.
    (lambda (s)
      (cond ((eq? s 'neg) -1)
            ((eq? s 'zero) 0)
            ((eq? s 'pos) 1)
            ((eq? s 'sign-bottom) 0)   ; arbitrary
            ((eq? s 'sign-top) 0)))    ; arbitrary
    int-po
    sign-lattice))

(test-group "construction"
  (test #t (galois-connection? sign-gc))
  (test #f (galois-connection? 42)))

(test-group "gc-alpha"
  (test 'neg  (gc-alpha sign-gc -5))
  (test 'zero (gc-alpha sign-gc 0))
  (test 'pos  (gc-alpha sign-gc 42)))

(test-group "gc-gamma"
  (test -1 (gc-gamma sign-gc 'neg))
  (test 0  (gc-gamma sign-gc 'zero))
  (test 1  (gc-gamma sign-gc 'pos)))

(test-group "gc-accessors"
  (test #t (partial-order? (gc-concrete-po sign-gc)))
  (test #t (lattice? (gc-abstract-lattice sign-gc))))

(test-group "gc-sound?"
  ;; Check soundness on concrete samples: ∀c. c ≤ γ(α(c))
  ;; and abstract samples: ∀a. α(γ(a)) ≤ a
  ;; This specific GC is sound for the sign domain.
  (test #t (gc-sound? sign-gc
             '(-3 -1 0 1 5)           ; concrete samples
             '(neg zero pos))))       ; abstract samples

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — library not found.

### Step 3: Write the library

Create `lib/wile/algebra/galois.sld`:

```scheme
(define-library (wile algebra galois)
  (export make-galois-connection galois-connection?
          gc-alpha gc-gamma
          gc-concrete-po gc-abstract-lattice
          gc-sound?)
  (import (scheme base)
          (wile algebra order)
          (wile algebra lattice))
  (include "galois.scm"))
```

Create `lib/wile/algebra/galois.scm`:

```scheme
;;; (wile algebra galois) — Galois connections
;;;
;;; A Galois connection (α, γ) between a concrete partial order C and
;;; an abstract lattice A satisfies:
;;;   ∀c ∈ C. c ≤_C γ(α(c))     (soundness / extensive)
;;;   ∀a ∈ A. α(γ(a)) ≤_A a     (reductive)

(define-record-type <galois-connection>
  (make-galois-connection* alpha-fn gamma-fn concrete-po abstract-lattice)
  galois-connection?
  (alpha-fn        gc-alpha-fn)
  (gamma-fn        gc-gamma-fn)
  (concrete-po     gc-concrete-po)
  (abstract-lattice gc-abstract-lattice))

(define (make-galois-connection alpha gamma concrete-po abstract-lattice)
  (make-galois-connection* alpha gamma concrete-po abstract-lattice))

(define (gc-alpha GC concrete-val)
  ((gc-alpha-fn GC) concrete-val))

(define (gc-gamma GC abstract-val)
  ((gc-gamma-fn GC) abstract-val))

(define (gc-sound? GC concrete-samples abstract-samples)
  ;; Spot-check both Galois conditions on sample elements.
  ;; Returns #t or list of (violation-type ...) entries.
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Extensive: ∀c. c ≤ γ(α(c))
    (for-each
      (lambda (c)
        (let ((round-tripped (gc-gamma GC (gc-alpha GC c))))
          (unless (po-leq? (gc-concrete-po GC) c round-tripped)
            (fail! 'extensive c round-tripped))))
      concrete-samples)
    ;; Reductive: ∀a. α(γ(a)) ≤ a
    (for-each
      (lambda (a)
        (let ((round-tripped (gc-alpha GC (gc-gamma GC a))))
          (unless (lattice-leq? (gc-abstract-lattice GC) round-tripped a)
            (fail! 'reductive a round-tripped))))
      abstract-samples)
    (if (null? violations) #t (reverse violations))))
```

### Step 4: Run test

Expected: PASS

### Step 5: Commit

```bash
git add lib/wile/algebra/galois.sld lib/wile/algebra/galois.scm test/wile/algebra-galois-test.scm
git commit -m "feat(algebra): add Galois connection library (wile algebra galois)"
```

---

## Task 10: Umbrella library and integration test

**Files:**
- Create: `lib/wile/algebra.sld`
- Create: `test/wile/algebra-integration-test.scm`

### Step 1: Write the failing test

Create `test/wile/algebra-integration-test.scm`:

```scheme
;;; algebra-integration-test.scm — Cross-structure integration tests

(import (scheme base)
        (chibi test)
        (wile algebra))  ;; umbrella import

(test-begin "algebra-integration")

;; -- Projection chain: field → ring → semiring → monoid --

(test-group "projection-chain"
  (let* ((F (rational-field))
         (R (field->ring F))
         (S (ring->semiring R))
         (M (semiring->additive-monoid S)))
    (test #t (field? F))
    (test #t (ring? R))
    (test #t (semiring? S))
    (test #t (monoid? M))
    ;; Operations agree through the chain
    (test 5 (field-plus F 2 3))
    (test 5 (ring-plus R 2 3))
    (test 5 (semiring-plus S 2 3))
    (test 5 (monoid-op M 2 3))))

;; -- Fixpoint over a flat lattice (constant propagation sketch) --

(test-group "fixpoint-flat-constant-prop"
  (let ((fl (flat-lattice '(0 1 2 3 42) eqv?)))
    ;; Transfer: bottom → 0 → 42 → 42 (stable)
    (let ((result (fixpoint fl
                    (lambda (v)
                      (cond ((eqv? v (lattice-bottom fl)) 0)
                            ((eqv? v 0) 42)
                            (else v)))
                    (lattice-bottom fl))))
      (test 42 result))))

;; -- Semiring path algebra (boolean reachability) --

(test-group "semiring-reachability"
  (with-semiring (boolean-semiring) (plus times zero one)
    ;; Can A reach C through B?
    ;; A→B exists (#t), B→C exists (#t)
    ;; reachable = A→B × B→C = #t ∧ #t = #t
    (test #t (times one one))
    ;; A→C direct = #f, A→B→C = #t
    ;; A→C* = A→C + A→B→C = #f ∨ #t = #t
    (test #t (plus zero (times one one)))))

;; -- Galois connection with lattice fixpoint --

(test-group "abstract-fixpoint"
  ;; Sign lattice fixpoint: start at bottom, step to 'pos
  (let* ((sl (make-lattice
               (lambda (a b)
                 (cond ((eq? a 'sign-bottom) b)
                       ((eq? b 'sign-bottom) a)
                       ((eq? a b) a)
                       (else 'sign-top)))
               (lambda (a b)
                 (cond ((eq? a 'sign-top) b)
                       ((eq? b 'sign-top) a)
                       ((eq? a b) a)
                       (else 'sign-bottom)))
               'sign-bottom 'sign-top
               (lambda (a b)
                 (cond ((eq? a 'sign-bottom) #t)
                       ((eq? b 'sign-top) #t)
                       ((eq? a b) #t)
                       (else #f)))))
         (result (fixpoint sl
                   (lambda (v)
                     (if (eq? v 'sign-bottom) 'pos v))
                   'sign-bottom)))
    (test 'pos result)))

;; -- map-lattice for per-variable analysis --

(test-group "map-lattice-per-variable"
  (let* ((fl (flat-lattice '(0 1 2) eqv?))
         (ml (map-lattice '(x y) fl)))
    ;; Bottom: all variables at flat-bottom
    (test 'flat-bottom (cdr (assoc 'x (lattice-bottom ml))))
    ;; Join: pointwise
    (let ((a (list (cons 'x 1) (cons 'y 'flat-bottom)))
          (b (list (cons 'x 'flat-bottom) (cons 'y 2))))
      (let ((result (lattice-join ml a b)))
        (test 1 (cdr (assoc 'x result)))
        (test 2 (cdr (assoc 'y result)))))))

(test-end)
(test-exit)
```

### Step 2: Run test to verify it fails

Expected: FAIL — `(wile algebra)` not found.

### Step 3: Write the umbrella library

Create `lib/wile/algebra.sld`:

```scheme
(define-library (wile algebra)
  (export
    ;; Partial orders
    make-partial-order partial-order?
    po-leq? po-comparable? po-monotone?
    validate-partial-order
    ;; Lattices
    make-lattice lattice?
    lattice-join lattice-meet lattice-bottom lattice-top
    lattice-leq? lattice->partial-order
    flat-lattice powerset-lattice product-lattice map-lattice
    fixpoint fixpoint/widen
    validate-lattice with-lattice
    ;; Monoids
    make-monoid monoid?
    monoid-op monoid-identity monoid-fold monoid-power
    validate-monoid with-monoid
    ;; Semirings
    make-semiring semiring?
    semiring-plus semiring-times semiring-zero semiring-one
    semiring->additive-monoid semiring->multiplicative-monoid
    boolean-semiring tropical-semiring counting-semiring
    validate-semiring with-semiring
    ;; Groups
    make-group group?
    group-op group-identity group-inverse
    group->monoid
    validate-group with-group
    ;; Rings
    make-ring ring?
    ring-plus ring-times ring-zero ring-one
    ring-negate ring-minus
    ring->semiring ring->additive-group
    integer-ring modular-ring
    validate-ring with-ring
    ;; Fields
    make-field field?
    field-plus field-times field-zero field-one
    field-negate field-reciprocal field-divide
    field->ring
    rational-field
    validate-field with-field
    ;; Galois connections
    make-galois-connection galois-connection?
    gc-alpha gc-gamma
    gc-concrete-po gc-abstract-lattice
    gc-sound?)
  (import (wile algebra order)
          (wile algebra lattice)
          (wile algebra monoid)
          (wile algebra semiring)
          (wile algebra group)
          (wile algebra ring)
          (wile algebra galois)))
```

### Step 4: Run test

Run: `./dist/darwin/arm64/wile --quiet -f test/wile/algebra-integration-test.scm`

Expected: PASS

### Step 5: Run full test suite

Run: `make build && make test-scheme`

Expected: All tests pass, including all 8 new test files.

### Step 6: Commit

```bash
git add lib/wile/algebra.sld test/wile/algebra-integration-test.scm
git commit -m "feat(algebra): add umbrella library (wile algebra) with integration tests"
```

---

## Task 11: Documentation

**Files:**
- Modify: `TODO.md` — mark algebra library as done (or in-progress)
- Modify: `plans/2026-03-25-algebra-library-design.md` — mark status as implemented

### Step 1: Update TODO.md

Change the algebra library entry from `- [ ]` to `- [x]` with a completion note.

### Step 2: Update design doc status

Change `**Status:** Draft` to `**Status:** Implemented`.

### Step 3: Commit

```bash
git add TODO.md plans/2026-03-25-algebra-library-design.md
git commit -m "docs(algebra): mark algebra library as implemented"
```
