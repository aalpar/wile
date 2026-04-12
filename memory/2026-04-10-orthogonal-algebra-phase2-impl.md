# Orthogonal Algebra Phase 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add four orthogonal algebraic types to `(wile algebra)`: Setoid, Category, Closure Operator, Differential Ring.

**Architecture:** Each type follows the established pattern: flat R7RS record storing closures, public constructor wrapping raw `make-*`, operation wrappers, `validate-*` checking axioms, `with-*` destructuring macro, pre-built instances. Each is a separate sub-library under `(wile algebra <name>)`, added to the umbrella `(wile algebra)`.

**Tech Stack:** R7RS Scheme. Tests use `(chibi test)`. Build via `make build && make test-scheme`.

**Design doc:** `plans/2026-04-10-orthogonal-algebra-phase2-design.md`

---

## Conventions (read before implementing)

Study these files to understand the exact patterns to follow:

- **Record + constructor:** `stdlib/lib/wile/algebra/monoid.scm` (simplest example)
- **Validation delegation:** `stdlib/lib/wile/algebra/heyting.scm` (calls `validate-lattice` then adds own checks)
- **Projection functions:** `stdlib/lib/wile/algebra/boolean.scm` (`boolean->heyting`, `boolean->ring`)
- **`.sld` format:** `stdlib/lib/wile/algebra/boolean.sld` (imports, exports, include)
- **Test format:** `test/wile/algebra-heyting-test.scm` (chibi test, test-group, test-begin/end/exit)
- **Umbrella library:** `stdlib/lib/wile/algebra.sld` (all sub-library exports)
- **Integration tests:** `test/wile/algebra-integration-test.scm`

**Docstring format:** Every public procedure gets a Guile-style docstring with `Examples:`, `Parameters:`, `Returns:`, `Category: algebra`, `Keywords:`, `See also:`. Match the style in `monoid.scm` or `heyting.scm` exactly.

**Equality in validation:** Use `equal?` for element comparison in validation (as `validate-monoid`, `validate-ring` do). The Setoid itself provides custom equivalence — but validation of *other* types uses Scheme's `equal?` for simplicity, matching existing convention.

---

## Task 1: Setoid — Library Definition

**Files:**
- Create: `stdlib/lib/wile/algebra/setoid.sld`
- Create: `stdlib/lib/wile/algebra/setoid.scm`

**Step 1: Create the library definition**

Create `stdlib/lib/wile/algebra/setoid.sld`:

```scheme
(define-library (wile algebra setoid)
  (description "Setoids: sets with explicit equivalence relations.")
  (export make-setoid setoid?
          setoid-equiv?
          default-setoid numeric-setoid string-setoid eqv-setoid
          setoid-equivalence-class
          validate-setoid
          with-setoid)
  (import (scheme base))
  (include "setoid.scm"))
```

**Step 2: Create the implementation**

Create `stdlib/lib/wile/algebra/setoid.scm`:

```scheme
;;; (wile algebra setoid) — Setoids
;;;
;;; A setoid is a set equipped with an explicit equivalence relation:
;;; a binary predicate equiv? satisfying reflexivity, symmetry, and
;;; transitivity.  Setoids make equality a first-class parameter,
;;; enabling quotient structures and custom equivalence.

;; ─── Record type ─────────────────────────────

(define-record-type <setoid>
  (make-setoid* equiv-fn)
  setoid?
  (equiv-fn setoid-equiv-fn))

(define (make-setoid equiv?)
  "Construct a setoid from an equivalence predicate EQUIV?.\nA setoid is a set equipped with an explicit equivalence relation\nsatisfying reflexivity (a ~ a), symmetry (a ~ b implies b ~ a),\nand transitivity (a ~ b and b ~ c implies a ~ c).\n\nExamples:\n  (setoid-equiv? (make-setoid equal?) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (make-setoid =) 3 3)                  => #t\n\nParameters:\n  equiv? : procedure\nReturns: any\nCategory: algebra\nKeywords: setoid, equivalence, equality, quotient, congruence, relation\n\nSee also: `default-setoid', `validate-setoid'."
  (make-setoid* equiv?))

;; ─── Core operation ──────────────────────────

(define (setoid-equiv? S a b)
  "Test whether A and B are equivalent under setoid S.\n\nExamples:\n  (setoid-equiv? (numeric-setoid) 1 1)     => #t\n  (setoid-equiv? (numeric-setoid) 1 2)     => #f\n  (setoid-equiv? (default-setoid) '(a) '(a))  => #t\n\nParameters:\n  S : any\n  a : any\n  b : any\nReturns: boolean\nCategory: algebra\nKeywords: equivalence, equal, equivalent, congruent, identify\n\nSee also: `make-setoid', `setoid-equivalence-class'."
  ((setoid-equiv-fn S) a b))

;; ─── Pre-built instances ─────────────────────

(define (default-setoid)
  "Construct a setoid using Scheme's equal? as the equivalence relation.\n\nExamples:\n  (setoid-equiv? (default-setoid) '(1 2) '(1 2))  => #t\n  (setoid-equiv? (default-setoid) 'a 'b)           => #f\n\nReturns: any\nCategory: algebra\nKeywords: equal, structural equality, default\n\nSee also: `numeric-setoid', `eqv-setoid'."
  (make-setoid equal?))

(define (numeric-setoid)
  "Construct a setoid using Scheme's = as the equivalence relation.\nOnly valid for numeric elements.\n\nExamples:\n  (setoid-equiv? (numeric-setoid) 1 1.0)    => #t\n  (setoid-equiv? (numeric-setoid) 1/2 0.5)  => #t\n\nReturns: any\nCategory: algebra\nKeywords: numeric, number, mathematical equality\n\nSee also: `default-setoid', `string-setoid'."
  (make-setoid =))

(define (string-setoid)
  "Construct a setoid using string=? as the equivalence relation.\nOnly valid for string elements.\n\nExamples:\n  (setoid-equiv? (string-setoid) \"abc\" \"abc\")  => #t\n  (setoid-equiv? (string-setoid) \"a\" \"b\")      => #f\n\nReturns: any\nCategory: algebra\nKeywords: string, text, string equality\n\nSee also: `default-setoid', `numeric-setoid'."
  (make-setoid string=?))

(define (eqv-setoid)
  "Construct a setoid using Scheme's eqv? as the equivalence relation.\nUses identity/value equality — same object or same simple value.\n\nExamples:\n  (setoid-equiv? (eqv-setoid) 'a 'a)  => #t\n  (setoid-equiv? (eqv-setoid) 1 1)    => #t\n\nReturns: any\nCategory: algebra\nKeywords: eqv, identity, pointer equality, value equality\n\nSee also: `default-setoid'."
  (make-setoid eqv?))

;; ─── Derived operations ──────────────────────

(define (setoid-equivalence-class S element samples)
  "Return all elements in SAMPLES equivalent to ELEMENT under setoid S.\nIncludes ELEMENT itself if it appears in SAMPLES.\n\nExamples:\n  (setoid-equivalence-class (numeric-setoid) 1 '(1 2 1 3))  => (1 1)\n  (setoid-equivalence-class (default-setoid) 'a '(a b a c))  => (a a)\n\nParameters:\n  S : any\n  element : any\n  samples : list\nReturns: list\nCategory: algebra\nKeywords: equivalence class, partition, quotient, orbit, fiber\n\nSee also: `setoid-equiv?'."
  (let loop ((xs samples) (acc '()))
    (cond ((null? xs) (reverse acc))
          ((setoid-equiv? S element (car xs))
           (loop (cdr xs) (cons (car xs) acc)))
          (else (loop (cdr xs) acc)))))

;; ─── Validation ──────────────────────────────

(define (validate-setoid S samples)
  "Spot-check that S satisfies the equivalence relation laws on SAMPLES.\nTests reflexivity (a ~ a), symmetry (a ~ b implies b ~ a), and\ntransitivity (a ~ b and b ~ c implies a ~ c) for elements and\ntriples in SAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (validate-setoid (default-setoid) '(1 2 3))  => #t\n\nParameters:\n  S : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: reflexivity, symmetry, transitivity, law checking, validation\n\nSee also: `make-setoid', `setoid-equiv?'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        ;; Reflexivity: a ~ a
        (unless (setoid-equiv? S a a)
          (fail! 'reflexivity a))
        ;; Symmetry: a ~ b implies b ~ a
        (for-each
          (lambda (b)
            (when (and (setoid-equiv? S a b)
                       (not (setoid-equiv? S b a)))
              (fail! 'symmetry a b)))
          samples)
        ;; Transitivity: a ~ b and b ~ c implies a ~ c
        (for-each
          (lambda (b)
            (when (setoid-equiv? S a b)
              (for-each
                (lambda (c)
                  (when (and (setoid-equiv? S b c)
                             (not (setoid-equiv? S a c)))
                    (fail! 'transitivity a b c)))
                samples)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))

;; ─── with-setoid macro ──────────────────────

(define-syntax with-setoid
  (syntax-rules ()
    ((with-setoid S (equiv?) body ...)
     (let ((tmp S))
       (let ((equiv? (lambda (a b) (setoid-equiv? tmp a b))))
         body ...)))))
```

**Step 3: Build and verify it compiles**

Run: `make build`
Expected: success

**Step 4: Commit**

```
feat: add (wile algebra setoid) library
```

---

## Task 2: Setoid — Tests

**Files:**
- Create: `test/wile/algebra-setoid-test.scm`

**Step 1: Write the test file**

Create `test/wile/algebra-setoid-test.scm`:

```scheme
;;; algebra-setoid-test.scm — Setoid tests

(import (scheme base)
        (chibi test)
        (wile algebra setoid))

(test-begin "setoids")

;; ─── Construction ────────────────────────────

(test-group "construction"
  (test #t (setoid? (default-setoid)))
  (test #t (setoid? (numeric-setoid)))
  (test #t (setoid? (string-setoid)))
  (test #t (setoid? (eqv-setoid)))
  (test #f (setoid? 42))
  (test #f (setoid? "not a setoid")))

;; ─── default-setoid ──────────────────────────

(test-group "default-setoid"
  (let ((S (default-setoid)))
    (test #t (setoid-equiv? S '(1 2) '(1 2)))
    (test #f (setoid-equiv? S '(1 2) '(2 1)))
    (test #t (setoid-equiv? S 'a 'a))
    (test #f (setoid-equiv? S 'a 'b))
    (test #t (setoid-equiv? S 42 42))
    (test #f (setoid-equiv? S 42 43))))

;; ─── numeric-setoid ──────────────────────────

(test-group "numeric-setoid"
  (let ((S (numeric-setoid)))
    (test #t (setoid-equiv? S 1 1))
    (test #f (setoid-equiv? S 1 2))
    (test #t (setoid-equiv? S 1 1.0))
    (test #t (setoid-equiv? S 1/2 0.5))))

;; ─── string-setoid ───────────────────────────

(test-group "string-setoid"
  (let ((S (string-setoid)))
    (test #t (setoid-equiv? S "abc" "abc"))
    (test #f (setoid-equiv? S "abc" "def"))))

;; ─── eqv-setoid ──────────────────────────────

(test-group "eqv-setoid"
  (let ((S (eqv-setoid)))
    (test #t (setoid-equiv? S 'a 'a))
    (test #f (setoid-equiv? S 'a 'b))
    (test #t (setoid-equiv? S 1 1))))

;; ─── Custom setoid (modular arithmetic) ──────

(test-group "custom-setoid"
  ;; Integers mod 3
  (let ((S (make-setoid (lambda (a b) (= (modulo a 3) (modulo b 3))))))
    (test #t (setoid-equiv? S 1 4))
    (test #t (setoid-equiv? S 2 5))
    (test #f (setoid-equiv? S 1 2))
    (test #t (setoid-equiv? S 0 9))))

;; ─── equivalence-class ───────────────────────

(test-group "equivalence-class"
  (let ((S (make-setoid (lambda (a b) (= (modulo a 3) (modulo b 3))))))
    ;; class of 1 in (0 1 2 3 4 5 6): {1, 4}
    (test '(1 4) (setoid-equivalence-class S 1 '(0 1 2 3 4 5 6)))
    ;; class of 0: {0, 3, 6}
    (test '(0 3 6) (setoid-equivalence-class S 0 '(0 1 2 3 4 5 6))))
  ;; Empty samples
  (test '() (setoid-equivalence-class (default-setoid) 'a '())))

;; ─── validate-setoid ─────────────────────────

(test-group "validate-setoid"
  ;; Valid: default-setoid
  (test #t (validate-setoid (default-setoid) '(1 2 3)))
  ;; Valid: numeric-setoid
  (test #t (validate-setoid (numeric-setoid) '(1 2 3)))
  ;; Valid: mod-3 setoid
  (test #t (validate-setoid
             (make-setoid (lambda (a b) (= (modulo a 3) (modulo b 3))))
             '(0 1 2 3 4 5)))
  ;; Invalid: non-reflexive
  (let ((bad (make-setoid (lambda (a b) #f))))
    (let ((result (validate-setoid bad '(1 2))))
      (test #f (eq? #t result))
      (test #t (pair? result))
      (test 'reflexivity (caar result))))
  ;; Invalid: non-symmetric
  (let ((bad (make-setoid (lambda (a b) (<= a b)))))
    (let ((result (validate-setoid bad '(1 2))))
      (test #f (eq? #t result))
      ;; Should catch symmetry violation: 1<=2 but not 2<=1
      (test #t (pair? result)))))

;; ─── with-setoid macro ──────────────────────

(test-group "with-setoid"
  (with-setoid (default-setoid) (equiv?)
    (test #t (equiv? 'a 'a))
    (test #f (equiv? 'a 'b))))

(test-end)
(test-exit)
```

**Step 2: Run the test**

Run: `make build && SCHEME=./dist/darwin/arm64/wile ./test/run-all.sh 2>&1 | grep -A2 setoid`

Or run the single test: `./dist/darwin/arm64/wile test/wile/algebra-setoid-test.scm`

Expected: all tests pass

**Step 3: Commit**

```
test: add setoid test suite
```

---

## Task 3: Setoid — Partial Order Integration

**Files:**
- Modify: `stdlib/lib/wile/algebra/order.scm` — add `validate-partial-order/setoid`
- Modify: `stdlib/lib/wile/algebra/order.sld` — add export and import

**Step 1: Update the .sld to import setoid and export the new function**

In `stdlib/lib/wile/algebra/order.sld`, add `validate-partial-order/setoid` to exports
and add `(wile algebra setoid)` to imports.

**Step 2: Add `validate-partial-order/setoid` to `order.scm`**

Append after `validate-partial-order`:

```scheme
(define (validate-partial-order/setoid po setoid samples)
  "Spot-check partial order PO laws on SAMPLES, including antisymmetry via SETOID.\nExtends validate-partial-order by also checking antisymmetry:\nif a <= b and b <= a, then a and b must be equivalent under SETOID.\nThis is the check that validate-partial-order cannot perform without\nan explicit equality predicate.\n\nExamples:\n  (validate-partial-order/setoid\n    (make-partial-order <=) (numeric-setoid) '(1 2 3))  => #t\n\nParameters:\n  po : any\n  setoid : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: antisymmetry, partial order, equivalence, validation, law checking\n\nSee also: `validate-partial-order', `make-setoid'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Delegate existing checks
    (let ((base-result (validate-partial-order po samples)))
      (when (not (eq? #t base-result))
        (set! violations (append base-result violations))))
    ;; Antisymmetry: a ≤ b ∧ b ≤ a ⟹ equiv?(a, b)
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (when (and (po-leq? po a b)
                       (po-leq? po b a)
                       (not (setoid-equiv? setoid a b)))
              (fail! 'antisymmetry a b)))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))
```

**Step 3: Run tests**

Run: `make build && ./dist/darwin/arm64/wile test/wile/algebra-order-test.scm`
Expected: existing tests still pass (no behavioral change)

**Step 4: Add antisymmetry test to `test/wile/algebra-order-test.scm`**

Add a test-group for the new function:

```scheme
(test-group "validate-partial-order/setoid"
  ;; Valid: numeric order with numeric equality
  (test #t (validate-partial-order/setoid
             (make-partial-order <=) (numeric-setoid) '(1 2 3)))
  ;; Invalid: <= is not antisymmetric under eqv? for 1 and 1.0
  ;; because (<= 1 1.0) and (<= 1.0 1) but (eqv? 1 1.0) is #f
  (let ((result (validate-partial-order/setoid
                  (make-partial-order <=) (eqv-setoid) '(1 1.0))))
    (test #f (eq? #t result))
    (test 'antisymmetry (caar result))))
```

Also add `(wile algebra setoid)` to the import list of the test file.

**Step 5: Run and verify**

Run: `./dist/darwin/arm64/wile test/wile/algebra-order-test.scm`
Expected: all tests pass

**Step 6: Commit**

```
feat: add validate-partial-order/setoid — antisymmetry via explicit equality
```

---

## Task 4: Category — Library Definition

**Files:**
- Create: `stdlib/lib/wile/algebra/category.sld`
- Create: `stdlib/lib/wile/algebra/category.scm`

**Step 1: Create the library definition**

Create `stdlib/lib/wile/algebra/category.sld`:

```scheme
(define-library (wile algebra category)
  (description "Categories: morphism composition with identity and associativity.")
  (export make-category category?
          category-compose category-identity category-equiv?
          category->endomorphism-monoid
          procedure-category
          validate-category
          with-category)
  (import (scheme base)
          (wile algebra monoid))
  (include "category.scm"))
```

**Step 2: Create the implementation**

Create `stdlib/lib/wile/algebra/category.scm`:

```scheme
;;; (wile algebra category) — Categories
;;;
;;; A category consists of morphisms (arrows) with an associative
;;; composition operation and an identity morphism for each object.
;;; Composition: if f : A→B and g : B→C, then (compose g f) : A→C.
;;; Identity: id_A : A→A such that f ∘ id = f and id ∘ f = f.

;; ─── Record type ─────────────────────────────

(define-record-type <category>
  (make-category* compose-fn identity-fn equiv-fn)
  category?
  (compose-fn  category-compose-fn)
  (identity-fn category-identity-fn)
  (equiv-fn    category-equiv-fn))

(define (make-category compose identity equiv?)
  "Construct a category from COMPOSE, IDENTITY, and EQUIV?.\nCOMPOSE takes two morphisms (f, g) and returns f . g (apply g\nfirst, then f — standard mathematical convention). IDENTITY\ntakes an object and returns its identity morphism. EQUIV?\ntests morphism equality.\n\nExamples:\n  (let ((C (procedure-category)))\n    (category-equiv? C\n      (category-compose C (lambda (x) (* x 2)) (lambda (x) (+ x 1)))\n      (lambda (x) (* (+ x 1) 2))))\n\nParameters:\n  compose : procedure\n  identity : procedure\n  equiv? : procedure\nReturns: any\nCategory: algebra\nKeywords: category, morphism, composition, arrow, functor, identity\n\nSee also: `procedure-category', `validate-category'."
  (make-category* compose identity equiv?))

;; ─── Core operations ─────────────────────────

(define (category-compose C f g)
  "Compose morphisms F and G in category C.\nReturns F . G (apply G first, then F). This follows the\nstandard mathematical convention and matches Scheme's compose.\n\nExamples:\n  (let* ((C (procedure-category))\n         (f (lambda (x) (* x 2)))\n         (g (lambda (x) (+ x 1)))\n         (fg (category-compose C f g)))\n    (fg 3))  => 8\n\nParameters:\n  C : any\n  f : any\n  g : any\nReturns: any\nCategory: algebra\nKeywords: composition, compose, sequential, pipeline, chain\n\nSee also: `category-identity', `category-equiv?'."
  ((category-compose-fn C) f g))

(define (category-identity C obj)
  "Return the identity morphism on OBJ in category C.\nThe identity morphism satisfies: f . id = f and id . f = f\nfor all composable morphisms f.\n\nExamples:\n  (let* ((C (procedure-category))\n         (id (category-identity C 'any)))\n    (id 42))  => 42\n\nParameters:\n  C : any\n  obj : any\nReturns: any\nCategory: algebra\nKeywords: identity, neutral, unit, id\n\nSee also: `category-compose'."
  ((category-identity-fn C) obj))

(define (category-equiv? C f g)
  "Test whether morphisms F and G are equivalent in category C.\n\nExamples:\n  (let ((C (procedure-category)))\n    (category-equiv? C (lambda (x) x) (lambda (x) x)))\n\nParameters:\n  C : any\n  f : any\n  g : any\nReturns: boolean\nCategory: algebra\nKeywords: morphism equality, equivalent, equal arrows"
  ((category-equiv-fn C) f g))

;; ─── Projection ──────────────────────────────

(define (category->endomorphism-monoid C obj)
  "Extract the endomorphism monoid on OBJ from category C.\nEndomorphisms are morphisms from OBJ to itself. They form a\nmonoid under composition with the identity morphism as the\nneutral element. This makes explicit the relationship: a\nmonoid is a category with one object.\n\nExamples:\n  (let* ((C (procedure-category))\n         (M (category->endomorphism-monoid C 'any)))\n    (monoid-op M (lambda (x) (* x 2)) (lambda (x) (+ x 1))))\n\nParameters:\n  C : any\n  obj : any\nReturns: any\nCategory: algebra\nKeywords: endomorphism, monoid, forgetful, one-object category\n\nSee also: `make-monoid', `category-compose'."
  (make-monoid
    (lambda (f g) (category-compose C f g))
    (category-identity C obj)))

;; ─── Pre-built instances ─────────────────────

(define (procedure-category)
  "Construct the category of Scheme procedures.\nMorphisms are procedures, composition is function composition\n(apply second argument first), and identity is the identity\nfunction. Morphism equality uses equal?.\n\nNote: extensional equality of procedures is undecidable in\ngeneral. The equal? test works for referential identity but\nnot for functionally equivalent but syntactically different\nprocedures.\n\nExamples:\n  (let* ((C (procedure-category))\n         (f (lambda (x) (* x 2)))\n         (g (lambda (x) (+ x 1)))\n         (fg (category-compose C f g)))\n    (fg 5))  => 12\n\nReturns: any\nCategory: algebra\nKeywords: function, procedure, Scheme, lambda, endofunction\n\nSee also: `make-category', `category->endomorphism-monoid'."
  (make-category
    (lambda (f g) (lambda (x) (f (g x))))
    (lambda (obj) (lambda (x) x))
    equal?))

;; ─── Validation ──────────────────────────────

(define (validate-category C morphism-triples identity-morphisms)
  "Spot-check that C satisfies the category laws.\nMORPHISM-TRIPLES is a list of (f g h) triples for checking\nassociativity: (f . g) . h = f . (g . h).\nIDENTITY-MORPHISMS is a list of (id f domain-id) triples for\nchecking identity laws: id . f = f and f . domain-id = f.\nReturns #t if all laws hold, or a list of (violation-type ...)\nentries describing failures.\n\nExamples:\n  (let* ((C (procedure-category))\n         (f (lambda (x) (* x 2)))\n         (g (lambda (x) (+ x 1)))\n         (h (lambda (x) (* x 3)))\n         (id (category-identity C 'any)))\n    (validate-category C\n      (list (list f g h))\n      (list (list id f id))))\n\nParameters:\n  C : any\n  morphism-triples : list\n  identity-morphisms : list\nReturns: any\nCategory: algebra\nKeywords: associativity, identity law, validation, law checking\n\nSee also: `make-category', `category-compose', `category-identity'."
  (let ((violations '()))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Associativity: (f . g) . h = f . (g . h)
    (for-each
      (lambda (triple)
        (let ((f (car triple))
              (g (cadr triple))
              (h (caddr triple)))
          (unless (category-equiv? C
                    (category-compose C (category-compose C f g) h)
                    (category-compose C f (category-compose C g h)))
            (fail! 'associativity f g h))))
      morphism-triples)
    ;; Identity laws: id . f = f and f . domain-id = f
    (for-each
      (lambda (triple)
        (let ((id (car triple))
              (f (cadr triple))
              (dom-id (caddr triple)))
          (unless (category-equiv? C (category-compose C id f) f)
            (fail! 'left-identity id f))
          (unless (category-equiv? C (category-compose C f dom-id) f)
            (fail! 'right-identity f dom-id))))
      identity-morphisms)
    (if (null? violations) #t (reverse violations))))

;; ─── with-category macro ────────────────────

(define-syntax with-category
  (syntax-rules ()
    ((with-category C (compose identity equiv?) body ...)
     (let ((tmp C))
       (let ((compose  (lambda (f g) (category-compose tmp f g)))
             (identity (lambda (obj) (category-identity tmp obj)))
             (equiv?   (lambda (f g) (category-equiv? tmp f g))))
         body ...)))))
```

**Step 3: Build**

Run: `make build`
Expected: success

**Step 4: Commit**

```
feat: add (wile algebra category) library
```

---

## Task 5: Category — Tests

**Files:**
- Create: `test/wile/algebra-category-test.scm`

**Step 1: Write the test file**

Create `test/wile/algebra-category-test.scm`:

```scheme
;;; algebra-category-test.scm — Category tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra category))

(test-begin "categories")

;; ─── Construction ────────────────────────────

(test-group "construction"
  (test #t (category? (procedure-category)))
  (test #f (category? 42))
  (test #f (category? (make-monoid + 0))))

;; ─── procedure-category basics ───────────────

(define C (procedure-category))
(define (double x) (* x 2))
(define (inc x) (+ x 1))
(define (triple x) (* x 3))

(test-group "procedure-category-compose"
  ;; f . g: apply g first, then f
  (let ((fg (category-compose C double inc)))
    (test 8 (fg 3))     ;; (3+1)*2 = 8
    (test 2 (fg 0)))    ;; (0+1)*2 = 2
  ;; g . f: apply f first, then g
  (let ((gf (category-compose C inc double)))
    (test 7 (gf 3))     ;; 3*2+1 = 7
    (test 1 (gf 0))))   ;; 0*2+1 = 1

(test-group "procedure-category-identity"
  (let ((id (category-identity C 'any)))
    (test 42 (id 42))
    (test 'a (id 'a))
    (test '() (id '()))))

;; ─── Associativity ───────────────────────────

(test-group "associativity"
  ;; (f . g) . h vs f . (g . h)
  (let ((fg-h (category-compose C (category-compose C double inc) triple))
        (f-gh (category-compose C double (category-compose C inc triple))))
    ;; Both should compute double(inc(triple(x)))
    (test (fg-h 2) (f-gh 2))   ;; double(inc(triple(2))) = double(inc(6)) = double(7) = 14
    (test 14 (fg-h 2))
    (test (fg-h 5) (f-gh 5))))

;; ─── Identity laws ───────────────────────────

(test-group "identity-laws"
  (let ((id (category-identity C 'any)))
    ;; id . f = f
    (let ((id-f (category-compose C id double)))
      (test 6 (id-f 3))
      (test 10 (id-f 5)))
    ;; f . id = f
    (let ((f-id (category-compose C double id)))
      (test 6 (f-id 3))
      (test 10 (f-id 5)))))

;; ─── Endomorphism monoid ─────────────────────

(test-group "endomorphism-monoid"
  (let ((M (category->endomorphism-monoid C 'any)))
    (test #t (monoid? M))
    ;; op = compose
    (let ((fg (monoid-op M double inc)))
      (test 8 (fg 3)))
    ;; identity = id function
    (let ((id (monoid-identity M)))
      (test 42 (id 42)))
    ;; fold = compose chain
    (let ((chain (monoid-fold M (list inc inc inc))))
      ;; +1 three times
      (test 5 (chain 2)))))

;; ─── validate-category ──────────────────────

(test-group "validate-category"
  ;; procedure-category: test with concrete integer morphisms
  ;; We need equiv? to work, so we test using a category of
  ;; integer endomorphisms where we can compare via equal?
  (let* ((int-cat (make-category
                    (lambda (f g)
                      ;; f and g are alists representing functions on {0,1,2}
                      (map (lambda (pair)
                             (let ((x (car pair))
                                   (gx (cdr pair)))
                               (let ((fgx (cdr (assv gx f))))
                                 (cons x fgx))))
                           g))
                    (lambda (obj)
                      ;; identity on {0,1,2}
                      '((0 . 0) (1 . 1) (2 . 2)))
                    equal?))
         (id '((0 . 0) (1 . 1) (2 . 2)))
         (f  '((0 . 1) (1 . 2) (2 . 0)))   ;; rotate
         (g  '((0 . 2) (1 . 0) (2 . 1)))   ;; rotate other way
         (h  '((0 . 0) (1 . 0) (2 . 0))))  ;; constant 0
    (test #t (validate-category int-cat
               (list (list f g h))
               (list (list id f id))))))

;; ─── with-category macro ────────────────────

(test-group "with-category"
  (with-category C (compose identity equiv?)
    (let ((fg (compose double inc)))
      (test 8 (fg 3)))
    (let ((id (identity 'any)))
      (test 42 (id 42)))))

(test-end)
(test-exit)
```

**Step 2: Run the test**

Run: `make build && ./dist/darwin/arm64/wile test/wile/algebra-category-test.scm`
Expected: all tests pass

**Step 3: Commit**

```
test: add category test suite
```

---

## Task 6: Closure Operator — Library Definition

**Files:**
- Create: `stdlib/lib/wile/algebra/closure.sld`
- Create: `stdlib/lib/wile/algebra/closure.scm`

**Step 1: Create the library definition**

Create `stdlib/lib/wile/algebra/closure.sld`:

```scheme
(define-library (wile algebra closure)
  (description "Closure operators: extensive, monotone, idempotent functions on lattices.")
  (export make-closure-operator closure-operator?
          closure-close closure-closed? closure-lattice
          closed-elements
          closure->closed-lattice
          downward-closure-operator
          validate-closure-operator
          with-closure)
  (import (scheme base)
          (wile algebra lattice)
          (wile algebra order))
  (include "closure.scm"))
```

**Step 2: Create the implementation**

Create `stdlib/lib/wile/algebra/closure.scm`:

```scheme
;;; (wile algebra closure) — Closure operators
;;;
;;; A closure operator on a lattice L is a function cl : L → L that
;;; is extensive (a ≤ cl(a)), monotone (a ≤ b implies cl(a) ≤ cl(b)),
;;; and idempotent (cl(cl(a)) = cl(a)). The fixed points of cl form
;;; a sublattice of L — the lattice of "closed" elements.

;; ─── Record type ─────────────────────────────

(define-record-type <closure-operator>
  (make-closure-operator* close-fn lattice)
  closure-operator?
  (close-fn closure-close-fn)
  (lattice   closure-lattice))

(define (make-closure-operator close lattice)
  "Construct a closure operator CLOSE on LATTICE.\nCLOSE must be extensive (a <= cl(a)), monotone\n(a <= b implies cl(a) <= cl(b)), and idempotent\n(cl(cl(a)) = cl(a)). The fixed points of CLOSE\nform the lattice of closed elements.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L)))\n    (closure-close C '(1)))  => (1 2 3)\n\nParameters:\n  close : procedure\n  lattice : any\nReturns: any\nCategory: algebra\nKeywords: closure, closure operator, hull, saturation, fixed point, topology\n\nSee also: `closure-close', `closure-closed?', `validate-closure-operator'."
  (make-closure-operator* close lattice))

;; ─── Core operations ─────────────────────────

(define (closure-close C a)
  "Apply closure operator C to element A.\nReturns cl(a), which is always >= a in the underlying lattice.\nApplying cl twice gives the same result: cl(cl(a)) = cl(a).\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L)))\n    (closure-close C '(1)))   => (1 2 3)\n    (closure-close C '(2)))   => (2)\n\nParameters:\n  C : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: close, apply, hull, closure, saturate\n\nSee also: `closure-closed?', `closure-lattice'."
  ((closure-close-fn C) a))

(define (closure-closed? C a)
  "Test whether A is a fixed point of closure operator C.\nReturns #t when cl(a) = a (using lattice equality).\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L)))\n    (closure-closed? C '(1 2 3))  => #t\n    (closure-closed? C '(1))      => #f\n    (closure-closed? C '(2))      => #t\n\nParameters:\n  C : any\n  a : any\nReturns: boolean\nCategory: algebra\nKeywords: fixed point, closed, stable, invariant\n\nSee also: `closure-close', `closed-elements'."
  (let ((L (closure-lattice C)))
    (lattice-equal? L (closure-close C a) a)))

;; ─── Derived operations ──────────────────────

(define (closed-elements C samples)
  "Filter SAMPLES to only those elements that are fixed points of C.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L)))\n    (closed-elements C '(() (1) (2) (1 2 3))))\n    ;; => (() (2) (1 2 3))  — (1) is not closed\n\nParameters:\n  C : any\n  samples : list\nReturns: list\nCategory: algebra\nKeywords: fixed points, closed set, filter, stable elements\n\nSee also: `closure-closed?', `closure->closed-lattice'."
  (let loop ((xs samples) (acc '()))
    (cond ((null? xs) (reverse acc))
          ((closure-closed? C (car xs))
           (loop (cdr xs) (cons (car xs) acc)))
          (else (loop (cdr xs) acc)))))

;; ─── Projection ──────────────────────────────

(define (closure->closed-lattice C samples)
  "Construct the lattice of closed elements from closure operator C.\nClosed elements form a lattice where join is inherited from the\nunderlying lattice (the join of closed elements is closed in a\ncomplete lattice), and meet is cl(meet_L(a, b)) — the closure\nof the lattice meet. Bottom is cl(bottom_L), top is cl(top_L).\nSAMPLES is used only for documentation; the lattice operations\nwork on any closed elements.\n\nNote: this assumes the underlying lattice is complete enough\nthat join of closed elements stays closed. This holds for\npowerset lattices and finite lattices.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L))\n         (CL (closure->closed-lattice C '())))\n    (lattice-join CL '(2) '(3)))  => (2 3)\n\nParameters:\n  C : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: closed lattice, sublattice, fixed point lattice, Moore family\n\nSee also: `closed-elements', `make-lattice'."
  (let ((L (closure-lattice C)))
    (make-lattice
      ;; join: inherited from L (join of closed elements is closed
      ;; in a Moore family / complete lattice)
      (lattice-join-fn L)
      ;; meet: cl(meet_L(a, b))
      (lambda (a b)
        (closure-close C ((lattice-meet-fn L) a b)))
      ;; bottom: cl(bottom_L)
      (closure-close C (lattice-bottom L))
      ;; top: cl(top_L)
      (closure-close C (lattice-top L))
      ;; leq: inherited from L
      (lattice-leq-fn L))))

;; ─── Pre-built instances ─────────────────────

(define (downward-closure-operator po universe)
  "Construct a closure operator that downward-closes sets under PO.\nOperates on the powerset lattice of UNIVERSE. Given a set S,\ncl(S) adds all elements y in UNIVERSE where y <= x for some\nx in S. The closed elements are the downward-closed (lower)\nsets of the partial order.\n\nExamples:\n  (let* ((po (make-partial-order <=))\n         (C (downward-closure-operator po '(1 2 3 4 5))))\n    (closure-close C '(3)))  => (1 2 3)\n\nParameters:\n  po : any\n  universe : list\nReturns: any\nCategory: algebra\nKeywords: downward closed, lower set, down-set, ideal, order ideal, Alexandrov\n\nSee also: `make-closure-operator', `powerset-lattice'."
  (define (subset? a b)
    (cond ((null? a) #t)
          ((member (car a) b) (subset? (cdr a) b))
          (else #f)))
  (define (union a b)
    (cond ((null? a) b)
          ((member (car a) b) (union (cdr a) b))
          (else (cons (car a) (union (cdr a) b)))))
  (define (downward-close s)
    ;; Add all y in universe where y <= x for some x in s
    (let loop ((us universe) (acc s))
      (cond ((null? us) acc)
            ((member (car us) acc) (loop (cdr us) acc))
            ((let inner ((xs s))
               (cond ((null? xs) #f)
                     ((po-leq? po (car us) (car xs)) #t)
                     (else (inner (cdr xs)))))
             (loop (cdr us) (cons (car us) acc)))
            (else (loop (cdr us) acc)))))
  (make-closure-operator
    downward-close
    (powerset-lattice universe)))

;; ─── Validation ──────────────────────────────

(define (validate-closure-operator C samples)
  "Spot-check that C satisfies the closure operator laws on SAMPLES.\nTests extensiveness (a <= cl(a)), monotonicity (a <= b implies\ncl(a) <= cl(b)), and idempotency (cl(cl(a)) = cl(a)) for\nelements and pairs in SAMPLES. Returns #t if all laws hold,\nor a list of (violation-type element ...) entries.\n\nExamples:\n  (let* ((L (powerset-lattice '(1 2 3)))\n         (C (make-closure-operator\n              (lambda (s) (if (member 1 s) '(1 2 3) s))\n              L)))\n    (validate-closure-operator C '(() (1) (2) (1 2) (1 2 3))))  => #t\n\nParameters:\n  C : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: extensiveness, monotonicity, idempotency, validation, law checking\n\nSee also: `make-closure-operator', `closure-close'."
  (let ((violations '())
        (L (closure-lattice C)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    (for-each
      (lambda (a)
        (let ((cl-a (closure-close C a)))
          ;; Extensive: a ≤ cl(a)
          (unless (lattice-leq? L a cl-a)
            (fail! 'extensive a))
          ;; Idempotent: cl(cl(a)) = cl(a)
          (unless (lattice-equal? L (closure-close C cl-a) cl-a)
            (fail! 'idempotent a))
          ;; Monotone: a ≤ b ⟹ cl(a) ≤ cl(b)
          (for-each
            (lambda (b)
              (when (lattice-leq? L a b)
                (unless (lattice-leq? L cl-a (closure-close C b))
                  (fail! 'monotone a b))))
            samples)))
      samples)
    (if (null? violations) #t (reverse violations))))

;; ─── with-closure macro ─────────────────────

(define-syntax with-closure
  (syntax-rules ()
    ((with-closure C (close lattice) body ...)
     (let ((tmp C))
       (let ((close   (lambda (a) (closure-close tmp a)))
             (lattice (closure-lattice tmp)))
         body ...)))))
```

**Step 3: Build**

Run: `make build`
Expected: success

**Step 4: Commit**

```
feat: add (wile algebra closure) — closure operators on lattices
```

---

## Task 7: Closure Operator — Tests

**Files:**
- Create: `test/wile/algebra-closure-test.scm`

**Step 1: Write the test file**

Create `test/wile/algebra-closure-test.scm`:

```scheme
;;; algebra-closure-test.scm — Closure operator tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra closure))

(test-begin "closure-operators")

;; ─── A simple closure: "if 1 is in the set, add everything" ──

(define L (powerset-lattice '(1 2 3)))
(define C (make-closure-operator
            (lambda (s)
              (if (member 1 s) '(1 2 3) s))
            L))

;; ─── Construction ────────────────────────────

(test-group "construction"
  (test #t (closure-operator? C))
  (test #f (closure-operator? 42))
  (test #f (closure-operator? L)))

;; ─── closure-close ───────────────────────────

(test-group "closure-close"
  ;; Contains 1 -> everything
  (test 3 (length (closure-close C '(1))))
  (test #t (and (member 1 (closure-close C '(1)))
                (member 2 (closure-close C '(1)))
                (member 3 (closure-close C '(1))) #t))
  ;; Does not contain 1 -> unchanged
  (test '(2) (closure-close C '(2)))
  (test '(2 3) (closure-close C '(2 3)))
  ;; Empty -> unchanged
  (test '() (closure-close C '()))
  ;; Already closed
  (test 3 (length (closure-close C '(1 2 3)))))

;; ─── closure-closed? ─────────────────────────

(test-group "closure-closed?"
  (test #t (closure-closed? C '()))
  (test #f (closure-closed? C '(1)))      ;; cl({1}) = {1,2,3} ≠ {1}
  (test #t (closure-closed? C '(2)))      ;; cl({2}) = {2}
  (test #t (closure-closed? C '(3)))
  (test #t (closure-closed? C '(2 3)))
  (test #t (closure-closed? C '(1 2 3)))) ;; cl(top) = top

;; ─── closed-elements ─────────────────────────

(test-group "closed-elements"
  (let ((samples '(() (1) (2) (3) (1 2) (2 3) (1 2 3))))
    (let ((closed (closed-elements C samples)))
      ;; {1} and {1,2} are not closed
      (test #f (member '(1) closed))
      (test #f (member '(1 2) closed))
      ;; These should be closed
      (test #t (and (member '() closed) #t))
      (test #t (and (member '(2) closed) #t))
      (test #t (and (member '(3) closed) #t))
      (test #t (and (member '(2 3) closed) #t))
      (test #t (and (member '(1 2 3) closed) #t)))))

;; ─── closure->closed-lattice ─────────────────

(test-group "closed-lattice"
  (let ((CL (closure->closed-lattice C '())))
    (test #t (lattice? CL))
    ;; Bottom = cl(∅) = ∅
    (test '() (lattice-bottom CL))
    ;; Top = cl({1,2,3}) = {1,2,3}
    (test 3 (length (lattice-top CL)))
    ;; Join of closed elements (inherited from powerset)
    (let ((j (lattice-join CL '(2) '(3))))
      (test #t (and (member 2 j) (member 3 j) #t))
      (test 2 (length j)))
    ;; Meet of closed elements: cl(meet_L(a, b))
    ;; meet of {2,3} and {1,2,3} in powerset = {2,3}
    ;; cl({2,3}) = {2,3} (no 1, so unchanged)
    (let ((m (lattice-meet CL '(2 3) '(1 2 3))))
      (test #t (and (member 2 m) (member 3 m) #t))
      (test 2 (length m)))))

;; ─── downward-closure-operator ───────────────

(test-group "downward-closure"
  (let* ((po (make-partial-order <=))
         (DC (downward-closure-operator po '(1 2 3 4 5))))
    (test #t (closure-operator? DC))
    ;; cl({3}) = {1, 2, 3}
    (let ((result (closure-close DC '(3))))
      (test 3 (length result))
      (test #t (and (member 1 result) (member 2 result)
                    (member 3 result) #t)))
    ;; cl({5}) = {1, 2, 3, 4, 5} = universe
    (test 5 (length (closure-close DC '(5))))
    ;; cl({1}) = {1} (nothing below 1)
    (test '(1) (closure-close DC '(1)))
    ;; Already closed
    (test #t (closure-closed? DC '(1 2 3)))
    ;; Not closed: {2, 4} is missing 1 and 3
    (test #f (closure-closed? DC '(2 4)))))

;; ─── validate-closure-operator ───────────────

(test-group "validate-closure-operator"
  ;; Valid
  (test #t (validate-closure-operator C '(() (1) (2) (1 2) (1 2 3))))
  ;; Valid: downward closure
  (let ((DC (downward-closure-operator (make-partial-order <=) '(1 2 3))))
    (test #t (validate-closure-operator DC '(() (1) (2) (1 2) (1 2 3)))))
  ;; Invalid: non-extensive (shrinks elements)
  (let ((bad (make-closure-operator (lambda (s) '()) L)))
    (let ((result (validate-closure-operator bad '((1) (2)))))
      (test #f (eq? #t result))
      (test 'extensive (caar result)))))

;; ─── with-closure macro ─────────────────────

(test-group "with-closure"
  (with-closure C (close lattice)
    (test #t (lattice? lattice))
    (test 3 (length (close '(1))))
    (test '(2) (close '(2)))))

(test-end)
(test-exit)
```

**Step 2: Run the test**

Run: `make build && ./dist/darwin/arm64/wile test/wile/algebra-closure-test.scm`
Expected: all tests pass

**Step 3: Commit**

```
test: add closure operator test suite
```

---

## Task 8: Differential Ring — Library Definition

**Files:**
- Create: `stdlib/lib/wile/algebra/differential.sld`
- Create: `stdlib/lib/wile/algebra/differential.scm`

**Step 1: Create the library definition**

Create `stdlib/lib/wile/algebra/differential.sld`:

```scheme
(define-library (wile algebra differential)
  (description "Differential rings: rings equipped with a derivation satisfying the Leibniz rule.")
  (export make-differential-ring differential-ring?
          differential-deriv differential-ring-ring
          differential-nth-deriv differential-constant?
          differential-ring->ring
          dual-number-ring polynomial-derivation
          validate-differential-ring
          with-differential)
  (import (scheme base)
          (wile algebra ring))
  (include "differential.scm"))
```

**Step 2: Create the implementation**

Create `stdlib/lib/wile/algebra/differential.scm`:

```scheme
;;; (wile algebra differential) — Differential rings
;;;
;;; A differential ring is a ring R equipped with a derivation
;;; D : R → R satisfying additivity D(a+b) = D(a)+D(b) and the
;;; Leibniz rule D(a·b) = D(a)·b + a·D(b).

;; ─── Record type ─────────────────────────────

(define-record-type <differential-ring>
  (make-differential-ring* ring deriv-fn)
  differential-ring?
  (ring     differential-ring-ring)
  (deriv-fn differential-ring-deriv-fn))

(define (make-differential-ring ring deriv)
  "Construct a differential ring from RING and DERIV.\nDERIV must satisfy additivity (D(a+b) = D(a)+D(b)) and the\nLeibniz rule (D(a*b) = D(a)*b + a*D(b)). This models the\nessence of differentiation as an algebraic operation.\n\nExamples:\n  (let* ((D (dual-number-ring))\n         (x (cons 2 1)))\n    (differential-deriv D x))  => 1\n\nParameters:\n  ring : any\n  deriv : procedure\nReturns: any\nCategory: algebra\nKeywords: differential, derivation, Leibniz, calculus, differentiation\n\nSee also: `dual-number-ring', `validate-differential-ring'."
  (make-differential-ring* ring deriv))

;; ─── Core operations ─────────────────────────

(define (differential-deriv D a)
  "Apply the derivation of differential ring D to element A.\nReturns D(a). The derivation satisfies additivity and the\nLeibniz product rule.\n\nExamples:\n  (let ((D (dual-number-ring)))\n    (differential-deriv D (cons 3 1)))   => 1\n  (let ((D (dual-number-ring)))\n    (differential-deriv D (cons 5 0)))   => 0\n\nParameters:\n  D : any\n  a : any\nReturns: any\nCategory: algebra\nKeywords: derivative, derive, differentiate, D, d/dx\n\nSee also: `differential-nth-deriv', `differential-constant?'."
  ((differential-ring-deriv-fn D) a))

;; ─── Derived operations ──────────────────────

(define (differential-nth-deriv D n a)
  "Apply the derivation N times: D^n(a).\nReturns the nth derivative. D^0(a) = a.\n\nExamples:\n  (let ((D (polynomial-derivation (integer-ring))))\n    (differential-nth-deriv D 0 '(3 2 1)))  => (3 2 1)\n  (let ((D (polynomial-derivation (integer-ring))))\n    (differential-nth-deriv D 1 '(3 2 1)))  => (2 2)\n  (let ((D (polynomial-derivation (integer-ring))))\n    (differential-nth-deriv D 2 '(3 2 1)))  => (2)\n\nParameters:\n  D : any\n  n : integer\n  a : any\nReturns: any\nCategory: algebra\nKeywords: nth derivative, higher order, iterated, repeated differentiation\n\nSee also: `differential-deriv'."
  (let loop ((remaining n) (current a))
    (if (<= remaining 0) current
        (loop (- remaining 1) (differential-deriv D current)))))

(define (differential-constant? D a)
  "Test whether A is a constant under D's derivation (D(a) = zero).\n\nExamples:\n  (let ((D (dual-number-ring)))\n    (differential-constant? D (cons 5 0)))  => #t\n  (let ((D (dual-number-ring)))\n    (differential-constant? D (cons 5 1)))  => #f\n\nParameters:\n  D : any\n  a : any\nReturns: boolean\nCategory: algebra\nKeywords: constant, kernel, annihilated, zero derivative\n\nSee also: `differential-deriv'."
  (equal? (differential-deriv D a) (ring-zero (differential-ring-ring D))))

;; ─── Projection ──────────────────────────────

(define (differential-ring->ring D)
  "Project differential ring D to its underlying ring, forgetting the derivation.\n\nExamples:\n  (let* ((D (dual-number-ring))\n         (R (differential-ring->ring D)))\n    (ring-plus R (cons 1 2) (cons 3 4)))  => (4 . 6)\n\nParameters:\n  D : any\nReturns: any\nCategory: algebra\nKeywords: forgetful functor, projection, underlying ring, forget derivation\n\nSee also: `differential-ring-ring', `make-ring'."
  (differential-ring-ring D))

;; ─── Pre-built instances ─────────────────────

(define (dual-number-ring)
  "Construct the ring of dual numbers R[e]/(e^2=0).\nElements are pairs (a . b) representing a + b*e, where\ne^2 = 0. The derivation extracts the e-coefficient.\nThis gives forward-mode automatic differentiation:\nto compute f'(x), evaluate f on (x . 1) and read\noff the cdr of the result.\n\nArithmetic:\n  (a,b) + (c,d) = (a+c, b+d)\n  (a,b) * (c,d) = (a*c, a*d + b*c)    [since e^2=0]\n  -(a,b) = (-a, -b)\n  D(a,b) = b\n\nExamples:\n  ;; f(x) = x^2, f'(2) = 4\n  (let* ((D (dual-number-ring))\n         (R (differential-ring-ring D))\n         (x (cons 2 1))\n         (x2 (ring-times R x x)))\n    (differential-deriv D x2))  => 4\n\nReturns: any\nCategory: algebra\nKeywords: dual numbers, automatic differentiation, AD, forward mode, tangent, epsilon\n\nSee also: `polynomial-derivation', `make-differential-ring'."
  (let ((R (make-ring
             ;; plus: (a,b) + (c,d) = (a+c, b+d)
             (lambda (x y) (cons (+ (car x) (car y))
                                 (+ (cdr x) (cdr y))))
             ;; times: (a,b) * (c,d) = (a*c, a*d + b*c)
             (lambda (x y) (cons (* (car x) (car y))
                                 (+ (* (car x) (cdr y))
                                    (* (cdr x) (car y)))))
             ;; zero
             (cons 0 0)
             ;; one
             (cons 1 0)
             ;; negate
             (lambda (x) (cons (- (car x)) (- (cdr x)))))))
    (make-differential-ring R cdr)))

(define (polynomial-derivation R)
  "Construct a differential ring of polynomials over ring R.\nPolynomials are represented as coefficient lists in ascending\npower order: (a0 a1 a2 ...) = a0 + a1*x + a2*x^2 + ...\nThe empty list represents zero. The derivation is the formal\nderivative: D(a0 + a1*x + ... + an*x^n) = a1 + 2*a2*x + ...\n\nExamples:\n  ;; D(3 + 2x + x^2) = 2 + 2x\n  (let ((D (polynomial-derivation (integer-ring))))\n    (differential-deriv D '(3 2 1)))  => (2 2)\n  ;; D(5) = 0\n  (let ((D (polynomial-derivation (integer-ring))))\n    (differential-deriv D '(5)))      => ()\n\nParameters:\n  R : any\nReturns: any\nCategory: algebra\nKeywords: polynomial, formal derivative, coefficient, power series, symbolic\n\nSee also: `dual-number-ring', `make-differential-ring'."
  (let ((rplus (ring-plus-fn R))
        (rtimes (ring-times-fn R))
        (rzero (ring-zero R))
        (rone (ring-one R))
        (rneg (ring-negate-fn R)))
    ;; Normalize: strip trailing zeros
    (define (normalize p)
      (let loop ((rev (reverse p)))
        (cond ((null? rev) '())
              ((equal? (car rev) rzero) (loop (cdr rev)))
              (else (reverse rev)))))
    ;; Polynomial addition
    (define (poly+ a b)
      (normalize
        (cond ((null? a) b)
              ((null? b) a)
              (else (cons (rplus (car a) (car b))
                          (poly+ (cdr a) (cdr b)))))))
    ;; Polynomial multiplication
    (define (poly* a b)
      (normalize
        (cond ((null? a) '())
              ((null? b) '())
              (else
                ;; a0*b + x*(a_rest * b)
                (let ((a0b (map (lambda (bi) (rtimes (car a) bi)) b))
                      (rest (poly* (cdr a) b)))
                  (poly+ a0b (cons rzero rest)))))))
    ;; Polynomial negation
    (define (poly-neg a)
      (map (lambda (ai) (rneg ai)) a))
    ;; Formal derivative
    (define (poly-deriv p)
      (if (or (null? p) (null? (cdr p))) '()
          (normalize
            (let loop ((coeffs (cdr p)) (k 1) (acc '()))
              (if (null? coeffs) (reverse acc)
                  (let ((nat-k (let kloop ((i 0) (s rzero))
                                 (if (>= i k) s
                                     (kloop (+ i 1) (rplus s rone))))))
                    (loop (cdr coeffs) (+ k 1)
                          (cons (rtimes nat-k (car coeffs)) acc))))))))
    (let ((poly-ring (make-ring poly+ poly* '() (list rone) poly-neg)))
      (make-differential-ring poly-ring poly-deriv))))

;; ─── Validation ──────────────────────────────

(define (validate-differential-ring D samples)
  "Spot-check that D satisfies differential ring laws on SAMPLES.\nDelegates ring law checks to validate-ring on the underlying\nring, then tests additivity (D(a+b) = D(a)+D(b)) and the\nLeibniz rule (D(a*b) = D(a)*b + a*D(b)) for all pairs in\nSAMPLES. Returns #t if all laws hold, or a list of\n(violation-type element ...) entries describing failures.\n\nExamples:\n  (let ((D (dual-number-ring)))\n    (validate-differential-ring D\n      (list (cons 1 0) (cons 0 1) (cons 2 3))))  => #t\n\nParameters:\n  D : any\n  samples : list\nReturns: any\nCategory: algebra\nKeywords: Leibniz rule, additivity, product rule, validation, law checking\n\nSee also: `make-differential-ring', `validate-ring'."
  (let ((violations '())
        (R (differential-ring-ring D)))
    (define (fail! type . args)
      (set! violations (cons (cons type args) violations)))
    ;; Delegate ring laws
    (let ((ring-result (validate-ring R samples)))
      (when (not (eq? #t ring-result))
        (set! violations (append ring-result violations))))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            ;; Additivity: D(a+b) = D(a) + D(b)
            (let ((lhs (differential-deriv D (ring-plus R a b)))
                  (rhs (ring-plus R (differential-deriv D a)
                                    (differential-deriv D b))))
              (unless (equal? lhs rhs)
                (fail! 'additivity a b)))
            ;; Leibniz: D(a*b) = D(a)*b + a*D(b)
            (let ((lhs (differential-deriv D (ring-times R a b)))
                  (rhs (ring-plus R
                         (ring-times R (differential-deriv D a) b)
                         (ring-times R a (differential-deriv D b)))))
              (unless (equal? lhs rhs)
                (fail! 'leibniz a b))))
          samples))
      samples)
    (if (null? violations) #t (reverse violations))))

;; ─── with-differential macro ────────────────

(define-syntax with-differential
  (syntax-rules ()
    ((with-differential D (plus times zero one negate deriv) body ...)
     (let ((tmp D))
       (let ((r (differential-ring-ring tmp)))
         (let ((plus   (lambda (a b) (ring-plus r a b)))
               (times  (lambda (a b) (ring-times r a b)))
               (zero   (ring-zero r))
               (one    (ring-one r))
               (negate (lambda (a) (ring-negate r a)))
               (deriv  (lambda (a) (differential-deriv tmp a))))
           body ...))))))
```

**Step 3: Build**

Run: `make build`
Expected: success

**Step 4: Commit**

```
feat: add (wile algebra differential) — differential rings with dual numbers and polynomial derivation
```

---

## Task 9: Differential Ring — Tests

**Files:**
- Create: `test/wile/algebra-differential-test.scm`

**Step 1: Write the test file**

Create `test/wile/algebra-differential-test.scm`:

```scheme
;;; algebra-differential-test.scm — Differential ring tests

(import (scheme base)
        (chibi test)
        (wile algebra ring)
        (wile algebra differential))

(test-begin "differential-rings")

;; ─── Construction ────────────────────────────

(test-group "construction"
  (test #t (differential-ring? (dual-number-ring)))
  (test #t (differential-ring? (polynomial-derivation (integer-ring))))
  (test #f (differential-ring? 42))
  (test #f (differential-ring? (integer-ring))))

;; ═══════════════════════════════════════════════
;; Dual number ring
;; ═══════════════════════════════════════════════

(define D (dual-number-ring))
(define R (differential-ring-ring D))

(test-group "dual-ring-arithmetic"
  ;; (1,0) + (2,3) = (3,3)
  (test (cons 3 3) (ring-plus R (cons 1 0) (cons 2 3)))
  ;; (2,3) * (4,5) = (8, 2*5+3*4) = (8, 22)
  (test (cons 8 22) (ring-times R (cons 2 3) (cons 4 5)))
  ;; zero
  (test (cons 0 0) (ring-zero R))
  ;; one
  (test (cons 1 0) (ring-one R))
  ;; negate
  (test (cons -3 -4) (ring-negate R (cons 3 4))))

(test-group "dual-deriv"
  ;; D(a, b) = b
  (test 1 (differential-deriv D (cons 3 1)))
  (test 0 (differential-deriv D (cons 5 0)))
  (test 7 (differential-deriv D (cons 0 7))))

(test-group "dual-ad-x-squared"
  ;; f(x) = x^2, f'(2) = 4
  (let* ((x (cons 2 1))   ; x=2, dx=1
         (x2 (ring-times R x x)))
    ;; x^2 at x=2: (4, 4)  since (2,1)*(2,1) = (4, 2*1+1*2) = (4,4)
    (test (cons 4 4) x2)
    (test 4 (differential-deriv D x2))))

(test-group "dual-ad-polynomial"
  ;; f(x) = x^3 + 2x, f'(3) = 3*9 + 2 = 29
  (let* ((x (cons 3 1))
         (x2 (ring-times R x x))
         (x3 (ring-times R x x2))
         (two (cons 2 0))
         (2x (ring-times R two x))
         (result (ring-plus R x3 2x)))
    ;; x^3 at 3: (27, 27)
    ;; 2x at 3: (6, 2)
    ;; sum: (33, 29)
    (test 33 (car result))
    (test 29 (cdr result))
    (test 29 (differential-deriv D result))))

(test-group "dual-ad-product-rule"
  ;; f(x) = x * (x+1), f'(x) = 2x+1
  ;; At x=3: f(3) = 12, f'(3) = 7
  (let* ((x (cons 3 1))
         (one (cons 1 0))
         (x+1 (ring-plus R x one))
         (result (ring-times R x x+1)))
    (test 12 (car result))
    (test 7 (differential-deriv D result))))

;; ─── nth-deriv and constant? ─────────────────

(test-group "nth-deriv"
  (test (cons 3 1) (differential-nth-deriv D 0 (cons 3 1)))
  (test 1 (differential-nth-deriv D 1 (cons 3 1))))

(test-group "constant?"
  (test #t (differential-constant? D (cons 5 0)))
  (test #f (differential-constant? D (cons 5 1))))

;; ─── Projection ──────────────────────────────

(test-group "differential-ring->ring"
  (let ((R2 (differential-ring->ring D)))
    (test #t (ring? R2))
    (test (cons 3 3) (ring-plus R2 (cons 1 0) (cons 2 3)))))

;; ═══════════════════════════════════════════════
;; Polynomial derivation
;; ═══════════════════════════════════════════════

(define PD (polynomial-derivation (integer-ring)))
(define PR (differential-ring-ring PD))

(test-group "polynomial-ring-arithmetic"
  ;; (1 + 2x) + (3 + 4x) = (4 + 6x)
  (test '(4 6) (ring-plus PR '(1 2) '(3 4)))
  ;; (1 + x) * (1 + x) = 1 + 2x + x^2
  (test '(1 2 1) (ring-times PR '(1 1) '(1 1)))
  ;; zero
  (test '() (ring-zero PR))
  ;; one
  (test '(1) (ring-one PR))
  ;; negate
  (test '(-1 -2) (ring-negate PR '(1 2))))

(test-group "polynomial-deriv"
  ;; D(3 + 2x + x^2) = 2 + 2x
  (test '(2 2) (differential-deriv PD '(3 2 1)))
  ;; D(5) = 0
  (test '() (differential-deriv PD '(5)))
  ;; D(0) = 0
  (test '() (differential-deriv PD '()))
  ;; D(x) = 1
  (test '(1) (differential-deriv PD '(0 1)))
  ;; D(x^3) = 3x^2
  (test '(0 0 3) (differential-deriv PD '(0 0 0 1))))

(test-group "polynomial-nth-deriv"
  ;; D^2(3 + 2x + x^2) = D(2 + 2x) = 2
  (test '(2) (differential-nth-deriv PD 2 '(3 2 1)))
  ;; D^3(3 + 2x + x^2) = D(2) = 0
  (test '() (differential-nth-deriv PD 3 '(3 2 1))))

(test-group "polynomial-constant?"
  (test #t (differential-constant? PD '(5)))
  (test #t (differential-constant? PD '()))
  (test #f (differential-constant? PD '(1 1))))

;; ─── Validation ──────────────────────────────

(test-group "validate-differential-ring"
  ;; Valid: dual numbers
  (test #t (validate-differential-ring D
             (list (cons 0 0) (cons 1 0) (cons 0 1) (cons 2 3))))
  ;; Valid: polynomial derivation
  (test #t (validate-differential-ring PD
             (list '() '(1) '(0 1) '(1 1) '(3 2 1))))
  ;; Invalid: constant derivation D(x) = 0 breaks Leibniz
  ;; D(a*b) = 0 but D(a)*b + a*D(b) = 0 — actually this DOES satisfy Leibniz!
  ;; Instead: D(x) = 1 (constant) breaks Leibniz
  (let* ((bad-R (integer-ring))
         (bad-D (make-differential-ring bad-R (lambda (a) 1))))
    ;; D(a*b) = 1, but D(a)*b + a*D(b) = 1*b + a*1 = a+b
    ;; For a=2, b=3: D(6)=1, but 1*3+2*1 = 5 ≠ 1
    (let ((result (validate-differential-ring bad-D '(0 1 2 3))))
      (test #f (eq? #t result)))))

;; ─── with-differential macro ────────────────

(test-group "with-differential"
  (with-differential D (plus times zero one negate deriv)
    (test (cons 3 3) (plus (cons 1 0) (cons 2 3)))
    (test 1 (deriv (cons 3 1)))
    (test (cons 0 0) zero)
    (test (cons 1 0) one)))

(test-end)
(test-exit)
```

**Step 2: Run the test**

Run: `make build && ./dist/darwin/arm64/wile test/wile/algebra-differential-test.scm`
Expected: all tests pass

**Step 3: Commit**

```
test: add differential ring test suite (dual numbers + polynomial derivation)
```

---

## Task 10: Umbrella Library + Integration Tests

**Files:**
- Modify: `stdlib/lib/wile/algebra.sld` — add new sub-library imports and exports
- Modify: `test/wile/algebra-integration-test.scm` — add cross-type tests

**Step 1: Update umbrella library**

Add to `stdlib/lib/wile/algebra.sld`:

Imports section — add:
```scheme
(wile algebra setoid)
(wile algebra category)
(wile algebra closure)
(wile algebra differential)
```

Exports section — add new blocks:
```scheme
;; Setoids
make-setoid setoid?
setoid-equiv?
default-setoid numeric-setoid string-setoid eqv-setoid
setoid-equivalence-class
validate-setoid with-setoid
;; Categories
make-category category?
category-compose category-identity category-equiv?
category->endomorphism-monoid
procedure-category
validate-category with-category
;; Closure operators
make-closure-operator closure-operator?
closure-close closure-closed? closure-lattice
closed-elements
closure->closed-lattice
downward-closure-operator
validate-closure-operator with-closure
;; Differential rings
make-differential-ring differential-ring?
differential-deriv differential-ring-ring
differential-nth-deriv differential-constant?
differential-ring->ring
dual-number-ring polynomial-derivation
validate-differential-ring with-differential
```

Also update `order` exports to include `validate-partial-order/setoid`.

**Step 2: Add integration tests**

Append to `test/wile/algebra-integration-test.scm`:

```scheme
;; -- Setoid + partial order: antisymmetry check --

(test-group "setoid-partial-order-antisymmetry"
  (let ((po (make-partial-order <=))
        (S (numeric-setoid)))
    (test #t (validate-partial-order/setoid po S '(1 2 3)))))

;; -- Category -> endomorphism monoid -> validate-monoid --

(test-group "category-endomorphism-validation"
  (let* ((C (procedure-category))
         (M (category->endomorphism-monoid C 'any)))
    (test #t (monoid? M))
    ;; Fold a chain of +1 operations
    (let ((add3 (monoid-fold M (list (lambda (x) (+ x 1))
                                     (lambda (x) (+ x 1))
                                     (lambda (x) (+ x 1))))))
      (test 13 (add3 10)))))

;; -- Closure -> closed-lattice -> validate-lattice --

(test-group "closure-closed-lattice-validation"
  (let* ((L (powerset-lattice '(1 2 3)))
         (C (make-closure-operator
              (lambda (s) (if (member 1 s) '(1 2 3) s))
              L))
         (CL (closure->closed-lattice C '())))
    (test #t (lattice? CL))
    ;; Closed elements: {}, {2}, {3}, {2,3}, {1,2,3}
    (test #t (validate-lattice CL '(() (2) (3) (2 3) (1 2 3))))))

;; -- Differential + ring: dual numbers verify derivative --

(test-group "dual-number-ad-integration"
  ;; f(x) = x^2 + x + 1, f'(x) = 2x + 1, f'(3) = 7
  (let* ((D (dual-number-ring))
         (R (differential-ring-ring D))
         (x (cons 3 1))
         (x2 (ring-times R x x))
         (one (ring-one R))
         (result (ring-plus R (ring-plus R x2 x) one)))
    (test 13 (car result))     ;; f(3) = 9 + 3 + 1 = 13
    (test 7 (cdr result))))    ;; f'(3) = 6 + 1 = 7
```

Also add `(wile algebra setoid)` to the imports if using `(wile algebra)` umbrella — it should come through the umbrella, but verify.

**Step 3: Run all tests**

Run: `make build && make test-scheme`
Expected: all tests pass, including the new integration tests

**Step 4: Commit**

```
feat: add setoid, category, closure, differential to (wile algebra) umbrella + integration tests
```

---

## Task 11: Final Verification

**Step 1: Full build and lint**

Run: `make lint && make covercheck`
Expected: both pass

**Step 2: Run complete test suite**

Run: `make test`
Expected: all Go and Scheme tests pass

**Step 3: Update plan status**

Update `plans/2026-04-10-orthogonal-algebra-phase2-design.md`: change Status to `Implemented`.

Update `plans/CLAUDE.md`: add entry for the new plan file.

**Step 4: Final commit**

```
docs: mark orthogonal algebra phase 2 as implemented
```
