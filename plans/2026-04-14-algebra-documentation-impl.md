# Algebra Library Documentation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create user-facing documentation and examples for the `(wile algebra)` library.

**Architecture:** Three deliverables: 6 runnable example files in `examples/algebra/`, an introduction doc at `docs/ALGEBRA.md`, and an API reference at `docs/ALGEBRA_REFERENCE.md`. Examples are written first (they inform what the docs need to explain), then docs, then README update.

**Tech Stack:** Scheme (R7RS), Markdown. All examples must run via `./dist/wile --file <path>`.

**Design doc:** `plans/2026-04-14-algebra-documentation-design.md`

---

## Conventions

All example files follow the existing format in `examples/`:

```scheme
;;; filename.scm - Brief description
;;;
;;; Demonstrates: feature1, feature2, feature3
;;;
;;; Usage: ./dist/wile --file examples/algebra/filename.scm
```

- Self-contained and runnable standalone
- Import only what's needed (prefer `(wile algebra)` umbrella for simplicity in examples)
- Use `(display ...)` and `(newline)` for output — no `format` or `write` unless demonstrating something
- Produce visible output explaining what's happening
- Explanatory comments for the non-obvious; don't comment obvious Scheme

## Key API Signatures (reference for all tasks)

```scheme
;; Monoid
(make-monoid op identity)
(monoid-op M a b)    (monoid-identity M)
(monoid-fold M lst)  (monoid-power M a n)
(validate-monoid M samples)
(with-monoid M (op identity) body ...)

;; Group
(make-group op identity inverse)
(group->monoid G)
(with-group G (op identity inverse) body ...)

;; Lattice
(make-lattice join meet bottom top leq?)
(flat-lattice elements equal?)  (powerset-lattice universe)
(fixpoint L f x)  (fixpoint L f x fuel)
(with-lattice L (join meet bottom top leq?) body ...)

;; Ring / Field
(make-ring plus times zero one negate)
(integer-ring)  (modular-ring n)
(ring->semiring R)  (ring->additive-group R)
(make-field plus times zero one negate reciprocal)
(rational-field)  (field-divide F a b)  (field->ring F)

;; Boolean / Heyting
(powerset-boolean universe)
(boolean->heyting B)  (boolean->lattice B)  (boolean->ring B)
(powerset-heyting universe)
(heyting->lattice H)

;; Rewriting
(make-term-protocol compound? get-op get-args make-term compare)
(make-identity-axiom op-sym identity?)
(make-commutativity-axiom op-sym)
(make-absorbing-axiom op-sym absorbing?)
(make-idempotence-axiom op-sym)
(make-involution-axiom op-sym)
(make-absorption-axiom outer-op inner-op)
(make-associativity-axiom op-sym)
(axiom->rules axiom proto)
(make-normalizer axioms proto)

;; Symbolic
(sexp-term-protocol compare)
(make-named-axiom name general-form axiom)
(make-theory named-axioms associative-ops)
(theory-filter th names)  (theory-exclude th names)
(theory-prioritize th names)  (theory-merge th1 th2)
(make-recursive-normalizer theory proto)        ; default fuel
(make-recursive-normalizer theory proto fuel)   ; explicit fuel
;; returns (values result trace)
(monoid->theory M op-sym)
(group->theory G op-sym inv-sym)
(ring->theory R plus-sym times-sym neg-sym)
(boolean->theory B or-sym and-sym not-sym)
(discover-equivalences theory proto term)
(discover-equivalences theory proto term fuel)
(format-trace trace)
```

---

### Task 1: Create `examples/algebra/getting-started.scm`

**Files:**
- Create: `examples/algebra/getting-started.scm`

First contact with the algebra library. Introduces monoids (the simplest structure), demonstrates core patterns.

**Step 1: Create the example file**

```scheme
;;; getting-started.scm - First steps with algebraic structures
;;;
;;; Demonstrates: monoids, fold, power, validation, with-monoid
;;;
;;; Usage: ./dist/wile --file examples/algebra/getting-started.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Getting Started with (wile algebra) ===\n\n")

;; -----------------------------------------------------------------------
;; What's a monoid?
;;
;; A monoid is the simplest algebraic structure: a binary operation
;; and an identity element.  Addition with 0, multiplication with 1,
;; string-append with "" — these are all monoids.
;; -----------------------------------------------------------------------

(display "--- Building monoids ---\n\n")

;; make-monoid takes two arguments: an operation and its identity element.
(define add-monoid (make-monoid + 0))
(define mul-monoid (make-monoid * 1))

(display "  Addition monoid:\n")
(display "    (monoid-op add 3 4)      = ")
(display (monoid-op add-monoid 3 4))
(newline)
(display "    (monoid-identity add)    = ")
(display (monoid-identity add-monoid))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Fold: reduce a list using the monoid
;; -----------------------------------------------------------------------

(display "--- Folding lists ---\n\n")

(display "  Sum of (1 2 3 4 5):  ")
(display (monoid-fold add-monoid '(1 2 3 4 5)))
(newline)

(display "  Product of (1 2 3 4 5): ")
(display (monoid-fold mul-monoid '(1 2 3 4 5)))
(newline)

;; Fold on empty list returns the identity — that's the point of having one.
(display "  Sum of ():           ")
(display (monoid-fold add-monoid '()))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Power: apply the operation n times
;; -----------------------------------------------------------------------

(display "--- Powers ---\n\n")

;; (monoid-power M a n) computes a `op` a `op` ... `op` a  (n times)
(display "  2^10 via mul-monoid: ")
(display (monoid-power mul-monoid 2 10))
(newline)

(display "  5*4  via add-monoid: ")
(display (monoid-power add-monoid 5 4))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Validation: check that your monoid actually satisfies the laws
;; -----------------------------------------------------------------------

(display "--- Validation ---\n\n")

;; validate-monoid spot-checks associativity and identity on sample elements.
;; Returns #t if all checks pass.
(display "  (validate-monoid add '(-2 -1 0 1 2)) = ")
(display (validate-monoid add-monoid '(-2 -1 0 1 2)))
(newline)

;; Here's a broken "monoid" — subtraction isn't associative.
(define bad-monoid (make-monoid - 0))
(display "  (validate-monoid bad '(1 2 3))        = ")
(display (validate-monoid bad-monoid '(1 2 3)))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; with-monoid: destructure for clean syntax
;; -----------------------------------------------------------------------

(display "--- with-monoid ---\n\n")

;; Instead of passing the monoid to every operation, bind them locally.
(display "  Using with-monoid for addition:\n")
(display "    ")
(display
  (with-monoid add-monoid (op identity)
    (op (op 1 2) (op 3 4))))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; String monoid: not just for numbers
;; -----------------------------------------------------------------------

(display "--- Monoids aren't just numbers ---\n\n")

(define string-monoid (make-monoid string-append ""))

(display "  Fold strings: ")
(display (monoid-fold string-monoid '("hello" " " "world")))
(newline)

(display "  Power string: ")
(display (monoid-power string-monoid "ha" 3))
(newline)
(newline)

(display "A monoid is just an operation with an identity.\n")
(display "The library gives you fold, power, and validation for free.\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/getting-started.scm`
Expected: Clean output with section headers, computed values, and a validation failure for the broken monoid.

---

### Task 2: Create `examples/algebra/structures.scm`

**Files:**
- Create: `examples/algebra/structures.scm`

Shows the breadth of algebraic structures and how they relate via projection.

**Step 1: Create the example file**

```scheme
;;; structures.scm - Algebraic structures and their relationships
;;;
;;; Demonstrates: lattices, rings, fields, boolean algebras,
;;;               forgetful projections, with-X macros
;;;
;;; Usage: ./dist/wile --file examples/algebra/structures.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Algebraic Structures ===\n\n")

;; -----------------------------------------------------------------------
;; Lattices: partial orders with join (least upper bound) and meet
;;           (greatest lower bound)
;; -----------------------------------------------------------------------

(display "--- Lattices ---\n\n")

;; A lattice over integers [0, 100] with max as join, min as meet.
(define int-lattice (make-lattice max min 0 100 <=))

(display "  join(30, 70) = ")
(display (lattice-join int-lattice 30 70))
(newline)
(display "  meet(30, 70) = ")
(display (lattice-meet int-lattice 30 70))
(newline)
(display "  bottom       = ")
(display (lattice-bottom int-lattice))
(newline)
(display "  top          = ")
(display (lattice-top int-lattice))
(newline)
(newline)

;; Powerset lattice: all subsets of a universe, ordered by inclusion.
;; join = union, meet = intersection, bottom = empty, top = universe.
(define P (powerset-lattice '(a b c)))

(display "  Powerset lattice over {a, b, c}:\n")
(display "    join({a}, {b, c}) = ")
(display (lattice-join P '(a) '(b c)))
(newline)
(display "    meet({a, b}, {b, c}) = ")
(display (lattice-meet P '(a b) '(b c)))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Rings and fields
;; -----------------------------------------------------------------------

(display "--- Rings ---\n\n")

(define Z (integer-ring))

(display "  Integer ring:\n")
(display "    3 + 4 = ")
(display (ring-plus Z 3 4))
(newline)
(display "    3 * 4 = ")
(display (ring-times Z 3 4))
(newline)
(display "    -5    = ")
(display (ring-negate Z 5))
(newline)

;; Modular arithmetic: Z/7Z
(define Z7 (modular-ring 7))

(display "  Z/7Z (mod 7):\n")
(display "    5 + 4 = ")
(display (ring-plus Z7 5 4))
(newline)
(display "    3 * 5 = ")
(display (ring-times Z7 3 5))
(newline)
(newline)

(display "--- Fields ---\n\n")

(define Q (rational-field))

(display "  Rational field:\n")
(display "    1/3 + 1/4    = ")
(display (field-plus Q 1/3 1/4))
(newline)
(display "    1/3 * 3      = ")
(display (field-times Q 1/3 3))
(newline)
(display "    reciprocal(7) = ")
(display (field-reciprocal Q 7))
(newline)
(display "    5 / 3        = ")
(display (field-divide Q 5 3))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Boolean algebras
;; -----------------------------------------------------------------------

(display "--- Boolean algebras ---\n\n")

(define B (powerset-boolean '(x y z)))

(display "  Powerset Boolean algebra over {x, y, z}:\n")
(display "    {x} OR {y, z}  = ")
(display (boolean-join B '(x) '(y z)))
(newline)
(display "    {x, y} AND {y} = ")
(display (boolean-meet B '(x y) '(y)))
(newline)
(display "    NOT {x}        = ")
(display (boolean-complement B '(x)))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Forgetful projections: structures contain substructures
;;
;; A ring contains an additive group, which contains a monoid.
;; A Boolean algebra contains a Heyting algebra, which contains a lattice.
;; Projection functions extract these substructures.
;; -----------------------------------------------------------------------

(display "--- Projections ---\n\n")

;; Ring → Semiring → Additive Monoid
(let ((S (ring->semiring Z)))
  (display "  integer-ring → semiring:\n")
  (display "    semiring-plus(3, 4)  = ")
  (display (semiring-plus S 3 4))
  (newline)
  (display "    semiring-times(3, 4) = ")
  (display (semiring-times S 3 4))
  (newline))

;; Ring → Additive Group → Monoid
(let* ((G (ring->additive-group Z))
       (M (group->monoid G)))
  (display "  integer-ring → additive group → monoid:\n")
  (display "    monoid-fold(1 2 3 4) = ")
  (display (monoid-fold M '(1 2 3 4)))
  (newline))

;; Boolean → Heyting → Lattice
(let* ((H (boolean->heyting B))
       (L (heyting->lattice H)))
  (display "  boolean → heyting → lattice:\n")
  (display "    heyting-implies({x}, {x,y}) = ")
  (display (heyting-implies H '(x) '(x y)))
  (newline)
  (display "    lattice-join({x}, {y})      = ")
  (display (lattice-join L '(x) '(y)))
  (newline))

;; Boolean → Ring (symmetric difference + intersection)
(let ((R (boolean->ring B)))
  (display "  boolean → ring (symmetric difference):\n")
  (display "    {x,y} + {y,z} = ")
  (display (ring-plus R '(x y) '(y z)))
  (newline)
  (display "    {x,y} * {y,z} = ")
  (display (ring-times R '(x y) '(y z)))
  (newline))

(newline)

;; -----------------------------------------------------------------------
;; with-X macros: clean syntax for sustained use
;; -----------------------------------------------------------------------

(display "--- with-X macros ---\n\n")

(display "  with-ring on integer-ring:\n")
(display "    (a + b) * (a - b) where a=7, b=3: ")
(display
  (with-ring Z (plus times zero one negate)
    (let ((a 7) (b 3))
      (times (plus a b) (plus a (negate b))))))
(newline)
(newline)

(display "Structures compose via projection, not inheritance.\n")
(display "Extract what you need; the types enforce what's available.\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/structures.scm`
Expected: Sections showing lattice, ring, field, boolean operations; projections producing correct results; with-ring computing 40.

---

### Task 3: Create `examples/algebra/rewriting.scm`

**Files:**
- Create: `examples/algebra/rewriting.scm`

Introduces the equational rewriting engine: term protocols, axiom types, normalizers.

**Step 1: Create the example file**

```scheme
;;; rewriting.scm - Equational rewriting with axiom-driven normalization
;;;
;;; Demonstrates: term protocols, axiom types (identity, commutativity,
;;;               absorbing, idempotence, involution, absorption,
;;;               associativity), single-step normalization
;;;
;;; Usage: ./dist/wile --file examples/algebra/rewriting.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Equational Rewriting ===\n\n")

;; -----------------------------------------------------------------------
;; Term protocols
;;
;; The rewrite engine doesn't know what your terms look like.  You tell
;; it via a term protocol: how to detect compound terms, extract their
;; operator and operands, and rebuild them.
;; -----------------------------------------------------------------------

(display "--- Term protocol ---\n\n")

;; For S-expression terms like (+ a b), the library provides a
;; convenience constructor.  The argument is a comparison function
;; used by commutativity normalization to sort operands.
(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

(display "  (term-compound? '(+ a b)) = ")
(display (term-compound? proto '(+ a b)))
(newline)
(display "  (term-compound? 'x)       = ")
(display (term-compound? proto 'x))
(newline)
(display "  (term-get-operator '(+ a b)) = ")
(display (term-get-operator proto '(+ a b)))
(newline)
(display "  (term-get-operands '(+ a b)) = ")
(display (term-get-operands proto '(+ a b)))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Axiom types
;;
;; Each axiom type captures one algebraic law.  The rewriter turns
;; axioms into rewrite rules via axiom->rules.
;; -----------------------------------------------------------------------

(display "--- Axioms and normalizers ---\n\n")

;; Identity: op(x, e) = x  and  op(e, x) = x
(define id-axiom (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
(define id-norm (make-normalizer (list id-axiom) proto))

(display "  Identity (+ with zero):\n")
(display "    (+ x zero) → ")
(display (id-norm '(+ x zero)))
(newline)
(display "    (+ zero x) → ")
(display (id-norm '(+ zero x)))
(newline)
(display "    (+ x y)    → ")
(display (id-norm '(+ x y)))        ; #f means no rule applied
(newline)
(newline)

;; Commutativity: op(b, a) = op(a, b)  when a < b in the term ordering
(define comm-axiom (make-commutativity-axiom '+))
(define comm-norm (make-normalizer (list comm-axiom) proto))

(display "  Commutativity (+):\n")
(display "    (+ b a)    → ")
(display (comm-norm '(+ b a)))      ; swaps to (+ a b)
(newline)
(display "    (+ a b)    → ")
(display (comm-norm '(+ a b)))      ; already ordered → #f
(newline)
(newline)

;; Absorbing: op(x, z) = z  (zero annihilates)
(define abs-axiom (make-absorbing-axiom '* (lambda (x) (eq? x 'zero))))
(define abs-norm (make-normalizer (list abs-axiom) proto))

(display "  Absorbing (* with zero):\n")
(display "    (* x zero) → ")
(display (abs-norm '(* x zero)))
(newline)
(display "    (* zero x) → ")
(display (abs-norm '(* zero x)))
(newline)
(newline)

;; Idempotence: op(x, x) = x
(define idem-axiom (make-idempotence-axiom 'and))
(define idem-norm (make-normalizer (list idem-axiom) proto))

(display "  Idempotence (and):\n")
(display "    (and x x)  → ")
(display (idem-norm '(and x x)))
(newline)
(display "    (and x y)  → ")
(display (idem-norm '(and x y)))
(newline)
(newline)

;; Involution: op(op(x)) = x  (double application cancels)
(define invol-axiom (make-involution-axiom 'neg))
(define invol-norm (make-normalizer (list invol-axiom) proto))

(display "  Involution (neg):\n")
(display "    (neg (neg x)) → ")
(display (invol-norm '(neg (neg x))))
(newline)
(display "    (neg x)       → ")
(display (invol-norm '(neg x)))
(newline)
(newline)

;; Absorption: op1(a, op2(a, b)) = a
(define absorp-axiom (make-absorption-axiom 'and 'or))
(define absorp-norm (make-normalizer (list absorp-axiom) proto))

(display "  Absorption (and over or):\n")
(display "    (and x (or x y)) → ")
(display (absorp-norm '(and x (or x y))))
(newline)
(display "    (and (or x y) x) → ")
(display (absorp-norm '(and (or x y) x)))
(newline)
(newline)

;; Associativity: op(op(a, b), c) = op(a, op(b, c))  [directional: left→right]
(define assoc-axiom (make-associativity-axiom '+))
(define assoc-norm (make-normalizer (list assoc-axiom) proto))

(display "  Associativity (+, left-to-right):\n")
(display "    (+ (+ a b) c)   → ")
(display (assoc-norm '(+ (+ a b) c)))
(newline)
(display "    (+ a (+ b c))   → ")
(display (assoc-norm '(+ a (+ b c))))   ; already right-assoc → #f
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Composing axioms
;; -----------------------------------------------------------------------

(display "--- Composed normalizer ---\n\n")

;; A normalizer tries axioms in order, returns the first match.
(define composed
  (make-normalizer
    (list (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))
          (make-commutativity-axiom '+))
    proto))

(display "  Identity + commutativity:\n")
(display "    (+ x zero) → ")
(display (composed '(+ x zero)))    ; identity fires first
(newline)
(display "    (+ b a)    → ")
(display (composed '(+ b a)))       ; commutativity fires
(newline)
(newline)

(display "Axioms are data.  Normalization is mechanical.\n")
(display "A normalizer is a single step; for deep simplification,\n")
(display "see the symbolic layer (symbolic.scm).\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/rewriting.scm`
Expected: Each axiom type demonstrated with matching/non-matching cases. `#f` shown for non-matching terms.

---

### Task 4: Create `examples/algebra/symbolic.scm`

**Files:**
- Create: `examples/algebra/symbolic.scm`

The symbolic layer: named axioms, theories, recursive normalization with tracing.

**Step 1: Create the example file**

```scheme
;;; symbolic.scm - Symbolic algebra with theories and traced normalization
;;;
;;; Demonstrates: named axioms, theories, theory combinators,
;;;               recursive normalizer, transformation traces,
;;;               structure-to-theory projection
;;;
;;; Usage: ./dist/wile --file examples/algebra/symbolic.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Symbolic Algebra ===\n\n")

;; -----------------------------------------------------------------------
;; Named axioms
;;
;; A named axiom wraps a rewrite axiom with a name and human-readable
;; description.  This metadata flows through to transformation traces.
;; -----------------------------------------------------------------------

(display "--- Named axioms ---\n\n")

(define id-axiom
  (make-named-axiom "identity" "a + 0 = a"
    (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))

(define comm-axiom
  (make-named-axiom "commutativity" "a + b = b + a"
    (make-commutativity-axiom '+)))

(display "  name: ")
(display (named-axiom-name id-axiom))
(newline)
(display "  form: ")
(display (named-axiom-general-form id-axiom))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Theories
;;
;; A theory groups named axioms with a list of associative operators.
;; The associative-ops hint tells the recursive normalizer which
;; operators to flatten.
;; -----------------------------------------------------------------------

(display "--- Theories ---\n\n")

(define plus-theory
  (make-theory (list id-axiom comm-axiom) '(+)))

(display "  Axiom count: ")
(display (length (theory-axioms plus-theory)))
(newline)
(display "  Associative ops: ")
(display (theory-associative-ops plus-theory))
(newline)
(newline)

;; -----------------------------------------------------------------------
;; Theory combinators: filter, exclude, prioritize, merge
;; -----------------------------------------------------------------------

(display "--- Theory combinators ---\n\n")

;; Keep only the identity axiom
(let ((filtered (theory-filter plus-theory '("identity"))))
  (display "  Filtered to 'identity': ")
  (display (length (theory-axioms filtered)))
  (display " axiom\n"))

;; Remove commutativity
(let ((excluded (theory-exclude plus-theory '("commutativity"))))
  (display "  Excluded 'commutativity': ")
  (display (named-axiom-name (car (theory-axioms excluded))))
  (newline))

;; Reorder: commutativity first
(let ((reordered (theory-prioritize plus-theory '("commutativity"))))
  (display "  Prioritized 'commutativity': ")
  (display (named-axiom-name (car (theory-axioms reordered))))
  (newline))

(newline)

;; -----------------------------------------------------------------------
;; Recursive normalizer
;;
;; Unlike the single-step make-normalizer, the recursive normalizer:
;;   1. Normalizes children first (bottom-up)
;;   2. Applies axioms until fixed point at each node
;;   3. Returns both the result and a trace of every rewrite step
;; -----------------------------------------------------------------------

(display "--- Recursive normalization ---\n\n")

;; Term protocol for S-expressions.
(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

(define norm (make-recursive-normalizer plus-theory proto))

;; Simple: (+ x zero) → x
(let-values (((result trace) (norm '(+ x zero))))
  (display "  (+ x zero) → ")
  (display result)
  (display "  [")
  (display (length trace))
  (display " step(s)]\n"))

;; Nested: (+ (+ x zero) zero) → x
(let-values (((result trace) (norm '(+ (+ x zero) zero))))
  (display "  (+ (+ x zero) zero) → ")
  (display result)
  (display "  [")
  (display (length trace))
  (display " step(s)]\n"))

(newline)

;; -----------------------------------------------------------------------
;; Transformation traces
;;
;; Each step records which rule fired, what it looked like before, and
;; what it became.  format-trace renders these as human-readable strings.
;; -----------------------------------------------------------------------

(display "--- Transformation traces ---\n\n")

(let-values (((result trace) (norm '(+ (+ x zero) zero))))
  (for-each
    (lambda (line)
      (display "  ")
      (display line)
      (newline))
    (format-trace trace)))

(newline)

;; -----------------------------------------------------------------------
;; Structure-to-theory projection
;;
;; Instead of building theories by hand, project them from algebraic
;; structures.  The library knows which axioms each structure satisfies.
;; -----------------------------------------------------------------------

(display "--- Structure → theory ---\n\n")

;; A monoid gives you identity + associativity
(let* ((M (make-monoid + 0))
       (th (monoid->theory M '+))
       (norm (make-recursive-normalizer th proto)))
  (display "  monoid->theory (2 axioms: identity, associativity):\n")
  (for-each
    (lambda (ax)
      (display "    - ")
      (display (named-axiom-name ax))
      (display ": ")
      (display (named-axiom-general-form ax))
      (newline))
    (theory-axioms th))
  (let-values (((result trace) (norm '(+ x 0))))
    (display "    (+ x 0) → ")
    (display result)
    (newline)))

(newline)

;; A ring gives you 7 axioms covering both + and *
(let* ((R (integer-ring))
       (th (ring->theory R '+ '* 'neg)))
  (display "  ring->theory (")
  (display (length (theory-axioms th)))
  (display " axioms):\n")
  (for-each
    (lambda (ax)
      (display "    - ")
      (display (named-axiom-name ax))
      (newline))
    (theory-axioms th)))

(newline)

;; -----------------------------------------------------------------------
;; Fuel: bounded normalization
;; -----------------------------------------------------------------------

(display "--- Fuel exhaustion ---\n\n")

(let* ((theory (make-theory
                 (list (make-named-axiom "identity" "a + 0 = a"
                         (make-identity-axiom '+ (lambda (x) (eq? x 'zero)))))
                 '(+)))
       ;; Only 1 unit of fuel — not enough to fully normalize
       (norm (make-recursive-normalizer theory proto 1)))
  (let-values (((result trace) (norm '(+ (+ (+ x zero) zero) zero))))
    (display "  With fuel=1, (+ (+ (+ x zero) zero) zero) → ")
    (display result)
    (newline)
    (display "  Trace:\n")
    (for-each
      (lambda (line)
        (display "    ")
        (display line)
        (newline))
      (format-trace trace))))

(newline)
(display "Theories are composable.  Normalization is explainable.\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/symbolic.scm`
Expected: Named axioms displayed, theory combinators working, recursive normalization with traces, structure projections listing axiom names, fuel exhaustion producing partial result.

---

### Task 5: Create `examples/algebra/boolean-simplifier.scm`

**Files:**
- Create: `examples/algebra/boolean-simplifier.scm`

End-to-end: build a Boolean algebra, derive its theory, simplify expressions.

**Step 1: Create the example file**

```scheme
;;; boolean-simplifier.scm - End-to-end Boolean expression simplification
;;;
;;; Demonstrates: powerset-boolean, boolean->theory, recursive normalizer,
;;;               traced simplification, Heyting vs Boolean comparison
;;;
;;; Usage: ./dist/wile --file examples/algebra/boolean-simplifier.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Boolean Expression Simplifier ===\n\n")

;; -----------------------------------------------------------------------
;; Build the algebra and its theory
;; -----------------------------------------------------------------------

(define B (powerset-boolean '(x y z)))
(define bool-theory (boolean->theory B 'or 'and 'not))

(display "Boolean theory has ")
(display (length (theory-axioms bool-theory)))
(display " axioms:\n")
(for-each
  (lambda (ax)
    (display "  - ")
    (display (named-axiom-name ax))
    (display ": ")
    (display (named-axiom-general-form ax))
    (newline))
  (theory-axioms bool-theory))
(newline)

;; -----------------------------------------------------------------------
;; Set up the normalizer
;; -----------------------------------------------------------------------

(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

(define simplify (make-recursive-normalizer bool-theory proto))

;; Helper to display simplification with trace
(define (show-simplification label expr)
  (display label)
  (display ":\n")
  (display "  input:  ")
  (display expr)
  (newline)
  (let-values (((result trace) (simplify expr)))
    (display "  output: ")
    (display result)
    (newline)
    (when (> (length trace) 0)
      (display "  steps:\n")
      (for-each
        (lambda (line)
          (display "    ")
          (display line)
          (newline))
        (format-trace trace)))
    (newline)))

;; -----------------------------------------------------------------------
;; Simplify expressions
;; -----------------------------------------------------------------------

(display "--- Simplifications ---\n\n")

;; Absorption: x AND (x OR y) = x
(show-simplification "Absorption" '(and x (or x y)))

;; Double negation: NOT (NOT x) = x
(show-simplification "Double negation" '(not (not x)))

;; Nested: (x OR (x AND (x OR y))) OR (NOT (NOT z))
(show-simplification "Nested"
  '(or (and x (or x y)) (not (not z))))

;; Identity: x AND top, x OR bottom (using idempotence as proxy)
(show-simplification "Idempotence" '(or x x))

;; -----------------------------------------------------------------------
;; Heyting vs Boolean
;;
;; A Heyting algebra is like a Boolean algebra but without the law of
;; excluded middle.  NOT (NOT x) = x holds in Boolean but not Heyting.
;; -----------------------------------------------------------------------

(display "--- Heyting vs Boolean ---\n\n")

(define H (powerset-heyting '(x y z)))
(define heyt-theory (heyting->theory H 'join 'meet))
(define heyt-simplify (make-recursive-normalizer heyt-theory proto))

(display "  Double negation in Boolean:\n")
(let-values (((result trace) (simplify '(not (not x)))))
  (display "    (not (not x)) → ")
  (display result)
  (newline))

(display "  Absorption in Heyting (still works — it's a lattice law):\n")
(let-values (((result trace) (heyt-simplify '(join x (meet x y)))))
  (display "    (join x (meet x y)) → ")
  (display result)
  (newline))

(newline)
(display "The algebraic structure determines which simplifications are valid.\n")
(display "Boolean algebras simplify more aggressively than Heyting algebras.\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/boolean-simplifier.scm`
Expected: Boolean theory axiom listing, traced simplifications showing absorption/involution/nested rewrites, Heyting comparison.

---

### Task 6: Create `examples/algebra/equivalence-discovery.scm`

**Files:**
- Create: `examples/algebra/equivalence-discovery.scm`

Explores how different sub-theories produce different normal forms.

**Step 1: Create the example file**

```scheme
;;; equivalence-discovery.scm - Exploring equivalences across sub-theories
;;;
;;; Demonstrates: discover-equivalences, sub-theory exploration,
;;;               multiple normal forms from different axiom sets
;;;
;;; Usage: ./dist/wile --file examples/algebra/equivalence-discovery.scm

(import (scheme base)
        (scheme write)
        (wile algebra))

(display "=== Equivalence Discovery ===\n\n")

(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

;; -----------------------------------------------------------------------
;; What discover-equivalences does
;;
;; Given a theory and a term, it tries every non-trivial sub-theory
;; (each non-directional axiom individually, then the full theory)
;; and collects the distinct normal forms.
;;
;; This answers the question: "What can this expression simplify to,
;; depending on which laws we assume?"
;; -----------------------------------------------------------------------

(display "--- Boolean expression ---\n\n")

(let* ((B (powerset-boolean '(x y z)))
       (th (boolean->theory B 'or 'and 'not))
       (equivs (discover-equivalences th proto '(and x (or x y)))))
  (display "  Expression: (and x (or x y))\n")
  (display "  Discovered equivalences:\n")
  (for-each
    (lambda (entry)
      (display "    → ")
      (display (car entry))
      (display "  via ")
      (display (length (cdr entry)))
      (display " axiom(s)\n"))
    equivs)
  (newline))

;; -----------------------------------------------------------------------
;; Different axiom sets, different normal forms
;;
;; With identity alone, (+ zero x) reduces to x.
;; With commutativity alone, (+ zero x) reorders to (+ x zero).
;; With both, you get x.
;; -----------------------------------------------------------------------

(display "--- Different axiom sets, different results ---\n\n")

(let* ((th (make-theory
             (list (make-named-axiom "identity" "a + 0 = a"
                     (make-identity-axiom '+ (lambda (x) (eq? x 'zero))))
                   (make-named-axiom "commutativity" "a + b = b + a"
                     (make-commutativity-axiom '+)))
             '(+)))
       (equivs (discover-equivalences th proto '(+ zero x))))
  (display "  Expression: (+ zero x)\n")
  (display "  Discovered forms:\n")
  (for-each
    (lambda (entry)
      (display "    → ")
      (display (car entry))
      (newline))
    equivs)
  (newline))

;; -----------------------------------------------------------------------
;; Ring equivalences
;; -----------------------------------------------------------------------

(display "--- Ring expression ---\n\n")

(let* ((R (integer-ring))
       (th (ring->theory R '+ '* 'neg))
       (equivs (discover-equivalences th proto '(+ (* 0 y) (+ x 0)))))
  (display "  Expression: (+ (* 0 y) (+ x 0))\n")
  (display "  Discovered forms:\n")
  (for-each
    (lambda (entry)
      (display "    → ")
      (display (car entry))
      (newline))
    equivs)
  (newline))

;; -----------------------------------------------------------------------
;; Already-normal terms
;; -----------------------------------------------------------------------

(display "--- Already normal ---\n\n")

(let* ((B (powerset-boolean '(x y z)))
       (th (boolean->theory B 'or 'and 'not))
       (equivs (discover-equivalences th proto 'x)))
  (display "  Expression: x (already irreducible)\n")
  (display "  Forms: ")
  (display (length equivs))
  (display " (just itself)\n")
  (newline))

(display "Equivalence depends on which laws you assume.\n")
(display "discover-equivalences explores the space for you.\n")
```

**Step 2: Run the example**

Run: `./dist/wile --file examples/algebra/equivalence-discovery.scm`
Expected: Multiple discovered normal forms for boolean/ring expressions, single form for already-normal terms.

---

### Task 7: Create `docs/ALGEBRA.md`

**Files:**
- Create: `docs/ALGEBRA.md`

Introduction document covering design intent, hierarchy, patterns, learning path.

**Step 1: Write the introduction document**

Content outline (write cohesive prose, not bullet dumps):

1. **Overview** (1 paragraph) — What the library provides: algebraic structures as composable records, equational rewriting, symbolic normalization with transformation tracing. Import `(wile algebra)` for all exports or individual sub-libraries.

2. **Design** (2-3 paragraphs) — Three principles:
   - Structures are records storing operation closures (not classes, not inheritance)
   - Explicit composition via forgetful projections (`ring->semiring`, `boolean->heyting->lattice`)
   - Three orthogonal roles: operational (compute), equational (rewrite), explanatory (trace)

3. **Structure hierarchy** — ASCII diagram showing:
   ```
   Setoid → Order → Lattice → Closure
                           → Heyting → Boolean
   Monoid → Category
         → Semiring → Ring → Field
                          → Differential Ring
         → Group (→ Ring via additive-group)
   
   Rewrite (axioms + term protocols)
     → Symbolic (theories + recursive normalizer + equivalence discovery)
   ```

4. **Patterns** — Brief description of each recurring pattern:
   - `validate-X` — spot-check laws on sample elements
   - `with-X` — destructure operations for clean syntax
   - Forgetful projections — `X->Y` extracts substructure
   - Predicate-based matching — axioms use predicates, not `equal?`
   - `#f` for no-match — normalizers return `#f` when no rule applies

5. **Learning path** — Ordered list pointing to examples:
   - Start: `getting-started.scm` (monoids, the simplest structure)
   - Build: `structures.scm` (lattices, rings, fields, projections)
   - Rewrite: `rewriting.scm` (axioms and single-step normalization)
   - Compose: `symbolic.scm` (theories and traced normalization)
   - Apply: `boolean-simplifier.scm` (end-to-end workflow)
   - Explore: `equivalence-discovery.scm` (sub-theory exploration)

6. **See also** — Links to reference doc, bibliography, test files as additional examples

**Step 2: Review the document**

Verify all example file paths exist, all structure names match the library, and the hierarchy diagram is accurate.

---

### Task 8: Create `docs/ALGEBRA_REFERENCE.md`

**Files:**
- Create: `docs/ALGEBRA_REFERENCE.md`

Complete API reference organized by sub-library.

**Step 1: Write the reference document**

Structure: one section per sub-library in dependency order. Each section follows this template:

```markdown
## Setoid — `(wile algebra setoid)`

Sets with explicit equivalence relations.

### Constructors

- `(make-setoid equiv-fn)` — Create a setoid from an equivalence predicate.

### Operations

- `(setoid-equiv? S a b)` — Test equivalence of `a` and `b` under `S`.
- `(setoid-equivalence-class S x samples)` — ...

### Built-in Instances

- `default-setoid` — Uses `equal?`
- ...

### Validation

- `(validate-setoid S samples)` — Checks reflexivity, symmetry, transitivity.

### Destructuring

- `(with-setoid S (equiv?) body ...)` — Binds `equiv?` locally.
```

Sub-libraries to cover (16 total):
1. Setoid
2. Partial Order
3. Lattice (including flat-lattice, powerset-lattice, product-lattice, map-lattice, fixpoint, fixpoint/widen)
4. Closure Operator
5. Heyting Algebra
6. Boolean Algebra
7. Monoid
8. Category
9. Semiring (including boolean-semiring, tropical-semiring, counting-semiring)
10. Group
11. Ring
12. Differential Ring
13. Field
14. Galois Connection
15. Rewrite (term protocol, all 7 axiom types, axiom->rules, make-normalizer)
16. Symbolic (named axioms, theories, combinators, recursive normalizer, projections, discover-equivalences, format-trace)

**Step 2: Cross-check against `algebra.sld` exports**

Read `stdlib/lib/wile/algebra.sld` and verify every exported symbol appears in the reference. Flag any missing.

---

### Task 9: Update `examples/README.md`

**Files:**
- Modify: `examples/README.md`

**Step 1: Add Algebra section**

Add after the Applications section (before Logic Programming), following the existing table format:

```markdown
### Algebra

Algebraic structures, equational rewriting, and symbolic normalization.

| File | Description |
|------|-------------|
| [getting-started.scm](algebra/getting-started.scm) | Monoids - fold, power, validation, the simplest algebraic structure |
| [structures.scm](algebra/structures.scm) | Lattices, rings, fields, Boolean algebras, and forgetful projections |
| [rewriting.scm](algebra/rewriting.scm) | Term protocols, axiom types, single-step normalization |
| [symbolic.scm](algebra/symbolic.scm) | Theories, recursive normalizer, transformation traces |
| [boolean-simplifier.scm](algebra/boolean-simplifier.scm) | End-to-end Boolean expression simplification with traced output |
| [equivalence-discovery.scm](algebra/equivalence-discovery.scm) | Exploring normal forms across different axiom sets |

**Key Feature**: Algebraic structures are composable records with automatic validation, equational rewriting, and traced symbolic normalization. Build a structure, project it to a theory, and simplify expressions with step-by-step explanations.
```

**Step 2: Add to learning paths**

In the "Learning Path" section, add under "Experienced Schemer?":
```
5. [algebra/getting-started.scm](algebra/getting-started.scm) - Algebraic structures
```

**Step 3: Verify README renders correctly**

Visually check that the table formatting is consistent with existing sections.

---

### Task 10: Verify all examples run

**Step 1: Build Wile**

Run: `make build`

**Step 2: Run each example**

Run each in sequence:
```bash
./dist/wile --file examples/algebra/getting-started.scm
./dist/wile --file examples/algebra/structures.scm
./dist/wile --file examples/algebra/rewriting.scm
./dist/wile --file examples/algebra/symbolic.scm
./dist/wile --file examples/algebra/boolean-simplifier.scm
./dist/wile --file examples/algebra/equivalence-discovery.scm
```

Expected: All produce output without errors. Fix any runtime issues.

**Step 3: Verify docs link integrity**

Check that all file paths referenced in `docs/ALGEBRA.md` and `docs/ALGEBRA_REFERENCE.md` exist.
