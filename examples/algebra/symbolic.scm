;;; symbolic.scm - Symbolic algebra with theories and traced normalization
;;;
;;; Demonstrates: named axioms, theories, theory combinators, recursive
;;;               normalization with tracing, structure-to-theory projections,
;;;               and fuel exhaustion
;;;
;;; Usage: ./dist/wile --file examples/algebra/symbolic.scm

(import (scheme base) (scheme write) (wile algebra))

;; ============================================================
;; 1. Named axioms
;; ============================================================
;;
;; An axiom (identity, commutativity, etc.) is a rewrite law.
;; A named axiom wraps an axiom with a human-readable name and
;; a general-form string describing the rewrite pattern.

(display "=== Named Axioms ===\n\n")

;; Identity predicate: matches the symbol 'zero
(define zero? (lambda (x) (eq? x 'zero)))

;; Build raw axioms
(define id-axiom (make-identity-axiom '+ zero?))
(define comm-axiom (make-commutativity-axiom '+))

;; Wrap them with names and general-form descriptions
(define na-identity
  (make-named-axiom "additive-identity" "(+ x zero) -> x" id-axiom))
(define na-commutativity
  (make-named-axiom "commutativity" "(+ a b) -> (+ b a) when b < a" comm-axiom))

(display "Named axiom: ")
(display (named-axiom-name na-identity))
(newline)
(display "  General form: ")
(display (named-axiom-general-form na-identity))
(newline)

(display "Named axiom: ")
(display (named-axiom-name na-commutativity))
(newline)
(display "  General form: ")
(display (named-axiom-general-form na-commutativity))
(newline)
(newline)

;; ============================================================
;; 2. Theories
;; ============================================================
;;
;; A theory groups named axioms together with a list of operators
;; whose associativity the normalizer should handle.

(display "=== Theories ===\n\n")

(define add-theory
  (make-theory (list na-identity na-commutativity) '(+)))

(display "Axiom count: ")
(display (length (theory-axioms add-theory)))
(newline)

(display "Axiom names: ")
(for-each
  (lambda (na)
    (display (named-axiom-name na))
    (display "  "))
  (theory-axioms add-theory))
(newline)

(display "Associative ops: ")
(display (theory-associative-ops add-theory))
(newline)
(newline)

;; ============================================================
;; 3. Theory combinators
;; ============================================================
;;
;; Theories can be filtered, pruned, and reordered without
;; rebuilding from scratch. This controls which axioms fire
;; and in what order.

(display "=== Theory Combinators ===\n\n")

;; Keep only the identity axiom
(define id-only (theory-filter add-theory '("additive-identity")))
(display "theory-filter (keep identity): ")
(display (length (theory-axioms id-only)))
(display " axiom(s) -> ")
(display (named-axiom-name (car (theory-axioms id-only))))
(newline)

;; Remove the identity axiom, leaving commutativity
(define no-id (theory-exclude add-theory '("additive-identity")))
(display "theory-exclude (drop identity): ")
(display (length (theory-axioms no-id)))
(display " axiom(s) -> ")
(display (named-axiom-name (car (theory-axioms no-id))))
(newline)

;; Reorder: move commutativity to front
(define comm-first (theory-prioritize add-theory '("commutativity")))
(display "theory-prioritize (commutativity first): ")
(display (named-axiom-name (car (theory-axioms comm-first))))
(display ", ")
(display (named-axiom-name (cadr (theory-axioms comm-first))))
(newline)
(newline)

;; ============================================================
;; 4. Recursive normalizer
;; ============================================================
;;
;; make-recursive-normalizer compiles a theory into a procedure
;; that rewrites terms to a fixed point, tracking every step.
;; It returns (values result trace).

(display "=== Recursive Normalizer ===\n\n")

;; Build a term protocol for S-expression terms.
;; The compare function orders atoms for commutativity normalization.
(define proto
  (sexp-term-protocol
    (lambda (a b)
      (string<? (symbol->string a) (symbol->string b)))))

(define normalize (make-recursive-normalizer add-theory proto))

;; Simple case: (+ x zero) -> x
(display "Normalizing (+ x zero):\n")
(let-values (((result trace) (normalize '(+ x zero))))
  (display "  Result: ")
  (display result)
  (newline)
  (display "  Steps:  ")
  (display (length trace))
  (newline))
(newline)

;; Nested case: (+ (+ x zero) zero) -> x  (two rewrites)
(display "Normalizing (+ (+ x zero) zero):\n")
(let-values (((result trace) (normalize '(+ (+ x zero) zero))))
  (display "  Result: ")
  (display result)
  (newline)
  (display "  Steps:  ")
  (display (length trace))
  (newline))
(newline)

;; Commutativity: (+ y x) -> (+ x y) since x < y
(display "Normalizing (+ y x):\n")
(let-values (((result trace) (normalize '(+ y x))))
  (display "  Result: ")
  (display result)
  (newline)
  (display "  Steps:  ")
  (display (length trace))
  (newline))
(newline)

;; Combined: identity + commutativity on nested term
(display "Normalizing (+ zero (+ y x)):\n")
(let-values (((result trace) (normalize '(+ zero (+ y x)))))
  (display "  Result: ")
  (display result)
  (newline)
  (display "  Steps:  ")
  (display (length trace))
  (newline))
(newline)

;; ============================================================
;; 5. Transformation traces
;; ============================================================
;;
;; format-trace renders each rewrite step as a human-readable
;; string showing the rule name, general form, and before/after.

(display "=== Transformation Traces ===\n\n")

(display "Trace for (+ zero (+ y x)) -> (+ x y):\n")
(let-values (((result trace) (normalize '(+ zero (+ y x)))))
  (for-each
    (lambda (line)
      (display "  ")
      (display line)
      (newline))
    (format-trace trace)))
(newline)

(display "Trace for (+ (+ x zero) zero) -> x:\n")
(let-values (((result trace) (normalize '(+ (+ x zero) zero))))
  (for-each
    (lambda (line)
      (display "  ")
      (display line)
      (newline))
    (format-trace trace)))
(newline)

;; Individual step inspection
(display "Step inspection on first step:\n")
(let-values (((result trace) (normalize '(+ x zero))))
  (let ((s (car trace)))
    (display "  rule-name:    ") (display (step-rule-name s)) (newline)
    (display "  general-form: ") (display (step-general-form s)) (newline)
    (display "  before:       ") (display (step-before s)) (newline)
    (display "  after:        ") (display (step-after s)) (newline)
    (display "  fuel-exhausted? ") (display (fuel-exhausted-step? s)) (newline)))
(newline)

;; ============================================================
;; 6. Structure-to-theory projections
;; ============================================================
;;
;; Algebraic structures can be projected into theories
;; automatically. The projection generates named axioms from
;; the structure's operations.

(display "=== Structure -> Theory Projections ===\n\n")

;; Monoid projection: identity + associativity = 2 axioms
(define add-monoid (make-monoid + 0))
(define monoid-th (monoid->theory add-monoid '+))

(display "monoid->theory: ")
(display (length (theory-axioms monoid-th)))
(display " axioms\n")
(for-each
  (lambda (na)
    (display "  ")
    (display (named-axiom-name na))
    (display " -- ")
    (display (named-axiom-general-form na))
    (newline))
  (theory-axioms monoid-th))

(display "  Associative ops: ")
(display (theory-associative-ops monoid-th))
(newline)
(newline)

;; Ring projection: 7 axioms (6 semiring + negate involution)
(define int-ring (integer-ring))
(define ring-th (ring->theory int-ring '+ '* 'neg))

(display "ring->theory: ")
(display (length (theory-axioms ring-th)))
(display " axioms\n")
(for-each
  (lambda (na)
    (display "  ")
    (display (named-axiom-name na))
    (newline))
  (theory-axioms ring-th))

(display "  Associative ops: ")
(display (theory-associative-ops ring-th))
(newline)
(newline)

;; ============================================================
;; 7. Fuel exhaustion
;; ============================================================
;;
;; The recursive normalizer accepts an optional fuel parameter
;; that caps the number of rewrite iterations. When fuel runs
;; out before reaching a fixed point, the normalizer stops and
;; appends a fuel-exhausted marker to the trace.

(display "=== Fuel Exhaustion ===\n\n")

;; A deeply nested term that needs 3 identity rewrites
(define deep-term '(+ (+ (+ x zero) zero) zero))

;; Full normalization (default fuel)
(display "Full normalization of (+ (+ (+ x zero) zero) zero):\n")
(let-values (((result trace) (normalize deep-term)))
  (display "  Result: ")
  (display result)
  (newline)
  (display "  Steps:  ")
  (display (length trace))
  (newline))
(newline)

;; Fuel-limited normalization: only 1 rewrite allowed
(define normalize-limited (make-recursive-normalizer add-theory proto 1))

(display "Fuel=1 normalization of same term:\n")
(let-values (((result trace) (normalize-limited deep-term)))
  (display "  Result: ")
  (display result)
  (display "  (partial -- not fully reduced)\n")
  (display "  Steps:  ")
  (display (length trace))
  (newline)
  (newline)
  (display "  Trace:\n")
  (for-each
    (lambda (line)
      (display "    ")
      (display line)
      (newline))
    (format-trace trace))
  (newline)
  ;; Check the fuel-exhaustion marker
  (let ((last-step (car (reverse trace))))
    (display "  Last step fuel-exhausted? ")
    (display (fuel-exhausted-step? last-step))
    (newline)))
(newline)

;; ============================================================
;; Summary
;; ============================================================

(display "=== What We Covered ===\n")
(display "  make-named-axiom          Wrap axioms with name + general-form\n")
(display "  make-theory               Group named axioms + associative ops\n")
(display "  theory-filter/exclude     Select or remove axioms by name\n")
(display "  theory-prioritize         Control axiom application order\n")
(display "  make-recursive-normalizer Compile a theory into a traced normalizer\n")
(display "  format-trace              Render rewrite steps as readable strings\n")
(display "  monoid->theory            Project monoid into 2-axiom theory\n")
(display "  ring->theory              Project ring into 7-axiom theory\n")
(display "  fuel exhaustion           Cap iterations, detect partial results\n")
(newline)
(display "Next: examples/algebra/boolean-simplifier.scm — real-world simplification.\n")
