;;; boolean-simplifier.scm - End-to-end Boolean expression simplification
;;;
;;; Demonstrates: powerset-boolean, boolean->theory, recursive normalization
;;;               with tracing, Heyting vs Boolean comparison
;;;
;;; Usage: ./dist/wile --file examples/algebra/boolean-simplifier.scm

(import (scheme base) (scheme write) (wile algebra))

;; ============================================================
;; 1. Build the algebra and derive its theory
;; ============================================================
;;
;; A powerset Boolean algebra over {x, y, z} gives us concrete
;; join/meet/complement operations on subsets. boolean->theory
;; projects this into a symbolic rewriting theory: named axioms
;; that fire on S-expression terms using the operator symbols
;; we choose (or, and, not).

(display "=== Building Boolean Algebra & Theory ===\n\n")

(define B (powerset-boolean '(x y z)))
(define bool-theory (boolean->theory B 'or 'and 'not))

(display "Derived ")
(display (length (theory-axioms bool-theory)))
(display " axioms from powerset Boolean algebra:\n\n")

(for-each
  (lambda (na)
    (display "  ")
    (display (named-axiom-name na))
    (display "\n    ")
    (display (named-axiom-general-form na))
    (newline))
  (theory-axioms bool-theory))
(newline)

;; ============================================================
;; 2. Set up the normalizer
;; ============================================================
;;
;; The normalizer needs a term protocol that tells it how to
;; inspect S-expression terms, and a compare function that
;; orders atoms for commutativity normalization.

(display "=== Setting Up Normalizer ===\n\n")

(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

(define normalize (make-recursive-normalizer bool-theory proto))

(display "Normalizer ready. It will:\n")
(display "  - Rewrite terms to a fixed point\n")
(display "  - Track every rewrite step in a trace\n")
(display "  - Return (values result trace)\n")
(newline)

;; ============================================================
;; 3. Helper: show-simplification
;; ============================================================
;;
;; Takes a label and expression, normalizes it, and displays
;; the input, output, and full trace.

(define (show-simplification label expr)
  (display label)
  (display ":\n")
  (display "  Input:  ")
  (display expr)
  (newline)
  (let-values (((result trace) (normalize expr)))
    (display "  Output: ")
    (display result)
    (newline)
    (display "  Trace:\n")
    (for-each
      (lambda (line)
        (display "    ")
        (display line)
        (newline))
      (format-trace trace))
    (newline)))

;; ============================================================
;; 4. Simplify Boolean expressions
;; ============================================================

(display "=== Boolean Simplifications ===\n\n")

;; Absorption: and(x, or(x, y)) = x
;; The inner or(x, y) is absorbed — x already guarantees the meet.
(show-simplification
  "Absorption"
  '(and x (or x y)))

;; Double negation: not(not(x)) = x
;; Boolean complement is an involution — applying it twice cancels.
(show-simplification
  "Double negation"
  '(not (not x)))

;; Nested expression combining absorption and involution:
;; or(and(x, or(x, y)), not(not(z)))
;;   -> or(x, z) via absorption on the left, involution on the right.
(show-simplification
  "Nested (absorption + involution)"
  '(or (and x (or x y)) (not (not z))))

;; Idempotence: or(x, x) = x
;; Joining a value with itself changes nothing.
(show-simplification
  "Idempotence"
  '(or x x))

;; ============================================================
;; 5. Heyting vs Boolean comparison
;; ============================================================
;;
;; A Heyting algebra is a lattice with implication but WITHOUT
;; a complement involution. Every Boolean algebra is Heyting,
;; but not vice versa. The key difference: in Heyting logic,
;; not(not(x)) = x does NOT hold in general.
;;
;; We build a Heyting theory from the powerset to compare.

(display "=== Heyting vs Boolean ===\n\n")

(define H (powerset-heyting '(x y z)))
(define heyt-theory (heyting->theory H 'join 'meet))

(display "Heyting theory: ")
(display (length (theory-axioms heyt-theory)))
(display " axioms\n")
(display "Boolean theory: ")
(display (length (theory-axioms bool-theory)))
(display " axioms\n\n")

(display "Heyting axioms:\n")
(for-each
  (lambda (na)
    (display "  ")
    (display (named-axiom-name na))
    (newline))
  (theory-axioms heyt-theory))
(newline)

(display "The missing axiom: complement-involution.\n")
(display "Heyting algebras have no general complement, so there is\n")
(display "no involution axiom to fire.\n\n")

;; Absorption works in both — it's a lattice law.
(define heyt-normalize (make-recursive-normalizer heyt-theory proto))

(display "--- Absorption (works in both) ---\n\n")

(display "Boolean: (and x (or x y))\n")
(let-values (((result trace) (normalize '(and x (or x y)))))
  (display "  => ")
  (display result)
  (display "  [")
  (display (length trace))
  (display " step(s)]\n"))

(display "Heyting: (meet x (join x y))\n")
(let-values (((result trace) (heyt-normalize '(meet x (join x y)))))
  (display "  => ")
  (display result)
  (display "  [")
  (display (length trace))
  (display " step(s)]\n"))
(newline)

;; Double negation: Boolean simplifies, Heyting cannot.
(display "--- Double negation (Boolean only) ---\n\n")

(display "Boolean: (not (not x))\n")
(let-values (((result trace) (normalize '(not (not x)))))
  (display "  => ")
  (display result)
  (display "  [")
  (display (length trace))
  (display " step(s), involution fires]\n"))

(display "Heyting: no complement operator — double negation\n")
(display "  cannot even be expressed, let alone simplified.\n")
(display "  The theory has no axiom for it.\n")
(newline)

;; ============================================================
;; 6. Closing
;; ============================================================

(display "=== Takeaway ===\n\n")
(display "The algebraic structure determines which simplifications are valid.\n")
(display "Boolean algebras admit complement involution (not(not(x)) = x),\n")
(display "while Heyting algebras — the logic of intuitionistic reasoning —\n")
(display "do not. Both share lattice laws like absorption and idempotence.\n")
(display "The theory projection captures exactly the axioms the structure\n")
(display "supports, so the normalizer only applies valid rewrites.\n")
