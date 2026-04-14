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

;; Term protocol for S-expression terms. The compare function orders
;; atoms for commutativity normalization: symbols alphabetically,
;; atoms before compounds.

(define proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        ((symbol? b) #f)
        (else #f)))))

;; ============================================================
;; 1. What discover-equivalences does
;; ============================================================
;;
;; Given a theory and a term, discover-equivalences tries every
;; non-trivial sub-theory (each non-directional axiom individually,
;; then the full theory) and collects the distinct normal forms.
;;
;; Directional axioms (like associativity) are not explored
;; individually -- they would produce combinatorial bracketings
;; without reducing term size.
;;
;; This answers the question: "What can this expression simplify to,
;; depending on which laws we assume?"
;;
;; Each entry in the result is a pair (normal-form . trace) where:
;;   (car entry) = the normal form
;;   (cdr entry) = the trace (list of rewrite steps)

;; ============================================================
;; 2. Boolean expression
;; ============================================================
;;
;; The absorption law says (and x (or x y)) = x. But without it,
;; the term is irreducible. discover-equivalences shows both forms.

(display "--- Boolean expression ---\n\n")

(let* ((B (powerset-boolean '(x y z)))
       (th (boolean->theory B 'or 'and 'not))
       (equivs (discover-equivalences th proto '(and x (or x y)))))
  (display "  Expression: (and x (or x y))\n")
  (display "  Discovered equivalences:\n")
  (for-each
    (lambda (entry)
      (display "    -> ")
      (display (car entry))
      (display "  via ")
      (display (length (cdr entry)))
      (display " step(s)\n"))
    equivs)
  (newline))

;; ============================================================
;; 3. Different axiom sets, different results
;; ============================================================
;;
;; Build a theory with identity + commutativity for +.
;; Run discover-equivalences on (+ zero x).
;;
;; Full theory (identity + commutativity): reduces to x
;;   identity fires on (+ zero x) -> x
;;
;; Commutativity alone: reorders to (+ x zero)
;;   no identity axiom to remove the zero
;;
;; Identity alone would also give x, so it's deduplicated
;; with the full-theory result.

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
      (display "    -> ")
      (display (car entry))
      (newline))
    equivs)
  (display "  ")
  (display (length equivs))
  (display " distinct forms: identity removes zero, commutativity just reorders\n")
  (newline))

;; ============================================================
;; 4. Ring expression
;; ============================================================
;;
;; A ring has many axioms: additive identity, multiplicative
;; absorbing element, commutativity, involution, etc.
;; Different subsets produce different normal forms for the
;; compound expression (+ (* 0 y) (+ x 0)).

(display "--- Ring expression ---\n\n")

(let* ((R (integer-ring))
       (th (ring->theory R '+ '* 'neg))
       (equivs (discover-equivalences th proto '(+ (* 0 y) (+ x 0)))))
  (display "  Expression: (+ (* 0 y) (+ x 0))\n")
  (display "  Discovered forms:\n")
  (for-each
    (lambda (entry)
      (display "    -> ")
      (display (car entry))
      (display "  (")
      (display (length (cdr entry)))
      (display " step(s))\n"))
    equivs)
  (display "  ")
  (display (length equivs))
  (display " distinct forms from different axiom subsets\n")
  (newline))

;; ============================================================
;; 5. Already-normal terms
;; ============================================================
;;
;; An irreducible symbol like x cannot be rewritten by any axiom.
;; discover-equivalences returns exactly 1 entry: the term itself
;; with an empty trace (0 steps).

(display "--- Already normal ---\n\n")

(let* ((B (powerset-boolean '(x y z)))
       (th (boolean->theory B 'or 'and 'not))
       (equivs (discover-equivalences th proto 'x)))
  (display "  Expression: x (already irreducible)\n")
  (display "  Forms: ")
  (display (length equivs))
  (display " (just itself)\n")
  (display "  Steps: ")
  (display (length (cdr (car equivs))))
  (display "\n")
  (newline))

;; ============================================================
;; Summary
;; ============================================================

(display "=== What We Covered ===\n")
(display "  discover-equivalences   Explore distinct normal forms across sub-theories\n")
(display "  (car entry)             Extract the normal form from a result entry\n")
(display "  (cdr entry)             Extract the trace (list of rewrite steps)\n")
(display "  boolean->theory         Project a Boolean algebra into a rewriting theory\n")
(display "  ring->theory            Project a ring into a rewriting theory\n")
(display "  make-theory             Build custom theories from named axioms\n")
(newline)
(display "Equivalence depends on which laws you assume.\n")
(display "discover-equivalences explores the space for you.\n")
