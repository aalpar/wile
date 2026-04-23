;; ================================================================
;; Chapter 11 -- Equivalence discovery: sub-theory comparison
;;
;; What you will learn:
;;   - How `discover-equivalences` enumerates distinct normal forms by
;;     running a term through the full theory and each single-axiom
;;     sub-theory.
;;   - The practical consequence: different axiom subsets recognize
;;     different equivalences, and you can see which axiom is "carrying
;;     the weight" for a particular simplification.
;;   - How to read and interpret the traces that accompany each
;;     distinct normal form.
;;   - `theory-filter`, `theory-exclude`, `theory-prioritize`, and
;;     `theory-merge` for adapting theories to a specific need.
;;
;; Prerequisites: chapters 03, 04 (rewriting, theories).
;; Sub-libraries used:
;;   (wile algebra rewrite), (wile algebra symbolic),
;;   (wile algebra boolean).
;; ================================================================

(import (scheme base) (scheme write)
        (srfi 1)
        (wile algebra rewrite)
        (wile algebra boolean)
        (wile algebra symbolic))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: Build a Boolean theory and its sub-theories.
;;
;; Boolean theory has 11 axioms. We'll use it as the source for
;; equivalence-discovery experiments.
;; ----------------------------------------------------------------

(define B (powerset-boolean '(a b c)))
(define bool-theory (boolean->theory B 'or 'and 'not))
(check= (length (theory-axioms bool-theory))  11
        "bool-theory has 11 named axioms")

(define (bool-atom-compare a b)
  (cond ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        (else
         (string<? (call-with-port (open-output-string)
                     (lambda (p) (write a p) (get-output-string p)))
                   (call-with-port (open-output-string)
                     (lambda (p) (write b p) (get-output-string p)))))))

(define proto (sexp-term-protocol bool-atom-compare))

;; ----------------------------------------------------------------
;; Part 2: discover-equivalences on (or x x) -- idempotence is the
;; only axiom that simplifies this.
;;
;; The full theory collapses (or x x) to x. Single-axiom sub-theories:
;; only the idempotence-of-or axiom produces the same collapse; most
;; others leave the term alone.
;; ----------------------------------------------------------------

(define discoveries-1 (discover-equivalences bool-theory proto '(or x x)))

;; Each element is (normal-form . trace).
(check-true (pair? (car discoveries-1))
            "each discovery is (normal-form . trace)")

;; Distinct normal forms found.
(define forms-1 (map car discoveries-1))
(check-true (member 'x forms-1)
            "at least one sub-theory normalizes to x")
(check-true (member '(or x x) forms-1)
            "at least one sub-theory leaves (or x x) untouched")

;; ----------------------------------------------------------------
;; Part 3: discover-equivalences on (not (not x)) -- only the
;; complement-involution axiom collapses it.
;; ----------------------------------------------------------------

(define discoveries-2 (discover-equivalences bool-theory proto '(not (not x))))
(define forms-2 (map car discoveries-2))

(check-true (member 'x forms-2)
            "involution collapses (not (not x)) to x")
(check-true (member '(not (not x)) forms-2)
            "other sub-theories leave the double-not alone")

;; ----------------------------------------------------------------
;; Part 4: Theory combinators -- filter, exclude, prioritize, merge.
;;
;; `theory-filter` keeps only named axioms whose names are in the list.
;; `theory-exclude` drops axioms by name.
;; `theory-prioritize` reorders axioms so named ones come first.
;; `theory-merge` combines axiom lists (merges associative-ops too).
;; ----------------------------------------------------------------

;; Filter to just commutativity and idempotence. Note the axioms coming
;; out of boolean->theory use the lattice-internal names "join" and
;; "meet", not the operator symbols we passed in (`or` and `and`). Pass
;; the internal names to theory-filter.
(define comm-idemp-only
  (theory-filter bool-theory '("commutativity-join" "commutativity-meet"
                                "idempotence-join" "idempotence-meet")))
(check= (length (theory-axioms comm-idemp-only))  4
        "filter keeps exactly the four named axioms")

;; Exclude complement-involution.
(define no-involution
  (theory-exclude bool-theory '("complement-involution")))
(check= (length (theory-axioms no-involution))  10
        "exclude removes complement-involution (11 -> 10)")

;; Normalizing (not (not x)) under no-involution does not collapse.
(define no-inv-norm (make-recursive-normalizer no-involution proto))
(define-values (nf-noinv _tr-noinv) (no-inv-norm '(not (not x))))
(check= nf-noinv  '(not (not x))
        "without involution: (not (not x)) stays")

;; ----------------------------------------------------------------
;; Part 5: prioritize -- moves named axioms to the front of rule order.
;;
;; Prioritization matters when multiple rules could fire. Putting the
;; rule you want to fire first ensures it gets tried first. The other
;; rules are still in the theory -- just lower priority.
;; ----------------------------------------------------------------

(define idempotence-first
  (theory-prioritize bool-theory '("idempotence-or")))
(check= (length (theory-axioms idempotence-first))
        (length (theory-axioms bool-theory))
        "prioritize does not add or remove axioms")

;; ----------------------------------------------------------------
;; Part 6: theory-merge.
;;
;; Combining a small "just involution" theory with a fresh axiom to
;; demonstrate merging. merge preserves associative-ops from both.
;; ----------------------------------------------------------------

(define involution-only
  (theory-filter bool-theory '("complement-involution")))

(define idemp-or-axiom
  (make-named-axiom "idempotence-or" "x or x = x" (make-idempotence-axiom 'or)))
(define idemp-only (make-theory (list idemp-or-axiom) '()))

(define merged (theory-merge involution-only idemp-only))

(check-true (theory? merged)                         "merged is a theory")
(check-true (>= (length (theory-axioms merged)) 2)
            "merged has at least the two input axioms")

;; Normalizing with the merged theory collapses both (not (not x)) and
;; (or x x).
(define merged-norm (make-recursive-normalizer merged proto))
(define-values (m1 _t1) (merged-norm '(or x x)))
(define-values (m2 _t2) (merged-norm '(not (not x))))
(check= m1  'x   "merged theory collapses (or x x) to x")
(check= m2  'x   "merged theory collapses (not (not x)) to x")

;; ----------------------------------------------------------------
;; Part 7: format-trace for human-readable rewrite explanations.
;;
;; When something went wrong or you want to understand why a term
;; simplified the way it did, format the trace. Each step is rendered
;; as "rule-name : before => after".
;; ----------------------------------------------------------------

(define norm (make-recursive-normalizer bool-theory proto))
(define-values (_nf trace) (norm '(not (not x))))

(define formatted (format-trace trace))
;; format-trace returns a list of strings (one per step), not a single
;; string. Each entry reads "rule-name (form): before => after".
(check-true (list? formatted)                       "format-trace returns a list")
(check-true (every string? formatted)               "every entry is a string")
(check-true (>= (length formatted) 1)               "trace has at least one step")

;; ----------------------------------------------------------------
;; Part 8: Fuel exhaustion.
;;
;; Some theories on some terms would reduce forever. The fuel parameter
;; bounds iteration. The trace includes a fuel-exhausted-step? marker
;; when the limit is reached.
;; ----------------------------------------------------------------

;; A theory with only associativity -- it would reassociate indefinitely
;; if we gave it the wrong direction on a deeply nested term. Low fuel
;; makes the exhaustion visible.
(define assoc-only
  (make-theory
    (list (make-named-axiom "assoc-+" "(a+b)+c = a+(b+c)"
                            (make-associativity-axiom '+)))
    '(+)))

(define short-fuel-norm (make-recursive-normalizer assoc-only proto 3))

;; A left-associated term that keeps reassociating right: (+ (+ (+ a b) c) d).
;; With fuel = 3, the solver cannot reach the fully right-associated form
;; in so few steps -- associativity only moves one pair of parens per step,
;; and there are 3 nested compounds to unwind. The trace must include a
;; fuel-exhausted step. This is the diagnostic pattern for non-termination.
(define-values (_shrink fuel-trace)
  (short-fuel-norm '(+ (+ (+ a b) c) d)))

(check-true (any fuel-exhausted-step? fuel-trace)
            "trace includes a fuel-exhausted marker at low fuel")

;; ----------------------------------------------------------------
;; Part 9: Which axiom set recognizes which equivalence?
;;
;; Put it together: discover-equivalences gives a menu of "this axiom
;; subset recognizes the term = X". You can use this to ask which
;; laws your domain actually needs. If two terms are equivalent under
;; all sub-theories, they're syntactically identical. If they agree
;; under the full theory but differ under some sub-theory, you know
;; which axiom is carrying the equivalence.
;; ----------------------------------------------------------------

(define discoveries-3 (discover-equivalences bool-theory proto '(and x (or x y))))
(define forms-3 (map car discoveries-3))

(check-true (member 'x forms-3)
            "at least one sub-theory normalizes to x (absorption)")
(check-true (> (length forms-3) 1)
            "multiple distinct normal forms found")

(display "chapter 11 complete") (newline)
