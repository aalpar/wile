;; ================================================================
;; Chapter 04 -- Boolean simplifier: theories, recursive normalization,
;;                and why Heyting can't do what Boolean can.
;;
;; What you will learn:
;;   - How a *theory* differs from a bag of axioms (named axioms + trace).
;;   - How `boolean->theory` projects a Boolean algebra into a rewriting
;;     theory with 7 axioms (AC normalization + absorption + negation +
;;     De Morgan).
;;   - How `make-recursive-normalizer` differs from `make-normalizer` --
;;     recursive, fueled, and trace-emitting.
;;   - Why Heyting algebras cannot simplify (not (not x)) and Boolean
;;     algebras can -- the structural difference becomes visible in the
;;     normal forms.
;;   - The `symbolic-boolean-normalize` shortcut that hides the machinery.
;;
;; Prerequisites: chapters 02, 03.
;; Sub-libraries used:
;;   (wile algebra boolean), (wile algebra heyting), (wile algebra symbolic).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra rewrite)
        (wile algebra boolean)
        (wile algebra heyting)
        (wile algebra symbolic))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: A concrete Boolean algebra.
;;
;; `powerset-boolean` constructs the Boolean algebra of subsets of a
;; universe. Join = union, meet = intersection, complement = set
;; difference from universe, bottom = empty set, top = universe.
;; ----------------------------------------------------------------

(define B (powerset-boolean '(a b c)))
(check-true (boolean-algebra? B)   "B is a Boolean algebra")

;; ----------------------------------------------------------------
;; Part 2: Projection to a theory.
;;
;; `boolean->theory` projects B into an *equational theory*: a collection
;; of named axioms over three operator symbols (join, meet, complement
;; -- we pass our choice of symbols). The result has 7 axioms: an AC
;; (associative-commutative) axiom for each of join and meet -- each one
;; folding commutativity, associativity, idempotence, identity,
;; annihilation, and complement laws in a single terminating pass -- the
;; two absorption laws, Boolean negation, and the two De Morgan laws.
;; ----------------------------------------------------------------

(define bool-theory (boolean->theory B 'or 'and 'not))
(check-true (theory? bool-theory)  "boolean->theory built a theory")
(check= (length (theory-axioms bool-theory)) 7
        "7 axioms (ac-join, ac-meet, 2 absorption, negation, 2 De Morgan)")

;; ----------------------------------------------------------------
;; Part 3: A compare function that handles mixed atom types.
;;
;; `sexp-term-protocol` requires a `compare` function that totally
;; orders atoms. For this chapter, terms are boolean expressions with
;; symbol atoms. A symbol-only comparator suffices.
;; ----------------------------------------------------------------

(define (bool-atom-compare a b)
  (cond ((and (symbol? a) (symbol? b))
         (string<? (symbol->string a) (symbol->string b)))
        ;; Fallback: numbers/other -- compare their write representations.
        (else
         (string<? (call-with-port (open-output-string)
                     (lambda (p) (write a p) (get-output-string p)))
                   (call-with-port (open-output-string)
                     (lambda (p) (write b p) (get-output-string p)))))))

(define proto (sexp-term-protocol bool-atom-compare))

;; ----------------------------------------------------------------
;; Part 4: Recursive normalization.
;;
;; `make-normalizer` (chapter 3) does one step and returns #f when no
;; rule fires. `make-recursive-normalizer` walks the full term tree,
;; applies rules at every level, loops until fixed point (or fuel runs
;; out), and emits a trace of every rewrite step. Returns two values
;; (result and trace) via `values`.
;; ----------------------------------------------------------------

(define normalize (make-recursive-normalizer bool-theory proto))

;; Involution: (not (not x)) collapses to x.
(define-values (nf1 tr1) (normalize '(not (not x))))
(check= nf1 'x  "double negation eliminated")
;; Exactly one rewrite step for single (not (not x)) -- the negation rule.
(check= (length tr1) 1  "trace records exactly one step")

;; Absorption: (and x (or x y)) collapses to x.
(define-values (nf2 tr2) (normalize '(and x (or x y))))
(check= nf2 'x  "absorption: x /\\ (x \\/ y) = x")

;; Idempotence: (or x x) collapses to x.
(define-values (nf3 tr3) (normalize '(or x x)))
(check= nf3 'x  "idempotence: x \\/ x = x")

;; Commutativity canonicalizes operand order.
(define-values (nf4 tr4) (normalize '(and b a)))
(check= nf4 '(and a b)  "commutativity: alphabetical")

;; ----------------------------------------------------------------
;; Part 5: Trace inspection.
;;
;; Each step is a <rewrite-step> record with four accessors:
;;   step-rule-name   : the axiom that fired
;;   step-general-form: the rule in math notation
;;   step-before      : the subterm before rewriting
;;   step-after       : the subterm after rewriting
;;
;; `format-trace` renders the whole trace for human reading.
;; ----------------------------------------------------------------

(define-values (nf-inspect tr-inspect) (normalize '(not (not x))))
(check= (length tr-inspect) 1  "double-not -> x is one step")

(define step (car tr-inspect))
(check-true (rewrite-step? step)            "trace entry is a rewrite-step")
(check= (step-before step) '(not (not x))   "step-before shows input")
(check= (step-after  step)  'x              "step-after shows output")

;; The rule name is a string. For the negation axiom it is "negation".
(check-true (string? (step-rule-name step))     "rule name is a string")
(check-true (string? (step-general-form step))  "general form is a string")

;; ----------------------------------------------------------------
;; Part 6: What Boolean can do that Heyting cannot.
;;
;; Heyting algebras generalize Boolean: every Boolean algebra is a
;; Heyting algebra, but not vice versa. Heyting models intuitionistic
;; logic; Boolean models classical logic. The concrete difference:
;; Boolean has negation laws (not not x = x, De Morgan, complement),
;; Heyting does not.
;; ----------------------------------------------------------------

;; Project B to a Heyting algebra. Derive its theory -- only lattice
;; axioms, no negation.
(define H (boolean->heyting B))
(check-true (heyting-algebra? H)   "H is a Heyting algebra")

(define heyting-theory (heyting->theory H 'or 'and))
(check-true (theory? heyting-theory)  "Heyting projects to theory")

;; Heyting theory has no `not` operator, so we cannot normalize a term
;; containing `not` against it -- the rules do not know about `not`.
;; Any (not ...) subterm passes through unchanged.
(define normalize-heyting (make-recursive-normalizer heyting-theory proto))

(define-values (hnf-involution ht-involution) (normalize-heyting '(not (not x))))
(check= hnf-involution '(not (not x))
        "Heyting leaves (not (not x)) alone -- no double-negation axiom")

;; Absorption, idempotence, commutativity still work under Heyting
;; (they come from the underlying lattice).
(define-values (hnf-abs ht-abs) (normalize-heyting '(and x (or x y))))
(check= hnf-abs 'x  "Heyting still simplifies absorption")

(define-values (hnf-comm ht-comm) (normalize-heyting '(or b a)))
(check= hnf-comm '(or a b)  "Heyting still canonicalizes commutativity")

;; ----------------------------------------------------------------
;; Part 7: The `symbolic-boolean-normalize` shortcut.
;;
;; The library bundles the most common pattern ("normalize this S-expr
;; as a Boolean term using a standard theory") into a single procedure.
;; It uses operators `and`, `or`, `not`. Internally it builds the same
;; `boolean->theory` projection, but over a two-element {#f, #t} Boolean
;; algebra, so a contradiction normalizes to #f and a tautology to #t.
;;
;; Returns (values result trace) like the general normalizer. Use it
;; when you want Boolean simplification without choosing operator names
;; or building a Boolean algebra manually.
;; ----------------------------------------------------------------

(define-values (sbn1 sbt1) (symbolic-boolean-normalize '(not (not x))))
(check= sbn1 'x  "shortcut: double-not eliminated")

(define-values (sbn2 sbt2) (symbolic-boolean-normalize '(and x (or x y))))
(check= sbn2 'x  "shortcut: absorption simplified")

(define-values (sbn3 sbt3) (symbolic-boolean-normalize '(or b a)))
(check= sbn3 '(or a b)  "shortcut: commutativity canonicalized")

;; `symbolic-boolean-equivalent?` returns #t if two terms normalize
;; to the same form under the Boolean theory.
(check-true  (symbolic-boolean-equivalent? '(not (not x)) 'x)
             "not-not-x equivalent to x")
(check-true  (symbolic-boolean-equivalent? '(and x (or x y)) 'x)
             "absorption equivalence")
(check-false (symbolic-boolean-equivalent? '(and x y) 'x)
             "and-x-y not equivalent to x under this theory")

;; ----------------------------------------------------------------
;; Part 8: De Morgan, complement laws, and the bounds.
;;
;; The 7-axiom theory applies De Morgan, complement laws, and bound
;; folding, so these terms simplify fully. A contradiction collapses to
;; bottom (#f) and a tautology to top (#t). What it still does NOT do is
;; distribute -- there is no distributivity axiom -- so it is not a
;; Boolean decision procedure. Reach for (wile algebra sat) when you need
;; one.
;; ----------------------------------------------------------------

;; De Morgan rewrites (not (and x y)) to (or (not x) (not y)).
(define-values (de-morgan-nf _dm-trace)
  (symbolic-boolean-normalize '(not (and x y))))
(check= de-morgan-nf '(or (not x) (not y))
        "De Morgan: not(x and y) = (not x) or (not y)")

;; The complement law collapses a contradiction to bottom (#f)...
(define-values (comp-nf _c-trace)
  (symbolic-boolean-normalize '(and x (not x))))
(check= comp-nf #f
        "complement: x and not-x = bottom (#f)")

;; ...and a tautology to top (#t). Even n-way contradictions collapse,
;; because AC normalization folds complement over the whole flattened
;; operand list -- not just two adjacent operands.
(define-values (taut-nf _t-trace)
  (symbolic-boolean-normalize '(or x (not x))))
(check= taut-nf #t
        "complement: x or not-x = top (#t)")

(define-values (nway-nf _n-trace)
  (symbolic-boolean-normalize '(and a (and b (not a)))))
(check= nway-nf #f
        "n-way contradiction also collapses to bottom")

(display "chapter 04 complete") (newline)
