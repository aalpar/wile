;; ================================================================
;; Chapter 03 -- Rewriting basics: axioms and normalizers
;;
;; What you will learn:
;;   - What a *term protocol* is and why rewriting is defined against it.
;;   - The seven core single-operator axiom types and the shape of each
;;     (chapter 04 adds the AC, De Morgan, and negation axioms).
;;   - How `axiom->rules` compiles axioms into rewrite procedures.
;;   - How `make-normalizer` composes several axioms into one
;;     single-step rewriter and what its #f return means.
;;
;; Prerequisites: chapters 01, 02 (comfortable with records and
;;   algebraic structures).
;; Sub-libraries used: (wile algebra rewrite).
;; ================================================================

(import (scheme base) (scheme write) (wile algebra rewrite))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: The term protocol.
;;
;; Rewriting does not hardcode any syntax. Instead, it takes a protocol
;; that says "here is how to recognize a compound term, extract the
;; operator, extract the operands, rebuild the term with new operands,
;; and compare two operands."
;;
;; Five fields, all closures. Nothing about s-expressions built in --
;; we could just as easily use records, vectors, or something else.
;; This chapter uses ordinary list-style s-expressions like (+ x 0).
;; ----------------------------------------------------------------

(define (sexp-before? a b)
  ;; A total order on terms: numbers before symbols before pairs.
  ;; Within each kind, use the natural order.
  (cond
    ((and (number? a) (number? b)) (< a b))
    ((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
    ((and (pair? a) (pair? b))     (string<? (call-with-port (open-output-string)
                                                (lambda (p) (write a p) (get-output-string p)))
                                              (call-with-port (open-output-string)
                                                (lambda (p) (write b p) (get-output-string p)))))
    ((number? a) #t)                 ; numbers before everything else
    ((number? b) #f)
    ((symbol? a) #t)                 ; symbols before pairs
    (else #f)))

(define sexp-proto
  (make-term-protocol
    pair?                                         ; compound?
    car                                           ; operator
    cdr                                           ; operands
    (lambda (t new) (cons (car t) new))           ; rebuild
    sexp-before?))                                ; compare

(check-true (term-protocol? sexp-proto)           "term protocol built")
(check-true (term-compound? sexp-proto '(+ 1 2))  "(+ 1 2) is compound")
(check-false (term-compound? sexp-proto 42)       "42 is not compound")
(check= (term-get-operator sexp-proto '(+ 1 2)) '+ "extract operator")
(check= (term-get-operands sexp-proto '(+ 1 2)) '(1 2) "extract operands")
(check= (term-make-term sexp-proto '(+ 1 2) '(3 4)) '(+ 3 4) "rebuild term")

;; ----------------------------------------------------------------
;; Part 2: Identity axiom -- (+ x 0) and (+ 0 x) both collapse to x.
;;
;; Identity axioms take the operator symbol and a *predicate* on the
;; identity element (not the element itself). Predicates make it easy
;; to match multiple literal representations of zero -- both 0 and 0.0
;; can count as the additive identity.
;; ----------------------------------------------------------------

(define plus-id   (make-identity-axiom '+ (lambda (x) (eqv? x 0))))
(check-true (identity-axiom? plus-id)             "identity axiom recognized")
(check-true (axiom? plus-id)                      "identity axiom is an axiom")
(check-false (directional-axiom? plus-id)         "identity is symmetric, not directional")

(define norm1 (make-normalizer (list plus-id) sexp-proto))

(check= (norm1 '(+ x 0))  'x   "(+ x 0) fires -> x")
(check= (norm1 '(+ 0 y))  'y   "(+ 0 y) fires -> y")
(check= (norm1 '(+ a b))  #f   "(+ a b) does not fire")
(check= (norm1 '(* a 0))  #f   "wrong operator does not fire")
(check= (norm1 'x)        #f   "non-compound term does not fire")

;; ----------------------------------------------------------------
;; Part 3: Commutativity axiom -- (+ 2 x) rewrites to (+ x 2)?
;; Wrong direction. It actually rewrites to (+ x 2) -> (+ 2 x) when the
;; first operand sorts *after* the second. The purpose is a *canonical*
;; form: arguments in a fixed order. Whether to flip depends on the
;; protocol's compare function.
;; ----------------------------------------------------------------

(define plus-comm (make-commutativity-axiom '+))
(check-true (commutativity-axiom? plus-comm)      "commutativity axiom recognized")

(define norm2 (make-normalizer (list plus-comm) sexp-proto))

;; Numbers sort before symbols per our sexp-before?. So (+ y 2) should
;; be rewritten to (+ 2 y) -- moving the number to the front.
(check= (norm2 '(+ y 2))  '(+ 2 y)  "commutativity moves number forward")

;; (+ 2 y) is already canonical. No rewrite.
(check= (norm2 '(+ 2 y))  #f        "already canonical: no firing")

;; (+ b a) -> (+ a b).
(check= (norm2 '(+ b a))  '(+ a b)  "alphabetical canonicalization")

;; ----------------------------------------------------------------
;; Part 4: Absorbing element -- (* x 0) rewrites to 0.
;;
;; Absorbing axioms collapse the whole term to the absorbing element.
;; 0 absorbs under multiplication, #f absorbs under conjunction, #t
;; absorbs under disjunction.
;; ----------------------------------------------------------------

(define times-absorb (make-absorbing-axiom '* (lambda (x) (eqv? x 0))))
(check-true (absorbing-axiom? times-absorb)       "absorbing axiom recognized")

(define norm3 (make-normalizer (list times-absorb) sexp-proto))

(check= (norm3 '(* x 0))  0   "(* x 0) absorbs -> 0")
(check= (norm3 '(* 0 y))  0   "(* 0 y) absorbs -> 0")
(check= (norm3 '(* a b))  #f  "no absorbing element present")

;; ----------------------------------------------------------------
;; Part 5: Idempotence -- (max x x) rewrites to x.
;;
;; Pure idempotence: f(x,x) = x. Meet and join on lattices are
;; idempotent. Boolean AND and OR are idempotent.
;; ----------------------------------------------------------------

(define and-idemp (make-idempotence-axiom 'and))
(check-true (idempotence-axiom? and-idemp)        "idempotence axiom recognized")

(define norm4 (make-normalizer (list and-idemp) sexp-proto))

(check= (norm4 '(and x x))   'x   "(and x x) collapses to x")
(check= (norm4 '(and x y))   #f   "distinct args: no collapse")
(check= (norm4 '(or x x))    #f   "wrong operator: no firing")

;; ----------------------------------------------------------------
;; Part 6: Involution -- f(f(x)) rewrites to x.
;;
;; Double negation, double complement, two swaps of a pair.
;; ----------------------------------------------------------------

(define neg-inv (make-involution-axiom 'neg))
(check-true (involution-axiom? neg-inv)           "involution axiom recognized")

(define norm5 (make-normalizer (list neg-inv) sexp-proto))

(check= (norm5 '(neg (neg x)))        'x          "double neg collapses")
(check= (norm5 '(neg x))              #f          "single neg: no firing")
(check= (norm5 '(neg (abs x)))        #f          "different inner op: no firing")

;; ----------------------------------------------------------------
;; Part 7: Absorption -- (and x (or x y)) rewrites to x.
;;
;; Absorption laws link two operations in a lattice:
;;   x /\ (x \/ y) = x
;;   x \/ (x /\ y) = x
;;
;; The axiom takes *two* operator names, outer and inner.
;; ----------------------------------------------------------------

(define and-or-abs (make-absorption-axiom 'and 'or))
(check-true (absorption-axiom? and-or-abs)        "absorption axiom recognized")

(define norm6 (make-normalizer (list and-or-abs) sexp-proto))

(check= (norm6 '(and x (or x y)))  'x      "and absorbs or: (x /\\ (x \\/ y)) = x")
(check= (norm6 '(and x (or y z)))  #f      "no shared subterm: no firing")

;; ----------------------------------------------------------------
;; Part 8: Associativity -- directional, right-to-left.
;;
;; Unlike the other six axioms, associativity is directional: it always
;; reassociates a specific direction. The library implements
;; right-associativity: (+ (+ a b) c) gets rewritten to (+ a (+ b c));
;; the already-right-associated form (+ a (+ b c)) is left alone.
;; ----------------------------------------------------------------

(define plus-assoc (make-associativity-axiom '+))
(check-true (associativity-axiom? plus-assoc)     "associativity axiom recognized")
(check-true (directional-axiom? plus-assoc)       "associativity is directional")

(define norm7 (make-normalizer (list plus-assoc) sexp-proto))

(check= (norm7 '(+ (+ a b) c))  '(+ a (+ b c))  "right-associate")
(check= (norm7 '(+ a (+ b c)))  #f              "already right-associated")

;; ----------------------------------------------------------------
;; Part 9: Composing several axioms into one normalizer.
;;
;; `make-normalizer` takes a *list* of axioms. The compiled rules run
;; in order; the first matching rule fires. Use this to build a full
;; simplification pass over several axioms at once.
;;
;; Because `make-normalizer` is single-step, a full simplifier needs
;; to loop: keep applying until the result is #f. That loop lives in
;; `make-recursive-normalizer` from (wile algebra symbolic) -- chapter 4
;; covers it.
;; ----------------------------------------------------------------

(define full-plus-times
  (make-normalizer
    (list plus-id plus-comm times-absorb)
    sexp-proto))

(check= (full-plus-times '(+ x 0))  'x     "identity fires")
(check= (full-plus-times '(+ y 2))  '(+ 2 y) "commutativity fires")
(check= (full-plus-times '(* x 0))  0      "absorbing fires")
(check= (full-plus-times '(- 3 x))  #f     "no axiom for - : no firing")

;; ----------------------------------------------------------------
;; Part 10: Manual fixpoint loop -- "simplify until stable."
;;
;; Chapter 4 shows the library's traced recursive normalizer. For now,
;; here is the idea in ten lines.
;; ----------------------------------------------------------------

(define (simplify-fully norm term)
  (let loop ((current term))
    (let ((next (norm current)))
      (if next (loop next) current))))

(check= (simplify-fully full-plus-times '(+ y 2))  '(+ 2 y)  "one step to normal form")
(check= (simplify-fully full-plus-times '(+ x 0))  'x        "one step to literal")
(check= (simplify-fully full-plus-times '(foo))   '(foo)     "already normal")

;; ----------------------------------------------------------------
;; Part 11: Exercises.
;; ----------------------------------------------------------------

;; (check= (full-plus-times '(+ 5 0))  <?>  "identity with number")
;; (check= (full-plus-times '(+ x 0) 'x? 'y?)  <?>  "two operands already ok")

(display "chapter 03 complete") (newline)
