;; ================================================================
;; Chapter 10 -- Unification: syntactic and AC-modulo
;;
;; What you will learn:
;;   - Pattern variables as records (`<pattern-var>`), and the `?x`
;;     convention that `parse-pattern` sugars into those records.
;;   - Substitutions: `empty-substitution`, `substitution-lookup`,
;;     `substitution-compose`, `substitution-apply`.
;;   - Ordinary (positional) unification via `ac-unify` with an empty
;;     AC-theory.
;;   - AC unification: `+` and `*` as associative-commutative operators.
;;     The result is a CSU (complete set of unifiers) -- a list.
;;   - `diophantine-basis`: the standalone number-theoretic primitive
;;     behind Stickel's AC algorithm.
;;
;; Prerequisites: chapter 03 (rewrite, term protocols), chapter 04
;;   (theories, named axioms).
;; Sub-libraries used:
;;   (wile algebra rewrite), (wile algebra symbolic),
;;   (wile algebra unification).
;; ================================================================

(import (scheme base) (scheme write)
        (wile algebra rewrite)
        (wile algebra symbolic)
        (wile algebra unification))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: Pattern variables.
;;
;; A pattern variable is a record, not a bare symbol. This lets the
;; library distinguish pattern variables from ordinary operator names
;; without relying on string-prefix heuristics at match time.
;; ----------------------------------------------------------------

(define ?x (make-pattern-var 'x))
(define ?y (make-pattern-var 'y))

(check-true (pattern-var? ?x)                      "?x is a pattern-var")
(check-false (pattern-var? 'x)                     "'x (symbol) is not")
(check= (pattern-var-name ?x)  'x                  "name accessor")

;; ----------------------------------------------------------------
;; Part 2: parse-pattern -- the ?x convention.
;;
;; Writing (make-pattern-var 'x) everywhere is painful. `parse-pattern`
;; walks an S-expression and replaces each `?name` symbol with a
;; pattern-var of that name. Repeated `?name` symbols intern to the
;; same record (so `(? x) + (? x)` is "the same variable twice").
;; ----------------------------------------------------------------

(define pat1 (parse-pattern '(+ ?x ?y)))
(check-true (pair? pat1)                           "parsed pattern is compound")
(check= (car pat1) '+                              "operator preserved")
(check-true (pattern-var? (cadr pat1))             "first operand became pattern-var")
(check= (pattern-var-name (cadr pat1))  'x         "first var is ?x")
(check-true (pattern-var? (caddr pat1))            "second operand became pattern-var")
(check= (pattern-var-name (caddr pat1))  'y        "second var is ?y")

;; Repeated occurrences share identity.
(define pat2 (parse-pattern '(+ ?x ?x)))
(check-true (eq? (cadr pat2) (caddr pat2))
            "repeated ?x interns to same record")

;; ----------------------------------------------------------------
;; Part 3: Substitutions.
;;
;; An empty substitution, lookups, composition, and application.
;; A substitution is a map from pattern variables to terms.
;; ----------------------------------------------------------------

(check-true (substitution? empty-substitution)    "empty-substitution exists")
(check= (substitution-lookup empty-substitution ?x)  #f
        "lookup in empty is #f")

;; Building a substitution via make-substitution is just cons-ing bindings.
(define sub1 (make-substitution (list (cons ?x 'a))))
(check= (substitution-lookup sub1 ?x)  'a          "lookup after binding")
(check= (substitution-lookup sub1 ?y)  #f          "?y still unbound")

;; ----------------------------------------------------------------
;; Part 4: Applying a substitution to a term.
;;
;; Substitution-apply walks a pattern and replaces pattern-vars with
;; their bindings. Terms without pattern-vars pass through unchanged.
;; ----------------------------------------------------------------

;; The unification library uses `term-compare` as a *3-way* comparator:
;; it calls `(zero? (term-compare ...))` to test equality. A boolean
;; comparator (as chapter 3 used for rewriting) would fail here. Use a
;; compare returning -1 / 0 / 1.
(define (three-way-compare a b)
  (cond
    ((and (number? a) (number? b))
     (cond ((< a b) -1) ((> a b) 1) (else 0)))
    ((and (symbol? a) (symbol? b))
     (let ((sa (symbol->string a)) (sb (symbol->string b)))
       (cond ((string<? sa sb) -1) ((string>? sa sb) 1) (else 0))))
    ((equal? a b) 0)
    (else 1)))

(define proto (sexp-term-protocol three-way-compare))

(check= (substitution-apply sub1 proto (parse-pattern '(+ ?x 1)))
        '(+ a 1)
        "apply replaces ?x with a")

(check= (substitution-apply empty-substitution proto (parse-pattern '(+ ?x 1)))
        (parse-pattern '(+ ?x 1))
        "empty substitution: term unchanged")

;; ----------------------------------------------------------------
;; Part 5: Ordinary (positional) unification via ac-unify with an
;; empty AC-theory.
;;
;; When the theory has no AC operators, ac-unify falls back to
;; syntactic unification. The CSU has either 0 or 1 element.
;; ----------------------------------------------------------------

(define empty-theory (make-theory '() '()))

;; Unifying (f ?x) with (f a): ?x <- a.
(define csu1
  (ac-unify (parse-pattern '(f ?x))
            '(f a)
            empty-theory
            proto))
(check= (length csu1)  1                           "1 unifier for (f ?x) = (f a)")
(check= (substitution-lookup (car csu1) ?x)  'a    "?x bound to a")

;; Unifying (f a) with (f b): no unifier (different ground terms).
(define csu2 (ac-unify '(f a) '(f b) empty-theory proto))
(check= csu2 '()                                   "no unifier for (f a) = (f b)")

;; Unifying (f ?x) with (g ?y): no unifier (different operators).
(define csu3 (ac-unify (parse-pattern '(f ?x)) (parse-pattern '(g ?y))
                       empty-theory proto))
(check= csu3 '()                                   "no unifier for (f ?x) = (g ?y)")

;; ----------------------------------------------------------------
;; Part 6: AC unification -- `+` as associative-commutative.
;;
;; Build a theory whose `+` operator is AC. Unifying modulo AC
;; means a + b unifies with b + a.
;; ----------------------------------------------------------------

(define ac-plus-theory
  (make-theory
    (list (make-named-axiom "comm-+"
                            "a + b = b + a"
                            (make-commutativity-axiom '+))
          (make-named-axiom "assoc-+"
                            "(a + b) + c = a + (b + c)"
                            (make-associativity-axiom '+)))
    '(+)))  ; `+` is associative

;; Match (?x + a) against (a + b). Under AC, ?x can bind to b (from the
;; second position) because + is commutative. ac-match returns the list.
(define match-result
  (ac-match (parse-pattern '(+ ?x a))
            '(+ a b)
            ac-plus-theory
            proto))

(check-true (> (length match-result) 0)
            "AC match finds at least one unifier")

;; At least one of the returned substitutions binds ?x to b.
(define (binds-to sub var term)
  (equal? (substitution-lookup sub var) term))
(check-true (memq #t (map (lambda (s) (binds-to s ?x 'b)) match-result))
            "some unifier binds ?x -> b")

;; ----------------------------------------------------------------
;; Part 7: diophantine-basis -- the Stickel kernel.
;;
;; AC unification of flat operand multisets reduces to a non-negative
;; integer linear system. diophantine-basis enumerates minimal
;; non-negative solutions of a·u = b·v.
;;
;; Classic instance: 2u = 3v. Minimal solution is (u, v) = (3, 2).
;; The basis is ((3) . (2)) -- one solution, since gcd(2,3) = 1.
;; ----------------------------------------------------------------

(define basis-2-3 (diophantine-basis '(2) '(3)))
(check= (length basis-2-3)  1                      "one minimal solution to 2u = 3v")
(check= (car basis-2-3)  '((3) . (2))
        "(u, v) = (3, 2) is the minimal solution")

;; 2u = 2v: trivial identity u = v. Minimal solution: (1, 1).
(define basis-2-2 (diophantine-basis '(2) '(2)))
(check= basis-2-2  '(((1) . (1)))                  "2u = 2v: minimal (1, 1)")

;; 2u + 3v = 5w: multiple minimal solutions.
(define basis-multi (diophantine-basis '(2 3) '(5)))
(check-true (> (length basis-multi) 0)             "has at least one basis vector")

;; ----------------------------------------------------------------
;; Part 8: Substitution composition.
;;
;; (compose s1 s2) produces the substitution that applies s2 first,
;; then s1 -- order matters for nested bindings.
;; ----------------------------------------------------------------

(define sub-xb (make-substitution (list (cons ?x 'b))))
(define sub-ya (make-substitution (list (cons ?y 'a))))

(define composed (substitution-compose sub-xb sub-ya))
(check= (substitution-lookup composed ?x)  'b      "composed: ?x -> b")
(check= (substitution-lookup composed ?y)  'a      "composed: ?y -> a")

;; ----------------------------------------------------------------
;; Part 9: flatten-ac -- flattening nested AC applications.
;;
;; `(+ (+ a b) c)` and `(+ a (+ b c))` and `(+ a b c)` all represent
;; the same multiset {a, b, c} under AC. flatten-ac returns that list.
;; ----------------------------------------------------------------

(check= (flatten-ac '(+ (+ a b) c) '+ proto)  '(a b c)
        "left-associated flattens")
(check= (flatten-ac '(+ a (+ b c)) '+ proto)  '(a b c)
        "right-associated flattens")

;; Non-compound terms become singletons.
(check= (flatten-ac 'a '+ proto)  '(a)            "atom becomes singleton")

;; Wrong operator: treated as opaque.
(check= (flatten-ac '(* a b) '+ proto)  '((* a b))
        "non-matching operator stays intact")

(display "chapter 10 complete") (newline)
