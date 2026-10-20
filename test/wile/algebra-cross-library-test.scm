;;; algebra-cross-library-test.scm — one term protocol, both algebra libraries
;;;
;;; (wile algebra rewrite) and (wile algebra unification) both consume
;;; `term-compare`. The rewrite library's contract is a boolean "sorts
;;; before" predicate; the unification library used to read the same
;;; procedure as a three-way comparator and test it with `zero?`. Each
;;; library's own suite passed over that divergence, so only a protocol
;;; shared between them can see it. The protocol below is built from the
;;; stdlib's own shipped comparator, not a copy of it.

(import (scheme base)
        (chibi test)
        (wile algebra unification)
        (wile algebra rewrite)
        (wile algebra symbolic))

(test-begin "algebra-cross-library")

(define proto (sexp-term-protocol sexp-atom-compare))

(define comm-theory (list (make-commutativity-axiom '+)))
(define ac-theory
  (make-theory
    (list (make-named-axiom 'ass-+ #f (make-associativity-axiom '+))
          (make-named-axiom 'com-+ #f (make-commutativity-axiom '+)))
    '(+)))

;; The normalizer is a single step: a rewritten term, or #f when the
;; term is already canonical. A comparator that answers "sorts before"
;; leaves a sorted pair alone; one that answered -1/0/1 would swap
;; (+ a b) into (+ b a) and back on the next call.
(test-group "rewrite: commutativity normalizer"
  (let ((norm (make-normalizer comm-theory proto)))
    (test '(+ a b) (norm '(+ b a)))
    (test #f (norm '(+ a b)))
    (test #f (norm '(+ a a)))
    (test #f (norm (norm '(+ b a))))))

(test-group "rewrite: normalizer reaches a fixpoint"
  (let ((norm (make-normalizer comm-theory proto)))
    (let loop ((t '(+ b a)) (steps 0))
      (let ((next (norm t)))
        (cond
          ((not next) (test '(+ a b) t) (test #t (<= steps 1)))
          ((> steps 5) (test-assert "normalizer oscillates" #f))
          (else (loop next (+ steps 1))))))))

(test-group "unification: ac-match with the same protocol"
  (test 1 (length (ac-match '(+ a b) '(+ b a) ac-theory proto)))
  (test 1 (length (ac-match '(+ a b) '(+ a b) ac-theory proto)))
  (test 0 (length (ac-match '(+ a b) '(+ a c) ac-theory proto)))
  (test 0 (length (ac-match '(f a b) '(f b a) (make-theory '() '()) proto)))
  (test 1 (length (ac-match '(f a b) '(f a b) (make-theory '() '()) proto))))

(test-group "unification: ac-match binds variables with the same protocol"
  (let* ((vx (make-pattern-var 'x))
         (results (ac-match (list '+ vx 'b) '(+ b a) ac-theory proto)))
    (test 1 (length results))
    (test 'a (substitution-lookup (car results) vx))))

(test-group "unification: ac-unify with the same protocol"
  (test 1 (length (ac-unify '(+ a b) '(+ b a) ac-theory proto)))
  (test 0 (length (ac-unify '(+ a b) '(+ a c) ac-theory proto)))
  (let* ((vx (make-pattern-var 'x))
         (results (ac-unify (list '+ vx vx) '(+ a a) ac-theory proto)))
    (test 1 (length results))
    (test 'a (substitution-lookup (car results) vx))))

;; Atoms that are not symbols order by written form, so equality by
;; antisymmetry must tell 1 from "1" and a from b alike.
(test-group "unification: equality is antisymmetry over the boolean comparator"
  (test 1 (length (ac-match 1 1 ac-theory proto)))
  (test 0 (length (ac-match 1 "1" ac-theory proto)))
  (test 0 (length (ac-match 'a 'b ac-theory proto)))
  (test 1 (length (ac-match "s" "s" ac-theory proto))))

(test-end)
