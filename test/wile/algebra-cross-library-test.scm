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
(define no-theory (make-theory '() '()))
(define ac-theory
  (make-theory
    (list (make-named-axiom 'ass-+ #f (make-associativity-axiom '+))
          (make-named-axiom 'com-+ #f (make-commutativity-axiom '+)))
    '(+)))

;; The normalizer is a single step: a rewritten term, or #f when the
;; term is already canonical. A comparator that answers "sorts before"
;; leaves a sorted pair alone; one that answered -1/0/1 would swap
;; (+ a b) into (+ b a) and back on the next call. These rows only show that
;; sorting happens at all.
(test-group "rewrite: commutativity normalizer"
  (let ((norm (make-normalizer comm-theory proto)))
    (test '(+ a b) (norm '(+ b a)))
    (test #f (norm '(+ a b)))
    (test #f (norm '(+ a a)))
    (test #f (norm (norm '(+ b a))))))

;; The AC axiom sorts through rewrite.scm's insertion sort, the one place the
;; shared protocol is consulted on more than a pair.
(test-group "rewrite: AC normalizer sorts with the shared protocol"
  (let ((norm (make-normalizer
                (list (make-ac-axiom '+ #f ac-absent ac-absent #f))
                proto)))
    (test '(+ a (+ b c)) (norm '(+ c (+ b a))))
    (test #f (norm '(+ a (+ b c))))))

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
  (test 0 (length (ac-unify '(+ b c) '(+ a c) ac-theory proto)))
  (let* ((vx (make-pattern-var 'x))
         (results (ac-unify (list '+ vx vx) '(+ a a) ac-theory proto)))
    (test 1 (length results))
    (test 'a (substitution-lookup (car results) vx)))
  (let ((vx (make-pattern-var 'x)))
    (test 0 (length (ac-unify (list '+ vx vx) '(+ a b) ac-theory proto)))))

(test-group "unification: positional ground terms"
  (test 0 (length (ac-unify '(f a) '(f b) no-theory proto)))
  (test 0 (length (ac-unify '(f b) '(f a) no-theory proto)))
  (test 1 (length (ac-unify '(f a) '(f a) no-theory proto))))

(test-group "unification: multisets with duplicates"
  (test 0 (length (ac-unify '(+ a a b) '(+ a b b) ac-theory proto)))
  (test 1 (length (ac-unify '(+ a a b) '(+ b a a) ac-theory proto))))

;; Equality is "neither sorts before the other", so both directions of the
;; comparator are consulted. Atoms that are not symbols order by written form.
(test-group "unification: equality needs both directions"
  (test 1 (length (ac-match 1 1 ac-theory proto)))
  (test 0 (length (ac-match 1 "1" ac-theory proto)))
  (test 0 (length (ac-match 'a 'b ac-theory proto)))
  (test 1 (length (ac-match "s" "s" ac-theory proto))))

;; The comparator contract is enforced, not assumed. Each of these used to
;; match silently (or, for the three-way comparator, be misread).
(define (partial-before? a b)
  (and (string? a) (string? b) (string<? a b)))

(define partial-proto (sexp-term-protocol partial-before?))

(test-group "contract: a partial boolean comparator raises"
  (test-error (ac-match 'a 'b ac-theory partial-proto))
  (test-error (ac-unify '(f a) '(f b) no-theory partial-proto))
  (test-error (ac-unify '(f b) '(f a) no-theory partial-proto))
  (test-error (ac-unify '(+ a b) '(+ c d) ac-theory partial-proto))
  (test-error (ac-match 'a '(+ b c) ac-theory partial-proto)))

;; A tie between two COMPOUNDS that are not equal? is legal: nested AC terms
;; are equal modulo AC without being equal?, so only atom ties are errors.
;; This comparator ranks atoms before compounds and ties every pair of
;; compounds, so (g a) and (g c) tie. Matching them is the over-match filed as
;; the nested-AC row of "Filed by the P6.9 crosscheck"; this row pins only that
;; the tie does not raise.
(define atoms-first-proto
  (sexp-term-protocol
    (lambda (a b)
      (cond
        ((and (symbol? a) (symbol? b)) (string<? (symbol->string a) (symbol->string b)))
        ((symbol? a) #t)
        (else #f)))))

(test-group "contract: a compound/compound tie does not raise"
  (test 1 (length (ac-unify '(+ (g a) b) '(+ b (g c)) ac-theory atoms-first-proto))))

(define-record-type <cell>
  (make-cell v)
  cell?
  (v cell-v))

(test-group "contract: written-form ties between distinct atoms raise"
  (test-error (ac-match (make-cell 1) (make-cell 2) ac-theory proto))
  (test-error (ac-unify (list 'f car) (list 'f cdr) no-theory proto))
  (let ((c (make-cell 1)))
    (test 1 (length (ac-match c c ac-theory proto)))))

;; Only a NUMERIC result is rejected; any other truthy value reads as #t. An
;; order that answers with a list (a memq tail) keeps working in both libraries.
(define (order-by-position a b)
  (let ((order '(a b c)))
    (and (memq a order) (memq b order)
         (memq b (cdr (memq a order))))))

(define memq-proto (sexp-term-protocol order-by-position))

(test-group "contract: a truthy non-boolean comparator is read as #t"
  (test '(+ a b) ((make-normalizer comm-theory memq-proto) '(+ b a)))
  (test #f ((make-normalizer comm-theory memq-proto) '(+ a b)))
  (test 1 (length (ac-match '(+ a b) '(+ b a) ac-theory memq-proto)))
  (test 1 (length (ac-unify '(+ a b) '(+ b a) ac-theory memq-proto))))

(define (three-way a b)
  (cond ((equal? a b) 0) ((sexp-atom-compare a b) -1) (else 1)))

(define three-way-proto (sexp-term-protocol three-way))

(test-group "contract: the old -1/0/1 comparator raises in every consumer"
  (test-error (ac-match 'a 'a ac-theory three-way-proto))
  (test-error (ac-unify '(+ a b) '(+ b a) ac-theory three-way-proto))
  (test-error ((make-normalizer comm-theory three-way-proto) '(+ a b))))

(test-group "contract: a comparator that ranks equal atoms raises"
  (let ((always (sexp-term-protocol (lambda (a b) #t))))
    (test-error (ac-match 'a 'a ac-theory always))
    (test-error (ac-unify 'a 'a ac-theory always))))

(test-end)
(test-exit)
