;;; algebra-closure-test.scm -- Closure operator tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra closure))

(test-begin "closure-operators")

;; ---- Test fixture: powerset-lattice {1,2,3} ----
;; Closure: if 1 is in the set, add everything.
;; cl(S) = S if 1 not in S, else {1,2,3}.

(define L (powerset-lattice '(1 2 3)))

(define (if-1-add-all s)
  (if (member 1 s)
      '(1 2 3)
      s))

(define C (make-closure-operator if-1-add-all L))

;; ---- Helper: set-equal? ----
;; Order-insensitive list-as-set equality.

(define (subset? a b)
  (cond ((null? a) #t)
        ((member (car a) b) (subset? (cdr a) b))
        (else #f)))

(define (set-equal? a b)
  (and (subset? a b) (subset? b a)))

;; ---- construction ----

(test-group "construction"
  (test #t (closure-operator? C))
  (test #f (closure-operator? 42))
  (test #f (closure-operator? L))
  (test #t (lattice? (closure-lattice C))))

;; ---- closure-close ----

(test-group "closure-close"
  ;; 1 present => everything
  (test #t (set-equal? '(1 2 3) (closure-close C '(1))))
  (test #t (set-equal? '(1 2 3) (closure-close C '(1 2))))
  (test #t (set-equal? '(1 2 3) (closure-close C '(1 3))))
  (test #t (set-equal? '(1 2 3) (closure-close C '(1 2 3))))
  ;; 1 absent => unchanged
  (test '() (closure-close C '()))
  (test '(2) (closure-close C '(2)))
  (test '(3) (closure-close C '(3)))
  (test '(2 3) (closure-close C '(2 3)))
  ;; idempotent: cl(cl(x)) = cl(x)
  (test #t (set-equal? (closure-close C '(1))
                        (closure-close C (closure-close C '(1)))))
  (test #t (set-equal? (closure-close C '(2))
                        (closure-close C (closure-close C '(2))))))

;; ---- closure-closed? ----

(test-group "closure-closed?"
  ;; Fixed points: sets without 1, plus {1,2,3}
  (test #t (closure-closed? C '()))
  (test #t (closure-closed? C '(2)))
  (test #t (closure-closed? C '(3)))
  (test #t (closure-closed? C '(2 3)))
  (test #t (closure-closed? C '(1 2 3)))
  ;; Not fixed points: sets containing 1 but not everything
  (test #f (closure-closed? C '(1)))
  (test #f (closure-closed? C '(1 2)))
  (test #f (closure-closed? C '(1 3))))

;; ---- closed-elements ----

(test-group "closed-elements"
  (let ((samples '(() (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3))))
    ;; Closed: (), (2), (3), (2 3), (1 2 3)
    ;; Not closed: (1), (1 2), (1 3)
    (test 5 (length (closed-elements C samples)))
    ;; Verify each closed element is actually closed
    (for-each
      (lambda (e)
        (test #t (closure-closed? C e)))
      (closed-elements C samples))))

;; ---- closed-lattice ----

(test-group "closed-lattice"
  (let ((CL (closure->closed-lattice C '(() (2) (3) (2 3) (1 2 3)))))
    (test #t (lattice? CL))
    ;; bottom = cl({}) = {} (1 not in {})
    (test '() (lattice-bottom CL))
    ;; top = cl({1,2,3}) = {1,2,3}
    (test #t (set-equal? '(1 2 3) (lattice-top CL)))
    ;; join: inherited from L (union)
    (test #t (set-equal? '(2 3) (lattice-join CL '(2) '(3))))
    ;; meet: cl(meet_L(a,b)) -- intersection then close
    ;; meet({2,3}, {1,2,3}) = cl(intersect({2,3},{1,2,3})) = cl({2,3}) = {2,3}
    (test #t (set-equal? '(2 3) (lattice-meet CL '(2 3) '(1 2 3))))
    ;; meet({2}, {3}) = cl(intersect({2},{3})) = cl({}) = {}
    (test '() (lattice-meet CL '(2) '(3)))
    ;; leq: inherited from L (subset)
    (test #t (lattice-leq? CL '(2) '(2 3)))
    (test #f (lattice-leq? CL '(2 3) '(2)))))

;; ---- downward-closure ----

(test-group "downward-closure"
  (let* ((po (make-partial-order <=))
         (DC (downward-closure-operator po '(1 2 3 4 5))))
    (test #t (closure-operator? DC))
    ;; cl({3}) = {1,2,3} (everything <= 3)
    (test #t (set-equal? '(1 2 3) (closure-close DC '(3))))
    ;; cl({1}) = {1} (only 1 <= 1)
    (test #t (set-equal? '(1) (closure-close DC '(1))))
    ;; cl({5}) = {1,2,3,4,5} (everything)
    (test #t (set-equal? '(1 2 3 4 5) (closure-close DC '(5))))
    ;; cl({}) = {} (nothing below nothing)
    (test '() (closure-close DC '()))
    ;; cl({2,4}) = {1,2,3,4}
    (test #t (set-equal? '(1 2 3 4) (closure-close DC '(2 4))))
    ;; Fixed points: downward-closed sets
    (test #t (closure-closed? DC '()))
    (test #t (closure-closed? DC '(1)))
    (test #t (closure-closed? DC '(1 2)))
    (test #t (closure-closed? DC '(1 2 3)))
    (test #f (closure-closed? DC '(2 3)))  ; missing 1
    (test #f (closure-closed? DC '(3)))))  ; missing 1,2

;; ---- validate-closure-operator ----

(test-group "validate-closure-operator"
  ;; Valid: our if-1-add-all closure
  (test #t (validate-closure-operator C '(() (1) (2) (3) (1 2) (2 3) (1 2 3))))
  ;; Valid: downward closure
  (let* ((po (make-partial-order <=))
         (DC (downward-closure-operator po '(1 2 3))))
    (test #t (validate-closure-operator DC '(() (1) (2) (3) (1 2) (2 3) (1 2 3)))))
  ;; Invalid: non-extensive operator that shrinks
  (let ((bad (make-closure-operator
               (lambda (s) '())  ; maps everything to empty -- not extensive
               L)))
    (test #f (eq? #t (validate-closure-operator bad '((1) (2) (1 2 3)))))))

;; ---- with-closure macro ----

(test-group "with-closure"
  (with-closure C (close lattice)
    ;; close works
    (test #t (set-equal? '(1 2 3) (close '(1))))
    (test '(2) (close '(2)))
    ;; lattice is the underlying lattice
    (test #t (lattice? lattice))
    (test '() (lattice-bottom lattice))
    (test #t (set-equal? '(1 2 3) (lattice-top lattice)))))

(test-end)
(test-exit)
