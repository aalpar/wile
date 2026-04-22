;;; algebra-lattice-test.scm — Lattice tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra setoid)
        (wile algebra lattice))

(test-begin "lattices")

;; -- A simple lattice: divisibility on {1,2,3,6} --
;; join = lcm, meet = gcd, bottom = 1, top = 6

(define div-lat
  (make-lattice
    (lambda (a b) (lcm a b))        ; join
    (lambda (a b) (gcd a b))        ; meet
    1                                ; bottom
    6                                ; top
    (lambda (a b) (zero? (modulo b a)))))  ; leq: a divides b

(test-group "construction"
  (test #t (lattice? div-lat))
  (test #f (lattice? 42)))

(test-group "lattice-join"
  (test 6  (lattice-join div-lat 2 3))
  (test 2  (lattice-join div-lat 1 2))
  (test 6  (lattice-join div-lat 2 6)))

(test-group "lattice-meet"
  (test 1  (lattice-meet div-lat 2 3))
  (test 2  (lattice-meet div-lat 2 6))
  (test 3  (lattice-meet div-lat 3 6)))

(test-group "lattice-bottom and lattice-top"
  (test 1 (lattice-bottom div-lat))
  (test 6 (lattice-top div-lat)))

(test-group "lattice-leq?"
  (test #t (lattice-leq? div-lat 1 6))
  (test #t (lattice-leq? div-lat 2 6))
  (test #f (lattice-leq? div-lat 6 2))
  (test #f (lattice-leq? div-lat 2 3)))

(test-group "lattice->partial-order"
  (let ((po (lattice->partial-order div-lat)))
    (test #t (partial-order? po))
    (test #t (po-leq? po 1 6))
    (test #f (po-leq? po 6 1))))

;; -- flat-lattice --

(test-group "flat-lattice"
  (let ((fl (flat-lattice '(a b c) eq?)))
    (test #t (lattice? fl))
    ;; bottom ≤ everything
    (test #t (lattice-leq? fl (lattice-bottom fl) 'a))
    ;; everything ≤ top
    (test #t (lattice-leq? fl 'a (lattice-top fl)))
    ;; elements are incomparable
    (test #f (lattice-leq? fl 'a 'b))
    ;; join of incomparable = top
    (test 'flat-top (lattice-join fl 'a 'b))
    ;; meet of incomparable = bottom
    (test 'flat-bottom (lattice-meet fl 'a 'b))
    ;; join with bottom = identity
    (test 'a (lattice-join fl (lattice-bottom fl) 'a))
    ;; join of same = same
    (test 'a (lattice-join fl 'a 'a))))

;; -- powerset-lattice --

(test-group "powerset-lattice"
  (let ((ps (powerset-lattice '(x y z))))
    (test #t (lattice? ps))
    ;; empty set is bottom
    (test '() (lattice-bottom ps))
    ;; universe is top
    (test '(x y z) (lattice-top ps))
    ;; subset ordering
    (test #t (lattice-leq? ps '() '(x y)))
    (test #t (lattice-leq? ps '(x) '(x y)))
    (test #f (lattice-leq? ps '(x y) '(x)))
    ;; join = union (order may vary, test membership)
    (let ((result (lattice-join ps '(x) '(y))))
      (test #t (and (member 'x result) (member 'y result) #t)))))

;; -- product-lattice --

(test-group "product-lattice"
  (let* ((fl (flat-lattice '(a b) eq?))
         (pl (product-lattice fl fl)))
    (test #t (lattice? pl))
    ;; bottom is (flat-bottom flat-bottom)
    (test (list 'flat-bottom 'flat-bottom) (lattice-bottom pl))
    ;; pointwise join
    (test (list 'a 'b)
      (lattice-join pl
        (list 'a 'flat-bottom)
        (list 'flat-bottom 'b)))))

;; -- fixpoint --

(test-group "fixpoint"
  ;; Fixpoint on powerset: start from empty, add 'x each step until {x y z}
  (let* ((ps (powerset-lattice '(x y z)))
         ;; transfer: add 'x, 'y, 'z one at a time based on what's there
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (let ((result (fixpoint ps f '())))
      ;; Should reach {x y z}
      (test #t (and (member 'x result) (member 'y result)
                    (member 'z result) #t)))))

(test-group "fixpoint-bounded"
  ;; Same as above but with fuel=1, should return #f (not converged)
  (let* ((ps (powerset-lattice '(x y z)))
         (f (lambda (s)
              (cond ((null? s) '(x))
                    ((and (member 'x s) (not (member 'y s)))
                     (cons 'y s))
                    ((and (member 'y s) (not (member 'z s)))
                     (cons 'z s))
                    (else s)))))
    (test #f (fixpoint ps f '() 1))))

;; -- with-lattice macro --

(test-group "with-lattice"
  (test 6 (with-lattice div-lat (join meet bottom top leq?)
            (join (join bottom 2) 3))))

;; -- validate-lattice --

(test-group "validate-lattice"
  (test #t (validate-lattice div-lat '(1 2 3 6))))

;; ─── §5.5 — Phase 1: extended <lattice> metadata ─────────────────

(test-group "extended <lattice> with optional metadata"
  (let ((L (make-lattice
             max min 0 4 <=
             (cons 'cardinality 5)
             (cons 'elements '(0 1 2 3 4))
             (cons 'setoid (numeric-setoid)))))
    (test #t (lattice? L))
    (test 5 (lattice-cardinality L))
    (test '(0 1 2 3 4) (lattice-elements L))
    (test #t (finite-lattice? L))
    (test #t (lattice-equiv? L 2 2))
    (test #f (lattice-equiv? L 2 3))))

(test-group "backward compatibility — 5-arg make-lattice"
  (let ((L (make-lattice max min 0 100 <=)))
    (test #t (lattice? L))
    (test 50 (lattice-join L 20 50))
    (test #t (and (lattice-setoid L) #t))
    (test #f (lattice-cardinality L))
    (test #f (lattice-elements L))
    (test #f (finite-lattice? L))))

(test-end)
(test-exit)
