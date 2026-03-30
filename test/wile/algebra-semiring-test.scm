;;; algebra-semiring-test.scm — Semiring tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra semiring))

(test-begin "semirings")

(test-group "construction"
  (let ((s (make-semiring + * 0 1)))
    (test #t (semiring? s))
    (test #f (semiring? 42))))

(test-group "operations"
  (let ((s (make-semiring + * 0 1)))
    (test 5 (semiring-plus s 2 3))
    (test 6 (semiring-times s 2 3))
    (test 0 (semiring-zero s))
    (test 1 (semiring-one s))))

(test-group "boolean-semiring"
  (let ((bs (boolean-semiring)))
    (test #t  (semiring-plus bs #f #t))
    (test #f  (semiring-plus bs #f #f))
    (test #t  (semiring-times bs #t #t))
    (test #f  (semiring-times bs #t #f))
    (test #f  (semiring-zero bs))
    (test #t  (semiring-one bs))))

(test-group "tropical-semiring"
  (let ((ts (tropical-semiring)))
    ;; plus = min
    (test 2 (semiring-plus ts 2 5))
    (test 2 (semiring-plus ts 5 2))
    ;; times = +
    (test 7 (semiring-times ts 2 5))
    ;; zero = tropical-inf (symbol sentinel preserves exactness for finite values)
    (test tropical-inf (semiring-zero ts))
    ;; one = 0
    (test 0 (semiring-one ts))))

(test-group "counting-semiring"
  (let ((cs (counting-semiring)))
    (test 5 (semiring-plus cs 2 3))
    (test 6 (semiring-times cs 2 3))))

(test-group "semiring->additive-monoid"
  (let* ((s (make-semiring + * 0 1))
         (m (semiring->additive-monoid s)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))))

(test-group "semiring->multiplicative-monoid"
  (let* ((s (make-semiring + * 0 1))
         (m (semiring->multiplicative-monoid s)))
    (test #t (monoid? m))
    (test 1  (monoid-identity m))
    (test 6  (monoid-op m 2 3))))

(test-group "with-semiring"
  (let ((s (make-semiring + * 0 1)))
    (test 11 (with-semiring s (plus times zero one)
               (plus (times 2 3) (times one 5))))))

(test-group "validate-semiring"
  (test #t (validate-semiring (counting-semiring) '(0 1 2 3))))

(test-end)
(test-exit)
