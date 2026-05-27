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

(test-group "carrier — absent on built-ins without declaration"
  ;; The unannotated built-ins return #f for semiring-carrier; consumer
  ;; fast-path dispatch keys off non-#f symbols.
  (test #f (semiring-carrier (counting-semiring)))
  (test #f (semiring-carrier (boolean-semiring)))
  (test #f (semiring-carrier (tropical-semiring)))
  (test #f (semiring-carrier (make-semiring + * 0 1))))

(test-group "carrier — declared via opts"
  (let ((S (make-semiring + * 0 1 '(carrier . big-int))))
    (test 'big-int (semiring-carrier S))
    ;; Carrier annotation must not perturb arithmetic.
    (test 7 (semiring-plus S 3 4))
    (test 12 (semiring-times S 3 4))))

(test-group "carrier — unknown opt key is rejected"
  ;; validate-opts-keys must catch typos at construction.
  (test-error (make-semiring + * 0 1 '(carriers . big-int))))

(test-group "bigint-counting-semiring"
  (let ((C (bigint-counting-semiring)))
    (test #t (semiring? C))
    (test 'big-int (semiring-carrier C))
    (test 0 (semiring-zero C))
    (test 1 (semiring-one C))
    (test 7 (semiring-plus C 3 4))
    (test 12 (semiring-times C 3 4))
    ;; Auto-promotes on bignum overflow, matching counting-semiring's
    ;; arithmetic semantics — the difference is the carrier annotation,
    ;; not the Scheme-visible arithmetic.
    (test #t (positive? (semiring-times C 99999999999999 100000000000)))))

(test-end)
(test-exit)
