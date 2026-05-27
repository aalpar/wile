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

(test-group "semiring-eq? — default is equal?"
  (let ((S (make-semiring + * 0 1)))
    (test #t (semiring-eq? S 3 3))
    (test #f (semiring-eq? S 3 4))
    ;; equal? handles compound values; default semiring-eq? inherits that.
    (test #t (semiring-eq? S '(1 2) '(1 2)))))

(test-group "semiring-eq? — built-ins use natural equality"
  ;; Boolean uses eq?: #t/#f are interned singletons.
  (let ((B (boolean-semiring)))
    (test #t (semiring-eq? B #t #t))
    (test #t (semiring-eq? B #f #f))
    (test #f (semiring-eq? B #t #f)))
  ;; Tropical uses tropical-eq?: handles tropical-inf + finite numerics.
  (let ((T (tropical-semiring)))
    (test #t (semiring-eq? T tropical-inf tropical-inf))
    (test #f (semiring-eq? T tropical-inf 5))
    (test #f (semiring-eq? T 5 tropical-inf))
    (test #t (semiring-eq? T 5 5))
    (test #f (semiring-eq? T 5 7)))
  ;; Counting uses =: works on bignums via in-place big.Int compare.
  (let ((C (counting-semiring)))
    (test #t (semiring-eq? C 3 3))
    (test #f (semiring-eq? C 3 4))
    (test #t (semiring-eq? C (expt 2 100) (expt 2 100)))
    (test #f (semiring-eq? C (expt 2 100) (- (expt 2 100) 1)))))

(test-group "semiring-eq? — custom predicate via opts"
  ;; Verify the custom predicate is actually consulted, not equal?.
  (let* ((calls   0)
         (counting-eq? (lambda (a b)
                         (set! calls (+ calls 1))
                         (equal? a b)))
         (S (make-semiring + * 0 1 (cons 'eq? counting-eq?))))
    (semiring-eq? S 1 1)
    (semiring-eq? S 1 2)
    (test 2 calls)))

(test-group "semiring-eq? — tolerance-based equality"
  ;; The headline non-equal? use case: tolerance on floats.
  (let ((S (make-semiring + * 0.0 1.0
                          (cons 'eq?
                                (lambda (a b) (< (abs (- a b)) 1e-9))))))
    (test #t (semiring-eq? S 1.0 1.0000000001))
    (test #f (semiring-eq? S 1.0 1.1))))

(test-group "make-semiring — :eq? opt validation"
  ;; Unknown opts key is rejected.
  (test-error (make-semiring + * 0 1 '(eqq? . equal?)))
  ;; Non-procedure :eq? raises via assert-procedure.
  (test-error (make-semiring + * 0 1 (cons 'eq? 42))))

(test-group "tropical-eq? — exported helper"
  ;; Available for composition outside the constructor.
  (test #t (tropical-eq? tropical-inf tropical-inf))
  (test #f (tropical-eq? tropical-inf 5))
  (test #f (tropical-eq? 5 tropical-inf))
  (test #t (tropical-eq? 5 5))
  (test #f (tropical-eq? 5 7)))

(test-end)
(test-exit)
