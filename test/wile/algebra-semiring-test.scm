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

;; ─── Approximate counting variants ───────────

(test-group "modular-counting-semiring — basic operations"
  (let ((S (modular-counting-semiring 7)))
    (test #t (semiring? S))
    (test 'modular (semiring-carrier S))
    (test 0 (semiring-zero S))
    (test 1 (semiring-one S))
    ;; (3+5) mod 7 = 1, (3*5) mod 7 = 1
    (test 1 (semiring-plus S 3 5))
    (test 1 (semiring-times S 3 5))
    ;; modulo canonicalizes negative inputs
    (test 6 (semiring-plus S -2 1))
    ;; Identity laws hold
    (test 3 (semiring-plus S 0 3))
    (test 3 (semiring-times S 1 3))
    (test 0 (semiring-times S 0 5))))

(test-group "modular-counting-semiring — axioms via validate-semiring"
  ;; Small prime where we can spot-check exhaustively.
  (let ((S (modular-counting-semiring 5)))
    (test #t (validate-semiring S '(0 1 2 3 4)))))

(test-group "modular-counting-semiring — construction validation"
  (test-error (modular-counting-semiring 1))            ; P < 2
  (test-error (modular-counting-semiring 0))            ; P = 0
  (test-error (modular-counting-semiring -7))           ; P negative
  (test-error (modular-counting-semiring 7.0))          ; not exact integer
  (test-error (modular-counting-semiring "7")))         ; not a number

(test-group "modular-counting-semiring — large primes via numeric tower"
  ;; mersenne-31 stays in fixnum/int64 range; mersenne-61 promotes
  ;; intermediates to bignum during multiplication, but the result
  ;; collapses back via modulo. Verify both produce canonical values.
  (let ((S31 (modular-counting-semiring mersenne-31))
        (S61 (modular-counting-semiring mersenne-61)))
    ;; Multiplication of values near sqrt(P) on mersenne-31.
    (let* ((a (- (expt 2 15) 1))
           (b (- (expt 2 15) 1))
           (r (semiring-times S31 a b)))
      (test #t (and (>= r 0) (< r mersenne-31))))
    ;; Multiplication of values near P on mersenne-61.
    (let* ((a (- mersenne-61 1))
           (b (- mersenne-61 1))
           (r (semiring-times S61 a b)))
      (test #t (and (>= r 0) (< r mersenne-61))))))

(test-group "modular-counting-semiring — named prime constants"
  (test mersenne-31 (- (expt 2 31) 1))
  (test mersenne-61 (- (expt 2 61) 1))
  (test 2147483647 mersenne-31)
  (test 2305843009213693951 mersenne-61))

(test-group "log-counting-semiring — basic operations"
  (let ((L (log-counting-semiring)))
    (test #t (semiring? L))
    (test 'log-float (semiring-carrier L))
    (test -inf.0 (semiring-zero L))
    (test 0.0 (semiring-one L))
    ;; times = +
    (test 3.0 (semiring-times L 1.0 2.0))
    ;; plus on a -inf.0 operand short-circuits to the other
    (test 5.0 (semiring-plus L -inf.0 5.0))
    (test 5.0 (semiring-plus L 5.0 -inf.0))
    (test -inf.0 (semiring-plus L -inf.0 -inf.0))))

(test-group "log-counting-semiring — log-sum-exp correctness"
  (let ((L (log-counting-semiring)))
    ;; log(2e) = log 2 + log e = log 2 + 1
    (let ((r (semiring-plus L 1.0 1.0)))
      (test #t (< (abs (- r (+ 1.0 (log 2)))) 1e-12)))
    ;; log(e^100 + e^0) ≈ 100 (the smaller term underflows)
    (let ((r (semiring-plus L 100.0 0.0)))
      (test #t (< (abs (- r 100.0)) 1e-12)))
    ;; Symmetric: log(e^0 + e^100) ≈ 100
    (let ((r (semiring-plus L 0.0 100.0)))
      (test #t (< (abs (- r 100.0)) 1e-12)))))

(test-group "log-counting-semiring — no overflow on large counts"
  ;; The whole point: log of e^1000 should not overflow.
  (let ((L (log-counting-semiring)))
    (let ((r (semiring-times L 500.0 500.0)))
      (test 1000.0 r))
    (let ((r (semiring-plus L 1000.0 1000.0)))
      (test #t (< (abs (- r (+ 1000.0 (log 2)))) 1e-9)))))

(test-group "log-counting-semiring — tolerance-based eq?"
  (let ((L (log-counting-semiring)))
    (test #t (semiring-eq? L 5.0 5.0))
    (test #t (semiring-eq? L 5.0 5.0000000000001))     ; within 1e-12
    (test #f (semiring-eq? L 5.0 5.001))               ; outside 1e-12
    (test #t (semiring-eq? L -inf.0 -inf.0))
    (test #f (semiring-eq? L -inf.0 5.0))
    (test #f (semiring-eq? L 5.0 -inf.0))))

(test-group "saturating-counting-semiring — basic operations"
  (let ((S (saturating-counting-semiring 100)))
    (test #t (semiring? S))
    (test 'saturating (semiring-carrier S))
    (test 0 (semiring-zero S))
    (test 1 (semiring-one S))
    ;; Below cap: normal arithmetic.
    (test 80 (semiring-plus S 50 30))
    (test 50 (semiring-times S 5 10))
    ;; Saturation on both operations.
    (test 100 (semiring-plus S 80 50))
    (test 100 (semiring-times S 11 11))                ; 121 saturates
    (test 100 (semiring-plus S 100 1))                 ; once at cap, stay
    (test 100 (semiring-times S 100 100))
    ;; Zero annihilation (even against would-saturate multiplicand).
    (test 0 (semiring-times S 0 1000))
    (test 0 (semiring-times S 1000 0))))

(test-group "saturating-counting-semiring — distributivity at cap"
  ;; The case the plan explicitly locks in: a=b=c=CAP/2+1 verifies that
  ;; distributivity holds even at the saturation boundary. Both lhs
  ;; (a × (b + c)) and rhs ((a × b) + (a × c)) must equal CAP.
  (let* ((cap 10)
         (S (saturating-counting-semiring cap))
         (a 6) (b 6) (c 6)
         (lhs (semiring-times S a (semiring-plus S b c)))
         (rhs (semiring-plus S (semiring-times S a b)
                               (semiring-times S a c))))
    (test cap lhs)
    (test cap rhs)
    (test #t (= lhs rhs))))

(test-group "saturating-counting-semiring — axioms via validate-semiring"
  ;; Spot-check including post-saturation samples.
  (let ((S (saturating-counting-semiring 100)))
    (test #t (validate-semiring S '(0 1 5 11 50 100)))))

(test-group "saturating-counting-semiring — construction validation"
  (test-error (saturating-counting-semiring 0))         ; non-positive
  (test-error (saturating-counting-semiring -10))       ; negative
  (test-error (saturating-counting-semiring 100.0))     ; not exact integer
  (test-error (saturating-counting-semiring "100")))    ; not a number

(test-group "saturating-counting-semiring — pre-check avoids bignum promotion"
  ;; If 11 * 11 = 121 > 100 saturates to 100, the pre-check (a > cap/b)
  ;; must trigger before the multiplication is computed. We can't observe
  ;; this directly, but we can verify the result is correct for cases
  ;; where the multiplication would produce a bignum if not pre-checked.
  (let ((S (saturating-counting-semiring 100)))
    ;; (* (expt 2 50) (expt 2 50)) = 2^100 in exact Scheme would be a
    ;; bignum. The pre-check should short-circuit to cap before producing
    ;; that intermediate.
    (test 100 (semiring-times S (expt 2 50) (expt 2 50)))))

(test-group "bounded-carrier-semiring? predicate"
  (test #t (bounded-carrier-semiring? (saturating-counting-semiring 100)))
  ;; Modular is exact in Z/PZ; values are fingerprints, not approximations.
  (test #f (bounded-carrier-semiring? (modular-counting-semiring 7)))
  ;; Log is bounded precision but unbounded magnitude.
  (test #f (bounded-carrier-semiring? (log-counting-semiring)))
  ;; The exact counting semiring is also not bounded-carrier.
  (test #f (bounded-carrier-semiring? (counting-semiring)))
  (test #f (bounded-carrier-semiring? (boolean-semiring)))
  (test #f (bounded-carrier-semiring? (tropical-semiring)))
  ;; Non-semiring values just return #f rather than erroring.
  (test #f (bounded-carrier-semiring? 42))
  (test #f (bounded-carrier-semiring? '()))
  (test #f (bounded-carrier-semiring? "saturating")))

(test-end)
(test-exit)
