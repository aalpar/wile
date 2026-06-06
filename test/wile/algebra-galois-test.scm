;;; algebra-galois-test.scm — Galois connection tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra interval)
        (wile algebra abstract-domain)
        (wile algebra galois))

(test-begin "galois-connections")

;; Sign abstraction: integers → {neg, zero, pos, top, bottom}

(define sign-lattice
  (make-lattice
    ;; join
    (lambda (a b)
      (cond ((eq? a 'sign-bottom) b)
            ((eq? b 'sign-bottom) a)
            ((eq? a b) a)
            (else 'sign-top)))
    ;; meet
    (lambda (a b)
      (cond ((eq? a 'sign-top) b)
            ((eq? b 'sign-top) a)
            ((eq? a b) a)
            (else 'sign-bottom)))
    'sign-bottom
    'sign-top
    ;; leq?
    (lambda (a b)
      (cond ((eq? a 'sign-bottom) #t)
            ((eq? b 'sign-top) #t)
            ((eq? a b) #t)
            (else #f)))))

(define int-po (make-partial-order <=))

(define sign-gc
  (make-galois-connection
    ;; alpha: int → sign
    (lambda (n)
      (cond ((< n 0) 'neg)
            ((= n 0) 'zero)
            ((> n 0) 'pos)))
    ;; gamma: sign → best concrete representative
    ;; For extensiveness (c ≤ γ(α(c))), gamma must over-approximate.
    (lambda (s)
      (cond ((eq? s 'neg) -1)
            ((eq? s 'zero) 0)
            ((eq? s 'pos) +inf.0)
            ((eq? s 'sign-bottom) 0)
            ((eq? s 'sign-top) +inf.0)))
    int-po
    sign-lattice))

(test-group "construction"
  (test #t (galois-connection? sign-gc))
  (test #f (galois-connection? 42)))

(test-group "gc-alpha"
  (test 'neg  (gc-alpha sign-gc -5))
  (test 'zero (gc-alpha sign-gc 0))
  (test 'pos  (gc-alpha sign-gc 42)))

(test-group "gc-gamma"
  (test -1    (gc-gamma sign-gc 'neg))
  (test 0     (gc-gamma sign-gc 'zero))
  (test +inf.0 (gc-gamma sign-gc 'pos)))

(test-group "gc-accessors"
  (test #t (partial-order? (gc-concrete-po sign-gc)))
  (test #t (lattice? (gc-abstract-lattice sign-gc))))

(test-group "gc-sound?"
  (test #t (gc-sound? sign-gc
             '(-3 -1 0 1 5)           ; concrete samples
             '(neg zero pos))))       ; abstract samples

;;; --- Pre-built interval connection: P(Z) <-> interval -------------------

(test-group "interval-galois-connection — alpha/gamma"
  (let ((gc (interval-galois-connection)))
    (test '(0 . 5) (gc-alpha gc '(0 3 5)))
    (test '(-3 . 2) (gc-alpha gc '(-3 -1 2)))
    (test 'interval-bot (gc-alpha gc '()))
    (test '(0 1 2 3) (gc-gamma gc '(0 . 3)))
    (test '() (gc-gamma gc 'interval-bot))
    ;; typed sentinels for unbounded extents (invertible)
    (test '(all-ge . 0) (gc-gamma gc (cons 0 'pos-inf)))
    (test '(all-le . 5) (gc-gamma gc (cons 'neg-inf 5)))
    (test 'all-int (gc-gamma gc (cons 'neg-inf 'pos-inf)))
    ;; alpha inverts the sentinels — closes the round-trip on widening outputs
    (test '(0 . pos-inf) (gc-alpha gc '(all-ge . 0)))
    (test '(neg-inf . 5) (gc-alpha gc '(all-le . 5)))
    (test '(neg-inf . pos-inf) (gc-alpha gc 'all-int))))

(test-group "interval-galois-connection — gc-sound? over the full lattice"
  ;; Includes the one-sided intervals widening produces — the reductive law
  ;; alpha(gamma(a)) <= a now holds for (0 . pos-inf), not just bounded ones.
  (test #t (gc-sound? (interval-galois-connection)
             '((0 1 2) (-3 -1 2) () (5))                       ; finite int sets
             (list '(0 . 3) '(-2 . 2) 'interval-bot
                   (cons 0 'pos-inf) (cons 'neg-inf 0)
                   (cons 'neg-inf 'pos-inf)))))                ; + unbounded

(test-group "interval/sign concrete orders are valid partial orders"
  ;; Locks the partial-order axioms (reflexivity, antisymmetry, transitivity)
  ;; on sentinel-bearing samples — gc-sound? alone does not exercise these.
  (test #t (validate-partial-order
             (gc-concrete-po (interval-galois-connection))
             (list '() '(0 1) '(all-ge . 0) '(all-le . 0) 'all-int)))
  (test #t (validate-partial-order
             (gc-concrete-po (sign-galois-connection))
             (list '() '(1) '(-1) 'all-pos 'all-neg 'all-int))))

;;; --- Pre-built sign connection: P(Z) <-> sign --------------------------

(test-group "sign-galois-connection — alpha/gamma"
  (let ((gc (sign-galois-connection)))
    (test 'pos (gc-alpha gc '(1 2 3)))
    (test 'neg (gc-alpha gc '(-1 -2)))
    (test 'flat-top (gc-alpha gc '(-1 1)))
    (test 'flat-bottom (gc-alpha gc '()))
    (test '(0) (gc-gamma gc 'zero))
    (test 'all-pos (gc-gamma gc 'pos))))

(test-group "sign-galois-connection — gc-sound?"
  (test #t (gc-sound? (sign-galois-connection)
             '((1 2) (-1 -3) (0) (-1 0 1) ())      ; finite int sets
             '(neg zero pos flat-bottom flat-top)))) ; all five signs

(test-end)
(test-exit)
