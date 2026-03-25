;;; algebra-galois-test.scm — Galois connection tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
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

(test-end)
(test-exit)
