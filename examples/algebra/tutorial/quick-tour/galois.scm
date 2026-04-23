;; quick-tour: (wile algebra galois)
;;
;; A Galois connection is a pair (alpha, gamma) of monotone maps between
;; a concrete partial order and an abstract lattice. alpha abstracts,
;; gamma concretizes. It is the categorical backbone of abstract
;; interpretation -- you pick it up when you want to systematically
;; trade precision for decidability.

(import (scheme base)
        (wile algebra galois) (wile algebra order) (wile algebra lattice))
(include "../lib/check.scm")

;; -- Sign abstraction: int -> {neg, zero, pos} ---------------------

(define int-po    (make-partial-order <=))
(define sign-lat  (flat-lattice '(neg zero pos) eq?))

(define sign-gc
  (make-galois-connection
    (lambda (n) (cond ((< n 0) 'neg) ((= n 0) 'zero) (else 'pos)))
    (lambda (s) (case s ((neg) -1) ((zero) 0) ((pos) 1) (else 0)))
    int-po
    sign-lat))

(check-true (galois-connection? sign-gc)              "sign-gc is a Galois connection")

;; -- Abstraction (alpha) and concretization (gamma) ---------------

(check= (gc-alpha sign-gc  42)   'pos       "alpha(42) = pos")
(check= (gc-alpha sign-gc  -3)   'neg       "alpha(-3) = neg")
(check= (gc-alpha sign-gc   0)   'zero      "alpha(0) = zero")

(check= (gc-gamma sign-gc 'pos)   1          "gamma(pos) = 1 (representative)")
(check= (gc-gamma sign-gc 'neg)  -1          "gamma(neg) = -1")

;; -- Soundness check -----------------------------------------------
;;
;; Extensive  : c <= gamma(alpha(c))
;; Reductive  : alpha(gamma(a)) <= a (in the abstract lattice)
;;
;; The simple representative-returning gamma above is only sound when
;; concrete samples are at or below the gamma-chosen representatives.
;; Since gamma(pos) = 1 and gamma(neg) = -1, extensivity c <= gamma(alpha(c))
;; requires concrete samples in {-1, 0, 1}. A fully sound sign GC
;; for all of Z needs a richer gamma (e.g., returning an interval or
;; set), which is a natural follow-up exercise.

(check= (gc-sound? sign-gc '(-1 0 1) '(neg zero pos))  #t
        "sign-gc satisfies Galois laws on {-1, 0, 1}")

(display "galois tour complete") (newline)
