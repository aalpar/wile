;;; lazy-streams.scm - Infinite sequences with delay/force
;;;
;;; Demonstrates: delay, force, delay-force, syntax-rules macros,
;;;               memoization, infinite data structures, compositionality
;;;
;;; Usage: ./dist/scheme --file examples/data-structures/lazy-streams.scm

;; -----------------------------------------------------------------------
;; Stream primitives
;;
;; A stream is either '() (empty) or (value . promise) where promise
;; evaluates to the next stream element when forced.
;;
;; stream-cons MUST be a macro — the tail must not be evaluated eagerly.
;; -----------------------------------------------------------------------

(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons head tail)
     (cons head (delay tail)))))

(define stream-null '())
(define (stream-null? s) (null? s))
(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

;; -----------------------------------------------------------------------
;; Stream operations
;; -----------------------------------------------------------------------

(define (stream-take n s)
  (if (or (zero? n) (stream-null? s))
      '()
      (cons (stream-car s)
            (stream-take (- n 1) (stream-cdr s)))))

(define (stream-drop n s)
  (if (or (zero? n) (stream-null? s))
      s
      (stream-drop (- n 1) (stream-cdr s))))

(define (stream-ref s n)
  (if (zero? n)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map f s)
  (if (stream-null? s)
      stream-null
      (stream-cons (f (stream-car s))
                   (stream-map f (stream-cdr s)))))

(define (stream-filter pred s)
  (if (stream-null? s)
      stream-null
      (let loop ((s s))
        (cond
          ((stream-null? s) stream-null)
          ((pred (stream-car s))
           (stream-cons (stream-car s)
                        (stream-filter pred (stream-cdr s))))
          (else (loop (stream-cdr s)))))))

(define (stream-zip-with f s1 s2)
  (if (or (stream-null? s1) (stream-null? s2))
      stream-null
      (stream-cons (f (stream-car s1) (stream-car s2))
                   (stream-zip-with f (stream-cdr s1) (stream-cdr s2)))))

(define (stream-iterate f x)
  (stream-cons x (stream-iterate f (f x))))

(define (stream-fold f init s n)
  (if (zero? n) init
      (stream-fold f (f init (stream-car s)) (stream-cdr s) (- n 1))))

(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (stream-interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (stream-interleave s2 (stream-cdr s1)))))

;; -----------------------------------------------------------------------
;; Classic streams
;; -----------------------------------------------------------------------

;; Natural numbers: 0, 1, 2, 3, ...
(define naturals (stream-iterate (lambda (n) (+ n 1)) 0))

;; Fibonacci: 0, 1, 1, 2, 3, 5, 8, ...
(define fibs
  (stream-cons 0
    (stream-cons 1
      (stream-zip-with + fibs (stream-cdr fibs)))))

;; Sieve of Eratosthenes
(define (integers-from n)
  (stream-cons n (integers-from (+ n 1))))

(define (sieve s)
  (let ((p (stream-car s)))
    (stream-cons p
      (sieve (stream-filter
              (lambda (n) (not (zero? (remainder n p))))
              (stream-cdr s))))))

(define primes (sieve (integers-from 2)))

;; Powers of 2: 1, 2, 4, 8, 16, ...
(define powers-of-2 (stream-iterate (lambda (n) (* n 2)) 1))

;; Factorials as a stream
(define factorials
  (let helper ((n 1) (fact 1))
    (stream-cons fact (helper (+ n 1) (* fact (+ n 1))))))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(display "=== Lazy Streams ===\n\n")

(display "--- Natural numbers (first 15) ---\n  ")
(display (stream-take 15 naturals))
(newline)

(display "\n--- Fibonacci (first 15) ---\n  ")
(display (stream-take 15 fibs))
(newline)

(display "\n--- Primes (first 25) ---\n  ")
(display (stream-take 25 primes))
(newline)

(display "\n--- Powers of 2 (first 10) ---\n  ")
(display (stream-take 10 powers-of-2))
(newline)

(display "\n--- Factorials (first 8) ---\n  ")
(display (stream-take 8 factorials))
(newline)

(display "\n--- Stream pipeline: primes > 100 (first 10) ---\n  ")
(display (stream-take 10 (stream-filter (lambda (p) (> p 100)) primes)))
(newline)

(display "\n--- Composed: squares of odd naturals (first 10) ---\n  ")
(display (stream-take 10
  (stream-map (lambda (x) (* x x))
    (stream-filter odd? naturals))))
(newline)

(display "\n--- Sum of first 1000 naturals ---\n  ")
(display (stream-fold + 0 naturals 1000))
(newline)

(display "\n--- 100th prime ---\n  ")
(display (stream-ref primes 99))
(newline)

(display "\nInfinite sequences — zero allocation until consumed.\n")
