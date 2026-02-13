;;; sieve.scm - Sieve of Eratosthenes benchmark
;;;
;;; Classic list-processing benchmark. Finds prime numbers using
;;; the sieve algorithm with functional list operations.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/sieve.scm

;; filter is not a primitive in Wile, so we define it
(define (filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst))
         (cons (car lst) (filter pred (cdr lst))))
        (else (filter pred (cdr lst)))))

(define (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ m 1) n))))

(define (sieve l)
  (if (null? l)
      '()
      (cons (car l)
            (sieve (filter (lambda (x)
                             (not (= (modulo x (car l)) 0)))
                           (cdr l))))))

(define (primes n)
  (sieve (interval-list 2 n)))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (primes n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second))))
           (result (primes n)))
      (display "Benchmark: primes(") (display n) (display ")\n")
      (display "Found ") (display (length result)) (display " primes\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(primes 100)

;; Benchmark
(display "=== Sieve of Eratosthenes Benchmark ===\n\n")
(run-benchmark 10 1000)
