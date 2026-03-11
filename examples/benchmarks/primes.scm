;;; primes.scm - Prime number generation benchmark
;;;
;;; Generates prime numbers using trial division.
;;; Tests integer arithmetic and simple algorithms.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/primes.scm

(define (is-prime? n)
  (if (< n 2)
      #f
      (let loop ((i 2))
        (cond ((> (* i i) n) #t)
              ((= (modulo n i) 0) #f)
              (else (loop (+ i 1)))))))

(define (primes-upto n)
  (let loop ((i 2) (result '()))
    (if (> i n)
        (reverse result)
        (loop (+ i 1)
              (if (is-prime? i)
                  (cons i result)
                  result)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (primes-upto n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: primes up to ") (display n) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(primes-upto 1000)

;; Benchmark
(display "=== Prime Number Generation Benchmark ===\n\n")
(run-benchmark 100 1000)