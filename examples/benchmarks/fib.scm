;;; fib.scm - Fibonacci benchmark (naive recursive)
;;;
;;; Fibonacci is the most well-known recursion benchmark.
;;; This uses the naive doubly-recursive implementation to stress
;;; function calls and stack management.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/fib.scm

(define (fib n)
  (if (<= n 1)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (fib n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: fib(") (display n) (display ")\n")
      (display "Result: ") (display (fib n)) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(fib 20)

;; Benchmark
(display "=== Fibonacci Benchmark (Naive Recursive) ===\n\n")
(run-benchmark 10 25)
