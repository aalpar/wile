;;; sumfp.scm - Floating-point summation benchmark
;;;
;;; Tests floating-point arithmetic performance with a simple
;;; iterative loop. Exercises numeric tower and tail recursion.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/sumfp.scm

(define (sumfp n)
  (let loop ((i 0.0)
             (sum 0.0))
    (if (>= i n)
        sum
        (loop (+ i 1.0)
              (+ sum i)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (sumfp n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: sumfp(") (display n) (display ")\n")
      (display "Result: ") (display (sumfp n)) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(sumfp 100000.0)

;; Benchmark
(display "=== Floating-Point Summation Benchmark ===\n\n")
(run-benchmark 10 1000000.0)
