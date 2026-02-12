;;; sumloop.scm - Iterative summation benchmark
;;;
;;; Simple iterative summation from 1 to n using named let.
;;; Tests tail recursion optimization and integer arithmetic.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/sumloop.scm

(define (sumloop n)
  (let loop ((i 1) (acc 0))
    (if (> i n)
        acc
        (loop (+ i 1) (+ acc i)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (sumloop n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: sumloop(") (display n) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(sumloop 10000)

;; Benchmark
(display "=== Iterative Summation Benchmark ===\n\n")
(run-benchmark 1000 10000)