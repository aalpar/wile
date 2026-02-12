;;; triangl.scm - Double-recursive triangle calculation
;;;
;;; A benchmark that creates a doubly-recursive tree structure.
;;; Tests recursion depth and memory allocation patterns.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/triangl.scm

(define (tri n)
  (if (<= n 0)
      0
      (+ n (tri (- n 1)) (tri (- n 1)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (tri n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: tri(") (display n) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(tri 10)

;; Benchmark
(display "=== Triangle Double-Recursion Benchmark ===\n\n")
(run-benchmark 100 10)