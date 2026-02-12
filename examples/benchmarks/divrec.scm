;;; divrec.scm - Recursive division benchmark
;;;
;;; Tests recursive function calls with division operations.
;;; Benchmarks tail recursion optimization and arithmetic.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/divrec.scm

(define (divrec n)
  (if (<= n 0)
      1
      (/ (divrec (- n 1)) 2)))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (divrec n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: divrec(") (display n) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(divrec 1000)

;; Benchmark
(display "=== Recursive Division Benchmark ===\n\n")
(run-benchmark 10000 1000)