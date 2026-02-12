;;; diviter.scm - Iterative division benchmark
;;;
;;; Tests iterative loops with division operations.
;;; Benchmarks basic arithmetic in a tight loop.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/diviter.scm

(define (diviter n)
  (let loop ((i n) (result 1))
    (if (<= i 0)
        result
        (loop (- i 1) (/ result 2)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (diviter n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: diviter(") (display n) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(diviter 1000)

;; Benchmark
(display "=== Iterative Division Benchmark ===\n\n")
(run-benchmark 10000 1000)