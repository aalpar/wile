;;; sum.scm - Recursive summation benchmark
;;;
;;; Simple recursive summation from 1 to n.
;;; Tests basic recursion and integer arithmetic.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/sum.scm

(define (sum n)
  (if (<= n 0)
      0
      (+ n (sum (- n 1)))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (sum n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: sum(") (display n) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup (must stay within DefaultMaxCallDepth of 10000 frames;
;; sum is not tail-recursive, so (sum n) uses n+1 frames plus
;; the frames from the benchmark harness and file-level begin)
(sum 9990)

;; Benchmark
(display "=== Recursive Summation Benchmark ===\n\n")
(run-benchmark 15 9990)