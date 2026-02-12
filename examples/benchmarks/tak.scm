;;; tak.scm - Takeuchi function benchmark
;;;
;;; The Takeuchi function is a classic Lisp benchmark from the Gabriel suite.
;;; It stresses recursion and function call overhead.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/tak.scm

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

(define (run-benchmark iterations x y z)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (tak x y z)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: tak(") (display x) (display ", ")
      (display y) (display ", ") (display z) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(tak 18 12 6)

;; Benchmark
(display "=== Takeuchi Function Benchmark ===\n\n")
(run-benchmark 10 18 12 6)
