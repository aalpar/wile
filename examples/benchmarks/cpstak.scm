;;; cpstak.scm - Continuation-passing style Takeuchi benchmark
;;;
;;; CPS version of the Takeuchi function. Tests closure creation,
;;; higher-order function calls, and tail-call optimization.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/cpstak.scm

(define (cpstak x y z k)
  (if (not (< y x))
      (k z)
      (cpstak (- x 1) y z
              (lambda (v1)
                (cpstak (- y 1) z x
                        (lambda (v2)
                          (cpstak (- z 1) x y
                                  (lambda (v3)
                                    (cpstak v1 v2 v3 k)))))))))

(define (run-benchmark iterations x y z)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (cpstak x y z (lambda (result) result))
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: cpstak(") (display x) (display ", ")
      (display y) (display ", ") (display z) (display ")\n")
      (display "Result: ")
      (display (cpstak x y z (lambda (result) result)))
      (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(cpstak 18 12 6 (lambda (result) result))

;; Benchmark
(display "=== CPS Takeuchi Benchmark ===\n\n")
(run-benchmark 10 18 12 6)
