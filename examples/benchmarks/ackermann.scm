;;; ackermann.scm - Ackermann function benchmark
;;;
;;; The Ackermann function is a classic test of deep recursion.
;;; It grows extremely fast and stresses stack management.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/ackermann.scm

(define (ackermann m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (else (ackermann (- m 1)
                         (ackermann m (- n 1))))))

(define (run-benchmark iterations m n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (ackermann m n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: ackermann(") (display m) (display ", ")
      (display n) (display ")\n")
      (display "Result: ") (display (ackermann m n)) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(ackermann 3 4)

;; Benchmark
;; Note: ack(3,n) time grows ~8x per increment of n.
;; ack(3,7) takes ~3s; ack(3,9) takes ~330s per call.
(display "=== Ackermann Function Benchmark ===\n\n")
(run-benchmark 5 3 7)
