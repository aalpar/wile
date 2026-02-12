;;; ctak.scm - Continuation-based Takeuchi function
;;;
;;; A variant of tak that uses call/cc (call-with-current-continuation).
;;; Tests continuation capture and invocation performance.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/ctak.scm

(define (ctak x y z)
  (call-with-current-continuation
   (lambda (k)
     (ctak-aux k x y z))))

(define (ctak-aux k x y z)
  (if (not (< y x))
      (k z)
      (call-with-current-continuation
       (lambda (k)
         (ctak-aux k
                   (call-with-current-continuation
                    (lambda (k) (ctak-aux k (- x 1) y z)))
                   (call-with-current-continuation
                    (lambda (k) (ctak-aux k (- y 1) z x)))
                   (call-with-current-continuation
                    (lambda (k) (ctak-aux k (- z 1) x y))))))))

(define (run-benchmark iterations x y z)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (ctak x y z)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: ctak(") (display x) (display ", ")
      (display y) (display ", ") (display z) (display ")\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(ctak 18 12 6)

;; Benchmark
(display "=== Continuation-based Takeuchi Benchmark ===\n\n")
(run-benchmark 10 18 12 6)