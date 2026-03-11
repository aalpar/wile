;;; peval.scm - Partial evaluation benchmark
;;;
;;; Tests higher-order functions and partial evaluation patterns.
;;; Demonstrates function composition and currying.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/peval.scm

(define (peval-compose f g)
  (lambda (x) (f (g x))))

(define (peval-curry f)
  (lambda (x)
    (lambda (y) (f x y))))

(define (peval-add x y) (+ x y))
(define (peval-mul x y) (* x y))
(define (peval-square x) (* x x))

;; Build a complex composed function
(define peval-test-fn
  (peval-compose
   (peval-compose peval-square ((peval-curry peval-add) 3))
   ((peval-curry peval-mul) 2)))

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    (let loop ((i 0) (acc 0))
      (if (< i iterations)
          (loop (+ i 1) (+ acc (peval-test-fn i)))
          (let* ((end (current-jiffy))
                 (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
            (display "Benchmark: partial evaluation on composed function\n")
            (display "Iterations: ") (display iterations) (newline)
            (display "Total time: ") (display elapsed) (display "s\n")
            (display "Per iteration: ")
            (display (exact->inexact (/ elapsed iterations)))
            (display "s\n")
            elapsed)))))

;; Warmup
(peval-test-fn 42)

;; Benchmark
(display "=== Partial Evaluation Benchmark ===\n\n")
(run-benchmark 100000)