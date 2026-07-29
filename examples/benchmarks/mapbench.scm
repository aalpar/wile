;;; mapbench.scm - map through an opaque callback
;;;
;;; Measures the real bootstrap_procedures.scm definition of `map', not the
;;; compiler's inlined rewrite of it. `f' is a lambda PARAMETER of `mapper', so
;;; it carries no CaptureSafe+Stable stamp and tryInlineHOFCall deoptimizes to
;;; the Scheme-level definition. This is the arm where a source-level shape
;;; change to `map' is observable; a benchmark that calls `map' with a literal
;;; lambda measures the inline template instead and moves for other reasons.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/mapbench.scm

(define (mapper f lst)
  (map f lst))

(define (build n)
  (let loop ((i 0) (acc '()))
    (if (= i n)
        acc
        (loop (+ i 1) (cons i acc)))))

(define data (build 1000))

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (mapper (lambda (x) (+ x 1)) data)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: map through an opaque callback\n")
      (display "List length: ") (display (length data)) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(mapper (lambda (x) x) data)

;; Benchmark
(display "=== map (non-inlined) Benchmark ===\n\n")
(run-benchmark 2000)
