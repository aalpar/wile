;;; vmapbench.scm - vector-map through an opaque callback
;;;
;;; The vector twin of mapbench.scm. `f' is a lambda PARAMETER of `mapper', so
;;; tryInlineHOFCall deoptimizes to the Scheme-level `vector-map', whose loop is
;;; the (vector-set! r i (f (vector-ref v i))) shape. This is the arm that moves
;;; when vector-set!/vector-ref dispatch changes -- the canonical Gabriel suite
;;; contains no vector operations at all.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/vmapbench.scm

(define (mapper f v)
  (vector-map f v))

(define src (make-vector 1000 3))

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (mapper (lambda (x) (+ x 1)) src)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: vector-map through an opaque callback\n")
      (display "Vector length: ") (display (vector-length src)) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(mapper (lambda (x) x) src)

;; Benchmark
(display "=== vector-map (non-inlined) Benchmark ===\n\n")
(run-benchmark 2000)
