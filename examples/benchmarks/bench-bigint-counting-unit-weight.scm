;;; bench-bigint-counting-unit-weight.scm
;;;
;;; Acceptance benchmark for sub-path 4A of the bignum-allocation-reduction
;;; plan (plans/2026-05-24-bignum-allocation-reduction.md). Compares
;;; `counting-semiring' (existing slow path, fixnum-with-overflow-promotion)
;;; against `bigint-counting-semiring' (new fast path via
;;; `count-paths-in-dag') on the same DAG.
;;;
;;; The DAG is a chain of N diamonds:
;;;
;;;   s_0 → {a_0, b_0} → s_1 → {a_1, b_1} → s_2 → ... → s_N
;;;
;;; Each diamond contributes a factor of 2 to the path count, so the count
;;; from s_0 to s_N is 2^N. With N = 100, the last ~36 diamonds force
;;; bignum arithmetic on both paths — the fixnum fast-path-with-promotion
;;; in `counting-semiring' shifts to BigInteger arithmetic around diamond
;;; 63, after which every relaxation allocates 3 heap objects per op via
;;; (*BigInteger).Add. The bigint fast path uses in-place
;;; (*big.Int).Add against pre-allocated slots.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/bench-bigint-counting-unit-weight.scm

(import (scheme base)
        (scheme write)
        (wile algebra semiring)
        (wile algebra graph))

(define DIAMONDS 100)

;; Construct the adjacency. Vertices are "s0", "a0", "b0", "s1", ...
(define (vname tag i)
  (string-append tag (number->string i)))

(define (build-adjacency n)
  ;; Walk forwards and emit each node's out-edges. The terminal s_n has
  ;; no out-edges. Order: s_0, a_0, b_0, s_1, a_1, b_1, ..., s_n.
  (let loop ((i 0) (acc '()))
    (cond
      ((>= i n)
       (reverse (cons (cons (vname "s" n) '()) acc)))
      (else
       (let* ((si  (vname "s" i))
              (ai  (vname "a" i))
              (bi  (vname "b" i))
              (s+1 (vname "s" (+ i 1)))
              (entries (list
                        (cons si  (list (cons ai 1) (cons bi 1)))
                        (cons ai  (list (cons s+1 1)))
                        (cons bi  (list (cons s+1 1))))))
         (loop (+ i 1) (append (reverse entries) acc)))))))

(define adj (build-adjacency DIAMONDS))
(define source (vname "s" 0))
(define sink   (vname "s" DIAMONDS))
(define expected-count (expt 2 DIAMONDS))   ; 2^N paths

;; --- Timing helpers ---

(define (timed thunk)
  (let* ((start  (current-jiffy))
         (result (thunk))
         (end    (current-jiffy)))
    (values result
            (exact->inexact (/ (- end start) (jiffies-per-second))))))

(define (run-once label semiring)
  ;; Fresh analysis each run so caching doesn't hide cost on repeats.
  (call-with-values
    (lambda ()
      (timed (lambda ()
               (let ((ga (make-graph-analysis semiring adj #f)))
                 (graph-query ga source sink)))))
    (lambda (result elapsed-s)
      (display label) (display ": ")
      (display elapsed-s) (display "s, count = ") (display result)
      (when (not (equal? result expected-count))
        (display "  *** WRONG, expected ") (display expected-count) (display " ***"))
      (newline)
      elapsed-s)))

;; --- Run ---

(display "=== Bigint-counting unit-weight benchmark ===") (newline)
(display "Diamonds: ") (display DIAMONDS) (newline)
(display "Vertices: ") (display (length adj)) (newline)
(display "Expected count s0 → s") (display DIAMONDS)
(display " = 2^") (display DIAMONDS) (display " = ") (display expected-count) (newline)
(newline)

;; Warmup — exercise the compiler/expander on both code paths.
(make-graph-analysis (counting-semiring) adj #f)
(make-graph-analysis (bigint-counting-semiring) adj #f)

(define slow-t (run-once "  slow (counting-semiring)        " (counting-semiring)))
(define fast-t (run-once "  fast (bigint-counting-semiring) " (bigint-counting-semiring)))

(newline)
(display "Speedup (slow / fast): ")
(display (exact->inexact (/ slow-t fast-t)))
(display "x") (newline)
