;;; nqueens.scm - N-Queens puzzle benchmark
;;;
;;; Classic constraint satisfaction problem. Places N queens on an NxN
;;; chessboard such that no two queens attack each other.
;;; Tests backtracking and list manipulation.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/nqueens.scm

(define (safe? col positions)
  (let loop ((pos-list positions)
             (row 1))
    (cond ((null? pos-list) #t)
          ((let ((queen-col (car pos-list)))
             (or (= queen-col col)
                 (= queen-col (+ col row))
                 (= queen-col (- col row))))
           #f)
          (else (loop (cdr pos-list) (+ row 1))))))

(define (nqueens n)
  (let loop ((row n)
             (positions '())
             (solutions 0))
    (if (= row 0)
        (+ solutions 1)
        (let col-loop ((col 1)
                       (sols solutions))
          (if (> col n)
              sols
              (col-loop (+ col 1)
                        (if (safe? col positions)
                            (loop (- row 1)
                                  (cons col positions)
                                  sols)
                            sols)))))))

(define (run-benchmark iterations n)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (nqueens n)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second))))
           (result (nqueens n)))
      (display "Benchmark: nqueens(") (display n) (display ")\n")
      (display "Solutions: ") (display result) (newline)
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(nqueens 8)

;; Benchmark
(display "=== N-Queens Puzzle Benchmark ===\n\n")
(run-benchmark 2 10)
