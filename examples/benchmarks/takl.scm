;;; takl.scm - Takeuchi function with lists
;;;
;;; A variant of the Takeuchi function that uses lists instead of integers.
;;; Stresses list allocation, GC, and function call overhead.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/takl.scm

(define (listn n)
  (if (= n 0)
      '()
      (cons n (listn (- n 1)))))

(define (shorterp x y)
  (and (pair? y)
       (or (null? x)
           (shorterp (cdr x) (cdr y)))))

(define (takl x y z)
  (if (not (shorterp y x))
      z
      (takl (takl (cdr x) y z)
            (takl (cdr y) z x)
            (takl (cdr z) x y))))

(define (run-benchmark iterations)
  (let ((l18 (listn 18))
        (l12 (listn 12))
        (l6 (listn 6)))
    (let ((start (current-jiffy)))
      (let loop ((i 0))
        (when (< i iterations)
          (takl l18 l12 l6)
          (loop (+ i 1))))
      (let* ((end (current-jiffy))
             (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
        (display "Benchmark: takl(18, 12, 6)\n")
        (display "Iterations: ") (display iterations) (newline)
        (display "Total time: ") (display elapsed) (display "s\n")
        (display "Per iteration: ")
        (display (exact->inexact (/ elapsed iterations)))
        (display "s\n")
        elapsed))))

;; Warmup
(takl (listn 18) (listn 12) (listn 6))

;; Benchmark
(display "=== Takeuchi with Lists Benchmark ===\n\n")
(run-benchmark 10)