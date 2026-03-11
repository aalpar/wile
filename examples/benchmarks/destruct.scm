;;; destruct.scm - Destructuring benchmark
;;;
;;; Tests list destructuring and reconstruction patterns.
;;; Stresses cons cell allocation and list operations.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/destruct.scm

(define (destruct x)
  (cond ((null? x) '())
        ((null? (cdr x)) x)
        (else
         (cons (append (car x) (cadr x))
               (destruct (cddr x))))))

(define (make-test-list n)
  (if (<= n 0)
      '()
      (cons (list n (+ n 1) (+ n 2))
            (make-test-list (- n 1)))))

(define test-list (make-test-list 200))

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (destruct test-list)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: destruct on 200-element list\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(destruct test-list)

;; Benchmark
(display "=== List Destructuring Benchmark ===\n\n")
(run-benchmark 1000)