;;; deriv.scm - Symbolic differentiation
;;;
;;; Classic Lisp benchmark performing symbolic differentiation.
;;; Tests list manipulation and pattern matching.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/deriv.scm

(define (deriv a)
  (cond ((not (pair? a))
         (if (eq? a 'x) 1 0))
        ((eq? (car a) '+)
         (cons '+ (map deriv (cdr a))))
        ((eq? (car a) '-)
         (cons '- (map deriv (cdr a))))
        ((eq? (car a) '*)
         (list '*
               a
               (cons '+ (map (lambda (a) (list '/ (deriv a) a)) (cdr a)))))
        ((eq? (car a) '/)
         (list '-
               (list '/
                     (deriv (cadr a))
                     (caddr a))
               (list '/
                     (cadr a)
                     (list '*
                           (caddr a)
                           (caddr a)
                           (deriv (caddr a))))))
        (else
         (error "deriv: unknown operator" (car a)))))

;; Test expression: (+ (* 3 x x) (* a x x) (* b x) 5)
(define test-expr '(+ (* 3 x x) (* a x x) (* b x) 5))

(define (run-benchmark iterations)
  (let ((start (current-jiffy)))
    (let loop ((i 0))
      (when (< i iterations)
        (deriv test-expr)
        (loop (+ i 1))))
    (let* ((end (current-jiffy))
           (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
      (display "Benchmark: deriv on polynomial\n")
      (display "Iterations: ") (display iterations) (newline)
      (display "Total time: ") (display elapsed) (display "s\n")
      (display "Per iteration: ")
      (display (exact->inexact (/ elapsed iterations)))
      (display "s\n")
      elapsed)))

;; Warmup
(deriv test-expr)

;; Benchmark
(display "=== Symbolic Differentiation Benchmark ===\n\n")
(run-benchmark 10000)