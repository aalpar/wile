(define-library (test simple)
  (export make-adder
          double
          secret-value)
  (begin
    (define secret-value 42)
    (define (make-adder n)
      (lambda (x) (+ x n)))
    (define (double x)
      (* x 2))))
