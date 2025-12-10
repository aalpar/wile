(define-library (test importer)
  (export quadruple
          get-secret)
  (import (test simple))
  (begin
    (define (quadruple x)
      (double (double x)))
    (define (get-secret)
      secret-value)))
