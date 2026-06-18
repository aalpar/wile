(define-library (test circular-a)
  (export a-value)
  (import (test circular-b))
  (begin
    (define a-value 1)))
