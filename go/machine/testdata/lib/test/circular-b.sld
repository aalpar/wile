(define-library (test circular-b)
  (export b-value)
  (import (test circular-a))
  (begin
    (define b-value 2)))
