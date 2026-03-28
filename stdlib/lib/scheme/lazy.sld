(define-library (scheme lazy)
  (description "Lazy evaluation: delay, force, delay-force, make-promise, promise?.")
  (export
    delay
    delay-force
    force
    make-promise
    promise?))
