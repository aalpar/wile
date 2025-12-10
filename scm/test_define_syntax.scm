; Test define-syntax compilation
(define-syntax foo
  (syntax-rules ()
    ((foo x) x)))