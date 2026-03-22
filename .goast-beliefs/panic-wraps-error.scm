(import (wile goast belief))

;; Belief: every function that calls panic() should also call
;; werr.WrapForeignErrorf to wrap the error with context.
;; Project invariant: "NEVER panic with raw errors"
(define-belief "panic-wraps-error"
  (sites (functions-matching (contains-call "panic")))
  (expect (contains-call "WrapForeignErrorf"))
  (threshold 0.90 3))
