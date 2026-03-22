(import (wile goast belief))

;; Belief: every function that calls Lock() should also call Unlock().
;; Standard Go mutex discipline.
(define-belief "lock-unlock-paired"
  (sites (functions-matching (contains-call "Lock")))
  (expect (paired-with "Lock" "Unlock"))
  (threshold 0.90 3))
