(import (wile goast belief))

;; Belief: functions that create/wrap errors should use WrapForeignErrorf,
;; not fmt.Errorf or errors.New.
;; Project invariant: sentinel + wrap pattern, never bare fmt.Errorf
;; Sites: any function that calls WrapForeignErrorf, fmt.Errorf, or errors.New
;; Expect: should be using WrapForeignErrorf (not the others)
(define-belief "error-wrap-not-fmt"
  (sites (functions-matching
    (any-of
      (contains-call "WrapForeignErrorf")
      (contains-call "fmt.Errorf")
      (contains-call "errors.New"))))
  (expect (contains-call "WrapForeignErrorf"))
  (threshold 0.90 3))
