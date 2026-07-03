(import (wile goast belief))

;; Belief: every function that produces a capture continuation segment must mark
;; the live continuation chain shared, so a subsequent NORMAL return through any
;; of those frames takes RestoreAndRelease's shared branch and does NOT pool an
;; env frame the captured continuation still aliases.
;;
;; This is the structural guard for the precondition the vmState save/restore
;; oracle cannot see (staff-sweep #1 Tier-2). The two canonical continuation
;; reverts — tail-frame-recycling-unsound and c1-continuation-not-frame-reclaim —
;; were both a capture site (SliceContinuationAt) failing to MarkChainShared;
;; every save/restore method obeyed its descriptor, so only a capture-site guard
;; like this one catches that class.
;;
;; Sites: machine functions that slice the chain, construct a Composable/
;;   Captured continuation, or mark the chain directly. Marking is reached
;;   transitively for the delegating sites (e.g. resolveTimerInterrupt ->
;;   CaptureInterruptContinuationAt -> SliceContinuationAt -> MarkChainShared),
;;   so the check must follow call-graph edges — reaches-call, not the
;;   direct-only contains-call. Including MarkChainShared callers keeps the
;;   direct chokepoints (SliceContinuationAt, CurrentContinuation,
;;   AcquireSegment) in the site set: if one drops its mark it becomes a
;;   deviation, and the belief clears the min-sites floor.
(define-belief "continuation-capture-marks-shared"
  (sites (functions-matching
    (any-of
      (contains-call "SliceContinuationAt")
      (contains-call "NewComposableContinuation")
      (contains-call "NewCapturedContinuation")
      (contains-call "MarkChainShared"))))
  (expect (reaches-call "MarkChainShared"))
  (threshold 0.90 3))
