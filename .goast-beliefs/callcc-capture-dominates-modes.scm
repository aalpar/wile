(import (wile goast belief))

;; Belief: every continuation-capturing primitive that applies a callback must
;; capture the continuation BEFORE it applies — the capture must dominate the
;; application on every control path.
;;
;; This is the structural guard for staff-sweep #9 (PrimCallCC dual-mode). The
;; hazard: PrimCallCC runs the user callback through one of two hand-written arms
;; (inline: mc.ApplyCallable when a live continuation chain exists; sub-context:
;; sub.ApplyCallable under a fresh RunWithEscapeHandling when rootless). Both arms
;; call ApplyCallable, and the continuation capture (SliceContinuationAt +
;; SnapshotReachableMarksInto + NewCapturedContinuation) is shared code that runs
;; BEFORE the mode branch. If a future edit were to move any capture step into one
;; arm, the callback in the OTHER arm would be applied without the capture — a
;; silent, green-but-wrong divergence in the most-reverted neighborhood in the
;; tree. This belief asserts the capture dominates every ApplyCallable call site
;; on every path, in every arm.
;;
;; Site set (natural consensus group, verified 2026-07-03): the primitives that
;; call BOTH SliceContinuationAt and ApplyCallable — currently PrimCallCC and
;; PrimCallWithComposableContinuation. Both are 'dominates-all today. The
;; invariant is the shared law of the continuation-capture primitive FAMILY, not
;; a one-off assertion about PrimCallCC.
;;
;; Assertion mechanism (why this fails on regression): dominates-call returns
;; 'dominates-all only when EVERY ApplyCallable block is dominated by a capture
;; block (unlike `ordered`, which checks only the first). With min-adherence 1.0
;; and >= 2 sites, both at 'dominates-all gives ratio 1.0 -> strong. If either
;; regresses to 'partial (capture moved into one arm), the majority splits and
;; ratio drops to 0.5 < 1.0 -> weak. A shrink of the family below two sites also
;; drops below min-sites, forcing a re-review of the family shape.
;;
;; Consensus limit (shared by all consensus beliefs — see B3
;; callcc-mode-selection-single-seam): a CORRELATED regression of BOTH members
;; (both -> 'partial) is again a unanimous majority, ratio 1.0 -> strong; what
;; surfaces then is the reported pattern flipping, not the status. Read the
;; pattern, not only strong/weak.
;;
;; NOTE: this uses dominates-call, added to wile-goast for this belief (SSA idom;
;; the multi-site generalization of `ordered`). See wile-goast belief-checkers.scm.
(define-belief "callcc-capture-dominates-modes"
  (sites (functions-matching
    (all-of (contains-call "SliceContinuationAt")
            (contains-call "ApplyCallable"))))
  (expect (dominates-call "SliceContinuationAt" "ApplyCallable"))
  (threshold 1.0 2))
