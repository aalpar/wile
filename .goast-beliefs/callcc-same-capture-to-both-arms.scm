(import (wile goast belief))

;; Belief: every continuation-capturing primitive that applies a callback must
;; hand the SAME captured continuation value to that callback on every path —
;; the captured value must FLOW to every application site, in every mode arm.
;;
;; This is the data-flow companion to callcc-capture-dominates-modes (B1) for
;; staff-sweep #9 (PrimCallCC dual-mode). B1 asserts the capture DOMINATES every
;; ApplyCallable call (control: capture happens before every arm's application).
;; B2 asserts the same captured VALUE reaches every ApplyCallable call (data: no
;; arm re-captures or substitutes a different continuation for its callback).
;; Same anchor, two axes: SliceContinuationAt is the shared capture primitive;
;; B1 checks it dominates ApplyCallable, B2 checks its value flows to
;; ApplyCallable.
;;
;; The hazard B2 adds over B1: an edit that leaves the capture dominating both
;; arms (B1 still green) but rebuilds a DIFFERENT continuation in one arm before
;; applying the callback — the two arms would then hand the user callback
;; different continuations, a silent divergence B1's dominance check cannot see.
;;
;; Site set (same natural consensus group as B1, verified 2026-07-03): the
;; primitives that call BOTH SliceContinuationAt and ApplyCallable — currently
;; PrimCallCC and PrimCallWithComposableContinuation. Both are 'flows-all today.
;;
;; Assertion mechanism (why this fails on regression): flows-to-all returns
;; 'flows-all only when a SINGLE SliceContinuationAt value reaches EVERY
;; ApplyCallable site. This needs the aggregate-alias edge in wile-goast's
;; value-flow-reached: the captured continuation is handed to the callback
;; variadically (mc.ApplyCallable(mcls, capt)), so in SSA it is packed into a
;; backing array and passed as a slice — a plain def-use walk returns a false
;; negative there. With min-adherence 1.0 and >= 2 sites, both at 'flows-all
;; gives ratio 1.0 -> strong. If an arm re-captures (two separate captures, each
;; reaching only its own arm), that member drops to 'partial, the majority
;; splits, and ratio falls to 0.5 < 1.0 -> weak.
;;
;; NOTE: this uses flows-to-all, added to wile-goast for this belief (the
;; value-flow analog of dominates-call, with an aggregate-alias edge for
;; value-through-variadic-slice flow). See wile-goast belief-checkers.scm.
(define-belief "callcc-same-capture-to-both-arms"
  (sites (functions-matching
    (all-of (contains-call "SliceContinuationAt")
            (contains-call "ApplyCallable"))))
  (expect (flows-to-all "SliceContinuationAt" "ApplyCallable"))
  (threshold 1.0 2))
