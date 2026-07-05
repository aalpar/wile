(import (wile goast belief))

;; Belief: every continuation-capturing primitive that applies a callback must
;; apply it through a SINGLE seam — one callback-application site, with mode
;; expressed as a target selection (ambient context vs a fresh sub-context), NOT
;; two hand-written apply arms.
;;
;; This is the Tier-2 ACCEPTANCE belief for staff-sweep #9 (PrimCallCC dual-mode)
;; and its ongoing drift guard. Before the restructure, PrimCallCC and its
;; sibling PrimCallWithComposableContinuation each ran the user callback through
;; TWO hand-written ApplyCallable arms branched on mc.Parent() — the inline arm
;; (mc.ApplyCallable) and the sub-context arm (sub.ApplyCallable). Correctness
;; that the two arms stayed observationally equivalent lived only in prose; a
;; future edit to one arm could diverge from the other and still pass ordinary
;; tests (the documented green-but-wrong failure mode in the most-reverted
;; neighborhood in the tree). The restructure collapsed each to ONE apply site:
;; the capture is shared, and the only per-mode difference (driver provenance) is
;; a target selection before a single ApplyCallable. This belief guards that: a
;; re-split of ONE member into two arms makes the family diverge (weak). See the
;; Assertion mechanism note below for the limit of consensus scoring against a
;; CORRELATED re-split of both members.
;;
;; Companion to callcc-capture-dominates-modes (B1, control: capture dominates
;; the apply) and callcc-same-capture-to-both-arms (B2, data: the captured value
;; flows to the apply). B1/B2 assert properties OF the apply; B3 asserts there is
;; exactly ONE apply. Same 2-member family, same anchor (ApplyCallable).
;;
;; Site set (same natural consensus group as B1/B2, verified 2026-07-03): the
;; primitives that call BOTH SliceContinuationAt and ApplyCallable — currently
;; PrimCallCC and PrimCallWithComposableContinuation. Both are 'single today.
;;
;; Assertion mechanism (what this catches, and its limit): single-call-site
;; returns 'single only when EXACTLY ONE SSA call to ApplyCallable exists (a
;; re-split into two arms is two source calls -> two SSA calls -> 'multiple).
;; With min-adherence 1.0 and >= 2 sites, both at 'single gives ratio 1.0 ->
;; strong. If ONE member re-splits back into two arms, that member drops to
;; 'multiple, so no category covers both sites, ratio falls to 0.5 < 1.0 ->
;; weak. This is a MUTUAL-DIVERGENCE guard: the belief-framework scores by
;; majority consensus (belief.scm evaluate-belief: ratio = maj-count/total), so
;; a CORRELATED re-split of BOTH members (both -> 'multiple) is again a unanimous
;; majority, ratio 1.0 -> strong. Consensus cannot distinguish "both correct"
;; from "both regressed"; what it surfaces on a correlated regression is the
;; reported PATTERN flipping 'single -> 'multiple (read the pattern, not only the
;; status). This limit is shared by all consensus beliefs, including the sibling
;; B1/B2 above. Closing it fully would need a pinned-expect mechanism the belief
;; DSL does not have today (tracked as a wile-goast follow-up).
;;
;; NOTE: this uses single-call-site, added to wile-goast for this belief (the
;; call-site-cardinality analog of dominates-call/flows-to-all; counts SSA calls
;; to the op). See wile-goast belief-checkers.scm.
(define-belief "callcc-mode-selection-single-seam"
  (sites (functions-matching
    (all-of (contains-call "SliceContinuationAt")
            (contains-call "ApplyCallable"))))
  (expect (single-call-site "ApplyCallable"))
  (threshold 1.0 2))
