;; hashtable-update! — its own bootstrap procedure source, and it is separate for
;; TWO independent reasons, either of which alone would justify the split.
;;
;; 1. It IS a mutation: the body is (hashtable-set! ht key (proc ...)), and there
;;    is no mutation-free variant to write. The NoMutation dialect therefore drops
;;    this source entirely rather than swapping it, which is why it cannot ride in
;;    bootstrap_maps_mutable.scm's slot — that slot is SWAPPED, not dropped.
;;
;; 2. It is the only bootstrap procedure that depends on the "hashtables"
;;    primitive category. Registry.WithoutCategory filters PRIMITIVES and leaves
;;    procedure sources alone, so an embedder removing that category must drop
;;    this source too, via WithProcedureSources. Keeping it addressable as its own
;;    slice entry is what makes that possible. (Four other categories — vectors,
;;    strings, pairs, lists — are already unremovable for exactly this reason,
;;    because their bootstrap dependents are not separable. See
;;    TestWithoutCategory_RemoveHashtables.)
;;
;; It is Scheme rather than Go because it calls PROC. A Go implementation would
;; need a sub-context and would truncate any continuation PROC captures — the
;; failure that moved map, for-each, member and assoc out of Go. The same rule
;; pre-decides any future hashtable-walk / hashtable-fold. R6RS defines
;; hashtable-update! as literally equivalent to the body below, so this is the
;; spec text, not a reimplementation.

(define (hashtable-update! ht key proc default)
  "Sets HT's value for KEY to (PROC v), where v is the current value for KEY, or\nDEFAULT if KEY is absent.\n\nParameters:\n  ht : hashtable\n  key : any\n  proc : procedure\n  default : any\nReturns: void\nCategory: hashtables\n\nSee also: `hashtable-set!', `hashtable-ref'."
  (hashtable-set! ht key (proc (hashtable-ref ht key default))))
