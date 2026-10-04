;; (rnrs hashtables) — R6RS Standard Libraries, chapter 13.
;;
;; Every binding is a core primitive or a bootstrap procedure in the sealed base;
;; this library re-exports them under the R6RS name so a portable R6RS program's
;; import form resolves. It is the FIRST (rnrs ...) library in the tree, and that
;; namespace precedent is something this change acquires rather than argues for.
;;
;; make-equal-hashtable is exported but is NOT R6RS — it is the Chez / Larceny /
;; Vicare / Ypsilon extension. A program that imports only this library and uses
;; it is not portable to a strict R6RS implementation. The portable spelling is
;; (make-hashtable equal-hash equal?).
;;
;; NOT EXPORTED: equal-hash, string-hash and symbol-hash, which R6RS does list in
;; this library. They are bound in Wile's sealed base at BOTH phase 0 and phase 1,
;; so a program importing this library can still call them from ordinary code and
;; from a macro body alike, and for those three the export list is a completeness
;; gap rather than a reachability one.
;;
;; string-ci-hash IS exported (2026-10-03), and the reason is the phase, not
;; completeness. It is registered PhaseSetRuntime only, because it lives in
;; pkg/internal/extensions/all beside string-ci=? whose x/text FULL fold it must
;; share, and nothing in that package is registered at phase 1. So unlike its
;; three siblings it was NOT reachable from a macro body by any route: the sealed
;; base does not carry it at phase 1 and no library exported it. Exporting it gives
;; it the same import path its contract partner already has —
;; (import (for-syntax (scheme char))) reaches string-ci=?, and
;; (import (for-syntax (rnrs hashtables))) now reaches the hash — which is the
;; R7RS-sanctioned way to get a binding at phase 1. Measured: the phase-1 pair
;; agrees on the full-fold case, (string-ci=? "ß" "SS") and
;; (= (string-ci-hash "ß") (string-ci-hash "SS")) both #t.
;;
;; This was first fixed by widening the registration to
;; PhaseSetRuntime|PhaseSetExpand, which would have made string-ci-hash the only
;; both-phase registration outside pkg/registry/core (1 against 34) and bought
;; AMBIENT parity at the cost of the layer pattern. Reverted in favour of this,
;; which buys REACHABILITY parity and leaves ambient availability following the
;; layer. The residual asymmetry is deliberate: three of the four are callable
;; bare in a macro body and this one needs a for-syntax import.
;;
;; It used to be load-bearing. make-hashtable recognized its (hash, equiv) pair by
;; CLOSURE POINTER against the sealed base, and an import installs the exporting
;; library's own copy of a primitive — a library environment is a flat island that
;; mints its own — so exporting equal-hash rebound the name to an object
;; recognition no longer matched. That is fixed: recognition is by primitive
;; IDENTITY (machine.PrimitiveIdentity), which every copy of a primitive shares.
;; Exporting these four is now safe, and the one thing left to weigh for the
;; remaining three is that (srfi 13) exports a DIFFERENT, bounded string-hash — so
;; a program importing both would hit the R7RS §5.6 conflict, correctly but newly.
;; That conflict does not reach string-ci-hash: (srfi 13) exports string-hash
;; alone, which is why the phase fix could be scoped to the one name. Pinned by
;; TestRnrsHashtablesRecognitionIsIndependentOfTheExportList.
;;
;; Not provided: make-hashtable with a USER-SUPPLIED hash/equivalence pair, which
;; raises, and the R6RS condition system — every R6RS "raises &assertion" here
;; raises a Wile sentinel instead. See docs/reference/r7rs-differences.md
;; items 12-15.
(define-library (rnrs hashtables)
  (description "R6RS (rnrs hashtables): eq / eqv / equal hashtables, the R6RS procedure surface, and the standard hash functions. make-hashtable accepts only the built-in (equal-hash, equal?) pair; user-supplied hash and equivalence procedures raise.")
  (import (scheme base))
  (export
   ;; Constructors. make-equal-hashtable is the non-R6RS extension.
   make-eq-hashtable make-eqv-hashtable make-hashtable
   make-equal-hashtable
   ;; Predicates and access
   hashtable? hashtable-size hashtable-ref hashtable-set!
   hashtable-delete! hashtable-contains? hashtable-update!
   ;; Whole-table operations
   hashtable-copy hashtable-clear! hashtable-keys hashtable-entries
   ;; Inspection
   hashtable-equivalence-function hashtable-hash-function
   hashtable-mutable?
   ;; Hash procedures. Only string-ci-hash, and for a PHASE reason rather than a
   ;; completeness one — see the NOT EXPORTED note above.
   string-ci-hash))
