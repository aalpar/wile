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
;; THE HASH PROCEDURES. All four are exported, and that is how a macro body
;; reaches them: equal-hash, string-hash, string-ci-hash and symbol-hash all
;; register PhaseSetRuntime, so (import (for-syntax (rnrs hashtables))) is how a
;; phase-1 environment includes them, as
;; (import (for-syntax (scheme char))) includes string-ci=?. Needing the import
;; is a trade, not a cost: each environment chooses whether it includes the
;; hashes, at the price of verbosity and of every environment being built alike.
;; At phase 0 all four are also ambient, and importing this library does not
;; conflict with the ambient copies.
;;
;; History. string-ci-hash was exported first (2026-10-03, I066): it lives in
;; pkg/internal/extensions/all beside string-ci=? whose x/text FULL fold it must
;; share, nothing there is registered at phase 1, and no library exported it, so
;; it was reachable from a macro body by no route. Widening its registration to
;; PhaseSetRuntime|PhaseSetExpand was the first fix and was reverted. The other
;; three were ambient at phase 1 and unexported until 2026-10-04, when they moved
;; to the same route. Do not widen any of the four without re-deciding the trade
;; above.
;;
;; Two costs of the export, both ACCEPTED:
;;   - This library needs every hash it exports registered to load at all. An
;;     engine built with (Registry).Without("string-ci-hash") fails
;;     (import (rnrs hashtables)) with "exports 1 identifier(s) with no binding",
;;     where an unexported name would only have gone missing. Every profile that
;;     can load (scheme base) registers all four, and (scheme char) depends on
;;     string-ci=? the same way.
;;   - (srfi 13) exports a DIFFERENT, bounded string-hash, so a program importing
;;     both libraries meets the R7RS §5.6 conflict and narrows one side with
;;     except, only or rename. R6RS's own (rnrs hashtables) carries the same
;;     conflict. Pinned by TestStringHashConflictsBetweenRnrsHashtablesAndSRFI13.
;;
;; The export list was once load-bearing. make-hashtable recognized its
;; (hash, equiv) pair by CLOSURE POINTER against the sealed base, and an import
;; installs the exporting library's own copy of a primitive (a library
;; environment is a flat island that mints its own), so exporting equal-hash
;; rebound the name to an object recognition no longer matched. Recognition is
;; now by primitive IDENTITY (machine.PrimitiveIdentity), which every copy
;; shares. Pinned by TestRnrsHashtablesRecognitionIsIndependentOfTheExportList.
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
   ;; Hash procedures. This export is how a macro body reaches them; see the
   ;; note above.
   equal-hash string-hash string-ci-hash symbol-hash))
