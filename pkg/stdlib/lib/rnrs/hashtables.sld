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
;; DELIBERATELY NOT EXPORTED: equal-hash, string-hash, string-ci-hash and
;; symbol-hash, which R6RS does list in this library. They are already bound in
;; Wile's sealed base, so a program importing this library can still call them —
;; and exporting them here would BREAK the library's own headline feature.
;;
;; make-hashtable recognizes its (hash, equiv) pair by POINTER IDENTITY against
;; the sealed base. An import COPIES the exported binding into the importing
;; environment as a distinct object, so re-exporting equal-hash rebinds the name
;; to something make-hashtable no longer recognizes: measured, (import (rnrs
;; hashtables)) followed by (make-hashtable equal-hash equal?) raised
;; "unsupported hash/equivalence pair". Importing the R6RS library must not break
;; the R6RS spelling. Pinned by TestRnrsHashtablesDoesNotShadowTheRecognizedPair.
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
   hashtable-mutable?))
