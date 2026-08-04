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
;; NOT EXPORTED: equal-hash, string-hash, string-ci-hash and symbol-hash, which
;; R6RS does list in this library. They are bound in Wile's sealed base, so a
;; program importing this library can still call them, and the export list is
;; therefore a completeness gap rather than a reachability one.
;;
;; It used to be load-bearing. make-hashtable recognized its (hash, equiv) pair by
;; CLOSURE POINTER against the sealed base, and an import installs the exporting
;; library's own copy of a primitive — a library environment is a flat island that
;; mints its own — so exporting equal-hash rebound the name to an object
;; recognition no longer matched. That is fixed: recognition is by primitive
;; IDENTITY (machine.PrimitiveIdentity), which every copy of a primitive shares.
;; Exporting these four is now safe, and the one thing to weigh before doing it is
;; that (srfi 13) exports a DIFFERENT, bounded string-hash — so a program importing
;; both would hit the R7RS §5.6 conflict, correctly but newly. Pinned by
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
   hashtable-mutable?))
