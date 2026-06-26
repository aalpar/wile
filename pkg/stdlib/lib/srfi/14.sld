(define-library (srfi 14)
  (description "SRFI 14: Character-Set Library — immutable inversion-list char-set value type, set algebra, named char-sets sourced from Unicode.")
  (export
    ;; Predicates
    char-set?
    ;; Constructor and basic accessors
    char-set char-set-contains? char-set-size char-set-copy
    ;; Bulk constructors
    string->char-set list->char-set
    ;; Range constructor
    ucs-range->char-set
    ;; Converters
    char-set->list char-set->string
    ;; Dispatching coercing constructor
    ->char-set
    ;; Equality and subset predicates
    char-set= char-set<=
    ;; Set algebra
    char-set-union char-set-intersection char-set-difference
    char-set-xor char-set-complement
    ;; Derived + ! aliases
    char-set-adjoin char-set-delete
    char-set-adjoin! char-set-delete! char-set-complement!
    char-set-union! char-set-intersection! char-set-difference!
    char-set-xor! list->char-set! string->char-set!
    ucs-range->char-set!
    ;; Iteration
    char-set-fold char-set-for-each
    ;; Map + filter
    char-set-map char-set-filter char-set-filter!
    ;; Count + short-circuit predicates
    char-set-count char-set-every char-set-any
    ;; Unfold
    char-set-unfold char-set-unfold!
    ;; Cursor protocol
    char-set-cursor char-set-ref char-set-cursor-next end-of-char-set?
    ;; Hash + combined diff/intersection
    char-set-hash char-set-diff+intersection char-set-diff+intersection!
    ;; Named char-set constants
    char-set:letter char-set:lower-case char-set:upper-case char-set:title-case
    char-set:digit char-set:letter+digit char-set:graphic char-set:printing
    char-set:whitespace char-set:iso-control char-set:punctuation char-set:symbol
    char-set:hex-digit char-set:blank
    char-set:ascii char-set:empty char-set:full)

  (import (scheme base)
          (wile charsets))   ; for char-set-ranges, used by util.scm and cursor.scm

  ;; FFI primitives (char-set?, char-set-contains?, char-set-size,
  ;; %char-set, %empty-char-set, char-set-copy, and the %char-set-union /
  ;; %char-set-intersection / %char-set-xor folds) are bound at the namespace
  ;; level by the `charsets` extension. The includes below define the
  ;; Scheme-level (char-set ...) dispatcher, the n-ary set-algebra wrappers
  ;; (which supply zero-arg identities atop the % folds), and the cursor
  ;; protocol — all dispatching to those FFI primitives.
  (include "14/dispatcher.scm")
  (include "14/algebra.scm")
  (include "14/util.scm")
  (include "14/iteration.scm")
  (include "14/cursor.scm")
  (include "14/named-sets.scm"))
