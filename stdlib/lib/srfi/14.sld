;; SRFI-14 Character-Set Library
;;
;; Status: Phase 4 complete — 17 named char-set constants (letter, lower-case,
;;          upper-case, title-case, digit, letter+digit, graphic, printing,
;;          whitespace, iso-control, punctuation, symbol, hex-digit, blank,
;;          ascii, empty, full).
;;          Phase 3 complete — iteration / query layer (fold, for-each, map,
;;          filter, count, every, any, unfold).
;;
;; Cuts deferred from v1 (per plans/2026-05-04-srfi-14-design.md §11):
;;   char-set-hash             — spec is loose; no portable algorithm.
;;   char-set-cursor protocol  — redundant with for-each / fold.
;;   char-set-ref              — (counted above)
;;   char-set-cursor-next      — (counted above)
;;   end-of-char-set?          — (counted above)
;;   char-set-diff+intersection / !  — niche optimization.

(define-library (srfi 14)
  (export
    ;; Phase 0: predicates
    char-set?
    ;; Phase 1: constructor and basic accessors
    char-set char-set-contains? char-set-size char-set-copy
    ;; Phase 1 Task 1.3: bulk constructors
    string->char-set list->char-set
    ;; Phase 1 Task 1.4: range constructor
    ucs-range->char-set
    ;; Phase 1 Task 1.5: char-set converters
    char-set->list char-set->string
    ;; Phase 1 Task 1.7: dispatching coercing constructor
    ->char-set
    ;; Phase 2 Task 2.1: equality and subset predicates
    char-set= char-set<=
    ;; Phase 2 Task 2.2: set-algebra
    char-set-union char-set-intersection char-set-difference
    char-set-xor char-set-complement
    ;; Phase 2 Task 2.4: derived + ! aliases
    char-set-adjoin char-set-delete
    char-set-adjoin! char-set-delete! char-set-complement!
    char-set-union! char-set-intersection! char-set-difference!
    char-set-xor! list->char-set! string->char-set!
    ucs-range->char-set!
    ;; Phase 3 Task 3.1: iteration
    char-set-fold char-set-for-each
    ;; Phase 3 Task 3.2: map + filter
    char-set-map char-set-filter char-set-filter!
    ;; Phase 3 Task 3.3: count + short-circuit predicates
    char-set-count char-set-every char-set-any
    ;; Phase 3 Task 3.4: unfold
    char-set-unfold char-set-unfold!
    ;; Phase 4 Task 4.2: named char-set constants
    char-set:letter char-set:lower-case char-set:upper-case char-set:title-case
    char-set:digit char-set:letter+digit char-set:graphic char-set:printing
    char-set:whitespace char-set:iso-control char-set:punctuation char-set:symbol
    char-set:hex-digit char-set:blank
    char-set:ascii char-set:empty char-set:full)

  (import (scheme base)
          (wile charsets))   ; for char-set-ranges, used by util.scm

  ;; FFI primitives (char-set?, char-set-contains?, char-set-size,
  ;; %char-set, %empty-char-set, char-set-copy) are bound at the namespace
  ;; level by the `charsets` extension. The include below defines the
  ;; Scheme-level (char-set ...) dispatcher that dispatches to those FFI
  ;; primitives.
  (include "14/dispatcher.scm")
  (include "14/algebra.scm")
  (include "14/util.scm")
  (include "14/iteration.scm")
  (include "14/named-sets.scm"))
