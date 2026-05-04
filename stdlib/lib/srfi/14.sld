;; SRFI-14 Character-Set Library
;;
;; Status: Phase 1 — constructor, size, contains?, copy.
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
    ucs-range->char-set)

  (import (scheme base))

  ;; FFI primitives (char-set?, char-set-contains?, char-set-size,
  ;; %char-set, %empty-char-set, char-set-copy) are bound at the namespace
  ;; level by the `charsets` extension. The include below defines the
  ;; Scheme-level (char-set ...) dispatcher that dispatches to those FFI
  ;; primitives.
  (include "14/dispatcher.scm"))
