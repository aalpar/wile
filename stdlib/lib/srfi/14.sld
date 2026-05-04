;; SRFI-14 Character-Set Library
;;
;; Status: Phase 0 — predicate-only stub; constructors and algebra in later phases.
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
    ;; Phase 0: predicates only
    char-set?)

  (import (scheme base))

  ;; FFI primitives are bound at the namespace level by the `charsets` extension;
  ;; this library re-exports the SRFI-14-named subset.
  ;;
  ;; (No body forms in phase 0 — the export is satisfied directly by the
  ;; primitive bound in the importing environment.)
  )
