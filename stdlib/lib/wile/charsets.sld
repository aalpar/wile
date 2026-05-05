;; stdlib/lib/wile/charsets.sld
;;
;; (wile charsets) — Wile-specific extensions to SRFI-14.
;;
;; This library exposes Wile-internal helpers that aren't part of SRFI-14
;; but are useful for efficient char-set iteration in Scheme. See
;; plans/2026-05-04-srfi-14-design.md §5 for rationale.

(define-library (wile charsets)
  (description "Wile-specific extensions to SRFI-14: char-set-ranges for efficient iteration.")
  (export char-set-ranges)
  (import (scheme base)))
