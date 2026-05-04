(define-library (wile strings)
  (description "Wile string library: SRFI-13 surface plus Wile-specific extras (string-split, string-replace-all, etc.). Single-import convenience over (srfi 13).")
  (import (scheme base)
          (srfi 13))
  (export
   ;; Re-exported from (srfi 13) -- Phase 1
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join
   ;; Wile extras -- Phase 1
   string-split)
  (include "strings/extras.scm"))
