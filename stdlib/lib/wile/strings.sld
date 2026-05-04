(define-library (wile strings)
  (description "Wile string library: SRFI-13 surface plus Wile-specific extras (string-split, string-replace-all, etc.). Single-import convenience over (srfi 13).")
  (import (scheme base)
          (srfi 13))
  (export
   ;; Re-exported from (srfi 13) -- Phase 1
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join
   ;; Re-exported from (srfi 13) -- Phase 2
   string-null? string-every string-any
   string-take string-drop string-take-right string-drop-right
   substring/shared string-tabulate
   string-prefix-ci? string-suffix-ci?
   string-prefix-length string-suffix-length
   string-prefix-length-ci string-suffix-length-ci
   ;; Re-exported from (srfi 13) -- Phase 3
   string-index string-index-right
   string-skip string-skip-right
   string-count
   ;; Re-exported from (srfi 13) -- Phase 4
   string-trim string-trim-right string-trim-both string-trim-left
   string-pad string-pad-right
   ;; Wile extras -- Phase 1
   string-split)
  (include "strings/extras.scm"))
