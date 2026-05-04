(define-library (wile strings)
  (description "Wile string library: SRFI-13 surface plus Wile-specific extras (string-split, string-replace-all, etc.). Single-import convenience over (srfi 13). Re-exports the variadic R7RS comparison forms (string=?, string-ci=?, ...) alongside the SRFI-13 binary forms (string=, string-ci=, ...) so consumers can pick whichever shape fits. NOTE: SRFI-13 string-map shadows R7RS string-map; this library re-exports the SRFI-13 form. Programs needing R7RS multi-string string-map should import from (scheme base) directly.")
  (import (except (scheme base) string-map)
          (scheme char)
          (srfi 13))
  (export
   ;; -- R7RS variadic comparisons (no slicing) -- from (scheme base) --
   string=? string<? string>? string<=? string>=?
   ;; -- R7RS case-insensitive variadic comparisons -- from (scheme char) --
   string-ci=? string-ci<? string-ci>? string-ci<=? string-ci>=?
   ;; -- R7RS case conversion -- from (scheme char) --
   string-upcase string-downcase string-foldcase
   ;; -- Re-exported from (srfi 13) -- Phase 1 --
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join
   ;; -- Re-exported from (srfi 13) -- Phase 2 --
   string-null? string-every string-any
   string-take string-drop string-take-right string-drop-right
   substring/shared string-tabulate
   string-prefix-ci? string-suffix-ci?
   string-prefix-length string-suffix-length
   string-prefix-length-ci string-suffix-length-ci
   ;; -- Re-exported from (srfi 13) -- Phase 3 --
   string-index string-index-right
   string-skip string-skip-right
   string-count
   ;; -- Re-exported from (srfi 13) -- Phase 4 --
   string-trim string-trim-right string-trim-both string-trim-left
   string-pad string-pad-right
   ;; -- Re-exported from (srfi 13) -- Phase 5 (binary, with optional ranges) --
   string=  string<  string>  string<=  string>=  string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>
   string-compare string-compare-ci
   ;; -- Re-exported from (srfi 13) -- Phase 6 --
   ;; (string-map is the SRFI-13 single-string form; R7RS variadic excluded above)
   string-reverse string-reverse!
   string-replace string-tokenize
   string-filter string-delete
   string-concatenate reverse-list->string
   string-for-each-index string-map
   string-fold string-fold-right
   ;; -- Re-exported from (srfi 13) -- Phase 7 --
   string-upcase! string-downcase!
   ;; -- Wile extras -- Phase 1 --
   string-split)
  (include "strings/extras.scm"))
