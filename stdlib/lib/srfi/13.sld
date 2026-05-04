(define-library (srfi 13)
  (description "SRFI 13: String Library — string predicates, search, trim, pad, replace, tokenize. v1: char + predicate criteria only (char-set deferred to SRFI-14). v1 implementation is pure Scheme; FFI promotion deferred to profile-driven future work.")
  (import (scheme base)
          (scheme char))
  (export
   ;; Phase 1: wile-goast top-five (SRFI-13 subset)
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join
   ;; Phase 2: predicates
   string-null? string-every string-any
   ;; Phase 2: selection
   string-take string-drop string-take-right string-drop-right
   substring/shared string-tabulate
   ;; Phase 2: full prefix/suffix family
   string-prefix-ci? string-suffix-ci?
   string-prefix-length string-suffix-length
   string-prefix-length-ci string-suffix-length-ci
   ;; Phase 3: search family
   string-index string-index-right
   string-skip string-skip-right
   string-count
   ;; Phase 4: trim and pad (string-trim-left is an alias for string-trim)
   string-trim string-trim-right string-trim-both string-trim-left
   string-pad string-pad-right
   ;; Phase 5: comparison (binary, with optional ranges)
   string=  string<  string>  string<=  string>=  string<>
   string-ci= string-ci< string-ci> string-ci<= string-ci>= string-ci<>
   string-compare string-compare-ci)
  (include "13/util.scm"
           "13/predicates.scm"
           "13/selection.scm"
           "13/prefix-suffix.scm"
           "13/search.scm"
           "13/replace.scm"
           "13/trim-pad.scm"
           "13/comparison.scm"))
