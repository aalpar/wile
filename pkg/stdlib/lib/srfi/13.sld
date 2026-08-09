(define-library (srfi 13)
  (description "SRFI 13: String Library — string predicates, search, trim, pad, replace, tokenize. Criterion arguments accept char, char-set (SRFI-14), or predicate procedure. v1 implementation is pure Scheme; FFI promotion deferred to profile-driven future work.")
  (import (scheme base)
          ;; SRFI-13's string-upcase / string-downcase take an optional
          ;; [start [end]] range that R7RS's one-argument forms do not, so this
          ;; library defines its own and delegates the no-range case to R7RS's
          ;; full Unicode mapping. The rename is what makes that possible: a
          ;; body-level (define string-upcase ...) shadows the import across the
          ;; whole library body, so the delegate has to arrive under another name.
          (except (scheme char) string-upcase string-downcase)
          (rename (only (scheme char) string-upcase string-downcase)
                  (string-upcase %r7rs-string-upcase)
                  (string-downcase %r7rs-string-downcase))
          (srfi 14))
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
   xsubstring
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
   string-compare string-compare-ci
   string-hash
   ;; Phase 6: reverse, replace/tokenize/filter/delete, concat, fold/map.
   ;; Note: string-map shadows R7RS string-map (single-string + range vs variadic).
   string-reverse string-reverse!
   string-replace string-tokenize
   string-filter string-delete
   string-concatenate reverse-list->string
   string-for-each-index string-map string-map!
   string-fold string-fold-right
   ;; Phase 7: mutating case (simple per-char mapping; see case.scm header)
   string-upcase! string-downcase!
   ;; Non-mutating case with an optional [start [end]] range. Like string-map
   ;; these shadow the R7RS bindings of the same name, so importing (scheme char)
   ;; alongside (srfi 13) needs (except (scheme char) string-upcase string-downcase).
   ;; The no-range call delegates to R7RS, so it is unchanged.
   string-upcase string-downcase
   ;; Phase 8: titlecase (non-mutating; see case.scm header)
   string-titlecase)
  (include "13/util.scm"
           "13/predicates.scm"
           "13/selection.scm"
           "13/prefix-suffix.scm"
           "13/search.scm"
           "13/replace.scm"
           "13/trim-pad.scm"
           "13/comparison.scm"
           "13/reverse.scm"
           "13/concat.scm"
           "13/fold.scm"
           "13/case.scm"))
