(define-library (srfi 13)
  (description "SRFI 13: String Library — string predicates, search, trim, pad, replace, tokenize. v1: char + predicate criteria only (char-set deferred to SRFI-14). v1 implementation is pure Scheme; FFI promotion deferred to profile-driven future work.")
  (import (scheme base)
          (scheme char))
  (export
   ;; Phase 1: wile-goast top-five (SRFI-13 subset)
   string-prefix? string-suffix?
   string-contains string-contains-ci
   string-join)
  (include "13/util.scm"
           "13/prefix-suffix.scm"
           "13/search.scm"
           "13/replace.scm"))
