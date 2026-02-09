(define-library (scheme char)
  (export char-alphabetic?
          char-numeric?
          char-whitespace?
          char-upper-case?
          char-lower-case?
          char-upcase
          char-downcase
          char-foldcase
          digit-value
          ;; Case-insensitive character comparisons (R7RS §6.6)
          char-ci=?
          char-ci<?
          char-ci>?
          char-ci<=?
          char-ci>=?
          ;; Case-insensitive string comparisons (R7RS §6.7)
          string-ci=?
          string-ci<?
          string-ci>?
          string-ci<=?
          string-ci>=?
          ;; String case conversion (R7RS §6.7)
          string-upcase
          string-downcase
          string-foldcase))
