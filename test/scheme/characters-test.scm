;;; characters-test.scm - R7RS 6.6 Characters: edge cases and detailed coverage
;;;
;;; Edge cases and detailed coverage extracted from Go test suite
;;; (internal/extensions/all/prim_characters_test.go).
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base) (scheme char) (chibi test))

(test-begin "characters")

;; ── Case-insensitive comparison: edge cases ──────────────────────

(test-group "char-ci=? edge cases"
  ;; single arg: vacuous truth
  (test #t (char-ci=? #\a))
  ;; non-alpha characters
  (test #t (char-ci=? #\1 #\1)))

(test-group "char-ci=? error"
  (test-error (char-ci=? #\a 42)))

(test-group "char-ci<? edge cases"
  (test #f (char-ci<? #\a #\B #\b)))

(test-group "char-ci<=? edge cases"
  (test #t (char-ci<=? #\a #\A #\b)))

(test-group "char-ci>=? edge cases"
  (test #t (char-ci>=? #\b #\A #\a)))

;; ── Capital sharp S folding ──────────────────────────────────────

(test-group "capital sharp S"
  ;; ẞ (U+1E9E) folds to ß (U+00DF)
  (test #t (char-ci=? #\ẞ #\ß))
  (test #f (char-ci<? #\ẞ #\ß))
  (test #f (char-ci>? #\ẞ #\ß))
  (test #t (char-ci<=? #\ẞ #\ß))
  (test #t (char-ci>=? #\ẞ #\ß)))

(test-group "consistency with char-foldcase"
  ;; R7RS: char-ci comparisons should use char-foldcase semantics
  (test #t (eq? (char-ci<? #\A #\a)
                (char<? (char-foldcase #\A) (char-foldcase #\a))))
  (test #t (eq? (char-ci<? #\Z #\z)
                (char<? (char-foldcase #\Z) (char-foldcase #\z))))
  (test #t (eq? (char-ci<? #\ẞ #\ß)
                (char<? (char-foldcase #\ẞ) (char-foldcase #\ß)))))

;; ── Classification predicates: edge cases and errors ─────────────

(test-group "char-alphabetic?"
  (test #t (char-alphabetic? #\a))
  (test #t (char-alphabetic? #\Z))
  (test #f (char-alphabetic? #\0))
  (test #f (char-alphabetic? #\space))
  (test #t (char-alphabetic? #\α)))

(test-group "char-alphabetic? errors"
  (test-error (char-alphabetic? "a")))

(test-group "char-numeric?"
  (test #t (char-numeric? #\5))
  (test #t (char-numeric? #\0))
  (test #t (char-numeric? #\9))
  (test #f (char-numeric? #\a)))

(test-group "char-whitespace?"
  (test #t (char-whitespace? #\space))
  (test #t (char-whitespace? #\tab))
  (test #t (char-whitespace? #\newline))
  (test #f (char-whitespace? #\a))
  (test #f (char-whitespace? #\0)))

(test-group "char-upper-case?"
  (test #t (char-upper-case? #\A))
  (test #f (char-upper-case? #\a))
  (test #f (char-upper-case? #\0)))

(test-group "char-lower-case?"
  (test #t (char-lower-case? #\a))
  (test #f (char-lower-case? #\A))
  (test #f (char-lower-case? #\0)))

;; ── Case mapping ─────────────────────────────────────────────────

(test-group "char-upcase"
  (test #\A (char-upcase #\a))
  (test #\A (char-upcase #\A))
  (test #\5 (char-upcase #\5))
  (test #\space (char-upcase #\space)))

(test-group "char-upcase error"
  (test-error (char-upcase "a")))

(test-group "char-downcase"
  (test #\a (char-downcase #\A))
  (test #\a (char-downcase #\a))
  (test #\5 (char-downcase #\5)))

(test-group "char-foldcase"
  (test #\a (char-foldcase #\A))
  (test #\a (char-foldcase #\a))
  (test #\5 (char-foldcase #\5))
  ;; foldcase is idempotent
  (test #t (char=? (char-foldcase (char-foldcase #\Z))
                   (char-foldcase #\Z))))

;; ── digit-value ──────────────────────────────────────────────────

(test-group "digit-value"
  ;; ASCII digits 0-9
  (test 0 (digit-value #\0))
  (test 1 (digit-value #\1))
  (test 2 (digit-value #\2))
  (test 3 (digit-value #\3))
  (test 4 (digit-value #\4))
  (test 5 (digit-value #\5))
  (test 6 (digit-value #\6))
  (test 7 (digit-value #\7))
  (test 8 (digit-value #\8))
  (test 9 (digit-value #\9))
  ;; non-digits return #f
  (test #f (digit-value #\a))
  (test #f (digit-value #\space))
  ;; Unicode decimal digit scripts
  (test 0 (digit-value #\٠))  ; Arabic-Indic zero U+0660
  (test 5 (digit-value #\٥))  ; Arabic-Indic five U+0665
  (test 0 (digit-value #\०))  ; Devanagari zero U+0966
  (test 9 (digit-value #\९))) ; Devanagari nine U+096F

(test-group "digit-value errors"
  (test-error (digit-value 5)))

(test-end)
