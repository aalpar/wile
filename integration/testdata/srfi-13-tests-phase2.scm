;; SRFI-13 Phase-2 integration tests
;; Covers: string-null?, string-every, string-any, string-take, string-drop,
;; string-take-right, string-drop-right, substring/shared, string-tabulate,
;; string-prefix-ci?, string-suffix-ci?, string-prefix-length,
;; string-suffix-length, string-prefix-length-ci, string-suffix-length-ci.

(import (except (scheme base) string-map) (scheme write) (except (scheme char) string-upcase string-downcase) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 2")

;; ============================================================
;; Predicates
;; ============================================================

(test-begin "string-null?")
(test "empty"     #t (string-null? ""))
(test "non-empty" #f (string-null? "x"))
(test "spaces"    #f (string-null? "   "))
(test-end "string-null?")

(test-begin "string-every")
(test "all-alpha"   #t (string-every char-alphabetic? "abc"))
(test "mixed"       #f (string-every char-alphabetic? "ab1"))
(test "empty"       #t (string-every char-alphabetic? ""))
(test "char-crit"   #t (string-every #\a "aaa"))
(test "char-crit-no" #f (string-every #\a "aab"))
(test "with-start"  #t (string-every char-numeric? "ab12" 2))
(test "with-end"    #t (string-every char-alphabetic? "ab12" 0 2))
(test "with-range"  #t (string-every char-numeric? "ab12cd" 2 4))
(test-end "string-every")

(test-begin "string-any")
(test "found-num"   #t (string-any char-numeric? "abc1"))
(test "no-num"      #f (string-any char-numeric? "abcd"))
(test "char-crit"   #t (string-any #\b "abc"))
(test "empty"       #f (string-any char-alphabetic? ""))
(test "with-start"  #f (string-any char-numeric? "12ab" 2))
(test "with-end"    #f (string-any char-numeric? "ab12" 0 2))
(test-end "string-any")

;; ============================================================
;; Selection
;; ============================================================

(test-begin "string-take")
(test "normal" "hel"   (string-take "hello" 3))
(test "zero"   ""      (string-take "hello" 0))
(test "all"    "hello" (string-take "hello" 5))
(test-error                  (string-take "hello" 6))   ;; n > length
(test-error                  (string-take "hello" -1))  ;; n negative
(test-end "string-take")

(test-begin "string-drop")
(test "normal" "lo"    (string-drop "hello" 3))
(test "zero"   "hello" (string-drop "hello" 0))
(test "all"    ""      (string-drop "hello" 5))
(test-end "string-drop")

(test-begin "string-take-right")
(test "normal" "llo"  (string-take-right "hello" 3))
(test "zero"   ""     (string-take-right "hello" 0))
(test "all"    "hello" (string-take-right "hello" 5))
(test-end "string-take-right")

(test-begin "string-drop-right")
(test "normal" "he"    (string-drop-right "hello" 3))
(test "zero"   "hello" (string-drop-right "hello" 0))
(test "all"    ""      (string-drop-right "hello" 5))
(test-end "string-drop-right")

(test-begin "substring/shared")
(test "basic" "ell" (substring/shared "hello" 1 4))
;; END is optional in SRFI-13. The former assertion here was
;; (eq? substring substring/shared), which pinned the defect rather than the
;; contract: it held precisely because substring/shared was an alias for the
;; 3-argument substring, and so passed while (substring/shared "abcdef" 2)
;; raised a wrong-number-of-arguments error.
(test "optional-end" "cdef" (substring/shared "abcdef" 2))
(test "optional-end-whole" "abcdef" (substring/shared "abcdef" 0))
(test "optional-end-empty" "" (substring/shared "abcdef" 6))
;; SRFI-13 permits sharing but does not require it; Wile copies, so the result
;; must not be the input even when the range is the whole string.
(test "copies" #f (eq? "abcdef" (substring/shared "abcdef" 0)))
(test-end "substring/shared")

(test-begin "string-tabulate")
(test "letters" "ABCDE"
      (string-tabulate (lambda (i) (integer->char (+ 65 i))) 5))
(test "fixed" "xxx"
      (string-tabulate (lambda (i) #\x) 3))
(test "empty" ""
      (string-tabulate (lambda (i) #\x) 0))
(test "indices" "01234"
      (string-tabulate (lambda (i) (integer->char (+ (char->integer #\0) i))) 5))
(test-end "string-tabulate")

;; ============================================================
;; Prefix/suffix length
;; ============================================================

(test-begin "string-prefix-length")
(test "match-5"   5 (string-prefix-length "foobar" "foobaz"))
(test "no-match"  0 (string-prefix-length "abc" "xyz"))
(test "empty1"    0 (string-prefix-length "" "abc"))
(test "empty2"    0 (string-prefix-length "abc" ""))
(test "exact"     3 (string-prefix-length "abc" "abc"))
;; With start1=1, s1 substring is "foobar" -> common prefix with "foobaz" = "fooba" = 5
(test "with-start1" 5 (string-prefix-length "xfoobar" "foobaz" 1))
;; With end1=4 on "foobar" gives "foob" vs "foobaz" -> 4
(test "with-end1" 4 (string-prefix-length "foobar" "foobaz" 0 4))
(test-end "string-prefix-length")

(test-begin "string-suffix-length")
;; "foobar" vs "goobar" -> common suffix "oobar" = 5
(test "match-5"   5 (string-suffix-length "foobar" "goobar"))
(test "no-match"  0 (string-suffix-length "abc" "xyz"))
(test "empty1"    0 (string-suffix-length "" "abc"))
(test "exact"     3 (string-suffix-length "abc" "abc"))
;; With end1=6 on "foobarz" gives "foobar" -> common suffix with "goobar" = 5
(test "with-end1" 5 (string-suffix-length "foobarz" "goobar" 0 6))
(test-end "string-suffix-length")

;; ============================================================
;; Case-insensitive prefix/suffix
;; ============================================================

(test-begin "string-prefix-ci?")
(test "upper-pattern" #t (string-prefix-ci? "FOO" "foobar"))
(test "mixed"         #t (string-prefix-ci? "FoO" "fOObar"))
(test "no-match"      #f (string-prefix-ci? "BAR" "foobar"))
(test "empty"         #t (string-prefix-ci? "" "anything"))
(test-end "string-prefix-ci?")

(test-begin "string-suffix-ci?")
(test "upper-pattern" #t (string-suffix-ci? "BAR" "foobar"))
(test "mixed"         #t (string-suffix-ci? "BaR" "FooBaR"))
(test "no-match"      #f (string-suffix-ci? "FOO" "foobar"))
(test "empty"         #t (string-suffix-ci? "" "anything"))
(test-end "string-suffix-ci?")

(test-begin "string-prefix-length-ci")
(test "match-5"  5 (string-prefix-length-ci "FOObar" "foobaz"))
(test "no-match" 0 (string-prefix-length-ci "ABC" "xyz"))
(test "exact"    3 (string-prefix-length-ci "abc" "ABC"))
(test-end "string-prefix-length-ci")

(test-begin "string-suffix-length-ci")
;; "FOOBAR" foldcased vs "goobar" -> common suffix "oobar" = 5
(test "match-5"  5 (string-suffix-length-ci "FOOBAR" "goobar"))
(test "no-match" 0 (string-suffix-length-ci "abc" "XYZ"))
(test "exact"    3 (string-suffix-length-ci "abc" "ABC"))
(test-end "string-suffix-length-ci")

(test-end "srfi-13 phase 2")
(test-exit)
