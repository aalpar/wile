;; SRFI-13 Phase-4 integration tests
;; Covers: string-trim, string-trim-right, string-trim-both, string-trim-left
;; (alias for string-trim), string-pad, string-pad-right.

(import (scheme base) (scheme write) (scheme char) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 4")

;; ============================================================
;; string-trim (left)
;; ============================================================

(test-begin "string-trim")
(test "leading-spaces"  "hello"     (string-trim "   hello"))
(test "leading-tabs"    "hello"     (string-trim "\t\thello"))
(test "no-trim"         "hello"     (string-trim "hello"))
(test "all-ws"          ""          (string-trim "   "))
(test "empty"           ""          (string-trim ""))
(test "trailing-kept"   "hello   "  (string-trim "   hello   "))
(test "char-criterion"  "bc"        (string-trim "aaaaabc" #\a))
(test "pred-criterion"  "abc"       (string-trim "123abc" char-numeric?))
(test "with-start"      "bc"        (string-trim "Xaabc" #\a 1))
(test-end "string-trim")

;; ============================================================
;; string-trim-right
;; ============================================================

(test-begin "string-trim-right")
(test "trailing-spaces" "hello"     (string-trim-right "hello   "))
(test "no-trim"         "hello"     (string-trim-right "hello"))
(test "all-ws"          ""          (string-trim-right "   "))
(test "leading-kept"    "   hello"  (string-trim-right "   hello   "))
(test "char-criterion"  "abc"       (string-trim-right "abcaaa" #\a))
(test-end "string-trim-right")

;; ============================================================
;; string-trim-both
;; ============================================================

(test-begin "string-trim-both")
(test "both-spaces"     "hello"     (string-trim-both "  hello  "))
(test "no-trim"         "hello"     (string-trim-both "hello"))
(test "all-ws"          ""          (string-trim-both "   "))
(test "char-criterion"  "bc"        (string-trim-both "aaabcaaa" #\a))
(test-end "string-trim-both")

;; ============================================================
;; string-trim-left  (alias)
;; ============================================================

(test-begin "string-trim-left alias")
(test "alias-eq"          #t        (eq? string-trim-left string-trim))
(test "behaves-like-trim" "hello"   (string-trim-left "   hello"))
(test-end "string-trim-left alias")

;; ============================================================
;; string-pad
;; ============================================================

(test-begin "string-pad")
(test "pad-left-default"  "   42"   (string-pad "42" 5))
(test "pad-with-zero"     "00042"   (string-pad "42" 5 #\0))
(test "truncate-left"     "llo"     (string-pad "hello" 3))
(test "exact-length"      "abc"     (string-pad "abc" 3))
(test "empty-padded"      "..."     (string-pad "" 3 #\.))
(test "zero-len"          ""        (string-pad "abc" 0))
(test-end "string-pad")

;; ============================================================
;; string-pad-right
;; ============================================================

(test-begin "string-pad-right")
(test "pad-right-default" "42   "   (string-pad-right "42" 5))
(test "pad-with-dot"      "42..."   (string-pad-right "42" 5 #\.))
(test "truncate-right"    "hel"     (string-pad-right "hello" 3))
(test "exact-length"      "abc"     (string-pad-right "abc" 3))
(test "empty-padded"      "..."     (string-pad-right "" 3 #\.))
(test "zero-len"          ""        (string-pad-right "abc" 0))
(test-end "string-pad-right")

(test-end "srfi-13 phase 4")
(test-exit)
