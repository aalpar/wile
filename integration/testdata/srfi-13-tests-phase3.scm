;; SRFI-13 Phase-3 integration tests
;; Covers: string-index, string-index-right, string-skip, string-skip-right,
;; string-count.

(import (except (scheme base) string-map) (scheme write) (scheme char) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 3")

;; ============================================================
;; string-index
;; ============================================================

(test-begin "string-index")
(test "char-found"     5  (string-index "hello world" #\space))
(test "char-missing"   #f (string-index "hello" #\z))
(test "pred-found"     3  (string-index "abc123" char-numeric?))
(test "pred-missing"   #f (string-index "hello" char-numeric?))
(test "with-start"     3  (string-index "hello" #\l 3))
(test "with-end"       2  (string-index "hello" #\l 0 3))
(test "out-of-range"   #f (string-index "hello" #\l 0 2))
(test "empty"          #f (string-index "" char-alphabetic?))
(test-error              (string-index "hello" "not-a-criterion"))
(test-end "string-index")

;; ============================================================
;; string-index-right
;; ============================================================

(test-begin "string-index-right")
(test "char-found"   3  (string-index-right "hello" #\l))
(test "char-missing" #f (string-index-right "hello" #\z))
(test "pred-found"   5  (string-index-right "abc123" char-numeric?))
(test "with-end"     2  (string-index-right "hello" #\l 0 3))
(test "with-start"   3  (string-index-right "hello" #\l 3))
(test "empty"        #f (string-index-right "" char-alphabetic?))
(test-end "string-index-right")

;; ============================================================
;; string-skip
;; ============================================================

(test-begin "string-skip")
(test "skip-spaces"  3  (string-skip "   hello" char-whitespace?))
(test "all-match"    #f (string-skip "   " char-whitespace?))
(test "first-mismatch" 0 (string-skip "hello" char-numeric?))
;; "aab" starting at 1: chars[1]=a (matches), chars[2]=b (does not) -> 2
(test "with-start"   2 (string-skip "aab" #\a 1))
(test "with-end"     #f (string-skip "aaa" #\a 0 3))
(test "empty"        #f (string-skip "" char-alphabetic?))
(test-end "string-skip")

;; ============================================================
;; string-skip-right
;; ============================================================

(test-begin "string-skip-right")
(test "skip-spaces"  4  (string-skip-right "hello   " char-whitespace?))
(test "all-match"    #f (string-skip-right "   " char-whitespace?))
(test "first-mismatch" 4 (string-skip-right "hello" char-numeric?))
(test "with-end"     1 (string-skip-right "aab" #\b 0 2))
(test "empty"        #f (string-skip-right "" char-alphabetic?))
(test-end "string-skip-right")

;; ============================================================
;; string-count
;; ============================================================

(test-begin "string-count")
(test "char-3"        3 (string-count "hello world" #\l))
(test "char-0"        0 (string-count "hello" #\z))
(test "pred-3"        3 (string-count "abc123" char-numeric?))
(test "pred-0"        0 (string-count "hello" char-numeric?))
(test "with-start"    1 (string-count "hello" #\l 3))
(test "with-range"    2 (string-count "hello world" #\l 0 5))
(test "empty"         0 (string-count "" char-alphabetic?))
(test-end "string-count")

(test-end "srfi-13 phase 3")
(test-exit)
