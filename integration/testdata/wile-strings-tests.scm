;; (wile strings) extras integration tests
;; Covers: string-split (Phase 1), string-replace-all, string-byte-length,
;; string-blank?, string-repeat (Phase 8).

(import (scheme base) (scheme write) (chibi test) (wile strings))

(test-begin "wile strings")

;; ============================================================
;; string-split (re-tested here as a (wile strings) consumer)
;; ============================================================

(test-begin "string-split")
(test "simple"   '("a" "b" "c") (string-split "a,b,c" #\,))
(test "no-delim" '("hello")     (string-split "hello" #\,))
(test "empty"    '("")          (string-split "" #\,))
(test-end "string-split")

;; ============================================================
;; string-replace-all
;; ============================================================

(test-begin "string-replace-all")
(test "basic"        "baz bar baz" (string-replace-all "foo bar foo" "foo" "baz"))
(test "no-match"     "hello"       (string-replace-all "hello" "x" "y"))
(test "empty-input"  ""            (string-replace-all "" "x" "y"))
;; Empty FROM is a no-op (matches Go strings.ReplaceAll behavior).
(test "empty-from"   "aaa"         (string-replace-all "aaa" "" "X"))
;; Left-to-right non-overlapping: "aaa" matches "aa" at 0, jump to 2, then 'a' remains.
(test "overlap"      "ba"          (string-replace-all "aaa" "aa" "b"))
;; Replacement longer than match (clean input with separator).
(test "longer-to"    "XXYXX"       (string-replace-all "abxYabx" "abx" "XX"))
(test "single-char"  "bbbbb"       (string-replace-all "aaaaa" "a" "b"))
(test "to-empty"     "abd"         (string-replace-all "abcd" "c" ""))
(test-end "string-replace-all")

;; ============================================================
;; string-byte-length
;; ============================================================

(test-begin "string-byte-length")
(test "ascii-3"        3 (string-byte-length "abc"))
(test "empty"          0 (string-byte-length ""))
;; é is U+00E9, 2 bytes in UTF-8 (0xC3 0xA9).
(test "two-byte-utf8"  2 (string-byte-length "é"))
;; Three-byte UTF-8 sequence (Cyrillic '€'-style char).
(test "three-byte"     3 (string-byte-length "€"))
(test-end "string-byte-length")

;; ============================================================
;; string-blank?
;; ============================================================

(test-begin "string-blank?")
(test "empty"        #t (string-blank? ""))
(test "spaces"       #t (string-blank? "   "))
(test "tabs"         #t (string-blank? "\t\t"))
(test "mixed-ws"     #t (string-blank? "  \t \n "))
(test "non-blank"    #f (string-blank? "hello"))
(test "leading-ws"   #f (string-blank? "  hi  "))
(test "single-char"  #f (string-blank? "x"))
(test-end "string-blank?")

;; ============================================================
;; string-repeat
;; ============================================================

(test-begin "string-repeat")
(test "basic"      "ababab"  (string-repeat "ab" 3))
(test "zero"       ""        (string-repeat "x" 0))
(test "empty-s"    ""        (string-repeat "" 5))
(test "single"     "x"       (string-repeat "x" 1))
(test-error                   (string-repeat "x" -1))
(test-end "string-repeat")

(test-end "wile strings")
(test-exit)
