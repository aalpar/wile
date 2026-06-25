;; SRFI-14 -> SRFI-13 unlock: verifies 7 SRFI-13 procedures now accept
;; char-set criteria (char-set-contains? dispatch via %match-char?).
;;
;; Pre-Phase-5 these procedures rejected char-set with the error message
;; "criterion must be char or procedure (char-set support deferred to SRFI-14)".

(import (except (scheme base) string-map) (chibi test) (srfi 13) (srfi 14))

(test-begin "srfi-14-srfi-13-unlock")

(test-group "string-index with char-set criterion"
  (test 5 (string-index "hello world" char-set:whitespace))
  (test 0 (string-index "abc"         char-set:letter))
  (test #f (string-index "12345"      char-set:letter)))

(test-group "string-trim with char-set criterion"
  ;; string-trim drops leading chars that match the criterion.
  (test "hi  " (string-trim "  hi  " char-set:whitespace))
  (test ""     (string-trim "    "   char-set:whitespace)))

(test-group "string-trim-both with char-set criterion"
  (test "hi" (string-trim-both "  hi  " char-set:whitespace))
  (test ""   (string-trim-both "    "   char-set:whitespace)))

(test-group "string-tokenize with char-set criterion"
  ;; string-tokenize collects runs of chars that MATCH the criterion.
  ;; char-set:letter matches word characters, so delimiters are skipped.
  (test '("foo" "bar" "baz")
        (string-tokenize "foo,bar;baz" char-set:letter))
  (test '("hello" "world")
        (string-tokenize "hello world" char-set:letter)))

(test-group "string-filter / string-delete with char-set criterion"
  (test "abc123" (string-filter
                   (char-set-union char-set:letter char-set:digit)
                   "abc 123!"))
  (test "abc 123" (string-delete (char-set #\!) "abc 123!")))

(test-group "string-count with char-set criterion"
  (test 3 (string-count "abc 12" char-set:letter))
  (test 2 (string-count "abc 12" char-set:digit)))

(test-group "negative: bad criterion still raises updated message"
  (test-error (string-index "hello" 42)))

(test-end "srfi-14-srfi-13-unlock")

(test-exit)
