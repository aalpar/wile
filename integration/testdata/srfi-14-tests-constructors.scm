;; SRFI-14 Phase 1: constructors + membership + querying integration tests.
;; Covers char-set, char-set-copy, string->char-set, list->char-set,
;; ucs-range->char-set, char-set->list, char-set->string, char-set-contains?,
;; char-set-size, ->char-set dispatcher.

(import (scheme base)
        (chibi test)
        (srfi 14))

(test-begin "srfi-14-constructors")

(test-group "->char-set dispatcher"
  (test #t (char-set? (->char-set "abc")))
  (test #t (char-set? (->char-set #\a)))
  (test #t (char-set? (->char-set (char-set #\a #\b))))
  (test-error (->char-set 42))
  (test-error (->char-set 'symbol)))

(test-group "(char-set ...) edge cases"
  (test 0 (char-set-size (char-set)))
  (test 1 (char-set-size (char-set #\a)))
  (test 3 (char-set-size (char-set #\a #\b #\c))))

(test-group "string<->char-set roundtrip"
  (test "abc" (char-set->string (string->char-set "abc")))
  (test "abc" (char-set->string (string->char-set "aabbcc"))))

(test-group "list<->char-set roundtrip"
  (test '(#\a #\b #\c) (char-set->list (list->char-set '(#\a #\b #\c))))
  (test '(#\a #\b)     (char-set->list (list->char-set '(#\a #\a #\b)))))

(test-group "ucs-range->char-set"
  (test 25 (char-set-size (ucs-range->char-set 65 90)))
  (test 0  (char-set-size (ucs-range->char-set 65 65)))
  (test-error    (ucs-range->char-set 90 65))
  (test-error    (ucs-range->char-set 0 #x200000))
  (test 2  (char-set-size (ucs-range->char-set #x10FFFE #x200000 #f))))

(test-group "char-set-copy"
  (test #t (char-set? (char-set-copy (char-set #\a)))))

(test-end "srfi-14-constructors")

(test-exit)
