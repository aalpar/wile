;;; strings-test.scm - R7RS 6.7 Strings: edge cases and detailed coverage
;;;
;;; Edge cases and detailed coverage extracted from Go test suite
;;; (internal/extensions/all/prim_strings_test.go).
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base) (scheme char) (chibi test))

(test-begin "strings")

;; ── Case-insensitive comparison: variadic and edge cases ─────────

(test-group "string-ci=? variadic"
  (test #t (string-ci=? "abc" "ABC" "Abc"))
  (test #f (string-ci=? "abc" "ABC" "abd"))
  (test #t (string-ci=? "abc")))

(test-group "string-ci<? variadic"
  (test #t (string-ci<? "abc" "DEF" "ghi"))
  (test #f (string-ci<? "abc" "DEF" "def"))
  (test #t (string-ci<? "ab" "ABC")))

(test-group "string-ci>? variadic"
  (test #t (string-ci>? "ghi" "DEF" "abc")))

(test-group "string-ci<=? variadic"
  (test #t (string-ci<=? "abc" "ABC" "def")))

(test-group "string-ci>=? variadic"
  (test #t (string-ci>=? "def" "ABC" "abc")))

(test-group "string-ci=? error"
  (test-error (string-ci=? "abc" 42)))

;; ── Eszett and Unicode case folding ──────────────────────────────

(test-group "eszett equality"
  ;; ß (U+00DF) and ẞ (U+1E9E) both fold to "ss"
  (test #t (string-ci=? "ß" "SS"))
  (test #t (string-ci=? "ẞ" "SS"))
  (test #t (string-ci=? "ß" "ss"))
  (test #t (string-ci=? "ẞ" "ss")))

(test-group "eszett ordering"
  ;; After case folding: ß → "ss", so ß and SS are equal
  (test #f (string-ci<? "ß" "SS"))
  (test #f (string-ci>? "ß" "SS"))
  (test #t (string-ci<=? "ß" "SS"))
  (test #t (string-ci>=? "ß" "SS"))
  ;; "ss" < "st"
  (test #t (string-ci<? "ß" "ST"))
  (test #t (string-ci<? "ß" "st"))
  ;; "ss" > "sr"
  (test #t (string-ci>? "ß" "SR"))
  (test #t (string-ci>? "ß" "sr")))

(test-group "consistency with string-foldcase"
  ;; R7RS §6.7: (string-ci<? s1 s2) ≡ (string<? (string-foldcase s1) (string-foldcase s2))
  (test #t (eq? (string-ci<? "ß" "SS")
                (string<? (string-foldcase "ß") (string-foldcase "SS"))))
  (test #t (eq? (string-ci<? "ẞ" "ss")
                (string<? (string-foldcase "ẞ") (string-foldcase "ss"))))
  (test #t (eq? (string-ci<? "Hello" "WORLD")
                (string<? (string-foldcase "Hello") (string-foldcase "WORLD"))))
  (test #t (eq? (string-ci<? "abc" "ABC")
                (string<? (string-foldcase "abc") (string-foldcase "ABC")))))

;; ── Case mapping ─────────────────────────────────────────────────

(test-group "string-upcase"
  (test "HELLO" (string-upcase "hello"))
  (test "HELLO" (string-upcase "HELLO"))
  (test "HELLO WORLD" (string-upcase "Hello World"))
  (test "" (string-upcase ""))
  (test "ABC123" (string-upcase "abc123"))
  (test-error (string-upcase 42)))

(test-group "string-downcase"
  (test "hello" (string-downcase "HELLO"))
  (test "hello" (string-downcase "hello"))
  (test "hello world" (string-downcase "Hello World"))
  (test "" (string-downcase ""))
  (test "abc123" (string-downcase "ABC123")))

(test-group "string-foldcase"
  (test "hello" (string-foldcase "HELLO"))
  (test "hello" (string-foldcase "hello"))
  (test "hello world" (string-foldcase "Hello World"))
  (test "" (string-foldcase ""))
  ;; foldcase is idempotent
  (test #t (string=? (string-foldcase (string-foldcase "HeLLo"))
                     (string-foldcase "HeLLo"))))

;; ── string-copy! ─────────────────────────────────────────────────

(test-group "string-copy!"
  (test "hello"
    (let ((s (string-copy "aaaaa"))) (string-copy! s 0 "hello") s))
  (test "aaxya"
    (let ((s (string-copy "aaaaa"))) (string-copy! s 2 "xy") s))
  (test "elloa"
    (let ((s (string-copy "aaaaa"))) (string-copy! s 0 "hello" 1) s))
  (test "aelaa"
    (let ((s (string-copy "aaaaa"))) (string-copy! s 1 "hello" 1 3) s))
  (test "hello"
    (let ((s (string-copy "hello"))) (string-copy! s 0 "xyz" 1 1) s))
  (test "abcde"
    (let ((s (string-copy "xxxxx"))) (string-copy! s 0 "abcde") s)))

(test-group "string-copy! errors"
  (test-error (let ((s (string-copy "abc"))) (string-copy! s 2 "hello")))
  (test-error (let ((s (string-copy "abc"))) (string-copy! s -1 "x")))
  (test-error (let ((s (string-copy "abc"))) (string-copy! s 0 "hello" 3 1)))
  (test-error (string-copy! 42 0 "abc"))
  (test-error (let ((s (string-copy "abc"))) (string-copy! s 0 42))))

;; ── string-fill! ─────────────────────────────────────────────────

(test-group "string-fill!"
  (test "xxxxx"
    (let ((s (string-copy "hello"))) (string-fill! s #\x) s))
  (test "hexxx"
    (let ((s (string-copy "hello"))) (string-fill! s #\x 2) s))
  (test "hxxlo"
    (let ((s (string-copy "hello"))) (string-fill! s #\x 1 3) s))
  (test "hello"
    (let ((s (string-copy "hello"))) (string-fill! s #\x 2 2) s))
  (test ""
    (let ((s (string-copy ""))) (string-fill! s #\x) s)))

(test-group "string-fill! errors"
  (test-error (let ((s (string-copy "hello"))) (string-fill! s #\x 3 1)))
  (test-error (let ((s (string-copy "abc"))) (string-fill! s #\x 0 5)))
  (test-error (string-fill! 42 #\x))
  (test-error (let ((s (string-copy "abc"))) (string-fill! s "x"))))

;; ── string-map ───────────────────────────────────────────────────

(test-group "string-map"
  (test "HELLO" (string-map char-upcase "hello"))
  (test "hello" (string-map (lambda (c) c) "hello"))
  (test "" (string-map char-upcase ""))
  ;; two strings of different lengths: operates on min length
  (test "abc" (string-map (lambda (a b) a) "abcde" "xyz")))

(test-group "string-map errors"
  (test-error (string-map 42 "hello"))
  (test-error (string-map char-upcase 42))
  (test-error (string-map char-upcase)))

;; ── string-for-each ──────────────────────────────────────────────

(test-group "string-for-each"
  (test "abc"
    (let ((acc ""))
      (string-for-each
       (lambda (c) (set! acc (string-append acc (string c))))
       "abc")
      acc))
  (test 0
    (let ((count 0))
      (string-for-each (lambda (c) (set! count (+ count 1))) "")
      count))
  (test 5
    (let ((count 0))
      (string-for-each (lambda (c) (set! count (+ count 1))) "hello")
      count))
  ;; two strings of different lengths: operates on min length
  (test 2
    (let ((count 0))
      (string-for-each (lambda (a b) (set! count (+ count 1))) "abcde" "xy")
      count)))

(test-group "string-for-each errors"
  (test-error (string-for-each 42 "hello"))
  (test-error (string-for-each (lambda (c) c) 42)))

(test-end)
(test-exit)
