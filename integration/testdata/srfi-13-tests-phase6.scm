;; SRFI-13 Phase-6 integration tests
;; Covers: string-reverse, string-reverse!, string-replace, string-tokenize,
;; string-filter, string-delete, string-concatenate, reverse-list->string,
;; string-for-each-index, string-map (SRFI-13 form), string-fold,
;; string-fold-right.

(import (except (scheme base) string-map) (scheme write) (except (scheme char) string-upcase string-downcase) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 6")

;; ============================================================
;; string-reverse
;; ============================================================

(test-begin "string-reverse")
(test "basic"     "olleh"  (string-reverse "hello"))
(test "empty"     ""       (string-reverse ""))
(test "single"    "a"      (string-reverse "a"))
(test "with-range" "lle"   (string-reverse "hello" 1 4))
(test-end "string-reverse")

;; ============================================================
;; string-reverse!
;; ============================================================

(test-begin "string-reverse!")
(test "basic"  "olleh"
      (let ((s (string-copy "hello")))
        (string-reverse! s)
        s))
(test "even-length" "dcba"
      (let ((s (string-copy "abcd")))
        (string-reverse! s)
        s))
(test "with-range" "hLLEo"
      (let ((s (string-copy "hELLo")))
        (string-reverse! s 1 4)
        s))
(test-end "string-reverse!")

;; ============================================================
;; string-replace (splice)
;; ============================================================

(test-begin "string-replace")
(test "splice"      "aXYZdef"  (string-replace "abcdef" "XYZ" 1 3))
(test "delete"      "ac"       (string-replace "abc" "" 1 2))
(test "insert-end"  "hi"       (string-replace "" "hi" 0 0))
(test "insert-mid"  "aPQRbc"   (string-replace "abc" "PQR" 1 1))
(test "with-range"  "aBCDef"   (string-replace "abcdef" "ABCDE" 1 4 1 4))
(test "out-of-bounds" #t (guard (exn (else #t)) (string-replace "abc" "X" 0 5) #f))
(test-end "string-replace")

;; ============================================================
;; string-tokenize
;; ============================================================

(test-begin "string-tokenize")
(test "two-words" '("hello" "world") (string-tokenize "hello world"))
(test "extra-spaces" '("many" "spaces") (string-tokenize "  many   spaces  "))
(test "empty"   '() (string-tokenize ""))
(test "all-ws"  '() (string-tokenize "   "))
(test "single"  '("x") (string-tokenize "x"))
(test "comma-criterion" '("a" "b" "c")
      (string-tokenize "a,b,c" (lambda (ch) (not (char=? ch #\,)))))
(test-end "string-tokenize")

;; ============================================================
;; string-filter / string-delete
;; ============================================================

(test-begin "string-filter")
(test "numeric"   "123"   (string-filter char-numeric? "abc123def"))
(test "char-crit" "aaa"   (string-filter #\a "banana"))
(test "all-pass"  "abc"   (string-filter char-alphabetic? "abc"))
(test "none-pass" ""      (string-filter char-numeric? "abc"))
(test "empty"     ""      (string-filter char-alphabetic? ""))
(test-end "string-filter")

(test-begin "string-delete")
(test "numeric"   "abcdef" (string-delete char-numeric? "abc123def"))
(test "char-crit" "bnn"    (string-delete #\a "banana"))
(test "all-pass"  ""       (string-delete char-alphabetic? "abc"))
(test "none-pass" "abc"    (string-delete char-numeric? "abc"))
(test-end "string-delete")

;; ============================================================
;; concat helpers
;; ============================================================

(test-begin "string-concatenate")
(test "basic"  "foobarbaz" (string-concatenate '("foo" "bar" "baz")))
(test "empty"  ""          (string-concatenate '()))
(test "single" "hi"        (string-concatenate '("hi")))
(test-end "string-concatenate")

(test-begin "reverse-list->string")
(test "basic"  "abc" (reverse-list->string '(#\c #\b #\a)))
(test "empty"  ""    (reverse-list->string '()))
(test "single" "x"   (reverse-list->string '(#\x)))
(test-end "reverse-list->string")

;; ============================================================
;; fold / map
;; ============================================================

(test-begin "string-fold")
(test "to-list-rev" '(#\c #\b #\a)
      (string-fold cons '() "abc"))
(test "count" 3 (string-fold (lambda (ch acc) (+ acc 1)) 0 "abc"))
(test "empty" 'init (string-fold cons 'init ""))
(test "with-range" '(#\d #\c #\b)
      (string-fold cons '() "abcde" 1 4))
(test-end "string-fold")

(test-begin "string-fold-right")
(test "to-list" '(#\a #\b #\c)
      (string-fold-right cons '() "abc"))
(test "empty"   'init  (string-fold-right cons 'init ""))
(test "with-range" '(#\b #\c #\d)
      (string-fold-right cons '() "abcde" 1 4))
(test-end "string-fold-right")

(test-begin "string-for-each-index")
(test "indices" '(0 1 2)
      (let ((acc '()))
        (string-for-each-index (lambda (i) (set! acc (cons i acc))) "abc")
        (reverse acc)))
(test "empty" '()
      (let ((acc '()))
        (string-for-each-index (lambda (i) (set! acc (cons i acc))) "")
        acc))
(test-end "string-for-each-index")

(test-begin "string-map (SRFI-13)")
(test "upcase" "HELLO"   (string-map char-upcase "hello"))
(test "empty"  ""        (string-map char-upcase ""))
(test "with-range" "ELL" (string-map char-upcase "hello" 1 4))
(test-end "string-map (SRFI-13)")

(test-begin "string-map!")
(test "upcase" "HELLO"
      (let ((s (string-copy "hello")))
        (string-map! char-upcase s)
        s))
(test "start-only" "hELLO"
      (let ((s (string-copy "hello")))
        (string-map! char-upcase s 1)
        s))
(test "with-range" "hELLo"
      (let ((s (string-copy "hello")))
        (string-map! char-upcase s 1 4)
        s))
(test "empty" ""
      (let ((s (string-copy "")))
        (string-map! char-upcase s)
        s))
(test-end "string-map!")

(test-end "srfi-13 phase 6")
(test-exit)
