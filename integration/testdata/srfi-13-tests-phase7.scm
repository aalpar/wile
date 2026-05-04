;; SRFI-13 Phase-7 integration tests
;; Covers: string-upcase!, string-downcase!.

(import (scheme base) (scheme write) (scheme char) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 7")

(test-begin "string-upcase!")

(test "basic" "HELLO"
      (let ((s (string-copy "hello")))
        (string-upcase! s)
        s))

(test "with-start" "heLLO"
      (let ((s (string-copy "hello")))
        (string-upcase! s 2)
        s))

(test "with-range" "hELLo"
      (let ((s (string-copy "hello")))
        (string-upcase! s 1 4)
        s))

(test "no-op-empty" ""
      (let ((s (string-copy "")))
        (string-upcase! s)
        s))

(test "already-upper" "ABC"
      (let ((s (string-copy "ABC")))
        (string-upcase! s)
        s))

;; Documented divergence: bang form uses simple per-char case mapping,
;; preserving length. R7RS string-upcase does full Unicode mapping.
(test "non-bang-full-Unicode" "SS" (string-upcase "ß"))
(test "bang-simple-mapping"   "ß"
      (let ((s (string-copy "ß")))
        (string-upcase! s)
        s))

(test-end "string-upcase!")

(test-begin "string-downcase!")

(test "basic" "hello"
      (let ((s (string-copy "HELLO")))
        (string-downcase! s)
        s))

(test "with-range" "HellO"
      (let ((s (string-copy "HELLO")))
        (string-downcase! s 1 4)
        s))

(test "no-op-empty" ""
      (let ((s (string-copy "")))
        (string-downcase! s)
        s))

(test "already-lower" "abc"
      (let ((s (string-copy "abc")))
        (string-downcase! s)
        s))

(test-end "string-downcase!")

(test-end "srfi-13 phase 7")
(test-exit)
