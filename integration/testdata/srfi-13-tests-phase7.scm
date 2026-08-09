;; SRFI-13 Phase-7 integration tests
;; Covers: string-upcase!, string-downcase!, and their non-mutating
;; optional-range counterparts string-upcase / string-downcase.
;;
;; The range arm exists because the export diff in
;; pkg/wile/library_export_diff_test.go compares NAMES: it cannot see that a
;; library exports the right name with the wrong arity, which is how
;; (string-upcase "abcd" 1 3) went on raising a wrong-number-of-arguments error
;; while every export assertion passed.

(import (except (scheme base) string-map) (scheme write) (except (scheme char) string-upcase string-downcase) (chibi test) (srfi 13))

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

;; ============================================================
;; Non-mutating case with an optional [start [end]] range.
;; SRFI-13 returns the mapped SUBSTRING, matching string-map's shape.
;; ============================================================

(test-begin "string-upcase")
(test "no-range"    "HELLO" (string-upcase "hello"))
(test "start-only"  "ELLO"  (string-upcase "hello" 1))
(test "start-end"   "ELL"   (string-upcase "hello" 1 4))
(test "empty-range" ""      (string-upcase "hello" 2 2))
;; The no-range call delegates to R7RS, so it keeps full Unicode mapping and may
;; change length — the property string-upcase! cannot have.
(test "full-Unicode" "SS" (string-upcase "ß"))
(test-end "string-upcase")

(test-begin "string-downcase")
(test "no-range"   "hello" (string-downcase "HELLO"))
(test "start-only" "ello"  (string-downcase "HELLO" 1))
(test "start-end"  "ell"   (string-downcase "HELLO" 1 4))
(test-end "string-downcase")

(test-end "srfi-13 phase 7")
(test-exit)
