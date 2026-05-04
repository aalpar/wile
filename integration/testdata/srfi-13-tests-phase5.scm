;; SRFI-13 Phase-5 integration tests
;; Covers: string=, string<, string>, string<=, string>=, string<>,
;; string-ci=, string-ci<, ..., string-ci<>,
;; string-compare, string-compare-ci.

(import (scheme base) (scheme write) (chibi test) (srfi 13))

(test-begin "srfi-13 phase 5")

;; ============================================================
;; string= and friends
;; ============================================================

(test-begin "string= (binary)")
(test "equal"      #t (string= "abc" "abc"))
(test "not-equal"  #f (string= "abc" "abd"))
(test "diff-len"   #f (string= "abc" "abcd"))
(test "empty-eq"   #t (string= "" ""))
(test "with-range" #t (string= "xabc" "abcy" 1 4 0 3))
(test "with-range-no" #f (string= "xabc" "abcy" 0 3 0 3))
(test "all-args"   #t (string= "ABC" "DEF" 0 0 0 0))   ; empty slices
(test-end "string= (binary)")

(test-begin "string< / >")
(test "lt"         #t (string< "abc" "abd"))
(test "lt-prefix"  #t (string< "abc" "abcd"))
(test "lt-no"      #f (string< "abd" "abc"))
(test "lt-equal"   #f (string< "abc" "abc"))
(test "gt"         #t (string> "abd" "abc"))
(test "gt-no"      #f (string> "abc" "abd"))
(test-end "string< / >")

(test-begin "string<= / >=")
(test "le-equal"   #t (string<= "abc" "abc"))
(test "le-lt"      #t (string<= "abc" "abd"))
(test "le-no"      #f (string<= "abd" "abc"))
(test "ge-equal"   #t (string>= "abc" "abc"))
(test "ge-gt"      #t (string>= "abd" "abc"))
(test "ge-no"      #f (string>= "abc" "abd"))
(test-end "string<= / >=")

(test-begin "string<>")
(test "diff"       #t (string<> "abc" "abd"))
(test "same"       #f (string<> "abc" "abc"))
(test "diff-len"   #t (string<> "abc" "abcd"))
(test-end "string<>")

;; ============================================================
;; CI variants
;; ============================================================

(test-begin "string-ci=")
(test "equal-mix"  #t (string-ci= "AbC" "aBc"))
(test "not-equal"  #f (string-ci= "abc" "abd"))
(test "with-range" #t (string-ci= "XaBc" "ABCy" 1 4 0 3))
(test-end "string-ci=")

(test-begin "string-ci< / > / <=")
(test "ci-lt"      #t (string-ci< "abc" "ABD"))
(test "ci-gt"      #t (string-ci> "ABD" "abc"))
(test "ci-le-eq"   #t (string-ci<= "abc" "ABC"))
(test "ci-ge-eq"   #t (string-ci>= "abc" "ABC"))
(test-end "string-ci< / > / <=")

(test-begin "string-ci<>")
(test "diff"       #t (string-ci<> "abc" "ABD"))
(test "same"       #f (string-ci<> "abc" "ABC"))
(test-end "string-ci<>")

;; ============================================================
;; string-compare (3-way)
;; ============================================================

(test-begin "string-compare")
(define (lt-tag i) (cons 'lt i))
(define (eq-tag i) (cons 'eq i))
(define (gt-tag i) (cons 'gt i))

(test "lt-at-2"        '(lt . 2) (string-compare "abc" "abd" lt-tag eq-tag gt-tag))
(test "eq-at-end"      '(eq . 3) (string-compare "abc" "abc" lt-tag eq-tag gt-tag))
(test "lt-prefix"      '(lt . 3) (string-compare "abc" "abcd" lt-tag eq-tag gt-tag))
(test "gt-longer"      '(gt . 3) (string-compare "abcd" "abc" lt-tag eq-tag gt-tag))
(test "gt-at-0"        '(gt . 0) (string-compare "b" "a" lt-tag eq-tag gt-tag))

;; with optional ranges -- compare slices
(test "range-eq"       '(eq . 4) (string-compare "xabc" "abcy" lt-tag eq-tag gt-tag 1 4 0 3))
;; "abc" vs "abcd": s1 fully matches as prefix of s2, so proc< is called with end1=4.
(test "range-lt"       '(lt . 4) (string-compare "xabc" "abcd" lt-tag eq-tag gt-tag 1 4 0 4))
(test-end "string-compare")

(test-begin "string-compare-ci")
(test "ci-eq-mixed"    '(eq . 3) (string-compare-ci "abc" "ABC" lt-tag eq-tag gt-tag))
(test "ci-lt-mixed"    '(lt . 2) (string-compare-ci "abc" "ABD" lt-tag eq-tag gt-tag))
(test "ci-gt-mixed"    '(gt . 2) (string-compare-ci "abD" "abc" lt-tag eq-tag gt-tag))
(test-end "string-compare-ci")

(test-end "srfi-13 phase 5")
(test-exit)
