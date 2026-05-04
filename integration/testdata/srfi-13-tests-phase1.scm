;; SRFI-13 Phase-1 integration tests
;; Covers: string-prefix?, string-suffix?, string-contains, string-contains-ci,
;; string-join, string-split (the wile-goast top-five + extras).

(import (scheme base) (scheme write) (chibi test) (srfi 13) (wile strings))

(test-begin "srfi-13 phase 1")

;; ============================================================
;; string-prefix?
;; ============================================================

(test-begin "string-prefix?")

(test "positive"     #t (string-prefix? "hello" "hello world"))
(test "negative"     #f (string-prefix? "world" "hello"))
(test "exact"        #t (string-prefix? "hello" "hello"))
(test "longer-s1"    #f (string-prefix? "helloworld" "hello"))
(test "empty-s1"     #t (string-prefix? "" "anything"))
(test "both-empty"   #t (string-prefix? "" ""))
(test "with-start1"  #t (string-prefix? "xfoo" "foobar" 1))
(test "with-end1"    #t (string-prefix? "fooz" "foobar" 0 3))
(test "with-start2"  #t (string-prefix? "foo" "xfoobar" 0 3 1))
(test "full-args"    #t (string-prefix? "xfoox" "ymfooz" 1 4 2 5))
(test "full-args-no" #f (string-prefix? "xfoox" "ymfobz" 1 4 2 5))

(test-end "string-prefix?")

;; ============================================================
;; string-suffix?
;; ============================================================

(test-begin "string-suffix?")

(test "positive"     #t (string-suffix? "world" "hello world"))
(test "negative"     #f (string-suffix? "hello" "world"))
(test "exact"        #t (string-suffix? "hello" "hello"))
(test "longer-s1"    #f (string-suffix? "helloworld" "world"))
(test "empty-s1"     #t (string-suffix? "" "anything"))
(test "both-empty"   #t (string-suffix? "" ""))
(test "with-start1"  #t (string-suffix? "xbar" "foobar" 1))
(test "with-end1"    #t (string-suffix? "barz" "foobar" 0 3))

(test-end "string-suffix?")

;; ============================================================
;; string-contains
;; ============================================================

(test-begin "string-contains")

(test "found-mid"      6  (string-contains "hello world" "world"))
(test "found-start"    0  (string-contains "hello world" "hello"))
(test "missing"        #f (string-contains "hello" "xyz"))
(test "empty-needle"   0  (string-contains "hello" ""))
(test "empty-haystack" #f (string-contains "" "x"))
(test "both-empty"     0  (string-contains "" ""))
(test "needle-equals"  0  (string-contains "abc" "abc"))
(test "with-start1"    3  (string-contains "abcabcabc" "abc" 1))
(test "with-start1-end1" #f (string-contains "abcabcabc" "abc" 1 5))
(test "with-start2"    6  (string-contains "hello world" "xworld" 0 11 1))
(test "with-all"       6  (string-contains "hello world" "xworldz" 0 11 1 6))

(test-end "string-contains")

;; ============================================================
;; string-contains-ci
;; ============================================================

(test-begin "string-contains-ci")

(test "upper-needle" 6  (string-contains-ci "Hello World" "WORLD"))
(test "lower-needle" 6  (string-contains-ci "Hello World" "world"))
(test "mixed"        0  (string-contains-ci "Hello" "hElLo"))
(test "missing"      #f (string-contains-ci "Hello" "xyz"))
(test "empty-needle" 0  (string-contains-ci "Hello" ""))

(test-end "string-contains-ci")

;; ============================================================
;; string-join
;; ============================================================

(test-begin "string-join")

(test "default-delim"        "abc"     (string-join '("a" "b" "c")))
(test "comma"                "a,b,c"   (string-join '("a" "b" "c") ","))
(test "multi-char"           "a, b, c" (string-join '("a" "b" "c") ", "))
(test "empty-list"           ""        (string-join '() ","))
(test "single"               "a"       (string-join '("a") ","))
(test "infix-explicit"       "a,b"     (string-join '("a" "b") "," 'infix))
(test "prefix"               ",a,b"    (string-join '("a" "b") "," 'prefix))
(test "suffix"               "a,b,"    (string-join '("a" "b") "," 'suffix))
(test "prefix-empty"         ""        (string-join '() "," 'prefix))
(test "suffix-empty"         ""        (string-join '() "," 'suffix))
(test-error                            (string-join '() "," 'strict-infix))
(test-error                            (string-join '("a") "," 'invalid))
(test "strict-infix-non-empty" "a,b"   (string-join '("a" "b") "," 'strict-infix))

(test-end "string-join")

;; ============================================================
;; string-split  -- (wile strings) extra
;; ============================================================

(test-begin "string-split")

(test "simple"            '("a" "b" "c") (string-split "a,b,c" #\,))
(test "no-delim"          '("hello")     (string-split "hello" #\,))
(test "empty"             '("")          (string-split "" #\,))
(test "leading"           '("" "a" "b")  (string-split ",a,b" #\,))
(test "trailing"          '("a" "b" "")  (string-split "a,b," #\,))
(test "double"            '("a" "" "b")  (string-split "a,,b" #\,))
(test "all-delims"        '("" "" "" "") (string-split ",,," #\,))
(test "single-delim"      '("" "")       (string-split "," #\,))

(test-end "string-split")

(test-end "srfi-13 phase 1")
(test-exit)
