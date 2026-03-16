;; Cross-library ER macro tests.
;; Macros are defined in (wile er-macro-test) and used here.
;; This exercises the critical cross-library paths:
;;   - rename resolves in the definition-site library
;;   - compare works when identifiers come from different libraries
;;   - anaphoric (un-renamed) symbols resolve at the use site

(import (wile er-macro-test))

;; Test 1: my-or — renamed symbols resolve correctly across libraries
(define r1 (my-or #f 42))
(if (= r1 42)
    (display "PASS: cross-lib my-or falsy\n")
    (begin (display "FAIL: cross-lib my-or falsy, got ")
           (display r1)
           (newline)
           (exit 1)))

(define r2 (my-or 7 42))
(if (= r2 7)
    (display "PASS: cross-lib my-or truthy\n")
    (begin (display "FAIL: cross-lib my-or truthy, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 2: my-or with user-defined 'tmp' — rename prevents capture
(define tmp 999)
(define r3 (my-or #f tmp))
(if (= r3 999)
    (display "PASS: cross-lib my-or no capture of tmp\n")
    (begin (display "FAIL: cross-lib my-or no capture of tmp, got ")
           (display r3)
           (newline)
           (exit 1)))

;; Test 3: aif — anaphoric 'it' visible at use site across libraries
(define r4 (aif (+ 1 2) (* it 10) 0))
(if (= r4 30)
    (display "PASS: cross-lib aif anaphoric\n")
    (begin (display "FAIL: cross-lib aif anaphoric, got ")
           (display r4)
           (newline)
           (exit 1)))

(define r5 (aif #f 'yes 'no))
(if (eq? r5 'no)
    (display "PASS: cross-lib aif else branch\n")
    (begin (display "FAIL: cross-lib aif else branch, got ")
           (display r5)
           (newline)
           (exit 1)))

;; Test 4: literal-check — compare works across library boundary
(define r6 (literal-check magic))
(if (eq? r6 'found-magic)
    (display "PASS: cross-lib compare found-magic\n")
    (begin (display "FAIL: cross-lib compare found-magic, got ")
           (display r6)
           (newline)
           (exit 1)))

(define r7 (literal-check other))
(if (eq? r7 'not-magic)
    (display "PASS: cross-lib compare not-magic\n")
    (begin (display "FAIL: cross-lib compare not-magic, got ")
           (display r7)
           (newline)
           (exit 1)))

;; Test 5: user shadows 'if' locally — renamed 'if' in library macro still works
(define if-val 'not-if)
(define r8 (my-or #f if-val))
(if (eq? r8 'not-if)
    (display "PASS: cross-lib shadowed if still works\n")
    (begin (display "FAIL: cross-lib shadowed if, got ")
           (display r8)
           (newline)
           (exit 1)))

(display "All cross-library ER macro tests passed\n")
