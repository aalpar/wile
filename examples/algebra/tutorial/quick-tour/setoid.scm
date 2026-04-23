;; quick-tour: (wile algebra setoid)
;;
;; A setoid is a set plus an explicit equivalence relation. Useful when
;; "equal" is domain-specific: case-insensitive strings, floats within
;; epsilon, lists as sets. Pick it up when you want to parameterize an
;; algorithm over "what counts as the same element."

(import (scheme base) (wile algebra setoid))
(include "../lib/check.scm")

;; -- Construction: the four built-in convenience constructors --------

(define S-eq      (eqv-setoid))
(define S-equal   (default-setoid))
(define S-num     (numeric-setoid))
(define S-str     (string-setoid))

(check-true (setoid? S-eq)     "eqv-setoid is a setoid")
(check-true (setoid? S-num)    "numeric-setoid is a setoid")

;; -- Equivalence queries ---------------------------------------------

(check-true  (setoid-equiv? S-equal '(1 2) '(1 2))     "equal? setoid: list equality")
(check-false (setoid-equiv? S-equal '(1 2) '(2 1))     "equal? setoid: order matters")
(check-true  (setoid-equiv? S-num 1 1.0)               "numeric setoid: 1 = 1.0")
(check-false (setoid-equiv? S-eq  1 1.0)               "eqv setoid: 1 not eqv to 1.0")

;; -- Membership and equivalence classes ------------------------------

(check-true  (setoid-member? S-num 1 '(1.0 2.0 3.0))    "numeric member: 1 is in {1.0,2.0,3.0}")

(check= (setoid-equivalence-class S-num 2 '(1 2 2.0 3))
        '(2 2.0)
        "class of 2 under numeric equivalence")

;; -- Validation ------------------------------------------------------

(check= (validate-setoid S-num '(1 2 3))  #t  "numeric setoid satisfies setoid laws")

;; -- Custom setoid: case-insensitive strings -------------------------

(define S-ci
  (make-setoid
    (lambda (a b)
      (string=? (string-downcase a) (string-downcase b)))))

(check-true (setoid-equiv? S-ci "Hello" "hello")   "case-insensitive match")
(check-true (setoid-equiv? S-ci "WORLD" "world")   "case-insensitive match (upper)")

(display "setoid tour complete") (newline)
