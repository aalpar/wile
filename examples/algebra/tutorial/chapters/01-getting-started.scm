;; ================================================================
;; Chapter 01 -- Getting Started: Monoids
;;
;; What you will learn:
;;   - What a monoid is (two pieces: an associative binary op + identity).
;;   - How to construct monoids from procedures and verify their laws hold.
;;   - How monoids apply to non-numeric data (strings, lists, booleans).
;;   - How `validate-monoid` catches structures that look like monoids but
;;     actually violate the laws.
;;
;; Prerequisites: none -- this is the tutorial's entry point.
;; Sub-libraries used: (wile algebra monoid).
;; Runtime: a second or two.
;; ================================================================

(import (scheme base) (scheme write) (wile algebra monoid))
(include "../lib/check.scm")

;; ----------------------------------------------------------------
;; Part 1: What is a monoid?
;;
;; A monoid is two pieces of data tied together:
;;   1. A binary operation `op` taking two values and returning a value.
;;   2. An identity element `e` such that (op e x) = x = (op x e) for all x.
;;
;; Plus one law: `op` must be associative --
;;   (op a (op b c)) must equal (op (op a b) c).
;;
;; That is it. No type hierarchy, no inheritance. The monoid `<M>` is an
;; R7RS record with two fields: the operation and the identity. Everything
;; else in the library (`monoid-fold`, `monoid-power`, `validate-monoid`)
;; is derived from those two fields.
;; ----------------------------------------------------------------

(define int+ (make-monoid + 0))           ; integer addition monoid
(define int* (make-monoid * 1))           ; integer multiplication monoid

(check-true (monoid? int+)                "int+ is a monoid")
(check-true (monoid? int*)                "int* is a monoid")
(check=     (monoid-identity int+) 0      "int+ identity is 0")
(check=     (monoid-identity int*) 1      "int* identity is 1")
(check=     (monoid-op int+ 3 4)   7      "int+ combines to sum")
(check=     (monoid-op int* 6 7)   42     "int* combines to product")

;; ----------------------------------------------------------------
;; Part 2: Folding a list under a monoid.
;;
;; Once you have a monoid, folding a list is automatic. `monoid-fold`
;; starts from the identity and combines left to right. The empty list
;; returns the identity by construction, which is why the identity is a
;; *required* part of the monoid definition rather than an afterthought.
;; ----------------------------------------------------------------

(check= (monoid-fold int+ '(1 2 3 4 5))  15   "sum of 1..5")
(check= (monoid-fold int* '(1 2 3 4 5))  120  "product of 1..5")
(check= (monoid-fold int+ '())            0   "empty sum returns identity")
(check= (monoid-fold int* '())            1   "empty product returns identity")

;; ----------------------------------------------------------------
;; Part 3: monoid-power -- repeated combination.
;;
;; (monoid-power M x n) combines x with itself n times under M.
;; For (+, 0) that is multiplication by n; for (*, 1) that is exponentiation.
;; Same operation, different monoid, different meaning.
;; ----------------------------------------------------------------

(check= (monoid-power int+ 5 3)   15   "5 added 3 times")
(check= (monoid-power int+ 7 0)    0   "power-0 returns identity")
(check= (monoid-power int* 2 10) 1024  "2 to the 10th")
(check= (monoid-power int* 2 0)    1   "multiplicative power-0 is 1")

;; ----------------------------------------------------------------
;; Part 4: Monoids on non-numeric carriers.
;;
;; Numbers are the most obvious monoids, but the structure does not care
;; what the elements look like. Any type with an associative op and an
;; identity is a monoid.
;; ----------------------------------------------------------------

;; String concatenation. Identity is the empty string.
(define str-cat (make-monoid string-append ""))
(check= (monoid-op str-cat "hello" " world") "hello world"  "string concat")
(check= (monoid-fold str-cat '("a" "b" "c" "d"))  "abcd"    "fold strings")
(check= (monoid-fold str-cat '())                 ""        "empty string fold")
(check= (monoid-power str-cat "ab" 3)             "ababab"  "repeat a string")

;; List append. Identity is the empty list.
(define list-cat (make-monoid append '()))
(check= (monoid-op list-cat '(1 2) '(3 4))     '(1 2 3 4)  "list append")
(check= (monoid-fold list-cat '((1) (2 3) (4))) '(1 2 3 4) "fold lists")
(check= (monoid-fold list-cat '())              '()        "empty list fold")

;; Boolean conjunction. Identity is #t.
(define bool-and (make-monoid (lambda (a b) (and a b)) #t))
(check-true  (monoid-fold bool-and '(#t #t #t))  "all true")
(check-false (monoid-fold bool-and '(#t #f #t))  "one false => false")
(check-true  (monoid-fold bool-and '())          "empty conjunction is true")

;; Boolean disjunction. Identity is #f.
(define bool-or (make-monoid (lambda (a b) (or a b)) #f))
(check-true  (monoid-fold bool-or '(#f #t #f))   "one true => true")
(check-false (monoid-fold bool-or '(#f #f #f))   "all false")
(check-false (monoid-fold bool-or '())           "empty disjunction is false")

;; ----------------------------------------------------------------
;; Part 5: Validating monoid laws.
;;
;; `validate-monoid` spot-checks left-identity, right-identity, and
;; associativity against a sample set. It returns #t if every law holds
;; on every sample, or a list of violation descriptions if any fail.
;;
;; This is a property-based sanity check, not a proof. A monoid that
;; passes validation on small integers is not proven to be a monoid on
;; all integers -- but a monoid that *fails* validation is definitely
;; not a monoid.
;; ----------------------------------------------------------------

(check= (validate-monoid int+ '(-2 -1 0 1 2))  #t  "int+ passes validation")
(check= (validate-monoid int* '(-2 -1 0 1 2))  #t  "int* passes validation")
(check= (validate-monoid str-cat '("" "a" "bc" "xyz"))  #t
        "string monoid passes validation")

;; ----------------------------------------------------------------
;; Part 6: Spotting a fake monoid.
;;
;; Subtraction with 0 as "identity" looks like a monoid at a glance but
;; fails two laws:
;;   - Left identity:  (- 0 a) = -a, not a.
;;   - Associativity:  (- (- 5 3) 2) = 0, but (- 5 (- 3 2)) = 4.
;;
;; `validate-monoid` catches both. This is why we spot-check rather than
;; trust a structure just because the constructor accepted it -- the
;; constructor only enforces that `op` is callable, not that it obeys
;; the laws.
;; ----------------------------------------------------------------

(define fake-sub (make-monoid - 0))
(define sub-violations (validate-monoid fake-sub '(1 2 3)))

(check-false (eq? sub-violations #t)        "subtraction fails validation")
;; "not #t" alone could be an empty list, which would falsely mean "no
;; violations found". Require at least one violation record.
(check-true  (pair? sub-violations)         "at least one violation recorded")

;; Every violation entry starts with a tag symbol. Validation finds at
;; least left-identity failures (and likely associativity failures too).
(define tags (map car sub-violations))
(check-true (memq 'left-identity tags)  "left-identity violation is reported")

;; ----------------------------------------------------------------
;; Part 7: with-monoid destructuring.
;;
;; Writing (monoid-op M a b) everywhere is noisy. `with-monoid` binds the
;; operation and identity to local names so the body reads like ordinary
;; arithmetic. The bindings are fresh each invocation, so you can
;; shadow without leaking.
;; ----------------------------------------------------------------

(define double-via-with-monoid
  (with-monoid int+ (op identity)
    (lambda (x) (op x x))))   ; captures `op` via closure
(check= (double-via-with-monoid 7) 14  "with-monoid bindings close over ops")

;; Use for a one-shot computation. The body can call `op` and `identity`
;; directly, no monoid-op threading.
(define int+sum-via-with
  (with-monoid int+ (op identity)
    (let loop ((acc identity) (xs '(10 20 30 40)))
      (if (null? xs) acc
          (loop (op acc (car xs)) (cdr xs))))))
(check= int+sum-via-with 100  "rewrote monoid-fold by hand with with-monoid")

;; ----------------------------------------------------------------
;; Part 8: Exercises -- uncomment, guess the answer, then run the file.
;;
;; Each commented check is a small puzzle. Replace <?> with your guess
;; and uncomment the line. If you are right, the file still passes; if
;; you are wrong, you get a loud FAIL with the actual value.
;; ----------------------------------------------------------------

;; (check= (monoid-power int+ 7 5)                        <?>  "7 added 5 times")
;; (check= (monoid-fold str-cat (list "wile" " " "algebra")) <?>  "space-joined words")
;; (check= (monoid-power (make-monoid append '()) '(1 2) 4) <?>  "list repeated 4 times")

(display "chapter 01 complete") (newline)
