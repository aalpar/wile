;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification))

(test-begin "unification")

(test-group "pattern-var construction and identity"
  (let ((vx (make-pattern-var 'x)))
    (test #t (pattern-var? vx))
    (test 'x (pattern-var-name vx))
    (test #f (pattern-var? 'x))               ; symbol, not pattern-var
    (test #f (pattern-var? '(+ 1 2)))))       ; pair, not pattern-var

(test-group "parse-pattern: ?-convention → <pattern-var> records"
  ;; Leaf var
  (let ((p (parse-pattern '?x)))
    (test #t (pattern-var? p))
    (test 'x (pattern-var-name p)))
  ;; Plain symbol stays a symbol
  (test 'foo (parse-pattern 'foo))
  ;; Nested: vars become records, operators stay symbols
  (let ((p (parse-pattern '(+ ?x ?y))))
    (test '+ (car p))
    (test #t (pattern-var? (cadr p)))
    (test #t (pattern-var? (caddr p))))
  ;; Repeated ?x interns to one record (eq? identity)
  (let ((p (parse-pattern '(+ ?x ?x))))
    (test #t (eq? (cadr p) (caddr p)))))

(test-end "unification")
