;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification)
        (wile algebra rewrite)
        (wile algebra symbolic))

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

(test-group "substitution: construction and accessors"
  (test #t (substitution? empty-substitution))
  (test '() (substitution-bindings empty-substitution))
  (let* ((vx (make-pattern-var 'x))
         (s (make-substitution (list (cons vx 42)))))
    (test #t (substitution? s))
    (test '((x . 42))
          (map (lambda (b) (cons (pattern-var-name (car b)) (cdr b)))
               (substitution-bindings s)))))

(test-group "substitution-lookup"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s (make-substitution (list (cons vx 1) (cons vy 2)))))
    (test 1 (substitution-lookup s vx))
    (test 2 (substitution-lookup s vy))
    (test #f (substitution-lookup s (make-pattern-var 'z)))
    (test #f (substitution-lookup empty-substitution vx))))

(test-group "substitution-compose: non-conflicting and conflicting"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s1 (make-substitution (list (cons vx 1))))
         (s2 (make-substitution (list (cons vy 2))))
         (s3 (make-substitution (list (cons vx 99)))))
    ;; Non-conflicting compose: union of bindings
    (let ((merged (substitution-compose s1 s2)))
      (test 1 (substitution-lookup merged vx))
      (test 2 (substitution-lookup merged vy)))
    ;; Conflicting compose: x↦1 vs x↦99 → #f
    (test #f (substitution-compose s1 s3))
    ;; Empty cases
    (test s1 (substitution-compose s1 empty-substitution))
    (test s1 (substitution-compose empty-substitution s1))))

(test-group "substitution-apply: rewrites pattern with bindings"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s (make-substitution (list (cons vx 10) (cons vy 'hello))))
         (proto (sexp-term-protocol (lambda (a b)
                                      (cond
                                        ((and (number? a) (number? b))
                                         (cond ((< a b) -1) ((> a b) 1) (else 0)))
                                        ((and (symbol? a) (symbol? b))
                                         (let ((sa (symbol->string a))
                                               (sb (symbol->string b)))
                                           (cond ((string<? sa sb) -1)
                                                 ((string>? sa sb) 1)
                                                 (else 0))))
                                        (else 0)))))) ; dummy: OK for this test
    ;; Atom var
    (test 10 (substitution-apply s proto vx))
    ;; Compound with vars
    (test '(+ 10 hello) (substitution-apply s proto (list '+ vx vy)))
    ;; Unbound var stays unchanged
    (let ((vz (make-pattern-var 'z)))
      (test vz (substitution-apply s proto vz)))
    ;; No vars: identity
    (test '(+ 1 2) (substitution-apply s proto '(+ 1 2)))))

(test-end "unification")
