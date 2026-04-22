;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification)
        (wile algebra rewrite)
        (wile algebra symbolic))

(test-begin "unification")

(define (default-compare a b)
  (cond
    ((and (number? a) (number? b))
     (cond ((< a b) -1) ((> a b) 1) (else 0)))
    ((and (symbol? a) (symbol? b))
     (let ((sa (symbol->string a)) (sb (symbol->string b)))
       (cond ((string<? sa sb) -1) ((string>? sa sb) 1) (else 0))))
    ((equal? a b) 0)
    (else 1)))

(define (sym-append . syms)
  (string->symbol
    (apply string-append (map symbol->string syms))))

(define (make-ac-theory ops)
  (let ((axioms
         (apply append
                (map (lambda (op)
                       (list (make-named-axiom (sym-append 'ass- op)
                               #f (make-associativity-axiom op))
                             (make-named-axiom (sym-append 'com- op)
                               #f (make-commutativity-axiom op))))
                     ops))))
    (make-theory axioms ops)))

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

(test-group "diophantine-basis: x = y trivial"
  ;; System: 1·x = 1·y (m=n=1, a=[1], b=[1])
  ;; Minimal solution: x=1, y=1. That's it.
  (let ((basis (diophantine-basis '(1) '(1))))
    (test 1 (length basis))
    (test '((1) . (1)) (car basis))))

(test-group "diophantine-basis: Stickel canonical x+y = z"
  ;; 1·x₁ + 1·x₂ = 1·y₁  →  basis: ((1 0).(1)), ((0 1).(1))
  (let ((basis (diophantine-basis '(1 1) '(1))))
    (test 2 (length basis))
    (test-assert (member '((1 0) . (1)) basis))
    (test-assert (member '((0 1) . (1)) basis))))

(test-group "diophantine-basis: asymmetric 2x = 3y"
  ;; 2·x = 3·y → minimal (x=3, y=2)
  (let ((basis (diophantine-basis '(2) '(3))))
    (test 1 (length basis))
    (test '((3) . (2)) (car basis))))

(test-group "diophantine-basis: errors on bad input"
  (test-error (diophantine-basis '(-1) '(1)))     ; negative
  (test-error (diophantine-basis '(1) '(2.5)))    ; non-integer
  (test-error (diophantine-basis '() '(1)))       ; empty a
  (test-error (diophantine-basis '(1) '())))      ; empty b

(test-group "ac-match: non-AC positional"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare)))
    (test 1 (length (ac-match '(f a b) '(f a b) theory proto)))
    (test 0 (length (ac-match '(f a b) '(f b a) theory proto)))))

(test-group "ac-match: single variable"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(f ?x ?y))))
    (let ((results (ac-match pat '(f a b) theory proto)))
      (test 1 (length results))
      (let ((bs (substitution-bindings (car results))))
        (test 2 (length bs))))))

(test-group "ac-match: nonlinear ?x ?x"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(f ?x ?x))))
    (test 1 (length (ac-match pat '(f a a) theory proto)))   ; match
    (test 0 (length (ac-match pat '(f a b) theory proto))))) ; fail

(test-group "flatten-ac: associativity collapse"
  (let ((proto (sexp-term-protocol default-compare)))
    (test '(a b c) (flatten-ac '(+ a (+ b c)) '+ proto))
    (test '(a b c d) (flatten-ac '(+ (+ a b) (+ c d)) '+ proto))
    ;; Non-AC op nested: don't flatten
    (test '(a (g b c)) (flatten-ac '(+ a (g b c)) '+ proto))
    ;; Leaf
    (test '(a) (flatten-ac 'a '+ proto))))

(test-end "unification")
