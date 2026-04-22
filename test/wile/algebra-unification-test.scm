;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification)
        (wile algebra rewrite)
        (wile algebra symbolic)
        (wile algebra matrix))

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

(test-group "ac-match: AC ground equality"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    (test 1 (length (ac-match '(+ a b) '(+ b a) theory proto)))
    (test 1 (length (ac-match '(+ a b c) '(+ c a b) theory proto)))
    (test 0 (length (ac-match '(+ a b) '(+ a c) theory proto)))))

(test-group "ac-match: variable in AC op — enumerates assignments"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(+ ?x a))))
    ;; ?x can bind to b (from (+ b a)) — 1 match, ?x↦b
    (test 1 (length (ac-match pat '(+ a b) theory proto)))))

(test-group "ac-match: free var binds to (op …) submultiset"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(+ ?x a))))
    ;; (+ a b c): ?x↦b, remaining a; OR ?x↦c, remaining a; OR ?x↦(+ b c)
    ;; Plus any further subset combinations — CSU may have several entries.
    (let ((results (ac-match pat '(+ a b c) theory proto)))
      (test #t (> (length results) 0)))))

(test-group "ac-match: mismatches and edge cases"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    ;; Operator mismatch
    (test 0 (length (ac-match '(+ a b) '(* a b) theory proto)))
    ;; Non-AC operator with mismatched arity
    (let ((theory-nac (make-theory '() '())))
      (test 0 (length (ac-match '(f a b) '(f a b c) theory-nac proto))))
    ;; Nested AC: (+ (* ?x 2) ?y) matches (+ ?y (* 2 a))?
    ;; (Requires * to also be AC; ?y can bind to any element.)
    (let* ((theory-both (make-ac-theory '(+ *)))
           (pat (parse-pattern '(+ (* ?x 2) ?y))))
      (test #t (> (length (ac-match pat '(+ (* 2 a) b) theory-both proto)) 0)))))

(test-group "ac-match: argument errors"
  (let ((proto (sexp-term-protocol default-compare)))
    (test-error (ac-match '(+ a b) '(+ a b) 'not-a-theory proto))
    (test-error (ac-match '(+ a b) '(+ a b) (make-theory '() '()) 'not-a-proto))))

(test-group "build-compat-matrix: smoke test"
  (let* ((proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x))
         (M (build-compat-matrix (list vx 'a) '(a b) proto)))
    (test #t (matrix? M))
    (test 2 (matrix-rows M))
    (test 2 (matrix-cols M))
    ;; pattern-var row: all #t
    (test #t (matrix-ref M 0 0))
    (test #t (matrix-ref M 0 1))
    ;; ground 'a row: #t only at column 0 (subject 'a)
    (test #t (matrix-ref M 1 0))
    (test #f (matrix-ref M 1 1))))

(test-group "can-position-match?: structural compatibility"
  (let* ((proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x)))
    ;; pattern-var compatible with anything
    (test #t (can-position-match? vx 'a proto))
    (test #t (can-position-match? vx '(+ 1 2) proto))
    ;; ground atom: only compatible with equal atom
    (test #t (can-position-match? 'a 'a proto))
    (test #f (can-position-match? 'a 'b proto))
    ;; compound: operator must match; ground-atom subject is not compatible
    (test #t (can-position-match? '(+ 1 2) '(+ 3 4) proto))
    (test #f (can-position-match? '(+ 1 2) '(* 3 4) proto))
    (test #f (can-position-match? '(+ 1 2) 'a proto))))

;;; Benchmark — recorded (not asserted). Compares pathological 8-element
;;; ac-match wall-clock time across Phase-4 iterations. Output lines begin
;;; with "[BENCH 4.x]" for easy grepping.
(test-group "ac-match: pathological 8-element bench"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    ;; Compatible case: 4 vars + 4 grounds, subject contains all grounds.
    (let* ((pat (parse-pattern '(+ ?v ?w ?x ?y a b c d)))
           (subj '(+ a b c d e f g h))
           (start (current-jiffy))
           (results (ac-match pat subj theory proto))
           (end (current-jiffy)))
      (test #t (> (length results) 0))
      (display "[BENCH 4.x] ac-match 8-elem compatible: ")
      (display (exact->inexact (/ (- end start) (jiffies-per-second))))
      (display "s, ") (display (length results)) (display " matches")
      (newline))
    ;; Infeasible case: 4 grounds not in subject — all-zero rows in compat
    ;; matrix, permanent is #f, prune should reject early.
    (let* ((pat (parse-pattern '(+ ?v ?w ?x ?y p q r s)))
           (subj '(+ a b c d e f g h))
           (start (current-jiffy))
           (results (ac-match pat subj theory proto))
           (end (current-jiffy)))
      (test 0 (length results))
      (display "[BENCH 4.x] ac-match 8-elem infeasible: ")
      (display (exact->inexact (/ (- end start) (jiffies-per-second))))
      (display "s, ") (display (length results)) (display " matches")
      (newline))))

(test-end "unification")
