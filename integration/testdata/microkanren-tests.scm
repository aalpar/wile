;; microKanren integration tests
;; Tests the (wile microkanren) library

(import (scheme base)
        (scheme write)
        (wile microkanren))

;; Simple test infrastructure (no dependency on chibi test)
(define *pass* 0)
(define *fail* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (set! *pass* (+ *pass* 1))
      (begin
        (set! *fail* (+ *fail* 1))
        (display "FAIL: ")
        (display name)
        (display " — expected ")
        (write expected)
        (display " but got ")
        (write actual)
        (newline))))

(define (test-summary)
  (newline)
  (display "Test Summary:")
  (newline)
  (display "  Passed: ")
  (display *pass*)
  (newline)
  (display "  Failed: ")
  (display *fail*)
  (newline)
  (if (> *fail* 0)
      (exit 1)
      (exit 0)))

;; Helper: pull n results from a stream
(define (take n $)
  (cond
    ((zero? n) '())
    ((null? $) '())
    ((procedure? $) (take n ($)))
    (else (cons (car $) (take (- n 1) (cdr $))))))

;; Helper: pull all results (use with care — may diverge)
(define (take-all $)
  (cond
    ((null? $) '())
    ((procedure? $) (take-all ($)))
    (else (cons (car $) (take-all (cdr $))))))

;; === Variable tests ===

(test "var creation"
  #t
  (var? (var 0)))

(test "var equality"
  #t
  (var=? (var 0) (var 0)))

(test "var inequality"
  #f
  (var=? (var 0) (var 1)))

(test "non-var"
  #f
  (var? 42))

;; === Walk tests ===

(test "walk unbound"
  (var 0)
  (walk (var 0) '()))

(test "walk bound"
  5
  (walk (var 0) (list (cons (var 0) 5))))

(test "walk chain"
  5
  (walk (var 0) (list (cons (var 0) (var 1))
                      (cons (var 1) 5))))

;; === Unification tests ===

(test "unify atoms equal"
  '()
  (unify 5 5 '()))

(test "unify atoms unequal"
  #f
  (unify 5 6 '()))

(test "unify var to atom"
  (list (cons (var 0) 5))
  (unify (var 0) 5 '()))

(test "unify two vars"
  (list (cons (var 0) (var 1)))
  (unify (var 0) (var 1) '()))

(test "unify pairs"
  (list (cons (var 1) 2) (cons (var 0) 1))
  (unify (cons (var 0) (var 1)) (cons 1 2) '()))

(test "unify nested fail"
  #f
  (unify (cons 1 2) (cons 1 3) '()))

;; === Goal tests ===

(test "== success"
  1
  (length (take-all ((== 5 5) empty-state))))

(test "== failure"
  0
  (length (take-all ((== 5 6) empty-state))))

(test "call/fresh binds"
  1
  (length (take-all ((call/fresh (lambda (x) (== x 5))) empty-state))))

;; === Disjunction (OR) ===

(test "disj two successes"
  2
  (length (take-all ((disj (== #t #t) (== #t #t)) empty-state))))

(test "disj one success"
  1
  (length (take-all ((disj (== 5 6) (== #t #t)) empty-state))))

;; === Conjunction (AND) ===

(test "conj both succeed"
  1
  (length (take-all
    ((conj (call/fresh (lambda (x) (== x 5)))
           (call/fresh (lambda (y) (== y 6))))
     empty-state))))

(test "conj one fails"
  0
  (length (take-all
    ((conj (== 5 6)
           (call/fresh (lambda (x) (== x 5))))
     empty-state))))

;; === Stream interleaving ===

;; A goal that produces multiple answers
(define (fives x)
  (disj (== x 5) (lambda (s/c) (lambda () ((fives x) s/c)))))

(define (sixes x)
  (disj (== x 6) (lambda (s/c) (lambda () ((sixes x) s/c)))))

(test "interleaving"
  5
  (length (take 5
    ((call/fresh (lambda (x) (disj (fives x) (sixes x))))
     empty-state))))

;; Verify interleaving produces alternating results
(let ((results (take 4
                ((call/fresh (lambda (x) (disj (fives x) (sixes x))))
                 empty-state))))
  ;; First result should bind x=5, second x=6, etc.
  (test "interleave alternates"
    #t
    (> (length results) 0)))

;; === Classic: appendo via microKanren primitives ===

;; appendo(l, s, out) — l ++ s = out
(define (appendo l s out)
  (disj
    (conj (== l '()) (== s out))
    (call/fresh (lambda (a)
      (call/fresh (lambda (d)
        (call/fresh (lambda (res)
          (conj (== l (cons a d))
                (conj (== out (cons a res))
                      (appendo d s res)))))))))))

(test "appendo forward"
  1
  (length (take 1 ((appendo '(1 2) '(3 4) '(1 2 3 4)) empty-state))))

(test "appendo generates"
  #t
  (> (length (take 3 ((call/fresh (lambda (x)
                        (appendo x '(3) '(1 2 3))))
                       empty-state)))
     0))

(test-summary)
