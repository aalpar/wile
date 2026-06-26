;; miniKanren integration tests
;; Tests the (wile kanren) macro layer

(import (scheme base)
        (scheme write)
        (wile kanren))

;; Test infrastructure
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

;; === fresh ===

(test "fresh single var"
  '(_.0)
  (run* (q) (fresh (x) (== q x))))

(test "fresh binds"
  '(5)
  (run* (q) (fresh (x) (== x 5) (== q x))))

(test "fresh multiple vars"
  '((5 6))
  (run* (q)
    (fresh (x y)
      (== x 5)
      (== y 6)
      (== q (list x y)))))

;; === conde ===

(test "conde two branches"
  '(tea cup)
  (run* (x)
    (conde
      ((== x 'tea))
      ((== x 'cup)))))

(test "conde with fresh"
  '(tea coffee)
  (run* (x)
    (conde
      ((== x 'tea))
      ((== x 'coffee)))))

;; === run with bound ===

(test "run bounded"
  2
  (length (run 2 (q) (conde ((== q 1)) ((== q 2)) ((== q 3))))))

(test "run 0"
  '()
  (run 0 (q) (== q 1)))

;; === Classic relations ===

;; appendo
(define (appendo l s out)
  (conde
    ((== l '()) (== s out))
    ((fresh (a d res)
       (== l (cons a d))
       (== out (cons a res))
       (appendo d s res)))))

;; membero
(define (membero x ls)
  (fresh (a d)
    (== ls (cons a d))
    (conde
      ((== a x))
      ((membero x d)))))

(test "appendo forward"
  '((1 2 3 4))
  (run* (q) (appendo '(1 2) '(3 4) q)))

(test "appendo backward"
  '((1 2))
  (run* (q) (appendo q '(3 4) '(1 2 3 4))))

(test "appendo generate splits"
  5
  (length (run* (q)
    (fresh (x y)
      (appendo x y '(1 2 3 4))
      (== q (list x y))))))

(test "membero found"
  '(_.0)
  (run 1 (q) (membero 2 '(1 2 3))))

(test "membero all"
  '(1 2 3)
  (run* (q) (membero q '(1 2 3))))

;; === Reification ===

(test "reify unbound"
  '(_.0)
  (run* (q) (fresh () (== q q))))

(test "reify pair with unbound"
  '((_.0 _.1))
  (run* (q)
    (fresh (x y)
      (== q (list x y)))))

;; === Diverging goals (fairness) ===

;; nevero: a goal that never succeeds
(define (nevero)
  (conde
    ((nevero))))

;; alwayso: a goal that always succeeds (infinitely)
(define (alwayso)
  (conde
    ((alwayso))
    ((== #t #t))))

(test "diverge with bound"
  1
  (length (run 1 (q)
    (conde
      ((== q 'yes))
      ((nevero))))))

;; === Multi-variable run / run* (tuples per solution) ===

(test "run* two vars yields a tuple"
  '((1 2))
  (run* (x y) (== x 1) (== y 2)))

(test "run* single var stays bare (unchanged)"
  '(5)
  (run* (x) (== x 5)))

(test "run* three vars yields a triple"
  '((1 2 3))
  (run* (a b c) (== a 1) (== b 2) (== c 3)))

(test "run* two vars, multiple solutions via conde"
  '((1 10) (2 20))
  (run* (x y)
    (conde
      ((== x 1) (== y 10))
      ((== x 2) (== y 20)))))

(test "run bounded two vars"
  '((1 10))
  (run 1 (x y)
    (conde
      ((== x 1) (== y 10))
      ((== x 2) (== y 20)))))

;; === Exported stream/reification helpers ===

(test "take-inf truncates"
  '(1 2)
  (take-inf 2 (list 1 2 3)))

(test "take-all-inf collects all"
  '(1 2 3)
  (take-all-inf (list 1 2 3)))

(test "reify-name produces _.N symbol"
  '_.3
  (reify-name 3))

(test "walk* resolves nested vars"
  '(a b)
  (walk* (list (var 0) (var 1))
         (list (cons (var 0) 'a) (cons (var 1) 'b))))

(test-summary)
