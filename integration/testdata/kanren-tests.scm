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

(test-summary)
