;;; higher-order.scm - Higher-order function patterns
;;;
;;; Demonstrates: map, filter, fold, function composition
;;;
;;; Usage: ./dist/scheme --file examples/basics/higher-order.scm

;; map - apply function to each element
(display "Square each element: ")
(display (map (lambda (x) (* x x)) '(1 2 3 4 5)))
(newline)

;; filter - keep elements that satisfy predicate
(define (even? n)
  (= (modulo n 2) 0))

(display "Filter even numbers: ")
(display (filter even? '(1 2 3 4 5 6 7 8)))
(newline)

;; fold (reduce) - accumulate values
;; Note: R7RS provides fold-left and fold-right in (scheme list)
;; but we'll define our own for clarity

(define (fold-left proc init lst)
  (if (null? lst)
      init
      (fold-left proc
                 (proc init (car lst))
                 (cdr lst))))

(define (fold-right proc init lst)
  (if (null? lst)
      init
      (proc (car lst)
            (fold-right proc init (cdr lst)))))

(display "Sum using fold-left: ")
(display (fold-left + 0 '(1 2 3 4 5)))
(newline)

(display "Product using fold-left: ")
(display (fold-left * 1 '(1 2 3 4 5)))
(newline)

;; Function composition
(define (compose f g)
  (lambda (x) (f (g x))))

(define square (lambda (x) (* x x)))
(define add1 (lambda (x) (+ x 1)))

(define square-then-add1 (compose add1 square))
(define add1-then-square (compose square add1))

(display "Compose (add1 (square 5)): ")
(display (square-then-add1 5))
(newline)

(display "Compose (square (add1 5)): ")
(display (add1-then-square 5))
(newline)

;; Partial application (currying)
(define (make-adder n)
  (lambda (x) (+ x n)))

(define add10 (make-adder 10))

(display "Add 10 to 5: ")
(display (add10 5))
(newline)

;; Building complex operations from simple ones
(define (take n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

(define (drop n lst)
  (if (or (= n 0) (null? lst))
      lst
      (drop (- n 1) (cdr lst))))

;; Pipeline: take first 5, filter evens, square each
(display "Pipeline (take 5, filter even, square): ")
(display (map square
              (filter even?
                      (take 5 '(1 2 3 4 5 6 7 8 9 10)))))
(newline)

;; Reduce example: find maximum
(define (maximum lst)
  (fold-left max (car lst) (cdr lst)))

(display "Maximum of (3 7 2 9 1): ")
(display (maximum '(3 7 2 9 1)))
(newline)

;; Count elements satisfying predicate
(define (count predicate lst)
  (fold-left (lambda (acc x)
               (if (predicate x)
                   (+ acc 1)
                   acc))
             0
             lst))

(display "Count even numbers in (1 2 3 4 5 6): ")
(display (count even? '(1 2 3 4 5 6)))
(newline)
