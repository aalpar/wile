;; constructors.scm -- list construction utilities
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (xcons a b)
  "Construct a pair with B as car and A as cdr.\nLike cons with arguments reversed. Useful as a combiner in\nfold where the accumulator position is swapped.\n\nExamples:\n  (xcons 1 2)              => (2 . 1)\n  (xcons '(a b) 'c)        => (c a b)"
  (cons b a))

(define (cons* x . args)
  "Construct a list from X and ARGS with the last argument as the tail.\nWith one argument returns it directly. With two, equivalent to cons.\nWith more, builds a chain: (cons* 1 2 3) produces (1 2 . 3).\n\nExamples:\n  (cons* 1 2 '(3 4))  => (1 2 3 4)\n  (cons* 1 2 3)        => (1 2 . 3)"
  (let lp ((rev '()) (x x) (ls args))
    (if (null? ls)
        (append-reverse rev x)
        (lp (cons x rev) (car ls) (cdr ls)))))

(define (list-tabulate n proc)
  "Return a list of N elements produced by applying PROC to indices 0 through N-1.\nEquivalent to (list (proc 0) (proc 1) ... (proc (- n 1))).\n\nExamples:\n  (list-tabulate 4 values)           => (0 1 2 3)\n  (list-tabulate 3 (lambda (i) (* i i)))  => (0 1 4)"
  (let lp ((n (- n 1)) (res '()))
    (if (< n 0) res (lp (- n 1) (cons (proc n) res)))))

(define (circular-list x . args)
  "Construct a circular list from X and ARGS.\nThe cdr of the last pair points back to the first, creating\nan infinite cycle. (circular-list 1 2 3) produces a list\nthat repeats 1 2 3 1 2 3 forever.\n\nExamples:\n  (take (circular-list 1 2 3) 7)  => (1 2 3 1 2 3 1)\n  (circular-list? (circular-list 'a))  => #t"
  (let ((res (cons x args)))
    (set-cdr! (last-pair res) res)
    res))

(define (iota count . o)
  "Return a list of COUNT numbers starting from START with step STEP.\nSTART defaults to 0, STEP defaults to 1. For example,\n(iota 5) produces (0 1 2 3 4) and (iota 3 1 2) produces (1 3 5).\n\nExamples:\n  (iota 5)      => (0 1 2 3 4)\n  (iota 3 1 2)  => (1 3 5)"
  (let ((start (if (pair? o) (car o) 0))
        (step (if (and (pair? o) (pair? (cdr o))) (cadr o) 1)))
    (let lp ((i count) (res '()))
      (if (<= i 0)
          res
          (lp (- i 1) (cons (+ start (* (- i 1) step)) res))))))
