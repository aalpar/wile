;; predicates.scm -- list prediates
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (proper-list? x)
  "Return #t if X is a proper list, i.e. a chain of pairs\nterminated by the empty list. Returns #f for dotted lists\nand non-pair values. Does not terminate on circular lists.\n\nExamples:\n  (proper-list? '(1 2 3))  => #t\n  (proper-list? '(1 . 2))  => #f"
  (cond ((null? x) #t)
        ((pair? x) (proper-list? (cdr x)))
        (else #f)))

(define (circular-list? x)
  "Return #t if X is a circular list, detected via the\ntortoise-and-hare algorithm. Returns #f for proper lists,\ndotted lists, and non-pair values.\n\nExamples:\n  (circular-list? (circular-list 1 2))  => #t\n  (circular-list? '(1 2 3))             => #f"
  (and (pair? x) (pair? (cdr x))
       (let race ((hare (cdr x)) (tortoise x))
         (or (eq? hare tortoise)
             (and (pair? hare) (pair? (cdr hare))
                  (race (cddr hare) (cdr tortoise)))))))

(define (dotted-list? x)
  "Return #t if X is a dotted list, i.e. a chain of pairs not\nterminated by the empty list. Also returns #t for non-pair,\nnon-null values. The complement of proper-list?.\n\nExamples:\n  (dotted-list? '(1 . 2))    => #t\n  (dotted-list? '(1 2 3))    => #f\n  (dotted-list? 42)           => #t"
  (not (proper-list? x)))

(define (not-pair? x)
  "Return #t if X is not a pair. Equivalent to (not (pair? x)).\nHandy as a predicate argument to higher-order functions.\n\nExamples:\n  (not-pair? 42)       => #t\n  (not-pair? '(1 2))   => #f"
  (not (pair? x)))

(define (null-list? x)
  "Return #t if X is the empty list. Unlike the SRFI-1 specification,\nthis implementation does not signal an error for non-list arguments.\n\nExamples:\n  (null-list? '())     => #t\n  (null-list? '(1 2))  => #f"
  (null? x))

(define (list= eq . lists)
  "Test if all LISTS have equal length and elements pairwise\nsatisfy the equality predicate EQ. With zero or one list,\nreturns #t. Compares adjacent pairs of lists left to right.\n\nExamples:\n  (list= = '(1 2 3) '(1 2 3))  => #t\n  (list= = '(1 2) '(1 2 3))    => #f"
  (let lp1 ((lists lists))
    (or (null? lists)
        (null? (cdr lists))
        (let lp2 ((ls1 (car lists)) (ls2 (cadr lists)))
          (if (null? ls1)
              (and (null? ls2)
                   (lp1 (cdr lists)))
              (and (pair? ls2)
                   (eq (car ls1) (car ls2))
                   (lp2 (cdr ls1) (cdr ls2))))))))

(define (length+ x)
  "Return the length of X if it is a proper list, or #f if X is\na circular list. Uses the tortoise-and-hare algorithm for\ncircle detection. Returns 0 for non-pair values.\n\nExamples:\n  (length+ '(a b c))              => 3\n  (length+ (circular-list 1 2))   => #f"
  (if (not (pair? x))
      0
      (let lp ((hare (cdr x)) (tortoise x) (res 1))
        (and (not (eq? hare tortoise))
             (if (pair? hare)
                 (if (not (pair? (cdr hare)))
                     (+ res 1)
                     (lp (cddr hare) (cdr tortoise) (+ res 2)))
                 res)))))
