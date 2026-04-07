;; search.scm -- list searching and splitting
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (take-while pred ls)
  "Return the longest initial prefix of LS whose elements all\nsatisfy PRED. The result is a freshly allocated list.\n\nExamples:\n  (take-while even? '(2 4 1 3 5))  => (2 4)\n  (take-while even? '(1 2 3))      => ()\n\nParameters:\n  pred : procedure\n  ls : list\nReturns: list\nCategory: srfi-1\n\nSee also: `drop-while', `span'."
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse! res))))

(define take-while! take-while)

(define (drop-while pred ls)
  "Skip the longest initial prefix of LS whose elements satisfy\nPRED and return the remaining tail. Shares structure with LS.\n\nExamples:\n  (drop-while even? '(2 4 1 3 5))  => (1 3 5)\n  (drop-while even? '(1 2 3))      => (1 2 3)\n\nParameters:\n  pred : procedure\n  ls : list\nReturns: list\nCategory: srfi-1\n\nSee also: `take-while', `span'."
  (or (find-tail (lambda (x) (not (pred x))) ls) '()))

(define (span pred ls)
  "Split LS at the first element that does not satisfy PRED.\nReturns two values: the longest initial prefix of elements\nsatisfying PRED (freshly allocated), and the remaining tail\n(sharing structure with LS).\n\nExamples:\n  (span even? '(2 4 1 3 5))  => (2 4) (1 3 5)\n  (span even? '(1 2 3))      => () (1 2 3)\n\nParameters:\n  pred : procedure\n  ls : list\nReturns: list\nCategory: srfi-1\n\nSee also: `break', `take-while', `drop-while'."
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (values (reverse! res) ls))))

(define span! span)

(define (break pred ls)
  "Split LS at the first element satisfying PRED. Returns two\nvalues: the prefix of elements not satisfying PRED, and the\nremaining tail. The complement of span.\n\nExamples:\n  (break even? '(1 3 2 4 5))  => (1 3) (2 4 5)\n  (break even? '(2 4 6))      => () (2 4 6)\n\nParameters:\n  pred : procedure\n  ls : list\nReturns: list\nCategory: srfi-1\n\nSee also: `span', `take-while', `drop-while'."
  (span (lambda (x) (not (pred x))) ls))

(define break! break)

(define (list-index pred ls . lists)
  "Return the index of the first element of LS satisfying PRED,\nor #f if no element matches. For multiple lists, PRED receives\ncorresponding elements and the search stops at the shortest list.\n\nExamples:\n  (list-index even? '(1 3 4 5))  => 2\n  (list-index even? '(1 3 5))    => #f\n\nParameters:\n  pred : procedure\n  ls : list\nReturns: any\nCategory: srfi-1\n\nSee also: `find', `find-tail'."
  (if (null? lists)
      (let lp ((ls ls) (n 0))
        (and (pair? ls) (if (pred (car ls)) n (lp (cdr ls) (+ n 1)))))
      (let lp ((lists (cons ls lists)) (n 0))
        (and (every pair? lists)
             (if (apply pred (map car lists)) n (lp (map cdr lists) (+ n 1)))
             ))))
