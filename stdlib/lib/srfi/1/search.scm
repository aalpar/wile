;; search.scm -- list searching and splitting
;; Copyright (c) 2009-2011 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (take-while pred ls)
  "Return the longest initial prefix of LS whose elements all\nsatisfy PRED. The result is a freshly allocated list."
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (reverse! res))))

(define take-while! take-while)

(define (drop-while pred ls)
  "Skip the longest initial prefix of LS whose elements satisfy\nPRED and return the remaining tail. Shares structure with LS."
  (or (find-tail (lambda (x) (not (pred x))) ls) '()))

(define (span pred ls)
  "Split LS at the first element that does not satisfy PRED.\nReturns two values: the longest initial prefix of elements\nsatisfying PRED (freshly allocated), and the remaining tail\n(sharing structure with LS)."
  (let lp ((ls ls) (res '()))
    (if (and (pair? ls) (pred (car ls)))
        (lp (cdr ls) (cons (car ls) res))
        (values (reverse! res) ls))))

(define span! span)

(define (break pred ls)
  "Split LS at the first element satisfying PRED. Returns two\nvalues: the prefix of elements not satisfying PRED, and the\nremaining tail. The complement of span."
  (span (lambda (x) (not (pred x))) ls))

(define break! break)

(define (list-index pred ls . lists)
  "Return the index of the first element of LS satisfying PRED,\nor #f if no element matches. For multiple lists, PRED receives\ncorresponding elements and the search stops at the shortest list."
  (if (null? lists)
      (let lp ((ls ls) (n 0))
        (and (pair? ls) (if (pred (car ls)) n (lp (cdr ls) (+ n 1)))))
      (let lp ((lists (cons ls lists)) (n 0))
        (and (every pair? lists)
             (if (apply pred (map car lists)) n (lp (map cdr lists) (+ n 1)))
             ))))
