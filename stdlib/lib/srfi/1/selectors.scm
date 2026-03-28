;; selectors.scm -- extended list selectors
;; Copyright (c) 2009 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define first car)
(define second cadr)
(define (third ls)
  "Return the third element of LS."
  (car (cdr (cdr ls))))
(define (fourth ls)
  "Return the fourth element of LS."
  (car (cdr (cdr (cdr ls)))))
(define (fifth ls)
  "Return the fifth element of LS."
  (car (cdr (cdr (cdr (cdr ls))))))
(define (sixth ls)
  "Return the sixth element of LS."
  (car (cdr (cdr (cdr (cdr (cdr ls)))))))
(define (seventh ls)
  "Return the seventh element of LS."
  (car (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))
(define (eighth ls)
  "Return the eighth element of LS."
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls)))))))))
(define (ninth ls)
  "Return the ninth element of LS."
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls))))))))))
(define (tenth ls)
  "Return the tenth element of LS."
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr ls)))))))))))

(define (car+cdr x)
  "Return two values: the car and cdr of pair X."
  (values (car x) (cdr x)))

(define (take ls i)
  "Return a freshly allocated list of the first I elements of LS.\nIt is an error if LS has fewer than I elements."
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (reverse! res)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (take! ls i)
  "Linear-update variant of take. May mutate LS to produce\nthe first I elements. The result may or may not share\nstructure with LS."
  (if (<= i 0)
      '()
      (let ((tail (list-tail ls (- i 1))))
        (set-cdr! tail '())
        ls)))

(define (drop ls i)
  "Return the tail of LS after skipping the first I elements.\nShares structure with the tail of LS."
  (if (<= i 0) ls (drop (cdr ls) (- i 1))))

(define (take-right ls i)
  "Return the last I elements of LS. Shares structure with\nthe tail of LS."
  (drop ls (- (length+ ls) i)))

(define (drop-right ls i)
  "Return a freshly allocated list containing all but the last\nI elements of LS."
  (take ls (- (length+ ls) i)))

(define (drop-right! ls i)
  "Linear-update variant of drop-right. May mutate LS to\nremove the last I elements."
  (take! ls (- (length+ ls) i)))

(define (split-at ls i)
  "Split LS at index I, returning two values: a freshly allocated\nlist of the first I elements and the remaining tail. Equivalent\nto (values (take ls i) (drop ls i)) but traverses only once."
  (let lp ((i i) (ls ls) (res '()))
    (if (<= i 0)
        (values (reverse! res) ls)
        (lp (- i 1) (cdr ls) (cons (car ls) res)))))

(define (split-at! ls i)
  "Linear-update variant of split-at. May mutate LS to split\nat index I. Returns two values: the prefix and the suffix."
  (if (<= i 0)
      (values '() ls)
      (let* ((tail (list-tail ls (- i 1)))
             (right (cdr tail)))
        (set-cdr! tail '())
        (values ls right))))

(define (last ls)
  "Return the last element of non-empty proper list LS."
  (if (null? (cdr ls)) (car ls) (last (cdr ls))))
(define (last-pair ls)
  "Return the last pair of non-empty proper list LS."
  (if (null? (cdr ls)) ls (last-pair (cdr ls))))

