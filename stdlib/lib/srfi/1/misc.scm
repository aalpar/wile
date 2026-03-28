;; misc.scm -- miscellaneous list utilities
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (map-onto proc ls init)
  "Internal helper: map PROC over LS and prepend the results\nonto INIT. Preserves element order from LS."
  (let lp ((ls (reverse ls)) (res init))
    (if (null? ls) res (lp (cdr ls) (cons (proc (car ls)) res)))))

(define (append! . lists)
  "Linear-update variant of append. May destructively concatenate\nLISTS by mutating the cdr of each list's last pair."
  (concatenate! lists))

(define (concatenate lists)
  "Append all lists in LISTS together. Equivalent to\n(apply append lists) but avoids the argument-count limit\nof apply."
  (let lp ((ls (reverse lists)) (res '()))
    (if (null? ls) res (lp (cdr ls) (append (car ls) res)))))

(define (concatenate! lists)
  "Linear-update variant of concatenate. Destructively appends\nall lists in LISTS by linking their last pairs together."
  (if (null? lists)
      '()
      (let loop ((acc    '())
                 (prev   '())
                 (rem    lists))
          (cond
            ((null? rem) acc)
            ((null? acc) (let ((cur (car rem))) (loop cur cur (cdr rem))))
            ((null? (car rem)) (loop acc prev (cdr rem)))
            (else (let ((cur (car rem)))
                    (set-cdr! (last-pair prev) cur)
                    (loop acc cur (cdr rem))))))))

(define (append-reverse rev tail)
  "Append the reverse of list REV onto TAIL. Equivalent to\n(append (reverse rev) tail) but more efficient, avoiding\nthe intermediate reversed list."
  (if (null? rev) tail (append-reverse (cdr rev) (cons (car rev) tail))))

(define (append-reverse! rev tail)
  "Linear-update variant of append-reverse. May destructively\nreverse REV and link it to TAIL."
  (if (null? rev)
      tail
      (let ((head (reverse! rev)))
        (set-cdr! rev tail)
        head)))

(define (zip . lists)
  "Return a list of lists, where the i-th sublist contains the\ni-th element from each of LISTS. Stops at the shortest list.\nEquivalent to (map list list1 list2 ...)."
  (apply map list lists))

(define (unzip1 ls)
  "Extract the first element from each sublist of LS.\nThe inverse of zip for one list."
  (map first ls))
(define (unzip2 ls)
  "Extract the first two elements from each sublist of LS.\nReturns two values: a list of first elements and a list\nof second elements."
  (values (map first ls) (map second ls)))
(define (unzip3 ls)
  "Extract the first three elements from each sublist of LS.\nReturns three values."
  (values (map first ls) (map second ls) (map third ls)))
(define (unzip4 ls)
  "Extract the first four elements from each sublist of LS.\nReturns four values."
  (values (map first ls) (map second ls) (map third ls) (map fourth ls)))
(define (unzip5 ls)
  "Extract the first five elements from each sublist of LS.\nReturns five values."
  (values (map first ls) (map second ls) (map third ls) (map fourth ls)
          (map fifth ls)))

(define (count pred ls . lists)
  "Count the number of elements in LS that satisfy PRED.\nFor multiple lists, PRED receives corresponding elements\nand counting stops at the shortest list."
  (if (null? lists)
      (let lp ((ls ls) (res 0))
        (if (pair? ls) (lp (cdr ls) (if (pred (car ls)) (+ res 1) res)) res))
      (let lp ((lists (cons ls lists)) (res 0))
        (if (every pair? lists)
            (lp (map cdr lists) (if (apply pred (map car lists)) (+ res 1) res))
            res))))
