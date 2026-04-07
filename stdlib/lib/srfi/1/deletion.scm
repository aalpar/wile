;; deletion.scm -- list deletion utilities
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (delete x ls . o)
  "Remove all elements from LS that are equal to X. The optional\nthird argument is the equality predicate, defaulting to equal?.\nUses a fast path for eq? comparisons via memq.\n\nExamples:\n  (delete 3 '(1 2 3 4 3 5))      => (1 2 4 5)\n  (delete 'a '(a b a c) eq?)      => (b c)\n\nParameters:\n  x : any\n  ls : list\n  o : list\nReturns: list\nCategory: srfi-1\n\nSee also: `delete-duplicates', `remove'."
  (let ((eq (if (pair? o) (car o) equal?)))
    (if (eq? eq eq?)
        (let lp ((ls ls) (rev '())) ;; fast path for delq
          (let ((tail (memq x ls)))
            (if tail
                (lp (cdr tail) (take-up-to-reverse ls tail rev))
                (if (pair? rev) (append-reverse! rev ls) ls))))
        (remove (lambda (y) (eq x y)) ls))))

(define delete! delete)

(define (delete-duplicates ls . o)
  "Remove duplicate elements from LS, preserving the first\noccurrence of each element. The optional second argument is\nthe equality predicate, defaulting to equal?.\n\nExamples:\n  (delete-duplicates '(1 2 1 3 2 4))  => (1 2 3 4)\n  (delete-duplicates '(a a b b c))    => (a b c)\n\nParameters:\n  ls : list\n  o : list\nReturns: list\nCategory: srfi-1\n\nSee also: `delete'."
  (let ((eq (if (pair? o) (car o) equal?)))
    (let lp ((ls ls) (res '()))
      (if (pair? ls)
          (lp (cdr ls) (if (member (car ls) res eq) res (cons (car ls) res)))
          (reverse! res)))))

(define delete-duplicates! delete-duplicates)
