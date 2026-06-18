;; lset.scm -- list set library
;; Copyright (c) 2009-2012 Alex Shinn.  All rights reserved.
;; BSD-style license: http://synthcode.com/license.txt

(define (lset<= eq . sets)
  "Return #t if each set in SETS is a subset of the following one,\nusing EQ as the element equality predicate. With zero or one\nset, returns #t. Tests that every element of each set appears\nin the next set.\n\nExamples:\n  (lset<= eq? '(a) '(a b c))       => #t\n  (lset<= eq? '(a b) '(a))         => #f\n\nParameters:\n  eq : procedure\nReturns: boolean\nCategory: srfi-1"
  (if (null? sets)
      #t
      (let lp1 ((set1 (car sets)) (sets (cdr sets)))
        (if (null? sets)
            #t
            (let ((set2 (car sets)))
              (let lp2 ((ls set1))
                (if (pair? ls)
                    (and (member (car ls) set2 eq) (lp2 (cdr ls)))
                    (lp1 set2 (cdr sets)))))))))

(define (lset= eq . sets)
  "Return #t if all SETS contain the same elements, using EQ as\nthe equality predicate. Implemented as mutual subset testing\nin both directions.\n\nExamples:\n  (lset= eq? '(a b c) '(c b a))  => #t\n  (lset= eq? '(a b) '(a b c))    => #f\n\nParameters:\n  eq : procedure\nReturns: boolean\nCategory: srfi-1"
  (and (apply lset<= eq sets)
       (apply lset<= (lambda (b a) (eq a b)) (reverse sets))))

(define (lset-adjoin eq set . elts)
  "Add each element in ELTS to SET if not already present,\nusing EQ as the equality predicate. Returns the augmented set.\n\nExamples:\n  (lset-adjoin eq? '(a b) 'c 'd)    => (d c a b)\n  (lset-adjoin eq? '(a b) 'a 'c)    => (c a b)\n\nParameters:\n  eq : procedure\n  set : list\n  elts : list\nReturns: list\nCategory: srfi-1"
  (lset-union2 eq set elts))

(define (lset-union2 eq a b)
  "Internal helper: merge elements of list B into set A, skipping\nduplicates according to equality predicate EQ.\n\nExamples:\n  (lset-union2 eq? '(a b) '(b c))  => (c a b)\n\nParameters:\n  eq : procedure\n  a : list\n  b : list\nReturns: list\nCategory: srfi-1"
  (if (null? b)
      a
      (lset-union2 eq (if (member (car b) a eq) a (cons (car b) a)) (cdr b))))

(define (lset-union eq . sets)
  "Return the union of all SETS, using EQ as the element equality\npredicate. Elements from later sets are added to earlier ones,\nskipping duplicates.\n\nExamples:\n  (lset-union eq? '(a b) '(b c) '(c d))  => (d c a b)\n\nParameters:\n  eq : procedure\nReturns: list\nCategory: srfi-1\nKeywords: set union, merge, combine, set operations\n\nSee also: `lset-intersection', `lset-difference', `lset-xor'."
  (reduce (lambda (a b) (lset-union2 eq b a)) '() sets))

(define (lset-intersection eq . sets)
  "Return the intersection of all SETS, using EQ as the element\nequality predicate. The result contains only elements present\nin every set.\n\nExamples:\n  (lset-intersection eq? '(a b c) '(b c d))  => (b c)\n  (lset-intersection eq? '(a b) '(c d))      => ()\n\nParameters:\n  eq : procedure\nReturns: list\nCategory: srfi-1\nKeywords: set intersection, common elements, set operations\n\nSee also: `lset-union', `lset-difference', `lset-diff+intersection'."
  (reduce (lambda (a b) (filter (lambda (x) (member x a eq)) b)) '() sets))

(define (lset-diff2 eq a b)
  "Internal helper: return elements of B not present in A,\nusing EQ as the equality predicate.\n\nExamples:\n  (lset-diff2 eq? '(b c) '(a b c d))  => (a d)\n\nParameters:\n  eq : procedure\n  a : list\n  b : list\nReturns: list\nCategory: srfi-1"
  (remove (lambda (x) (member x a eq)) b))

(define (lset-difference eq . sets)
  "Return elements of the first set not present in any of the\nremaining SETS, using EQ as the element equality predicate.\n\nExamples:\n  (lset-difference eq? '(a b c d) '(b c))  => (a d)\n  (lset-difference eq? '(a b c) '(a) '(c))  => (b)\n\nParameters:\n  eq : procedure\nReturns: list\nCategory: srfi-1\nKeywords: set difference, subtract, set minus, set operations\n\nSee also: `lset-union', `lset-intersection', `lset-diff+intersection'."
  (reduce (lambda (a b) (lset-diff2 eq a b)) '() sets))

(define (lset-xor eq . sets)
  "Return the symmetric difference of all SETS using EQ as the\nequality predicate. The result contains elements that appear\nin exactly one of each pairwise reduction.\n\nExamples:\n  (lset-xor eq? '(a b c) '(b c d))      => (d a)\n  (lset-xor eq? '(a b) '(b c) '(a c))   => ()\n\nParameters:\n  eq : procedure\nReturns: list\nCategory: srfi-1\nKeywords: symmetric difference, exclusive or, set operations\n\nSee also: `lset-union', `lset-difference', `lset-intersection'."
  (reduce (lambda (a b) (append (lset-diff2 eq a b) (lset-diff2 eq b a)))
          '()
          sets))

(define (lset-diff+intersection eq . sets)
  "Return two values: the difference and intersection of SETS,\nusing EQ as the equality predicate. Equivalent to calling\nlset-difference and lset-intersection separately.\n\nExamples:\n  (lset-diff+intersection eq? '(a b c d) '(b c))  => (a d) (b c)\n\nParameters:\n  eq : procedure\nReturns: list\nCategory: srfi-1\n\nSee also: `lset-difference', `lset-intersection'."
  (values (apply lset-difference eq sets) (apply lset-intersection eq sets)))

(define lset-diff+intersection! lset-diff+intersection)
(define lset-xor! lset-xor)
(define lset-difference! lset-difference)
(define lset-intersection! lset-intersection)
(define lset-union! lset-union)
