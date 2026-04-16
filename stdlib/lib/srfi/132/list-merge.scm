;; list-merge.scm -- stable list merge
;; Part of SRFI 132: Sort Libraries

(define (list-merge less? lis1 lis2)
  "Merge two sorted lists LIS1 and LIS2 into a single sorted list,\nallocating fresh cons cells. The merge is stable: when elements\ncompare equal, elements from LIS1 precede those from LIS2.\nBoth input lists must already be sorted according to LESS?.\n\nExamples:\n  (list-merge < '(1 3 5) '(2 4 6))  => (1 2 3 4 5 6)\n  (list-merge < '(1 2) '())          => (1 2)\n  (list-merge < '() '(3 4))          => (3 4)\n  (list-merge < '(1 1) '(1 1))       => (1 1 1 1)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis1 : list -- first sorted list\n  lis2 : list -- second sorted list\nReturns: list\nCategory: srfi-132\nKeywords: merge, combine, sorted, stable, list\n\nSee also: `list-merge!', `list-sort'."
  (cond
    ((null? lis1) lis2)
    ((null? lis2) lis1)
    ((less? (car lis2) (car lis1))
     (cons (car lis2) (list-merge less? lis1 (cdr lis2))))
    (else
     (cons (car lis1) (list-merge less? (cdr lis1) lis2)))))

(define (list-merge! less? lis1 lis2)
  "Merge two sorted lists LIS1 and LIS2 by relinking existing\ncons cells with set-cdr!. The merge is stable: when elements\ncompare equal, elements from LIS1 precede those from LIS2.\nBoth input lists must already be sorted according to LESS?.\nThe input lists should not be used after this call.\n\nExamples:\n  (list-merge! < (list 1 3 5) (list 2 4 6))  => (1 2 3 4 5 6)\n  (list-merge! < (list 1 2) '())              => (1 2)\n  (list-merge! < '() (list 3 4))              => (3 4)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis1 : list -- first sorted list (consumed)\n  lis2 : list -- second sorted list (consumed)\nReturns: list\nCategory: srfi-132\nKeywords: merge, combine, sorted, stable, destructive, linear update, list\n\nSee also: `list-merge', `list-sort!'."
  (cond
    ((null? lis1) lis2)
    ((null? lis2) lis1)
    ((less? (car lis2) (car lis1))
     ;; lis2 head is strictly less — take from lis2
     (set-cdr! lis2 (list-merge! less? lis1 (cdr lis2)))
     lis2)
    (else
     ;; equal or lis1 head is less — take from lis1 (stability)
     (set-cdr! lis1 (list-merge! less? (cdr lis1) lis2))
     lis1)))
