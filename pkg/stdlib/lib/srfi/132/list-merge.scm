;; list-merge.scm -- stable list merge
;; Part of SRFI 132: Sort Libraries

(define (list-merge less? lis1 lis2)
  "Merge two sorted lists LIS1 and LIS2 into a single sorted list,
allocating fresh cons cells. The merge is stable: when elements
compare equal, elements from LIS1 precede those from LIS2.
Both input lists must already be sorted according to LESS?.
Uses an iterative head/tail pointer algorithm for stack safety.

Examples:
  (list-merge < '(1 3 5) '(2 4 6))  => (1 2 3 4 5 6)
  (list-merge < '(1 2) '())          => (1 2)
  (list-merge < '() '(3 4))          => (3 4)
  (list-merge < '(1 1) '(1 1))       => (1 1 1 1)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  lis1 : list -- first sorted list
  lis2 : list -- second sorted list
Returns: list
Category: srfi-132
Keywords: merge, combine, sorted, stable, list

See also: `list-merge!', `list-sort'."
  (if (null? lis1) (list-copy lis2)
      (if (null? lis2) (list-copy lis1)
          (let ((head (list #f)))
            (let loop ((tail head) (a lis1) (b lis2))
              (cond
                ((null? a)
                 (set-cdr! tail (list-copy b))
                 (cdr head))
                ((null? b)
                 (set-cdr! tail (list-copy a))
                 (cdr head))
                ((less? (car b) (car a))
                 (set-cdr! tail (list (car b)))
                 (loop (cdr tail) a (cdr b)))
                (else
                 (set-cdr! tail (list (car a)))
                 (loop (cdr tail) (cdr a) b))))))))

(define (list-merge! less? lis1 lis2)
  "Merge two sorted lists LIS1 and LIS2 by relinking existing
cons cells with set-cdr!. The merge is stable: when elements
compare equal, elements from LIS1 precede those from LIS2.
Both input lists must already be sorted according to LESS?.
The input lists should not be used after this call.
Uses an iterative head/tail pointer algorithm for stack safety.

Examples:
  (list-merge! < (list 1 3 5) (list 2 4 6))  => (1 2 3 4 5 6)
  (list-merge! < (list 1 2) '())              => (1 2)
  (list-merge! < '() (list 3 4))              => (3 4)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  lis1 : list -- first sorted list (consumed)
  lis2 : list -- second sorted list (consumed)
Returns: list
Category: srfi-132
Keywords: merge, combine, sorted, stable, destructive, linear update, list

See also: `list-merge', `list-sort!'."
  (if (null? lis1) lis2
      (if (null? lis2) lis1
          (let ((head (list #f)))
            (let loop ((tail head) (a lis1) (b lis2))
              (cond
                ((null? a)
                 (set-cdr! tail b)
                 (cdr head))
                ((null? b)
                 (set-cdr! tail a)
                 (cdr head))
                ((less? (car b) (car a))
                 (set-cdr! tail b)
                 (loop b a (cdr b)))
                (else
                 (set-cdr! tail a)
                 (loop a (cdr a) b))))))))
