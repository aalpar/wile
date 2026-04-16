;; list-sort.scm -- bottom-up natural merge sort for lists
;; Part of SRFI 132: Sort Libraries

(define (%reverse-segment! head)
  "Internal: destructively reverse the list starting at HEAD.\nReturns the new head (former last pair)."
  (let loop ((prev '()) (cur head))
    (if (null? cur)
        prev
        (let ((next (cdr cur)))
          (set-cdr! cur prev)
          (loop cur next)))))

(define (%collect-runs less? lis)
  "Internal: scan LIS for ascending and descending runs.\nDescending runs are reversed in place. Each run is\nterminated by setting the cdr of its last pair to '().\nReturns a list of sorted sublists."
  (if (or (null? lis) (null? (cdr lis)))
      (list lis)
      (let outer ((rest lis) (runs '()))
        (if (null? rest)
            (reverse runs)
            (if (null? (cdr rest))
                ;; single trailing element is its own run
                (reverse (cons rest runs))
                (let ((head rest))
                  (if (less? (cadr rest) (car rest))
                      ;; descending run: scan while strictly descending
                      (let desc ((end rest))
                        (if (and (pair? (cdr end))
                                 (less? (cadr end) (car end)))
                            (desc (cdr end))
                            (let ((next (cdr end)))
                              (set-cdr! end '())
                              (outer next
                                     (cons (%reverse-segment! head) runs)))))
                      ;; ascending run: scan while non-descending
                      (let asc ((end rest))
                        (if (and (pair? (cdr end))
                                 (not (less? (cadr end) (car end))))
                            (asc (cdr end))
                            (let ((next (cdr end)))
                              (set-cdr! end '())
                              (outer next
                                     (cons head runs))))))))))))

(define (%merge-pairs less? runs)
  "Internal: pairwise merge the list of sorted runs using\nlist-merge! until one sorted list remains."
  (if (null? (cdr runs))
      (car runs)
      (%merge-pairs
       less?
       (let loop ((rs runs))
         (cond
           ((null? rs) '())
           ((null? (cdr rs)) rs)
           (else
            (cons (list-merge! less? (car rs) (cadr rs))
                  (loop (cddr rs)))))))))

(define (list-sort! less? lis)
  "Sort the list LIS according to the comparison procedure LESS?\nusing a bottom-up natural merge sort. This is a destructive\noperation that may reuse cons cells from the input. The merge\nis stable: equal elements preserve their original order.\nReturns the sorted list.\n\nExamples:\n  (list-sort! < (list 3 1 4 1 5 9 2 6))  => (1 1 2 3 4 5 6 9)\n  (list-sort! < (list 5 4 3 2 1))         => (1 2 3 4 5)\n  (list-sort! < '())                      => ()\n  (list-sort! < (list 42))                => (42)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis : list (consumed)\nReturns: list\nCategory: srfi-132\nKeywords: sort, order, merge sort, stable, destructive, linear update, list\n\nSee also: `list-sort', `list-stable-sort!', `list-merge!'."
  (if (or (null? lis) (null? (cdr lis)))
      lis
      (%merge-pairs less? (%collect-runs less? lis))))

(define (list-sort less? lis)
  "Return a freshly allocated list containing the elements of LIS\nsorted according to the comparison procedure LESS?. The input\nlist is not modified. Uses a stable merge sort: equal elements\npreserve their original order.\n\nExamples:\n  (list-sort < '(3 1 4 1 5 9 2 6))  => (1 1 2 3 4 5 6 9)\n  (list-sort < '(5 4 3 2 1))         => (1 2 3 4 5)\n  (list-sort < '())                   => ()\n  (list-sort > '(1 2 3))              => (3 2 1)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis : list\nReturns: list\nCategory: srfi-132\nKeywords: sort, order, merge sort, stable, non-destructive, list\n\nSee also: `list-sort!', `list-stable-sort', `list-merge'."
  (list-sort! less? (list-copy lis)))

(define (list-stable-sort less? lis)
  "Return a freshly allocated list containing the elements of LIS\nsorted stably according to LESS?. Equivalent to list-sort since\nthe underlying merge sort is inherently stable.\n\nExamples:\n  (list-stable-sort < '(3 1 2))  => (1 2 3)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis : list\nReturns: list\nCategory: srfi-132\nKeywords: sort, order, merge sort, stable, non-destructive, list\n\nSee also: `list-sort', `list-stable-sort!'."
  (list-sort less? lis))

(define (list-stable-sort! less? lis)
  "Sort the list LIS stably according to LESS?, possibly reusing\ncons cells. Equivalent to list-sort! since the underlying merge\nsort is inherently stable.\n\nExamples:\n  (list-stable-sort! < (list 3 1 2))  => (1 2 3)\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis : list (consumed)\nReturns: list\nCategory: srfi-132\nKeywords: sort, order, merge sort, stable, destructive, linear update, list\n\nSee also: `list-sort!', `list-stable-sort'."
  (list-sort! less? lis))
