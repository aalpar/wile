;; predicates.scm -- sorted predicates for lists and vectors
;; Part of SRFI 132: Sort Libraries

(define (list-sorted? less? lis)
  "Return #t if LIS is sorted according to the comparison\nprocedure LESS?, i.e. no element is less than the one\nbefore it. Returns #t for empty and single-element lists.\n\nExamples:\n  (list-sorted? < '(1 2 3))    => #t\n  (list-sorted? < '(1 3 2))    => #f\n  (list-sorted? < '())         => #t\n  (list-sorted? < '(42))       => #t\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lis : list\nReturns: boolean\nCategory: srfi-132\nKeywords: sorted, ordered, monotone, predicate, list\n\nSee also: `vector-sorted?'."
  (or (null? lis)
      (null? (cdr lis))
      (let loop ((prev (car lis)) (rest (cdr lis)))
        (or (null? rest)
            (let ((cur (car rest)))
              (and (not (less? cur prev))
                   (loop cur (cdr rest))))))))

(define (%vector-sorted? less? v start end)
  "Internal: check whether V is sorted in the range [START, END)."
  (or (<= (- end start) 1)
      (let loop ((i (+ start 1)))
        (or (>= i end)
            (and (not (less? (vector-ref v i)
                             (vector-ref v (- i 1))))
                 (loop (+ i 1)))))))

(define vector-sorted?
  (case-lambda
    ((less? v)
     "Return #t if vector V is sorted according to the comparison\nprocedure LESS?. Returns #t for empty and single-element vectors.\nOptional START and END arguments restrict the check to a subrange.\n\nExamples:\n  (vector-sorted? < #(1 2 3))      => #t\n  (vector-sorted? < #(1 3 2))      => #f\n  (vector-sorted? < #())           => #t\n  (vector-sorted? < #(5 1 2 3) 1 4) => #t\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  v : vector\n  start : integer (optional, default 0)\n  end : integer (optional, default (vector-length v))\nReturns: boolean\nCategory: srfi-132\nKeywords: sorted, ordered, monotone, predicate, vector\n\nSee also: `list-sorted?'."
     (%vector-sorted? less? v 0 (vector-length v)))
    ((less? v start)
     (%vector-sorted? less? v start (vector-length v)))
    ((less? v start end)
     (%vector-sorted? less? v start end))))
