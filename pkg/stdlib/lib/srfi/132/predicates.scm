;; predicates.scm -- sorted predicates for lists and vectors
;; Part of SRFI 132: Sort Libraries

(define (%check-range who v start end)
  "Internal: validate that START and END form a valid subrange of
vector V. Raises an error if start is negative, end exceeds the
vector length, or start exceeds end.

Parameters:
  who : string -- the name of the calling procedure (for error messages)
  v : vector -- the vector whose bounds are checked
  start : integer -- start index (inclusive)
  end : integer -- end index (exclusive)
Returns: unspecified
Category: srfi-132
Keywords: validation, bounds, range, vector, internal"
  (let ((len (vector-length v)))
    (when (< start 0)
      (error (string-append who ": start index negative") start))
    (when (> end len)
      (error (string-append who ": end index exceeds vector length") end len))
    (when (> start end)
      (error (string-append who ": start index exceeds end index") start end))))

(define (list-sorted? less? lis)
  "Return #t if LIS is sorted according to the comparison
procedure LESS?, i.e. no element is less than the one
before it. Returns #t for empty and single-element lists.

Examples:
  (list-sorted? < '(1 2 3))    => #t
  (list-sorted? < '(1 3 2))    => #f
  (list-sorted? < '())         => #t
  (list-sorted? < '(42))       => #t

Parameters:
  less? : procedure -- a two-argument comparison predicate
  lis : list
Returns: boolean
Category: srfi-132
Keywords: sorted, ordered, monotone, predicate, list

See also: `vector-sorted?'."
  (or (null? lis)
      (null? (cdr lis))
      (let loop ((prev (car lis)) (rest (cdr lis)))
        (or (null? rest)
            (let ((cur (car rest)))
              (and (not (less? cur prev))
                   (loop cur (cdr rest))))))))

(define (%vector-sorted? less? v start end)
  "Internal: check whether V is sorted in the range [START, END)
according to LESS?.

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v : vector
  start : integer -- start index (inclusive)
  end : integer -- end index (exclusive)
Returns: boolean
Category: srfi-132
Keywords: sorted, ordered, predicate, vector, internal"
  (or (<= (- end start) 1)
      (let loop ((i (+ start 1)))
        (or (>= i end)
            (and (not (less? (vector-ref v i)
                             (vector-ref v (- i 1))))
                 (loop (+ i 1)))))))

(define vector-sorted?
  (case-lambda
    ((less? v)
     "Return #t if vector V is sorted according to the comparison
procedure LESS?. Returns #t for empty and single-element vectors.
Optional START and END arguments restrict the check to a subrange.

Examples:
  (vector-sorted? < #(1 2 3))      => #t
  (vector-sorted? < #(1 3 2))      => #f
  (vector-sorted? < #())           => #t
  (vector-sorted? < #(5 1 2 3) 1 4) => #t

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v : vector
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: boolean
Category: srfi-132
Keywords: sorted, ordered, monotone, predicate, vector

See also: `list-sorted?'."
     (%vector-sorted? less? v 0 (vector-length v)))
    ((less? v start)
     (let ((end (vector-length v)))
       (%check-range "vector-sorted?" v start end)
       (%vector-sorted? less? v start end)))
    ((less? v start end)
     (%check-range "vector-sorted?" v start end)
     (%vector-sorted? less? v start end))))
