;; dedup.scm -- neighbor deduplication for lists and vectors
;; Part of SRFI 132: Sort Libraries

(define (list-delete-neighbor-dups = lis)
  "Return a freshly allocated list with adjacent duplicate elements
removed. Elements are compared pairwise using the equality predicate
=. Only the first element of each run of equal neighbors is kept.
The input list is not modified.

Examples:
  (list-delete-neighbor-dups equal? '(1 1 2 3 3 3 4))  => (1 2 3 4)
  (list-delete-neighbor-dups equal? '(1 2 3))           => (1 2 3)
  (list-delete-neighbor-dups equal? '())                 => ()
  (list-delete-neighbor-dups equal? '(7))                => (7)
  (list-delete-neighbor-dups equal? '(1 1 1))            => (1)

Parameters:
  = : procedure -- a two-argument equality predicate
  lis : list
Returns: list
Category: srfi-132
Keywords: dedup, deduplicate, unique, neighbor, adjacent, list

See also: `list-delete-neighbor-dups!', `vector-delete-neighbor-dups'."
  (if (null? lis)
      '()
      (let loop ((prev (car lis))
                 (rest (cdr lis))
                 (acc (list (car lis))))
        (if (null? rest)
            (reverse acc)
            (let ((cur (car rest)))
              (if (= prev cur)
                  (loop prev (cdr rest) acc)
                  (loop cur (cdr rest) (cons cur acc))))))))

(define (list-delete-neighbor-dups! = lis)
  "Remove adjacent duplicate elements from LIS by relinking pairs
with set-cdr!. Elements are compared pairwise using the equality
predicate =. Only the first element of each run of equal neighbors
is kept. This is a destructive operation that reuses the cons cells
of the input list. Returns the (possibly shortened) list starting
at the original head.

Examples:
  (list-delete-neighbor-dups! equal? (list 1 1 2 3 3 3 4))
    => (1 2 3 4)
  (list-delete-neighbor-dups! equal? (list 1 2 3))
    => (1 2 3)
  (list-delete-neighbor-dups! equal? '())  => ()
  (list-delete-neighbor-dups! equal? (list 7))  => (7)

Parameters:
  = : procedure -- a two-argument equality predicate
  lis : list (mutated)
Returns: list
Category: srfi-132
Keywords: dedup, deduplicate, unique, neighbor, adjacent, destructive, list

See also: `list-delete-neighbor-dups', `vector-delete-neighbor-dups!'."
  (if (or (null? lis) (null? (cdr lis)))
      lis
      (let loop ((prev lis) (cur (cdr lis)))
        (cond
          ((null? cur)
           lis)
          ((= (car prev) (car cur))
           (set-cdr! prev (cdr cur))
           (loop prev (cdr cur)))
          (else
           (loop cur (cdr cur)))))))

(define (%vector-delete-neighbor-dups = v start end)
  "Internal: compact v[start..end) removing adjacent duplicates.
Returns a freshly allocated vector containing the compacted result."
  (if (>= start end)
      (vector)
      (let ((temp (make-vector (- end start))))
        (vector-set! temp 0 (vector-ref v start))
        (let loop ((i (+ start 1)) (j 1))
          (if (>= i end)
              (vector-copy temp 0 j)
              (let ((cur (vector-ref v i)))
                (if (= (vector-ref temp (- j 1)) cur)
                    (loop (+ i 1) j)
                    (begin
                      (vector-set! temp j cur)
                      (loop (+ i 1) (+ j 1))))))))))

(define vector-delete-neighbor-dups
  (case-lambda
    ((= v)
     "Return a freshly allocated vector with adjacent duplicate
elements removed from V. Elements are compared pairwise using
the equality predicate =. Only the first element of each run of
equal neighbors is kept. The input vector is not modified.

Optional START and END arguments restrict the operation to a
subrange of V.

Examples:
  (vector-delete-neighbor-dups equal? #(1 1 2 3 3 3 4))
    => #(1 2 3 4)
  (vector-delete-neighbor-dups equal? #(1 2 3))
    => #(1 2 3)
  (vector-delete-neighbor-dups equal? #())  => #()
  (vector-delete-neighbor-dups equal? #(1 1 2 2 3) 1 4)
    => #(1 2)

Parameters:
  = : procedure -- a two-argument equality predicate
  v : vector
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: vector
Category: srfi-132
Keywords: dedup, deduplicate, unique, neighbor, adjacent, vector

See also: `vector-delete-neighbor-dups!', `list-delete-neighbor-dups'."
     (%vector-delete-neighbor-dups = v 0 (vector-length v)))
    ((= v start)
     (%vector-delete-neighbor-dups = v start (vector-length v)))
    ((= v start end)
     (%vector-delete-neighbor-dups = v start end))))

(define (%vector-delete-neighbor-dups! = v start end)
  "Internal: compact v[start..end) in place, removing adjacent
duplicates. Returns the new logical end index (integer)."
  (if (>= start end)
      start
      (let loop ((i (+ start 1)) (j (+ start 1)))
        (if (>= i end)
            j
            (if (= (vector-ref v (- j 1)) (vector-ref v i))
                (loop (+ i 1) j)
                (begin
                  (vector-set! v j (vector-ref v i))
                  (loop (+ i 1) (+ j 1))))))))

(define vector-delete-neighbor-dups!
  (case-lambda
    ((= v)
     "Remove adjacent duplicate elements from V by compacting in
place. Elements are compared pairwise using the equality predicate
=. Only the first element of each run of equal neighbors is kept.
Returns an integer: the new logical end index. Elements at or
beyond the returned index are unspecified.

Optional START and END arguments restrict the operation to a
subrange of V.

Examples:
  (let ((v (vector 1 1 2 3 3 3 4)))
    (vector-delete-neighbor-dups! equal? v))  => 4
  (let ((v (vector 1 2 3)))
    (vector-delete-neighbor-dups! equal? v))  => 3
  (vector-delete-neighbor-dups! equal? #())   => 0

Parameters:
  = : procedure -- a two-argument equality predicate
  v : vector (mutated)
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: integer -- the new logical end index
Category: srfi-132
Keywords: dedup, deduplicate, unique, neighbor, adjacent, destructive, vector

See also: `vector-delete-neighbor-dups', `list-delete-neighbor-dups!'."
     (%vector-delete-neighbor-dups! = v 0 (vector-length v)))
    ((= v start)
     (%vector-delete-neighbor-dups! = v start (vector-length v)))
    ((= v start end)
     (%vector-delete-neighbor-dups! = v start end))))
