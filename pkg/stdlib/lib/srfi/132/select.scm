;; select.scm -- selection and separation for vectors
;; Part of SRFI 132: Sort Libraries

(define (%swap! v i j)
  "Internal: swap elements at indices I and J in vector V.

Parameters:
  v : vector (mutated)
  i : integer -- first index
  j : integer -- second index
Returns: unspecified
Category: srfi-132
Keywords: swap, vector, index, internal"
  (let ((tmp (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp)))

(define (%median-of-3 less? v a b c)
  "Internal: return the index of the median of v[a], v[b], v[c]
according to the comparison predicate LESS?.

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector
  a : integer -- first index
  b : integer -- second index
  c : integer -- third index
Returns: integer -- the index of the median element
Category: srfi-132
Keywords: median, pivot, quickselect, internal"
  (let ((va (vector-ref v a))
        (vb (vector-ref v b))
        (vc (vector-ref v c)))
    (if (less? va vb)
        (if (less? vb vc)
            b
            (if (less? va vc)
                c
                a))
        (if (less? va vc)
            a
            (if (less? vb vc)
                c
                b)))))

(define (%partition3! less? v lo hi pivot)
  "Internal: three-way partition v[lo..hi) around PIVOT.
Rearranges elements so that:
  v[lo..lt-end)    < pivot
  v[lt-end..eq-end) = pivot (neither < nor >)
  v[eq-end..hi)    > pivot
Returns two values: lt-end and eq-end.

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  lo : integer -- start index (inclusive)
  hi : integer -- end index (exclusive)
  pivot : value -- the pivot element
Returns: (values integer integer) -- lt-end and eq-end
Category: srfi-132
Keywords: partition, three-way, quickselect, internal"
  (let loop ((i lo) (lt lo) (gt hi))
    (if (>= i gt)
        (values lt gt)
        (let ((vi (vector-ref v i)))
          (cond
            ((less? vi pivot)
             (%swap! v i lt)
             (loop (+ i 1) (+ lt 1) gt))
            ((less? pivot vi)
             (%swap! v i (- gt 1))
             (loop i lt (- gt 1)))
            (else
             (loop (+ i 1) lt gt)))))))

(define (%quickselect! less? v k lo hi)
  "Internal: rearrange v so that v[k] is the k-th smallest
element of v[lo..hi). Uses quickselect with median-of-3 pivot.

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  k : integer -- target rank (absolute index)
  lo : integer -- start index (inclusive)
  hi : integer -- end index (exclusive)
Returns: unspecified
Category: srfi-132
Keywords: quickselect, selection, order statistic, internal"
  (when (>= (- hi lo) 2)
    (let* ((mid (+ lo (quotient (- hi lo) 2)))
           (pivot-idx (%median-of-3 less? v lo mid (- hi 1)))
           (pivot (vector-ref v pivot-idx)))
      (call-with-values
        (lambda ()
          (%partition3! less? v lo hi pivot))
        (lambda (lt-end eq-end)
          (cond
            ((< k lt-end)
             (%quickselect! less? v k lo lt-end))
            ((>= k eq-end)
             (%quickselect! less? v k eq-end hi))))))))

(define vector-select!
  (case-lambda
    ((less? v k)
     "Find the K-th smallest element in vector V according to the
comparison procedure LESS?. The vector is mutated as a side effect of
the selection algorithm (quickselect with median-of-3 pivot).
K is zero-based relative to the subrange.

Optional START and END arguments restrict the selection to a
subrange of V.

Examples:
  (let ((v (vector 5 3 1 4 2)))
    (vector-select! < v 0))   => 1
  (let ((v (vector 5 3 1 4 2)))
    (vector-select! < v 2))   => 3
  (let ((v (vector 5 3 1 4 2)))
    (vector-select! < v 4))   => 5
  (let ((v (vector 9 8 7 6 5 4 3 2 1)))
    (vector-select! < v 1 2 7))  => 4

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  k : integer -- zero-based rank within the subrange
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: value -- the k-th smallest element
Category: srfi-132
Keywords: select, kth, quickselect, order statistic, vector

See also: `vector-separate!', `vector-find-median!', `vector-sort!'."
     (let ((len (vector-length v)))
       (%check-range "vector-select!" v 0 len)
       (when (or (< k 0) (>= k len))
         (error "vector-select!: k out of range" k 0 len))
       (%quickselect! less? v k 0 len)
       (vector-ref v k)))
    ((less? v k start)
     (let ((end (vector-length v)))
       (%check-range "vector-select!" v start end)
       (when (or (< k 0) (>= (+ start k) end))
         (error "vector-select!: k out of range" k start end))
       (%quickselect! less? v (+ start k) start end)
       (vector-ref v (+ start k))))
    ((less? v k start end)
     (%check-range "vector-select!" v start end)
     (when (or (< k 0) (>= (+ start k) end))
       (error "vector-select!: k out of range" k start end))
     (%quickselect! less? v (+ start k) start end)
     (vector-ref v (+ start k)))))

(define vector-separate!
  (case-lambda
    ((less? v k)
     "Rearrange V so that the K smallest elements (according to LESS?)
are in positions 0 through K-1 (in no particular order among
themselves). This is equivalent to a partial sort. K is zero-based
relative to the subrange.

Optional START and END arguments restrict the operation to a
subrange of V.

Examples:
  (let ((v (vector 5 3 1 4 2)))
    (vector-separate! < v 2)
    (vector-ref v 0))  ;; one of 1 or 2

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  k : integer -- number of smallest elements to place at the front
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: unspecified
Category: srfi-132
Keywords: separate, partition, kth, quickselect, vector

See also: `vector-select!', `vector-sort!'."
     (let ((len (vector-length v)))
       (%check-range "vector-separate!" v 0 len)
       (when (or (< k 0) (> k len))
         (error "vector-separate!: k out of range" k 0 len))
       (when (< k len)
         (%quickselect! less? v k 0 len))))
    ((less? v k start)
     (let ((end (vector-length v)))
       (%check-range "vector-separate!" v start end)
       (when (or (< k 0) (> k (- end start)))
         (error "vector-separate!: k out of range" k start end))
       (when (< (+ start k) end)
         (%quickselect! less? v (+ start k) start end))))
    ((less? v k start end)
     (%check-range "vector-separate!" v start end)
     (when (or (< k 0) (> k (- end start)))
       (error "vector-separate!: k out of range" k start end))
     (when (< (+ start k) end)
       (%quickselect! less? v (+ start k) start end)))))
