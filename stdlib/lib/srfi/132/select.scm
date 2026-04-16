;; select.scm -- selection and separation for vectors
;; Part of SRFI 132: Sort Libraries

(define (%swap! v i j)
  "Internal: swap elements at indices I and J in vector V."
  (let ((tmp (vector-ref v i)))
    (vector-set! v i (vector-ref v j))
    (vector-set! v j tmp)))

(define (%median-of-3 < v a b c)
  "Internal: return the index of the median of v[a], v[b], v[c]
according to the comparison predicate <."
  (let ((va (vector-ref v a))
        (vb (vector-ref v b))
        (vc (vector-ref v c)))
    (if (< va vb)
        (if (< vb vc)
            b
            (if (< va vc)
                c
                a))
        (if (< va vc)
            a
            (if (< vb vc)
                c
                b)))))

(define (%partition3! < v lo hi pivot)
  "Internal: three-way partition v[lo..hi) around PIVOT.
Rearranges elements so that:
  v[lo..lt-end)    < pivot
  v[lt-end..eq-end) = pivot (neither < nor >)
  v[eq-end..hi)    > pivot
Returns two values: lt-end and eq-end."
  (let loop ((i lo) (lt lo) (gt hi))
    (if (>= i gt)
        (values lt gt)
        (let ((vi (vector-ref v i)))
          (cond
            ((< vi pivot)
             (%swap! v i lt)
             (loop (+ i 1) (+ lt 1) gt))
            ((< pivot vi)
             (%swap! v i (- gt 1))
             (loop i lt (- gt 1)))
            (else
             (loop (+ i 1) lt gt)))))))

(define (%quickselect! < v k lo hi)
  "Internal: rearrange v so that v[k] is the k-th smallest
element of v[lo..hi). Uses quickselect with median-of-3 pivot."
  (when (>= (- hi lo) 2)
    (let* ((mid (+ lo (quotient (- hi lo) 2)))
           (pivot-idx (%median-of-3 < v lo mid (- hi 1)))
           (pivot (vector-ref v pivot-idx)))
      (call-with-values
        (lambda ()
          (%partition3! < v lo hi pivot))
        (lambda (lt-end eq-end)
          (cond
            ((< k lt-end)
             (%quickselect! < v k lo lt-end))
            ((>= k eq-end)
             (%quickselect! < v k eq-end hi))))))))

(define vector-select!
  (case-lambda
    ((< v k)
     "Find the K-th smallest element in vector V according to the
comparison procedure <. The vector is mutated as a side effect of
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
  < : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  k : integer -- zero-based rank within the subrange
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: value -- the k-th smallest element
Category: srfi-132
Keywords: select, kth, quickselect, order statistic, vector

See also: `vector-separate!', `vector-find-median!', `vector-sort!'."
     (%quickselect! < v k 0 (vector-length v))
     (vector-ref v k))
    ((< v k start)
     (%quickselect! < v (+ start k) start (vector-length v))
     (vector-ref v (+ start k)))
    ((< v k start end)
     (%quickselect! < v (+ start k) start end)
     (vector-ref v (+ start k)))))

(define vector-separate!
  (case-lambda
    ((< v k)
     "Rearrange V so that the K smallest elements (according to <)
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
  < : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  k : integer -- number of smallest elements to place at the front
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: unspecified
Category: srfi-132
Keywords: separate, partition, kth, quickselect, vector

See also: `vector-select!', `vector-sort!'."
     (%quickselect! < v k 0 (vector-length v)))
    ((< v k start)
     (%quickselect! < v (+ start k) start (vector-length v)))
    ((< v k start end)
     (%quickselect! < v (+ start k) start end))))
