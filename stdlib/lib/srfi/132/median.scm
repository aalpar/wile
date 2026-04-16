;; median.scm -- median finding for vectors
;; Part of SRFI 132: Sort Libraries

(define (%default-mean a b)
  "Internal: default mean procedure for vector-find-median when no
mean is provided. Returns the arithmetic mean (/ (+ a b) 2),
per SRFI 132 specification.

Parameters:
  a : value -- first middle element
  b : value -- second middle element
Returns: value -- the arithmetic mean of a and b
Category: srfi-132
Keywords: mean, average, median, internal"
  (/ (+ a b) 2))

(define (%vector-find-median! less? v knil mean)
  "Internal: find the median of V, which may be mutated.
Uses vector-select! for O(n) expected time. For even-length
vectors, applies MEAN to the two middle elements.

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  knil : value -- returned when v is empty
  mean : procedure -- two-argument mean procedure for even-length case
Returns: value -- the median or mean of two middle elements
Category: srfi-132
Keywords: median, middle, order statistic, select, vector, internal"
  (let ((len (vector-length v)))
    (cond
      ((= len 0)
       knil)
      ((= len 1)
       (vector-ref v 0))
      ((odd? len)
       (vector-select! less? v (quotient len 2)))
      (else
       ;; Even length: find the two middle elements.
       ;; k is the index of the lower middle.
       (let ((k (- (quotient len 2) 1)))
         (let ((lower (vector-select! less? v k)))
           ;; After quickselect, v[k] is correct, but v[k+1..len)
           ;; is not necessarily sorted. Scan for the minimum of
           ;; v[k+1..len) to find the upper middle.
           (let ((upper (vector-ref v (+ k 1))))
             (let scan ((i (+ k 2)) (m upper))
               (if (>= i len)
                   (mean lower m)
                   (let ((vi (vector-ref v i)))
                     (if (less? vi m)
                         (scan (+ i 1) vi)
                         (scan (+ i 1) m))))))))))))

(define vector-find-median!
  (case-lambda
    ((less? v knil)
     "Find the median of vector V according to the comparison
procedure LESS?. If V is empty, return KNIL. If V has odd length,
return the middle element. If V has even length, return the result
of applying the mean procedure (default: arithmetic mean
(/ (+ a b) 2)) to the two middle elements.

The vector V may be mutated as a side effect of the selection
algorithm.

An optional fourth argument MEAN is a two-argument procedure used
to compute the mean of the two middle elements for even-length
vectors. When omitted, the arithmetic mean is used.

Examples:
  (vector-find-median! < (vector 3 1 4 1 5) 0)    => 3
  (vector-find-median! < (vector 3 1 4 1 5 9) 0)   => 7/2
  (vector-find-median! < (vector 3 1 4 1 5 9) 0
    (lambda (a b) (/ (+ a b) 2)))                   => 7/2
  (vector-find-median! < (vector) 42)               => 42
  (vector-find-median! < (vector 7) 0)              => 7

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (mutated)
  knil : value -- returned when v is empty
  mean : procedure (optional) -- two-argument mean procedure
Returns: value -- the median or mean of two middle elements
Category: srfi-132
Keywords: median, middle, order statistic, select, vector

See also: `vector-find-median', `vector-select!'."
     (%vector-find-median! less? v knil %default-mean))
    ((less? v knil mean)
     (%vector-find-median! less? v knil mean))))

(define vector-find-median
  (case-lambda
    ((less? v knil)
     "Find the median of vector V without modifying V. Makes a copy
of V and delegates to vector-find-median!.

If V is empty, return KNIL. If V has odd length, return the middle
element. If V has even length, return the result of applying the
mean procedure (default: arithmetic mean (/ (+ a b) 2)) to the
two middle elements.

An optional fourth argument MEAN is a two-argument procedure used
to compute the mean of the two middle elements for even-length
vectors. When omitted, the arithmetic mean is used.

Examples:
  (vector-find-median < #(3 1 4 1 5) 0)    => 3
  (vector-find-median < #(3 1 4 1 5 9) 0)   => 7/2
  (vector-find-median < #(3 1 4 1 5 9) 0
    (lambda (a b) (/ (+ a b) 2)))            => 7/2
  (vector-find-median < #() 42)              => 42
  (vector-find-median < #(7) 0)              => 7

Parameters:
  less? : procedure -- a two-argument comparison (less-than) predicate
  v : vector (not modified)
  knil : value -- returned when v is empty
  mean : procedure (optional) -- two-argument mean procedure
Returns: value -- the median or mean of two middle elements
Category: srfi-132
Keywords: median, middle, order statistic, select, non-destructive, vector

See also: `vector-find-median!', `vector-select!'."
     (vector-find-median! less? (vector-copy v) knil %default-mean))
    ((less? v knil mean)
     (vector-find-median! less? (vector-copy v) knil mean))))
