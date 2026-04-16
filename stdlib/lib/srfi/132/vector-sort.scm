;; vector-sort.scm -- bottom-up merge sort for vectors
;; Part of SRFI 132: Sort Libraries

(define (%vector-sort! less? v start end)
  "Internal: sort v[start..end) in place using bottom-up merge sort
with buffer alternation. Uses %vector-merge-into! for each merge
pass.

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v : vector (mutated)
  start : integer -- start index (inclusive)
  end : integer -- end index (exclusive)
Returns: unspecified
Category: srfi-132
Keywords: sort, merge sort, stable, vector, internal"
  (let ((len (- end start)))
    (when (> len 1)
      (let ((temp (make-vector len)))
        ;; Copy v[start..end) into temp[0..len)
        (let init ((i 0))
          (when (< i len)
            (vector-set! temp i (vector-ref v (+ start i)))
            (init (+ i 1))))
        ;; Bottom-up merge passes.
        ;; Pass 1: temp(src) -> v(dst), offset src=0, dst=start
        ;; Pass 2: v(src) -> temp(dst), offset src=start, dst=0
        ;; ...and so on, alternating each pass.
        (let pass ((width 1) (src temp) (src-off 0) (dst v) (dst-off start))
          (if (>= width len)
              ;; All passes done. If result is in temp, copy back to v.
              (when (eq? src temp)
                (let copy ((i 0))
                  (when (< i len)
                    (vector-set! v (+ start i) (vector-ref temp i))
                    (copy (+ i 1)))))
              (begin
                ;; Merge adjacent width-sized runs from src into dst
                (let merge-runs ((lo 0))
                  (cond
                    ((>= lo len)
                     ;; All runs merged for this pass
                     (values))
                    ((>= (+ lo width) len)
                     ;; Only one run left — copy it to dst as-is
                     (let copy ((i lo))
                       (when (< i len)
                         (vector-set! dst (+ dst-off i)
                                      (vector-ref src (+ src-off i)))
                         (copy (+ i 1)))))
                    (else
                     ;; Merge src[lo..mid) with src[mid..hi) into dst
                     (let* ((mid (+ lo width))
                            (hi (min (+ lo width width) len)))
                       (%vector-merge-into! less?
                                            dst (+ dst-off lo)
                                            src (+ src-off lo) (+ src-off mid)
                                            src (+ src-off mid) (+ src-off hi))
                       (merge-runs hi)))))
                ;; Next pass: swap src/dst
                (pass (* width 2) dst dst-off src src-off))))))))

(define vector-sort!
  (case-lambda
    ((less? v)
     "Sort vector V in place according to the comparison procedure
LESS? using a stable bottom-up merge sort. Optional START and END
arguments restrict the sort to a subrange.

Examples:
  (let ((v (vector 3 1 4 1 5 9 2 6)))
    (vector-sort! < v)
    v)  => #(1 1 2 3 4 5 6 9)
  (let ((v (vector 5 3 1 2 4)))
    (vector-sort! < v 1 4)
    v)  => #(5 1 2 3 4)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v : vector (mutated)
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: unspecified
Category: srfi-132
Keywords: sort, order, merge sort, stable, destructive, vector

See also: `vector-sort', `vector-stable-sort!', `list-sort!'."
     (%vector-sort! less? v 0 (vector-length v)))
    ((less? v start)
     (let ((end (vector-length v)))
       (%check-range "vector-sort!" v start end)
       (%vector-sort! less? v start end)))
    ((less? v start end)
     (%check-range "vector-sort!" v start end)
     (%vector-sort! less? v start end))))

(define vector-sort
  (case-lambda
    ((less? v)
     "Return a freshly allocated vector containing the elements of V
sorted according to the comparison procedure LESS?. The input
vector is not modified. Uses a stable merge sort: equal elements
preserve their original order. Optional START and END arguments
restrict the sort to a subrange.

Examples:
  (vector-sort < #(3 1 4 1 5 9 2 6))  => #(1 1 2 3 4 5 6 9)
  (vector-sort < #(5 4 3 2 1))         => #(1 2 3 4 5)
  (vector-sort < #())                   => #()
  (vector-sort > #(1 2 3))              => #(3 2 1)
  (vector-sort < #(5 3 1 2 4) 1 4)     => #(1 2 3)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v : vector
  start : integer (optional, default 0)
  end : integer (optional, default (vector-length v))
Returns: vector
Category: srfi-132
Keywords: sort, order, merge sort, stable, non-destructive, vector

See also: `vector-sort!', `vector-stable-sort', `list-sort'."
     (let ((copy (vector-copy v)))
       (%vector-sort! less? copy 0 (vector-length copy))
       copy))
    ((less? v start)
     (let ((end (vector-length v)))
       (%check-range "vector-sort" v start end)
       (let ((copy (vector-copy v start)))
         (%vector-sort! less? copy 0 (vector-length copy))
         copy)))
    ((less? v start end)
     (%check-range "vector-sort" v start end)
     (let ((copy (vector-copy v start end)))
       (%vector-sort! less? copy 0 (vector-length copy))
       copy))))

(define vector-stable-sort vector-sort)

(define vector-stable-sort! vector-sort!)
