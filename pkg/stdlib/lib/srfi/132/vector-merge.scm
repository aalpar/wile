;; vector-merge.scm -- stable vector merge
;; Part of SRFI 132: Sort Libraries

(define (%vector-merge-into! less? target tstart v1 s1 e1 v2 s2 e2)
  "Internal: merge v1[s1..e1) and v2[s2..e2) into TARGET starting
at index TSTART. Two-pointer merge. Stable: when elements compare
equal, the element from v1 is taken first.

Parameters:
  less? : procedure -- a two-argument comparison predicate
  target : vector (mutated) -- destination vector
  tstart : integer -- write offset in target
  v1 : vector -- first sorted source vector
  s1 : integer -- start index in v1 (inclusive)
  e1 : integer -- end index in v1 (exclusive)
  v2 : vector -- second sorted source vector
  s2 : integer -- start index in v2 (inclusive)
  e2 : integer -- end index in v2 (exclusive)
Returns: unspecified
Category: srfi-132
Keywords: merge, vector, two-pointer, stable, internal"
  (let loop ((i tstart) (j s1) (k s2))
    (cond
      ((>= j e1)
       ;; v1 exhausted — copy remainder of v2
       (let copy ((i i) (k k))
         (when (< k e2)
           (vector-set! target i (vector-ref v2 k))
           (copy (+ i 1) (+ k 1)))))
      ((>= k e2)
       ;; v2 exhausted — copy remainder of v1
       (let copy ((i i) (j j))
         (when (< j e1)
           (vector-set! target i (vector-ref v1 j))
           (copy (+ i 1) (+ j 1)))))
      ((less? (vector-ref v2 k) (vector-ref v1 j))
       ;; v2 element is strictly less — take from v2
       (vector-set! target i (vector-ref v2 k))
       (loop (+ i 1) j (+ k 1)))
      (else
       ;; equal or v1 element is less — take from v1 (stability)
       (vector-set! target i (vector-ref v1 j))
       (loop (+ i 1) (+ j 1) k)))))

(define (%vector-merge less? v1 s1 e1 v2 s2 e2)
  "Internal: allocate a fresh vector and merge v1[s1..e1) with
v2[s2..e2) into it. Returns the new vector.

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v1 : vector -- first sorted source vector
  s1 : integer -- start index in v1 (inclusive)
  e1 : integer -- end index in v1 (exclusive)
  v2 : vector -- second sorted source vector
  s2 : integer -- start index in v2 (inclusive)
  e2 : integer -- end index in v2 (exclusive)
Returns: vector -- freshly allocated merged vector
Category: srfi-132
Keywords: merge, vector, allocate, stable, internal"
  (let* ((len1 (- e1 s1))
         (len2 (- e2 s2))
         (result (make-vector (+ len1 len2))))
    (%vector-merge-into! less? result 0 v1 s1 e1 v2 s2 e2)
    result))

(define vector-merge
  (case-lambda
    ((less? v1 v2)
     "Merge two sorted vectors V1 and V2 into a freshly allocated
sorted vector. The merge is stable: when elements compare equal,
elements from V1 precede those from V2. Both input vectors must
already be sorted according to LESS?.

Optional START and END arguments restrict the merge to subranges
of V1 and V2.

Examples:
  (vector-merge < #(1 3 5) #(2 4 6))        => #(1 2 3 4 5 6)
  (vector-merge < #(1 2) #())                => #(1 2)
  (vector-merge < #() #(3 4))                => #(3 4)
  (vector-merge < #(1 1) #(1 1))             => #(1 1 1 1)
  (vector-merge < #(0 1 3 5 9) #(0 2 4 6 8) 1 4 1 4)
    => #(1 2 3 4 5 6)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  v1 : vector -- first sorted vector
  v2 : vector -- second sorted vector
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (vector-length v1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (vector-length v2))
Returns: vector
Category: srfi-132
Keywords: merge, combine, sorted, stable, vector

See also: `vector-merge!', `vector-sort', `list-merge'."
     (%vector-merge less? v1 0 (vector-length v1)
                          v2 0 (vector-length v2)))
    ((less? v1 v2 start1)
     (%check-range "vector-merge" v1 start1 (vector-length v1))
     (%vector-merge less? v1 start1 (vector-length v1)
                          v2 0 (vector-length v2)))
    ((less? v1 v2 start1 end1)
     (%check-range "vector-merge" v1 start1 end1)
     (%vector-merge less? v1 start1 end1
                          v2 0 (vector-length v2)))
    ((less? v1 v2 start1 end1 start2)
     (%check-range "vector-merge" v1 start1 end1)
     (%check-range "vector-merge" v2 start2 (vector-length v2))
     (%vector-merge less? v1 start1 end1
                          v2 start2 (vector-length v2)))
    ((less? v1 v2 start1 end1 start2 end2)
     (%check-range "vector-merge" v1 start1 end1)
     (%check-range "vector-merge" v2 start2 end2)
     (%vector-merge less? v1 start1 end1
                          v2 start2 end2))))

(define vector-merge!
  (case-lambda
    ((less? to from1 from2)
     "Merge sorted vectors FROM1 and FROM2 into the vector TO,
writing results starting at index TSTART (default 0). The merge
is stable: when elements compare equal, elements from FROM1
precede those from FROM2. Both input vectors must already be
sorted according to LESS?.

Optional START and TSTART arguments restrict the merge to
subranges of FROM1 and FROM2, and control the write offset in TO.

Examples:
  (let ((v (make-vector 6)))
    (vector-merge! < v #(1 3 5) #(2 4 6))
    v)  => #(1 2 3 4 5 6)

Parameters:
  less? : procedure -- a two-argument comparison predicate
  to : vector -- destination vector (mutated)
  from1 : vector -- first sorted vector
  from2 : vector -- second sorted vector
  tstart : integer (optional, default 0)
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (vector-length from1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (vector-length from2))
Returns: unspecified
Category: srfi-132
Keywords: merge, combine, sorted, stable, destructive, vector

See also: `vector-merge', `vector-sort!', `list-merge!'."
     (let ((len1 (vector-length from1))
           (len2 (vector-length from2)))
       (when (> (+ len1 len2) (vector-length to))
         (error "vector-merge!: target too small" (vector-length to) (+ len1 len2)))
       (%vector-merge-into! less? to 0
                            from1 0 len1
                            from2 0 len2)))
    ((less? to from1 from2 tstart)
     (let ((len1 (vector-length from1))
           (len2 (vector-length from2)))
       (when (> (+ tstart len1 len2) (vector-length to))
         (error "vector-merge!: target too small" (vector-length to) (+ tstart len1 len2)))
       (%vector-merge-into! less? to tstart
                            from1 0 len1
                            from2 0 len2)))
    ((less? to from1 from2 tstart start1)
     (let ((e1 (vector-length from1))
           (e2 (vector-length from2)))
       (%check-range "vector-merge!" from1 start1 e1)
       (when (> (+ tstart (- e1 start1) e2) (vector-length to))
         (error "vector-merge!: target too small" (vector-length to) (+ tstart (- e1 start1) e2)))
       (%vector-merge-into! less? to tstart
                            from1 start1 e1
                            from2 0 e2)))
    ((less? to from1 from2 tstart start1 end1)
     (let ((e2 (vector-length from2)))
       (%check-range "vector-merge!" from1 start1 end1)
       (when (> (+ tstart (- end1 start1) e2) (vector-length to))
         (error "vector-merge!: target too small" (vector-length to) (+ tstart (- end1 start1) e2)))
       (%vector-merge-into! less? to tstart
                            from1 start1 end1
                            from2 0 e2)))
    ((less? to from1 from2 tstart start1 end1 start2)
     (let ((e2 (vector-length from2)))
       (%check-range "vector-merge!" from1 start1 end1)
       (%check-range "vector-merge!" from2 start2 e2)
       (when (> (+ tstart (- end1 start1) (- e2 start2)) (vector-length to))
         (error "vector-merge!: target too small" (vector-length to) (+ tstart (- end1 start1) (- e2 start2))))
       (%vector-merge-into! less? to tstart
                            from1 start1 end1
                            from2 start2 e2)))
    ((less? to from1 from2 tstart start1 end1 start2 end2)
     (%check-range "vector-merge!" from1 start1 end1)
     (%check-range "vector-merge!" from2 start2 end2)
     (when (> (+ tstart (- end1 start1) (- end2 start2)) (vector-length to))
       (error "vector-merge!: target too small" (vector-length to) (+ tstart (- end1 start1) (- end2 start2))))
     (%vector-merge-into! less? to tstart
                          from1 start1 end1
                          from2 start2 end2))))
