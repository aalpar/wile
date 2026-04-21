;;; (wile algebra matrix) — Semiring-parameterized matrix algebra
;;;
;;; Representation: dense row-major Scheme vector of length rows*cols.
;;; Element (r, c) is at index (+ (* r cols) c).
;;; All operations are parameterized by the semiring stored on the matrix;
;;; binary operations require operand semirings to match by eq? identity.
;;;
;;; Over a general semiring we have + and × but no subtraction; this
;;; rules out Ryser's formula for the permanent (which folds an
;;; inclusion-exclusion with signed terms) and forces direct permutation
;;; enumeration. Kleene closure (M* = I + M + M² + ...) is computed by
;;; iterating T ← I + M·T to fixpoint; the caller specifies a bound to
;;; guard against non-convergent semirings (e.g., counting with cycles).

;; ─── Internal utilities ──────────────────────

(define (smat-iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc) (loop (+ i 1) (cons i acc)))))

;; Remove the first occurrence of X from LST (equal?-based).
(define (smat-remove-first x lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) x) (cdr lst))
    (else (cons (car lst) (smat-remove-first x (cdr lst))))))

;; Fold F over all permutations of LST, threading an accumulator.
;; F has signature (lambda (perm acc) -> new-acc). At most O(n) stack
;; depth and one cons per prefix step — we do NOT materialize the
;; O(n!) permutation list, so consumers such as `semiring-matrix-permanent'
;; run in O(n) working memory rather than O(n! * n).
(define (smat-fold-permutations f init lst)
  (if (null? lst)
      (f '() init)
      (let walk ((remaining lst) (prefix '()) (acc init))
        (if (null? remaining)
            (f (reverse prefix) acc)
            (let loop ((xs remaining) (acc acc))
              (if (null? xs)
                  acc
                  (loop (cdr xs)
                        (walk (smat-remove-first (car xs) remaining)
                              (cons (car xs) prefix)
                              acc))))))))

;; ─── Dense matrix record ─────────────────────

(define-record-type <semiring-matrix>
  (make-semiring-matrix* semiring rows cols data)
  semiring-matrix?
  (semiring smat-semiring)
  (rows     smat-rows)
  (cols     smat-cols)
  (data     smat-data))

(define (smat-flat-index M r c)
  (+ (* r (smat-cols M)) c))

;; ─── Accessors ───────────────────────────────

(define (semiring-matrix-rows M)
  "Return the number of rows in matrix M.\n\nExamples:\n  (semiring-matrix-rows\n    (make-semiring-matrix (counting-semiring) 3 4))  => 3\n\nParameters:\n  M : semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: matrix shape, dimensions, rows, height\n\nSee also: `semiring-matrix-cols', `semiring-matrix-shape'."
  (smat-rows M))

(define (semiring-matrix-cols M)
  "Return the number of columns in matrix M.\n\nExamples:\n  (semiring-matrix-cols\n    (make-semiring-matrix (counting-semiring) 3 4))  => 4\n\nParameters:\n  M : semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: matrix shape, dimensions, columns, width\n\nSee also: `semiring-matrix-rows', `semiring-matrix-shape'."
  (smat-cols M))

(define (semiring-matrix-semiring M)
  "Return the semiring parameter of matrix M.\nOperations on M interpret + and × under this semiring.\n\nExamples:\n  (semiring? (semiring-matrix-semiring\n               (make-semiring-matrix (boolean-semiring) 2 2)))  => #t\n\nParameters:\n  M : semiring-matrix\nReturns: semiring\nCategory: algebra\nKeywords: matrix parameter, semiring, underlying structure\n\nSee also: `make-semiring-matrix'."
  (smat-semiring M))

(define (semiring-matrix-shape M)
  "Return the shape of M as a pair (ROWS . COLS).\n\nExamples:\n  (semiring-matrix-shape\n    (make-semiring-matrix (counting-semiring) 3 4))  => (3 . 4)\n\nParameters:\n  M : semiring-matrix\nReturns: pair\nCategory: algebra\nKeywords: matrix shape, dimensions, size\n\nSee also: `semiring-matrix-rows', `semiring-matrix-cols'."
  (cons (smat-rows M) (smat-cols M)))

(define (semiring-matrix-ref M r c)
  "Return element (R, C) of matrix M.\nRaises an error if the indices are out of bounds.\n\nExamples:\n  (let ((M (semiring-matrix-from-rows (counting-semiring)\n             '((1 2 3) (4 5 6)))))\n    (semiring-matrix-ref M 1 2))  => 6\n\nParameters:\n  M : semiring-matrix\n  r : integer\n  c : integer\nReturns: any\nCategory: algebra\nKeywords: matrix element, indexing, subscript, lookup\n\nSee also: `semiring-matrix-shape', `semiring-matrix->rows'."
  (when (or (< r 0) (>= r (smat-rows M)) (< c 0) (>= c (smat-cols M)))
    (error "semiring-matrix-ref: index out of bounds" r c
           (semiring-matrix-shape M)))
  (vector-ref (smat-data M) (smat-flat-index M r c)))

;; ─── Constructors ────────────────────────────

(define (make-semiring-matrix S rows cols . rest)
  "Construct a ROWS×COLS matrix over semiring S filled with FILL.\nFILL is the optional fourth argument; when omitted, every cell is\ninitialized to (semiring-zero S).\n\nExamples:\n  (semiring-matrix->rows\n    (make-semiring-matrix (counting-semiring) 2 3))\n  => ((0 0 0) (0 0 0))\n  (semiring-matrix->rows\n    (make-semiring-matrix (counting-semiring) 2 2 7))\n  => ((7 7) (7 7))\n\nParameters:\n  S : semiring\n  rows : integer\n  cols : integer\n  [fill] : any\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix constructor, allocate, fill, zero matrix\n\nSee also: `semiring-matrix-from-rows', `semiring-matrix-identity'."
  (when (or (not (integer? rows)) (not (integer? cols))
            (< rows 0) (< cols 0))
    (error "make-semiring-matrix: dimensions must be non-negative integers"
           rows cols))
  (let ((fill (if (null? rest) (semiring-zero S) (car rest))))
    (make-semiring-matrix* S rows cols (make-vector (* rows cols) fill))))

(define (semiring-matrix-from-rows S rows-list)
  "Construct a matrix over semiring S from a list of equal-length rows.\nROWS-LIST must be non-empty; all rows must have the same length.\n\nExamples:\n  (semiring-matrix->rows\n    (semiring-matrix-from-rows (counting-semiring)\n      '((1 2) (3 4) (5 6))))\n  => ((1 2) (3 4) (5 6))\n\nParameters:\n  S : semiring\n  rows-list : list of list\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix literal, build matrix, rows, from-list\n\nSee also: `make-semiring-matrix', `semiring-matrix->rows'."
  (when (null? rows-list)
    (error "semiring-matrix-from-rows: need at least one row"))
  (let* ((n    (length rows-list))
         (m    (length (car rows-list)))
         (data (make-vector (* n m) (semiring-zero S))))
    (let loop-i ((i 0) (rs rows-list))
      (if (null? rs)
          (make-semiring-matrix* S n m data)
          (let ((row (car rs)))
            (unless (= (length row) m)
              (error "semiring-matrix-from-rows: jagged rows" row))
            (let loop-j ((j 0) (cs row))
              (unless (null? cs)
                (vector-set! data (+ (* i m) j) (car cs))
                (loop-j (+ j 1) (cdr cs))))
            (loop-i (+ i 1) (cdr rs)))))))

(define (semiring-matrix->rows M)
  "Convert matrix M back to a list of row lists.\nInverse of `semiring-matrix-from-rows' up to structural equality.\n\nExamples:\n  (semiring-matrix->rows\n    (semiring-matrix-from-rows (counting-semiring)\n      '((1 2) (3 4))))\n  => ((1 2) (3 4))\n\nParameters:\n  M : semiring-matrix\nReturns: list of list\nCategory: algebra\nKeywords: matrix export, to-list, rows, flatten\n\nSee also: `semiring-matrix-from-rows'."
  (let ((n (smat-rows M)) (m (smat-cols M)) (d (smat-data M)))
    (let loop-i ((i 0) (acc '()))
      (if (= i n)
          (reverse acc)
          (let loop-j ((j 0) (row '()))
            (if (= j m)
                (loop-i (+ i 1) (cons (reverse row) acc))
                (loop-j (+ j 1)
                        (cons (vector-ref d (+ (* i m) j)) row))))))))

(define (semiring-matrix-identity S n)
  "Construct the N×N identity matrix over semiring S.\nDiagonal is (semiring-one S); off-diagonal is (semiring-zero S).\nSatisfies I·M = M and M·I = M under `semiring-matrix-mul'.\n\nExamples:\n  (semiring-matrix->rows\n    (semiring-matrix-identity (counting-semiring) 3))\n  => ((1 0 0) (0 1 0) (0 0 1))\n\nParameters:\n  S : semiring\n  n : integer\nReturns: semiring-matrix\nCategory: algebra\nKeywords: identity matrix, unit matrix, I, diagonal, multiplicative identity\n\nSee also: `make-semiring-matrix', `semiring-matrix-mul'."
  (when (or (not (integer? n)) (< n 0))
    (error "semiring-matrix-identity: n must be a non-negative integer" n))
  (let* ((z (semiring-zero S))
         (o (semiring-one S))
         (d (make-vector (* n n) z)))
    (let loop ((i 0))
      (if (= i n)
          (make-semiring-matrix* S n n d)
          (begin
            (vector-set! d (+ (* i n) i) o)
            (loop (+ i 1)))))))

;; ─── Addition ────────────────────────────────

(define (semiring-matrix-add A B)
  "Add matrices A and B elementwise under their (shared) semiring's + operation.\nA and B must have identical shape and share the same semiring by eq?\nidentity; otherwise an error is raised.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))\n    (semiring-matrix->rows (semiring-matrix-add A B)))\n  => ((6 8) (10 12))\n\nParameters:\n  A : semiring-matrix\n  B : semiring-matrix\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix addition, elementwise, add, plus, oplus, sum\n\nSee also: `semiring-matrix-mul', `make-semiring-matrix'."
  (unless (eq? (smat-semiring A) (smat-semiring B))
    (error "semiring-matrix-add: semirings differ"))
  (unless (and (= (smat-rows A) (smat-rows B))
               (= (smat-cols A) (smat-cols B)))
    (error "semiring-matrix-add: shape mismatch"
           (semiring-matrix-shape A) (semiring-matrix-shape B)))
  (let* ((S  (smat-semiring A))
         (n  (smat-rows A))
         (m  (smat-cols A))
         (da (smat-data A))
         (db (smat-data B))
         (len (* n m))
         (dc (make-vector len (semiring-zero S))))
    (let loop ((k 0))
      (if (= k len)
          (make-semiring-matrix* S n m dc)
          (begin
            (vector-set! dc k
              (semiring-plus S (vector-ref da k) (vector-ref db k)))
            (loop (+ k 1)))))))

;; ─── Multiplication ──────────────────────────
;;
;; Schoolbook O(n³) over the shared semiring. Result[i,j] =
;; Σ_k A[i,k] ⊗ B[k,j] where Σ uses semiring-plus and ⊗ is
;; semiring-times. Strassen/Karatsuba not attempted — no benchmark
;; justifies the added complexity, and the constant factor of ring
;; operations dominates the wall-clock cost anyway.

(define (semiring-matrix-mul A B)
  "Multiply matrix A by matrix B under their (shared) semiring.\nRequires A.cols = B.rows and A and B to share a semiring by eq?\nidentity. Uses schoolbook O(n^3) multiplication: C[i,j] =\n(semiring-plus S (semiring-times S A[i,k] B[k,j])) summed over k.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))\n    (semiring-matrix->rows (semiring-matrix-mul A B)))\n  => ((19 22) (43 50))\n\nParameters:\n  A : semiring-matrix\n  B : semiring-matrix\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix multiplication, matmul, schoolbook, tensor, otimes, product\n\nSee also: `semiring-matrix-add', `semiring-matrix-power'."
  (unless (eq? (smat-semiring A) (smat-semiring B))
    (error "semiring-matrix-mul: semirings differ"))
  (unless (= (smat-cols A) (smat-rows B))
    (error "semiring-matrix-mul: A.cols != B.rows"
           (smat-cols A) (smat-rows B)))
  (let* ((S  (smat-semiring A))
         (z  (semiring-zero S))
         (n  (smat-rows A))
         (k* (smat-cols A))       ; inner dimension
         (m  (smat-cols B))
         (da (smat-data A))
         (db (smat-data B))
         (dc (make-vector (* n m) z)))
    (let loop-i ((i 0))
      (if (= i n)
          (make-semiring-matrix* S n m dc)
          (begin
            (let loop-j ((j 0))
              (when (< j m)
                (let loop-k ((k 0) (acc z))
                  (if (= k k*)
                      (vector-set! dc (+ (* i m) j) acc)
                      (loop-k (+ k 1)
                              (semiring-plus S acc
                                (semiring-times S
                                  (vector-ref da (+ (* i k*) k))
                                  (vector-ref db (+ (* k m) j)))))))
                (loop-j (+ j 1))))
            (loop-i (+ i 1)))))))

;; ─── Power (repeated squaring) ───────────────
;;
;; M^0 = I, M^1 = M, otherwise decompose k = 2q + r and use
;; (M²)^q ⊗ M^r. O(log k) matrix multiplications.

(define (semiring-matrix-power M k)
  "Return M^K where K is a non-negative exact integer.\nM must be square. M^0 is the identity matrix; M^1 is M.\nComputed by repeated squaring in O(log K) matrix multiplications.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 1) (0 1)))))\n    (semiring-matrix->rows (semiring-matrix-power M 3)))\n  => ((1 3) (0 1))\n\nParameters:\n  M : semiring-matrix\n  k : integer\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix power, exponentiation, repeated squaring, iteration\n\nSee also: `semiring-matrix-mul', `semiring-matrix-closure'."
  (unless (= (smat-rows M) (smat-cols M))
    (error "semiring-matrix-power: non-square matrix"
           (semiring-matrix-shape M)))
  (unless (and (integer? k) (exact? k) (>= k 0))
    (error "semiring-matrix-power: K must be a non-negative exact integer"
           k))
  (let ((S (smat-semiring M))
        (n (smat-rows M)))
    (cond
      ((= k 0) (semiring-matrix-identity S n))
      ((= k 1) M)
      (else
        ;; Terminate when exp=1 by folding the final base into acc,
        ;; avoiding one wasted (mul base base) at the top of the last
        ;; iteration that the old (= exp 0) termination performed.
        (let loop ((base M)
                   (exp  k)
                   (acc  (semiring-matrix-identity S n)))
          (cond
            ((= exp 1) (semiring-matrix-mul acc base))
            ((odd? exp)
              (loop (semiring-matrix-mul base base)
                    (quotient exp 2)
                    (semiring-matrix-mul acc base)))
            (else
              (loop (semiring-matrix-mul base base)
                    (quotient exp 2)
                    acc))))))))

;; ─── Kleene closure ──────────────────────────
;;
;; M* = I + M + M² + M³ + ...  Computed by iterating
;;   T_0 = I,   T_{k+1} = I + M·T_k
;; which yields T_k = I + M + M² + ... + M^k, detecting convergence
;; when T_{k+1} = T_k. For Boolean or tropical (non-negative weights)
;; semirings on an n×n matrix the series saturates at k = n−1. For
;; counting semirings with cycles the series does not converge; the
;; implementation raises an error when MAX-ITERATIONS is exhausted.

(define (semiring-matrix-closure M . rest)
  "Return the Kleene closure M* = I + M + M^2 + ...\nM must be square. Computed by iterating T_{k+1} <- I + M * T_k until\n(equal? T_k T_{k+1}). Raises an error if convergence is not reached\nwithin MAX-ITERATIONS (optional; defaults to (semiring-matrix-rows M)).\n\nIntended consumers: Boolean semiring (reachability), tropical\nsemiring with non-negative weights (all-pairs shortest paths), and\nany idempotent semiring where the series saturates.\n\nExamples:\n  (let* ((B (boolean-semiring))\n         (G (semiring-matrix-from-rows B\n              '((#f #t #f) (#f #f #t) (#f #f #f)))))\n    (semiring-matrix->rows (semiring-matrix-closure G)))\n  => ((#t #t #t) (#f #t #t) (#f #f #t))\n\nParameters:\n  M : semiring-matrix\n  [max-iterations] : integer\nReturns: semiring-matrix\nCategory: algebra\nKeywords: Kleene closure, transitive closure, reflexive closure, fixpoint, reachability, all-pairs shortest path, star\n\nSee also: `semiring-matrix-power', `semiring-matrix-mul'."
  (unless (= (smat-rows M) (smat-cols M))
    (error "semiring-matrix-closure: non-square matrix"
           (semiring-matrix-shape M)))
  (unless (or (null? rest) (null? (cdr rest)))
    (error "semiring-matrix-closure: expected at most one optional argument"
           rest))
  (let* ((S        (smat-semiring M))
         (n        (smat-rows M))
         (max-iter (if (null? rest) n (car rest)))
         (I        (semiring-matrix-identity S n)))
    (unless (and (integer? max-iter) (exact? max-iter) (>= max-iter 0))
      (error "semiring-matrix-closure: max-iterations must be a non-negative exact integer"
             max-iter))
    ;; Attempt at most max-iter update steps. iter counts completed
    ;; updates, so the guard is (>= iter max-iter) rather than >.
    (let loop ((T I) (iter 0))
      (if (>= iter max-iter)
          (error "semiring-matrix-closure: did not converge" max-iter)
          (let ((T* (semiring-matrix-add I (semiring-matrix-mul M T))))
            (if (equal? (smat-data T) (smat-data T*))
                T
                (loop T* (+ iter 1))))))))

;; ─── Permanent ───────────────────────────────
;;
;; perm(A) = Σ_σ Π_i A[i, σ(i)] over σ ∈ S_n.  We cannot use
;; Ryser's O(n·2^n) formula because it needs additive inverses.
;; Direct enumeration is O(n! · n) — acceptable for the small n
;; that arise in matching / assignment instances, intrinsic cost
;; for larger n over a general semiring.
;;
;; Notable specializations:
;;   Counting semiring    → ordinary matrix permanent
;;   Tropical semiring    → minimum-cost assignment
;;   Boolean  semiring    → existence of a perfect matching
;;   Max-plus  semiring   → maximum-profit assignment

(define (semiring-matrix-permanent M)
  "Return the permanent of square matrix M over its semiring.\nperm(M) = sum over all permutations sigma of {0..n-1} of the\nproduct M[i, sigma(i)]. Enumerated directly — O(n! * n) — because\nRyser's O(n * 2^n) formula requires additive inverses that a\ngeneral semiring does not provide.\n\nSemiring-specific readings:\n  counting → ordinary matrix permanent\n  tropical → minimum-cost perfect assignment\n  Boolean  → #t iff the matrix admits a perfect matching\n\nExamples:\n  (let ((S (counting-semiring)))\n    (semiring-matrix-permanent\n      (semiring-matrix-from-rows S '((1 2) (3 4)))))\n  => 10   ; 1*4 + 2*3\n  (let ((T (tropical-semiring)))\n    (semiring-matrix-permanent\n      (semiring-matrix-from-rows T '((4 1) (2 5)))))\n  => 3    ; min(4+5, 1+2)\n\nParameters:\n  M : semiring-matrix\nReturns: any\nCategory: algebra\nKeywords: matrix permanent, assignment problem, matching, bipartite, perfect matching, cost minimization\n\nSee also: `semiring-matrix-mul', `semiring-matrix-closure'."
  (unless (= (smat-rows M) (smat-cols M))
    (error "semiring-matrix-permanent: non-square matrix"
           (semiring-matrix-shape M)))
  (let* ((S (smat-semiring M))
         (n (smat-rows M))
         (z (semiring-zero S))
         (o (semiring-one S))
         (d (smat-data M)))
    ;; For each permutation sigma, fold the product M[i, sigma(i)]
    ;; into ACC via semiring-plus. The fold visits permutations in
    ;; O(n) working memory; for n=0 the helper invokes f with the
    ;; empty permutation, yielding the conventional perm = 1.
    (smat-fold-permutations
      (lambda (sigma acc)
        (let prod-loop ((i 0) (sig sigma) (p o))
          (if (= i n)
              (semiring-plus S acc p)
              (prod-loop (+ i 1)
                         (cdr sig)
                         (semiring-times S p
                           (vector-ref d (+ (* i n) (car sig))))))))
      z
      (smat-iota n))))

;; ─── Sparse matrix ───────────────────────────
;;
;; ENTRIES is an alist of ((row . col) . value) pairs. Entries whose
;; value equals (semiring-zero S) are omitted from the representation;
;; queries for missing positions return semiring-zero.

(define-record-type <sparse-semiring-matrix>
  (make-sparse-semiring-matrix* semiring rows cols entries)
  sparse-semiring-matrix?
  (semiring ssmat-semiring)
  (rows     ssmat-rows)
  (cols     ssmat-cols)
  (entries  ssmat-entries))

(define (make-sparse-semiring-matrix S rows cols entries)
  "Construct a sparse ROWS x COLS matrix over semiring S from ENTRIES.\nENTRIES is an alist ((ROW . COL) . VALUE). Positions not listed read\nas (semiring-zero S). Entries whose value is (semiring-zero S) are\nstripped from the stored representation (matching the invariant that\nthe sparse form lists only non-zero cells); duplicate coordinates are\nkept as provided, with the first matching entry winning under assoc.\n\nExamples:\n  (let ((S (counting-semiring)))\n    (sparse-semiring-matrix-ref\n      (make-sparse-semiring-matrix S 3 3 '(((0 . 0) . 5) ((1 . 2) . 7)))\n      1 2))\n  => 7\n\nParameters:\n  S : semiring\n  rows : integer\n  cols : integer\n  entries : list\nReturns: sparse-semiring-matrix\nCategory: algebra\nKeywords: sparse matrix, coordinate list, COO, non-zero entries\n\nSee also: `semiring-matrix->sparse', `sparse->semiring-matrix'."
  (when (or (not (integer? rows)) (not (integer? cols))
            (< rows 0) (< cols 0))
    (error "make-sparse-semiring-matrix: dimensions must be non-negative"
           rows cols))
  (let ((z (semiring-zero S)))
    (make-sparse-semiring-matrix* S rows cols
      (let loop ((es entries) (acc '()))
        (cond
          ((null? es) (reverse acc))
          ((equal? (cdar es) z) (loop (cdr es) acc))
          (else (loop (cdr es) (cons (car es) acc))))))))

(define (sparse-semiring-matrix-rows SM)
  "Return the number of rows in sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: sparse matrix, shape, rows"
  (ssmat-rows SM))

(define (sparse-semiring-matrix-cols SM)
  "Return the number of columns in sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: sparse matrix, shape, columns"
  (ssmat-cols SM))

(define (sparse-semiring-matrix-semiring SM)
  "Return the semiring parameter of sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: semiring\nCategory: algebra\nKeywords: sparse matrix, semiring"
  (ssmat-semiring SM))

(define (sparse-semiring-matrix-entries SM)
  "Return the alist of non-zero entries of sparse matrix SM.\nEach entry is ((ROW . COL) . VALUE).\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: list\nCategory: algebra\nKeywords: sparse matrix, entries, non-zero, coordinate list"
  (ssmat-entries SM))

(define (sparse-semiring-matrix-ref SM r c)
  "Return element (R, C) of sparse matrix SM, or (semiring-zero S) if absent.\n\nExamples:\n  (let ((S (counting-semiring)))\n    (sparse-semiring-matrix-ref\n      (make-sparse-semiring-matrix S 2 2 '(((0 . 1) . 9)))\n      0 1))  => 9\n\nParameters:\n  SM : sparse-semiring-matrix\n  r : integer\n  c : integer\nReturns: any\nCategory: algebra\nKeywords: sparse matrix, element, indexing, lookup\n\nSee also: `make-sparse-semiring-matrix'."
  (when (or (< r 0) (>= r (ssmat-rows SM)) (< c 0) (>= c (ssmat-cols SM)))
    (error "sparse-semiring-matrix-ref: index out of bounds" r c))
  (let ((found (assoc (cons r c) (ssmat-entries SM))))
    (if found (cdr found) (semiring-zero (ssmat-semiring SM)))))

(define (semiring-matrix->sparse M)
  "Convert dense matrix M to its sparse representation.\nEntries equal to (semiring-zero S) are omitted. Round-trips via\n`sparse->semiring-matrix' preserve all non-zero values.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 0) (0 2))))\n         (SM (semiring-matrix->sparse M)))\n    (sparse-semiring-matrix-entries SM))\n  => (((0 . 0) . 1) ((1 . 1) . 2))\n\nParameters:\n  M : semiring-matrix\nReturns: sparse-semiring-matrix\nCategory: algebra\nKeywords: dense to sparse, conversion, compression, non-zero filter\n\nSee also: `sparse->semiring-matrix'."
  (let* ((S (smat-semiring M))
         (z (semiring-zero S))
         (n (smat-rows M))
         (m (smat-cols M))
         (d (smat-data M)))
    (let loop-i ((i 0) (acc '()))
      (if (= i n)
          (make-sparse-semiring-matrix* S n m (reverse acc))
          (let loop-j ((j 0) (acc acc))
            (if (= j m)
                (loop-i (+ i 1) acc)
                (let ((v (vector-ref d (+ (* i m) j))))
                  (loop-j (+ j 1)
                          (if (equal? v z)
                              acc
                              (cons (cons (cons i j) v) acc))))))))))

(define (sparse->semiring-matrix SM)
  "Convert sparse matrix SM to its dense representation.\nMissing entries are filled with (semiring-zero S). When duplicate\ncoordinates appear in the sparse entries the first one wins, matching\nthe assoc-based semantics of `sparse-semiring-matrix-ref'. Inverse of\n`semiring-matrix->sparse' up to equality on non-zero positions.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 5)))))\n    (semiring-matrix->rows (sparse->semiring-matrix SM)))\n  => ((5 0) (0 0))\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: semiring-matrix\nCategory: algebra\nKeywords: sparse to dense, conversion, expansion, materialize\n\nSee also: `semiring-matrix->sparse'."
  (let* ((S (ssmat-semiring SM))
         (n (ssmat-rows SM))
         (m (ssmat-cols SM))
         (z (semiring-zero S))
         (d (make-vector (* n m) z)))
    ;; Iterate in reverse so the first entry in the alist writes last
    ;; and therefore wins — matching assoc's first-match semantics in
    ;; sparse-semiring-matrix-ref.
    (for-each
      (lambda (entry)
        (let ((rc (car entry)) (v (cdr entry)))
          (vector-set! d (+ (* (car rc) m) (cdr rc)) v)))
      (reverse (ssmat-entries SM)))
    (make-semiring-matrix* S n m d)))

;; ─── Destructuring macro ─────────────────────

(define-syntax with-semiring-matrix
  (syntax-rules ()
    ((with-semiring-matrix (add mul power closure ref) body ...)
     (let ((add     semiring-matrix-add)
           (mul     semiring-matrix-mul)
           (power   semiring-matrix-power)
           (closure semiring-matrix-closure)
           (ref     semiring-matrix-ref))
       body ...))))
