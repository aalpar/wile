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

;; ─── Path D dispatch scaffold ────────────────
;;
;; Single-file load-order invariant: all polymorphic matrix operations
;; dispatch through *matrix-ops*, an association list populated by
;; register-matrix-op! calls at library load time. Top-level forms in this
;; file execute in source order, so any external caller reaching a
;; dispatcher sees a fully-populated registry. If this library is ever
;; split across files, the split boundary must preserve this ordering or
;; introduce an explicit ensure-registered! guard in each dispatcher.
;;
;; Rationale for an alist rather than a hashtable: Wile's make-hashtable
;; rejects list keys ("key is not hashable"), but dispatch keys are
;; naturally lists like (add dense sparse). At ≤20 entries for Path D's
;; scope, assoc's linear scan is negligible next to any realistic matrix
;; operation. Keys stay readable; semantics stay as designed.

(define *matrix-ops* '())

;; Register IMPL as the dispatcher for KEY. KEY is a list whose first
;; element is the op-symbol (e.g. 'add) and whose remaining elements are
;; rep-tags (e.g. 'dense 'sparse) in argument order.
(define (register-matrix-op! key impl)
  (set! *matrix-ops* (cons (cons key impl) *matrix-ops*)))

;; Return the IMPL registered under KEY, or #f if no entry exists.
;; Dispatchers translate #f into a typed error via their own error path.
(define (matrix-op-lookup key)
  (let ((pair (assoc key *matrix-ops*)))
    (if pair (cdr pair) #f)))

;; *matrix-reps* is the single source of truth for rep enumeration.
;; Adding a new rep (CSR, views, ...) is one (register-matrix-rep! ...)
;; call after the record's define-record-type. matrix? and matrix-rep-tag
;; both derive from this list via the private matrix-rep-of helper, so
;; the two public functions can no longer drift out of sync.
;;
;; Registrations happen as record definitions flow past (same pattern
;; as register-matrix-op! for *matrix-ops*). Top-level form order
;; guarantees all reps are registered by the time any caller reaches a
;; polymorphic dispatcher. No call-site in this library invokes matrix?
;; or matrix-rep-tag at top level; all uses are inside procedure bodies
;; that run after library load completes.
;;
;; register-matrix-rep! prepends, so the LAST registered pred is tested
;; FIRST. Current reps (dense, sparse) have disjoint predicates so order
;; is immaterial. A future rep whose predicate is a subtype of another
;; rep's predicate must register AFTER the supertype to win precedence.

(define *matrix-reps* '())

;; Register PRED as the matrix-rep predicate tagged TAG.
(define (register-matrix-rep! pred tag)
  (set! *matrix-reps* (cons (cons pred tag) *matrix-reps*)))

;; Return the (pred . tag) pair in *matrix-reps* whose pred accepts M,
;; or #f if none matches. Private helper shared by matrix? and
;; matrix-rep-tag so they cannot diverge.
(define (matrix-rep-of M)
  (let loop ((reps *matrix-reps*))
    (cond ((null? reps) #f)
          (((caar reps) M) (car reps))
          (else (loop (cdr reps))))))

(define (matrix? M)
  "Return #t if M is a matrix of any registered representation, #f otherwise.\nDispatches through the shared *matrix-reps* registry with matrix-rep-tag.\n\nExamples:\n  (matrix? (make-semiring-matrix (counting-semiring) 2 2))        => #t\n  (matrix? (make-sparse-semiring-matrix\n             (counting-semiring) 2 2 '()))                         => #t\n  (matrix? 42)                                                     => #f\n\nParameters:\n  M : any\nReturns: boolean\nCategory: algebra\nKeywords: matrix predicate, type check, polymorphic, dense, sparse\n\nSee also: `matrix-rep-tag', `semiring-matrix?', `sparse-semiring-matrix?'."
  (if (matrix-rep-of M) #t #f))

(define (matrix-rep-tag M)
  "Return the representation tag for matrix M.\nRaises an error \"matrix-rep-tag: not a matrix\" if M is not a registered\nmatrix representation. Dispatches through the shared *matrix-reps*\nregistry with matrix?.\n\nExamples:\n  (matrix-rep-tag\n    (make-semiring-matrix (counting-semiring) 2 2))  => dense\n  (matrix-rep-tag\n    (make-sparse-semiring-matrix\n      (counting-semiring) 2 2 '()))                  => sparse\n\nParameters:\n  M : matrix (dense or sparse semiring matrix)\nReturns: symbol ('dense or 'sparse)\nCategory: algebra\nKeywords: matrix representation, dispatch tag, polymorphic, dense, sparse\n\nSee also: `matrix?', `semiring-matrix?', `sparse-semiring-matrix?'."
  (let ((pair (matrix-rep-of M)))
    (if pair
        (cdr pair)
        (error "matrix-rep-tag: not a matrix" M))))

;; ─── Polymorphic iterator API (Path D Q1c) ───

;; Per-representation iterator implementations. Registered into *matrix-ops*
;; immediately below so the public dispatchers reach them through the Path D
;; scaffold rather than re-enumerating reps at the call site. Forward references
;; to record accessors (semiring-matrix-rows, ssmat-entries, ...) resolve at
;; call time, not at define/register time, so load order is not a concern.

(define (matrix-for-each-entry/dense M proc)
  (let ((n (semiring-matrix-rows M))
        (m (semiring-matrix-cols M)))
    (let row-loop ((r 0))
      (when (< r n)
        (let col-loop ((c 0))
          (when (< c m)
            (proc r c (semiring-matrix-ref M r c))
            (col-loop (+ c 1))))
        (row-loop (+ r 1))))))

(define (matrix-for-each-entry/sparse M proc)
  (for-each (lambda (entry)
              (let ((rc (car entry))
                    (v  (cdr entry)))
                (proc (car rc) (cdr rc) v)))
            (ssmat-entries M)))

(define (matrix-fold-entries/dense M init proc)
  (let ((n (semiring-matrix-rows M))
        (m (semiring-matrix-cols M)))
    (let row-loop ((r 0) (acc init))
      (if (= r n)
          acc
          (let col-loop ((c 0) (acc acc))
            (if (= c m)
                (row-loop (+ r 1) acc)
                (col-loop (+ c 1)
                          (proc r c (semiring-matrix-ref M r c) acc))))))))

(define (matrix-fold-entries/sparse M init proc)
  (let loop ((es (ssmat-entries M)) (acc init))
    (if (null? es)
        acc
        (let* ((entry (car es))
               (rc (car entry))
               (v  (cdr entry)))
          (loop (cdr es) (proc (car rc) (cdr rc) v acc))))))

(register-matrix-op! '(for-each dense)  matrix-for-each-entry/dense)
(register-matrix-op! '(for-each sparse) matrix-for-each-entry/sparse)
(register-matrix-op! '(fold     dense)  matrix-fold-entries/dense)
(register-matrix-op! '(fold     sparse) matrix-fold-entries/sparse)

(define (matrix-for-each-entry M proc)
  "Call (PROC ROW COL VALUE) for each entry of matrix M. Returns unspecified.\nDense matrices visit every cell in row-major order. Sparse matrices visit\nonly stored non-zero cells; the enumeration order is representation-dependent\nand not guaranteed stable across reps or releases. Callers that need a\ncanonical order must fold into a structure they sort themselves.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (SM (make-sparse-semiring-matrix S 2 2\n                '(((0 . 0) . 5) ((1 . 1) . 7)))))\n    (matrix-for-each-entry SM\n      (lambda (r c v) (display (list r c v)) (display \" \"))))\n\nParameters:\n  M : matrix\n  proc : procedure of three arguments (row col value)\nReturns: unspecified\nCategory: algebra\nKeywords: matrix iteration, for-each, traversal, entries, visit, scan\n\nSee also: `matrix-fold-entries'."
  (unless (matrix? M) (error "matrix-for-each-entry: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'for-each rep))))
    (if impl
        (impl M proc)
        (error "matrix-for-each-entry: no iterator for representation" rep))))

(define (matrix-fold-entries M init proc)
  "Left fold over the entries of matrix M. PROC is called with\n(ROW COL VALUE ACC) and returns the new ACC. INIT seeds the fold.\nReturns the final accumulator.\n\nDense matrices visit every cell in row-major order; sparse matrices visit\nonly stored non-zero cells in representation-dependent order (see\n`matrix-for-each-entry').\n\nExamples:\n  (let* ((S (counting-semiring))\n         (SM (make-sparse-semiring-matrix S 3 3\n                '(((0 . 0) . 5) ((1 . 2) . 7)))))\n    (matrix-fold-entries SM 0 (lambda (r c v acc) (+ acc 1))))\n  => 2\n\nParameters:\n  M : matrix\n  init : any\n  proc : procedure of four arguments (row col value acc)\nReturns: any\nCategory: algebra\nKeywords: matrix iteration, fold, reduce, accumulate, entries, traverse\n\nSee also: `matrix-for-each-entry'."
  (unless (matrix? M) (error "matrix-fold-entries: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'fold rep))))
    (if impl
        (impl M init proc)
        (error "matrix-fold-entries: no iterator for representation" rep))))

;; ─── Polymorphic accessors (Path D Q2a, P4) ──

;; Per-rep accessor wrappers. These exist so the (register-matrix-op! ...)
;; calls below can name define-bound identifiers; the wrappers' free
;; references to per-rep primitives like sparse-semiring-matrix-ref
;; resolve at call time, after the sparse record section defines them.
;; Same pattern as the P3 iterator impls.

(define (matrix-ref/dense M r c)  (semiring-matrix-ref M r c))
(define (matrix-ref/sparse M r c) (sparse-semiring-matrix-ref M r c))

(define (matrix-rows/dense M)  (semiring-matrix-rows M))
(define (matrix-rows/sparse M) (sparse-semiring-matrix-rows M))

(define (matrix-cols/dense M)  (semiring-matrix-cols M))
(define (matrix-cols/sparse M) (sparse-semiring-matrix-cols M))

(define (matrix-semiring/dense M)  (semiring-matrix-semiring M))
(define (matrix-semiring/sparse M) (sparse-semiring-matrix-semiring M))

(register-matrix-op! '(ref      dense)  matrix-ref/dense)
(register-matrix-op! '(ref      sparse) matrix-ref/sparse)
(register-matrix-op! '(rows     dense)  matrix-rows/dense)
(register-matrix-op! '(rows     sparse) matrix-rows/sparse)
(register-matrix-op! '(cols     dense)  matrix-cols/dense)
(register-matrix-op! '(cols     sparse) matrix-cols/sparse)
(register-matrix-op! '(semiring dense)  matrix-semiring/dense)
(register-matrix-op! '(semiring sparse) matrix-semiring/sparse)

;; Each dispatcher guards its own entry with matrix? so non-matrix input
;; produces a site-tagged "matrix-<op>: not a matrix" error per the D2=(a)
;; convention. Without the guard the error would leak through as
;; "matrix-rep-tag: not a matrix" and lose the caller's site. The "no impl
;; for representation" error path remains for the rare case that
;; matrix-rep-tag gains a new tag ahead of its registrations.

(define (matrix-ref M r c)
  "Return the (R, C) element of matrix M. Dispatches on representation:\ndense reads the stored cell; sparse returns the value if present, else\n(semiring-zero (matrix-semiring M)).\n\nExamples:\n  (matrix-ref (semiring-matrix-from-rows (counting-semiring)\n                '((1 2) (3 4))) 1 0)                              => 3\n\nParameters:\n  M : matrix\n  r : integer\n  c : integer\nReturns: any\nCategory: algebra\nKeywords: matrix element, indexing, polymorphic, subscript, lookup\n\nSee also: `matrix-shape', `matrix-rows', `matrix-cols'."
  (unless (matrix? M) (error "matrix-ref: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'ref rep))))
    (if impl
        (impl M r c)
        (error "matrix-ref: no impl for representation" rep))))

(define (matrix-rows M)
  "Return the number of rows in matrix M. Polymorphic over dense and sparse.\n\nParameters:\n  M : matrix\nReturns: integer\nCategory: algebra\nKeywords: matrix shape, dimensions, rows, height, polymorphic\n\nSee also: `matrix-cols', `matrix-shape'."
  (unless (matrix? M) (error "matrix-rows: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'rows rep))))
    (if impl
        (impl M)
        (error "matrix-rows: no impl for representation" rep))))

(define (matrix-cols M)
  "Return the number of columns in matrix M. Polymorphic over dense and sparse.\n\nParameters:\n  M : matrix\nReturns: integer\nCategory: algebra\nKeywords: matrix shape, dimensions, columns, width, polymorphic\n\nSee also: `matrix-rows', `matrix-shape'."
  (unless (matrix? M) (error "matrix-cols: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'cols rep))))
    (if impl
        (impl M)
        (error "matrix-cols: no impl for representation" rep))))

(define (matrix-shape M)
  "Return the shape of matrix M as a pair (ROWS . COLS). Polymorphic over\ndense and sparse.\n\nExamples:\n  (matrix-shape (make-sparse-semiring-matrix\n                  (counting-semiring) 3 4 '()))                    => (3 . 4)\n\nParameters:\n  M : matrix\nReturns: pair\nCategory: algebra\nKeywords: matrix shape, dimensions, size, polymorphic\n\nSee also: `matrix-rows', `matrix-cols'."
  (unless (matrix? M) (error "matrix-shape: not a matrix" M))
  (cons (matrix-rows M) (matrix-cols M)))

(define (matrix-semiring M)
  "Return the semiring parameter of matrix M. Polymorphic over dense and sparse.\nOperations on M interpret + and × under this semiring.\n\nParameters:\n  M : matrix\nReturns: semiring\nCategory: algebra\nKeywords: matrix parameter, semiring, underlying structure, polymorphic\n\nSee also: `matrix?'."
  (unless (matrix? M) (error "matrix-semiring: not a matrix" M))
  (let* ((rep  (matrix-rep-tag M))
         (impl (matrix-op-lookup (list 'semiring rep))))
    (if impl
        (impl M)
        (error "matrix-semiring: no impl for representation" rep))))

;; ─── Polymorphic arithmetic — add (Path D P5a) ───

;; Result-rep rule for add: the densest operand wins. D+D, D+S, S+D → D;
;; S+S → S. Explicit-conversion principle: add doesn't magically sparsify a
;; dense operand, and mixing a dense operand with a sparse one preserves
;; the dense rep (user-visible cell count unchanged).
(define (matrix-add-result-rep a-tag b-tag)
  (if (and (eq? a-tag 'sparse) (eq? b-tag 'sparse))
      'sparse
      'dense))

;; Validate that A and B are add-compatible: both are matrices, same shape,
;; same semiring (eq?). OP-NAME prefixes each error so attribution reflects
;; the public caller (matrix-add vs. matrix-add!) rather than this helper
;; or the first polymorphic accessor it happens to call.
(define (matrix-add-check-operands op-name A B)
  (unless (matrix? A)
    (error (string-append op-name ": A is not a matrix") A))
  (unless (matrix? B)
    (error (string-append op-name ": B is not a matrix") B))
  (unless (eq? (matrix-semiring A) (matrix-semiring B))
    (error (string-append op-name ": semirings differ")))
  (unless (equal? (matrix-shape A) (matrix-shape B))
    (error (string-append op-name ": shape mismatch")
           (matrix-shape A) (matrix-shape B))))

;; Allocate an empty matrix of given REP. Dense is zero-initialized;
;; sparse starts with no stored entries.
(define (matrix-allocate rep S rows cols)
  (case rep
    ((dense)  (make-semiring-matrix S rows cols))
    ((sparse) (make-sparse-semiring-matrix S rows cols '()))
    (else (error "matrix-allocate: unknown rep" rep))))

;; ── Private add! kernels ──

;; Dense ← Dense + Dense. In-place write to C's data vector.
(define (matrix-add!/dense/dense/dense C A B)
  (let* ((S (smat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (size (* (smat-rows A) (smat-cols A)))
         (dc (smat-data C))
         (da (smat-data A))
         (db (smat-data B)))
    (let loop ((k 0))
      (when (< k size)
        (vector-set! dc k (plus (vector-ref da k) (vector-ref db k)))
        (loop (+ k 1)))))
  C)

;; Dense ← Dense + Sparse. Copy A into C, then add each stored B entry.
(define (matrix-add!/dense/dense/sparse C A B)
  (let* ((S (smat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (m (smat-cols A))
         (size (* (smat-rows A) m))
         (dc (smat-data C))
         (da (smat-data A)))
    (let loop ((k 0))
      (when (< k size)
        (vector-set! dc k (vector-ref da k))
        (loop (+ k 1))))
    (for-each (lambda (entry)
                (let* ((rc (car entry))
                       (k  (+ (* (car rc) m) (cdr rc))))
                  (vector-set! dc k (plus (vector-ref dc k) (cdr entry)))))
              (ssmat-entries B)))
  C)

;; Dense ← Sparse + Dense. Commutativity: delegate with A/B swapped.
(define (matrix-add!/dense/sparse/dense C A B)
  (matrix-add!/dense/dense/sparse C B A))

;; Sparse ← Sparse + Sparse. Merge the two alists; strip zero-valued results.
(define (matrix-add!/sparse/sparse/sparse C A B)
  (let* ((S (ssmat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (zero (semiring-zero S))
         (ea (ssmat-entries A))
         (eb (ssmat-entries B))
         ;; For each entry in A, combine with B's matching entry (or zero).
         (merged-a
          (let loop ((es ea) (acc '()))
            (if (null? es)
                acc
                (let* ((entry (car es))
                       (k (car entry))
                       (va (cdr entry))
                       (b-pair (assoc k eb))
                       (vb (if b-pair (cdr b-pair) zero))
                       (sum (plus va vb)))
                  (if (equal? sum zero)
                      (loop (cdr es) acc)
                      (loop (cdr es) (cons (cons k sum) acc)))))))
         ;; Add B entries whose keys aren't in A.
         (b-only
          (let loop ((es eb) (acc '()))
            (if (null? es)
                acc
                (let* ((entry (car es))
                       (k (car entry)))
                  (if (assoc k ea)
                      (loop (cdr es) acc)
                      (if (equal? (cdr entry) zero)
                          (loop (cdr es) acc)
                          (loop (cdr es) (cons entry acc)))))))))
    ;; Reverse both accumulators so the output preserves each input's
    ;; traversal order. This matters when callers pass inputs with
    ;; duplicate coordinates (documented edge case at make-sparse-
    ;; semiring-matrix): assoc first-match on the result then reflects
    ;; A's first-matching entry, not the last-processed one.
    (ssmat-entries-set! C (append (reverse merged-a) (reverse b-only))))
  C)

;; ── Public dispatchers ──

(define (matrix-add! C A B)
  "Matrix addition in place. Writes C[i,j] = A[i,j] + B[i,j] under the shared\nsemiring. Dispatches on (C-rep, A-rep, B-rep). C must have the rep expected\nfrom A+B per the result-rep rule (OQ4 strict): D+D/D+S/S+D yield dense; S+S\nyields sparse. Any aliasing is legal (no-hazard class per OQ5), so\n(matrix-add! A A B) is the idiomatic A += B.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8))))\n         (C (make-semiring-matrix S 2 2)))\n    (matrix-add! C A B)\n    (semiring-matrix->rows C))\n  => ((6 8) (10 12))\n\nParameters:\n  C : matrix (destination)\n  A : matrix\n  B : matrix\nReturns: C\nCategory: algebra\nKeywords: matrix addition, elementwise, add, plus, in-place, destructive\n\nSee also: `matrix-add'."
  (matrix-add-check-operands "matrix-add!" A B)
  (unless (matrix? C)
    (error "matrix-add!: destination is not a matrix" C))
  (unless (equal? (matrix-shape C) (matrix-shape A))
    (error "matrix-add!: destination shape mismatch"
           (matrix-shape C) (matrix-shape A)))
  (unless (eq? (matrix-semiring C) (matrix-semiring A))
    (error "matrix-add!: destination semiring differs from operands"))
  (let* ((a-tag (matrix-rep-tag A))
         (b-tag (matrix-rep-tag B))
         (c-tag (matrix-rep-tag C))
         (expected (matrix-add-result-rep a-tag b-tag)))
    (unless (eq? c-tag expected)
      (error "matrix-add!: destination rep does not match expected result rep"
             c-tag expected))
    (let ((impl (matrix-op-lookup (list 'add! c-tag a-tag b-tag))))
      (unless impl
        (error "matrix-add!: unsupported rep combination"
               c-tag a-tag b-tag))
      (impl C A B))))

(define (matrix-add A B)
  "Matrix addition. Returns a new matrix where C[i,j] = A[i,j] + B[i,j].\nResult rep: D+D / D+S / S+D → dense; S+S → sparse.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))\n    (semiring-matrix->rows (matrix-add A B)))\n  => ((6 8) (10 12))\n\nParameters:\n  A : matrix\n  B : matrix\nReturns: matrix\nCategory: algebra\nKeywords: matrix addition, elementwise, add, plus, sum, oplus\n\nSee also: `matrix-add!'."
  (matrix-add-check-operands "matrix-add" A B)
  (let* ((result-rep (matrix-add-result-rep (matrix-rep-tag A)
                                            (matrix-rep-tag B)))
         (C (matrix-allocate result-rep (matrix-semiring A)
                             (matrix-rows A) (matrix-cols A))))
    (matrix-add! C A B)
    C))

;; ── Registrations ──

(register-matrix-op! '(add! dense  dense  dense)  matrix-add!/dense/dense/dense)
(register-matrix-op! '(add! dense  dense  sparse) matrix-add!/dense/dense/sparse)
(register-matrix-op! '(add! dense  sparse dense)  matrix-add!/dense/sparse/dense)
(register-matrix-op! '(add! sparse sparse sparse) matrix-add!/sparse/sparse/sparse)

;; ─── Polymorphic arithmetic — mul (Path D P5b) ───

;; Result-rep rule for mul: S×S → S; all other combinations → D. Rationale
;; follows the sparse-BLAS algorithm family: sparse-dense multiply uses a
;; scatter-into-dense kernel, which naturally produces a dense result. Only
;; S×S preserves sparsity through the operation (via by-entry accumulation).
(define (matrix-mul-result-rep a-tag b-tag)
  (if (and (eq? a-tag 'sparse) (eq? b-tag 'sparse))
      'sparse
      'dense))

;; Validate that A and B are mul-compatible: both are matrices, shape
;; (A.cols == B.rows), and shared semiring (eq?). OP-NAME prefixes each
;; error so attribution reflects the public caller (matrix-mul vs.
;; matrix-mul!) rather than this helper or the first polymorphic accessor
;; it happens to call.
(define (matrix-mul-check-operands op-name A B)
  (unless (matrix? A)
    (error (string-append op-name ": A is not a matrix") A))
  (unless (matrix? B)
    (error (string-append op-name ": B is not a matrix") B))
  (unless (eq? (matrix-semiring A) (matrix-semiring B))
    (error (string-append op-name ": semirings differ")))
  (unless (= (matrix-cols A) (matrix-rows B))
    (error (string-append op-name ": inner dimensions disagree")
           (matrix-shape A) (matrix-shape B))))

;; ── Private mul! kernels ──

;; Dense ← Dense × Dense. Schoolbook O(n·m·p); accumulates into a local per
;; cell then writes once to C — C may not alias A or B (incremental-write
;; hazard, enforced by the dispatcher).
(define (matrix-mul!/dense/dense/dense C A B)
  (let* ((S (smat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (times (lambda (a b) (semiring-times S a b)))
         (zero (semiring-zero S))
         (n (smat-rows A))
         (p (smat-cols A))
         (m (smat-cols B))
         (da (smat-data A))
         (db (smat-data B))
         (dc (smat-data C)))
    (let i-loop ((i 0))
      (when (< i n)
        (let j-loop ((j 0))
          (when (< j m)
            (let k-loop ((k 0) (acc zero))
              (if (= k p)
                  (vector-set! dc (+ (* i m) j) acc)
                  (k-loop (+ k 1)
                          (plus acc (times (vector-ref da (+ (* i p) k))
                                           (vector-ref db (+ (* k m) j)))))))
            (j-loop (+ j 1))))
        (i-loop (+ i 1)))))
  C)

;; Dense ← Sparse × Dense. Scatter: C starts at zero; for each A entry
;; (i,k,v), accumulate v·B[k,j] into C[i,j] for all j.
(define (matrix-mul!/dense/sparse/dense C A B)
  (let* ((S (ssmat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (times (lambda (a b) (semiring-times S a b)))
         (zero (semiring-zero S))
         (m (smat-cols B))
         (n (ssmat-rows A))
         (dc (smat-data C))
         (db (smat-data B))
         (size (* n m)))
    ;; Zero-init C's data.
    (let loop ((k 0))
      (when (< k size)
        (vector-set! dc k zero)
        (loop (+ k 1))))
    ;; For each stored entry of A, scatter across B's row.
    (for-each (lambda (entry)
                (let* ((rc (car entry))
                       (i (car rc))
                       (k (cdr rc))
                       (v (cdr entry)))
                  (let j-loop ((j 0))
                    (when (< j m)
                      (let* ((dc-idx (+ (* i m) j))
                             (prod (times v (vector-ref db (+ (* k m) j)))))
                        (vector-set! dc dc-idx (plus (vector-ref dc dc-idx) prod)))
                      (j-loop (+ j 1))))))
              (ssmat-entries A)))
  C)

;; Dense ← Dense × Sparse. Transpose-scatter: for each B entry (k,j,v),
;; accumulate A[i,k]·v into C[i,j] for all i.
(define (matrix-mul!/dense/dense/sparse C A B)
  (let* ((S (smat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (times (lambda (a b) (semiring-times S a b)))
         (zero (semiring-zero S))
         (n (smat-rows A))
         (p (smat-cols A))
         (m (ssmat-cols B))
         (da (smat-data A))
         (dc (smat-data C))
         (size (* n m)))
    (let loop ((k 0))
      (when (< k size)
        (vector-set! dc k zero)
        (loop (+ k 1))))
    (for-each (lambda (entry)
                (let* ((rc (car entry))
                       (k (car rc))
                       (j (cdr rc))
                       (v (cdr entry)))
                  (let i-loop ((i 0))
                    (when (< i n)
                      (let* ((dc-idx (+ (* i m) j))
                             (prod (times (vector-ref da (+ (* i p) k)) v)))
                        (vector-set! dc dc-idx (plus (vector-ref dc dc-idx) prod)))
                      (i-loop (+ i 1))))))
              (ssmat-entries B)))
  C)

;; Sparse ← Sparse × Sparse. For each A entry (i,k,va), iterate B entries and
;; accumulate products where B's row-index equals k. Accumulator is a local
;; alist keyed on (i,j); once built, strip zero-valued results and assign.
(define (matrix-mul!/sparse/sparse/sparse C A B)
  (let* ((S (ssmat-semiring A))
         (plus (lambda (a b) (semiring-plus S a b)))
         (times (lambda (a b) (semiring-times S a b)))
         (zero (semiring-zero S))
         (ea (ssmat-entries A))
         (eb (ssmat-entries B)))
    ;; Build result alist by iterating A and cross-checking B. When the
    ;; (i,j) coord is already present, mutate the existing pair's cdr in
    ;; place — assoc returns the actual pair from acc, so set-cdr! is
    ;; O(1) per update vs. O(nnz_acc) for a list rebuild via map. Safe
    ;; here because acc is freshly allocated inside this function and
    ;; never escapes (ssmat-entries-set! receives the stripped result,
    ;; not the accumulator itself).
    (define (accum-into acc i j v)
      (let ((existing (assoc (cons i j) acc)))
        (if existing
            (begin
              (set-cdr! existing (plus (cdr existing) v))
              acc)
            (cons (cons (cons i j) v) acc))))
    (let a-loop ((ra ea) (acc '()))
      (if (null? ra)
          (ssmat-entries-set! C
            ;; Strip zeros to preserve the sparse invariant.
            (let strip ((es acc) (out '()))
              (cond
                ((null? es) (reverse out))
                ((equal? (cdar es) zero) (strip (cdr es) out))
                (else (strip (cdr es) (cons (car es) out))))))
          (let* ((a-entry (car ra))
                 (a-rc (car a-entry))
                 (i (car a-rc))
                 (k (cdr a-rc))
                 (va (cdr a-entry)))
            (let b-loop ((rb eb) (acc acc))
              (if (null? rb)
                  (a-loop (cdr ra) acc)
                  (let* ((b-entry (car rb))
                         (b-rc (car b-entry))
                         (bk (car b-rc))
                         (j (cdr b-rc))
                         (vb (cdr b-entry)))
                    (if (= k bk)
                        (b-loop (cdr rb) (accum-into acc i j (times va vb)))
                        (b-loop (cdr rb) acc)))))))))
  C)

;; ── Public dispatchers ──

(define (matrix-mul! C A B)
  "Matrix multiplication in place. Writes C[i,j] = Σ_k A[i,k] ⊗ B[k,j] under\nthe shared semiring. Dispatches on (C-rep, A-rep, B-rep). C must have the\nrep expected from A×B per the result-rep rule (OQ4 strict): D×D, D×S, S×D\nyield dense; S×S yields sparse. C must NOT alias A or B (incremental-write\nhazard class per OQ5 — every destination cell depends on A/B cells that\nhave not yet been overwritten; self-aliasing would corrupt).\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8))))\n         (C (make-semiring-matrix S 2 2)))\n    (matrix-mul! C A B)\n    (semiring-matrix->rows C))\n  => ((19 22) (43 50))\n\nParameters:\n  C : matrix (destination, must not eq? alias A or B)\n  A : matrix\n  B : matrix\nReturns: C\nCategory: algebra\nKeywords: matrix multiplication, matmul, in-place, destructive, schoolbook\n\nSee also: `matrix-mul', `matrix-add!'."
  (matrix-mul-check-operands "matrix-mul!" A B)
  (unless (matrix? C)
    (error "matrix-mul!: destination is not a matrix" C))
  (unless (and (= (matrix-rows C) (matrix-rows A))
               (= (matrix-cols C) (matrix-cols B)))
    (error "matrix-mul!: destination shape mismatch; expected"
           (cons (matrix-rows A) (matrix-cols B))
           "got" (matrix-shape C)))
  (unless (eq? (matrix-semiring C) (matrix-semiring A))
    (error "matrix-mul!: destination semiring differs from operands"))
  ;; OQ5 incremental-write: forbid eq? overlap between dest and any operand.
  (when (or (eq? C A) (eq? C B))
    (error "matrix-mul!: destination cannot alias operand; use a scratch matrix or rebind to (matrix-mul A B)"))
  (let* ((a-tag (matrix-rep-tag A))
         (b-tag (matrix-rep-tag B))
         (c-tag (matrix-rep-tag C))
         (expected (matrix-mul-result-rep a-tag b-tag)))
    (unless (eq? c-tag expected)
      (error "matrix-mul!: destination rep does not match expected result rep"
             c-tag expected))
    (let ((impl (matrix-op-lookup (list 'mul! c-tag a-tag b-tag))))
      (unless impl
        (error "matrix-mul!: unsupported rep combination"
               c-tag a-tag b-tag))
      (impl C A B))))

(define (matrix-mul A B)
  "Matrix multiplication. Returns a new matrix C where C[i,j] = Σ_k A[i,k] ⊗\nB[k,j] under the shared semiring. Result rep: D×D / D×S / S×D → dense;\nS×S → sparse.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (A (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))\n    (semiring-matrix->rows (matrix-mul A B)))\n  => ((19 22) (43 50))\n\nParameters:\n  A : matrix\n  B : matrix\nReturns: matrix\nCategory: algebra\nKeywords: matrix multiplication, matmul, tensor, product, otimes, schoolbook\n\nSee also: `matrix-mul!', `matrix-add'."
  (matrix-mul-check-operands "matrix-mul" A B)
  (let* ((result-rep (matrix-mul-result-rep (matrix-rep-tag A)
                                            (matrix-rep-tag B)))
         (C (matrix-allocate result-rep (matrix-semiring A)
                             (matrix-rows A) (matrix-cols B))))
    (matrix-mul! C A B)
    C))

(register-matrix-op! '(mul! dense  dense  dense)  matrix-mul!/dense/dense/dense)
(register-matrix-op! '(mul! dense  dense  sparse) matrix-mul!/dense/dense/sparse)
(register-matrix-op! '(mul! dense  sparse dense)  matrix-mul!/dense/sparse/dense)
(register-matrix-op! '(mul! sparse sparse sparse) matrix-mul!/sparse/sparse/sparse)

;; ─── Capability predicate (Path D P6, OQ3) ───

;; Pure-form registrations: data-driven marker entries so matrix-op-supported?
;; answers with one table lookup regardless of pure vs bang. Value is the
;; dispatcher function itself, for introspection tools that might want to
;; find the entry point. The allocator-and-dispatch logic lives in
;; matrix-add / matrix-mul; these entries just record "supported".
(register-matrix-op! '(add dense  dense)  matrix-add)
(register-matrix-op! '(add dense  sparse) matrix-add)
(register-matrix-op! '(add sparse dense)  matrix-add)
(register-matrix-op! '(add sparse sparse) matrix-add)
(register-matrix-op! '(mul dense  dense)  matrix-mul)
(register-matrix-op! '(mul dense  sparse) matrix-mul)
(register-matrix-op! '(mul sparse dense)  matrix-mul)
(register-matrix-op! '(mul sparse sparse) matrix-mul)

;; ─── Dense-only polymorphic ops (Path D P7) ─

;; Dispatcher for ops that currently admit only a dense implementation.
;; Looks up (op rep-tag M); if missing, raises with OQ3's typed-error
;; message advising the matrix-op-supported? capability query. OP-NAME is
;; the public-caller name ("matrix-power" etc.) used for error attribution;
;; OP is the dispatch-table key symbol ('power, 'closure, ...). The
;; register-matrix-op! entries keyed on (op dense) route to the existing
;; semiring-matrix-* implementations; non-dense operands hit the
;; not-registered branch and raise.
(define (matrix-dense-only-op op-name op . args)
  (let ((M (car args)))
    (unless (matrix? M)
      (error (string-append op-name ": not a matrix") M))
    (let* ((tag (matrix-rep-tag M))
           (impl (matrix-op-lookup (cons op (list tag)))))
      (if impl
          (apply impl args)
          (error (string-append
                   op-name
                   ": "
                   (symbol->string tag)
                   " operand not supported; check "
                   "(matrix-op-supported? '" (symbol->string op) " M)")
                 tag)))))

(define (matrix-power M k)
  "Return M^K. Dispatches on M's representation.\nM must be square. K is a non-negative exact integer; M^0 is the identity\nmatrix; M^1 is M. Computed by repeated squaring in O(log K) multiplications.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 1) (0 1)))))\n    (semiring-matrix->rows (matrix-power M 3)))\n  => ((1 3) (0 1))\n\nUnsupported: sparse matrices — convert via (sparse->semiring-matrix M)\nfirst, or check (matrix-op-supported? 'power M) before calling.\n\nParameters:\n  M : matrix (dense)\n  k : integer\nReturns: matrix\nCategory: algebra\nKeywords: matrix power, exponentiation, repeated squaring, polymorphic\n\nSee also: `matrix-mul', `matrix-closure', `matrix-op-supported?'."
  (matrix-dense-only-op "matrix-power" 'power M k))

(define (matrix-closure M . opt)
  "Return the Kleene closure M* = I + M + M^2 + ... of matrix M.\nDispatches on M's representation. M must be square. Optional argument\nMAX-ITERATIONS bounds the fixpoint search (defaults to (matrix-rows M)).\n\nExamples:\n  (let* ((B (boolean-semiring))\n         (G (semiring-matrix-from-rows B\n              '((#f #t #f) (#f #f #t) (#f #f #f)))))\n    (semiring-matrix->rows (matrix-closure G)))\n  => ((#t #t #t) (#f #t #t) (#f #f #t))\n\nUnsupported: sparse matrices — convert via (sparse->semiring-matrix M)\nfirst, or check (matrix-op-supported? 'closure M) before calling.\n\nParameters:\n  M : matrix (dense)\n  [max-iterations] : integer\nReturns: matrix\nCategory: algebra\nKeywords: Kleene closure, transitive closure, reflexive closure, fixpoint, polymorphic\n\nSee also: `matrix-power', `matrix-op-supported?'."
  (apply matrix-dense-only-op "matrix-closure" 'closure M opt))

(define (matrix-permanent M)
  "Return the permanent of square matrix M under its semiring.\nDispatches on M's representation. Direct O(n!) permutation enumeration;\nRyser's formula is impossible over a general semiring (no subtraction).\n\nExamples:\n  (matrix-permanent (semiring-matrix-from-rows (counting-semiring)\n                     '((1 2) (3 4))))\n  => 10  ; 1*4 + 2*3\n\nUnsupported: sparse matrices — convert via (sparse->semiring-matrix M)\nfirst, or check (matrix-op-supported? 'permanent M) before calling.\n\nParameters:\n  M : matrix (dense)\nReturns: any\nCategory: algebra\nKeywords: matrix permanent, combinatorial, permutation sum, polymorphic\n\nSee also: `matrix-power', `matrix-op-supported?'."
  (matrix-dense-only-op "matrix-permanent" 'permanent M))

;; Registrations for the dense-only ops live at the bottom of the file,
;; where the semiring-matrix-* implementations are already defined (top-
;; level forms execute in source order; referencing a not-yet-bound
;; identifier as an IMPL value would fail at load time).

;; ─── matrix-copy / matrix-copy! (Path D P8) ──

;; Kernels — private. Same-rep only; matrix-copy! enforces this.
(define (matrix-copy!/dense/dense C M)
  (let* ((size (* (smat-rows M) (smat-cols M)))
         (dc (smat-data C))
         (dm (smat-data M)))
    (let loop ((k 0))
      (when (< k size)
        (vector-set! dc k (vector-ref dm k))
        (loop (+ k 1)))))
  C)

(define (matrix-copy!/sparse/sparse C M)
  ;; Replace C's entries with a shallow copy of M's; values share references.
  (ssmat-entries-set! C (map (lambda (e) (cons (car e) (cdr e)))
                             (ssmat-entries M)))
  C)

(define (matrix-copy! C M)
  "Copy matrix M into C in place. C and M must share rep, shape, and semiring\n(eq?). Returns C. OQ5 aliasing: trivially safe when (eq? C M) because every\ncell is read then written with the same value, but matrix-copy! rejects\n(eq? C M) as a no-op the user almost certainly didn't mean.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (C (make-semiring-matrix S 2 2)))\n    (matrix-copy! C M)\n    (semiring-matrix->rows C))\n  => ((1 2) (3 4))\n\nParameters:\n  C : matrix (destination)\n  M : matrix\nReturns: C\nCategory: algebra\nKeywords: matrix copy, clone, duplicate, in-place, destructive\n\nSee also: `matrix-copy', `matrix-add!'."
  (unless (matrix? C)
    (error "matrix-copy!: destination is not a matrix" C))
  (unless (matrix? M)
    (error "matrix-copy!: source is not a matrix" M))
  (when (eq? C M)
    (error "matrix-copy!: source and destination alias; this is a no-op and probably a bug"))
  (unless (equal? (matrix-shape C) (matrix-shape M))
    (error "matrix-copy!: shape mismatch" (matrix-shape C) (matrix-shape M)))
  (unless (eq? (matrix-semiring C) (matrix-semiring M))
    (error "matrix-copy!: semirings differ"))
  (let ((c-tag (matrix-rep-tag C))
        (m-tag (matrix-rep-tag M)))
    (unless (eq? c-tag m-tag)
      (error "matrix-copy!: rep mismatch" c-tag m-tag))
    (let ((impl (matrix-op-lookup (list 'copy! c-tag m-tag))))
      (unless impl
        (error "matrix-copy!: unsupported rep" c-tag))
      (impl C M))))

(define (matrix-copy M)
  "Return a fresh matrix with the same rep, shape, semiring, and contents as M.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 2) (3 4))))\n         (C (matrix-copy M)))\n    (eq? M C))\n  => #f\n\nParameters:\n  M : matrix\nReturns: matrix\nCategory: algebra\nKeywords: matrix copy, clone, duplicate, allocate\n\nSee also: `matrix-copy!'."
  (unless (matrix? M)
    (error "matrix-copy: not a matrix" M))
  (let* ((rep (matrix-rep-tag M))
         (C (matrix-allocate rep (matrix-semiring M)
                             (matrix-rows M) (matrix-cols M))))
    (matrix-copy! C M)
    C))

(register-matrix-op! '(copy! dense  dense)  matrix-copy!/dense/dense)
(register-matrix-op! '(copy! sparse sparse) matrix-copy!/sparse/sparse)
(register-matrix-op! '(copy  dense)  matrix-copy)
(register-matrix-op! '(copy  sparse) matrix-copy)

(define (matrix-op-supported? op . args)
  "Return #t iff every ARG is a matrix and the dispatch table has a kernel\nregistered under (OP . ARGS' rep-tags); #f otherwise. Symbol-based (OP is a\nScheme symbol like 'add, 'add!, 'mul, etc.). For pure binary ops 'add and\n'mul, every rep combination is registered. For bang forms, the destination\nrep must match the expected result rep per OQ4. Unary ops registered on\nboth reps: 'copy (P8). Unary ops currently dense-only: 'power, 'closure,\n'permanent (P7). 'copy! requires destination and source to share rep.\n\nThis is a representation-level capability query, not a call-validity check.\n#t means a kernel exists for those reps; it does NOT promise the operation\nwill succeed. Shape compatibility, semiring-identity, and per-op runtime\nconstraints (e.g. `matrix-mul!` forbidding destination/operand aliasing per\nOQ5) are still checked by the operation itself and may raise on invocation.\n\nThis is the programmatic capability query OQ3 promised — callers can branch\non kernel availability rather than catching errors for missing reps:\n\n  (if (matrix-op-supported? 'permanent M)\n      (matrix-permanent M)\n      (matrix-permanent (sparse->semiring-matrix M)))\n\nReturns #f (rather than raising) when an ARG is not a matrix — the predicate\nis safe to call on any value.\n\nExamples:\n  (matrix-op-supported? 'add A B)           ; => #t for valid matrices A, B\n  (matrix-op-supported? 'add! C A B)        ; => #t if C's rep matches\n  (matrix-op-supported? 'mul  A 42)         ; => #f (non-matrix)\n\nParameters:\n  op : symbol\n  args : matrices\nReturns: boolean\nCategory: algebra\nKeywords: matrix capability, support query, dispatch, introspection\n\nSee also: `matrix-rep-tag', `matrix-add', `matrix-mul'."
  (define (all-matrices? xs)
    (cond ((null? xs) #t)
          ((not (matrix? (car xs))) #f)
          (else (all-matrices? (cdr xs)))))
  (and (all-matrices? args)
       (matrix-op-lookup (cons op (map matrix-rep-tag args)))
       #t))

;; ─── Internal utilities ──────────────────────

;; Validate that X is a non-negative exact integer; raise an error
;; tagged with WHERE (the caller's name) and WHAT (the parameter role)
;; otherwise. Centralizes a rule that was previously expressed two
;; different ways across construction/power/closure.
(define (smat-check-nat where what x)
  (unless (and (integer? x) (exact? x) (>= x 0))
    (error (string-append where ": " what
                          " must be a non-negative exact integer")
           x)))

(define (smat-iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc) (loop (+ i 1) (cons i acc)))))

;; Remove the first occurrence of X from LST (equal?-based).
;; Used only by smat-fold-permutations — which passes lists of
;; distinct integers produced by smat-iota. If LST contains
;; duplicates the permutation walker will overcount: e.g.
;; (smat-iota 3) yields (0 1 2) and 3! = 6 permutations, but
;; '(0 0 1) would yield 6 orderings where only 3 are distinct.
;; Callers that want set-of-permutations semantics over a multiset
;; must deduplicate before calling.
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
;; Precondition (see smat-remove-first): LST must have distinct
;; elements for the fold to visit each permutation exactly once.
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

(register-matrix-rep! semiring-matrix? 'dense)

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
  (smat-check-nat "semiring-matrix-ref" "row" r)
  (smat-check-nat "semiring-matrix-ref" "col" c)
  (when (or (>= r (smat-rows M)) (>= c (smat-cols M)))
    (error "semiring-matrix-ref: index out of bounds" r c
           (semiring-matrix-shape M)))
  (vector-ref (smat-data M) (+ (* r (smat-cols M)) c)))

;; ─── Constructors ────────────────────────────

(define (make-semiring-matrix S rows cols . rest)
  "Construct a ROWS×COLS matrix over semiring S filled with FILL.\nFILL is the optional fourth argument; when omitted, every cell is\ninitialized to (semiring-zero S).\n\nExamples:\n  (semiring-matrix->rows\n    (make-semiring-matrix (counting-semiring) 2 3))\n  => ((0 0 0) (0 0 0))\n  (semiring-matrix->rows\n    (make-semiring-matrix (counting-semiring) 2 2 7))\n  => ((7 7) (7 7))\n\nParameters:\n  S : semiring\n  rows : integer\n  cols : integer\n  [fill] : any\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix constructor, allocate, fill, zero matrix\n\nSee also: `semiring-matrix-from-rows', `semiring-matrix-identity'."
  (unless (semiring? S)
    (error "make-semiring-matrix: expected semiring" S))
  (smat-check-nat "make-semiring-matrix" "rows" rows)
  (smat-check-nat "make-semiring-matrix" "cols" cols)
  (let ((fill (if (null? rest) (semiring-zero S) (car rest))))
    (make-semiring-matrix* S rows cols (make-vector (* rows cols) fill))))

(define (semiring-matrix-from-rows S rows-list)
  "Construct a matrix over semiring S from a list of equal-length rows.\nROWS-LIST must be non-empty; all rows must have the same length.\n\nExamples:\n  (semiring-matrix->rows\n    (semiring-matrix-from-rows (counting-semiring)\n      '((1 2) (3 4) (5 6))))\n  => ((1 2) (3 4) (5 6))\n\nParameters:\n  S : semiring\n  rows-list : list of list\nReturns: semiring-matrix\nCategory: algebra\nKeywords: matrix literal, build matrix, rows, from-list\n\nSee also: `make-semiring-matrix', `semiring-matrix->rows'."
  (unless (semiring? S)
    (error "semiring-matrix-from-rows: expected semiring" S))
  (when (null? rows-list)
    (error "semiring-matrix-from-rows: need at least one row"))
  ;; The first row's length determines the column count; if it isn't
  ;; a list the subsequent (length (car rows-list)) blows up with
  ;; "length: contract violation" far from the real mistake. Common
  ;; mistake this catches: forgetting the outer parens around the
  ;; first row, e.g. (from-rows S '(5 (1 2))).
  (unless (list? (car rows-list))
    (error "semiring-matrix-from-rows: each row must be a list"
           (car rows-list)))
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
  (unless (semiring? S)
    (error "semiring-matrix-identity: expected semiring" S))
  (smat-check-nat "semiring-matrix-identity" "n" n)
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
  (smat-check-nat "semiring-matrix-power" "k" k)
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
;;
;; Convergence is detected via `equal?` on the underlying vectors.
;; This imposes a constraint the record type cannot express: the
;; semiring's carrier must be meaningfully comparable under `equal?`.
;; Cases to watch for:
;;   - IEEE floats: `(equal? +nan.0 +nan.0)` is #f, so a NaN that
;;     propagates through the sum will spin until MAX-ITERATIONS and
;;     look like a legitimate non-convergence.
;;   - Fresh records, procedures, ports: two "equivalent" values may
;;     not be `equal?`; such carriers will never converge even if the
;;     series mathematically saturates.

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
         (max-iter (if (null? rest) n (car rest))))
    (smat-check-nat "semiring-matrix-closure" "max-iterations" max-iter)
    ;; Short-circuit the 0x0 case: M* = I = the 0x0 matrix itself.
    ;; Without this the default max-iter (= n = 0) would trip the
    ;; guard on the very first iteration and error "did not converge".
    (if (= n 0)
        (semiring-matrix-identity S 0)
        (let ((I (semiring-matrix-identity S n)))
          ;; Attempt at most max-iter update steps. iter counts completed
          ;; updates, so the guard is (>= iter max-iter) rather than >.
          (let loop ((T I) (iter 0))
            (if (>= iter max-iter)
                (error "semiring-matrix-closure: did not converge" max-iter)
                (let ((T* (semiring-matrix-add I (semiring-matrix-mul M T))))
                  (if (equal? (smat-data T) (smat-data T*))
                      T
                      (loop T* (+ iter 1))))))))))

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
  ;; entries is mutable so Path D bang forms can replace the alist in place.
  ;; Callers must hold the sparse record reference; mutating entries through
  ;; an external alist binding does nothing (the setter replaces the field).
  (entries  ssmat-entries ssmat-entries-set!))

(register-matrix-rep! sparse-semiring-matrix? 'sparse)

(define (make-sparse-semiring-matrix S rows cols entries)
  "Construct a sparse ROWS x COLS matrix over semiring S from ENTRIES.\nENTRIES is an alist ((ROW . COL) . VALUE). Positions not listed read\nas (semiring-zero S). Entries whose value is (semiring-zero S) are\nstripped from the stored representation (matching the invariant that\nthe sparse form lists only non-zero cells); duplicate coordinates are\nkept as provided, with the first matching entry winning under assoc.\n\nExamples:\n  (let ((S (counting-semiring)))\n    (sparse-semiring-matrix-ref\n      (make-sparse-semiring-matrix S 3 3 '(((0 . 0) . 5) ((1 . 2) . 7)))\n      1 2))\n  => 7\n\nParameters:\n  S : semiring\n  rows : integer\n  cols : integer\n  entries : list\nReturns: sparse-semiring-matrix\nCategory: algebra\nKeywords: sparse matrix, coordinate list, COO, non-zero entries\n\nSee also: `semiring-matrix->sparse', `sparse->semiring-matrix'."
  (unless (semiring? S)
    (error "make-sparse-semiring-matrix: expected semiring" S))
  (smat-check-nat "make-sparse-semiring-matrix" "rows" rows)
  (smat-check-nat "make-sparse-semiring-matrix" "cols" cols)
  (let ((z (semiring-zero S)))
    (make-sparse-semiring-matrix* S rows cols
      ;; Validate each entry structurally ((row . col) . value) and
      ;; check that coordinates land in [0, rows) x [0, cols). Without
      ;; this, out-of-range entries would either be silently stored
      ;; and become unreachable via ref (making inserts appear to
      ;; vanish) or blow up deep inside sparse->semiring-matrix with
      ;; an opaque vector-set! range error. A row that is in-range
      ;; but a column that overflows is particularly dangerous —
      ;; (* r m) + c can still land inside the flat vector, silently
      ;; corrupting an unrelated cell.
      (let loop ((es entries) (acc '()))
        (cond
          ((null? es) (reverse acc))
          ((not (pair? es))
           (error "make-sparse-semiring-matrix: entries must be a proper list"
                  entries))
          (else
           (let ((entry (car es)))
             (unless (and (pair? entry) (pair? (car entry)))
               (error "make-sparse-semiring-matrix: malformed entry, expected ((row . col) . value)"
                      entry))
             (let ((r (caar entry)) (c (cdar entry)))
               (unless (and (integer? r) (exact? r) (<= 0 r) (< r rows))
                 (error "make-sparse-semiring-matrix: row out of range"
                        r (cons rows cols)))
               (unless (and (integer? c) (exact? c) (<= 0 c) (< c cols))
                 (error "make-sparse-semiring-matrix: col out of range"
                        c (cons rows cols))))
             (if (equal? (cdr entry) z)
                 (loop (cdr es) acc)
                 (loop (cdr es) (cons entry acc))))))))))

(define (sparse-semiring-matrix-rows SM)
  "Return the number of rows in sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: sparse matrix, shape, rows"
  (ssmat-rows SM))

(define (sparse-semiring-matrix-cols SM)
  "Return the number of columns in sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: integer\nCategory: algebra\nKeywords: sparse matrix, shape, columns"
  (ssmat-cols SM))

(define (sparse-semiring-matrix-semiring SM)
  "Return the semiring parameter of sparse matrix SM.\n\nParameters:\n  SM : sparse-semiring-matrix\nReturns: semiring\nCategory: algebra\nKeywords: sparse matrix, semiring"
  (ssmat-semiring SM))

(define (sparse-semiring-matrix-ref SM r c)
  "Return element (R, C) of sparse matrix SM, or (semiring-zero S) if absent.\n\nExamples:\n  (let ((S (counting-semiring)))\n    (sparse-semiring-matrix-ref\n      (make-sparse-semiring-matrix S 2 2 '(((0 . 1) . 9)))\n      0 1))  => 9\n\nParameters:\n  SM : sparse-semiring-matrix\n  r : integer\n  c : integer\nReturns: any\nCategory: algebra\nKeywords: sparse matrix, element, indexing, lookup\n\nSee also: `make-sparse-semiring-matrix'."
  (smat-check-nat "sparse-semiring-matrix-ref" "row" r)
  (smat-check-nat "sparse-semiring-matrix-ref" "col" c)
  (when (or (>= r (ssmat-rows SM)) (>= c (ssmat-cols SM)))
    (error "sparse-semiring-matrix-ref: index out of bounds" r c
           (cons (ssmat-rows SM) (ssmat-cols SM))))
  (let ((found (assoc (cons r c) (ssmat-entries SM))))
    (if found (cdr found) (semiring-zero (ssmat-semiring SM)))))

(define (semiring-matrix->sparse M)
  "Convert dense matrix M to its sparse representation.\nEntries equal to (semiring-zero S) are omitted. Round-trips via\n`sparse->semiring-matrix' preserve all non-zero values.\n\nExamples:\n  (let* ((S (counting-semiring))\n         (M (semiring-matrix-from-rows S '((1 0) (0 2))))\n         (SM (semiring-matrix->sparse M)))\n    (matrix-fold-entries SM 0 (lambda (r c v acc) (+ acc 1))))\n  => 2\n\nParameters:\n  M : semiring-matrix\nReturns: sparse-semiring-matrix\nCategory: algebra\nKeywords: dense to sparse, conversion, compression, non-zero filter\n\nSee also: `sparse->semiring-matrix', `matrix-fold-entries'."
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
;;
;; Binding is POSITIONAL, not by name. The five identifiers map to
;; `semiring-matrix-{add, mul, power, closure, ref}` in that fixed
;; order, so `(with-semiring-matrix (mul add ...) ...)` would bind
;; `mul` to `semiring-matrix-add` — a typo, not a feature. Callers
;; that want shorter names should use this exact order.

(define-syntax with-semiring-matrix
  (syntax-rules ()
    ((with-semiring-matrix (add mul power closure ref) body ...)
     (let ((add     semiring-matrix-add)
           (mul     semiring-matrix-mul)
           (power   semiring-matrix-power)
           (closure semiring-matrix-closure)
           (ref     semiring-matrix-ref))
       body ...))))

;; ─── Registrations for dense-only ops (Path D P7) ───
;; Must appear after the semiring-matrix-* definitions so their values
;; are bound when register-matrix-op! evaluates its IMPL argument.

(register-matrix-op! '(power     dense) semiring-matrix-power)
(register-matrix-op! '(closure   dense) semiring-matrix-closure)
(register-matrix-op! '(permanent dense) semiring-matrix-permanent)
