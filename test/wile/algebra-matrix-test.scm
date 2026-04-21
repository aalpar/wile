;;; algebra-matrix-test.scm — Semiring-parameterized matrix algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra semiring)
        (wile algebra matrix))

(test-begin "semiring-matrix")

;; -- Construction & predicate --

(test-group "make-semiring-matrix and predicate"
  (let* ((S (counting-semiring))
         (M (make-semiring-matrix S 2 3)))
    (test #t (semiring-matrix? M))
    (test #f (semiring-matrix? '(1 2 3)))
    (test #f (semiring-matrix? 42))
    (test 2 (semiring-matrix-rows M))
    (test 3 (semiring-matrix-cols M))
    (test '(2 . 3) (semiring-matrix-shape M))
    (test #t (eq? S (semiring-matrix-semiring M)))))

(test-group "make-semiring-matrix default fill is semiring-zero"
  (let* ((S (counting-semiring))
         (M (make-semiring-matrix S 2 2)))
    (test '((0 0) (0 0)) (semiring-matrix->rows M))))

(test-group "make-semiring-matrix custom fill"
  (let* ((S (counting-semiring))
         (M (make-semiring-matrix S 2 2 9)))
    (test '((9 9) (9 9)) (semiring-matrix->rows M))))

(test-group "make-semiring-matrix rejects negative dims"
  (test-error (make-semiring-matrix (counting-semiring) -1 3))
  (test-error (make-semiring-matrix (counting-semiring) 3 -1)))

(test-group "constructors reject non-semiring first argument"
  (test-error (make-semiring-matrix 42 2 2))
  (test-error (make-semiring-matrix 'x 2 2))
  (test-error (semiring-matrix-from-rows 42 '((1 2) (3 4))))
  (test-error (semiring-matrix-identity 42 2))
  (test-error (make-sparse-semiring-matrix 42 2 2 '())))

(test-group "make-semiring-matrix rejects inexact or non-integer dims"
  ;; 2.0 is integer-valued but inexact; uniform rule is "exact integer".
  (test-error (make-semiring-matrix (counting-semiring) 2.0 3))
  (test-error (make-semiring-matrix (counting-semiring) 2 3.0))
  (test-error (make-semiring-matrix (counting-semiring) 'x 3))
  (test-error (make-sparse-semiring-matrix (counting-semiring) 2.0 2 '()))
  (test-error (semiring-matrix-identity (counting-semiring) 2.0)))

(test-group "semiring-matrix-from-rows round-trip"
  (let* ((S (counting-semiring))
         (rows '((1 2 3) (4 5 6)))
         (M (semiring-matrix-from-rows S rows)))
    (test rows (semiring-matrix->rows M))
    (test 2 (semiring-matrix-rows M))
    (test 3 (semiring-matrix-cols M))))

(test-group "semiring-matrix-from-rows rejects jagged rows"
  (test-error
    (semiring-matrix-from-rows (counting-semiring) '((1 2) (3 4 5)))))

(test-group "semiring-matrix-from-rows rejects empty"
  (test-error
    (semiring-matrix-from-rows (counting-semiring) '())))

;; -- Identity --

(test-group "semiring-matrix-identity shape and content"
  (let* ((S (counting-semiring))
         (I (semiring-matrix-identity S 3)))
    (test '((1 0 0) (0 1 0) (0 0 1)) (semiring-matrix->rows I))))

(test-group "identity under boolean semiring"
  (let* ((B (boolean-semiring))
         (I (semiring-matrix-identity B 2)))
    (test '((#t #f) (#f #t)) (semiring-matrix->rows I))))

;; -- Element access --

(test-group "semiring-matrix-ref"
  (let ((M (semiring-matrix-from-rows (counting-semiring)
             '((10 20 30) (40 50 60)))))
    (test 10 (semiring-matrix-ref M 0 0))
    (test 30 (semiring-matrix-ref M 0 2))
    (test 40 (semiring-matrix-ref M 1 0))
    (test 60 (semiring-matrix-ref M 1 2))))

(test-group "semiring-matrix-ref bounds checking"
  (let ((M (make-semiring-matrix (counting-semiring) 2 2)))
    (test-error (semiring-matrix-ref M -1 0))
    (test-error (semiring-matrix-ref M 0 -1))
    (test-error (semiring-matrix-ref M 2 0))
    (test-error (semiring-matrix-ref M 0 2))))

;; -- Addition --

(test-group "semiring-matrix-add (counting)"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))
    (test '((6 8) (10 12))
          (semiring-matrix->rows (semiring-matrix-add A B)))))

(test-group "semiring-matrix-add (boolean)"
  (let* ((B (boolean-semiring))
         (X (semiring-matrix-from-rows B '((#t #f) (#f #t))))
         (Y (semiring-matrix-from-rows B '((#f #f) (#t #t)))))
    ;; plus = or
    (test '((#t #f) (#t #t))
          (semiring-matrix->rows (semiring-matrix-add X Y)))))

(test-group "semiring-matrix-add rejects shape mismatch"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((1 2 3) (4 5 6)))))
    (test-error (semiring-matrix-add A B))))

(test-group "semiring-matrix-add rejects distinct semirings"
  (let ((A (make-semiring-matrix (counting-semiring) 2 2))
        (B (make-semiring-matrix (counting-semiring) 2 2)))
    ;; Distinct records even if "equivalent" — eq? identity required.
    (test-error (semiring-matrix-add A B))))

;; -- Multiplication --

(test-group "semiring-matrix-mul 2x2 counting"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))
    ;; [1 2] [5 6]   [1*5+2*7  1*6+2*8]   [19 22]
    ;; [3 4] [7 8] = [3*5+4*7  3*6+4*8] = [43 50]
    (test '((19 22) (43 50))
          (semiring-matrix->rows (semiring-matrix-mul A B)))))

(test-group "semiring-matrix-mul rectangular"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2 3))))           ; 1x3
         (B (semiring-matrix-from-rows S '((4) (5) (6)))))       ; 3x1
    (test '((32)) (semiring-matrix->rows (semiring-matrix-mul A B)))))

(test-group "semiring-matrix-mul with identity"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2 3) (4 5 6))))
         (I2 (semiring-matrix-identity S 2))
         (I3 (semiring-matrix-identity S 3)))
    (test (semiring-matrix->rows M)
          (semiring-matrix->rows (semiring-matrix-mul I2 M)))
    (test (semiring-matrix->rows M)
          (semiring-matrix->rows (semiring-matrix-mul M I3)))))

(test-group "semiring-matrix-mul rejects incompatible inner dim"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))        ; 2x2
         (B (semiring-matrix-from-rows S '((1 2 3)))))           ; 1x3
    (test-error (semiring-matrix-mul A B))))

(test-group "semiring-matrix-mul boolean reachability one step"
  (let* ((B (boolean-semiring))
         ;; 0 -> 1, 1 -> 2
         (M (semiring-matrix-from-rows B
              '((#f #t #f) (#f #f #t) (#f #f #f)))))
    ;; M · M represents 2-step reachability: only 0 -> 2 via 1.
    (test '((#f #f #t) (#f #f #f) (#f #f #f))
          (semiring-matrix->rows (semiring-matrix-mul M M)))))

;; -- Power --

(test-group "semiring-matrix-power M^0 = I"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4)))))
    (test '((1 0) (0 1))
          (semiring-matrix->rows (semiring-matrix-power M 0)))))

(test-group "semiring-matrix-power M^1 = M"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4)))))
    (test '((1 2) (3 4))
          (semiring-matrix->rows (semiring-matrix-power M 1)))))

(test-group "semiring-matrix-power M^2 = M*M"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4)))))
    (test (semiring-matrix->rows (semiring-matrix-mul M M))
          (semiring-matrix->rows (semiring-matrix-power M 2)))))

(test-group "semiring-matrix-power upper-triangular accumulation"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 1) (0 1)))))
    ;; [1 1]^k = [1 k]
    ;; [0 1]    [0 1]
    (test '((1 3) (0 1))
          (semiring-matrix->rows (semiring-matrix-power M 3)))
    (test '((1 10) (0 1))
          (semiring-matrix->rows (semiring-matrix-power M 10)))))

(test-group "semiring-matrix-power rejects non-square or negative k"
  (let ((M (semiring-matrix-from-rows (counting-semiring) '((1 2 3)))))
    (test-error (semiring-matrix-power M 2)))
  (let ((M (semiring-matrix-from-rows (counting-semiring) '((1 2) (3 4)))))
    (test-error (semiring-matrix-power M -1))))

;; -- Kleene closure --

(test-group "boolean closure (reachability) 3-vertex chain"
  (let* ((B (boolean-semiring))
         ;; 0 -> 1, 1 -> 2
         (M (semiring-matrix-from-rows B
              '((#f #t #f) (#f #f #t) (#f #f #f)))))
    ;; Reflexive-transitive closure: every vertex reaches itself,
    ;; 0 reaches 1 and 2, 1 reaches 2.
    (test '((#t #t #t) (#f #t #t) (#f #f #t))
          (semiring-matrix->rows (semiring-matrix-closure M)))))

(test-group "boolean closure with a cycle"
  (let* ((B (boolean-semiring))
         ;; 0 -> 1, 1 -> 0 (mutually reachable)
         (M (semiring-matrix-from-rows B
              '((#f #t) (#t #f)))))
    ;; Both vertices reach each other and themselves.
    (test '((#t #t) (#t #t))
          (semiring-matrix->rows (semiring-matrix-closure M)))))

(test-group "tropical closure (shortest paths)"
  (let* ((T tropical-inf)
         (Tr (tropical-semiring))
         ;; 0 -> 1 (w=1), 1 -> 2 (w=2), 0 -> 2 (w=5)
         (M (semiring-matrix-from-rows Tr
              `((,T 1  5 )
                (,T ,T 2 )
                (,T ,T ,T)))))
    ;; Shortest-path matrix: diagonal = 0 (via I), 0->1=1, 0->2=min(5,1+2)=3,
    ;; 1->2=2, other off-diagonals = inf.
    (test `((0  1  3 )
            (,T 0  2 )
            (,T ,T 0 ))
          (semiring-matrix->rows (semiring-matrix-closure M)))))

(test-group "closure errors on non-convergent semiring when max-iter exhausted"
  ;; Counting semiring on a cyclic graph diverges: every additional
  ;; M^k contributes strictly more paths, so the fixpoint test never
  ;; succeeds and the guard must fire.
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 1) (1 1)))))
    (test-error (semiring-matrix-closure M 5))))

(test-group "closure respects a tight user-supplied max-iter"
  ;; Boolean matrix that would naturally converge at k = n-1 = 2,
  ;; but we cap iteration at 1 to force the error path.
  (let* ((B (boolean-semiring))
         (M (semiring-matrix-from-rows B
              '((#f #t #f) (#f #f #t) (#f #f #f)))))
    (test-error (semiring-matrix-closure M 1))))

(test-group "closure on empty matrix = empty matrix"
  ;; M* of a 0x0 matrix is trivially the 0x0 identity; the default
  ;; max-iter = n = 0 must not error here.
  (let ((S (counting-semiring)))
    (test '() (semiring-matrix->rows
                (semiring-matrix-closure (make-semiring-matrix S 0 0))))))

(test-group "semiring-matrix-closure rejects non-square"
  (test-error
    (semiring-matrix-closure
      (semiring-matrix-from-rows (boolean-semiring) '((#t #f #t))))))

;; -- Permanent --

(test-group "permanent 2x2 counting"
  (let ((S (counting-semiring)))
    ;; perm [[a b] [c d]] = a*d + b*c
    (test 10 (semiring-matrix-permanent
               (semiring-matrix-from-rows S '((1 2) (3 4)))))  ; 1*4+2*3
    (test 0  (semiring-matrix-permanent
               (semiring-matrix-from-rows S '((0 0) (0 0)))))
    (test 1  (semiring-matrix-permanent
               (semiring-matrix-identity S 2)))))

(test-group "permanent 3x3 counting"
  (let ((S (counting-semiring)))
    ;; perm of identity is 1 (only the identity permutation contributes).
    (test 1 (semiring-matrix-permanent (semiring-matrix-identity S 3)))
    ;; perm of all-ones 3x3 = 3! = 6
    (test 6 (semiring-matrix-permanent
              (semiring-matrix-from-rows S '((1 1 1) (1 1 1) (1 1 1)))))))

(test-group "permanent on empty matrix = semiring-one"
  (let ((S (counting-semiring)))
    ;; perm(∅) = 1 by convention: the empty product over the empty
    ;; permutation of {}.
    (test 1 (semiring-matrix-permanent (make-semiring-matrix S 0 0)))))

(test-group "permanent under tropical = minimum-cost assignment"
  (let ((Tr (tropical-semiring)))
    ;; min(4+5, 1+2) = 3
    (test 3 (semiring-matrix-permanent
              (semiring-matrix-from-rows Tr '((4 1) (2 5)))))
    ;; 3x3: one of 6 permutations is the minimum cost.
    ;; perms of (0 1 2): costs are
    ;;   (0 1 2): 1+5+9 = 15
    ;;   (0 2 1): 1+6+8 = 15
    ;;   (1 0 2): 2+4+9 = 15
    ;;   (1 2 0): 2+6+7 = 15
    ;;   (2 0 1): 3+4+8 = 15
    ;;   (2 1 0): 3+5+7 = 15
    (test 15 (semiring-matrix-permanent
               (semiring-matrix-from-rows Tr
                 '((1 2 3) (4 5 6) (7 8 9)))))))

(test-group "permanent under boolean = perfect-matching existence"
  (let ((B (boolean-semiring)))
    ;; Bipartite graph with a perfect matching -> #t
    (test #t (semiring-matrix-permanent
               (semiring-matrix-from-rows B
                 '((#t #f) (#f #t)))))
    ;; No edges -> no matching
    (test #f (semiring-matrix-permanent
               (semiring-matrix-from-rows B
                 '((#f #f) (#f #f)))))
    ;; 3x3 identity -> matching exists
    (test #t (semiring-matrix-permanent (semiring-matrix-identity B 3)))))

(test-group "semiring-matrix-permanent rejects non-square"
  (test-error
    (semiring-matrix-permanent
      (semiring-matrix-from-rows (counting-semiring) '((1 2 3))))))

;; -- Sparse representation --

(test-group "sparse construction and ref"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 3 3
               '(((0 . 0) . 5) ((1 . 2) . 7)))))
    (test #t (sparse-semiring-matrix? SM))
    (test 5 (sparse-semiring-matrix-ref SM 0 0))
    (test 7 (sparse-semiring-matrix-ref SM 1 2))
    ;; Missing positions read as semiring-zero.
    (test 0 (sparse-semiring-matrix-ref SM 0 1))
    (test 0 (sparse-semiring-matrix-ref SM 2 2))))

(test-group "make-sparse-semiring-matrix rejects out-of-range coords"
  (let ((S (counting-semiring)))
    ;; Row overflow, col overflow, and the mixed case where row is in
    ;; range but col overflows — the latter would silently land in a
    ;; wrong cell in the flat vector if left unchecked.
    (test-error (make-sparse-semiring-matrix S 2 2 '(((2 . 0) . 1))))
    (test-error (make-sparse-semiring-matrix S 2 2 '(((0 . 2) . 1))))
    (test-error (make-sparse-semiring-matrix S 2 2 '(((-1 . 0) . 1))))
    ;; 2x3 matrix: (1 . 3) flat-index = 1*3 + 3 = 6, but 6 is out of
    ;; the 6-element vector; a milder (1 . 4) = 1*3+4 = 7 would still
    ;; silently corrupt.
    (test-error (make-sparse-semiring-matrix S 2 3 '(((1 . 3) . 1))))))

(test-group "make-sparse-semiring-matrix rejects malformed entries"
  (let ((S (counting-semiring)))
    (test-error (make-sparse-semiring-matrix S 2 2 '(5)))
    (test-error (make-sparse-semiring-matrix S 2 2 '((0 . 5))))
    (test-error (make-sparse-semiring-matrix S 2 2 '((("x" . 0) . 1))))
    (test-error (make-sparse-semiring-matrix S 2 2 '(((0.0 . 0) . 1))))))

(test-group "sparse ref bounds"
  (let ((SM (make-sparse-semiring-matrix (counting-semiring) 2 2 '())))
    (test-error (sparse-semiring-matrix-ref SM -1 0))
    (test-error (sparse-semiring-matrix-ref SM 2 0))))

(test-group "dense -> sparse omits zeros"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 0) (0 2))))
         (SM (semiring-matrix->sparse M)))
    (test 2 (length (sparse-semiring-matrix-entries SM)))
    (test 1 (sparse-semiring-matrix-ref SM 0 0))
    (test 2 (sparse-semiring-matrix-ref SM 1 1))
    (test 0 (sparse-semiring-matrix-ref SM 0 1))))

(test-group "sparse -> dense fills zeros"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 5))))
         (M (sparse->semiring-matrix SM)))
    (test '((5 0) (0 0)) (semiring-matrix->rows M))))

(test-group "sparse round-trip preserves non-zero entries"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 0 2) (0 3 0) (4 0 5))))
         (M* (sparse->semiring-matrix (semiring-matrix->sparse M))))
    (test (semiring-matrix->rows M) (semiring-matrix->rows M*))))

;; -- Macro --

(test-group "with-semiring-matrix rebinds operation names"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))
    (with-semiring-matrix (add mul power closure ref)
      (test '((6 8) (10 12))       (semiring-matrix->rows (add A B)))
      (test '((19 22) (43 50))     (semiring-matrix->rows (mul A B)))
      (test '((7 10) (15 22))      (semiring-matrix->rows (power A 2)))
      (test 4                      (ref A 1 1)))))

(test-end)
(test-exit)
