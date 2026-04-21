;;; algebra-matrix-test.scm — Semiring-parameterized matrix algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra semiring)
        (wile algebra ring)
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

(test-group "default fill uses the actual semiring-zero, not literal 0"
  ;; Regression guard: a bug that filled with literal 0 instead of
  ;; (semiring-zero S) would pass every counting test and silently
  ;; break Boolean (where zero is #f) and tropical (zero is the
  ;; tropical-inf symbol).
  (let ((B (boolean-semiring)))
    (test '((#f #f) (#f #f))
          (semiring-matrix->rows (make-semiring-matrix B 2 2))))
  (let ((Tr (tropical-semiring)) (T tropical-inf))
    (test `((,T ,T) (,T ,T))
          (semiring-matrix->rows (make-semiring-matrix Tr 2 2)))))

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

(test-group "semiring-matrix-from-rows rejects non-list first row"
  ;; Catches the forgot-the-outer-parens mistake at the API boundary
  ;; instead of surfacing an opaque (length 5) type error.
  (test-error
    (semiring-matrix-from-rows (counting-semiring) '(5 (1 2)))))

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

(test-group "semiring-matrix-mul rejects distinct semirings"
  ;; Symmetric with the semiring-matrix-add test above; both guards
  ;; exist at the same site and should be covered in tandem.
  (let ((A (make-semiring-matrix (counting-semiring) 2 2))
        (B (make-semiring-matrix (counting-semiring) 2 2)))
    (test-error (semiring-matrix-mul A B))))

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

(test-group "permanent 1x1 is the single element"
  ;; The minimum non-empty case — exercises smat-fold-permutations'
  ;; single-element path where off-by-ones would most plausibly show.
  (let ((S (counting-semiring)))
    (test 7 (semiring-matrix-permanent
              (semiring-matrix-from-rows S '((7)))))
    (test 0 (semiring-matrix-permanent
              (semiring-matrix-from-rows S '((0)))))))

(test-group "permanent under tropical = minimum-cost assignment"
  (let ((Tr (tropical-semiring)))
    ;; min(4+5, 1+2) = 3
    (test 3 (semiring-matrix-permanent
              (semiring-matrix-from-rows Tr '((4 1) (2 5)))))
    ;; 3x3 with a uniquely-minimal diagonal assignment: only the
    ;; identity permutation (0 1 2) yields 1+1+1 = 3; every other
    ;; permutation hits at least one 100-weight cell and sums to at
    ;; least 102. A buggy implementation that returned the wrong
    ;; permutation's cost would fail loudly instead of coincidentally
    ;; matching (which happens on symmetric matrices like the old
    ;; 1..9 case where every permutation sums to 15).
    (test 3 (semiring-matrix-permanent
              (semiring-matrix-from-rows Tr
                '((1   100 100)
                  (100 1   100)
                  (100 100 1  )))))))

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

;; -- User-defined semiring --

(test-group "operations thread a user-defined semiring (max-plus)"
  ;; The parameterization over an arbitrary semiring is the product.
  ;; Exercise it explicitly by building a max-plus semiring ad-hoc via
  ;; make-semiring so a regression that hardcoded + / * / 0 / 1 in
  ;; any matrix operation would fail here without triggering any of
  ;; the built-in semiring tests.
  (let* ((neg-inf 'neg-inf)
         (mp-add (lambda (a b)
                   (cond ((eq? a neg-inf) b)
                         ((eq? b neg-inf) a)
                         (else (max a b)))))
         (mp-times (lambda (a b)
                     (cond ((eq? a neg-inf) neg-inf)
                           ((eq? b neg-inf) neg-inf)
                           (else (+ a b)))))
         (S (make-semiring mp-add mp-times neg-inf 0))
         (M (semiring-matrix-from-rows S '((4 1) (2 5)))))
    ;; permanent = max(4+5, 1+2) = 9 — distinct from the tropical
    ;; answer (min = 3) and the counting answer (sum = 22).
    (test 9 (semiring-matrix-permanent M))
    ;; mul [[4 1][2 5]] [[4 1][2 5]] under max-plus:
    ;;   C[0,0] = max(4+4, 1+2) = 8
    ;;   C[0,1] = max(4+1, 1+5) = 6
    ;;   C[1,0] = max(2+4, 5+2) = 7
    ;;   C[1,1] = max(2+1, 5+5) = 10
    (test '((8 6) (7 10))
          (semiring-matrix->rows (semiring-matrix-mul M M)))))

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

;; Count stored entries via the polymorphic iterator API (Path D P3).
;; Replaces (length (sparse-semiring-matrix-entries SM)); order-
;; independent since it only accumulates a counter.
(define (matrix-stored-count M)
  (matrix-fold-entries M 0 (lambda (r c v acc) (+ acc 1))))

(test-group "dense -> sparse omits zeros"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 0) (0 2))))
         (SM (semiring-matrix->sparse M)))
    (test 2 (matrix-stored-count SM))
    (test 1 (sparse-semiring-matrix-ref SM 0 0))
    (test 2 (sparse-semiring-matrix-ref SM 1 1))
    (test 0 (sparse-semiring-matrix-ref SM 0 1))))

(test-group "sparse -> dense fills zeros"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 5))))
         (M (sparse->semiring-matrix SM)))
    (test '((5 0) (0 0)) (semiring-matrix->rows M))))

(test-group "make-sparse-semiring-matrix strips zero-valued entries"
  ;; Documented invariant: entries whose value equals (semiring-zero
  ;; S) are filtered out of the stored representation so the sparse
  ;; form lists only non-zero cells. The dense->sparse direction was
  ;; tested; the constructor's own filter was not.
  (let ((S (counting-semiring)))
    (test 1 (matrix-stored-count
              (make-sparse-semiring-matrix S 3 3
                '(((0 . 0) . 0) ((1 . 1) . 7) ((2 . 2) . 0))))))
  ;; Under the boolean semiring zero is #f (not the integer 0); a
  ;; regression that hardcoded = 0 would break this case.
  (let ((B (boolean-semiring)))
    (test 1 (matrix-stored-count
              (make-sparse-semiring-matrix B 2 2
                '(((0 . 0) . #f) ((1 . 1) . #t)))))))

(test-group "sparse duplicate coordinates: first entry wins"
  ;; Documented invariant: duplicate coordinates in the alist retain
  ;; the first-match-wins semantics of assoc. This must hold both for
  ;; direct ref and when materialized via sparse->semiring-matrix
  ;; (which writes entries in reverse so the first wins under
  ;; vector-set!). Without the reverse trick the two reads would
  ;; disagree — the test pins both sides.
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 2 2
               '(((0 . 0) . 5) ((0 . 0) . 9)))))
    (test 5 (sparse-semiring-matrix-ref SM 0 0))
    (test 5 (semiring-matrix-ref (sparse->semiring-matrix SM) 0 0))))

(test-group "sparse round-trip preserves non-zero entries"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 0 2) (0 3 0) (4 0 5))))
         (M* (sparse->semiring-matrix (semiring-matrix->sparse M))))
    (test (semiring-matrix->rows M) (semiring-matrix->rows M*))))

(test-group "sparse round-trip under boolean semiring"
  ;; Exercises the zero-detection path where (semiring-zero B) is #f,
  ;; not the integer 0. A regression that used zero? or = 0 would
  ;; pass the counting case and silently break here.
  (let* ((B (boolean-semiring))
         (M (semiring-matrix-from-rows B
              '((#t #f #t) (#f #t #f) (#f #f #t))))
         (M* (sparse->semiring-matrix (semiring-matrix->sparse M))))
    (test (semiring-matrix->rows M) (semiring-matrix->rows M*))))

(test-group "sparse round-trip under tropical semiring"
  ;; Exercises the path where (semiring-zero Tr) is tropical-inf, a
  ;; symbol sentinel rather than a number.
  (let* ((Tr (tropical-semiring))
         (T tropical-inf)
         (M (semiring-matrix-from-rows Tr
              `((0  1  ,T)
                (,T 0  2 )
                (,T ,T 0 ))))
         (M* (sparse->semiring-matrix (semiring-matrix->sparse M))))
    (test (semiring-matrix->rows M) (semiring-matrix->rows M*))))

;; -- Macro --

(test-group "with-semiring-matrix rebinds operation names"
  ;; Positional binding order is (add mul power closure ref). Every
  ;; identifier must be exercised — leaving any out lets a template
  ;; typo that swapped two bindings ship undetected.
  (let* ((S (counting-semiring))
         (B (boolean-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (A2 (semiring-matrix-from-rows S '((5 6) (7 8))))
         (G (semiring-matrix-from-rows B
              '((#f #t #f) (#f #f #t) (#f #f #f)))))
    (with-semiring-matrix (add mul power closure ref)
      (test '((6 8) (10 12))       (semiring-matrix->rows (add A A2)))
      (test '((19 22) (43 50))     (semiring-matrix->rows (mul A A2)))
      (test '((7 10) (15 22))      (semiring-matrix->rows (power A 2)))
      (test '((#t #t #t) (#f #t #t) (#f #f #t))
            (semiring-matrix->rows (closure G)))
      (test 4                      (ref A 1 1)))))

;; ─── Path D scaffold sanity (P2) ─────────────

(test-group "matrix-rep-tag returns 'dense for a dense matrix"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4)))))
    (test 'dense (matrix-rep-tag M))))

(test-group "matrix-rep-tag returns 'sparse for a sparse matrix"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 5)))))
    (test 'sparse (matrix-rep-tag SM))))

(test-group "matrix-rep-tag rejects non-matrix input"
  (test-error (matrix-rep-tag 42)))

(test-group "matrix? and matrix-rep-tag agree on the registered rep set"
  ;; Structural invariant: both functions derive from the shared
  ;; *matrix-reps* registry, so for every known rep they must agree
  ;; (matrix? accepts it; matrix-rep-tag returns a usable dispatch
  ;; symbol). For a non-matrix, matrix? is #f and matrix-rep-tag
  ;; raises. If a future rep registers only one side, this group
  ;; catches the drift.
  (let* ((S  (counting-semiring))
         (D  (make-semiring-matrix S 1 1))
         (SM (make-sparse-semiring-matrix S 1 1 '())))
    ;; Each registered rep: matrix? is #t, matrix-rep-tag returns the
    ;; expected symbol, and the scaffold dispatches through it.
    (for-each (lambda (M expected-tag)
                (test #t (matrix? M))
                (test expected-tag (matrix-rep-tag M))
                (test (semiring-zero S) (matrix-ref M 0 0)))
              (list D SM)
              '(dense sparse))
    ;; Non-matrix: matrix? is #f, matrix-rep-tag raises.
    (test #f (matrix? 42))
    (test #f (matrix? 'not-a-matrix))
    (test-error (matrix-rep-tag 42))))

;; ─── Iterator API (P3) ───────────────────────

(test-group "matrix-for-each-entry visits every cell of a dense matrix in row-major order"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4))))
         (seen '()))
    (matrix-for-each-entry M
      (lambda (r c v) (set! seen (cons (list r c v) seen))))
    ;; `seen' is built with cons, so reversing recovers visitation order.
    ;; This single assertion pins count, per-cell (r c v), and row-major
    ;; traversal — a sorted check would hide any ordering regression.
    (test '((0 0 1) (0 1 2) (1 0 3) (1 1 4))
          (reverse seen))))

(test-group "matrix-for-each-entry visits only stored cells of a sparse matrix"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 3 3
               '(((0 . 0) . 5) ((1 . 2) . 7))))
         (count 0))
    (matrix-for-each-entry SM
      (lambda (r c v) (set! count (+ count 1))))
    (test 2 count)))

(test-group "matrix-fold-entries sums values over a dense matrix"
  (let* ((S (counting-semiring))
         (M (semiring-matrix-from-rows S '((1 2) (3 4)))))
    (test 10 (matrix-fold-entries M 0 (lambda (r c v acc) (+ acc v))))))

(test-group "matrix-fold-entries sums only stored values on sparse"
  (let* ((S (counting-semiring))
         (SM (make-sparse-semiring-matrix S 4 4
               '(((0 . 0) . 5) ((3 . 3) . 7)))))
    (test 12 (matrix-fold-entries SM 0 (lambda (r c v acc) (+ acc v))))))

(test-group "iterator API rejects non-matrix input"
  (test-error (matrix-for-each-entry 42 (lambda (r c v) #f)))
  (test-error (matrix-fold-entries 42 0 (lambda (r c v acc) acc))))

;; ─── Polymorphic accessors (P4) ──────────────

(test-group "matrix? recognizes both reps"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2))
         (SM (make-sparse-semiring-matrix S 2 2 '())))
    (test #t (matrix? D))
    (test #t (matrix? SM))
    (test #f (matrix? 42))
    (test #f (matrix? 'not-a-matrix))
    (test #f (matrix? '()))))

(test-group "matrix-ref dispatches on rep"
  (let* ((S (counting-semiring))
         (D (semiring-matrix-from-rows S '((1 2) (3 4))))
         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 1) . 9)))))
    ;; Dense path agrees with semiring-matrix-ref.
    (test 3 (matrix-ref D 1 0))
    ;; Sparse path: present entry.
    (test 9 (matrix-ref SM 0 1))
    ;; Sparse path: absent entry returns semiring zero.
    (test 0 (matrix-ref SM 1 1))
    (test-error (matrix-ref 42 0 0))))

(test-group "matrix-rows / matrix-cols / matrix-shape on both reps"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 3 5))
         (SM (make-sparse-semiring-matrix S 4 7 '())))
    (test 3 (matrix-rows D))
    (test 5 (matrix-cols D))
    (test '(3 . 5) (matrix-shape D))
    (test 4 (matrix-rows SM))
    (test 7 (matrix-cols SM))
    (test '(4 . 7) (matrix-shape SM))
    (test-error (matrix-rows 42))
    (test-error (matrix-cols 42))
    (test-error (matrix-shape 42))))

(test-group "matrix-semiring returns the parameter semiring"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2))
         (SM (make-sparse-semiring-matrix S 2 2 '())))
    (test #t (eq? S (matrix-semiring D)))
    (test #t (eq? S (matrix-semiring SM)))
    (test-error (matrix-semiring 42))))

;; ─── Polymorphic add (P5a) ───────────────────

(test-group "matrix-add pure form: dense + dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8))))
         (C (matrix-add A B)))
    (test 'dense (matrix-rep-tag C))
    (test '((6 8) (10 12)) (semiring-matrix->rows C))))

(test-group "matrix-add pure form: sparse + sparse yields sparse"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 3 3 '(((0 . 0) . 1) ((1 . 1) . 2))))
         (B (make-sparse-semiring-matrix S 3 3 '(((0 . 0) . 3) ((2 . 2) . 5))))
         (C (matrix-add A B)))
    (test 'sparse (matrix-rep-tag C))
    ;; Merged: (0,0)=4 (summed), (1,1)=2 (A-only), (2,2)=5 (B-only).
    (test 3 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))
    (test 4 (matrix-ref C 0 0))
    (test 2 (matrix-ref C 1 1))
    (test 5 (matrix-ref C 2 2))))

(test-group "matrix-add pure form: mixed yields dense"
  (let* ((S (counting-semiring))
         (D (semiring-matrix-from-rows S '((1 0) (0 2))))
         (SM (make-sparse-semiring-matrix S 2 2 '(((0 . 1) . 7))))
         (C1 (matrix-add D SM))
         (C2 (matrix-add SM D)))
    (test 'dense (matrix-rep-tag C1))
    (test 'dense (matrix-rep-tag C2))
    (test '((1 7) (0 2)) (semiring-matrix->rows C1))
    (test '((1 7) (0 2)) (semiring-matrix->rows C2))))

(test-group "matrix-add! in place: dense += dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))
    ;; A += B via (matrix-add! A A B) — idiomatic bang aliasing.
    (matrix-add! A A B)
    (test '((6 8) (10 12)) (semiring-matrix->rows A))))

(test-group "matrix-add! in place: sparse sparse → sparse destination"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 1))))
         (B (make-sparse-semiring-matrix S 2 2 '(((1 . 1) . 2))))
         (C (make-sparse-semiring-matrix S 2 2 '())))
    (matrix-add! C A B)
    (test 2 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))
    (test 1 (matrix-ref C 0 0))
    (test 2 (matrix-ref C 1 1))))

(test-group "matrix-add! rejects wrong destination rep (OQ4 strict)"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2 '()))
         (B (make-sparse-semiring-matrix S 2 2 '()))
         ;; S+S expects sparse destination; dense C is wrong.
         (C-wrong (make-semiring-matrix S 2 2)))
    (test-error (matrix-add! C-wrong A B))))

(test-group "matrix-add! rejects shape mismatch"
  (let* ((S (counting-semiring))
         (A (make-semiring-matrix S 2 2))
         (B (make-semiring-matrix S 3 3))
         (C (make-semiring-matrix S 2 2)))
    (test-error (matrix-add! C A B))))

(test-group "matrix-add! rejects semiring mismatch"
  (let* ((A (make-semiring-matrix (counting-semiring) 2 2))
         (B (make-semiring-matrix (boolean-semiring) 2 2))
         (C (make-semiring-matrix (counting-semiring) 2 2)))
    (test-error (matrix-add! C A B))))

(test-group "matrix-add zero-sum under sparse-sparse strips the entry"
  (let* ((R (ring->semiring (integer-ring)))
         (A (make-sparse-semiring-matrix R 2 2 '(((0 . 0) . 3))))
         (B (make-sparse-semiring-matrix R 2 2 '(((0 . 0) . -3))))
         (C (matrix-add A B)))
    ;; 3 + (-3) = 0, stripped from sparse invariant.
    (test 0 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))))

(test-group "matrix-add sparse+sparse preserves first-match on duplicate coords"
  ;; Regression for the merged-acc reverse fix. make-sparse-semiring-matrix
  ;; permits duplicate coordinates with "first entry wins under assoc"; the
  ;; add kernel must preserve that invariant so callers can reason about
  ;; matrix-ref on the result the same way they reason about the inputs.
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 3) ((0 . 0) . 99))))
         (B (make-sparse-semiring-matrix S 2 2 '(((0 . 0) . 1))))
         (C (matrix-add A B)))
    ;; First-match of A at (0,0) is 3; 3 + 1 = 4. Without the reverse fix
    ;; the result list would be ordered so that (0,0 . 100) wins assoc.
    (test 4 (sparse-semiring-matrix-ref C 0 0))))

(test-group "matrix-add attributes non-matrix errors to the caller"
  ;; Regression for the op-name threading fix in matrix-add-check-operands.
  ;; Previously matrix-add!(not-a-matrix, ...) would surface as either
  ;; "matrix-add: ..." (wrong caller) or "matrix-semiring: not a matrix"
  ;; (wrong layer) depending on the failure path.
  (let* ((S (counting-semiring))
         (M (make-semiring-matrix S 2 2)))
    (test-error (matrix-add 42 M))
    (test-error (matrix-add M 42))
    (test-error (matrix-add! M 42 M))
    (test-error (matrix-add! M M 42))))

;; ─── Polymorphic mul (P5b) ───────────────────

(test-group "matrix-mul pure form: dense × dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8))))
         (C (matrix-mul A B)))
    (test 'dense (matrix-rep-tag C))
    (test '((19 22) (43 50)) (semiring-matrix->rows C))))

(test-group "matrix-mul pure form: dense × sparse → dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         ;; B = [[0 5] [7 0]]
         (B (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 5) ((1 . 0) . 7))))
         (C (matrix-mul A B)))
    (test 'dense (matrix-rep-tag C))
    (test '((14 5) (28 15)) (semiring-matrix->rows C))))

(test-group "matrix-mul pure form: sparse × dense → dense"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 5) ((1 . 0) . 7))))
         (B (semiring-matrix-from-rows S '((1 2) (3 4))))
         (C (matrix-mul A B)))
    (test 'dense (matrix-rep-tag C))
    (test '((15 20) (7 14)) (semiring-matrix->rows C))))

(test-group "matrix-mul pure form: sparse × sparse → sparse"
  (let* ((S (counting-semiring))
         ;; A = [[1 0] [0 2]], B = [[0 3] [4 0]]
         (A (make-sparse-semiring-matrix S 2 2
              '(((0 . 0) . 1) ((1 . 1) . 2))))
         (B (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 3) ((1 . 0) . 4))))
         (C (matrix-mul A B)))
    (test 'sparse (matrix-rep-tag C))
    ;; A×B = [[0 3] [8 0]]
    (test 0 (matrix-ref C 0 0))
    (test 3 (matrix-ref C 0 1))
    (test 8 (matrix-ref C 1 0))
    (test 0 (matrix-ref C 1 1))
    ;; Exactly 2 stored non-zero entries.
    (test 2 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))))

(test-group "matrix-mul! in place: dense × dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8))))
         (C (make-semiring-matrix S 2 2)))
    (matrix-mul! C A B)
    (test '((19 22) (43 50)) (semiring-matrix->rows C))))

(test-group "matrix-mul! in place: dense × sparse → dense"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         ;; B = [[0 5] [7 0]]
         (B (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 5) ((1 . 0) . 7))))
         (C (make-semiring-matrix S 2 2)))
    (matrix-mul! C A B)
    (test '((14 5) (28 15)) (semiring-matrix->rows C))))

(test-group "matrix-mul! in place: sparse × dense → dense"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 5) ((1 . 0) . 7))))
         (B (semiring-matrix-from-rows S '((1 2) (3 4))))
         (C (make-semiring-matrix S 2 2)))
    (matrix-mul! C A B)
    (test '((15 20) (7 14)) (semiring-matrix->rows C))))

(test-group "matrix-mul! in place: sparse × sparse → sparse"
  (let* ((S (counting-semiring))
         ;; A = [[1 0] [0 2]], B = [[0 3] [4 0]]
         (A (make-sparse-semiring-matrix S 2 2
              '(((0 . 0) . 1) ((1 . 1) . 2))))
         (B (make-sparse-semiring-matrix S 2 2
              '(((0 . 1) . 3) ((1 . 0) . 4))))
         (C (make-sparse-semiring-matrix S 2 2 '())))
    (matrix-mul! C A B)
    ;; A×B = [[0 3] [8 0]]
    (test 0 (matrix-ref C 0 0))
    (test 3 (matrix-ref C 0 1))
    (test 8 (matrix-ref C 1 0))
    (test 0 (matrix-ref C 1 1))
    (test 2 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))))

(test-group "matrix-mul! in place: non-square inner dim"
  (let* ((S (counting-semiring))
         ;; A is 2x3, B is 3x2, C must be 2x2
         (A (semiring-matrix-from-rows S '((1 2 3) (4 5 6))))
         (B (semiring-matrix-from-rows S '((7 8) (9 10) (11 12))))
         (C (make-semiring-matrix S 2 2)))
    (matrix-mul! C A B)
    (test '((58 64) (139 154)) (semiring-matrix->rows C))))

(test-group "matrix-mul! rejects dest aliasing an operand (OQ5 incremental)"
  (let* ((S (counting-semiring))
         (A (semiring-matrix-from-rows S '((1 2) (3 4))))
         (B (semiring-matrix-from-rows S '((5 6) (7 8)))))
    ;; (matrix-mul! A A B) is the self-aliased case; must error.
    (test-error (matrix-mul! A A B))
    (test-error (matrix-mul! B A B))))

(test-group "matrix-mul! rejects inner-dim mismatch"
  (let* ((S (counting-semiring))
         (A (make-semiring-matrix S 2 3))
         (B (make-semiring-matrix S 2 2))
         (C (make-semiring-matrix S 2 2)))
    (test-error (matrix-mul! C A B))))

(test-group "matrix-mul! rejects wrong destination rep (OQ4 strict)"
  (let* ((S (counting-semiring))
         (A (make-sparse-semiring-matrix S 2 2 '()))
         (B (make-sparse-semiring-matrix S 2 2 '()))
         ;; S × S expects sparse dest; dense C is wrong.
         (C-wrong (make-semiring-matrix S 2 2)))
    (test-error (matrix-mul! C-wrong A B))))

(test-group "matrix-mul! rejects wrong destination shape"
  (let* ((S (counting-semiring))
         (A (make-semiring-matrix S 2 3))
         (B (make-semiring-matrix S 3 4))
         ;; A × B is 2x4; C with wrong shape must reject.
         (C-wrong (make-semiring-matrix S 3 3)))
    (test-error (matrix-mul! C-wrong A B))))

(test-group "matrix-mul on sparse × sparse preserves sparsity via zero-strip"
  (let* ((R (ring->semiring (integer-ring)))
         ;; A × B produces some zero intermediate cells that must be stripped.
         (A (make-sparse-semiring-matrix R 2 2
              '(((0 . 0) . 2) ((0 . 1) . 3))))
         (B (make-sparse-semiring-matrix R 2 2
              '(((0 . 0) . 1) ((1 . 0) . -2))))
         (C (matrix-mul A B)))
    ;; Row 0: 2·1 + 3·(-2) = 2 - 6 = -4, col 0. Col 1 entirely zero.
    ;; Row 1: zero (no A entries in row 1).
    (test -4 (matrix-ref C 0 0))
    (test 0 (matrix-ref C 0 1))
    (test 0 (matrix-ref C 1 0))
    ;; Only one non-zero stored entry.
    (test 1 (matrix-fold-entries C 0 (lambda (r c v acc) (+ acc 1))))))

;; ─── Capability predicate (P6) ───────────────

(test-group "matrix-op-supported? returns #t for all pure add/mul rep-pair combinations"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2))
         (SM (make-sparse-semiring-matrix S 2 2 '())))
    (test #t (matrix-op-supported? 'add D D))
    (test #t (matrix-op-supported? 'add D SM))
    (test #t (matrix-op-supported? 'add SM D))
    (test #t (matrix-op-supported? 'add SM SM))
    (test #t (matrix-op-supported? 'mul D D))
    (test #t (matrix-op-supported? 'mul D SM))
    (test #t (matrix-op-supported? 'mul SM D))
    (test #t (matrix-op-supported? 'mul SM SM))))

(test-group "matrix-op-supported? for bang forms requires correct dest rep"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2))
         (SM (make-sparse-semiring-matrix S 2 2 '())))
    ;; add! — dense dest, dense operands → registered.
    (test #t (matrix-op-supported? 'add! D D D))
    ;; add! — sparse dest, sparse operands → registered.
    (test #t (matrix-op-supported? 'add! SM SM SM))
    ;; add! — sparse dest with mixed operands → NOT registered (result would be dense).
    (test #f (matrix-op-supported? 'add! SM D SM))
    ;; add! — dense dest with sparse operands → NOT registered (result would be sparse).
    (test #f (matrix-op-supported? 'add! D SM SM))
    ;; mul! — dense dest, dense operands → registered.
    (test #t (matrix-op-supported? 'mul! D D D))
    ;; mul! — sparse dest, sparse operands → registered.
    (test #t (matrix-op-supported? 'mul! SM SM SM))
    ;; mul! — sparse dest with dense operands → NOT registered (result would be dense).
    (test #f (matrix-op-supported? 'mul! SM D D))
    ;; mul! — dense dest with sparse operands → NOT registered (S×S → sparse).
    (test #f (matrix-op-supported? 'mul! D SM SM))))

(test-group "matrix-op-supported? returns #f for non-matrix arguments"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2)))
    (test #f (matrix-op-supported? 'add D 42))
    (test #f (matrix-op-supported? 'add 'not-a-matrix D))
    (test #f (matrix-op-supported? 'mul '() D))))

(test-group "matrix-op-supported? returns #f for unknown ops"
  (let* ((S (counting-semiring))
         (D (make-semiring-matrix S 2 2)))
    (test #f (matrix-op-supported? 'bogus-op D D))
    ;; permanent / power / closure not registered (P7 wires sparse errors);
    ;; currently the dispatch table has no entry, so #f for all.
    (test #f (matrix-op-supported? 'permanent D))))

(test-end)
(test-exit)
