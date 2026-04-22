;;; algebra-group-test.scm — Group tests

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra setoid)
        (wile algebra monoid)
        (wile algebra group))

(test-begin "groups")

(define int-add-group (make-group + 0 -))

(test-group "construction"
  (test #t (group? int-add-group))
  (test #f (group? 42)))

(test-group "operations"
  (test 5  (group-op int-add-group 2 3))
  (test 0  (group-identity int-add-group))
  (test -3 (group-inverse int-add-group 3)))

(test-group "group->monoid"
  (let ((m (group->monoid int-add-group)))
    (test #t (monoid? m))
    (test 0  (monoid-identity m))
    (test 5  (monoid-op m 2 3))
    ;; fold works
    (test 10 (monoid-fold m '(1 2 3 4)))))

(test-group "with-group"
  (test 0 (with-group int-add-group (op identity inverse)
            (op 3 (inverse 3)))))

(test-group "validate-group"
  (test #t (validate-group int-add-group '(-2 -1 0 1 2))))

;; §5.4 — extended <group> record with optional metadata fields

(test-group "extended <group> — Z/3Z with full metadata"
  (let ((Z3 (make-group
              (lambda (a b) (modulo (+ a b) 3))    ; op
              0                                    ; identity
              (lambda (k) (modulo (- 3 k) 3))      ; inverse
              `(element? . ,integer?)
              `(setoid . ,(numeric-setoid))
              '(order . 3)
              '(elements . (0 1 2))
              '(generators . (1)))))
    (test #t (group? Z3))
    (test 0  (group-identity Z3))
    (test 0  (group-op Z3 1 2))
    (test 2  (group-inverse Z3 1))
    (test #t ((group-element? Z3) 0))
    (test #t (group-equal? Z3 0 0))
    (test #f (group-equal? Z3 0 1))
    (test 3  (group-order Z3))
    (test '(0 1 2) (group-elements Z3))
    (test '(1) (group-generators Z3))
    (test #t (finite-group? Z3))
    (test #t (finitely-generated-group? Z3))))

(test-group "trivial-group"
  (let ((T (trivial-group)))
    (test 1 (group-order T))
    (test #t (finite-group? T))
    (test (group-identity T)
          (group-op T (group-identity T) (group-identity T)))
    (test '() (group-generators T))
    (test '(e) (group-elements T))))

(test-group "cyclic-group"
  (let ((Z5 (cyclic-group 5)))
    (test 5 (group-order Z5))
    (test 1 (group-op Z5 2 4))
    (test 3 (group-inverse Z5 2))
    (test '(1) (group-generators Z5))
    (test #t (finitely-generated-group? Z5))
    (test #t (finite-group? Z5))
    (test '(0 1 2 3 4) (group-elements Z5))))

(test-group "cyclic-group/validation"
  (test-error (cyclic-group 0))
  (test-error (cyclic-group -1))
  (test-error (cyclic-group 'not-an-integer)))

(test-group "symmetric-group"
  (let ((S3 (symmetric-group 3)))
    (test 6 (group-order S3))
    (test #t (equal? #(0 1 2) (group-identity S3)))
    (test #t (not (equal? (group-op S3 #(1 0 2) #(0 2 1))
                          (group-op S3 #(0 2 1) #(1 0 2)))))
    (test #t (every (group-element? S3) (group-elements S3)))
    (test 6 (length (delete-duplicates (group-elements S3) equal?)))
    (let ((p #(2 0 1)))
      (test #t (equal? (group-identity S3)
                       (group-op S3 p (group-inverse S3 p)))))))

(test-group "symmetric-group/edge-cases"
  (test 1 (group-order (symmetric-group 0)))
  (test 1 (group-order (symmetric-group 1)))
  (test 2 (group-order (symmetric-group 2)))
  (test '() (group-generators (symmetric-group 1)))
  (test 1 (length (group-generators (symmetric-group 2))))
  (test 2 (length (group-generators (symmetric-group 3)))))

(test-group "product-group"
  (let ((Z2xZ3 (product-group (cyclic-group 2) (cyclic-group 3))))
    (test 6 (group-order Z2xZ3))
    (test #t (equal? '(0 0) (group-identity Z2xZ3)))
    (test '(1 0) (group-op Z2xZ3 '(1 2) '(0 1)))
    (test #t (every (lambda (e) (and (list? e) (= (length e) 2)))
                    (group-elements Z2xZ3)))
    (test 6 (length (group-elements Z2xZ3)))))

(test-group "product-group/edge-cases"
  (test #t (eq? (trivial-group) (product-group)))
  (let ((Z3 (cyclic-group 3)))
    (test #t (eq? Z3 (product-group Z3))))
  (let ((triple (product-group (cyclic-group 2)
                               (cyclic-group 3)
                               (cyclic-group 5))))
    (test 30 (group-order triple))
    (test #t (equal? '(0 0 0) (group-identity triple)))
    (test 3 (length (group-generators triple)))))

(test-group "subgroup-generated"
  (let ((Z6 (cyclic-group 6)))
    (let ((H (subgroup-generated Z6 '(2))))
      (test 3 (group-order H))
      (test #t (not (not (memv 0 (group-elements H)))))
      (test #t (not (not (memv 2 (group-elements H)))))
      (test #t (not (not (memv 4 (group-elements H))))))))

(test-group "subgroup?"
  (let* ((Z6 (cyclic-group 6))
         (H  (subgroup-generated Z6 '(2))))
    (test #t (subgroup? H Z6))
    ;; Z_5 is not a subgroup of Z_6 — different operation
    (test #f (subgroup? (cyclic-group 5) Z6))))

(test-group "enumerate-finite-group"
  (let ((Z6-gens (make-group
                   (lambda (a b) (modulo (+ a b) 6))
                   0
                   (lambda (k) (modulo (- 6 k) 6))
                   (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 6))))
                   (cons 'setoid (numeric-setoid))
                   '(generators . (1)))))
    (test #f (finite-group? Z6-gens))
    (test #t (finitely-generated-group? Z6-gens))
    (let ((Z6 (enumerate-finite-group Z6-gens)))
      (test #t (finite-group? Z6))
      (test 6 (group-order Z6))
      (test #t (every (lambda (k) (not (not (memv k (group-elements Z6)))))
                      (iota 6))))))

(test-group "enumerate-finite-group/idempotent"
  (let* ((Z5  (cyclic-group 5))
         (Z5* (enumerate-finite-group Z5)))
    (test #t (eq? Z5 Z5*))))

(test-group "enumerate-finite-group/max-size-cap"
  (let ((Z100-gens (make-group
                     (lambda (a b) (modulo (+ a b) 100))
                     0
                     (lambda (k) (modulo (- 100 k) 100))
                     (cons 'element? (lambda (k) (and (integer? k) (<= 0 k) (< k 100))))
                     (cons 'setoid (numeric-setoid))
                     '(generators . (1)))))
    (test-error (enumerate-finite-group Z100-gens '(max-size . 10)))))

(test-group "enumerate-finite-group/no-generators"
  (let ((R (make-group + 0 -
                       (cons 'element? real?)
                       (cons 'setoid (numeric-setoid)))))
    (test-error (enumerate-finite-group R))))

(test-group "subgroup-generated on symmetric-group (vector elements)"
  (let* ((S3 (symmetric-group 3))
         (H  (subgroup-generated S3 (list #(1 0 2)))))
    ;; ⟨(0 1)⟩ is a 2-element subgroup
    (test 2 (group-order H))
    (test #t (subgroup? H S3))))

(test-group "group-action — record and trivial action"
  (let* ((Z3 (cyclic-group 3))
         (A  (trivial-action Z3 integer?)))
    (test #t (group-action? A))
    (test #t (eq? Z3 (group-action-group A)))
    (test 42 ((group-action-apply A) 1 42))
    (test #t ((group-action-set-element? A) 7)))
  ;; make-group-action rejects non-groups
  (test-error (make-group-action 'not-a-group integer? (lambda (g x) x))))

;; Inline permutation action (permutation-action preset lands in Phase 7)
(define (%perm-action G)
  (make-group-action G integer? (lambda (p i) (vector-ref p i))))

(test-group "orbit — S_2 on {0, 1}"
  (let* ((S2 (symmetric-group 2))
         (A  (%perm-action S2))
         (o  (orbit A 0)))
    (test 2 (length o))
    (test #t (not (not (member 0 o))))
    (test #t (not (not (member 1 o))))))

(test-group "orbit-stabilizer theorem on S_3"
  (let* ((S3 (symmetric-group 3))
         (A  (%perm-action S3)))
    (let ((o (orbit A 0))
          (s (stabilizer A 0)))
      ;; |orbit(0)| · |stab(0)| = |S_3| = 6
      (test (group-order S3) (* (length o) (length s))))))

(test-group "orbit on infinite group acting on finite set (Z on Z/12Z)"
  (let* ((Z (make-group + 0 -
                        (cons 'element? integer?)
                        (cons 'setoid (numeric-setoid))
                        '(generators . (1))))
         (Z/12Z? (lambda (x) (and (integer? x) (<= 0 x) (< x 12))))
         (A (make-group-action Z Z/12Z?
                               (lambda (k x) (modulo (+ x k) 12)))))
    (test #f (finite-group? Z))
    (test #t (finitely-generated-group? Z))
    (let ((o (orbit A 0)))
      (test 12 (length o))
      (test #t (every (lambda (k) (not (not (member k o)))) (iota 12))))))

(test-group "fixed-points"
  (let* ((S3 (symmetric-group 3))
         (A  (%perm-action S3)))
    ;; identity fixes all 3 points
    (test 3 (length (fixed-points A #(0 1 2) '(0 1 2))))
    ;; transposition (0 1) fixes only 2
    (test 1 (length (fixed-points A #(1 0 2) '(0 1 2))))
    (test '(2) (fixed-points A #(1 0 2) '(0 1 2)))))

(test-group "orbit errors on unusable groups"
  ;; No generators, no elements → orbit should error
  (let ((G (make-group + 0 -
                       (cons 'element? integer?)
                       (cons 'setoid (numeric-setoid)))))
    (test-error (orbit (make-group-action G integer? (lambda (g x) (+ x g))) 0))))

(test-group "backward compatibility — 3-arg make-group"
  (let ((Z (make-group + 0 -)))
    (test #t (group? Z))
    (test 0  (group-identity Z))
    (test 5  (group-op Z 2 3))
    (test -3 (group-inverse Z 3))
    (test #f (group-element? Z))
    (test #f (group-order Z))
    (test #f (group-elements Z))
    (test #f (group-generators Z))
    (test #t (setoid? (group-setoid Z)))
    (test #f (finite-group? Z))
    (test #f (finitely-generated-group? Z))))

(test-end)
(test-exit)
