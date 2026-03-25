;;; algebra-integration-test.scm — Cross-structure integration tests

(import (scheme base)
        (chibi test)
        (wile algebra))  ;; umbrella import

(test-begin "algebra-integration")

;; -- Projection chain: field → ring → semiring → monoid --

(test-group "projection-chain"
  (let* ((F (rational-field))
         (R (field->ring F))
         (S (ring->semiring R))
         (M (semiring->additive-monoid S)))
    (test #t (field? F))
    (test #t (ring? R))
    (test #t (semiring? S))
    (test #t (monoid? M))
    ;; Operations agree through the chain
    (test 5 (field-plus F 2 3))
    (test 5 (ring-plus R 2 3))
    (test 5 (semiring-plus S 2 3))
    (test 5 (monoid-op M 2 3))))

;; -- Fixpoint over a flat lattice (constant propagation sketch) --

(test-group "fixpoint-flat-constant-prop"
  (let ((fl (flat-lattice '(0 1 2 3 42) eqv?)))
    ;; Transfer: bottom → 0 → 42 → 42 (stable)
    (let ((result (fixpoint fl
                    (lambda (v)
                      (cond ((eqv? v (lattice-bottom fl)) 0)
                            ((eqv? v 0) 42)
                            (else v)))
                    (lattice-bottom fl))))
      (test 42 result))))

;; -- Semiring path algebra (boolean reachability) --

(test-group "semiring-reachability"
  (with-semiring (boolean-semiring) (plus times zero one)
    ;; Can A reach C through B?
    ;; A→B exists (#t), B→C exists (#t)
    ;; reachable = A→B × B→C = #t ∧ #t = #t
    (test #t (times one one))
    ;; A→C direct = #f, A→B→C = #t
    ;; A→C* = A→C + A→B→C = #f ∨ #t = #t
    (test #t (plus zero (times one one)))))

;; -- Galois connection with lattice fixpoint --

(test-group "abstract-fixpoint"
  ;; Sign lattice fixpoint: start at bottom, step to 'pos
  (let* ((sl (make-lattice
               (lambda (a b)
                 (cond ((eq? a 'sign-bottom) b)
                       ((eq? b 'sign-bottom) a)
                       ((eq? a b) a)
                       (else 'sign-top)))
               (lambda (a b)
                 (cond ((eq? a 'sign-top) b)
                       ((eq? b 'sign-top) a)
                       ((eq? a b) a)
                       (else 'sign-bottom)))
               'sign-bottom 'sign-top
               (lambda (a b)
                 (cond ((eq? a 'sign-bottom) #t)
                       ((eq? b 'sign-top) #t)
                       ((eq? a b) #t)
                       (else #f)))))
         (result (fixpoint sl
                   (lambda (v)
                     (if (eq? v 'sign-bottom) 'pos v))
                   'sign-bottom)))
    (test 'pos result)))

;; -- map-lattice for per-variable analysis --

(test-group "map-lattice-per-variable"
  (let* ((fl (flat-lattice '(0 1 2) eqv?))
         (ml (map-lattice '(x y) fl)))
    ;; Bottom: all variables at flat-bottom
    (test 'flat-bottom (cdr (assoc 'x (lattice-bottom ml))))
    ;; Join: pointwise
    (let ((a (list (cons 'x 1) (cons 'y 'flat-bottom)))
          (b (list (cons 'x 'flat-bottom) (cons 'y 2))))
      (let ((result (lattice-join ml a b)))
        (test 1 (cdr (assoc 'x result)))
        (test 2 (cdr (assoc 'y result)))))))

(test-end)
(test-exit)
