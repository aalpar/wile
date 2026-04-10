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

;; -- Projection chain: boolean → heyting → lattice → partial-order --

(test-group "order-theoretic-projection-chain"
  (let* ((B (powerset-boolean '(x y z)))
         (H (boolean->heyting B))
         (L (heyting->lattice H))
         (PO (lattice->partial-order L)))
    (test #t (boolean-algebra? B))
    (test #t (heyting-algebra? H))
    (test #t (lattice? L))
    (test #t (partial-order? PO))
    ;; Operations agree through the chain
    (test #t (boolean-leq? B '(x) '(x y)))
    (test #t (heyting-leq? H '(x) '(x y)))
    (test #t (lattice-leq? L '(x) '(x y)))
    (test #t (po-leq? PO '(x) '(x y)))
    ;; Heyting implies agrees with Boolean complement + join
    (let ((h-imp (heyting-implies H '(x) '(x y)))
          (b-imp (boolean-join B (boolean-complement B '(x)) '(x y))))
      (test (length h-imp) (length b-imp))
      (for-each
        (lambda (e) (test #t (and (member e b-imp) #t)))
        h-imp))))

;; -- Boolean↔Ring bridge --

(test-group "boolean-ring-bridge"
  (let* ((B (powerset-boolean '(a b c)))
         (R (boolean->ring B))
         (S (ring->semiring R)))
    (test #t (ring? R))
    (test #t (semiring? S))
    ;; Semiring operations agree with ring
    (let ((r1 (ring-plus R '(a b) '(b c)))
          (s1 (semiring-plus S '(a b) '(b c))))
      (test (length r1) (length s1))
      (for-each
        (lambda (e) (test #t (and (member e s1) #t)))
        r1))
    ;; Ring times = Boolean meet
    (test '(b) (ring-times R '(a b) '(b c)))
    (test '(b) (boolean-meet B '(a b) '(b c)))))

;; -- Powerset round-trip: complement + join recovers universe --

(test-group "powerset-complement-roundtrip"
  (let ((B (powerset-boolean '(1 2 3 4 5))))
    (let* ((s '(1 3 5))
           (comp (boolean-complement B s))
           (whole (boolean-join B s comp)))
      ;; complement is {2, 4}
      (test 2 (length comp))
      (test #t (and (member 2 comp) (member 4 comp) #t))
      ;; union recovers universe
      (test 5 (length whole)))))

;; -- Fixpoint on Heyting lattice --

(test-group "fixpoint-on-heyting"
  ;; powerset-heyting projects to a lattice that fixpoint can use
  (let* ((H (powerset-heyting '(a b c)))
         (L (heyting->lattice H)))
    (let ((result (fixpoint L
                    (lambda (s)
                      (cond ((null? s) '(a))
                            ((and (member 'a s) (not (member 'b s)))
                             (cons 'b s))
                            (else s)))
                    '())))
      (test #t (and (member 'a result) (member 'b result) #t))
      (test 2 (length result)))))

(test-end)
(test-exit)
