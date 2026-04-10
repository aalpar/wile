;;; algebra-boolean-test.scm — Boolean algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra heyting)
        (wile algebra ring)
        (wile algebra semiring)
        (wile algebra boolean))

(test-begin "boolean-algebras")

;; ─── powerset-boolean ────────────────────────

(define B (powerset-boolean '(a b c)))

(test-group "construction"
  (test #t (boolean-algebra? B))
  (test #f (boolean-algebra? 42))
  (test #f (boolean-algebra? (powerset-lattice '(a b c))))
  (test #f (boolean-algebra? (powerset-heyting '(a b c)))))

(test-group "powerset-boolean-basics"
  (test '() (boolean-bottom B))
  (test '(a b c) (boolean-top B))
  ;; join = union
  (test #t (let ((r (boolean-join B '(a) '(b))))
             (and (member 'a r) (member 'b r)
                  (= 2 (length r)))))
  ;; meet = intersection
  (test '(b) (boolean-meet B '(a b) '(b c)))
  ;; leq = subset
  (test #t (boolean-leq? B '(a) '(a b)))
  (test #f (boolean-leq? B '(a b) '(a))))

(test-group "boolean-complement"
  ;; complement({a}) = {b,c}
  (let ((result (boolean-complement B '(a))))
    (test 2 (length result))
    (test #t (and (member 'b result) (member 'c result) #t)))
  ;; complement(⊥) = ⊤
  (let ((result (boolean-complement B '())))
    (test 3 (length result)))
  ;; complement(⊤) = ⊥
  (test '() (boolean-complement B '(a b c))))

(test-group "complement-laws"
  (let ((samples '((a) (b) (a b) (b c) () (a b c))))
    (for-each
      (lambda (a)
        (let ((neg-a (boolean-complement B a)))
          ;; Non-contradiction: a ∧ ¬a = ⊥
          (test '() (boolean-meet B a neg-a))
          ;; Excluded middle: a ∨ ¬a = ⊤
          (let ((r (boolean-join B a neg-a)))
            (test 3 (length r)))))
      samples)))

(test-group "involution"
  ;; ¬¬a = a for all samples
  (let ((samples '((a) (b c) () (a b c) (a b))))
    (for-each
      (lambda (a)
        (let* ((neg-a (boolean-complement B a))
               (neg-neg-a (boolean-complement B neg-a)))
          ;; Same elements (may differ in order)
          (test (length a) (length neg-neg-a))
          (for-each
            (lambda (x) (test #t (and (member x neg-neg-a) #t)))
            a)))
      samples)))

;; ─── boolean->heyting ────────────────────────

(test-group "boolean->heyting"
  (let ((H (boolean->heyting B)))
    (test #t (heyting-algebra? H))
    ;; implies = ¬a ∨ b
    ;; {a,b} → {b} = ¬{a,b} ∨ {b} = {c} ∨ {b} = {b,c}
    (let ((result (heyting-implies H '(a b) '(b))))
      (test 2 (length result))
      (test #t (and (member 'b result) (member 'c result) #t)))
    ;; Heyting negate agrees with Boolean complement
    (let ((hn (heyting-negate H '(a)))
          (bc (boolean-complement B '(a))))
      (test (length bc) (length hn))
      (for-each
        (lambda (x) (test #t (and (member x hn) #t)))
        bc))))

;; ─── boolean->lattice ────────────────────────

(test-group "boolean->lattice"
  (let ((L (boolean->lattice B)))
    (test #t (lattice? L))
    (test '(b) (lattice-meet L '(a b) '(b c)))
    (test '() (lattice-bottom L))
    (test #t (lattice-leq? L '(a) '(a b)))))

;; ─── boolean->ring ───────────────────────────

(test-group "boolean->ring"
  (let ((R (boolean->ring B)))
    (test #t (ring? R))
    ;; plus = symmetric difference
    ;; {a,b} △ {b,c} = {a,c}
    (let ((result (ring-plus R '(a b) '(b c))))
      (test 2 (length result))
      (test #t (and (member 'a result) (member 'c result) #t)))
    ;; times = meet = intersection
    (test '(b) (ring-times R '(a b) '(b c)))
    ;; zero = ⊥
    (test '() (ring-zero R))
    ;; one = ⊤
    (test '(a b c) (ring-one R))
    ;; a + a = ⊥ (characteristic 2)
    (test '() (ring-plus R '(a b) '(a b)))
    ;; negate = identity
    (test '(a b) (ring-negate R '(a b)))
    ;; ring-minus = ring-plus (since negate is identity)
    (let ((result (ring-minus R '(a b) '(b c))))
      (test 2 (length result))
      (test #t (and (member 'a result) (member 'c result) #t)))))

(test-group "boolean-ring-distributivity"
  ;; times distributes over plus:
  ;; {a} ∧ ({a,b} △ {b,c}) = {a} ∧ {a,c} = {a}
  ;; ({a} ∧ {a,b}) △ ({a} ∧ {b,c}) = {a} △ {} = {a}
  (let ((R (boolean->ring B)))
    (let ((lhs (ring-times R '(a) (ring-plus R '(a b) '(b c))))
          (rhs (ring-plus R (ring-times R '(a) '(a b))
                            (ring-times R '(a) '(b c)))))
      (test (length lhs) (length rhs))
      (test #t (and (member 'a lhs) (member 'a rhs) #t)))))

;; ─── validate-boolean-algebra ────────────────

(test-group "validate-boolean-algebra"
  ;; Valid: powerset Boolean
  (test #t (validate-boolean-algebra B '((a) (b) (a b) (b c))))
  ;; Invalid: broken complement (always returns empty)
  (let ((bad (make-boolean-algebra
               (lambda (a b) (if (< a b) b a))  ; max
               (lambda (a b) (if (< a b) a b))  ; min
               0 10 <=
               (lambda (a) 0))))  ; complement always returns bottom
    ;; Should detect excluded-middle violation: 5 ∨ ¬5 = max(5,0) = 5 ≠ 10
    (test #f (eq? #t (validate-boolean-algebra bad '(0 3 5 10))))))

;; ─── with-boolean macro ──────────────────────

(test-group "with-boolean"
  (with-boolean B (join meet bottom top leq? complement)
    ;; complement works
    (let ((result (complement '(a))))
      (test 2 (length result))
      (test #t (and (member 'b result) (member 'c result) #t)))
    ;; join works
    (test #t (let ((r (join '(a) '(b))))
               (and (member 'a r) (member 'b r) #t)))
    ;; bottom/top
    (test '() bottom)
    (test '(a b c) top)
    ;; leq?
    (test #t (leq? '(a) '(a b)))))

(test-end)
(test-exit)
