;;; algebra-heyting-test.scm — Heyting algebra tests

(import (scheme base)
        (chibi test)
        (wile algebra order)
        (wile algebra lattice)
        (wile algebra heyting))

(test-begin "heyting-algebras")

;; ─── powerset-heyting ────────────────────────

(define H (powerset-heyting '(a b c)))

(test-group "construction"
  (test #t (heyting-algebra? H))
  (test #f (heyting-algebra? 42))
  (test #f (heyting-algebra? (powerset-lattice '(a b c)))))

(test-group "powerset-heyting-basics"
  (test '() (heyting-bottom H))
  (test '(a b c) (heyting-top H))
  ;; join = union
  (test #t (let ((r (heyting-join H '(a) '(b))))
             (and (member 'a r) (member 'b r)
                  (= 2 (length r)))))
  ;; meet = intersection
  (test '(b) (heyting-meet H '(a b) '(b c)))
  ;; leq = subset
  (test #t (heyting-leq? H '(a) '(a b)))
  (test #f (heyting-leq? H '(a b) '(a))))

(test-group "heyting-implies"
  ;; {a} → {a,b}: complement({a}) ∪ {a,b} = {b,c} ∪ {a,b} = {a,b,c} = top
  (let ((result (heyting-implies H '(a) '(a b))))
    (test 3 (length result))
    (test #t (and (member 'a result) (member 'b result) (member 'c result) #t)))
  ;; {a,b} → {b}: complement({a,b}) ∪ {b} = {c} ∪ {b} = {b,c}
  (let ((result (heyting-implies H '(a b) '(b))))
    (test 2 (length result))
    (test #t (and (member 'b result) (member 'c result) #t)))
  ;; top → anything = anything
  (test '(a) (heyting-implies H '(a b c) '(a)))
  ;; anything → top = top
  (let ((result (heyting-implies H '(a) '(a b c))))
    (test 3 (length result)))
  ;; bottom → anything = top
  (let ((result (heyting-implies H '() '(a))))
    (test 3 (length result))))

(test-group "heyting-negate"
  ;; negate({a}) = {a} → ⊥ = complement({a}) = {b,c}
  (let ((result (heyting-negate H '(a))))
    (test 2 (length result))
    (test #t (and (member 'b result) (member 'c result) #t)))
  ;; negate(⊥) = top
  (let ((result (heyting-negate H '())))
    (test 3 (length result)))
  ;; negate(top) = bottom
  (test '() (heyting-negate H '(a b c))))

(test-group "modus-ponens"
  ;; a ∧ (a → b) ≤ b  for sample pairs
  (let ((samples '((a) (b) (a b) (b c) ())))
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (let* ((imp (heyting-implies H a b))
                   (conj (heyting-meet H a imp)))
              (test #t (heyting-leq? H conj b))))
          samples))
      samples)))

;; ─── heyting->lattice ────────────────────────

(test-group "heyting->lattice"
  (let ((L (heyting->lattice H)))
    (test #t (lattice? L))
    (test #f (heyting-algebra? L))
    ;; Operations agree
    (test '(b) (lattice-meet L '(a b) '(b c)))
    (test '() (lattice-bottom L))
    (test #t (lattice-leq? L '(a) '(a b)))
    ;; Partial order projection
    (let ((po (lattice->partial-order L)))
      (test #t (partial-order? po))
      (test #t (po-leq? po '(a) '(a b))))))

;; ─── map-heyting ─────────────────────────────

(test-group "map-heyting"
  (let* ((V (powerset-heyting '(1 2)))
         (M (map-heyting '(x y) V)))
    (test #t (heyting-algebra? M))
    ;; bottom: all keys at V's bottom
    (test '((x) (y)) (heyting-bottom M))
    ;; top: all keys at V's top
    (test '((x 1 2) (y 1 2)) (heyting-top M))
    ;; pointwise implication
    (let ((result (heyting-implies M
                    (list (cons 'x '(1)) (cons 'y '(1 2)))
                    (list (cons 'x '(1 2)) (cons 'y '(1))))))
      ;; x: {1} → {1,2} = {1,2} (top)
      (test #t (and (member 1 (cdr (assoc 'x result)))
                    (member 2 (cdr (assoc 'x result))) #t))
      ;; y: {1,2} → {1} = {1} (complement of {2} is empty... wait)
      ;; y: {1,2} → {1} = complement({1,2}) ∪ {1} = {} ∪ {1} = {1}
      (test '(1) (cdr (assoc 'y result))))
    ;; leq: pointwise
    (test #t (heyting-leq? M
               (list (cons 'x '(1)) (cons 'y '()))
               (list (cons 'x '(1 2)) (cons 'y '(1)))))
    (test #f (heyting-leq? M
               (list (cons 'x '(1 2)) (cons 'y '()))
               (list (cons 'x '(1)) (cons 'y '(1)))))))

;; ─── validate-heyting-algebra ────────────────

(test-group "validate-heyting-algebra"
  ;; Valid: powerset Heyting
  (test #t (validate-heyting-algebra H '((a) (b) (a b) (b c))))
  ;; Invalid: broken implies (always returns top)
  (let ((bad (make-heyting-algebra
               (lambda (a b) (if (< a b) b a))  ; max
               (lambda (a b) (if (< a b) a b))  ; min
               0 10
               <=
               (lambda (a b) 10))))  ; implies always returns top — breaks adjunction
    ;; This should detect adjunction violation:
    ;; c=5, a=3, b=2: c ≤ (a→b)=10 is #t, but a∧c = min(3,5) = 3 ≤ 2 is #f
    (test #f (eq? #t (validate-heyting-algebra bad '(0 2 3 5 10))))))

;; ─── with-heyting macro ──────────────────────

(test-group "with-heyting"
  (with-heyting H (join meet bottom top leq? implies)
    ;; join works
    (test #t (let ((r (join '(a) '(b))))
               (and (member 'a r) (member 'b r) #t)))
    ;; implies works
    (test '(a) (implies '(a b c) '(a)))
    ;; bottom/top
    (test '() bottom)
    (test '(a b c) top)
    ;; leq?
    (test #t (leq? '(a) '(a b)))))

(test-end)
(test-exit)
