;;; algebra-setoid-test.scm — Setoid tests

(import (scheme base)
        (chibi test)
        (wile algebra setoid))

(test-begin "setoids")

;; -- Construction and predicate --

(test-group "construction"
  (test #t (setoid? (default-setoid)))
  (test #t (setoid? (make-setoid eq?)))
  (test #f (setoid? 42))
  (test #f (setoid? "not a setoid")))

;; -- (default-setoid) (equal?) --

(test-group "default-setoid"
  (test #t (setoid-equiv? (default-setoid) 1 1))
  (test #f (setoid-equiv? (default-setoid) 1 2))
  (test #t (setoid-equiv? (default-setoid) '(1 2) '(1 2)))
  (test #f (setoid-equiv? (default-setoid) '(1 2) '(1 3)))
  (test #t (setoid-equiv? (default-setoid) "abc" "abc")))

;; -- (numeric-setoid) (=) --

(test-group "numeric-setoid"
  (test #t (setoid-equiv? (numeric-setoid) 1 1))
  (test #t (setoid-equiv? (numeric-setoid) 1 1.0))
  (test #f (setoid-equiv? (numeric-setoid) 1 2))
  (test #t (setoid-equiv? (numeric-setoid) 0 0.0)))

;; -- (string-setoid) (string=?) --

(test-group "string-setoid"
  (test #t (setoid-equiv? (string-setoid) "hello" "hello"))
  (test #f (setoid-equiv? (string-setoid) "hello" "world"))
  (test #t (setoid-equiv? (string-setoid) "" "")))

;; -- (eqv-setoid) --

(test-group "eqv-setoid"
  (test #t (setoid-equiv? (eqv-setoid) 1 1))
  (test #f (setoid-equiv? (eqv-setoid) 1 1.0))
  (test #t (setoid-equiv? (eqv-setoid) #t #t))
  (test #f (setoid-equiv? (eqv-setoid) #t #f)))

;; -- custom setoid (integers mod 3) --

(test-group "custom-setoid"
  (let ((mod3 (make-setoid
                (lambda (a b) (= (modulo a 3) (modulo b 3))))))
    (test #t (setoid-equiv? mod3 1 4))    ; 1 ≡ 4 (mod 3)
    (test #t (setoid-equiv? mod3 0 6))    ; 0 ≡ 6 (mod 3)
    (test #f (setoid-equiv? mod3 1 2))    ; 1 ≢ 2 (mod 3)
    (test #t (setoid-equiv? mod3 2 5)))) ; 2 ≡ 5 (mod 3)

;; -- equivalence-class --

(test-group "equivalence-class"
  ;; (numeric-setoid): 1 and 1.0 are equivalent
  (test '(1 1.0) (setoid-equivalence-class
                    (numeric-setoid) 1 '(1 1.0 2 3)))
  ;; (default-setoid): no match
  (test '() (setoid-equivalence-class
               (default-setoid) 5 '(1 2 3)))
  ;; mod 3: equivalence class of 1
  (let ((mod3 (make-setoid
                (lambda (a b) (= (modulo a 3) (modulo b 3))))))
    (test '(1 4 7) (setoid-equivalence-class
                     mod3 1 '(1 2 3 4 5 6 7)))))

;; -- validate-setoid --

(test-group "validate-setoid"
  ;; valid: (default-setoid) on mixed values
  (test #t (validate-setoid (default-setoid) '(1 2 "a" "b")))
  ;; valid: (numeric-setoid)
  (test #t (validate-setoid (numeric-setoid) '(0 1 2 3)))
  ;; invalid: non-reflexive relation
  (let ((result (validate-setoid
                  (make-setoid (lambda (a b) (not (equal? a b))))
                  '(1 2))))
    (test #f (eq? #t result))
    (test 'reflexivity (caar result)))
  ;; invalid: non-symmetric relation
  (let ((result (validate-setoid
                  (make-setoid (lambda (a b) (<= a b)))
                  '(1 2))))
    (test #f (eq? #t result))
    ;; <= is reflexive but not symmetric: 1<=2 but not 2<=1
    (test 'symmetry (caar result))))

;; -- with-setoid macro --

(test-group "with-setoid"
  (test #t (with-setoid (default-setoid) (equiv?)
             (equiv? 1 1)))
  (test #f (with-setoid (default-setoid) (equiv?)
             (equiv? 1 2)))
  ;; using (numeric-setoid) inside with-setoid
  (test #t (with-setoid (numeric-setoid) (equiv?)
             (equiv? 1 1.0))))

(test-end)
(test-exit)
