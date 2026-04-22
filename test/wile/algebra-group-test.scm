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
