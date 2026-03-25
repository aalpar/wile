;;; algebra-ring-test.scm — Ring and field tests

(import (scheme base)
        (chibi test)
        (wile algebra monoid)
        (wile algebra semiring)
        (wile algebra group)
        (wile algebra ring))

(test-begin "rings-and-fields")

;; -- Rings --

(test-group "integer-ring"
  (let ((R (integer-ring)))
    (test #t (ring? R))
    (test 5  (ring-plus R 2 3))
    (test 6  (ring-times R 2 3))
    (test 0  (ring-zero R))
    (test 1  (ring-one R))
    (test -3 (ring-negate R 3))
    (test -1 (ring-minus R 2 3))))

(test-group "modular-ring"
  (let ((R (modular-ring 7)))
    (test #t (ring? R))
    (test 2  (ring-plus R 5 4))    ; (5+4) mod 7 = 2
    (test 6  (ring-times R 2 3))   ; (2*3) mod 7 = 6
    (test 4  (ring-negate R 3))    ; (-3) mod 7 = 4
    (test 0  (ring-zero R))
    (test 1  (ring-one R))))

(test-group "ring-projections"
  (let ((R (integer-ring)))
    (test #t (semiring? (ring->semiring R)))
    (test #t (group? (ring->additive-group R)))
    ;; additive group inverse = negate
    (test -3 (group-inverse (ring->additive-group R) 3))))

(test-group "with-ring"
  (let ((R (integer-ring)))
    (test 7 (with-ring R (plus times zero one negate)
              (plus (times 2 3) one)))))

(test-group "validate-ring"
  (test #t (validate-ring (integer-ring) '(-2 -1 0 1 2))))

;; -- Fields --

(test-group "rational-field"
  (let ((F (rational-field)))
    (test #t (field? F))
    (test 5  (field-plus F 2 3))
    (test 6  (field-times F 2 3))
    (test 1/3 (field-reciprocal F 3))
    (test 2/3 (field-divide F 2 3))))

(test-group "field-projections"
  (let ((F (rational-field)))
    (test #t (ring? (field->ring F)))))

(test-group "with-field"
  (let ((F (rational-field)))
    (test 5/3 (with-field F (plus times zero one negate reciprocal)
                (plus (times 2 (reciprocal 3)) one)))))

(test-group "validate-field"
  ;; exclude 0 from samples for multiplicative inverse checks
  (test #t (validate-field (rational-field) '(-2 -1 1/2 1 2))))

(test-end)
(test-exit)
