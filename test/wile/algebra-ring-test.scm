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

;; -- Negative validation --
;;
;; validate-ring used to check four of roughly eight ring axioms — additive
;; left identity, multiplicative left identity, additive inverse and LEFT
;; distributivity — and returned #t for a structure violating the other four.
;;
;; Observed at 003b3353 for the counterexample below:
;;   (validate-ring ...)                 => #t
;;   (assert-validation (validate-ring ...)) raised nothing, returning #!void
;; while validate-semiring on the identical operations reported 67 violations.
;;
;; The witness is a·b = a²b: multiplication is not associative
;; ((2·2)·2 = 576 against 2·(2·2) = 144) and does not right-distribute
;; ((1+1)·2 = 8 against 1·2 + 1·2 = 4).
;;
;; WHAT THIS DOES NOT COVER: these are spot checks over a finite sample list,
;; not proofs. A structure that violates a law only outside the samples still
;; validates clean, by design.

(define (broken-ring)
  (make-ring + (lambda (a b) (* a a b)) 0 1 -))

(define (violation-types result)
  (if (eq? result #t) '() (map car result)))

(define (has-violation? result type)
  (let loop ((ts (violation-types result)))
    (cond ((null? ts) #f)
          ((eq? (car ts) type) #t)
          (else (loop (cdr ts))))))

(test-group "validate-ring rejects a non-ring"
  (let ((result (validate-ring (broken-ring) '(-2 -1 0 1 2))))
    (test #f (eq? result #t))
    ;; The two axioms the witness violates. Both were invisible before:
    ;; multiplicative associativity was checked by nothing, and only LEFT
    ;; distributivity was checked.
    (test #t (has-violation? result 'multiplicative-associativity))
    (test #t (has-violation? result 'right-distributivity))
    ;; Left distributivity genuinely holds for a²b, so a blanket "reject
    ;; everything" fix would not satisfy this row.
    (test #f (has-violation? result 'left-distributivity))
    ;; The additive structure is a real abelian group, so the additive-group
    ;; projection must stay clean — the prefix keeps it distinguishable from
    ;; the multiplicative side. These are the labels validate-group actually
    ;; emits once prefixed, not the old hand-rolled `additive-inverse', which
    ;; no longer exists and would pass vacuously.
    (test #f (has-violation? result 'additive-associativity))
    (test #f (has-violation? result 'additive-left-inverse))
    (test #f (has-violation? result 'additive-right-inverse))
    (test #f (has-violation? result 'additive-commutativity))
    ;; And the prefixing works at all: a broken additive group must surface
    ;; under the prefixed label.
    (let ((bad-add (validate-ring (make-ring - * 0 1 -) '(-2 -1 1 2))))
      (test #t (has-violation? bad-add 'additive-left-identity))))
  ;; A valid ring must still validate.
  (test #t (validate-ring (integer-ring) '(-2 -1 0 1 2))))

(test-group "validate-field rejects a non-field"
  ;; validate-field inherits validate-ring, so the same witness must surface
  ;; through it. Reciprocal is deliberately wrong too.
  (let ((result (validate-field
                  (make-field + (lambda (a b) (* a a b)) 0 1 - (lambda (a) a))
                  '(-2 -1 1 2))))
    (test #f (eq? result #t))
    (test #t (has-violation? result 'multiplicative-associativity))))

(test-group "validate-group rejects a non-group"
  ;; Associativity holds for -, but neither identity nor inverse does.
  (let ((result (validate-group (make-group - 0 -) '(-2 -1 1 2))))
    (test #f (eq? result #t))
    (test #t (has-violation? result 'left-identity)))
  (test #t (validate-group (make-group + 0 -) '(-2 -1 0 1 2))))

(test-end)
(test-exit)
