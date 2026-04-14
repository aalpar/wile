;;; algebra-interval-test.scm — Interval arithmetic and lattice tests

(import (scheme base)
        (chibi test)
        (wile algebra interval)
        (wile algebra lattice))

(test-begin "interval")

;;; ── Infinity comparisons ───────────────────────

(test-group "inf<="
  ;; neg-inf <= everything
  (test #t (inf<= 'neg-inf 'neg-inf))
  (test #t (inf<= 'neg-inf 0))
  (test #t (inf<= 'neg-inf 42))
  (test #t (inf<= 'neg-inf 'pos-inf))
  ;; everything <= pos-inf
  (test #t (inf<= 'pos-inf 'pos-inf))
  (test #t (inf<= 0 'pos-inf))
  (test #t (inf<= -100 'pos-inf))
  ;; pos-inf not <= finite
  (test #f (inf<= 'pos-inf 0))
  (test #f (inf<= 'pos-inf 999))
  ;; finite not <= neg-inf
  (test #f (inf<= 0 'neg-inf))
  (test #f (inf<= -100 'neg-inf))
  ;; finite comparison
  (test #t (inf<= 3 5))
  (test #t (inf<= 5 5))
  (test #f (inf<= 7 5)))

(test-group "inf-min/max"
  (test 'neg-inf (inf-min 'neg-inf 5))
  (test 5 (inf-max 'neg-inf 5))
  (test 3 (inf-min 3 'pos-inf))
  (test 'pos-inf (inf-max 3 'pos-inf))
  (test 2 (inf-min 2 7))
  (test 7 (inf-max 2 7)))

;;; ── Infinity arithmetic ────────────────────────

(test-group "inf+"
  ;; finite
  (test 7 (inf+ 3 4))
  ;; infinity propagation
  (test 'pos-inf (inf+ 'pos-inf 5))
  (test 'pos-inf (inf+ 5 'pos-inf))
  (test 'neg-inf (inf+ 'neg-inf 5))
  (test 'neg-inf (inf+ 5 'neg-inf))
  ;; indeterminate: pos-inf + neg-inf = pos-inf (conservative)
  (test 'pos-inf (inf+ 'pos-inf 'neg-inf))
  (test 'pos-inf (inf+ 'neg-inf 'pos-inf)))

(test-group "inf-"
  ;; finite
  (test 2 (inf- 5 3))
  ;; infinity
  (test 'pos-inf (inf- 'pos-inf 5))
  (test 'neg-inf (inf- 'neg-inf 5))
  (test 'neg-inf (inf- 5 'pos-inf))
  (test 'pos-inf (inf- 5 'neg-inf))
  ;; indeterminate: inf - inf = pos-inf (conservative)
  (test 'pos-inf (inf- 'pos-inf 'pos-inf))
  (test 'pos-inf (inf- 'neg-inf 'neg-inf)))

(test-group "inf*"
  ;; finite
  (test 12 (inf* 3 4))
  ;; 0 * infinity = 0 (absorbing)
  (test 0 (inf* 0 'pos-inf))
  (test 0 (inf* 'pos-inf 0))
  (test 0 (inf* 0 'neg-inf))
  (test 0 (inf* 'neg-inf 0))
  ;; sign rules
  (test 'pos-inf (inf* 'pos-inf 'pos-inf))
  (test 'pos-inf (inf* 'neg-inf 'neg-inf))
  (test 'neg-inf (inf* 'pos-inf 'neg-inf))
  (test 'neg-inf (inf* 'neg-inf 'pos-inf))
  ;; finite * infinity
  (test 'pos-inf (inf* 'pos-inf 3))
  (test 'neg-inf (inf* 'pos-inf -3))
  (test 'neg-inf (inf* 'neg-inf 3))
  (test 'pos-inf (inf* 'neg-inf -3))
  (test 'pos-inf (inf* 3 'pos-inf))
  (test 'neg-inf (inf* -3 'pos-inf)))

;;; ── Interval lattice ───────────────────────────

(test-group "lattice-validate"
  (let ((il (interval-lattice)))
    (test #t (validate-lattice il
               (list '(1 . 5) '(3 . 10) '(-2 . 2) '(0 . 0))))))

(test-group "lattice-join"
  (let ((il (interval-lattice)))
    ;; join widens to encompass both
    (test '(1 . 10) (lattice-join il '(1 . 5) '(3 . 10)))
    ;; bottom is join identity
    (test '(2 . 5) (lattice-join il (lattice-bottom il) '(2 . 5)))
    (test '(2 . 5) (lattice-join il '(2 . 5) (lattice-bottom il)))
    ;; join with self is self
    (test '(1 . 5) (lattice-join il '(1 . 5) '(1 . 5)))))

(test-group "lattice-meet"
  (let ((il (interval-lattice)))
    ;; meet narrows to intersection
    (test '(3 . 5) (lattice-meet il '(1 . 5) '(3 . 10)))
    ;; empty intersection = bottom
    (test 'interval-bot (lattice-meet il '(1 . 3) '(5 . 10)))
    ;; meet with bottom = bottom
    (test 'interval-bot (lattice-meet il (lattice-bottom il) '(1 . 5)))))

(test-group "lattice-leq"
  (let ((il (interval-lattice)))
    ;; containment
    (test #t (lattice-leq? il '(2 . 5) '(1 . 10)))
    (test #f (lattice-leq? il '(1 . 10) '(2 . 5)))
    ;; bottom <= everything
    (test #t (lattice-leq? il (lattice-bottom il) '(1 . 5)))
    ;; everything <= top
    (test #t (lattice-leq? il '(1 . 5) (lattice-top il)))
    ;; equal intervals
    (test #t (lattice-leq? il '(3 . 7) '(3 . 7)))))

;;; ── Interval arithmetic ────────────────────────

(test-group "interval-add"
  (test '(4 . 9) (interval-add '(1 . 3) '(3 . 6)))
  (test '(0 . 0) (interval-add '(0 . 0) '(0 . 0)))
  (test '(-1 . 8) (interval-add '(-3 . 2) '(2 . 6))))

(test-group "interval-sub"
  ;; [1,3] - [3,6] = [1-6, 3-3] = [-5, 0]
  (test '(-5 . 0) (interval-sub '(1 . 3) '(3 . 6)))
  (test '(0 . 0) (interval-sub '(5 . 5) '(5 . 5))))

(test-group "interval-mul"
  ;; positive * positive
  (test '(2 . 12) (interval-mul '(1 . 3) '(2 . 4)))
  ;; zero interval
  (test '(0 . 0) (interval-mul '(0 . 0) '(5 . 10)))
  ;; mixed signs: [-2,3] * [1,4] corners: -2, -8, 3, 12 => [-8, 12]
  (test '(-8 . 12) (interval-mul '(-2 . 3) '(1 . 4)))
  ;; negative * negative: [-3,-1] * [-4,-2] corners: 12,6,4,2 => [2, 12]
  (test '(2 . 12) (interval-mul '(-3 . -1) '(-4 . -2))))

(test-end)
(test-exit)
