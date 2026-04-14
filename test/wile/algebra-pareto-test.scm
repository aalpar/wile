;;; algebra-pareto-test.scm — Pareto dominance tests

(import (scheme base) (chibi test) (wile algebra pareto))

(test-begin "pareto")

(test-group "factor-comparison"
  (test #t (factor-leq? #f #t))
  (test #t (factor-leq? #f #f))
  (test #f (factor-leq? #t #f))
  (test #t (factor-leq? 3 5))
  (test #t (factor-leq? 5 5))
  (test #f (factor-leq? 7 5))
  (test #t (factor-less? 3 5))
  (test #f (factor-less? 5 5)))

(test-group "dominance"
  ;; X dominates Y: >= on all, > on at least one
  (test #t (dominates?
             '((a . 5) (b . 3))
             '((a . 4) (b . 2))))
  ;; equal — no strict improvement
  (test #f (dominates?
             '((a . 5) (b . 3))
             '((a . 5) (b . 3))))
  ;; incomparable
  (test #f (dominates?
             '((a . 5) (b . 2))
             '((a . 4) (b . 3))))
  ;; boolean factors
  (test #t (dominates?
             '((a . #t) (b . 3))
             '((a . #f) (b . 2)))))

(test-group "frontier"
  (let ((result (pareto-frontier
                  '(("x" ((a . 5) (b . 3)))
                    ("y" ((a . 4) (b . 2)))
                    ("z" ((a . 3) (b . 4))))
                  '(a b))))
    ;; x and z are on frontier (incomparable), y is dominated by x
    (test 2 (length (cdr (assoc 'frontier result))))
    (test #t (and (member "x" (cdr (assoc 'frontier result))) #t))
    (test #t (and (member "z" (cdr (assoc 'frontier result))) #t)))
  ;; single candidate = frontier of 1
  (let ((result (pareto-frontier '(("only" ((a . 1)))) '(a))))
    (test '("only") (cdr (assoc 'frontier result)))))

(test-end)
(test-exit)
