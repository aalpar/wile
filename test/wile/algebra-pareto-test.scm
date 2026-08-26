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

(test-group "direction"
  (test 'up   (factor-direction '() 'a))
  (test 'up   (factor-direction '((b . down)) 'a))
  (test 'down (factor-direction '((a . down)) 'a))
  ;; a misspelled direction is an error, never a silent 'up
  (test-error (factor-direction '((a . dwon)) 'a))
  ;; factor-direction normalizes its own argument, so an exported caller may
  ;; hand it either spec form
  (test 'up (factor-direction '(a b c) 'a))

  ;; the documentation-only form collapses to "no directions"
  (test '() (normalize-directions '(a b c)))
  (test '((a . down)) (normalize-directions '((a . down))))
  (test '() (normalize-directions '()))

  ;; The form is decided over the whole list, not its first element. A mixed
  ;; spec is a half-finished migration; reading it as either form silently
  ;; ranks an axis the caller meant to name, so it raises in both orders.
  (test-error (normalize-directions '(a (b . down))))
  (test-error (normalize-directions '((a . up) b)))
  ;; and anything that is not a list at all is not either form
  (test-error (normalize-directions 'down))
  (test-error (dominates? '((a . 5)) '((a . 4)) 'down))

  ;; A direction naming no factor is never consulted, so without an eager
  ;; check a misspelled NAME is silent where a misspelled VALUE raises.
  (test-error (dominates? '((a . 5)) '((a . 4)) '((aa . down))))

  ;; One optional argument, not "one and then whatever": positional directions
  ;; are a plausible misreading and used to run on (car opt) alone.
  (test-error (dominates? '((a . 5)) '((a . 4)) '((a . down)) '((a . up))))

  ;; The "keys in X must appear in Y" precondition is checked, not assumed
  (test-error (dominates? '((a . 5)) '((b . 4))))

  ;; Omitted directions reproduce the historical higher-is-better dominance.
  (test #t (dominates? '((a . 5)) '((a . 4)) '()))
  ;; 'down inverts it: fewer is better, so 4 dominates 5.
  (test #f (dominates? '((a . 5)) '((a . 4)) '((a . down))))
  (test #t (dominates? '((a . 4)) '((a . 5)) '((a . down))))
  ;; Mixed directions in one alist — the case the whole facility exists for.
  ;; x: benefit 269, params 1.  y: benefit 200, params 3.
  ;; x is better on both once params is read as lower-is-better.
  (test #t (dominates? '((benefit . 269) (params . 1))
                       '((benefit . 200) (params . 3))
                       '((benefit . up) (params . down))))
  ;; Read all-up (the old behaviour) the same pair is incomparable, because
  ;; y "wins" on params by having more of them. This is the silent misrank.
  (test #f (dominates? '((benefit . 269) (params . 1))
                       '((benefit . 200) (params . 3))))
  ;; equal on a 'down axis is still not strict improvement
  (test #f (dominates? '((a . 4)) '((a . 4)) '((a . down)))))

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
    (test '("only") (cdr (assoc 'frontier result))))
  ;; Directions reach the frontier, not just dominates?. Lower `cost' is
  ;; better, so "cheap" dominates "dear" and the frontier is a singleton;
  ;; under the documentation-only form both would survive as incomparable.
  (let ((cands '(("cheap" ((gain . 5) (cost . 1)))
                 ("dear"  ((gain . 4) (cost . 9))))))
    (test '("cheap")
          (cdr (assoc 'frontier (pareto-frontier cands '((gain . up) (cost . down))))))
    (test 2
          (length (cdr (assoc 'frontier (pareto-frontier cands '(gain cost))))))
    ;; The frontier validates directions too, not just dominates?
    (test-error (pareto-frontier cands '((gain . up) (kost . down))))
    (test-error (pareto-frontier cands '(gain (cost . down)))))
  ;; A one-candidate frontier calls dominates? zero times, so the check has to
  ;; be in pareto-frontier as well or this case goes unexamined.
  (test-error (pareto-frontier '(("only" ((a . 1)))) '((aa . down)))))

(test-end)
(test-exit)
