(define-library (wile algebra pareto)
  (description "Pareto dominance and multi-objective frontier computation.")
  (export dominates? pareto-frontier
          factor-leq? factor-less?
          factor-direction normalize-directions)
  (import (scheme base))
  (include "pareto.scm"))
