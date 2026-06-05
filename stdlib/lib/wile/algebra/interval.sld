(define-library (wile algebra interval)
  (description "Interval arithmetic with infinity-aware operations and interval lattice.")
  (export interval-lattice
          interval-add interval-sub interval-mul
          abstract-interval interval-widen
          inf<= inf-min inf-max inf+ inf- inf*)
  (import (scheme base)
          (wile algebra lattice))
  (include "interval.scm"))
