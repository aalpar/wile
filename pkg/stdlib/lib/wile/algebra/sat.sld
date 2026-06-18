(define-library (wile algebra sat)
  (description "Propositional SAT decision via a CDCL kernel. Use sat? for arbitrary boolean formulas (Tseitin-transformed internally) and sat-cnf? for raw CNF. Returns #t / #f / 'unknown (third value when conflict budget or ctx is exhausted). boolean-decide-equivalent? closes the De Morgan / complement-law gap left by symbolic-boolean-equivalent?.")
  (export sat? sat-model
          sat-cnf? sat-cnf-model
          boolean-decide-sat? boolean-decide-equivalent?
          cnf->flat)
  (import (scheme base))
  (include "sat.scm"))
