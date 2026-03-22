(import (wile goast belief))

;; Belief: in MachineContext, when 'pc' is stored, 'template' is typically
;; also stored (they represent a code position together).
;; Exploratory belief — lower threshold to discover deviations.
(define-belief "pc-template-co-mutation"
  (sites (functions-matching
    (stores-to-fields "MachineContext" "pc")))
  (expect (co-mutated "pc" "template"))
  (threshold 0.66 3))
