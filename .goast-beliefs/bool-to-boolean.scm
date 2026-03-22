(import (wile goast belief))

;; Belief: functions producing Scheme boolean values should prefer
;; BoolToBoolean over manual if/else with TrueValue/FalseValue.
;; Sites: functions that call BoolToBoolean OR SetValue (boolean producers)
;; Expect: should use BoolToBoolean for the conversion
;; Threshold is lower (0.66) since many SetValue calls aren't boolean-related
(define-belief "bool-to-boolean"
  (sites (functions-matching
    (any-of
      (contains-call "BoolToBoolean")
      (all-of
        (contains-call "SetValue")
        (contains-call "TrueValue" "FalseValue")))))
  (expect (contains-call "BoolToBoolean"))
  (threshold 0.66 3))
