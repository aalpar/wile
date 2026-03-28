(define-library (wile control)
  (description "Extended control flow: reset, shift, delimited continuations.")
  (export
    ;; Escape continuation aliases
    call-with-escape-continuation call/ec

    ;; Tag alias
    new-prompt

    ;; prompt/control (Felleisen)
    prompt control prompt-at control-at

    ;; reset/shift (Danvy & Filinski)
    reset shift reset-at shift-at

    ;; prompt0/control0
    prompt0 control0 prompt0-at control0-at

    ;; reset0/shift0
    reset0 shift0 reset0-at shift0-at

    ;; spawn (Hieb & Dybvig)
    spawn spawn-at

    ;; set/cupto (Queinnec & Serpette)
    set cupto set-at cupto-at

    ;; Continuation mark utilities
    continuation-mark-set->iterator
    continuation-mark-set->context)
  (import (scheme base))
  (include "control.scm"))
