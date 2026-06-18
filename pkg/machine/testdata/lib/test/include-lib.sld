(define-library (test include-lib)
  (export double-base base-value)
  (include "include-body.scm")
  (begin
    ;; double-base is a macro that references base-value from the
    ;; included file. The macro template's free identifier base-value
    ;; carries the library scope (because it's in the begin body).
    ;; Without scope stamping on included forms, base-value's binding
    ;; has empty scopes, and the scoped reference fails to find it.
    (define-syntax double-base
      (syntax-rules ()
        ((double-base) (* 2 base-value))))))
