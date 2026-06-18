;; ER macros defined in a library for cross-library testing.
;; These macros use rename and compare to verify hygiene
;; works correctly when the macro is used from a different module.

;; my-or: uses rename for let/if/tmp to ensure hygiene across libraries
(define-syntax my-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))

;; aif: anaphoric if — 'it' is deliberately NOT renamed
(define-syntax aif
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((test (cadr form))
            (then (caddr form))
            (els  (if (null? (cdddr form)) #f (cadddr form))))
        (list (rename 'let) (list (list 'it test))
              (list (rename 'if) 'it then els))))))

;; literal-check: uses compare to match a literal keyword
(define-syntax literal-check
  (er-macro-transformer
    (lambda (form rename compare)
      (if (compare (cadr form) (rename 'magic))
          (list (rename 'quote) 'found-magic)
          (list (rename 'quote) 'not-magic)))))
