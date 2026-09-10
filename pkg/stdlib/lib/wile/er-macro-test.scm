;; ER macros defined in a library for cross-library testing.
;; These macros use rename and compare to verify hygiene
;; works correctly when the macro is used from a different module.

;; A procedural transformer body compiles at phase 1, where the dialect declares
;; only the macro-writing vocabulary — car and cdr, not the bootstrap-Scheme
;; cadr/caddr/cdddr/cadddr these bodies use. So the accessors must be imported
;; for-syntax; (scheme cxr) is the second import because caddr and cadddr live
;; there rather than in base.sld.
;;
;; The import goes in the INCLUDED BODY, not beside the (import (scheme base))
;; in the .sld. Measured: a for-syntax import in a define-library's declaration
;; position silently drops the phase shift, while the same form inside the body
;; composes it correctly. Filed in TODO.md; moving this line into the .sld makes
;; every macro below unbound again, with no diagnostic naming the move.
(import (for-syntax (scheme base)))
(import (for-syntax (scheme cxr)))

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

;; er-contract: reports (identifier? symbol?) of the first form element, so a
;; caller can read what shape the proc receives across a LIBRARY boundary — the
;; one path on which the form leaves the engine's own namespace.
;;
;; The answer is layer-specific: (#t #f) under the Scheme syntax forms, where the
;; form is a spine with identifier leaves kept (design Q6), and (#f #t) under the
;; Go ones, which hand the proc a fully unwrapped s-expression. So the assertion
;; is TestP2_ERContractCrossLibrary in pkg/wile, which selects the layer, and not
;; a row in integration/testdata/er_macro_cross_library.scm, which must stay green
;; under both.
(define-syntax er-contract
  (er-macro-transformer
    (lambda (form rename compare)
      (list (rename 'list) (identifier? (cadr form)) (symbol? (cadr form))))))
