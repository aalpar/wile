;; with-input-from-file and with-output-to-file macros
;; R7RS §6.13.2: These primitives temporarily redirect I/O to a file.
;;
;; These are implemented as macros using parameterize to ensure proper
;; integration with the continuation and dynamic-wind system.
;;
;; Fixes T3 from architectural review: Using parameterize (which expands to
;; dynamic-wind) ensures that parameter changes are properly tracked on the
;; winding stack. This makes them safe to use with call/cc and provides
;; correct restoration semantics.

(define-syntax with-input-from-file
  (syntax-rules ()
    ((with-input-from-file filename thunk)
     (call-with-input-file filename
       (lambda (port)
         (parameterize ((current-input-port port))
           (thunk)))))))

(define-syntax with-output-to-file
  (syntax-rules ()
    ((with-output-to-file filename thunk)
     (call-with-output-file filename
       (lambda (port)
         (parameterize ((current-output-port port))
           (thunk)))))))
