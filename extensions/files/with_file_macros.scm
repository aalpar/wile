;; with-input-from-file and with-output-to-file macros
;; R7RS §6.13.2: These primitives temporarily redirect I/O to a file.
;;
;; These are implemented as macros using parameterize to ensure proper
;; integration with the continuation system. parameterize uses
;; with-continuation-mark, so port bindings ride on the continuation
;; frames and compose correctly with call/cc and composable continuations.

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
