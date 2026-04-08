;; I/O port procedures
;;
;; Scheme procedure definitions for the io extension.
;; Loaded after io primitives are registered (close-port is available).
;;
;; This file is embedded at compile-time via go:embed.

;; call-with-port — R7RS §6.13.1
;; Calls proc with port as argument. Closes port when proc returns.
;; Preserves multiple return values from proc.
(define (call-with-port port proc)
  "Call PROC with PORT as its sole argument, then close PORT.\nIf PROC returns normally, the port is closed and PROC's\nvalue(s) are returned. If PROC raises an exception, the\nport is not closed (R7RS §6.13.1).\n\nParameters:\n  port : port\n  proc : procedure\nReturns: any\nCategory: ports\n\nSee also: `call-with-input-file', `call-with-output-file'."
  (let ((results (call-with-values (lambda () (proc port)) list)))
    (close-port port)
    (apply values results)))
