;;; error-source-tracking.scm - Exception handling with source locations
;;;
;;; Demonstrates: guard, error, raise, error-object-message,
;;;               error-object-irritants, source-tracked error output
;;;
;;; Usage: ./dist/scheme --file examples/basics/error-source-tracking.scm
;;;
;;; When run via the interpreter, uncaught exceptions display the source
;;; filename, line, and column where the error was raised, plus a VM
;;; stack trace. Caught exceptions use standard R7RS condition accessors.

;; -----------------------------------------------------------------------
;; 1. Caught errors with guard
;;
;; guard catches exceptions and lets handlers inspect the condition.
;; error-object-message and error-object-irritants extract the parts
;; passed to (error ...).
;; -----------------------------------------------------------------------

(display "--- Caught errors ---")
(newline)

;; Basic error caught by guard
(guard (exn
        (#t
         (display "  caught error: ")
         (display (error-object-message exn))
         (display ", irritants: ")
         (display (error-object-irritants exn))
         (newline)))
  (error "example failure" 42 "extra"))

;; Non-error condition (raise a plain value)
(guard (exn
        (#t
         (display "  caught non-error: ")
         (display exn)
         (newline)))
  (raise "plain string condition"))

;; -----------------------------------------------------------------------
;; 2. Nested call chain
;;
;; Defines a -> b -> c chain where c raises an error.
;; The guard handler catches and displays the message.
;; When the error escapes to the Go runtime, the stack trace shows
;; the call depth through foreign function boundaries.
;; -----------------------------------------------------------------------

(display "--- Nested call chain ---")
(newline)

(define (fn-c x)
  (error "deep failure" x))

(define (fn-b x)
  (fn-c (+ x 1)))

(define (fn-a x)
  (fn-b (+ x 1)))

(guard (exn
        (#t
         (display "  caught from chain: ")
         (display (error-object-message exn))
         (display ", irritants: ")
         (display (error-object-irritants exn))
         (newline)))
  (fn-a 0))

;; -----------------------------------------------------------------------
;; 3. Division by zero
;;
;; Arithmetic errors originate from Go primitive panic recovery and are
;; converted to Scheme exceptions automatically.
;; -----------------------------------------------------------------------

(display "--- Arithmetic error ---")
(newline)

(guard (exn
        (#t
         (display "  caught division error: ")
         (display (error-object-message exn))
         (newline)))
  (/ 1 0))

;; -----------------------------------------------------------------------
;; 4. Uncaught error (intentional)
;;
;; This final expression raises WITHOUT guard. The interpreter's error
;; handler prints the full source-tracked message with filename, line,
;; column, and stack trace to stderr.
;;
;; Expected output (approximately):
;;   error-source-tracking.scm:NN:CC: error: intentional uncaught error
;;   <stack trace>
;; -----------------------------------------------------------------------

(display "--- Uncaught error (next line is the interpreter error output) ---")
(newline)

(error "intentional uncaught error" "this demonstrates source tracking")
