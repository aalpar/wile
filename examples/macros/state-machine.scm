;;; state-machine.scm - State machine DSL via syntax-rules
;;;
;;; Demonstrates: syntax-rules with nested ellipsis, case dispatch,
;;;               tail-recursive state transitions, macros-as-compilers
;;;
;;; Usage: ./dist/scheme --file examples/macros/state-machine.scm

;; -----------------------------------------------------------------------
;; The define-state-machine macro
;;
;; Compiles a declarative state machine specification into a procedure
;; that takes a current state and event, and returns the next state
;; after executing any transition actions.
;;
;; The nested ellipsis pattern matches:
;;   - outer ... : multiple (state ...) clauses
;;   - inner ... : multiple (on ...) transitions within each state
;;   - action ... : multiple expressions per transition
;;
;; The generated code is a case-within-case dispatch — no interpretation
;; overhead at runtime.
;; -----------------------------------------------------------------------

(define-syntax define-state-machine
  (syntax-rules (state on =>)
    ((define-state-machine name
       (state state-name
         (on event-name => next-state action ...) ...)
       ...)
     (define (name current-state event)
       (case current-state
         ((state-name)
          (case event
            ((event-name) action ... 'next-state)
            ...
            (else (error "No transition from state"
                         'state-name event))))
         ...
         (else (error "Unknown state" current-state)))))))

;; Helper: run a machine through a sequence of events
(define (run-machine machine initial-state events)
  (let loop ((state initial-state)
             (events events))
    (if (null? events)
        state
        (let ((next (machine state (car events))))
          (loop next (cdr events))))))

;; -----------------------------------------------------------------------
;; Example 1: Traffic light controller
;; -----------------------------------------------------------------------

(display "=== State Machine DSL ===\n\n")

(define-state-machine traffic-light
  (state red
    (on timer => green
      (display "  Red → Green\n"))
    (on emergency => flashing
      (display "  Red → Flashing!\n")))
  (state green
    (on timer => yellow
      (display "  Green → Yellow\n"))
    (on emergency => flashing
      (display "  Green → Flashing!\n")))
  (state yellow
    (on timer => red
      (display "  Yellow → Red\n"))
    (on emergency => flashing
      (display "  Yellow → Flashing!\n")))
  (state flashing
    (on resume => red
      (display "  Flashing → Red (resumed)\n"))))

(display "--- Traffic light ---\n")
(let ((final (run-machine traffic-light 'red
               '(timer timer timer timer emergency resume timer))))
  (display "  Final state: ")
  (display final)
  (newline))

;; -----------------------------------------------------------------------
;; Example 2: Door lock
;; -----------------------------------------------------------------------

(define-state-machine door-lock
  (state locked
    (on correct-code => unlocked
      (display "  *click* Door unlocked\n"))
    (on wrong-code => locked
      (display "  *beep* Wrong code\n")))
  (state unlocked
    (on open => opened
      (display "  Door opened\n"))
    (on lock => locked
      (display "  Door locked\n"))
    (on timeout => locked
      (display "  Timeout — door re-locked\n")))
  (state opened
    (on close => unlocked
      (display "  Door closed\n"))))

(display "\n--- Door lock ---\n")
(run-machine door-lock 'locked
  '(wrong-code wrong-code correct-code open close lock correct-code timeout))

;; -----------------------------------------------------------------------
;; Example 3: HTTP request parser states
;; -----------------------------------------------------------------------

(define-state-machine http-parser
  (state request-line
    (on header-found => headers
      (display "  Parsing headers...\n"))
    (on error => failed
      (display "  Parse error in request line\n")))
  (state headers
    (on header-found => headers
      (display "  Another header...\n"))
    (on blank-line => body
      (display "  Headers done, reading body...\n"))
    (on error => failed
      (display "  Parse error in headers\n")))
  (state body
    (on complete => done
      (display "  Body received — request complete\n"))
    (on error => failed
      (display "  Parse error in body\n")))
  (state done
    (on reset => request-line
      (display "  Ready for next request\n")))
  (state failed
    (on reset => request-line
      (display "  Reset after failure\n"))))

(display "\n--- HTTP parser states ---\n")
(let ((final (run-machine http-parser 'request-line
               '(header-found header-found header-found blank-line
                 complete reset header-found error reset))))
  (display "  Final state: ")
  (display final)
  (newline))

(display "\nDeclarative state machines — compiled by macros, not interpreted.\n")
