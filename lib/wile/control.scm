;;; (wile control) — Derived delimited continuation operators
;;;
;;; All operators are derived from three core primitives that Wile
;;; already provides:
;;;   call-with-continuation-prompt
;;;   call-with-composable-continuation
;;;   abort-current-continuation
;;;
;;; Operator matrix:
;;;
;;;   | Family          | Handler reinstalls prompt? | k wraps in prompt? |
;;;   |-----------------|---------------------------|--------------------|
;;;   | prompt/control  | Yes                       | No                 |
;;;   | reset/shift     | Yes                       | Yes                |
;;;   | prompt0/control0| No                        | No                 |
;;;   | reset0/shift0   | No                        | Yes                |
;;;
;;; Sources:
;;;   Felleisen 1988 (prompt/control)
;;;   Danvy & Filinski 1990 (reset/shift)
;;;   Hieb & Dybvig 1990 (spawn)
;;;   Queinnec & Serpette 1991 (set/cupto)

;; ─────────────────────────────────────────────────
;; Aliases
;; ─────────────────────────────────────────────────

(define call-with-escape-continuation call-with-exit)
(define call/ec call-with-exit)
(define new-prompt make-continuation-prompt-tag)

;; ─────────────────────────────────────────────────
;; Runtime helpers for prompt reinstallation
;;
;; Self-referential syntax-rules macros cause infinite
;; compile-time expansion: (reset-at t (thunk)) in the
;; handler template triggers another expansion of reset-at,
;; which contains another reset-at, ad infinitum.
;;
;; Moving the recursion to runtime (function self-call)
;; avoids this: function bodies are compiled once.
;; ─────────────────────────────────────────────────

;; %prompt-reinstall: call-with-continuation-prompt where the
;; handler reinstalls the same prompt around the abort thunk.
;; Used by prompt-at, reset-at, prompt, reset.
(define (%prompt-reinstall tag thunk)
  (call-with-continuation-prompt
    thunk
    tag
    (lambda (abort-thunk)
      (%prompt-reinstall tag abort-thunk))))

;; ─────────────────────────────────────────────────
;; prompt/control (Felleisen)
;; Handler reinstalls prompt. k is raw.
;; ─────────────────────────────────────────────────

(define-syntax prompt-at
  (syntax-rules ()
    ((_ tag body ...)
     (%prompt-reinstall tag (lambda () body ...)))))

(define-syntax control-at
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation t (lambda () body ...)))
         t)))))

(define-syntax prompt
  (syntax-rules ()
    ((_ body ...)
     (prompt-at (default-continuation-prompt-tag) body ...))))

(define-syntax control
  (syntax-rules ()
    ((_ k body ...)
     (control-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; reset/shift (Danvy & Filinski)
;; Handler reinstalls prompt. k wraps in reset.
;; ─────────────────────────────────────────────────

(define-syntax reset-at
  (syntax-rules ()
    ((_ tag body ...)
     (%prompt-reinstall tag (lambda () body ...)))))

(define-syntax shift-at
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (raw-k)
           (abort-current-continuation
             t
             (lambda ()
               (let ((k (lambda args
                          (%prompt-reinstall t (lambda () (apply raw-k args))))))
                 body ...))))
         t)))))

(define-syntax reset
  (syntax-rules ()
    ((_ body ...)
     (reset-at (default-continuation-prompt-tag) body ...))))

(define-syntax shift
  (syntax-rules ()
    ((_ k body ...)
     (shift-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; prompt0/control0
;; Handler does NOT reinstall prompt. k is raw.
;; ─────────────────────────────────────────────────

(define-syntax prompt0-at
  (syntax-rules ()
    ((_ tag body ...)
     (call-with-continuation-prompt
       (lambda () body ...)
       tag
       (lambda (thunk) (thunk))))))

(define-syntax control0-at
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation t (lambda () body ...)))
         t)))))

(define-syntax prompt0
  (syntax-rules ()
    ((_ body ...)
     (prompt0-at (default-continuation-prompt-tag) body ...))))

(define-syntax control0
  (syntax-rules ()
    ((_ k body ...)
     (control0-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; reset0/shift0
;; Handler does NOT reinstall prompt. k wraps in reset.
;; ─────────────────────────────────────────────────

(define-syntax reset0-at
  (syntax-rules ()
    ((_ tag body ...)
     (call-with-continuation-prompt
       (lambda () body ...)
       tag
       (lambda (thunk) (thunk))))))

(define-syntax shift0-at
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (raw-k)
           (abort-current-continuation
             t
             (lambda ()
               (let ((k (lambda args
                          (%prompt-reinstall t (lambda () (apply raw-k args))))))
                 body ...))))
         t)))))

(define-syntax reset0
  (syntax-rules ()
    ((_ body ...)
     (reset0-at (default-continuation-prompt-tag) body ...))))

(define-syntax shift0
  (syntax-rules ()
    ((_ k body ...)
     (shift0-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; spawn (Hieb & Dybvig)
;; Equivalent to (control k (k body))
;; ─────────────────────────────────────────────────

(define-syntax spawn-at
  (syntax-rules ()
    ((_ tag body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation
             t
             (lambda () (k (begin body ...)))))
         t)))))

(define-syntax spawn
  (syntax-rules ()
    ((_ body ...)
     (spawn-at (default-continuation-prompt-tag) body ...))))

;; ─────────────────────────────────────────────────
;; set/cupto (Queinnec & Serpette)
;; Aliases for prompt0/control0.
;; ─────────────────────────────────────────────────

(define-syntax set-at
  (syntax-rules ()
    ((_ tag body ...)
     (prompt0-at tag body ...))))

(define-syntax cupto-at
  (syntax-rules ()
    ((_ tag k body ...)
     (control0-at tag k body ...))))

(define-syntax set
  (syntax-rules ()
    ((_ body ...)
     (prompt0 body ...))))

(define-syntax cupto
  (syntax-rules ()
    ((_ k body ...)
     (control0 k body ...))))

;; ─────────────────────────────────────────────────
;; Continuation mark utilities
;; ─────────────────────────────────────────────────

;; continuation-mark-set->iterator returns a procedure that yields one
;; vector per frame on each call, or #f when exhausted.
;; Built on continuation-mark-set->list*.
(define (continuation-mark-set->iterator cms keys . args)
  (let ((none-v (if (pair? args) (car args) #f)))
    (let ((remaining (continuation-mark-set->list* cms keys none-v)))
      (lambda ()
        (if (null? remaining)
            #f
            (let ((v (car remaining)))
              (set! remaining (cdr remaining))
              v))))))

;; continuation-mark-set->context extracts source-location marks using the
;; well-known key 'wile/source-location. Users can install marks with this
;; key via with-continuation-mark; this function collects them.
(define (continuation-mark-set->context marks)
  (continuation-mark-set->list marks 'wile/source-location))
