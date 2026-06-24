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
  "Install a continuation prompt tagged TAG, run THUNK, and\nreinstall the same prompt if an abort delivers a new thunk.\nUsed internally by prompt-at, reset-at, prompt, and reset to\nprovide handler-reinstalling behavior.\n\nExamples:\n  (%prompt-reinstall (default-continuation-prompt-tag)\n    (lambda () 42))  => 42\n\nParameters:\n  tag : any\n  thunk : procedure\nReturns: any\nCategory: control\n\nSee also: `call-with-continuation-prompt', `abort-current-continuation'."
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
  "Delimit the current continuation with prompt tag TAG (Felleisen prompt,\nexplicit tag). Establishes a boundary that a matching control-at captures\nup to; the prompt is reinstalled when an aborting control-at delivers a\nthunk to it.\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `prompt', `control-at', `reset-at'."
  (syntax-rules ()
    ((_ tag body ...)
     (%prompt-reinstall tag (lambda () body ...)))))

(define-syntax control-at
  "Capture the delimited continuation up to the prompt tagged TAG and bind it\nto K (Felleisen control, explicit tag). K is a composable continuation that\ndoes not reinstall a prompt when invoked; BODY runs with the captured\ncontext aborted.\n\nParameters:\n  tag : continuation prompt tag\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `control', `prompt-at', `shift-at'."
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation t (lambda () body ...)))
         t)))))

(define-syntax prompt
  "Delimit the current continuation with the default prompt tag (Felleisen\nprompt). Establishes the boundary that a matching control captures up to.\nShorthand for prompt-at with the default continuation prompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `control', `prompt-at', `reset'."
  (syntax-rules ()
    ((_ body ...)
     (prompt-at (default-continuation-prompt-tag) body ...))))

(define-syntax control
  "Capture the current delimited continuation up to the nearest prompt and\nbind it to K (Felleisen control). K is a composable continuation that does\nnot reinstall a prompt when invoked. Shorthand for control-at with the\ndefault continuation prompt tag.\n\nParameters:\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `prompt', `shift', `control-at'."
  (syntax-rules ()
    ((_ k body ...)
     (control-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; reset/shift (Danvy & Filinski)
;; Handler reinstalls prompt. k wraps in reset.
;; ─────────────────────────────────────────────────

(define-syntax reset-at
  "Delimit the current continuation with prompt tag TAG (Danvy & Filinski\nreset, explicit tag). Like prompt-at; the prompt is reinstalled when a\nmatching shift-at aborts to it.\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `reset', `shift-at', `prompt-at'."
  (syntax-rules ()
    ((_ tag body ...)
     (%prompt-reinstall tag (lambda () body ...)))))

(define-syntax shift-at
  "Capture the delimited continuation up to the reset tagged TAG and bind it\nto K (Danvy & Filinski shift, explicit tag). Unlike control-at, invoking K\nre-delimits its result within a fresh reset, so K behaves as a function\nfrom values to values.\n\nParameters:\n  tag : continuation prompt tag\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `shift', `reset-at', `control-at'."
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
  "Delimit the current continuation with the default prompt tag (Danvy &\nFilinski reset). Equivalent to prompt; provided under the reset/shift name.\nShorthand for reset-at with the default continuation prompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `shift', `reset-at', `prompt'."
  (syntax-rules ()
    ((_ body ...)
     (reset-at (default-continuation-prompt-tag) body ...))))

(define-syntax shift
  "Capture the current delimited continuation up to the nearest reset and bind\nit to K (Danvy & Filinski shift). Invoking K re-delimits its result within a\nfresh reset, so K is a function from values to values. Shorthand for shift-at\nwith the default continuation prompt tag.\n\nParameters:\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `reset', `control', `shift-at'."
  (syntax-rules ()
    ((_ k body ...)
     (shift-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; prompt0/control0
;; Handler does NOT reinstall prompt. k is raw.
;; ─────────────────────────────────────────────────

(define-syntax prompt0-at
  "Delimit the current continuation with prompt tag TAG (prompt0, explicit\ntag). Unlike prompt-at, the prompt is NOT reinstalled when a matching\ncontrol0-at aborts to it — the delimiter is consumed.\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `prompt0', `control0-at', `prompt-at'."
  (syntax-rules ()
    ((_ tag body ...)
     (call-with-continuation-prompt
       (lambda () body ...)
       tag
       (lambda (thunk) (thunk))))))

(define-syntax control0-at
  "Capture the delimited continuation up to the prompt tagged TAG and bind it\nto K (control0, explicit tag). Like control-at, but pairs with prompt0-at,\nwhich does not reinstall the prompt.\n\nParameters:\n  tag : continuation prompt tag\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `control0', `prompt0-at', `control-at'."
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (k)
           (abort-current-continuation t (lambda () body ...)))
         t)))))

(define-syntax prompt0
  "Delimit the current continuation with the default prompt tag (prompt0).\nLike prompt but the delimiter is not reinstalled when a matching control0\naborts to it. Shorthand for prompt0-at with the default prompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `control0', `prompt0-at', `prompt'."
  (syntax-rules ()
    ((_ body ...)
     (prompt0-at (default-continuation-prompt-tag) body ...))))

(define-syntax control0
  "Capture the current delimited continuation up to the nearest prompt0 and\nbind it to K (control0). Pairs with prompt0, which does not reinstall the\nprompt. Shorthand for control0-at with the default prompt tag.\n\nParameters:\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `prompt0', `control', `control0-at'."
  (syntax-rules ()
    ((_ k body ...)
     (control0-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; reset0/shift0
;; Handler does NOT reinstall prompt. k wraps in reset0.
;; ─────────────────────────────────────────────────

(define-syntax reset0-at
  "Delimit the current continuation with prompt tag TAG (reset0, explicit\ntag). Like reset-at but the delimiter is not reinstalled when a matching\nshift0-at aborts to it.\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `reset0', `shift0-at', `reset-at'."
  (syntax-rules ()
    ((_ tag body ...)
     (call-with-continuation-prompt
       (lambda () body ...)
       tag
       (lambda (thunk) (thunk))))))

(define-syntax shift0-at
  "Capture the delimited continuation up to the reset0 tagged TAG and bind it\nto K (shift0, explicit tag). Like shift-at but K re-delimits with reset0,\nwhich does not reinstall the prompt.\n\nParameters:\n  tag : continuation prompt tag\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `shift0', `reset0-at', `shift-at'."
  (syntax-rules ()
    ((_ tag k body ...)
     (let ((t tag))
       (call-with-composable-continuation
         (lambda (raw-k)
           (abort-current-continuation
             t
             (lambda ()
               (let ((k (lambda args
                          (call-with-continuation-prompt
                            (lambda () (apply raw-k args))
                            t
                            (lambda (thunk) (thunk))))))
                 body ...))))
         t)))))

(define-syntax reset0
  "Delimit the current continuation with the default prompt tag (reset0). Like\nreset but the delimiter is not reinstalled when a matching shift0 aborts.\nShorthand for reset0-at with the default prompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `shift0', `reset0-at', `reset'."
  (syntax-rules ()
    ((_ body ...)
     (reset0-at (default-continuation-prompt-tag) body ...))))

(define-syntax shift0
  "Capture the current delimited continuation up to the nearest reset0 and bind\nit to K (shift0). Like shift but K re-delimits with reset0, which does not\nreinstall the prompt. Shorthand for shift0-at with the default prompt tag.\n\nParameters:\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `reset0', `control0', `shift0-at'."
  (syntax-rules ()
    ((_ k body ...)
     (shift0-at (default-continuation-prompt-tag) k body ...))))

;; ─────────────────────────────────────────────────
;; spawn (Hieb & Dybvig)
;; Equivalent to (control k (k body))
;; ─────────────────────────────────────────────────

(define-syntax spawn-at
  "Capture the delimited continuation up to the prompt tagged TAG and apply it\nimmediately to BODY's value, under a fresh prompt (Hieb & Dybvig spawn,\nexplicit tag). Equivalent to (control-at tag k (k (begin body ...))).\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `spawn', `control-at'."
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
  "Capture the current delimited continuation and apply it immediately to\nBODY's value, under a fresh prompt (Hieb & Dybvig spawn). Equivalent to\n(control k (k body)). Shorthand for spawn-at with the default prompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `spawn-at', `control'."
  (syntax-rules ()
    ((_ body ...)
     (spawn-at (default-continuation-prompt-tag) body ...))))

;; ─────────────────────────────────────────────────
;; set/cupto (Queinnec & Serpette)
;; Aliases for prompt0/control0.
;; ─────────────────────────────────────────────────

(define-syntax set-at
  "Delimit the current continuation with prompt tag TAG (Queinnec & Serpette\nset, explicit tag). An alias for prompt0-at: the delimiter is not\nreinstalled on abort.\n\nParameters:\n  tag : continuation prompt tag\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `set', `cupto-at', `prompt0-at'."
  (syntax-rules ()
    ((_ tag body ...)
     (prompt0-at tag body ...))))

(define-syntax cupto-at
  "Capture the delimited continuation up to the prompt tagged TAG and bind it\nto K (Queinnec & Serpette cupto; the name abbreviates control-up-to,\nexplicit tag). An alias for control0-at.\n\nParameters:\n  tag : continuation prompt tag\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `cupto', `set-at', `control0-at'."
  (syntax-rules ()
    ((_ tag k body ...)
     (control0-at tag k body ...))))

(define-syntax set
  "Delimit the current continuation with the default prompt tag (Queinnec &\nSerpette set). An alias for prompt0. Shorthand for set-at with the default\nprompt tag.\n\nParameters:\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `cupto', `set-at', `prompt0'."
  (syntax-rules ()
    ((_ body ...)
     (prompt0 body ...))))

(define-syntax cupto
  "Capture the current delimited continuation up to the nearest set and bind it\nto K (Queinnec & Serpette cupto; the name abbreviates control-up-to). An\nalias for control0. Shorthand for cupto-at with the default prompt tag.\n\nParameters:\n  k : identifier bound to the captured continuation\n  body : expression ...\nReturns: any\nCategory: control\n\nSee also: `set', `cupto-at', `control0'."
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
  "Return a stateful iterator over continuation mark set CMS.\nEach call to the returned procedure yields one vector of mark\nvalues for KEYS from the next frame, or #f when exhausted.\nThe optional third argument supplies the value used for frames\nthat lack a given key (defaults to #f).\n\nExamples:\n  (with-continuation-mark 'k 1\n    (let ((it (continuation-mark-set->iterator\n                (current-continuation-marks) '(k))))\n      (it)))  => #(1)\n\nParameters:\n  cms : any\n  keys : list\nReturns: procedure\nCategory: control\n\nSee also: `continuation-mark-set->context', `continuation-mark-set->list*'."
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
  "Extract source-location marks from continuation mark set MARKS.\nCollects all values stored under the well-known key\n'wile/source-location, returning them as a list ordered from\ninnermost frame outward.\n\nExamples:\n  (with-continuation-mark 'wile/source-location \"foo.scm:10\"\n    (continuation-mark-set->context\n      (current-continuation-marks)))  => (\"foo.scm:10\")\n\nParameters:\n  marks : any\nReturns: list\nCategory: control\n\nSee also: `continuation-mark-set->iterator', `continuation-mark-set->list'."
  (continuation-mark-set->list marks 'wile/source-location))
