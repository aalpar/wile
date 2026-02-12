;;; anaphoric.scm - Intentionally breaking hygiene
;;;
;;; Demonstrates: Anaphoric macros, deliberate variable introduction
;;; Wile-specific: Techniques for controlled hygiene violation
;;;
;;; Usage: ./dist/scheme --file examples/macros/anaphoric.scm

;; Anaphoric macros deliberately introduce bindings that user code can reference.
;; Common in languages like Common Lisp (e.g., aif, awhen).
;; In R7RS, we achieve this by having the user pass the binding name explicitly.

(display "=== Anaphoric Macros in Wile ===\n")
(newline)

;; Note: True anaphoric macros (implicit 'it' binding) aren't directly
;; possible in R7RS syntax-rules due to hygiene. We demonstrate the
;; pattern by having users name the binding explicitly.

(display "Anaphoric patterns in R7RS:\n")
(display "  syntax-rules is hygienic, so we use explicit binding names\n")
(display "  instead of implicit 'it' variables.\n")
(newline)

;; aif-style: bind test result to a name
(define-syntax aif
  (syntax-rules ()
    ((aif var test then else)
     (let ((var test))
       (if var then else)))))

(display "aif (anaphoric if):\n")
(display "  ")
(aif result (assoc 'b '((a 1) (b 2) (c 3)))
  (begin
    (display "Found: ")
    (display result)
    (newline))
  (display "Not found\n"))
(newline)

;; awhen-style: only execute body if test is true
(define-syntax awhen
  (syntax-rules ()
    ((awhen var test body ...)
     (let ((var test))
       (when var
         body ...)))))

(display "awhen (anaphoric when):\n")
(display "  ")
(awhen value (+ 2 3)
  (display "The value is ")
  (display value)
  (newline))
(newline)

;; acond-style: bind each test result
(define-syntax acond
  (syntax-rules ()
    ((acond)
     (if #f #f))
    ((acond (var test body ...) rest ...)
     (let ((var test))
       (if var
           (begin body ...)
           (acond rest ...))))))

(display "acond (anaphoric cond):\n")
(display "  ")
(acond
 (x (assoc 'z '((a 1) (b 2)))
    (display "Found z: ")
    (display x)
    (newline))
 (x (assoc 'b '((a 1) (b 2)))
    (display "Found b: ")
    (display x)
    (newline))
 (else
  (display "Nothing found\n")))
(newline)

;; Implicit continuation pattern
(define-syntax with-continuation
  (syntax-rules ()
    ((with-continuation k body ...)
     (call/cc
      (lambda (k)
        body ...)))))

(display "with-continuation (explicit continuation binding):\n")
(display "  ")
(with-continuation escape
  (display "Starting...")
  (newline)
  (display "  ")
  (when (> 5 3)
    (escape "Escaped early!"))
  (display "This won't print\n"))
(newline)

;; Building block accumulator
(define-syntax with-builder
  (syntax-rules ()
    ((with-builder add get body ...)
     (let ((items '()))
       (let ((add (lambda (x) (set! items (cons x items))))
             (get (lambda () (reverse items))))
         body ...)))))

(display "with-builder (builder pattern):\n")
(display "  ")
(let ((result
       (with-builder add get
         (add 1)
         (add 2)
         (add 3)
         (get))))
  (display "Built list: ")
  (display result)
  (newline))
(newline)

;; Named let pattern (this is standard R7RS, but demonstrates the concept)
(display "Named let (standard R7RS, but anaphoric in spirit):\n")
(display "  ")
(let loop ((n 5))
  (when (> n 0)
    (display n)
    (display " ")
    (loop (- n 1))))
(newline)
(display "  'loop' is implicitly bound by named let\n")
(newline)

;; Practical example: optional chaining
(define-syntax chain
  (syntax-rules (=>)
    ((chain value)
     value)
    ((chain value => proc rest ...)
     (let ((result (proc value)))
       (if result
           (chain result rest ...)
           #f)))))

(display "chain (optional chaining with =>):\n")
(define data '((user (name "Alice") (age 30))))

(display "  ")
(let ((result (chain data
                     => (lambda (d) (assoc 'user d))
                     => cdr
                     => (lambda (u) (assoc 'name u))
                     => cdr
                     => car)))
  (display "Extracted name: ")
  (display result)
  (newline))
(newline)

;; with-slots-like pattern
(define-syntax with-bindings
  (syntax-rules ()
    ((with-bindings ((var val) ...) body ...)
     (let ((var val) ...)
       body ...))))

(display "with-bindings (explicit multi-binding):\n")
(display "  ")
(with-bindings ((x 10)
                (y 20)
                (z 30))
  (display "x=")
  (display x)
  (display ", y=")
  (display y)
  (display ", z=")
  (display z)
  (newline))
(newline)

;; Collecting results
(define-syntax collect
  (syntax-rules ()
    ((collect result (loop-form ...) body ...)
     (let ((result '()))
       loop-form ...
       (lambda ()
         body ...
         (set! result (cons (begin body ...) result)))
       (reverse result)))))

;; Error handling with bound error
(define-syntax with-error-handler
  (syntax-rules ()
    ((with-error-handler err body ...)
     (guard (err (else err))
       body ...))))

(display "with-error-handler (bind caught error):\n")
(display "  ")
(let ((result
       (with-error-handler e
         (/ 1 0))))
  (display "Caught error: ")
  (display (error-object-message result))
  (newline))
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Anaphoric patterns in R7RS:\n")
(display "  • syntax-rules is hygienic by design\n")
(display "  • Use explicit binding names instead of implicit 'it'\n")
(display "  • Pattern: (macro binding-name test body ...)\n")
(display "  • User controls the name, avoiding capture risks\n")
(display "  • Common patterns: aif, awhen, acond, with-continuation\n")
(newline)
(display "This approach maintains safety while providing convenience!\n")
