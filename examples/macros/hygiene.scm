;;; hygiene.scm - Hygienic macro variable capture prevention
;;;
;;; Demonstrates: Automatic hygiene, scope sets, preventing unintended capture
;;; Wile-specific: "Sets of scopes" hygiene model (Flatt 2016) - same as Racket
;;;
;;; Usage: ./dist/scheme --file examples/macros/hygiene.scm

;; Hygienic macros prevent accidental variable capture.
;; Wile uses "sets of scopes" to track binding origins.

(display "=== Macro Hygiene in Wile ===\n")
(newline)

;; Example 1: No accidental capture
(display "Example 1: Variables introduced by macros don't capture user variables\n")
(newline)

(define-syntax my-swap
  (syntax-rules ()
    ((my-swap a b)
     (let ((temp a))      ; 'temp' is macro-introduced
       (set! a b)
       (set! b temp)))))

(display "User code with 'temp' variable:\n")
(let ((temp 100)
      (x 1)
      (y 2))
  (display "  Before: temp=")
  (display temp)
  (display ", x=")
  (display x)
  (display ", y=")
  (display y)
  (newline)
  (my-swap x y)
  (display "  After (my-swap x y): temp=")
  (display temp)
  (display " (unchanged!), x=")
  (display x)
  (display ", y=")
  (display y)
  (newline))
(display "  The macro's 'temp' doesn't interfere with the user's 'temp'\n")
(newline)

;; Example 2: Macro-introduced bindings are distinct
(display "Example 2: Each macro expansion creates distinct bindings\n")
(newline)

(define-syntax with-temp
  (syntax-rules ()
    ((with-temp value body ...)
     (let ((temp value))
       body ...))))

(display "Nested macro expansions:\n")
(display "  ")
(with-temp 1
  (with-temp 2
    (display "Inner temp is 2, not 1")
    (newline)))
(display "  Each 'temp' has its own scope\n")
(newline)

;; Example 3: No output variable capture
(display "Example 3: Macro output doesn't capture user-intended variables\n")
(newline)

(define-syntax my-for
  (syntax-rules ()
    ((my-for var from to body ...)
     (let loop ((var from))
       (when (<= var to)
         body ...
         (loop (+ var 1)))))))

(display "User code:\n")
(let ((loop "I'm a user variable"))
  (display "  loop = \"")
  (display loop)
  (display "\"\n")
  (display "  Using (my-for i 1 3 ...):\n")
  (display "    ")
  (my-for i 1 3
    (display i)
    (display " "))
  (newline)
  (display "  loop = \"")
  (display loop)
  (display "\" (unchanged!)\n")
  (display "  The macro's 'loop' doesn't shadow the user's 'loop'\n"))
(newline)

;; Example 4: Referential transparency
(display "Example 4: Macros refer to definitions from their definition site\n")
(newline)

;; Define a helper used by the macro
(define (helper-function x)
  (* x 2))

(define-syntax use-helper
  (syntax-rules ()
    ((use-helper value)
     (helper-function value))))

(display "Macro defined with helper-function:\n")
(display "  (use-helper 5) = ")
(display (use-helper 5))
(newline)
(newline)

;; Now shadow helper-function at the call site
(display "Shadowing helper-function at call site:\n")
(let ((helper-function (lambda (x) (+ x 100))))  ; Different definition!
  (display "  Local helper-function adds 100\n")
  (display "  (use-helper 5) = ")
  (display (use-helper 5))
  (display " (still uses original!)\n")
  (display "  Macros maintain referential transparency\n"))
(newline)

;; Example 5: Pattern variables don't leak
(display "Example 5: Pattern-matched variables are properly scoped\n")
(newline)

(define-syntax compute-and-display
  (syntax-rules ()
    ((compute-and-display expr)
     (let ((result expr))
       (display "Result: ")
       (display result)
       (newline)))))

(display "User code:\n")
(let ((result "user value"))
  (display "  result = \"")
  (display result)
  (display "\"\n")
  (display "  Calling macro:\n  ")
  (compute-and-display (+ 2 3))
  (display "  result = \"")
  (display result)
  (display "\" (unchanged!)\n"))
(newline)

;; Example 6: Recursive macros maintain hygiene
(display "Example 6: Recursive macros don't accumulate bindings\n")
(newline)

(define-syntax sum
  (syntax-rules ()
    ((sum) 0)
    ((sum x) x)
    ((sum x y ...) (+ x (sum y ...)))))

(display "Recursive macro expansion:\n")
(display "  (sum 1 2 3 4 5) = ")
(display (sum 1 2 3 4 5))
(newline)
(display "  Each expansion creates fresh bindings\n")
(newline)

;; Example 7: Counter-example - what would break without hygiene
(display "Example 7: Understanding the problem hygiene solves\n")
(newline)

(display "Without hygiene, this macro would be dangerous:\n")
(display "(define-syntax bad-swap\n")
(display "  (syntax-rules ()\n")
(display "    ((bad-swap a b)\n")
(display "     (let ((temp a))  ; Could capture user's 'temp'!\n")
(display "       (set! a b)\n")
(display "       (set! b temp)))))\n")
(newline)
(display "But Wile's hygiene prevents capture automatically.\n")
(display "The 'temp' in the macro is distinct from any 'temp' in user code.\n")
(newline)

;; Example 8: Macro expanding to macro maintains hygiene
(display "Example 8: Multi-level macro expansion preserves hygiene\n")
(newline)

(define-syntax double
  (syntax-rules ()
    ((double x)
     (let ((value x))
       (+ value value)))))

(define-syntax quad
  (syntax-rules ()
    ((quad x)
     (double (double x)))))

(display "Nested macro calls:\n")
(let ((value 999))  ; Shadow 'value' used in macro
  (display "  value = ")
  (display value)
  (newline)
  (display "  (quad 5) = ")
  (display (quad 5))
  (newline)
  (display "  value = ")
  (display value)
  (display " (unchanged!)\n"))
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Hygienic macros ensure:\n")
(display "  1. Macro-introduced bindings don't capture user variables\n")
(display "  2. User bindings at call site don't capture macro bindings\n")
(display "  3. Macros refer to their definition-site environment\n")
(display "  4. Each macro expansion creates fresh bindings\n")
(display "  5. Recursive/nested macros maintain distinct scopes\n")
(newline)
(display "This prevents subtle bugs and makes macros safe to use!\n")
