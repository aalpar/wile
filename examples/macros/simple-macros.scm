;;; simple-macros.scm - Basic syntax-rules macro patterns
;;;
;;; Demonstrates: syntax-rules, pattern matching, macro expansion
;;; Wile-specific: Hygienic macros using "sets of scopes" (Flatt 2016)
;;;
;;; Usage: ./dist/wile --file examples/macros/simple-macros.scm

;; Macros transform code at compile time.
;; syntax-rules provides pattern-based macro definition.

(display "=== Basic Macros in Wile ===\n")
(newline)

;; Simple replacement macro
(define-syntax when
  (syntax-rules ()
    ((when condition body ...)
     (if condition
         (begin body ...)))))

(display "when macro (single-branch if):\n")
(display "  ")
(when (> 5 3)
  (display "5 is greater than 3")
  (newline))
(newline)

;; Macro with multiple patterns
(define-syntax my-or
  (syntax-rules ()
    ((my-or) #f)
    ((my-or e) e)
    ((my-or e1 e2 ...)
     (let ((temp e1))
       (if temp temp (my-or e2 ...))))))

(display "my-or macro (short-circuit disjunction):\n")
(display "  (my-or #f #f 5 #f) = ")
(display (my-or #f #f 5 #f))
(display "\n")
(display "  (my-or #f #f #f) = ")
(display (my-or #f #f #f))
(display "\n\n")

;; Pattern with ellipsis (...)
(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (begin body ...))
    ((let* ((var val) rest ...) body ...)
     (let ((var val))
       (let* (rest ...) body ...)))))

(display "let* macro (sequential binding):\n")
(display "  ")
(let* ((x 1)
       (y (+ x 1))
       (z (+ y 1)))
  (display "x=")
  (display x)
  (display ", y=")
  (display y)
  (display ", z=")
  (display z)
  (newline))
(newline)

;; Macro creating definitions
(define-syntax define-constant
  (syntax-rules ()
    ((define-constant name value)
     (define name value))))

(display "define-constant macro:\n")
(define-constant pi 3.14159)
(display "  pi = ")
(display pi)
(display "\n\n")

;; Assertion macro
(define-syntax assert
  (syntax-rules ()
    ((assert condition message)
     (if (not condition)
         (error message)))))

(display "assert macro:\n")
(display "  (assert (> 5 3) \"5 should be > 3\") -> ")
(assert (> 5 3) "5 should be > 3")
(display "passed\n\n")

;; Repeat macro
(define-syntax repeat
  (syntax-rules ()
    ((repeat n body ...)
     (let loop ((count n))
       (when (> count 0)
         body ...
         (loop (- count 1)))))))

(display "repeat macro:\n")
(display "  ")
(repeat 3
  (display "Hello ")
  (display "World! "))
(newline)
(newline)

;; while macro
(define-syntax while
  (syntax-rules ()
    ((while condition body ...)
     (let loop ()
       (when condition
         body ...
         (loop))))))

(display "while macro:\n")
(display "  ")
(let ((i 0))
  (while (< i 5)
    (display i)
    (display " ")
    (set! i (+ i 1))))
(newline)
(newline)

;; increment/decrement macros
(define-syntax inc!
  (syntax-rules ()
    ((inc! var)
     (set! var (+ var 1)))
    ((inc! var delta)
     (set! var (+ var delta)))))

(define-syntax dec!
  (syntax-rules ()
    ((dec! var)
     (set! var (- var 1)))
    ((dec! var delta)
     (set! var (- var delta)))))

(display "inc!/dec! macros:\n")
(let ((x 10))
  (display "  x = ")
  (display x)
  (newline)
  (inc! x)
  (display "  after (inc! x): ")
  (display x)
  (newline)
  (inc! x 5)
  (display "  after (inc! x 5): ")
  (display x)
  (newline)
  (dec! x 3)
  (display "  after (dec! x 3): ")
  (display x)
  (newline))
(newline)

;; swap macro
(define-syntax swap!
  (syntax-rules ()
    ((swap! a b)
     (let ((temp a))
       (set! a b)
       (set! b temp)))))

(display "swap! macro:\n")
(let ((x 1)
      (y 2))
  (display "  Before: x=")
  (display x)
  (display ", y=")
  (display y)
  (newline)
  (swap! x y)
  (display "  After swap: x=")
  (display x)
  (display ", y=")
  (display y)
  (newline))
(newline)

;; Literal matching
(define-syntax cond-with-arrow
  (syntax-rules (=> else)
    ((cond-with-arrow (else body ...))
     (begin body ...))
    ((cond-with-arrow (test => proc) rest ...)
     (let ((temp test))
       (if temp
           (proc temp)
           (cond-with-arrow rest ...))))
    ((cond-with-arrow (test body ...) rest ...)
     (if test
         (begin body ...)
         (cond-with-arrow rest ...)))
    ((cond-with-arrow)
     (if #f #f))))

(display "cond with => (literal matching):\n")
(display "  ")
(cond-with-arrow
 ((assoc 'b '((a 1) (b 2) (c 3))) => cdr)
 (else (display "not found")))
(newline)
(newline)

;; Macro expanding to macro call
(define-syntax double
  (syntax-rules ()
    ((double x)
     (* 2 x))))

(define-syntax quad
  (syntax-rules ()
    ((quad x)
     (double (double x)))))

(display "Nested macro expansion:\n")
(display "  (quad 5) = ")
(display (quad 5))
(display " (expands to (* 2 (* 2 5)))\n\n")

;; push!/pop! for list mutation
(define-syntax push!
  (syntax-rules ()
    ((push! item list)
     (set! list (cons item list)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! list)
     (let ((result (car list)))
       (set! list (cdr list))
       result))))

(display "push!/pop! macros (stack operations):\n")
(let ((stack '()))
  (display "  Empty stack: ")
  (display stack)
  (newline)
  (push! 1 stack)
  (push! 2 stack)
  (push! 3 stack)
  (display "  After pushing 1, 2, 3: ")
  (display stack)
  (newline)
  (display "  Pop: ")
  (display (pop! stack))
  (newline)
  (display "  Stack now: ")
  (display stack)
  (newline))
