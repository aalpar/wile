;;; hello.scm - First Wile program
;;;
;;; Demonstrates: Basic syntax, display, string operations
;;;
;;; Usage: ./dist/wile --file examples/basics/hello.scm

;; The classic first program
(display "Hello, World!")
(newline)

;; String concatenation using string-append
(display (string-append "Hello, " "Wile " "Scheme!"))
(newline)

;; Using format-style output
(let ((name "Wile"))
  (display "Welcome to ")
  (display name)
  (display "!")
  (newline))

;; Simple arithmetic
(display "2 + 2 = ")
(display (+ 2 2))
(newline)

;; Working with lists
(display "First three primes: ")
(display '(2 3 5))
(newline)
