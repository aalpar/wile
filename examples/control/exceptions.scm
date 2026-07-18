;;; exceptions.scm - Exception handling with guard and raise
;;;
;;; Demonstrates: guard, raise, with-exception-handler, error objects
;;; Wile-specific: Full R7RS exception system
;;;
;;; Usage: ./dist/wile --file examples/control/exceptions.scm

;; R7RS provides structured exception handling.
;; guard: pattern-matching exception handler
;; raise: signal an exception
;; with-exception-handler: install a handler

(display "=== Exception Handling in Wile ===\n")
(newline)

;; Helper for string-contains (not in R7RS-small)
(define (string-contains str substr)
  (define (search s-pos)
    (cond
     ((> (+ s-pos (string-length substr)) (string-length str)) #f)
     ((string=? (substring str s-pos (+ s-pos (string-length substr))) substr) #t)
     (else (search (+ s-pos 1)))))
  (search 0))

;; Helper for make-error
(define (make-error msg)
  (error msg))

;; Example 1: Basic guard
(display "Example 1: Catching exceptions with guard\n")
(display "  ")
(guard (err
        (else
         (display "Caught error: ")
         (display (error-object-message err))
         (newline)))
  (display "About to raise exception...\n  ")
  (error "Something went wrong!"))
(newline)

;; Example 2: Pattern matching on error conditions
(display "Example 2: Multiple guard clauses\n")
(define (safe-div a b)
  (guard (err
          ((and (error-object? err)
                (string-contains (error-object-message err) "division"))
           (display "  Division error: returning 0\n")
           0)
          (else
           (display "  Other error: ")
           (display (error-object-message err))
           (newline)
           #f))
    (if (= b 0)
        (error "division by zero")
        (/ a b))))

(display "  (safe-div 10 2) = ")
(display (safe-div 10 2))
(newline)
(display "  (safe-div 10 0) = ")
(display (safe-div 10 0))
(newline)
(newline)

;; Example 3: Re-raising exceptions
(display "Example 3: Re-raising after logging\n")
(display "  ")
(guard (outer-err
        (else
         (display "Outer handler caught: ")
         (display (error-object-message outer-err))
         (newline)))
  (guard (inner-err
          (else
           (display "Inner handler logging error\n  ")
           (raise inner-err)))  ; Re-raise to outer
    (error "Original error")))
(newline)

;; Example 4: with-exception-handler
(display "Example 4: with-exception-handler (handler doesn't return)\n")
(display "  ")
(call-with-current-continuation
  (lambda (k)
    (with-exception-handler
      (lambda (err)
        (display "Handler called: ")
        (display (error-object-message err))
        (newline)
        (display "  Handler exiting via continuation\n")
        (k #f))
      (lambda ()
        (display "Body executing...\n  ")
        (raise (make-error "Test error"))
        (display "This won't print\n")))))
(newline)

;; Example 5: Error object inspection
(display "Example 5: Inspecting error objects\n")
(guard (err
        (else
         (display "  error-object? ")
         (display (error-object? err))
         (newline)
         (display "  message: ")
         (display (error-object-message err))
         (newline)
         (display "  irritants: ")
         (display (error-object-irritants err))
         (newline)))
  (error "Test error" 'with 'irritants 123))
(newline)

;; Example 6: Custom error types (using symbols)
(display "Example 6: Custom error types\n")
(define (validate-age age)
  (cond
   ((not (number? age))
    (error "Age must be a number" 'type-error age))
   ((< age 0)
    (error "Age cannot be negative" 'value-error age))
   ((> age 150)
    (error "Age unrealistic" 'value-error age))
   (else age)))

(define (test-validate age)
  (guard (err
          (else
           (display "  Error for age=")
           (display age)
           (display ": ")
           (display (error-object-message err))
           (display " ")
           (display (error-object-irritants err))
           (newline)))
    (validate-age age)
    (display "  Age ")
    (display age)
    (display " is valid\n")))

(test-validate 25)
(test-validate -5)
(test-validate "not-a-number")
(newline)

;; Example 7: Cleanup with dynamic-wind and exceptions
(display "Example 7: Exception-safe resource cleanup\n")
(define (with-file filename proc)
  (let ((port #f))
    (guard (err
            (else
             (display "  Error occurred, port is ")
             (display (if port "open" "not open"))
             (newline)
             (when port
               (close-output-port port)
               (display "  Port closed during error handling\n"))
             (raise err)))
      (dynamic-wind
        (lambda ()
          (set! port (open-output-file filename))
          (display "  Port opened\n"))
        (lambda ()
          (proc port))
        (lambda ()
          (when port
            (close-output-port port)
            (display "  Port closed\n")))))))

(display "  Normal case:\n")
(with-file "/tmp/test1.txt"
  (lambda (port)
    (display "  Writing to file\n")
    (write-string "Data\n" port)))
(newline)

(display "  Error case:\n")
(guard (outer
        (else
         (display "  Outer handler: ")
         (display (error-object-message outer))
         (newline)))
  (with-file "/tmp/test2.txt"
    (lambda (port)
      (display "  Writing to file\n")
      (write-string "Data\n" port)
      (error "Simulated error"))))
(newline)

;; Example 8: Exception as control flow
(display "Example 8: Using exceptions for control flow (break pattern)\n")
(define (find-first pred lst)
  (guard (found
          ((equal? "found" (error-object-message found))
           (car (error-object-irritants found))))
    (for-each
     (lambda (x)
       (when (pred x)
         (error "found" x)))
     lst)
    #f))

(display "  (find-first even? '(1 3 5 6 7 8)) = ")
(display (find-first even? '(1 3 5 6 7 8)))
(newline)
(newline)

;; Example 9: Nested exception handling
(display "Example 9: Nested exception handlers\n")
(display "  ")
(guard (outer
        (else
         (display "Outer caught: ")
         (display (error-object-message outer))
         (newline)))
  (guard (inner
          ((string-contains (error-object-message inner) "inner")
           (display "Inner handler for 'inner' error\n  "))
          (else
           (display "Inner handler passing through\n  ")
           (raise inner)))
    (display "Raising 'inner' error: ")
    (error "inner problem")
    (display "Raising 'outer' error: ")
    (error "outer problem")))
(newline)

;; Example 10: Assert-like pattern
(display "Example 10: Assertion with exceptions\n")
(define-syntax assert
  (syntax-rules ()
    ((assert condition message ...)
     (when (not condition)
       (error "Assertion failed" 'condition message ...)))))

(define (safe-sqrt x)
  (assert (>= x 0) "sqrt requires non-negative input")
  (sqrt x))

(display "  (safe-sqrt 16) = ")
(display (safe-sqrt 16))
(newline)
(display "  (safe-sqrt -4):\n")
(guard (err
        (else
         (display "    Caught: ")
         (display (error-object-message err))
         (newline)))
  (safe-sqrt -4))
(newline)

;; Summary
(display "=== Summary ===\n")
(display "R7RS exception system:\n")
(display "  • guard: pattern-matching exception handler\n")
(display "  • raise: signal an exception (doesn't return)\n")
(display "  • with-exception-handler: install a handler\n")
(display "  • error: create and raise an error object\n")
(display "  • error-object?: test if value is error object\n")
(display "  • error-object-message: get error message\n")
(display "  • error-object-irritants: get additional info\n")
(newline)
(display "Combine with dynamic-wind for exception-safe resource management!\n")
