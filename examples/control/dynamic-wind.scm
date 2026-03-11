;;; dynamic-wind.scm - Resource cleanup with dynamic-wind
;;;
;;; Demonstrates: dynamic-wind, before/after guards, cleanup with continuations
;;; Wile-specific: Full dynamic-wind support for continuation-safe resource management
;;;
;;; Usage: ./dist/wile --file examples/control/dynamic-wind.scm

;; dynamic-wind ensures cleanup code runs even when continuations are involved.
;; Form: (dynamic-wind before-thunk body-thunk after-thunk)

(display "=== dynamic-wind in Wile ===\n")
(newline)

;; Example 1: Basic structure
(display "Example 1: Basic before/body/after pattern\n")
(display "  ")
(dynamic-wind
  (lambda ()
    (display "BEFORE ")
    (newline)
    (display "  "))
  (lambda ()
    (display "BODY ")
    'body-result)
  (lambda ()
    (display "AFTER")
    (newline)))
(newline)

;; Example 2: Cleanup even with early exit
(display "Example 2: Cleanup runs even with call/cc escape\n")
(display "  ")
(call/cc
 (lambda (escape)
   (dynamic-wind
     (lambda ()
       (display "BEFORE ")
       (newline)
       (display "  "))
     (lambda ()
       (display "BODY (escaping...) ")
       (escape 'escaped))
     (lambda ()
       (display "AFTER (cleanup ran!)")
       (newline)))))
(newline)

;; Example 3: File I/O pattern
(display "Example 3: Safe file operations\n")
(define (with-output-file filename proc)
  (let ((port #f))
    (dynamic-wind
      (lambda ()
        (set! port (open-output-file filename)))
      (lambda ()
        (proc port))
      (lambda ()
        (when port
          (close-output-port port))))))

(display "  Writing to temp file with guaranteed close:\n")
(with-output-file "/tmp/wile-test.txt"
  (lambda (port)
    (display "    Writing data...\n")
    (write-string "Hello from Wile!\n" port)
    (write-string "File will be closed properly.\n" port)))
(display "    File closed automatically\n")
(newline)

;; Example 4: Mutex-like locking
(display "Example 4: Critical section with guaranteed unlock\n")
(define mutex-locked? #f)

(define (with-lock thunk)
  (dynamic-wind
    (lambda ()
      (when mutex-locked?
        (error "Mutex already locked"))
      (set! mutex-locked? #t)
      (display "    [LOCKED] "))
    thunk
    (lambda ()
      (set! mutex-locked? #f)
      (display "[UNLOCKED]\n"))))

(display "  Critical section:\n")
(with-lock
  (lambda ()
    (display "executing critical code ")))
(newline)

;; Example 5: Nesting dynamic-wind
(display "Example 5: Nested dynamic-wind forms\n")
(display "  ")
(dynamic-wind
  (lambda () (display "OUTER-BEFORE "))
  (lambda ()
    (display "OUTER-BODY-START ")
    (dynamic-wind
      (lambda () (display "INNER-BEFORE "))
      (lambda () (display "INNER-BODY "))
      (lambda () (display "INNER-AFTER ")))
    (display "OUTER-BODY-END "))
  (lambda () (display "OUTER-AFTER")))
(newline)
(newline)

;; Example 6: Continuation invocation triggers unwinding
(display "Example 6: Re-entering via continuation triggers guards\n")
(let ((saved-k #f))
  (display "  First entry:\n    ")
  (dynamic-wind
    (lambda () (display "BEFORE "))
    (lambda ()
      (call/cc (lambda (k) (set! saved-k k)))
      (display "BODY "))
    (lambda () (display "AFTER\n")))

  (when saved-k
    (display "  Re-entering via continuation:\n    ")
    (let ((k saved-k))
      (set! saved-k #f)  ; Only do this once
      (k 'ignored))))
(newline)

;; Example 7: Exception-safe resource management
(display "Example 7: Resource cleanup with error handling\n")
(define (with-resource acquire release use)
  (let ((resource #f))
    (guard (err
            (else
             (display "    Error occurred, cleaning up...\n")
             (when resource (release resource))
             (raise err)))
      (dynamic-wind
        (lambda ()
          (set! resource (acquire)))
        (lambda ()
          (use resource))
        (lambda ()
          (when resource
            (release resource)))))))

(display "  Normal operation:\n")
(with-resource
  (lambda ()
    (display "    Acquiring resource\n")
    'my-resource)
  (lambda (r)
    (display "    Releasing resource\n"))
  (lambda (r)
    (display "    Using resource\n")))
(newline)

;; Example 8: State tracking across control flow
(display "Example 8: Tracking entry/exit count\n")
(let ((entry-count 0)
      (exit-count 0)
      (k #f))
  (display "  ")
  (dynamic-wind
    (lambda ()
      (set! entry-count (+ entry-count 1))
      (display "ENTER#")
      (display entry-count)
      (display " "))
    (lambda ()
      (call/cc (lambda (cont) (set! k cont)))
      (display "BODY "))
    (lambda ()
      (set! exit-count (+ exit-count 1))
      (display "EXIT#")
      (display exit-count)
      (display " ")))

  (when (and k (< entry-count 3))
    (let ((cont k))
      (set! k #f)  ; Prevent infinite loop
      (cont 'reenter))))
(newline)
(newline)

;; Example 9: Simulating try-finally
(display "Example 9: try-finally pattern\n")
(define-syntax try-finally
  (syntax-rules ()
    ((try-finally body finally-clause ...)
     (dynamic-wind
       (lambda () (if #f #f))  ; No-op before
       (lambda () body)
       (lambda () finally-clause ...)))))

(display "  ")
(try-finally
  (begin
    (display "TRY-BLOCK ")
    'result)
  (display "FINALLY-BLOCK")
  (newline))
(newline)

;; Example 10: Parameterization (simplified)
(display "Example 10: Parameter-like behavior with dynamic-wind\n")
(define current-indent 0)

(define (with-indent thunk)
  (dynamic-wind
    (lambda ()
      (set! current-indent (+ current-indent 2)))
    thunk
    (lambda ()
      (set! current-indent (- current-indent 2)))))

(define (print-indented msg)
  (display (make-string current-indent #\space))
  (display msg)
  (newline))

(display "  Indented output:\n")
(print-indented "Level 0")
(with-indent
  (lambda ()
    (print-indented "Level 1")
    (with-indent
      (lambda ()
        (print-indented "Level 2")
        (print-indented "Still level 2")))
    (print-indented "Back to level 1")))
(print-indented "Back to level 0")
(newline)

;; Summary
(display "=== Summary ===\n")
(display "dynamic-wind ensures:\n")
(display "  • Before thunk runs on entry\n")
(display "  • After thunk runs on exit (even via call/cc)\n")
(display "  • Guards are re-run when continuation re-enters\n")
(display "  • Perfect for resource management (files, locks, connections)\n")
(display "  • Enables try-finally semantics\n")
(newline)
(display "Use dynamic-wind whenever cleanup must happen, no matter what!\n")
