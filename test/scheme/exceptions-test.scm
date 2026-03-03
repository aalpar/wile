;;; exceptions-test.scm - R7RS 6.11 Exceptions and 4.2.7 Guard
;;;
;;; Edge cases and detailed coverage extracted from Go test suites:
;;;   extensions/exceptions/prim_exceptions_test.go
;;;   registry/core/prim_exception_test.go
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base)
        (scheme read)
        (scheme write)
        (scheme file)
        (chibi test))

(test-begin "exceptions")

;; ── Guard: matching test clauses ─────────────────────────────────

(test-group "guard with test clauses"
  ;; matching test clause (not just else)
  (test 'matched
    (guard (exn ((eq? exn 'specific) 'matched))
      (raise 'specific)))

  ;; test clause using exception value
  (test 142
    (guard (exn ((number? exn) (+ exn 100)))
      (raise 42)))

  ;; multiple clauses - first matches
  (test 'was-number
    (guard (exn
            ((number? exn) 'was-number)
            ((string? exn) 'was-string)
            (else 'other))
      (raise 123)))

  ;; multiple clauses - second matches
  (test 'was-string
    (guard (exn
            ((number? exn) 'was-number)
            ((string? exn) 'was-string)
            (else 'other))
      (raise "hello")))

  ;; multiple clauses - else matches
  (test 'other
    (guard (exn
            ((number? exn) 'was-number)
            ((string? exn) 'was-string)
            (else 'other))
      (raise 'symbol))))

;; ── Guard: normal execution (no exception) ───────────────────────

(test-group "guard normal execution"
  (test 3
    (guard (exn (else 'error))
      (+ 1 2)))

  (test 200
    (guard (exn (else 'error))
      (let ((x 10) (y 20))
        (* x y)))))

;; ── Guard: => clause ─────────────────────────────────────────────

(test-group "guard with => clause"
  ;; assq result passed to cdr
  (test 42
    (guard (exn ((assq 'a exn) => cdr))
      (raise '((a . 42)))))

  ;; custom procedure in => clause
  (test 42
    (guard (exn ((assq 'val exn) => (lambda (p) (* (cdr p) 2))))
      (raise '((val . 21)))))

  ;; => clause fallthrough to second clause
  (test '(b . 23)
    (guard (exn
            ((assq 'a exn) => cdr)
            ((assq 'b exn)))
      (raise '((b . 23))))))

;; ── Guard: re-raise to outer handler ─────────────────────────────

(test-group "guard re-raise"
  ;; re-raise when no clause matches
  (test '(outer symbol-error)
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape (list 'outer e)))
          (lambda ()
            (guard (exn ((number? exn) 'was-number))
              (raise 'symbol-error)))))))

  ;; re-raise preserves exception value
  (test 42
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape e))
          (lambda ()
            (guard (exn ((string? exn) 'was-string))
              (raise 42)))))))

  ;; R7RS §4.2.7: re-raise in original dynamic extent allows
  ;; non-escaping outer handler (raise-continuable path)
  (test '(caught 42)
    (with-exception-handler
      (lambda (e) (list 'caught e))
      (lambda ()
        (guard (inner ((symbol? inner) 'sym))
          (raise 42)))))

  ;; re-raise with non-escaping handler, string exception
  (test '(handled oops)
    (with-exception-handler
      (lambda (e) (list 'handled e))
      (lambda ()
        (guard (exn ((number? exn) 'was-number))
          (raise 'oops))))))

;; ── Guard: catches error objects ─────────────────────────────────

(test-group "guard with error objects"
  (test "test message"
    (guard (exn ((error-object? exn) (error-object-message exn)))
      (error "test message")))

  (test '(a b c)
    (guard (exn ((error-object? exn) (error-object-irritants exn)))
      (error "msg" 'a 'b 'c)))

  (test 'was-error
    (guard (exn
            ((error-object? exn) 'was-error)
            (else 'was-other))
      (error "oops"))))

;; ── Guard: inside let and procedures ─────────────────────────────

(test-group "guard in nested contexts"
  ;; guard inside let
  (test 15
    (let ((x 10))
      (guard (exn (else (+ x exn)))
        (raise 5))))

  ;; guard with computation before raise
  (test 'caught
    (guard (exn (else 'caught))
      (let ((x (+ 1 2)))
        (if (= x 3)
            (raise 'expected)
            'unexpected))))

  ;; guard in procedure - normal return
  (test 5
    (let ((safe-div (lambda (a b)
                      (guard (exn (else 0))
                        (/ a b)))))
      (safe-div 10 2))))

;; ── Guard: nested guards ─────────────────────────────────────────

(test-group "nested guards"
  ;; two nested - inner catches
  (test 142
    (guard (outer (else 'outer-caught))
      (guard (inner ((number? inner) (+ inner 100)))
        (raise 42))))

  ;; two nested - inner misses, outer catches
  (test '(outer-caught not-a-number)
    (guard (outer ((symbol? outer) (list 'outer-caught outer)))
      (guard (inner ((number? inner) 'was-number))
        (raise 'not-a-number))))

  ;; three nested - innermost catches
  (test 14
    (guard (L1 (else 'L1))
      (guard (L2 (else 'L2))
        (guard (L3 ((number? L3) (* L3 2)))
          (raise 7)))))

  ;; three nested - middle catches
  (test '(L2 oops)
    (guard (L1 (else 'L1))
      (guard (L2 ((symbol? L2) (list 'L2 L2)))
        (guard (L3 ((number? L3) 'was-number))
          (raise 'oops)))))

  ;; three nested - outermost catches
  (test 3
    (guard (L1 ((list? L1) (length L1)))
      (guard (L2 ((number? L2) 'was-number))
        (guard (L3 ((string? L3) 'was-string))
          (raise '(a b c))))))

  ;; four nested - deepest catches
  (test 'hit
    (guard (L1 (else 'L1))
      (guard (L2 (else 'L2))
        (guard (L3 (else 'L3))
          (guard (L4 ((eq? L4 'target) 'hit))
            (raise 'target))))))

  ;; four nested - outermost catches
  (test '(L1-caught #\x)
    (guard (L1 ((char? L1) (list 'L1-caught L1)))
      (guard (L2 ((number? L2) 'L2))
        (guard (L3 ((string? L3) 'L3))
          (guard (L4 ((symbol? L4) 'L4))
            (raise #\x)))))))

;; ── Guard: nested with body computation ──────────────────────────

(test-group "nested guards with computation"
  (test 1025
    (guard (L1 ((number? L1) (+ L1 1000)))
      (let ((a 10))
        (guard (L2 ((string? L2) 'was-string))
          (let ((b (* a 2)))
            (guard (L3 ((symbol? L3) 'was-symbol))
              (let ((c (+ b 5)))
                (raise c))))))))

  ;; normal return bypasses all guards
  (test 60
    (guard (L1 (else 'L1))
      (guard (L2 (else 'L2))
        (guard (L3 (else 'L3))
          (+ 10 20 30))))))

;; ── Guard: => clause at inner level of nesting ───────────────────

(test-group "nested guard with => clause"
  (test 81
    (guard (outer (else 'outer))
      (guard (inner ((assq 'n inner) => (lambda (p) (* (cdr p) (cdr p)))))
        (raise '((n . 9)))))))

;; ── Guard: with error objects propagating through levels ─────────

(test-group "nested guard with error objects"
  (test "caught: deep failure"
    (guard (outer
            ((error-object? outer)
             (string-append "caught: " (error-object-message outer))))
      (guard (inner ((number? inner) 'was-number))
        (error "deep failure" 'x 'y)))))

;; ── Guard: inside loop-like recursion ────────────────────────────

(test-group "guard inside recursion"
  (test '(1 2 3)
    (let loop ((n 3) (acc '()))
      (if (= n 0)
          acc
          (loop (- n 1)
                (cons
                  (guard (exn ((number? exn) exn))
                    (raise n))
                  acc))))))

;; ── Guard: handler re-raises to outer guard ──────────────────────

(test-group "guard handler re-raise patterns"
  ;; inner guard misses string, outer catches
  (test "outer: not-a-number"
    (guard (outer-exn
            ((string? outer-exn) (string-append "outer: " outer-exn)))
      (guard (inner-exn
              ((number? inner-exn) 'was-number))
        (raise "not-a-number"))))

  ;; guard body raises, handler clause re-raises transformed value
  (test 50
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape e))
          (lambda ()
            (guard (exn
                    ((number? exn) (raise (* exn 10))))
              (raise 5))))))))

;; ── Continuable resumption ───────────────────────────────────────

(test-group "raise-continuable resumption"
  ;; handler return value used in addition
  (test 106
    (with-exception-handler
      (lambda (e) (+ e 100))
      (lambda () (+ (raise-continuable 5) 1))))

  ;; handler return used in multiplication
  (test 42
    (with-exception-handler
      (lambda (e) (* e 2))
      (lambda () (* (raise-continuable 7) 3))))

  ;; raise-continuable in let binding
  (test 13
    (with-exception-handler
      (lambda (e) (* e 2))
      (lambda ()
        (let ((x (raise-continuable 5)))
          (+ x 3)))))

  ;; raise-continuable in conditional test
  (test 'yes
    (with-exception-handler
      (lambda (e) #t)
      (lambda ()
        (if (raise-continuable #f) 'yes 'no))))

  ;; nested raise-continuable: (+ (raise-continuable 1) (raise-continuable 2))
  (test 23
    (with-exception-handler
      (lambda (e) (+ e 10))
      (lambda ()
        (+ (raise-continuable 1)
           (raise-continuable 2)))))

  ;; raise-continuable result used in function call
  (test 2
    (with-exception-handler
      (lambda (e) (list e e))
      (lambda ()
        (length (raise-continuable 'x)))))

  ;; multiple expressions after raise-continuable
  (test '(after recovered)
    (with-exception-handler
      (lambda (e) 'recovered)
      (lambda ()
        (let ((x (raise-continuable 'warning)))
          (list 'after x))))))

;; ── Exception handler: transforms exception value ────────────────

(test-group "handler transforms exception"
  (test 105
    (with-exception-handler
      (lambda (e) (+ e 100))
      (lambda () (raise-continuable 5))))

  ;; handler transforms string
  (test "hello!"
    (with-exception-handler
      (lambda (e) (string-append e "!"))
      (lambda () (raise-continuable "hello"))))

  ;; handler with list processing
  (test 5
    (with-exception-handler
      (lambda (e) (length e))
      (lambda () (raise-continuable '(a b c d e))))))

;; ── Exception handler: inner raises, outer catches ───────────────

(test-group "exception handler chains"
  ;; inner handler raises, outer catches
  (test '(outer (reraised original))
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape (list 'outer e)))
          (lambda ()
            (with-exception-handler
              (lambda (e) (raise (list 'reraised e)))
              (lambda () (raise 'original))))))))

  ;; handler raises different exception type (error in handler)
  (test "handler failed"
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape (error-object-message e)))
          (lambda ()
            (with-exception-handler
              (lambda (e) (error "handler failed" e))
              (lambda () (raise 'bad))))))))

  ;; handler raises continuable to outer handler
  (test '(outer-handled (wrapped start))
    (with-exception-handler
      (lambda (e) (list 'outer-handled e))
      (lambda ()
        (with-exception-handler
          (lambda (e) (raise-continuable (list 'wrapped e)))
          (lambda () (raise-continuable 'start))))))

  ;; three layers: inner -> middle -> outer
  (test '(L1 (L2 (L3 origin)))
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape (list 'L1 e)))
          (lambda ()
            (with-exception-handler
              (lambda (e) (raise (list 'L2 e)))
              (lambda ()
                (with-exception-handler
                  (lambda (e) (raise (list 'L3 e)))
                  (lambda () (raise 'origin))))))))))

  ;; handler error object propagates to outer
  (test '(#t "inner handler broke")
    (call-with-current-continuation
      (lambda (escape)
        (with-exception-handler
          (lambda (e)
            (escape (list (error-object? e)
                          (error-object-message e))))
          (lambda ()
            (with-exception-handler
              (lambda (e) (error "inner handler broke"))
              (lambda () (raise 'trigger)))))))))

;; ── Error object predicates on non-errors ────────────────────────

(test-group "error-object? on non-errors"
  (test #f (error-object? 42))
  (test #f (error-object? "not an error"))
  (test #f (error-object? 'err))
  (test #f (error-object? '(1 2 3)))
  (test #f (error-object? '()))
  (test #f (error-object? #t))
  (test #f (error-object? (lambda () 1))))

;; ── Error object creation and inspection ─────────────────────────

(test-group "error object via call/cc"
  ;; error-object? on caught error
  (test #t
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object? e)))
          (lambda () (error "test"))))))

  ;; error-object-message
  (test "hello world"
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-message e)))
          (lambda () (error "hello world"))))))

  ;; error-object-message with empty message
  (test ""
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-message e)))
          (lambda () (error ""))))))

  ;; error with message and irritants combined check
  (test '(#t "test" (a b))
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e)
            (k (list (error-object? e)
                     (error-object-message e)
                     (error-object-irritants e))))
          (lambda () (error "test" 'a 'b)))))))

;; ── Error object irritants ───────────────────────────────────────

(test-group "error-object-irritants"
  ;; multiple irritants
  (test '(a b c)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-irritants e)))
          (lambda () (error "msg" 'a 'b 'c))))))

  ;; no irritants -> empty list
  (test '()
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-irritants e)))
          (lambda () (error "msg"))))))

  ;; single irritant
  (test '(42)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-irritants e)))
          (lambda () (error "msg" 42))))))

  ;; integer irritants
  (test '(1 2 3)
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k (error-object-irritants e)))
          (lambda () (error "test" 1 2 3)))))))

;; ── Error with wrong types ───────────────────────────────────────

(test-group "error with non-string message"
  (test-error (error 42))
  (test-error (error 'not-a-string)))

;; ── error-object-message/irritants on wrong types ────────────────

(test-group "error-object-message wrong type"
  (test-error (error-object-message 42))
  (test-error (error-object-message "hello")))

(test-group "error-object-irritants wrong type"
  (test-error (error-object-irritants 42))
  (test-error (error-object-irritants "hello"))
  (test-error (error-object-irritants '(1 2 3))))

;; ── read-error? and file-error? predicates ───────────────────────

(test-group "read-error? predicate"
  (test #t
    (read-error? (guard (exn (else exn))
                   (read (open-input-string "#\\badname")))))
  (test #f
    (read-error? (guard (exn (else exn))
                   (error "generic"))))
  (test #f (read-error? "hello"))
  (test #f (read-error? 42)))

(test-group "file-error? predicate"
  (test #t
    (file-error? (guard (exn (else exn))
                   (open-input-file " no such file "))))
  (test #f
    (file-error? (guard (exn (else exn))
                   (error "generic"))))
  (test #f (file-error? 42)))

;; ── Exception handler inheritance in apply ────────────────────────

(test-group "handler inheritance in apply"
  (test 'error-in-apply
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (apply (lambda (x y)
                   (if (= x 3)
                       (raise-continuable 'error-in-apply)
                       (+ x y)))
                 '(3 4))))
      caught)))

;; ── Exception handler inheritance in call-with-values ─────────────

(test-group "handler inheritance in call-with-values"
  ;; handler catches exception in producer
  (test 'producer-error
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (call-with-values
            (lambda () (raise-continuable 'producer-error) (values 1 2))
            (lambda (a b) (+ a b)))))
      caught))

  ;; handler catches exception in consumer
  (test 'consumer-error
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (call-with-values
            (lambda () (values 1 2))
            (lambda (a b) (raise-continuable 'consumer-error) (+ a b)))))
      caught)))

;; ── Exception handler inheritance in dynamic-wind ─────────────────

(test-group "handler inheritance in dynamic-wind"
  ;; handler catches exception in before thunk
  (test 'before-error
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (dynamic-wind
            (lambda () (raise-continuable 'before-error))
            (lambda () 'body)
            (lambda () 'after))))
      caught))

  ;; handler catches exception in after thunk
  (test 'after-error
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (dynamic-wind
            (lambda () 'before)
            (lambda () 'body)
            (lambda () (raise-continuable 'after-error)))))
      caught)))

;; ── with-exception-handler: thunk returns normally ────────────────

(test-group "with-exception-handler normal return"
  (test 42
    (with-exception-handler
      (lambda (e) 'not-called)
      (lambda () 42)))

  (test 6
    (with-exception-handler
      (lambda (e) 0)
      (lambda () (+ 1 2 3)))))

;; ── Nested handlers with continuable ──────────────────────────────

(test-group "nested handlers - inner catches"
  (test 'inner
    (with-exception-handler
      (lambda (e) 'outer)
      (lambda ()
        (with-exception-handler
          (lambda (e) 'inner)
          (lambda () (raise-continuable 'err)))))))

;; ── raise with call/cc escape ─────────────────────────────────────

(test-group "raise with call/cc escape"
  (test 'my-error
    (call/cc
      (lambda (escape)
        (with-exception-handler
          (lambda (e) (escape e))
          (lambda () (raise 'my-error))))))

  (test 'caught
    (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
          (lambda (e) (k 'caught))
          (lambda () (raise 'error)))))))

;; ── Guard: multiple-value body ────────────────────────────────────

(test-group "guard body multiple values"
  ;; two values propagate through guard when no exception is raised
  (define-values (a b)
    (guard (exn (else (values -1 -1)))
      (values 1 2)))
  (test 1 a)
  (test 2 b)

  ;; zero values (unusual but must not error)
  ;; Use call-with-values to consume the zero-value return.
  (test #t
    (call-with-values
     (lambda ()
       (guard (exn (else 'caught))
         (values)))
     (lambda () #t)))

  ;; single value still works (regression)
  (test 42
    (guard (exn (else 'caught))
      42))

  ;; multiple values with multi-expression body (e1 e2 are sequenced,
  ;; last expression produces multiple values)
  (define-values (x y z)
    (guard (exn (else (values -1 -1 -1)))
      (define ignored 0)
      (values 10 20 30)))
  (test 10 x)
  (test 20 y)
  (test 30 z))

(test-end)
