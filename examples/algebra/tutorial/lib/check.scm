;; check.scm -- minimal self-verification helpers for algebra tutorial files.
;;
;; The tutorial files under ../chapters/ and ../quick-tour/ include this
;; file and use the helpers below to assert expected behavior. A successful
;; check prints "  ok  <label>". A failed check prints the mismatch and
;; raises an error so the interpreter exits non-zero, making drift loud
;; under `make tutorial-test`.
;;
;; Five helpers, kept deliberately minimal:
;;   (check=        actual expected label)           equal? comparison
;;   (check-approx= actual expected tolerance label) floating-point
;;   (check-true    actual label)                    must be truthy (not #f)
;;   (check-false   actual label)                    must be strictly #f
;;   (check-error   thunk label)                     thunk must raise
;;   (check-error   thunk pred? label)               same, plus pred? must
;;                                                   accept the raised condition
;;
;; `check-true` treats any non-#f value as pass, matching Scheme's `if`
;; semantics -- so (memq x lst), (assq x lst), and other lookup procedures
;; that return non-#t truthy values are accepted. Use `check=` with an
;; explicit #t expected value when strict identity matters.
;;
;; `check-error` in its two-argument form accepts any raise, including ones
;; from unrelated bugs in the thunk (unbound identifier, wrong arity, etc.).
;; When the raised condition's identity matters, pass a predicate that
;; inspects it; the check fails if the predicate rejects the condition.

(define (check= actual expected label)
  (if (equal? actual expected)
      (begin (display "  ok  ") (display label) (newline))
      (begin
        (display "  FAIL ") (display label) (newline)
        (display "    expected: ") (write expected) (newline)
        (display "    actual:   ") (write actual) (newline)
        (error "tutorial check failed" label))))

(define (check-approx= actual expected tolerance label)
  (if (and (number? actual)
           (number? expected)
           (<= (abs (- actual expected)) tolerance))
      (begin (display "  ok  ") (display label) (newline))
      (begin
        (display "  FAIL ") (display label) (newline)
        (display "    expected: ") (write expected)
        (display " (+/- ") (write tolerance) (display ")") (newline)
        (display "    actual:   ") (write actual) (newline)
        (error "tutorial check failed" label))))

(define (check-true actual label)
  (if actual
      (begin (display "  ok  ") (display label) (newline))
      (begin
        (display "  FAIL ") (display label) (newline)
        (display "    expected: a truthy value") (newline)
        (display "    actual:   ") (write actual) (newline)
        (error "tutorial check failed" label))))

(define (check-false actual label)
  (if (eq? actual #f)
      (begin (display "  ok  ") (display label) (newline))
      (begin
        (display "  FAIL ") (display label) (newline)
        (display "    expected: #f") (newline)
        (display "    actual:   ") (write actual) (newline)
        (error "tutorial check failed" label))))

(define check-error
  (case-lambda
    ((thunk label) (check-error thunk (lambda (_e) #t) label))
    ((thunk pred? label)
     (let ((raised #f) (condition #f) (pred-ok #f))
       (guard (e (#t (set! raised #t)
                     (set! condition e)
                     (set! pred-ok (pred? e))))
         (thunk))
       (cond
         ((and raised pred-ok)
          (display "  ok  ") (display label) (newline))
         (raised
          (display "  FAIL ") (display label) (newline)
          (display "    condition raised but predicate rejected: ")
          (write condition) (newline)
          (error "tutorial check failed" label))
         (else
          (display "  FAIL ") (display label) (newline)
          (display "    expected an error; none was raised") (newline)
          (error "tutorial check failed" label)))))))
