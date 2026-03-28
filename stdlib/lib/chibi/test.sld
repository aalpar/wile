;; Minimal (chibi test) compatible library for Wile
;; Provides the essential testing interface used by r7rs-tests.scm
;;
;; This is a simplified implementation that doesn't require
;; (chibi diff), (chibi term ansi), or (chibi optional).

(define-library (chibi test)
  (export
   ;; Core testing interface
   test
   test-equal
   test-assert
   test-not
   test-error
   test-values
   test-group
   test-begin
   test-end
   test-exit
   ;; Parameters
   current-test-group
   current-test-epsilon
   current-test-comparator
   test-failure-count
   ;; Internal helpers that macros need access to
   %test-pass
   %test-fail
   %approx-equal?)
  (import (scheme base)
          (scheme write)
          (scheme process-context))
  (begin
    ;; Test state
    (define *test-group-stack* '())
    (define *test-pass-count* 0)
    (define *test-fail-count* 0)
    (define *group-indent* 0)

    ;; Parameters
    (define current-test-group
      (make-parameter #f))

    (define current-test-epsilon
      (make-parameter 1e-5))

    (define current-test-comparator
      (make-parameter equal?))

    (define test-failure-count
      (make-parameter (lambda () *test-fail-count*)))

    ;; Indentation helper
    (define (indent)
      "Display indentation spaces for the current group nesting level"
      (let loop ((i 0))
        (when (< i *group-indent*)
          (display "  ")
          (loop (+ i 1)))))

    ;; Begin a test group
    (define (test-begin name . args)
      "Begin a new test group with NAME and increase nesting level"
      (indent)
      (display "Testing ")
      (display name)
      (newline)
      (set! *test-group-stack* (cons name *test-group-stack*))
      (set! *group-indent* (+ *group-indent* 1))
      (current-test-group name))

    ;; End a test group
    (define (test-end . args)
      "End the current test group and decrease nesting level"
      (when (pair? *test-group-stack*)
        (set! *test-group-stack* (cdr *test-group-stack*))
        (set! *group-indent* (max 0 (- *group-indent* 1)))
        (current-test-group (if (pair? *test-group-stack*)
                                 (car *test-group-stack*)
                                 #f))))

    ;; Test result reporting - exported for macro use
    (define (%test-pass name)
      "Record a passing test result for NAME"
      (set! *test-pass-count* (+ *test-pass-count* 1)))

    (define (%test-fail name expected actual)
      "Record a failing test result for NAME and display EXPECTED vs ACTUAL"
      (set! *test-fail-count* (+ *test-fail-count* 1))
      (indent)
      (display "FAIL: ")
      (when name
        (display name)
        (display " - "))
      (display "expected ")
      (write expected)
      (display " but got ")
      (write actual)
      (newline))

    ;; Approximate equality for floating point - exported for macro use
    ;; Uses relative difference like the original chibi test library.
    ;; Handles special cases: infinities and NaN
    (define (%approx-equal? a b epsilon)
      "Return true if A and B are approximately equal within EPSILON.\nHandles infinities, NaN, zero, complex numbers, pairs, and vectors"
      (cond
       ((and (number? a) (number? b))
        (cond
         ;; Both infinite with same sign
         ((and (infinite? a) (infinite? b))
          (eqv? a b))
         ;; Both NaN - R7RS says (= +nan.0 +nan.0) is #f, but for testing
         ;; purposes we consider two NaNs as "equal"
         ((and (nan? a) (nan? b))
          #t)
         ;; One is infinite/NaN and other is not
         ((or (infinite? a) (infinite? b) (nan? a) (nan? b))
          #f)
         ;; Both zero (handles -0.0 vs +0.0)
         ((and (zero? a) (zero? b))
          #t)
         ;; One is zero - use absolute difference
         ;; R7RS: Use magnitude instead of abs for complex number support
         ((or (zero? a) (zero? b))
          (< (magnitude (- a b)) epsilon))
         ;; Normal numeric comparison using relative difference
         ;; Compare against the larger absolute value for stability
         (else
          (let ((max-mag (max (magnitude a) (magnitude b))))
            (< (magnitude (/ (- a b) max-mag)) epsilon)))))
       ((and (pair? a) (pair? b))
        (and (%approx-equal? (car a) (car b) epsilon)
             (%approx-equal? (cdr a) (cdr b) epsilon)))
       ((and (vector? a) (vector? b)
             (= (vector-length a) (vector-length b)))
        (let loop ((i 0))
          (or (= i (vector-length a))
              (and (%approx-equal? (vector-ref a i) (vector-ref b i) epsilon)
                   (loop (+ i 1))))))
       (else (equal? a b))))

    ;; Exit with summary
    (define (test-exit . args)
      "Print test summary and exit with status 1 if any tests failed"
      (newline)
      (display "Test Summary:")
      (newline)
      (display "  Passed: ")
      (display *test-pass-count*)
      (newline)
      (display "  Failed: ")
      (display *test-fail-count*)
      (newline)
      (if (> *test-fail-count* 0)
          (exit 1)
          (exit 0)))

    ;; Macro definitions
    (define-syntax test
      (syntax-rules ()
        ((test expected expr)
         (test #f expected expr))
        ((test name expected expr)
         (let ((e expected)
               (a expr))
           (if (if (and (number? e) (inexact? e))
                   (%approx-equal? e a (current-test-epsilon))
                   ((current-test-comparator) e a))
               (%test-pass name)
               (%test-fail name e a))))))

    (define-syntax test-equal
      (syntax-rules ()
        ((test-equal compare expected expr)
         (test-equal #f compare expected expr))
        ((test-equal name compare expected expr)
         (let ((e expected)
               (a expr))
           (if (compare e a)
               (%test-pass name)
               (%test-fail name e a))))))

    (define-syntax test-assert
      (syntax-rules ()
        ((test-assert expr)
         (test-assert #f expr))
        ((test-assert name expr)
         (if expr
             (%test-pass name)
             (%test-fail name #t #f)))))

    (define-syntax test-not
      (syntax-rules ()
        ((test-not expr)
         (test-not #f expr))
        ((test-not name expr)
         (if (not expr)
             (%test-pass name)
             (%test-fail name #f #t)))))

    (define-syntax test-error
      (syntax-rules ()
        ((test-error expr)
         (test-error #f expr))
        ((test-error name expr)
         (guard (exn (else (%test-pass name)))
           expr
           (%test-fail name 'error 'no-error)))))

    (define-syntax test-values
      (syntax-rules ()
        ((test-values expected expr)
         (test-values #f expected expr))
        ((test-values name expected expr)
         (test name
               (call-with-values (lambda () expected) list)
               (call-with-values (lambda () expr) list)))))

    (define-syntax test-group
      (syntax-rules ()
        ((test-group name body ...)
         (begin
           (test-begin name)
           body ...
           (test-end name)))))))
