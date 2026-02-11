;;; smoke-test.scm - Basic smoke test for Scheme test infrastructure
;;;
;;; This is a minimal test to verify the test infrastructure is working.
;;; Uses only R7RS-small and (chibi test), so it should work with any
;;; R7RS-compatible Scheme implementation that provides (chibi test).

(import (scheme base)
        (chibi test))

(test-begin "smoke-test")

(test-group "basic arithmetic"
  (test 4 (+ 2 2))
  (test 0 (- 5 5))
  (test 6 (* 2 3))
  (test 2 (/ 4 2)))

(test-group "basic list operations"
  (test '(1 2 3) (cons 1 '(2 3)))
  (test 1 (car '(1 2 3)))
  (test '(2 3) (cdr '(1 2 3)))
  (test #t (null? '()))
  (test #f (null? '(1))))

(test-group "boolean operations"
  (test #t (and #t #t))
  (test #f (and #t #f))
  (test #t (or #t #f))
  (test #f (or #f #f))
  (test #t (not #f))
  (test #f (not #t)))

(test-end)
