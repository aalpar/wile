;;; lazy-test.scm - R7RS 4.2.5 Lazy evaluation: extended coverage
;;;
;;; Test cases extracted from Go test suite:
;;;   - registry/core/prim_promise_test.go
;;;   - registry/core/prim_promise_extra_test.go
;;;   - internal/extensions/all/prim_all_test.go
;;;
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.
;;; Does not duplicate tests already present there (basic force/delay,
;;; memoization, promise?, make-promise, delay-force stream-filter,
;;; promise? on forced promise).

(import (scheme base)
        (scheme lazy)
        (chibi test))

(test-begin "lazy")

;; -- promise? on non-promise types -------------------------------------------

(test-group "promise? non-promise types"
  (test #f (promise? 42))
  (test #f (promise? "hello"))
  (test #f (promise? '(1 2 3)))
  (test #f (promise? #t))
  (test #f (promise? #f))
  (test #f (promise? #(1 2 3)))
  (test #f (promise? '()))
  (test #f (promise? (lambda () 1))))

;; -- make-promise ------------------------------------------------------------

(test-group "make-promise wraps values"
  (test 42 (force (make-promise 42)))
  (test 3 (force (make-promise (+ 1 2))))
  (test "hello" (force (make-promise "hello"))))

(test-group "make-promise idempotent on promises"
  (test #t (let ((p (delay 42)))
             (eq? p (make-promise p))))
  (test #t (let ((p (make-promise 'x)))
             (eq? p (make-promise p)))))

(test-group "make-promise edge cases"
  (test 42 (force (make-promise (make-promise 42))))
  (test '() (force (make-promise '())))
  (test #f (force (make-promise #f)))
  (test "test" (force (make-promise "test"))))

;; -- force -------------------------------------------------------------------

(test-group "force delayed arithmetic"
  (test 3 (force (delay (+ 1 2))))
  (test 42 (force (delay 42))))

(test-group "force non-promise passthrough"
  (test 5 (force 5))
  (test "hello" (force "hello")))

(test-group "force memoization"
  (test 1 (let ((count 0))
            (let ((p (delay (begin (set! count (+ count 1)) count))))
              (force p)
              (force p)
              (force p)
              count))))

(test-group "force nested delay"
  (test 10 (force (delay (delay 10))))
  (test 42 (force (delay (delay (delay (delay 42)))))))

(test-group "force result types"
  (test #t (force (delay #t)))
  (test '(1 2) (force (delay (cons 1 (cons 2 '())))))
  (test '(1 2) (force (delay (list 1 2)))))

(test-group "force with complex expressions"
  (test '(2 4 6) (force (delay (map (lambda (x) (* x 2)) '(1 2 3)))))
  (test '(2 3) (force (delay (cdr '(1 2 3)))))
  (test "hello world" (force (delay (string-append "hello" " " "world")))))

(test-group "force idempotent"
  (test #t (let ((p (delay (+ 1 2))))
             (let ((r1 (force p)))
               (let ((r2 (force p)))
                 (= r1 r2)))))
  (test #t (let ((p (delay 'hello)))
             (eq? (force p) (force p)))))

;; -- promise with side effects -----------------------------------------------

(test-group "promise body evaluates once"
  (test 2 (let ((x 0))
            (let ((p (delay (begin (set! x (+ x 1)) x))))
              (+ (force p) (force p))))))

;; -- delay-force -------------------------------------------------------------

(test-group "delay-force simple"
  (test 5 (force (delay-force (make-promise 5))))
  (test 10 (force (delay-force (delay 10)))))

(test-group "delay-force chain"
  (test 7 (force (delay-force (delay-force (make-promise 7)))))
  (test 7 (force (delay-force (delay-force (delay-force (delay 7)))))))

(test-group "delay-force recursive iteration"
  (test 'done
    (letrec ((lazy-countdown
               (lambda (n)
                 (if (= n 0)
                     (delay 'done)
                     (delay-force (lazy-countdown (- n 1)))))))
      (force (lazy-countdown 100))))
  (test 100
    (begin
      (define (stream-count n limit)
        (if (>= n limit)
            (delay n)
            (delay-force (stream-count (+ n 1) limit))))
      (force (stream-count 0 100)))))

;; -- promise returning a different promise -----------------------------------

(test-group "promise returning different promise"
  (test 99 (let ((p1 (delay 99)))
             (let ((p2 (delay (force p1))))
               (force p2)))))

;; -- force exception in delay body -------------------------------------------

(test-group "force exception in delay body"
  (test-error (force (delay (error "boom"))))
  (test 2 (let ((count 0))
            (let ((p (delay (begin (set! count (+ count 1))
                                   (if (= count 1) (error "first") count)))))
              (guard (e (#t 'caught))
                (force p))
              (force p)))))

(test-end)
(test-exit)
