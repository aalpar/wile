;;; control-test.scm - R7RS 6.10 Control features
;;;
;;; Edge cases and detailed coverage extracted from Go test suite:
;;;   registry/core/prim_control_test.go
;;; Complements the canonical R7RS tests in integration/testdata/r7rs-tests.scm.

(import (scheme base)
        (scheme write)
        (chibi test))

(test-begin "control")

;; ── call/cc: basic ───────────────────────────────────────────────

(test-group "call/cc basic"
  ;; normal return (no escape)
  (test 42 (call/cc (lambda (k) 42)))

  ;; escape via continuation
  (test 42 (call/cc (lambda (k) (k 42))))

  ;; escape skips remaining computation
  (test 11 (+ 1 (call/cc (lambda (k) (+ 2 (k 10))))))

  ;; normal return continues surrounding computation
  (test 11 (+ 1 (call/cc (lambda (k) 10))))

  ;; call-with-current-continuation alias
  (test 99 (call-with-current-continuation (lambda (k) (k 99))))

  ;; call/cc inside nested function
  (test 11
    (let ()
      (define (f) (+ 1 (call/cc (lambda (k) (k 10)))))
      (f))))

;; ── call/cc: multi-invoke ────────────────────────────────────────

(test-group "call/cc multi-invoke"
  ;; continuation invoked after call/cc returns
  (test 2
    (let ((k-saved #f))
      (let ((result (call/cc (lambda (k) (set! k-saved k) 1))))
        (if (= result 1)
            (k-saved 2)
            result))))

  ;; continuation invoked multiple times
  (test 3
    (let ((k-saved #f) (count 0))
      (let ((result (call/cc (lambda (k) (set! k-saved k) 'first))))
        (set! count (+ count 1))
        (if (< count 3)
            (k-saved count)
            count))))

  ;; saved continuation invoked later with begin
  (test 3
    (begin
      (define k-saved-ctrl #f)
      (define count-ctrl 0)
      (let ((result (call/cc (lambda (k) (set! k-saved-ctrl k) 'first))))
        (set! count-ctrl (+ count-ctrl 1))
        (if (< count-ctrl 3)
            (k-saved-ctrl 'again)
            count-ctrl)))))

;; ── call/cc: with higher-order functions ─────────────────────────

(test-group "call/cc with higher-order functions"
  ;; escape from apply
  (test 'big
    (call/cc (lambda (return)
      (apply (lambda (a b) (if (> b 5) (return 'big) (+ a b)))
             '(1 10)))))

  ;; escape from map
  (test 'found
    (call/cc (lambda (return)
      (map (lambda (x) (if (> x 3) (return 'found) (* x x)))
           '(1 2 3 4 5)))))

  ;; no escape from map returns list
  (test '(1 4 9)
    (call/cc (lambda (return)
      (map (lambda (x) (* x x)) '(1 2 3))))))

;; ── call/cc: coroutines ──────────────────────────────────────────

(test-group "call/cc coroutines"
  ;; single coroutine yield and resume
  ;; Output: "1 2 end" -- coroutine runs, yields, resumes, scheduler ends.
  (test "1 2 end"
    (let ((p (open-output-string)))
      (define *queue* '())
      (define (enqueue! thunk) (set! *queue* (append *queue* (list thunk))))
      (define (dequeue!) (let ((n (car *queue*))) (set! *queue* (cdr *queue*)) n))
      (define (scheduler-run) (if (not (null? *queue*)) ((dequeue!))))
      (define (spawn thunk) (enqueue! (lambda () (thunk) (scheduler-run))))
      (define (yield) (call/cc (lambda (k) (enqueue! (lambda () (k #f))) (scheduler-run))))

      (spawn (lambda () (display "1 " p) (yield) (display "2 " p)))

      (scheduler-run)
      (display "end" p)
      (get-output-string p)))

  ;; Note: multi-coroutine tests are omitted here because continuation
  ;; re-entry across Go-implemented primitives has known edge cases
  ;; (see TestCallCCSubContextReentry in prim_control_test.go).
  )

;; ── apply ────────────────────────────────────────────────────────

(test-group "apply"
  ;; basic apply
  (test 6 (apply + '(1 2 3)))

  ;; apply with prefix args
  (test 15 (apply + 1 2 '(3 4 5)))

  ;; apply with many prefix args
  (test 21 (apply + 1 2 3 4 '(5 6)))

  ;; apply with empty final list
  (test 6 (apply + 1 2 3 '()))

  ;; apply with empty list (identity for +)
  (test 0 (apply + '()))

  ;; apply list constructor
  (test '(1 2 3 4) (apply list 1 2 '(3 4))))

(test-group "apply errors"
  ;; non-list as final argument
  (test-error (apply + 42))

  ;; improper list as final argument
  (test-error (apply + '(1 . 2))))

;; ── apply with parameters ────────────────────────────────────────

(test-group "apply with parameters"
  ;; apply parameter get
  (test 42
    (let ((p (make-parameter 42)))
      (apply p '())))

  ;; apply parameter set then get
  (test 99
    (let ((p (make-parameter 0)))
      (apply p '(99))
      (p)))

  ;; apply parameter with converter
  (test 10
    (let ((p (make-parameter 0 (lambda (x) (* x 2)))))
      (apply p '(5))
      (p))))

;; ── values ───────────────────────────────────────────────────────

(test-group "values"
  ;; single value
  (test 42 (values 42)))

;; ── call-with-values ─────────────────────────────────────────────

(test-group "call-with-values"
  ;; single value producer
  (test 84
    (call-with-values (lambda () 42) (lambda (x) (* x 2))))

  ;; multiple values: sum
  (test 6
    (call-with-values (lambda () (values 1 2 3))
                      (lambda (a b c) (+ a b c))))

  ;; consumer builds list
  (test '(a b c)
    (call-with-values (lambda () (values 'a 'b 'c)) list))

  ;; no values producer
  (test 'done
    (call-with-values (lambda () (values)) (lambda () 'done)))

  ;; single value passthrough
  (test 42
    (call-with-values (lambda () 42) (lambda (x) x)))

  ;; two values
  (test 3
    (call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y))))

  ;; three values
  (test 6
    (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))))

(test-group "call-with-values errors"
  ;; exception in producer
  (test-error (call-with-values (lambda () (error "fail")) (lambda (x) x)))

  ;; exception in consumer
  (test-error (call-with-values (lambda () 42) (lambda (x) (error "fail")))))

;; ── map: multiple lists ──────────────────────────────────────────

(test-group "map multiple lists"
  ;; single list with lambda
  (test '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))

  ;; two lists with +
  (test '(11 22 33) (map + '(1 2 3) '(10 20 30)))

  ;; three lists
  (test '(111 222 333)
    (map (lambda (a b c) (+ a b c))
         '(1 2 3) '(10 20 30) '(100 200 300)))

  ;; map list constructor over two lists
  (test '((a 1) (b 2) (c 3))
    (map list '(a b c) '(1 2 3)))

  ;; empty lists
  (test '() (map + '() '()))

  ;; unequal length lists (stops at shortest)
  (test '(11 22) (map + '(1 2 3) '(10 20))))

(test-group "map errors"
  ;; exception in mapped procedure
  (test-error (map (lambda (x) (if (= x 2) (error "boom") x)) '(1 2 3)))

  ;; improper list argument
  (test-error (map (lambda (x) x) '(1 . 2))))

;; ── for-each: multiple lists ─────────────────────────────────────

(test-group "for-each multiple lists"
  ;; for-each with side effects
  (test '(3 2 1)
    (let ((result '()))
      (for-each (lambda (x) (set! result (cons x result)))
                '(1 2 3))
      result))

  ;; two lists
  (test '(30 20 10)
    (let ((result '()))
      (for-each (lambda (x y)
                  (set! result (cons (+ x y) result)))
                '(1 2 3) '(9 18 27))
      result))

  ;; unequal length lists stops at shortest
  (test 2
    (let ((count 0))
      (for-each (lambda (x y) (set! count (+ count 1)))
                '(1 2 3) '(10 20))
      count)))

;; ── dynamic-wind ─────────────────────────────────────────────────

(test-group "dynamic-wind basic"
  ;; returns thunk result
  (test 42
    (dynamic-wind (lambda () 'before) (lambda () 42) (lambda () 'after)))

  ;; before runs first (thunk sees before's mutation)
  (test 1
    (let ((v (make-vector 1 0)))
      (dynamic-wind
        (lambda () (vector-set! v 0 1))
        (lambda () (vector-ref v 0))
        (lambda () (vector-set! v 0 2)))))

  ;; before/during/after ordering
  (test '(after during before)
    (let ((result '()))
      (dynamic-wind
        (lambda () (set! result (cons 'before result)))
        (lambda () (set! result (cons 'during result)) 42)
        (lambda () (set! result (cons 'after result))))
      result)))

(test-group "dynamic-wind with escape"
  ;; escape returns correct value
  (test 77
    (call/cc (lambda (k)
      (dynamic-wind
        (lambda () #f)
        (lambda () (k 77))
        (lambda () #f)))))

  ;; after thunk runs on escape
  (test 2
    (let ((v (make-vector 1 0)))
      (call/cc (lambda (k)
        (dynamic-wind
          (lambda () (vector-set! v 0 1))
          (lambda () (k 99))
          (lambda () (vector-set! v 0 2)))))
      (vector-ref v 0))))

(test-group "dynamic-wind exception in thunks"
  ;; exception in before thunk
  (test-error
    (dynamic-wind (lambda () (error "before-fail"))
                  (lambda () 1)
                  (lambda () 2)))

  ;; exception in after thunk
  (test-error
    (dynamic-wind (lambda () #f)
                  (lambda () 42)
                  (lambda () (error "after-fail"))))

  ;; after thunk runs on body exception
  (test #t
    (let ((after-ran #f))
      (guard (e (#t after-ran))
        (dynamic-wind
          (lambda () #f)
          (lambda () (error "body-fail"))
          (lambda () (set! after-ran #t)))))))

(test-end)
(test-exit)
