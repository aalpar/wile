;;; continuation-test.scm - First-class continuations, dynamic-wind, guard, prompts.
;;;
;;; Item 5 of plans/2026-06-28-continuation-testing-enhancement.md: exercise the
;;; continuation subsystem through the CLI / run-all.sh path (EvalMultiple, begin-wrapped
;;; top level), which is DISTINCT from the Go internal-pipeline test path and is where
;;; two regressions in the reification+flip arc surfaced that `go test ./...` did not
;;; show (the consumer-handler-visibility bug and the tail-promoted-op nil-deref).
;;;
;;; Complements the Go oracle (continuation_escape_past_oracle_test.go), the golden
;;; corpus, the invariant matrix, and the fuzzer.

(import (scheme base)
        (scheme write)
        (chibi test))

(test-begin "continuations")

;; ── call/cc: escape, value flow, multi-shot ──────────────────────
(test-group "call/cc"
  (test 10 (call/cc (lambda (k) (+ 1 (k 10)))))
  (test 6 (+ 1 (call/cc (lambda (k) (k 5)))))
  (test #t (call/cc procedure?))
  ;; multi-shot counter converges
  (test 3 (let ((k #f) (n 0))
            (call/cc (lambda (c) (set! k c)))
            (set! n (+ n 1))
            (if (< n 3) (k #f) n)))
  ;; escape PAST a call-with-values boundary: the consumer must not run
  (test 'escaped (let ((k #f) (entered #f))
                   (call/cc (lambda (c) (set! k c)))
                   (if entered 'escaped
                       (begin (set! entered #t)
                              (call-with-values (lambda () (k 1))
                                                (lambda args 'consumer-ran)))))))

;; ── generators: the producer-in-sub-context truncation fix ───────
(test-group "generators"
  ;; a k captured inside the producer replays the consumer + tail -> counts to 3
  (test '(1 2 3)
    (let ((k #f) (count 0) (trace '()))
      (call-with-values
        (lambda () (+ 1 (call/cc (lambda (c) (set! k c) 0))))
        (lambda (x) (set! trace (cons x trace))))
      (set! count (+ count 1))
      (if (< count 3) (k count) (reverse trace)))))

;; ── dynamic-wind: ordering / balance ─────────────────────────────
(test-group "dynamic-wind"
  (test '(b d a)
    (let ((o '()))
      (dynamic-wind (lambda () (set! o (cons 'b o)))
                    (lambda () (set! o (cons 'd o)))
                    (lambda () (set! o (cons 'a o))))
      (reverse o)))
  ;; after-thunk fires exactly once when an exception escapes
  (test '(caught 1)
    (let ((fired 0))
      (guard (e (#t (list 'caught fired)))
        (dynamic-wind (lambda () #f)
                      (lambda () (raise 'x))
                      (lambda () (set! fired (+ fired 1)))))))
  ;; before-thunk re-runs on re-entry through a prompt (winding inheritance)
  (test 2
    (let ((k #f) (before 0))
      (call-with-continuation-prompt
        (lambda ()
          (dynamic-wind (lambda () (set! before (+ before 1)))
                        (lambda () (call/cc (lambda (c) (set! k c) 'first)))
                        (lambda () #f)))
        (default-continuation-prompt-tag) #f)
      (call-with-continuation-prompt
        (lambda () (k 'second))
        (default-continuation-prompt-tag) (lambda (v) v))
      before)))

;; ── guard / exceptions ───────────────────────────────────────────
(test-group "guard"
  (test 5 (guard (e ((number? e) e) (else 'other)) (raise 5)))
  (test '(o sym)
    (guard (outer ((symbol? outer) (list 'o outer)))
      (guard (inner ((number? inner) (list 'i inner)))
        (raise 'sym))))
  ;; re-raise to an outer with-exception-handler returns its value (no spurious escalate)
  (test '(caught 42)
    (with-exception-handler
      (lambda (e) (list 'caught e))
      (lambda () (guard (inner ((symbol? inner) 'sym)) (raise 42)))))
  ;; a raise-continuable inside a tail-position consumer reaches the handler
  (test 'consumer-error
    (let ((caught #f))
      (with-exception-handler
        (lambda (e) (set! caught e) 'handled)
        (lambda ()
          (call-with-values (lambda () (values 1 2))
                            (lambda (a b) (raise-continuable 'consumer-error) (+ a b)))))
      caught))
  ;; a promoted-op type error in a TAIL position under guard is caught, not a crash
  (test 'c (let ((f (lambda (x) (car x)))) (guard (e (#t 'c)) (f 5))))
  (test 'c (let ((sq (lambda (x) (* x x)))) (guard (e (#t 'c)) (sq "s")))))

;; ── call-with-values / call-with-exit / prompts ──────────────────
(test-group "values and exits"
  (test '(1 2 3) (call-with-values (lambda () (values 1 2 3)) list))
  (test 'none (call-with-values (lambda () (values)) (lambda () 'none)))
  (test 42 (call-with-exit (lambda (e) (e 42) 99)))
  ;; tail loop through call-with-values runs without overflow
  (test 'done
    (let loop ((n 200000))
      (call-with-values (lambda () (values n))
                        (lambda (m) (if (= m 0) 'done (loop (- m 1)))))))
  (test 82
    (call-with-continuation-prompt
      (lambda () (+ 1 (abort-current-continuation (default-continuation-prompt-tag) 41)))
      (default-continuation-prompt-tag) (lambda (v) (* v 2)))))

;; ── parameterize under control ───────────────────────────────────
(test-group "parameterize"
  (test '(1 2 1)
    (let ((p (make-parameter 1)))
      (list (p) (parameterize ((p 2)) (p)) (p)))))

(test-end)
(test-exit)
