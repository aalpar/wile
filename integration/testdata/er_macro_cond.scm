;; my-cond: a real-world ER macro pattern using compare for literal matching.
;; Tests recursive ER self-invocation and compare-based dispatch on 'else'.

(define-syntax my-cond
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cdr form))
          ;; No clauses — unspecified
          (list (rename 'if) #f #f)
          (let ((clause (cadr form))
                (rest (cddr form)))
            (if (and (symbol? (car clause))
                     (compare (car clause) (rename 'else)))
                ;; else clause: (begin body ...)
                (cons (rename 'begin) (cdr clause))
                ;; normal clause: (if test (begin body ...) (my-cond rest ...))
                ;; 'my-cond' un-renamed — recursive self-invocation via re-expansion
                (list (rename 'if) (car clause)
                      (cons (rename 'begin) (cdr clause))
                      (cons 'my-cond rest))))))))

;; Test 1: first clause matches
(define r1 (my-cond (#t 'first) (#f 'second) (else 'fallback)))
(if (eq? r1 'first)
    (display "PASS: my-cond first clause\n")
    (begin (display "FAIL: my-cond first clause, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Test 2: second clause matches
(define r2 (my-cond (#f 'first) (#t 'second) (else 'fallback)))
(if (eq? r2 'second)
    (display "PASS: my-cond second clause\n")
    (begin (display "FAIL: my-cond second clause, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 3: else clause
(define r3 (my-cond (#f 'first) (#f 'second) (else 'fallback)))
(if (eq? r3 'fallback)
    (display "PASS: my-cond else clause\n")
    (begin (display "FAIL: my-cond else clause, got ")
           (display r3)
           (newline)
           (exit 1)))

;; Test 4: single else clause
(define r4 (my-cond (else 42)))
(if (= r4 42)
    (display "PASS: my-cond single else\n")
    (begin (display "FAIL: my-cond single else, got ")
           (display r4)
           (newline)
           (exit 1)))

;; Test 5: clause body with multiple expressions
(define r5 0)
(my-cond (#t (set! r5 1) (set! r5 (+ r5 10)))
         (else 'nope))
(if (= r5 11)
    (display "PASS: my-cond multi-expression body\n")
    (begin (display "FAIL: my-cond multi-expression body, got ")
           (display r5)
           (newline)
           (exit 1)))

;; Test 6: no clauses — returns unspecified (not an error)
(my-cond)
(display "PASS: my-cond no clauses\n")

;; Test 7: many clauses — exercises deep recursive expansion
(define r7 (my-cond (#f 1) (#f 2) (#f 3) (#f 4) (#t 5) (else 6)))
(if (= r7 5)
    (display "PASS: my-cond deep recursion\n")
    (begin (display "FAIL: my-cond deep recursion, got ")
           (display r7)
           (newline)
           (exit 1)))

;; Test 8: else only matches as literal, not as a variable
;; Define a variable named 'else-val' to make sure compare doesn't false-match
(define else-val 'not-else)
(define r8 (my-cond (#f 'skip) (#t else-val) (else 'fell-through)))
(if (eq? r8 'not-else)
    (display "PASS: my-cond else is literal only\n")
    (begin (display "FAIL: my-cond else is literal only, got ")
           (display r8)
           (newline)
           (exit 1)))

(display "All my-cond ER macro tests passed\n")
