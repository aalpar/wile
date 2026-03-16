;; ER macro hygiene tests

;; Test 1: renamed keywords resolve to the correct bindings.
;; The macro uses (rename 'if), (rename 'let), (rename 'set!) to
;; ensure these resolve to the standard bindings even if the user
;; shadows them.
(define-syntax my-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))

;; Basic falsy case: first arg is #f so returns second arg
(define r1 (my-or #f 42))
(if (= r1 42)
    (display "PASS: my-or falsy\n")
    (begin (display "FAIL: my-or falsy, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Basic truthy case: first arg is truthy so returns it
(define r2 (my-or 7 42))
(if (= r2 7)
    (display "PASS: my-or truthy\n")
    (begin (display "FAIL: my-or truthy, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 2: swap macro -- renamed symbols create correct bindings
(define-syntax my-swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        (list (rename 'let) (list (list (rename 'temp) a))
              (list (rename 'set!) a b)
              (list (rename 'set!) b (rename 'temp)))))))

;; Swap works with user variables that are NOT named 'temp'
(define p 10)
(define q 20)
(my-swap! p q)
(if (and (= p 20) (= q 10))
    (display "PASS: my-swap! basic\n")
    (begin (display "FAIL: my-swap! basic, got p=")
           (display p)
           (display " q=")
           (display q)
           (newline)
           (exit 1)))

;; Test 3: un-renamed symbols resolve at use site (intentional hygiene breaking).
;; The anaphoric 'it' is deliberately NOT renamed so that it's visible at use site.
(define-syntax aif
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((test (cadr form))
            (then (caddr form))
            (els  (if (null? (cdddr form)) #f (cadddr form))))
        ;; 'it' is NOT renamed -- intentionally anaphoric
        (list (rename 'let) (list (list 'it test))
              (list (rename 'if) 'it then els))))))

(define r3 (aif (+ 1 2) (* it 10) 0))
(if (= r3 30)
    (display "PASS: aif anaphoric\n")
    (begin (display "FAIL: aif anaphoric, got ")
           (display r3)
           (newline)
           (exit 1)))

;; Test 4: aif with falsy test takes else branch
(define r4 (aif #f 'yes 'no))
(if (eq? r4 'no)
    (display "PASS: aif else branch\n")
    (begin (display "FAIL: aif else branch, got ")
           (display r4)
           (newline)
           (exit 1)))

(display "All hygiene ER macro tests passed\n")
