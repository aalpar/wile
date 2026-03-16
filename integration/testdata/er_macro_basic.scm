;; Basic ER macro tests

;; Identity macro — returns the second element of the form
(define-syntax my-id
  (er-macro-transformer
    (lambda (form rename compare)
      (cadr form))))

;; Test basic expansion
(define result (my-id 42))
(if (= result 42)
    (display "PASS: my-id basic\n")
    (begin (display "FAIL: my-id basic, got ")
           (display result)
           (newline)
           (exit 1)))

;; Swap macro using rename for hygiene
(define-syntax my-swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'set!) a b)
              (list (rename 'set!) b (rename 'tmp)))))))

(define x 1)
(define y 2)
(my-swap! x y)
(if (and (= x 2) (= y 1))
    (display "PASS: my-swap!\n")
    (begin (display "FAIL: my-swap!, got x=")
           (display x)
           (display " y=")
           (display y)
           (newline)
           (exit 1)))

;; Macro that constructs a list expression
(define-syntax make-list-of-three
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form))
            (c (cadddr form)))
        (list (rename 'list) a b c)))))

(define r2 (make-list-of-three 10 20 30))
(if (equal? r2 '(10 20 30))
    (display "PASS: make-list-of-three\n")
    (begin (display "FAIL: make-list-of-three, got ")
           (display r2)
           (newline)
           (exit 1)))

(display "All basic ER macro tests passed\n")
