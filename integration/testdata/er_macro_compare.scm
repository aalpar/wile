;; ER macro compare tests — literal matching via compare closure

;; Simple conditional macro
(define-syntax my-if
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((test (cadr form))
            (consequent (caddr form))
            (alternative (if (null? (cdddr form))
                             #f
                             (cadddr form))))
        (list (rename 'if) test consequent alternative)))))

;; Test 1: basic if
(define r1 (my-if #t 1 2))
(if (= r1 1)
    (display "PASS: my-if basic\n")
    (begin (display "FAIL: my-if basic, got ")
           (display r1)
           (newline)
           (exit 1)))

;; my-when uses cons to attach rename'd begin to the body list
(define-syntax my-when
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((test (cadr form))
            (body (cddr form)))
        (list (rename 'if) test
              (cons (rename 'begin) body)
              #f)))))

;; Test 2: my-when
(define r2 #f)
(my-when #t (set! r2 42))
(if (= r2 42)
    (display "PASS: my-when\n")
    (begin (display "FAIL: my-when, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 3: compare identifies same binding
(define-syntax literal-check
  (er-macro-transformer
    (lambda (form rename compare)
      (if (compare (cadr form) (rename 'magic))
          (list (rename 'quote) 'found-magic)
          (list (rename 'quote) 'not-magic)))))

(define r3 (literal-check magic))
(if (eq? r3 'found-magic)
    (display "PASS: compare found-magic\n")
    (begin (display "FAIL: compare found-magic, got ")
           (display r3)
           (newline)
           (exit 1)))

(define r4 (literal-check other))
(if (eq? r4 'not-magic)
    (display "PASS: compare not-magic\n")
    (begin (display "FAIL: compare not-magic, got ")
           (display r4)
           (newline)
           (exit 1)))

(display "All compare ER macro tests passed\n")
