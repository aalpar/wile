;; Mixed ER + syntax-rules tests — macros of one kind expanding to calls of the other.
;; Verifies that the two macro systems compose correctly through re-expansion.

;; --- syntax-rules macros ---

(define-syntax sr-add1
  (syntax-rules ()
    ((sr-add1 x) (+ x 1))))

(define-syntax sr-swap!
  (syntax-rules ()
    ((sr-swap! a b)
     (let ((tmp a))
       (set! a b)
       (set! b tmp)))))

;; --- ER macros ---

(define-syntax er-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        (list (rename 'let) (list (list (rename 'tmp) a))
              (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))

;; Test 1: ER macro expanding to syntax-rules macro call
(define-syntax er-add2
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form)))
        ;; Expands to (sr-add1 (sr-add1 x))
        (list 'sr-add1 (list 'sr-add1 x))))))

(define r1 (er-add2 10))
(if (= r1 12)
    (display "PASS: ER expanding to syntax-rules\n")
    (begin (display "FAIL: ER expanding to syntax-rules, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Test 2: syntax-rules macro expanding to ER macro call
(define-syntax sr-or-add1
  (syntax-rules ()
    ((sr-or-add1 a b)
     (sr-add1 (er-or a b)))))

(define r2 (sr-or-add1 #f 5))
(if (= r2 6)
    (display "PASS: syntax-rules expanding to ER\n")
    (begin (display "FAIL: syntax-rules expanding to ER, got ")
           (display r2)
           (newline)
           (exit 1)))

(define r3 (sr-or-add1 7 99))
(if (= r3 8)
    (display "PASS: syntax-rules expanding to ER (truthy)\n")
    (begin (display "FAIL: syntax-rules expanding to ER (truthy), got ")
           (display r3)
           (newline)
           (exit 1)))

;; Test 3: ER macro that delegates to syntax-rules swap — hygiene preserved across both systems
(define-syntax er-rotate3!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form))
            (c (cadddr form)))
        ;; Rotates a b c by doing two swaps: swap a b, then swap b c
        ;; Uses syntax-rules sr-swap! from ER expansion
        (list (rename 'begin)
              (list 'sr-swap! a b)
              (list 'sr-swap! b c))))))

(define x 1)
(define y 2)
(define z 3)
(er-rotate3! x y z)
(if (and (= x 2) (= y 3) (= z 1))
    (display "PASS: ER rotate using syntax-rules swap\n")
    (begin (display "FAIL: ER rotate using syntax-rules swap, got x=")
           (display x) (display " y=") (display y) (display " z=") (display z)
           (newline)
           (exit 1)))

;; Test 4: mixed nesting — syntax-rules wrapping ER wrapping syntax-rules
(define-syntax sr-double
  (syntax-rules ()
    ((sr-double x) (+ x x))))

(define-syntax er-double-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        ;; (sr-double (er-or a b))
        (list 'sr-double (list 'er-or a b))))))

(define-syntax sr-wrap-er-double-or
  (syntax-rules ()
    ((sr-wrap-er-double-or a b)
     (+ 1 (er-double-or a b)))))

(define r4 (sr-wrap-er-double-or #f 5))
(if (= r4 11)
    (display "PASS: three-level mixed nesting\n")
    (begin (display "FAIL: three-level mixed nesting, got ")
           (display r4)
           (newline)
           (exit 1)))

(display "All mixed ER + syntax-rules tests passed\n")
