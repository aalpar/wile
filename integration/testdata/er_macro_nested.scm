;; Nested ER macro tests — ER macros whose output contains other ER macro calls.
;; Exercises that scope sets survive multiple rounds of expansion.

;; Inner ER macro: doubles a value
(define-syntax er-double
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form)))
        (list (rename '+) x x)))))

;; Test 1: basic inner macro works
(define r1 (er-double 5))
(if (= r1 10)
    (display "PASS: er-double basic\n")
    (begin (display "FAIL: er-double basic, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Outer ER macro: expands to a call of er-double (un-renamed — use-site resolution)
(define-syntax er-double-plus
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form))
            (y (caddr form)))
        ;; Expands to (+ (er-double x) y)
        ;; 'er-double' is NOT renamed — resolves at use site where it's visible
        (list (rename '+) (list 'er-double x) y)))))

;; Test 2: outer expands to inner ER macro call
(define r2 (er-double-plus 3 7))
(if (= r2 13)
    (display "PASS: er-double-plus nested\n")
    (begin (display "FAIL: er-double-plus nested, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 3: ER macro that recursively invokes itself via un-renamed self-reference
(define-syntax er-sum
  (er-macro-transformer
    (lambda (form rename compare)
      (if (null? (cddr form))
          ;; Single argument: return it
          (cadr form)
          ;; Multiple: (+ first (er-sum rest...))
          (list (rename '+) (cadr form)
                (cons 'er-sum (cddr form)))))))

(define r3 (er-sum 1 2 3 4))
(if (= r3 10)
    (display "PASS: er-sum recursive\n")
    (begin (display "FAIL: er-sum recursive, got ")
           (display r3)
           (newline)
           (exit 1)))

;; Test 4: three levels of nesting — macro A expands to B which expands to C
(define-syntax er-negate
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form)))
        (list (rename '-) 0 x)))))

(define-syntax er-abs
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form)))
        ;; (if (< x 0) (er-negate x) x)
        (list (rename 'if) (list (rename '<) x 0)
              (list 'er-negate x)
              x)))))

(define-syntax er-abs-double
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((x (cadr form)))
        ;; (er-double (er-abs x))
        (list 'er-double (list 'er-abs x))))))

(define r4 (er-abs-double -3))
(if (= r4 6)
    (display "PASS: er-abs-double three levels\n")
    (begin (display "FAIL: er-abs-double three levels, got ")
           (display r4)
           (newline)
           (exit 1)))

(define r5 (er-abs-double 4))
(if (= r5 8)
    (display "PASS: er-abs-double positive\n")
    (begin (display "FAIL: er-abs-double positive, got ")
           (display r5)
           (newline)
           (exit 1)))

(display "All nested ER macro tests passed\n")
