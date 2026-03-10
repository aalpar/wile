;; Bootstrap Procedures
;;
;; Scheme procedure definitions (define) loaded after bootstrap macros.
;; This file is embedded at compile-time via go:embed.
;;
;; These procedures may use syntactic forms and macros defined in
;; bootstrap_macros.scm (e.g. forms/macros such as case-lambda, let,
;; begin, and). They are loaded before any user code runs.

;; CxR accessors (R7RS §6.4, also in (scheme cxr) library)
;; Defined here so they are available to bootstrap procedures below
;; (e.g. assoc uses caar, list? uses cddr).

;; 2-level
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
;; 3-level
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
;; 4-level
(define (caaaar x) (car (car (car (car x)))))
(define (caaadr x) (car (car (car (cdr x)))))
(define (caadar x) (car (car (cdr (car x)))))
(define (caaddr x) (car (car (cdr (cdr x)))))
(define (cadaar x) (car (cdr (car (car x)))))
(define (cadadr x) (car (cdr (car (cdr x)))))
(define (caddar x) (car (cdr (cdr (car x)))))
(define (cadddr x) (car (cdr (cdr (cdr x)))))
(define (cdaaar x) (cdr (car (car (car x)))))
(define (cdaadr x) (cdr (car (car (cdr x)))))
(define (cdadar x) (cdr (car (cdr (car x)))))
(define (cdaddr x) (cdr (car (cdr (cdr x)))))
(define (cddaar x) (cdr (cdr (car (car x)))))
(define (cddadr x) (cdr (cdr (car (cdr x)))))
(define (cdddar x) (cdr (cdr (cdr (car x)))))
(define (cddddr x) (cdr (cdr (cdr (cdr x)))))

;; Higher-order list operations
;; Implemented in Scheme so that iteration produces capturable Scheme
;; continuation frames (enabling call/cc inside map/for-each callbacks).
(define map
  (case-lambda
    ((f lst)
     (let loop ((lst lst))
       (if (null? lst) '()
           (cons (f (car lst)) (loop (cdr lst))))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           '()
           (cons (apply f (map car all))
                 (loop (map cdr all))))))))

(define for-each
  (case-lambda
    ((f lst)
     (let loop ((lst lst))
       (if (null? lst) (if #f #f)
           (begin (f (car lst)) (loop (cdr lst))))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           (if #f #f)
           (begin (apply f (map car all))
                  (loop (map cdr all))))))))

;; Vector higher-order operations
;; Implemented in Scheme so that iteration produces capturable Scheme
;; continuation frames (enabling call/cc inside callbacks).

(define vector-map
  (case-lambda
    ((f v)
     (let ((len (vector-length v)))
       (let ((result (make-vector len)))
         (let loop ((i 0))
           (if (< i len)
               (begin
                 (vector-set! result i (f (vector-ref v i)))
                 (loop (+ i 1)))
               result)))))
    ((f v1 . rest)
     (let ((vecs (cons v1 rest)))
       (let ((len (apply min (map vector-length vecs))))
         (let ((result (make-vector len)))
           (let loop ((i 0))
             (if (< i len)
                 (begin
                   (vector-set! result i
                     (apply f (map (lambda (v) (vector-ref v i)) vecs)))
                   (loop (+ i 1)))
                 result))))))))

(define vector-for-each
  (case-lambda
    ((f v)
     (let ((len (vector-length v)))
       (let loop ((i 0))
         (if (< i len)
             (begin
               (f (vector-ref v i))
               (loop (+ i 1)))))))
    ((f v1 . rest)
     (let ((vecs (cons v1 rest)))
       (let ((len (apply min (map vector-length vecs))))
         (let loop ((i 0))
           (if (< i len)
               (begin
                 (apply f (map (lambda (v) (vector-ref v i)) vecs))
                 (loop (+ i 1))))))))))

;; String higher-order operations

(define string-map
  (case-lambda
    ((f s)
     (let ((len (string-length s)))
       (let ((result (make-string len)))
         (let loop ((i 0))
           (if (< i len)
               (begin
                 (string-set! result i (f (string-ref s i)))
                 (loop (+ i 1)))
               result)))))
    ((f s1 . rest)
     (let ((strs (cons s1 rest)))
       (let ((len (apply min (map string-length strs))))
         (let ((result (make-string len)))
           (let loop ((i 0))
             (if (< i len)
                 (begin
                   (string-set! result i
                     (apply f (map (lambda (s) (string-ref s i)) strs)))
                   (loop (+ i 1)))
                 result))))))))

(define string-for-each
  (case-lambda
    ((f s)
     (let ((len (string-length s)))
       (let loop ((i 0))
         (if (< i len)
             (begin
               (f (string-ref s i))
               (loop (+ i 1)))))))
    ((f s1 . rest)
     (let ((strs (cons s1 rest)))
       (let ((len (apply min (map string-length strs))))
         (let loop ((i 0))
           (if (< i len)
               (begin
                 (apply f (map (lambda (s) (string-ref s i)) strs))
                 (loop (+ i 1))))))))))

;; List search with optional comparator
;; Default path uses equal?. Custom comparator path must be Scheme
;; to produce capturable continuation frames.

(define member
  (case-lambda
    ((obj lst) (member obj lst equal?))
    ((obj lst compare)
     (let loop ((lst lst))
       (cond
         ((null? lst) #f)
         ((compare obj (car lst)) lst)
         (else (loop (cdr lst))))))))

(define assoc
  (case-lambda
    ((obj alist) (assoc obj alist equal?))
    ((obj alist compare)
     (let loop ((alist alist))
       (cond
         ((null? alist) #f)
         ((compare obj (caar alist)) (car alist))
         (else (loop (cdr alist))))))))

;; Trivial predicates — pure compositions of existing primitives.

(define (not x) (if x #f #t))

(define (zero? z) (= z 0))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

(define (exact-integer? x) (and (integer? x) (exact? x)))

;; list? — must detect cycles (R7RS §6.4: "Returns #t if obj is a proper list")
;; Uses tortoise-and-hare for cycle detection.
(define (list? x)
  (let loop ((slow x) (fast x))
    (cond
      ((null? fast) #t)
      ((not (pair? fast)) #f)
      ((null? (cdr fast)) #t)
      ((not (pair? (cdr fast))) #f)
      ((eq? slow (cdr fast)) #f)
      (else (loop (cdr slow) (cddr fast))))))

;; boolean=? — variadic, all args must be booleans and equal.
;; Type check uses boolean? guard; non-boolean arg triggers a
;; car-of-non-pair error via (car #f) as a deliberate crash —
;; (error) is not available in core bootstrap.
(define (boolean=? b1 b2 . rest)
  (define (check x) (if (boolean? x) x (car #f)))
  (check b1)
  (let loop ((prev b1) (args (cons b2 rest)))
    (if (null? args) #t
        (let ((curr (car args)))
          (check curr)
          (and (eq? prev curr)
               (loop curr (cdr args)))))))

;; symbol=? — same pattern
(define (symbol=? s1 s2 . rest)
  (define (check x) (if (symbol? x) x (car #f)))
  (check s1)
  (let loop ((prev s1) (args (cons s2 rest)))
    (if (null? args) #t
        (let ((curr (car args)))
          (check curr)
          (and (eq? prev curr)
               (loop curr (cdr args)))))))

(define (square x) (* x x))
