;; Bootstrap Procedures
;;
;; Scheme procedure definitions (define) loaded after bootstrap macros.
;; This file is embedded at compile-time via go:embed.
;;
;; These procedures may use syntactic forms and macros defined in
;; bootstrap_macros.scm (e.g. forms/macros such as case-lambda, let,
;; begin, and). They are loaded before any user code runs.

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
