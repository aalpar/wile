;; Mutating vector-map / string-map — the DEFAULT bootstrap fragment.
;;
;; Extracted from bootstrap_procedures.scm into a standalone procedure source so a
;; dialect can swap it: NoMutation replaces this fragment with
;; bootstrap_maps_immutable.scm, letting the mutation primitives (vector-set!,
;; string-set!, …) be removed entirely. Every other engine loads this version.
;;
;; Implemented in Scheme so iteration produces capturable Scheme continuation
;; frames (call/cc inside the callback). The result is a fresh object filled in
;; place with vector-set! / string-set! (fastest; no intermediate list).

(define vector-map
  (case-lambda
    ((f v)
     "Apply F to each element of vector V, returning a new vector\nof results. With multiple vectors, F receives one element from\neach vector per call. The result length is the minimum of all\ninput lengths.\n\nParameters:\n  f : procedure\n  v : vector\nReturns: vector\nCategory: vectors\n\nSee also: `map', `vector-for-each'."
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

(define string-map
  (case-lambda
    ((f s)
     "Apply F to each character of string S, returning a new string\nof results. F must return a character. With multiple strings,\nF receives one character from each string per call. The result\nlength is the minimum of all input lengths.\n\nParameters:\n  f : procedure\n  s : string\nReturns: string\nCategory: strings\n\nSee also: `map', `string-for-each'."
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
