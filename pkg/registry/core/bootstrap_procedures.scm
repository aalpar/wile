;; Bootstrap Procedures
;;
;; Scheme procedure definitions (define) loaded after bootstrap macros.
;; This file is embedded at compile-time via go:embed.
;;
;; These procedures may use syntactic forms and macros defined in
;; bootstrap_macros.scm (e.g. forms/macros such as case-lambda, let,
;; begin, and). They are loaded before any user code runs.

;; Exception handling (R7RS §6.11)
;;
;; The current exception-handler stack rides %exception-handlers, a parameter
;; (continuation-mark-backed), so a continuation captured under a handler is
;; restored with that handler current on re-entry. with-exception-handler pushes
;; a handler by parameterizing the list; raise / raise-continuable / error and the
;; Go-error bridge read it via machine.RaiseInPlace (machine/exception_raise.go).
(define %exception-handlers (%exception-handler-parameter))

(define (with-exception-handler handler thunk)
  "Installs HANDLER as the current exception handler for the dynamic extent of THUNK. HANDLER receives the raised object.\n\nExamples:\n  (with-exception-handler\n    (lambda (e) 42)\n    (lambda () (raise-continuable \"oops\")))  => 42\n\nParameters:\n  handler : procedure\n  thunk : procedure\nReturns: any\nCategory: exceptions"
  (parameterize ((%exception-handlers (cons handler (%exception-handlers))))
    (thunk)))

;; CxR accessors (R7RS §6.4, also in (scheme cxr) library)
;; Defined here so they are available to bootstrap procedures below
;; (e.g. assoc uses caar, list? uses cddr).

;; 2-level
(define (caar x)
  "Return (car (car X)).\nExtract the car of the car of a pair.\n\nExamples:\n  (caar '((1 2) 3))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car x)))
(define (cadr x)
  "Return (car (cdr X)).\nExtract the second element of a list.\n\nExamples:\n  (cadr '(1 2 3))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr x)))
(define (cdar x)
  "Return (cdr (car X)).\nExtract the cdr of the car of a pair.\n\nExamples:\n  (cdar '((1 2 3) 4))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car x)))
(define (cddr x)
  "Return (cdr (cdr X)).\nDrop the first two elements of a list.\n\nExamples:\n  (cddr '(1 2 3 4))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr x)))
;; 3-level
(define (caaar x)
  "Return (car (car (car X))).\nThree levels deep: car of car of car.\n\nExamples:\n  (caaar '(((1 2) 3) 4))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (car x))))
(define (caadr x)
  "Return (car (car (cdr X))).\nCar of car of the tail of X.\n\nExamples:\n  (caadr '(1 (2 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (cdr x))))
(define (cadar x)
  "Return (car (cdr (car X))).\nSecond element of the car of X.\n\nExamples:\n  (cadar '((1 2 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (car x))))
(define (caddr x)
  "Return (car (cdr (cdr X))).\nExtract the third element of a list.\n\nExamples:\n  (caddr '(1 2 3 4))  => 3\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (cdr x))))
(define (cdaar x)
  "Return (cdr (car (car X))).\nCdr of car of car of X.\n\nExamples:\n  (cdaar '(((1 2 3) 4) 5))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (car x))))
(define (cdadr x)
  "Return (cdr (car (cdr X))).\nCdr of the second element of X.\n\nExamples:\n  (cdadr '(1 (2 3 4) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (cdr x))))
(define (cddar x)
  "Return (cdr (cdr (car X))).\nDrop two elements from the car of X.\n\nExamples:\n  (cddar '((1 2 3 4) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (car x))))
(define (cdddr x)
  "Return (cdr (cdr (cdr X))).\nDrop the first three elements of a list.\n\nExamples:\n  (cdddr '(1 2 3 4 5))  => (4 5)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (cdr x))))
;; 4-level
(define (caaaar x)
  "Return (car (car (car (car X)))).\nFour levels deep: car composed four times.\n\nExamples:\n  (caaaar '((((1)))))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (car (car x)))))
(define (caaadr x)
  "Return (car (car (car (cdr X)))).\nCar three times into the tail of X.\n\nExamples:\n  (caaadr '(0 ((1))))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (car (cdr x)))))
(define (caadar x)
  "Return (car (car (cdr (car X)))).\nCar of car of second element of car of X.\n\nExamples:\n  (caadar '((0 (1)) 2))  => 1\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (cdr (car x)))))
(define (caaddr x)
  "Return (car (car (cdr (cdr X)))).\nCar of car of the third tail element of X.\n\nExamples:\n  (caaddr '(0 1 (2 3)))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (car (cdr (cdr x)))))
(define (cadaar x)
  "Return (car (cdr (car (car X)))).\nSecond element of car of car of X.\n\nExamples:\n  (cadaar '(((1 2) 3) 4))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (car (car x)))))
(define (cadadr x)
  "Return (car (cdr (car (cdr X)))).\nSecond element of the second element of X.\n\nExamples:\n  (cadadr '(0 (1 2 3)))  => 2\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (car (cdr x)))))
(define (caddar x)
  "Return (car (cdr (cdr (car X)))).\nThird element of the car of X.\n\nExamples:\n  (caddar '((1 2 3 4) 5))  => 3\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (cdr (car x)))))
(define (cadddr x)
  "Return (car (cdr (cdr (cdr X)))).\nExtract the fourth element of a list.\n\nExamples:\n  (cadddr '(1 2 3 4 5))  => 4\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (car (cdr (cdr (cdr x)))))
(define (cdaaar x)
  "Return (cdr (car (car (car X)))).\nCdr of car of car of car of X.\n\nExamples:\n  (cdaaar '((((1 2 3)))))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (car (car x)))))
(define (cdaadr x)
  "Return (cdr (car (car (cdr X)))).\nCdr of car of car of the tail of X.\n\nExamples:\n  (cdaadr '(0 ((1 2 3))))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (car (cdr x)))))
(define (cdadar x)
  "Return (cdr (car (cdr (car X)))).\nCdr of second element of the car of X.\n\nExamples:\n  (cdadar '((0 (1 2 3)) 4))  => (2 3)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (cdr (car x)))))
(define (cdaddr x)
  "Return (cdr (car (cdr (cdr X)))).\nCdr of the third element of a list.\n\nExamples:\n  (cdaddr '(0 1 (2 3 4)))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (car (cdr (cdr x)))))
(define (cddaar x)
  "Return (cdr (cdr (car (car X)))).\nDrop two from car of car of X.\n\nExamples:\n  (cddaar '(((1 2 3 4)) 5))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (car (car x)))))
(define (cddadr x)
  "Return (cdr (cdr (car (cdr X)))).\nDrop two from the second element of X.\n\nExamples:\n  (cddadr '(0 (1 2 3 4)))  => (3 4)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (car (cdr x)))))
(define (cdddar x)
  "Return (cdr (cdr (cdr (car X)))).\nDrop three elements from the car of X.\n\nExamples:\n  (cdddar '((1 2 3 4 5) 6))  => (4 5)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (cdr (car x)))))
(define (cddddr x)
  "Return (cdr (cdr (cdr (cdr X)))).\nDrop the first four elements of a list.\n\nExamples:\n  (cddddr '(1 2 3 4 5 6))  => (5 6)\n\nParameters:\n  x : pair\nReturns: any\nCategory: pairs"
  (cdr (cdr (cdr (cdr x)))))

;; Higher-order list operations
;; Implemented in Scheme so that iteration produces capturable Scheme
;; continuation frames (enabling call/cc inside map/for-each callbacks).
;;
;; Both loops are TAIL recursive, so neither saves a continuation per element and
;; neither has a call-depth ceiling. for-each gets that for free (it discards
;; results); map has to work for it — see below.
;;
;; map accumulates results front-to-back and reverses once at the end. The obvious
;; alternative, structural recursion — (cons (f (car lst)) (loop (cdr lst))) —
;; leaves the cons pending, which is one continuation per element and
;; ErrCallDepthExceeded on a list longer than maxCallDepth.
;;
;; The other tail form, a set-cdr! tail pointer, allocates n+1 cells against this
;; form's 2n and is nonetheless MUCH slower — measured, interleaved A/B, min-of-5:
;; mapbench +34.6% and deriv +16.2% against the old non-tail SCHEME map, versus
;; +8.2% and +8.9% for this form. All three arms are Scheme; the Go PrimMap this
;; file replaced is NOT the baseline and was never benchmarked against any of
;; them. Two reasons, both structural rather than incidental. Per
;; element it runs set-cdr! and an extra cdr on top of the same f and cons this
;; form runs, while reverse links n cells in ONE primitive call through a single
;; PairBlock. And its set-cdr! never reaches the promoted OpSetCdr opcode: the
;; cell argument is compound ((cons (f ...) '())), which demotes the outer call to
;; a generic apply. Restructuring to make it promotable needs a let, whose env
;; frame per iteration costs more than the fused dispatch saves (+38.9%). Promoting
;; set-cdr! does not rescue it; that was tried and measured.
(define map
  (case-lambda
    ((f lst)
     "Apply F to each element of LST, returning a list of results.\nWith multiple lists, F receives one element from each list per\ncall. Stops at the shortest list.\n\nParameters:\n  f : procedure\n  lst : list\nReturns: list\nCategory: lists\n\nSee also: `for-each', `vector-map', `string-map'."
     (let loop ((rest lst) (acc '()))
       (if (null? rest)
           (reverse acc)
           (loop (cdr rest) (cons (f (car rest)) acc)))))
    ((f lst . lsts)
     (let loop ((all (cons lst lsts)) (acc '()))
       (if (let any-null? ((ls all))
             (if (null? ls) #f
                 (if (null? (car ls)) #t
                     (any-null? (cdr ls)))))
           (reverse acc)
           (loop (map cdr all)
                 (cons (apply f (map car all)) acc)))))))

(define for-each
  (case-lambda
    ((f lst)
     "Apply F to each element of LST for its side effects.\nWith multiple lists, F receives one element from each list per\ncall. Stops at the shortest list. Returns an unspecified value.\n\nParameters:\n  f : procedure\n  lst : list\nCategory: lists\n\nSee also: `map', `vector-for-each', `string-for-each'."
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
;;
;; vector-map lives in a separate, dialect-swappable fragment
;; (bootstrap_maps_mutable.scm) because it is the one bootstrap procedure that
;; depends on vector-set!; a no-mutation dialect swaps in the mutation-free
;; bootstrap_maps_immutable.scm. vector-for-each stays here — it uses no mutator.

(define vector-for-each
  (case-lambda
    ((f v)
     "Apply F to each element of vector V for its side effects.\nWith multiple vectors, F receives one element from each vector\nper call. Processes the minimum of all input lengths.\n\nParameters:\n  f : procedure\n  v : vector\nCategory: vectors\n\nSee also: `for-each', `vector-map'."
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
;; string-map lives in the dialect-swappable maps fragment (see vector-map above);
;; string-for-each stays here — it uses no mutator.

(define string-for-each
  (case-lambda
    ((f s)
     "Apply F to each character of string S for its side effects.\nWith multiple strings, F receives one character from each string\nper call. Processes the minimum of all input lengths.\n\nParameters:\n  f : procedure\n  s : string\nCategory: strings\n\nSee also: `for-each', `string-map'."
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
    ((obj lst)
     "Return the first sublist of LST whose car equals OBJ, or #f\nif not found. Uses COMPARE for equality (default `equal?').\n\nExamples:\n  (member 3 '(1 2 3 4))      => (3 4)\n  (member 5 '(1 2 3))        => #f\n  (member 2.0 '(1 2 3) =)    => (2 3)\n\nParameters:\n  obj : any\n  lst : list\nReturns: any\nCategory: lists\n\nSee also: `memq', `memv', `assoc'."
     (member obj lst equal?))
    ((obj lst compare)
     (let loop ((lst lst))
       (cond
         ((null? lst) #f)
         ((compare obj (car lst)) lst)
         (else (loop (cdr lst))))))))

(define assoc
  (case-lambda
    ((obj alist)
     "Return the first pair in ALIST whose car equals OBJ, or #f\nif not found. Uses COMPARE for equality (default `equal?').\n\nExamples:\n  (assoc 'b '((a 1) (b 2)))         => (b 2)\n  (assoc 2.0 '((1 a) (2 b)) =)      => (2 b)\n\nParameters:\n  obj : any\n  alist : list\nReturns: any\nCategory: lists\n\nSee also: `assq', `assv', `member'."
     (assoc obj alist equal?))
    ((obj alist compare)
     (let loop ((alist alist))
       (cond
         ((null? alist) #f)
         ((compare obj (caar alist)) (car alist))
         (else (loop (cdr alist))))))))

;; Trivial predicates — pure compositions of existing primitives.

(define (not x)
  "Return #t if X is #f, #f otherwise.\n\nParameters:\n  x : any\nReturns: boolean\nCategory: predicates"
  (if x #f #t))

(define (zero? z)
  "Return #t if Z is zero.\n\nParameters:\n  z : number\nReturns: boolean\nCategory: predicates"
  (= z 0))

(define (positive? x)
  "Return #t if X is positive.\n\nParameters:\n  x : number\nReturns: boolean\nCategory: predicates"
  (> x 0))

(define (negative? x)
  "Return #t if X is negative.\n\nParameters:\n  x : number\nReturns: boolean\nCategory: predicates"
  (< x 0))

(define (exact-integer? x)
  "Return #t if X is both exact and an integer.\n\nParameters:\n  x : any\nReturns: boolean\nCategory: predicates\n\nSee also: `integer?', `exact?'."
  (and (integer? x) (exact? x)))

;; list? — must detect cycles (R7RS §6.4: "Returns #t if obj is a proper list")
;; Uses tortoise-and-hare for cycle detection.
(define (list? x)
  "Return #t if X is a proper list. Detects cycles via the\ntortoise-and-hare algorithm.\n\nParameters:\n  x : any\nReturns: boolean\nCategory: predicates\n\nSee also: `pair?', `null?'."
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
  "Return #t if all arguments are booleans and are equal.\nRaises an error if any argument is not a boolean.\n\nParameters:\n  b1 : boolean\n  b2 : boolean\nReturns: boolean\nCategory: predicates\n\nSee also: `symbol=?'."
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
  "Return #t if all arguments are symbols and are equal.\nRaises an error if any argument is not a symbol.\n\nParameters:\n  s1 : symbol\n  s2 : symbol\nReturns: boolean\nCategory: predicates\n\nSee also: `boolean=?'."
  (define (check x) (if (symbol? x) x (car #f)))
  (check s1)
  (let loop ((prev s1) (args (cons s2 rest)))
    (if (null? args) #t
        (let ((curr (car args)))
          (check curr)
          (and (eq? prev curr)
               (loop curr (cdr args)))))))

(define (square x)
  "Return the square of X.\n\nParameters:\n  x : number\nReturns: number\nCategory: arithmetic"
  (* x x))

;; Sorting
;; Top-down merge sort. Non-destructive and stable.

(define (sort less? lst)
  "Return a list containing the elements of LST sorted according\nto the comparison procedure LESS?. The input list is not modified.\nUses a stable merge sort: equal elements preserve their original\norder.\n\nExamples:\n  (sort < '(3 1 4 1 5 9 2 6))  => (1 1 2 3 4 5 6 9)\n  (sort > '(1 2 3))              => (3 2 1)\n  (sort < '())                   => ()\n\nParameters:\n  less? : procedure -- a two-argument comparison predicate\n  lst : list\nReturns: list\nCategory: lists\nKeywords: sort, order, merge sort, stable, list\n\nSee also: `list-sort' (srfi 132)."
  (define (merge a b)
    (cond
      ((null? a) b)
      ((null? b) a)
      ((less? (car b) (car a))
       (cons (car b) (merge a (cdr b))))
      (else
       (cons (car a) (merge (cdr a) b)))))
  (define (msort lst n)
    (if (<= n 1)
        (if (= n 1) (list (car lst)) '())
        (let ((half (quotient n 2)))
          (merge (msort lst half)
                 (msort (list-tail lst half) (- n half))))))
  ;; Detect the common list-first habit -- (sort lst less?) -- carried over
  ;; from list-first dialects. wile is comparator-first. The swap is
  ;; unambiguous: the comparator slot holds a non-procedure while the list
  ;; slot holds a procedure (a procedure is never a valid sort input).
  (if (and (not (procedure? less?)) (procedure? lst))
      (error "sort: arguments look swapped -- wile is comparator-first: (sort less? lst), not (sort lst less?)" less? lst)
      (msort lst (length lst))))

;; dynamic-wind as a first-class PROCEDURE (R7RS §6.10).
;;
;; dynamic-wind is compiled as a special form, which is why (dynamic-wind b t a) has
;; always worked. But R7RS specifies it as a procedure, and it was not bound as a
;; value at all: (procedure? dynamic-wind) and (apply dynamic-wind …) were not merely
;; #f, they were a COMPILE ERROR — "no such binding".
;;
;; This binding is only ever reached in VALUE position. Form dispatch precedes symbol
;; resolution (a form is shadowed by a LOCAL binding, not by a top-level define), so
;; the body's own (dynamic-wind before thunk after) compiles to the wind opcodes and
;; does not call back into this procedure. Operator position keeps the fast path; value
;; position now has a procedure.
(define (dynamic-wind before thunk after)
  "Call THUNK with BEFORE run on every entry to its dynamic extent and AFTER run\non every exit, including exits and re-entries via continuations.\n\nExamples:\n  (procedure? dynamic-wind)  => #t\n\nParameters:\n  before : procedure with no arguments\n  thunk : procedure with no arguments\n  after : procedure with no arguments\nReturns: the value of THUNK\nCategory: control"
  (dynamic-wind before thunk after))
