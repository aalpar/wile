;;; generators.scm - Python-style generators via continuations
;;;
;;; Demonstrates: call/cc, closures capturing execution state,
;;;               eof-object sentinel, case-lambda, lazy iteration
;;;
;;; Usage: ./dist/wile --file examples/control/generators.scm

;; -----------------------------------------------------------------------
;; Generator protocol
;;
;; (make-generator proc) returns a thunk. Each call produces the next
;; value. proc receives a yield function; calling (yield v) suspends
;; the generator and returns v to the caller. When proc finishes,
;; subsequent calls return (eof-object).
;;
;; The key insight: call/cc captures the generator's entire execution
;; state. Yield swaps between producer and consumer continuations.
;; -----------------------------------------------------------------------

(define (make-generator proc)
  (let ((resume #f)
        (caller-k #f)
        (done? #f))
    (define (yield value)
      (call/cc
       (lambda (k)
         (set! resume k)
         (caller-k value))))
    (lambda ()
      (if done?
          (eof-object)
          (call/cc
           (lambda (k)
             (set! caller-k k)
             (if resume
                 (resume #f)
                 (begin
                   (proc yield)
                   (set! done? #t)
                   (caller-k (eof-object))))))))))

;; -----------------------------------------------------------------------
;; Generator combinators
;; -----------------------------------------------------------------------

;; Consume a generator into a list
(define (generator->list gen)
  (let loop ((acc '()))
    (let ((v (gen)))
      (if (eof-object? v)
          (reverse acc)
          (loop (cons v acc))))))

;; Take at most n values
(define (generator-take n gen)
  (make-generator
   (lambda (yield)
     (let loop ((i 0))
       (if (< i n)
           (let ((v (gen)))
             (if (not (eof-object? v))
                 (begin (yield v) (loop (+ i 1))))))))))

;; Filter values
(define (generator-filter pred gen)
  (make-generator
   (lambda (yield)
     (let loop ()
       (let ((v (gen)))
         (if (not (eof-object? v))
             (begin
               (if (pred v) (yield v))
               (loop))))))))

;; Transform values
(define (generator-map f gen)
  (make-generator
   (lambda (yield)
     (let loop ()
       (let ((v (gen)))
         (if (not (eof-object? v))
             (begin (yield (f v)) (loop))))))))

;; Fold over a generator
(define (generator-fold f init gen)
  (let loop ((acc init))
    (let ((v (gen)))
      (if (eof-object? v) acc
          (loop (f acc v))))))

;; -----------------------------------------------------------------------
;; Built-in generators
;; -----------------------------------------------------------------------

;; Numeric range (start inclusive, end exclusive)
(define (in-range start end)
  (make-generator
   (lambda (yield)
     (let loop ((i start))
       (if (< i end)
           (begin (yield i) (loop (+ i 1))))))))

;; Generate from a list
(define (in-list lst)
  (make-generator
   (lambda (yield)
     (for-each yield lst))))

;; Infinite Fibonacci sequence
(define (gen-fibs)
  (make-generator
   (lambda (yield)
     (let loop ((a 0) (b 1))
       (yield a)
       (loop b (+ a b))))))

;; Infinite sequence of natural numbers
(define (gen-naturals)
  (make-generator
   (lambda (yield)
     (let loop ((n 0))
       (yield n)
       (loop (+ n 1))))))

;; -----------------------------------------------------------------------
;; Demo
;; -----------------------------------------------------------------------

(display "=== Generators via Continuations ===\n\n")

(display "--- Basic generator ---\n")
(let ((g (in-range 0 5)))
  (display "  range(0,5): ")
  (display (generator->list g))
  (newline))

(display "\n--- Fibonacci (first 15) ---\n")
(display "  ")
(display (generator->list (generator-take 15 (gen-fibs))))
(newline)

(display "\n--- Composable pipeline ---\n")
(display "  naturals -> filter odd -> map square -> take 8:\n  ")
(display
 (generator->list
  (generator-take 8
   (generator-map (lambda (x) (* x x))
    (generator-filter odd?
     (gen-naturals))))))
(newline)

(display "\n--- Generator from list ---\n")
(let ((g (in-list '(hello world from a generator))))
  (display "  ")
  (display (generator->list g))
  (newline))

(display "\n--- Fold: sum of 1..100 ---\n")
(display "  ")
(display (generator-fold + 0 (in-range 1 101)))
(newline)

(display "\n--- Stateful generator (counter) ---\n")
(let ((counter (make-generator
                (lambda (yield)
                  (let loop ((n 1))
                    (yield (string-append "item-" (number->string n)))
                    (loop (+ n 1)))))))
  (display "  ")
  (display (generator->list (generator-take 5 counter)))
  (newline))

(display "\nLazy iteration over arbitrary computation — call/cc as yield.\n")
