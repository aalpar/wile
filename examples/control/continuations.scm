;;; continuations.scm - First-class continuations with call/cc
;;;
;;; Demonstrates: call/cc, continuation capture, non-local control flow
;;; Wile-specific: Full call/cc support, continuations can be invoked multiple times
;;;
;;; Usage: ./dist/wile --file examples/control/continuations.scm

;; Continuations represent "the rest of the computation."
;; call/cc captures the current continuation as a first-class value.

(display "=== First-Class Continuations in Wile ===\n")
(newline)

;; Example 1: Early return
(display "Example 1: Early return from computation\n")
(define (find-first predicate lst)
  (call/cc
   (lambda (return)
     (for-each
      (lambda (x)
        (when (predicate x)
          (return x)))
      lst)
     #f)))  ; Not found

(display "  (find-first even? '(1 3 5 8 9 10)) = ")
(display (find-first even? '(1 3 5 8 9 10)))
(newline)
(newline)

;; Example 2: Continuation can be called multiple times
(display "Example 2: Continuations are first-class, can be saved and reused\n")
(let ((saved-k #f)
      (invoked? #f))
  (display "  First call: ")
  (display (+ 1 (call/cc
                 (lambda (k)
                   (set! saved-k k)
                   10))))
  (newline)
  (when (and saved-k (not invoked?))
    (set! invoked? #t)
    (display "  Invoking saved continuation with 20: ")
    (display (saved-k 20))
    (newline)))
(newline)

;; Example 3: Non-local exit from deeply nested computation
(display "Example 3: Escape from deeply nested loops\n")
(define (find-2d predicate matrix)
  (call/cc
   (lambda (escape)
     (for-each
      (lambda (row)
        (for-each
         (lambda (elem)
           (when (predicate elem)
             (escape elem)))
         row))
      matrix)
     #f)))

(display "  Finding first element > 50 in matrix:\n")
(display "  ")
(display (find-2d (lambda (x) (> x 50))
                  '((1 2 3) (40 60 70) (80 90))))
(newline)
(newline)

;; Example 4: Simple coroutines (yield)
(display "Example 4: Simple generator using continuations\n")
(define (make-generator proc)
  (let ((return #f)
        (resume #f)
        (done? #f))
    (define (yield value)
      (call/cc
       (lambda (k)
         (set! resume k)
         (return value))))
    (lambda ()
      (if done?
          (error "Generator exhausted")
          (call/cc
           (lambda (k)
             (set! return k)
             (if resume
                 (resume 'ignored)
                 (begin
                   (proc yield)
                   (set! done? #t)
                   (error "Generator finished")))))))))

(define gen
  (make-generator
   (lambda (yield)
     (yield 1)
     (yield 2)
     (yield 3))))

(display "  Generator yields:\n")
(display "    ")
(display (gen))
(display " ")
(display (gen))
(display " ")
(display (gen))
(newline)
(newline)

;; Example 5: Tree traversal with early exit
(display "Example 5: Tree search with early exit\n")
(define (tree-find value tree)
  ;; tree ::= leaf | (node left-tree right-tree)
  (call/cc
   (lambda (return)
     (define (search t)
       (cond
        ((null? t) #f)
        ((not (pair? t))  ; Leaf
         (when (equal? t value)
           (return #t)))
        (else  ; Node
         (search (car t))
         (search (cdr t)))))
     (search tree)
     #f)))

(define tree '(1 (2 (3 4)) (5 (6 7))))
(display "  Tree: ")
(display tree)
(newline)
(display "  (tree-find 6 tree) = ")
(display (tree-find 6 tree))
(newline)
(display "  (tree-find 99 tree) = ")
(display (tree-find 99 tree))
(newline)
(newline)

;; Example 6: Exception-like control flow
(display "Example 6: Exception-like handling with continuations\n")
(define (safe-divide a b)
  (call/cc
   (lambda (error-handler)
     (if (= b 0)
         (error-handler 'division-by-zero)
         (/ a b)))))

(display "  (safe-divide 10 2) = ")
(display (safe-divide 10 2))
(newline)
(display "  (safe-divide 10 0) = ")
(display (safe-divide 10 0))
(newline)
(newline)

;; Example 7: Backtracking search
(display "Example 7: Backtracking with multiple choice points\n")
(define (amb choices)
  ;; Non-deterministic choice
  (call/cc
   (lambda (k)
     (for-each k choices)
     (error "No choices left"))))

(define (solve-puzzle)
  (call/cc
   (lambda (return)
     (let ((a (amb '(1 2 3)))
           (b (amb '(4 5 6))))
       (when (= (+ a b) 7)
         (return (list a b)))
       (return 'no-solution)))))

(display "  Finding a + b = 7 where a ∈ {1,2,3}, b ∈ {4,5,6}:\n")
(display "  ")
(display (solve-puzzle))
(newline)
(newline)

;; Example 8: Continuation-passing style (CPS) transformation
(display "Example 8: Direct style vs continuation-passing style\n")
(newline)

;; Direct style
(define (factorial-direct n)
  (if (<= n 1)
      1
      (* n (factorial-direct (- n 1)))))

;; CPS style
(define (factorial-cps n k)
  (if (<= n 1)
      (k 1)
      (factorial-cps (- n 1)
                     (lambda (result)
                       (k (* n result))))))

(display "  Direct: (factorial 5) = ")
(display (factorial-direct 5))
(newline)
(display "  CPS: (factorial 5 (lambda (x) x)) = ")
(display (factorial-cps 5 (lambda (x) x)))
(newline)
(newline)

;; Example 9: Time-travel debugging (undo/redo)
(display "Example 9: Checkpoint/restore using continuations\n")
(define checkpoints '())

(define (checkpoint!)
  (call/cc
   (lambda (k)
     (set! checkpoints (cons k checkpoints))
     'checkpoint-saved)))

(define (restore!)
  (if (null? checkpoints)
      (error "No checkpoints")
      (let ((k (car checkpoints)))
        (set! checkpoints (cdr checkpoints))
        (k 'restored))))

(display "  State tracking:\n")
(let ((x 0))
  (display "    x = ")
  (display x)
  (newline)
  (checkpoint!)
  (set! x 10)
  (display "    After set x=10: x = ")
  (display x)
  (newline)
  (checkpoint!)
  (set! x 20)
  (display "    After set x=20: x = ")
  (display x)
  (newline)
  (when (not (null? checkpoints))
    (restore!)  ; This returns to checkpoint!
    (display "    After restore: x = ")
    (display x)
    (newline)))
(newline)

;; Example 10: Loop breaking
(display "Example 10: Break from loop using continuation\n")
(define (break-example)
  (call/cc
   (lambda (break)
     (let loop ((i 0))
       (when (< i 10)
         (display i)
         (display " ")
         (when (= i 5)
           (break 'broke-at-5))
         (loop (+ i 1)))))))

(display "  ")
(display (break-example))
(newline)
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Continuations enable:\n")
(display "  • Early return from deep computation\n")
(display "  • Exception-like error handling\n")
(display "  • Generators and coroutines\n")
(display "  • Backtracking search\n")
(display "  • Time-travel/undo mechanisms\n")
(display "  • Implementing any control structure\n")
(newline)
(display "call/cc is one of Scheme's most powerful features!\n")
