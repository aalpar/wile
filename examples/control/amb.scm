;;; amb.scm - Nondeterministic programming with backtracking
;;;
;;; Demonstrates: call/cc, mutation, closures capturing continuations,
;;;               backtracking search, constraint programming
;;;
;;; Usage: ./dist/wile --file examples/control/amb.scm

;; -----------------------------------------------------------------------
;; Amb infrastructure
;;
;; (amb x y z) nondeterministically chooses one of x, y, z.
;; (require pred) fails (backtracks) if pred is false.
;; (amb) with no arguments triggers backtracking.
;;
;; Implemented with call/cc: each choice point captures the current
;; continuation. On failure, we jump back to the most recent choice
;; and try the next alternative.
;; -----------------------------------------------------------------------

(define fail
  (lambda () (error "amb: no more choices")))

(define-syntax amb
  (syntax-rules ()
    ((amb) (fail))
    ((amb expr) expr)
    ((amb expr rest ...)
     (let ((old-fail fail))
       (call/cc
        (lambda (cc)
          (set! fail
            (lambda ()
              (set! fail old-fail)
              (cc (amb rest ...))))
          (cc expr)))))))

(define (require pred)
  (if (not pred) (fail)))

;; Collect all solutions by exhaustive backtracking
(define-syntax amb-collect
  (syntax-rules ()
    ((amb-collect expr)
     (let ((results '()))
       (let ((saved-fail fail))
         (call/cc
          (lambda (exit)
            (set! fail
              (lambda ()
                (set! fail saved-fail)
                (exit (reverse results))))
            (let ((result expr))
              (set! results (cons result results))
              (fail)))))))))

;; -----------------------------------------------------------------------
;; Example 1: Pythagorean triples
;; -----------------------------------------------------------------------

(display "=== Amb: Nondeterministic Programming ===\n\n")

(display "--- Pythagorean triples (a² + b² = c², c ≤ 20) ---\n")
(let ((triples
       (amb-collect
        (let ((a (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
          (let ((b (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
            (require (<= a b))
            (let ((c (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)))
              (require (<= b c))
              (require (= (+ (* a a) (* b b)) (* c c)))
              (list a b c)))))))
  (for-each (lambda (t)
              (display "  ")
              (display t)
              (newline))
            triples))

;; -----------------------------------------------------------------------
;; Example 2: Map coloring
;;
;; Color a simplified map so no adjacent regions share a color.
;;
;;   A --- B
;;   |   / |
;;   |  /  |
;;   C --- D
;; -----------------------------------------------------------------------

(display "\n--- Map coloring (4 regions, 3 colors) ---\n")
(let ((solutions
       (amb-collect
        (let ((a (amb 'red 'green 'blue)))
          (let ((b (amb 'red 'green 'blue)))
            (let ((c (amb 'red 'green 'blue)))
              (let ((d (amb 'red 'green 'blue)))
                (require (not (eq? a b)))  ; A-B adjacent
                (require (not (eq? a c)))  ; A-C adjacent
                (require (not (eq? b c)))  ; B-C adjacent
                (require (not (eq? b d)))  ; B-D adjacent
                (require (not (eq? c d)))  ; C-D adjacent
                (list (list 'A a) (list 'B b)
                      (list 'C c) (list 'D d)))))))))
  (display "  Found ")
  (display (length solutions))
  (display " solutions. First:\n  ")
  (display (car solutions))
  (newline))

;; -----------------------------------------------------------------------
;; Example 3: Logic puzzle
;;
;; Baker, Cooper, Fletcher, Miller, and Smith live on floors 1-5.
;; - Baker does not live on floor 5.
;; - Cooper does not live on floor 1.
;; - Fletcher does not live on floor 1 or 5.
;; - Miller lives on a higher floor than Cooper.
;; - Fletcher does not live on an adjacent floor to Cooper or Smith.
;; -----------------------------------------------------------------------

(define (distinct? lst)
  (if (null? lst) #t
      (if (member (car lst) (cdr lst)) #f
          (distinct? (cdr lst)))))

(define (adjacent? a b) (= 1 (abs (- a b))))

(display "\n--- Logic puzzle (5 people, 5 floors) ---\n")
(let ((result
       (amb-collect
        (let ((baker    (amb 1 2 3 4 5)))
          (let ((cooper   (amb 1 2 3 4 5)))
            (let ((fletcher (amb 1 2 3 4 5)))
              (let ((miller   (amb 1 2 3 4 5)))
                (let ((smith    (amb 1 2 3 4 5)))
                  (require (distinct? (list baker cooper fletcher miller smith)))
                  (require (not (= baker 5)))
                  (require (not (= cooper 1)))
                  (require (not (= fletcher 1)))
                  (require (not (= fletcher 5)))
                  (require (> miller cooper))
                  (require (not (adjacent? fletcher cooper)))
                  (require (not (adjacent? fletcher smith)))
                  (list (list 'baker baker)     (list 'cooper cooper)
                        (list 'fletcher fletcher) (list 'miller miller)
                        (list 'smith smith))))))))))
  (for-each (lambda (s)
              (display "  ")
              (display s)
              (newline))
            result))

(display "\nBacktracking search — call/cc gives you Prolog in 30 lines.\n")
