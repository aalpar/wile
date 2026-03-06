;;; benchmark.scm - miniKanren benchmark suite for Wile
;;;
;;; Three benchmarks covering distinct hot paths:
;;;   1. Zebra puzzle    — deep backtracking, large substitutions (walk/unify)
;;;   2. Appendo scaling — stream interleaving at scale (mplus/bind)
;;;   3. Relational arith — deep recursion, variable chains (goal application)
;;;
;;; Usage (from project root):
;;;   SCHEME_LIBRARY_PATH=lib ./dist/scheme --file examples/logic/kanren/benchmark.scm

(import (scheme base)
        (scheme write)
        (scheme time)
        (wile kanren))

;; ---------------------------------------------------------------------------
;; Timing infrastructure
;; ---------------------------------------------------------------------------

(define (time-thunk name thunk)
  (let* ((start (current-jiffy))
         (result (thunk))
         (end (current-jiffy))
         (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
    (display "  ")
    (display name)
    (display ": ")
    (display elapsed)
    (display "s")
    (newline)
    (cons elapsed result)))

;; ---------------------------------------------------------------------------
;; Relational primitives
;; ---------------------------------------------------------------------------

(define (appendo l s out)
  (conde
    ((== l '()) (== s out))
    ((fresh (a d res)
       (== l (cons a d))
       (== out (cons a res))
       (appendo d s res)))))

(define (membero x ls)
  (fresh (a d)
    (== ls (cons a d))
    (conde
      ((== a x))
      ((membero x d)))))

;; Peano naturals for relational arithmetic
(define (zeroo n) (== n '()))
(define (succo n m) (== m (cons 's n)))

(define (pluso n m out)
  (conde
    ((zeroo n) (== m out))
    ((fresh (n-1 res)
       (succo n-1 n)
       (pluso n-1 m res)
       (succo res out)))))

(define (mulo n m out)
  (conde
    ((zeroo n) (== out '()))
    ((fresh (n-1 partial)
       (succo n-1 n)
       (mulo n-1 m partial)
       (pluso partial m out)))))

;; Build a Peano number from an integer
(define (make-peano n)
  (if (zero? n) '()
      (cons 's (make-peano (- n 1)))))

;; Convert Peano to integer
(define (peano->int p)
  (if (null? p) 0 (+ 1 (peano->int (cdr p)))))

;; ---------------------------------------------------------------------------
;; Benchmark 1: Zebra puzzle (Einstein's riddle)
;; ---------------------------------------------------------------------------
;;
;; 5 houses, 5 attributes each, 15 constraints.
;; Exercises: deep backtracking, unify on nested lists, walk over large
;; substitutions.

(define (nexto a b ls)
  (fresh (d)
    (conde
      ((== ls (cons a (cons b d))))
      ((fresh (x rest)
         (== ls (cons x rest))
         (nexto a b rest))))))

(define (lefto a b ls)
  (nexto a b ls))

(define (beside a b ls)
  (conde
    ((nexto a b ls))
    ((nexto b a ls))))

(define (zebra-puzzle)
  ;; Each house is (nationality color drink smoke pet).
  ;; Every "don't care" position needs a fresh logic variable.
  (run 1 (h)
    (fresh (n1 c1 d1 s1 p1
            n2 c2 d2 s2 p2
            n3 c3 d3 s3 p3
            n4 c4 d4 s4 p4
            n5 c5 d5 s5 p5)
      (== h (list
              (list n1 c1 d1 s1 p1)
              (list n2 c2 d2 s2 p2)
              (list n3 c3 d3 s3 p3)
              (list n4 c4 d4 s4 p4)
              (list n5 c5 d5 s5 p5)))
      ;; 9. The Norwegian lives in the first house
      (== n1 'norwegian)
      ;; 8. Milk is drunk in the middle house
      (== d3 'milk)
      ;; 1. The Englishman lives in the red house
      (fresh (a b c)
        (membero (list 'english 'red a b c) h))
      ;; 2. The Spaniard owns the dog
      (fresh (a b c)
        (membero (list 'spanish a b c 'dog) h))
      ;; 3. Coffee is drunk in the green house
      (fresh (a b c)
        (membero (list a 'green 'coffee b c) h))
      ;; 4. The Ukrainian drinks tea
      (fresh (a b c)
        (membero (list 'ukrainian a 'tea b c) h))
      ;; 5. The green house is immediately to the right of the ivory house
      ;; house = (nationality color drink smoke pet)
      (fresh (a1 b1 c1x d1x a2 b2 c2x d2x)
        (lefto (list a1 'ivory b1 c1x d1x)
               (list a2 'green b2 c2x d2x) h))
      ;; 6. The Old Gold smoker owns snails
      (fresh (a b c)
        (membero (list a b c 'old-gold 'snails) h))
      ;; 7. Kools are smoked in the yellow house
      (fresh (a b c)
        (membero (list a 'yellow b 'kools c) h))
      ;; 10. Chesterfields smoker lives next to the fox owner
      (fresh (a1 b1 c1x d1x a2 b2 c2x d2x)
        (beside (list a1 b1 c1x 'chesterfields d1x)
                (list a2 b2 c2x d2x 'fox) h))
      ;; 11. Kools smoker lives next to the horse owner
      (fresh (a1 b1 c1x d1x a2 b2 c2x d2x)
        (beside (list a1 b1 c1x 'kools d1x)
                (list a2 b2 c2x d2x 'horse) h))
      ;; 12. Lucky Strike smoker drinks orange juice
      (fresh (a b c)
        (membero (list a b 'oj 'lucky-strike c) h))
      ;; 13. The Japanese smokes Parliaments
      (fresh (a b c)
        (membero (list 'japanese a b 'parliaments c) h))
      ;; 14. The Norwegian lives next to the blue house
      (fresh (a1 b1 c1x d1x a2 b2 c2x d2x)
        (beside (list 'norwegian b1 c1x d1x a1)
                (list a2 'blue b2 c2x d2x) h))
      ;; Who drinks water?
      (fresh (a b c d)
        (membero (list a b 'water c d) h))
      ;; Who owns the zebra?
      (fresh (a b c d)
        (membero (list a b c d 'zebra) h)))))

;; ---------------------------------------------------------------------------
;; Benchmark 2: Appendo scaling
;; ---------------------------------------------------------------------------
;;
;; Generate all ways to split a list of length N into two parts.
;; N=20 produces 21 results. Exercises: mplus interleaving, bind,
;; closure creation/application.

(define (make-list-n n)
  (if (zero? n) '()
      (cons n (make-list-n (- n 1)))))

(define (appendo-splits n)
  (let ((ls (make-list-n n)))
    (length (run* (q)
      (fresh (x y)
        (appendo x y ls)
        (== q (list x y)))))))

;; ---------------------------------------------------------------------------
;; Benchmark 3: Relational arithmetic
;; ---------------------------------------------------------------------------
;;
;; Compute 3 * 4 = 12 relationally using Peano naturals.
;; Then enumerate factor pairs of 6.
;; Exercises: deep recursion, variable chains, goal application.

(define (arith-benchmark)
  ;; Forward: 3 + 4 = ?
  (let ((plus-fwd (run 1 (q) (pluso (make-peano 3) (make-peano 4) q))))
    ;; Forward: 2 * 3 = ?
    (let ((mul-fwd (run 1 (q) (mulo (make-peano 2) (make-peano 3) q))))
      ;; Reverse: ? + ? = 5  (enumerate all pairs, bounded)
      (let ((plus-rev (run 6 (q)
              (fresh (a b)
                (pluso a b (make-peano 5))
                (== q (list a b))))))
        (list (length plus-fwd)
              (if (pair? mul-fwd) (peano->int (car mul-fwd)) 0)
              (length plus-rev))))))

;; ---------------------------------------------------------------------------
;; Run suite
;; ---------------------------------------------------------------------------

(display "=== miniKanren Benchmark Suite ===\n\n")

(display "--- Zebra Puzzle (constraint satisfaction) ---\n")
(let ((result (time-thunk "zebra" zebra-puzzle)))
  (if (null? (cdr result))
      (begin (display "  FAIL: no solution found\n") (exit 1))
      (display "  OK: solution found\n")))

(newline)
(display "--- Appendo Scaling (stream interleaving) ---\n")
(let ((r10 (time-thunk "appendo-10" (lambda () (appendo-splits 10))))
      (r15 (time-thunk "appendo-15" (lambda () (appendo-splits 15))))
      (r20 (time-thunk "appendo-20" (lambda () (appendo-splits 20)))))
  (display "  splits: ")
  (display (cdr r10)) (display ", ")
  (display (cdr r15)) (display ", ")
  (display (cdr r20))
  (newline))

(newline)
(display "--- Relational Arithmetic (deep recursion) ---\n")
(let ((result (time-thunk "arith" arith-benchmark)))
  (let ((r (cdr result)))
    (display "  plus-fwd results: ") (display (car r)) (newline)
    (display "  mul 2*3 = ") (display (cadr r)) (newline)
    (display "  plus-rev pairs for 5: ") (display (caddr r)) (newline)))

(newline)
(display "=== Done ===\n")
