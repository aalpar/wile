;; SRFI-26 integration tests — cut, cute.
;; Covers: positional slots, rest-slots, operator-in-slot position, and
;; the cut-vs-cute evaluation-timing distinction.

(import (scheme base) (scheme write) (chibi test) (srfi 26))

(test-begin "srfi-26")

;; ============================================================
;; cut — structural cases
;; ============================================================

(test-begin "cut")

(test "one-slot"        6   ((cut + 1 <>) 5))
(test "two-slots"       '(1 a 3 b) ((cut list 1 <> 3 <>) 'a 'b))
(test "operator-slot"   3   ((cut <> 1 2) +))
(test "no-slots"        '() ((cut list)))
(test "rest-slot"       '(1 2 3 4) ((cut list 1 <...>) 2 3 4))
(test "slot-and-rest"   '(1 a 3 4 5) ((cut list 1 <> 3 <...>) 'a 4 5))

(test-end "cut")

;; ============================================================
;; cute — same structural cases must hold
;; ============================================================

(test-begin "cute")

(test "one-slot"        6   ((cute + 1 <>) 5))
(test "two-slots"       '(1 a 3 b) ((cute list 1 <> 3 <>) 'a 'b))
(test "rest-slot"       '(1 2 3 4) ((cute list 1 <...>) 2 3 4))

(test-end "cute")

;; ============================================================
;; The discriminating test: evaluation timing
;; ============================================================
;; The whole reason both forms exist. A mutable counter is the non-slot
;; operand: cut re-evaluates it on every call (counter advances); cute
;; captures it once at construction (frozen).

(test-begin "evaluation-timing")

;; cut: (next!) re-evaluates each call -> car advances.
(let ()
  (define n 0)
  (define (next!) (set! n (+ n 1)) n)
  (define fc (cut cons (next!) <>))
  (test "cut re-evaluates: first call"  '(1 . a) (fc 'a))
  (test "cut re-evaluates: second call" '(2 . b) (fc 'b)))

;; cute: (next!) evaluated once, when ge is constructed -> car frozen.
;; Constructing ge bumps the counter exactly once (to 1), then it stays.
(let ()
  (define n 0)
  (define (next!) (set! n (+ n 1)) n)
  (define ge (cute cons (next!) <>))
  (test "cute evaluates once: first call"  '(1 . a) (ge 'a))
  (test "cute evaluates once: second call" '(1 . b) (ge 'b))
  (test "cute did not re-evaluate"         1       n))

;; cute must evaluate its non-slot operands LEFT-TO-RIGHT (textual order),
;; matching cut and the SRFI-26 reference. Log each non-slot evaluation and
;; assert the order. (A prepend bug in the binding accumulator reverses this.)
(let ()
  (define log '())
  (define (note! k) (set! log (cons k log)) k)
  (define f (cute list (note! 1) <> (note! 2) <> (note! 3)))
  (f 'a 'b)
  (test "cute evaluates non-slots left-to-right" '(1 2 3) (reverse log)))

(test-end "evaluation-timing")

(test-end "srfi-26")

(test-exit)
