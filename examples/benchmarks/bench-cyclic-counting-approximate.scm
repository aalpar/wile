;;; bench-cyclic-counting-approximate.scm
;;;
;;; Acceptance benchmark for Phase 5 of the approximate-counting-semirings
;;; plan (plans/2026-05-24-approximate-counting-semirings.md). Verifies
;;; that `modular-counting-semiring' and `saturating-counting-semiring'
;;; tractably handle the cyclic-counting case that hangs the exact
;;; `counting-semiring' (3-hour incident on the 539-node machine package,
;;; documented in memory/feedback-counting-semiring-on-cycles.md).
;;;
;;; Shape matches the incident: 539-node forward chain with 12 back-edges
;;; sprinkled at regular intervals. Total V=539, E≈550. Worklist safety
;;; cap = 2·V·E ≈ 593k iterations.
;;;
;;; Acceptance: modular and saturating each finish in well under 1s.

(import (scheme base)
        (scheme write)
        (srfi 1)
        (wile algebra semiring)
        (wile algebra graph))

(define NODES 539)
(define BACK-EDGES 12)

(define (build-adj)
  (let* ((step (quotient NODES (+ BACK-EDGES 1)))
         (back-points
          (let loop ((k 1) (acc '()))
            (cond
              ((> k BACK-EDGES) (reverse acc))
              (else (loop (+ k 1) (cons (* k step) acc)))))))
    (let loop ((i 0) (acc '()))
      (cond
        ((>= i NODES)
         (reverse acc))
        (else
         (let* ((name (number->string i))
                (forward (if (< (+ i 1) NODES)
                             (list (cons (number->string (+ i 1)) 1))
                             '()))
                (back (if (memv i back-points)
                          (list (cons (number->string (max 0 (- i 30))) 1))
                          '()))
                (out (append forward back)))
           (loop (+ i 1) (cons (cons name out) acc))))))))

(define adj (build-adj))
(define source "0")

(define (back-edge-count)
  (apply + (map (lambda (entry)
                  (let ((from (string->number (car entry))))
                    (length (filter (lambda (out)
                                      (<= (string->number (car out)) from))
                                    (cdr entry)))))
                adj)))

(define (edge-count)
  (apply + (map (lambda (entry) (length (cdr entry))) adj)))

(define (elapsed-seconds start end)
  (exact->inexact (/ (- end start) (jiffies-per-second))))

(define (try-run label semiring)
  (display label) (display ": ")
  (let ((start (current-jiffy)))
    (guard (exn (#t
                 (let ((end (current-jiffy)))
                   (display (elapsed-seconds start end))
                   (display "s — ERROR: ")
                   (display (if (error-object? exn)
                                (error-object-message exn)
                                exn))
                   (newline))))
           (let* ((ga (make-graph-analysis semiring adj #f))
                  (result (graph-query-all ga source))
                  (end (current-jiffy)))
             (display (elapsed-seconds start end))
             (display "s, reachable-nodes = ")
             (display (length result))
             (newline)))))

(display "=== Cyclic-counting approximate semirings benchmark ===") (newline)
(display "Nodes: ") (display NODES)
(display ", back-edges: ") (display (back-edge-count)) (newline)
(display "Source: ") (display source) (newline)
(display "Worklist safety cap: 2·V·E = ")
(display (* 2 NODES (edge-count))) (display " iterations") (newline)
(newline)
(display "Acceptance gate: modular AND saturating each finish in < 1.0s") (newline)
(newline)

(try-run "modular  (mersenne-61)         "
         (modular-counting-semiring mersenne-61))
(try-run "saturating (cap 2^53)          "
         (saturating-counting-semiring (expt 2 53)))
(try-run "log      (cycles: unbounded)   "
         (log-counting-semiring))
