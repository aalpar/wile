;;; puzzle.scm - Combinatorial puzzle benchmark
;;;
;;; Classic puzzle-solving benchmark from the Gabriel suite.
;;; Tests list manipulation and backtracking algorithms.
;;;
;;; Usage: ./dist/scheme --file examples/benchmarks/puzzle.scm

(define (puzzle-iota n)
  (let loop ((i 0) (result '()))
    (if (>= i n)
        (reverse result)
        (loop (+ i 1) (cons i result)))))

(define (puzzle-remove x lst)
  (cond ((null? lst) '())
        ((equal? x (car lst)) (cdr lst))
        (else (cons (car lst) (puzzle-remove x (cdr lst))))))

(define (puzzle-choose lst)
  (if (null? lst)
      '(())
      (apply append
             (map (lambda (x)
                    (map (lambda (y) (cons x y))
                         (puzzle-choose (puzzle-remove x lst))))
                  lst))))

(define (puzzle-fit x y)
  (cond ((null? x) #t)
        ((null? y) #f)
        ((= (car x) (car y)) (puzzle-fit (cdr x) (cdr y)))
        (else #f)))

(define (puzzle-find x lst)
  (cond ((null? lst) #f)
        ((puzzle-fit x (car lst)) (car lst))
        (else (puzzle-find x (cdr lst)))))

(define (puzzle-pieces)
  '((1 2) (2 3) (3 4)))

(define (puzzle-solve pieces)
  (let* ((perms (puzzle-choose pieces))
         (result (puzzle-find '(1 2 3 4) perms)))
    result))

(define (run-benchmark iterations)
  (let ((pieces (puzzle-pieces)))
    (let ((start (current-jiffy)))
      (let loop ((i 0))
        (when (< i iterations)
          (puzzle-solve pieces)
          (loop (+ i 1))))
      (let* ((end (current-jiffy))
             (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
        (display "Benchmark: puzzle-solve\n")
        (display "Iterations: ") (display iterations) (newline)
        (display "Total time: ") (display elapsed) (display "s\n")
        (display "Per iteration: ")
        (display (exact->inexact (/ elapsed iterations)))
        (display "s\n")
        elapsed))))

;; Warmup
(puzzle-solve (puzzle-pieces))

;; Benchmark
(display "=== Combinatorial Puzzle Benchmark ===\n\n")
(run-benchmark 1000)