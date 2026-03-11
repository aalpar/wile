;;; browse.scm - Tree browsing benchmark
;;;
;;; Creates and traverses a tree structure repeatedly.
;;; Tests allocation, GC, and tree traversal patterns.
;;;
;;; Usage: ./dist/wile --file examples/benchmarks/browse.scm

(define (browse-random seed)
  (let ((seed (modulo (+ (* seed 3581) 12751) 131072)))
    seed))

(define (create-tree depth seed)
  (if (<= depth 0)
      seed
      (let ((seed1 (browse-random seed)))
        (let ((seed2 (browse-random seed1)))
          (cons (create-tree (- depth 1) seed1)
                (create-tree (- depth 1) seed2))))))

(define (browse-tree tree)
  (if (pair? tree)
      (+ (browse-tree (car tree))
         (browse-tree (cdr tree)))
      tree))

(define (run-benchmark iterations depth)
  (let ((tree (create-tree depth 42)))
    (let ((start (current-jiffy)))
      (let loop ((i 0) (result 0))
        (if (< i iterations)
            (loop (+ i 1) (browse-tree tree))
            (let* ((end (current-jiffy))
                   (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
              (display "Benchmark: browse tree of depth ") (display depth) (newline)
              (display "Iterations: ") (display iterations) (newline)
              (display "Total time: ") (display elapsed) (display "s\n")
              (display "Per iteration: ")
              (display (exact->inexact (/ elapsed iterations)))
              (display "s\n")
              elapsed))))))

;; Warmup
(browse-tree (create-tree 10 42))

;; Benchmark
(display "=== Tree Browsing Benchmark ===\n\n")
(run-benchmark 1000 10)