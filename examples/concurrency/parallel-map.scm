;;; parallel-map.scm - Parallel computation
;;;
;;; Demonstrates: Parallel map, work distribution, thread pools
;;;
;;; Usage: ./dist/wile --file examples/concurrency/parallel-map.scm

;; Output mutex to prevent interleaved output
(define output-mutex (make-mutex))

(define (safe-print . args)
  (mutex-lock! output-mutex)
  (for-each display args)
  (mutex-unlock! output-mutex))

;; Simple parallel map using threads
(define (parallel-map f lst)
  (let ((threads
          (map (lambda (item)
                 (thread-start!
                   (make-thread
                     (lambda ()
                       (f item)))))
               lst)))
    (map thread-join! threads)))

;; Expensive computation for demonstration
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(safe-print "Parallel map - computing Fibonacci numbers:" #\newline)

(define test-values '(20 21 22 23 24))

(safe-print "Input: ")
(mutex-lock! output-mutex)
(write test-values)
(display #\newline)
(mutex-unlock! output-mutex)

(safe-print "Computing in parallel..." #\newline)

(define start (current-jiffy))
(define results (parallel-map fibonacci test-values))
(define parallel-time (/ (- (current-jiffy) start)
                          (jiffies-per-second)))

(safe-print "Results: ")
(mutex-lock! output-mutex)
(write results)
(display #\newline)
(mutex-unlock! output-mutex)
(safe-print "Parallel time: " parallel-time " seconds" #\newline #\newline)

;; Compare with sequential map
(safe-print "Sequential map for comparison:" #\newline)

(set! start (current-jiffy))
(define seq-results (map fibonacci test-values))
(define sequential-time (/ (- (current-jiffy) start)
                            (jiffies-per-second)))

(safe-print "Results: ")
(mutex-lock! output-mutex)
(write seq-results)
(display #\newline)
(mutex-unlock! output-mutex)
(safe-print "Sequential time: " sequential-time " seconds" #\newline #\newline)

;; Parallel reduce
(safe-print "Parallel reduce (sum):" #\newline)

(define (parallel-reduce f identity lst chunk-size)
  (define (chunk-list lst size)
    (if (null? lst)
        '()
        (let ((chunk (let loop ((l lst) (n size) (acc '()))
                       (if (or (null? l) (= n 0))
                           (reverse acc)
                           (loop (cdr l) (- n 1) (cons (car l) acc))))))
          (cons chunk (chunk-list (list-tail lst (length chunk)) size)))))

  (let* ((chunks (chunk-list lst chunk-size))
         (chunk-sums
           (parallel-map
             (lambda (chunk)
               (let loop ((l chunk) (acc identity))
                 (if (null? l)
                     acc
                     (loop (cdr l) (f acc (car l))))))
             chunks)))
    (let loop ((sums chunk-sums) (acc identity))
      (if (null? sums)
          acc
          (loop (cdr sums) (f acc (car sums)))))))

(define big-list (let loop ((i 0) (acc '()))
                   (if (= i 100)
                       acc
                       (loop (+ i 1) (cons i acc)))))

(safe-print "Sum of 0-99 using parallel reduce: "
            (parallel-reduce + 0 big-list 10)
            #\newline)

(safe-print "Parallel map examples complete!" #\newline)
