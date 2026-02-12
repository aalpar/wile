;;; threads.scm - SRFI-18 basic threading
;;;
;;; Demonstrates: Thread creation, joining, thread-local state
;;; Wile-specific: Go-backed threads via SRFI-18
;;;
;;; Usage: ./dist/scheme --file examples/concurrency/threads.scm

;; Output mutex to prevent interleaved output
(define output-mutex (make-mutex))

(define (safe-display . args)
  (mutex-lock! output-mutex)
  (for-each display args)
  (mutex-unlock! output-mutex))

;; Simple thread creation and joining
(safe-display "Creating threads:" #\newline)

(define (worker id count)
  (lambda ()
    (let loop ((i 0))
      (when (< i count)
        (safe-display "Thread " id ": iteration " i #\newline)
        (thread-sleep! 0.1)
        (loop (+ i 1))))
    (safe-display "Thread " id " complete" #\newline)
    id))

;; Create and start threads
(define t1 (make-thread (worker 1 3)))
(define t2 (make-thread (worker 2 3)))

(thread-start! t1)
(thread-start! t2)

;; Wait for threads to complete
(safe-display "Waiting for threads..." #\newline)
(thread-join! t1)
(thread-join! t2)

(safe-display "All threads completed" #\newline #\newline)

;; Thread returning values
(safe-display "Threads returning values:" #\newline)

(define (factorial-worker n)
  (lambda ()
    (let loop ((i n) (acc 1))
      (if (<= i 1)
          acc
          (loop (- i 1) (* i acc))))))

(define t3 (thread-start! (make-thread (factorial-worker 10))))
(define t4 (thread-start! (make-thread (factorial-worker 12))))

(safe-display "Factorial(10) = " (thread-join! t3) #\newline)
(safe-display "Factorial(12) = " (thread-join! t4) #\newline #\newline)

;; Thread with specific data
(safe-display "Thread-specific data:" #\newline)

(define t5 (make-thread
             (lambda ()
               (thread-specific-set! (current-thread) 42)
               (safe-display "Thread stored data: "
                            (thread-specific (current-thread))
                            #\newline)
               "done")))

(thread-start! t5)
(thread-join! t5)
(safe-display #\newline)

;; Current thread
(safe-display "Current thread info:" #\newline)
(safe-display "Current thread: " (current-thread) #\newline #\newline)

;; Yielding execution
(safe-display "Thread yielding:" #\newline)

(define (yielding-worker id)
  (lambda ()
    (let loop ((i 0))
      (when (< i 3)
        (safe-display "Worker " id " iteration " i #\newline)
        (thread-yield!)  ;; Give other threads a chance
        (loop (+ i 1))))))

(define y1 (thread-start! (make-thread (yielding-worker 'A))))
(define y2 (thread-start! (make-thread (yielding-worker 'B))))

(thread-join! y1)
(thread-join! y2)

(safe-display "Thread examples complete!" #\newline)
