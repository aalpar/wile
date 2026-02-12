;;; producers-consumers.scm - Classic concurrency pattern
;;;
;;; Demonstrates: Producer-consumer pattern, work queues
;;;
;;; Usage: ./dist/scheme --file examples/concurrency/producers-consumers.scm

;; Output mutex to prevent interleaved output
(define output-mutex (make-mutex))

(define (safe-print . args)
  (mutex-lock! output-mutex)
  (for-each display args)
  (mutex-unlock! output-mutex))

;; Work queue with multiple producers and consumers
(safe-print "Producer-Consumer pattern:" #\newline)

(define work-queue (make-channel))
(define result-queue (make-channel))

;; Worker function - processes work items
(define (worker id)
  (lambda ()
    (safe-print "Worker " id " started" #\newline)
    (let loop ()
      (let ((work (channel-receive work-queue)))
        (unless (channel-closed? work-queue)
          (safe-print "Worker " id " processing: " work #\newline)
          ;; Simulate work
          (thread-sleep! 0.1)
          ;; Send result
          (let ((result (* work work)))
            (channel-send! result-queue (cons work result))
            (loop)))))
    (safe-print "Worker " id " done" #\newline)))

;; Producer - generates work items
(define (producer count)
  (lambda ()
    (safe-print "Producer started" #\newline)
    (let loop ((i 1))
      (when (<= i count)
        (safe-print "Producing work item: " i #\newline)
        (channel-send! work-queue i)
        (thread-sleep! 0.05)
        (loop (+ i 1))))
    (safe-print "Producer done" #\newline)))

;; Result collector
(define (collector expected-count)
  (lambda ()
    (safe-print "Collector started" #\newline)
    (let loop ((count 0))
      (when (< count expected-count)
        (let ((result (channel-receive result-queue)))
          (safe-print "Result: " (car result) " -> " (cdr result) #\newline)
          (loop (+ count 1)))))
    (safe-print "Collector done" #\newline)))

;; Start the system
(define work-count 10)

;; Start workers
(define workers
  (map (lambda (id)
         (thread-start! (make-thread (worker id))))
       '(A B C)))

;; Start producer
(define prod (thread-start! (make-thread (producer work-count))))

;; Start result collector
(define coll (thread-start! (make-thread (collector work-count))))

;; Wait for producer to finish
(thread-join! prod)

;; Signal workers to stop by closing work queue
(channel-close! work-queue)

;; Wait for all workers
(for-each thread-join! workers)

;; Close result queue
(channel-close! result-queue)

;; Wait for collector
(thread-join! coll)
(safe-print #\newline)

;; Bounded buffer example
(safe-print "Bounded buffer with semaphores:" #\newline)

;; Simple semaphore implementation using mutex and counter
(define (make-semaphore initial-count)
  (let ((count initial-count)
        (mutex (make-mutex)))
    (lambda (operation)
      (cond
        ((eq? operation 'acquire)
         (mutex-lock! mutex)
         (let loop ()
           (if (> count 0)
               (begin
                 (set! count (- count 1))
                 (mutex-unlock! mutex))
               (begin
                 (mutex-unlock! mutex)
                 (thread-yield!)
                 (mutex-lock! mutex)
                 (loop)))))
        ((eq? operation 'release)
         (mutex-lock! mutex)
         (set! count (+ count 1))
         (mutex-unlock! mutex))
        (else
         (error "Unknown operation" operation))))))

(define buffer-size 3)
(define buffer (make-vector buffer-size))
(define buffer-in 0)
(define buffer-out 0)
(define buffer-mutex (make-mutex))
(define items-available (make-semaphore 0))
(define spaces-available (make-semaphore buffer-size))

(define (bounded-producer id count)
  (lambda ()
    (let loop ((i 0))
      (when (< i count)
        (spaces-available 'acquire)
        (mutex-lock! buffer-mutex)
        (let ((item (+ (* id 100) i)))
          (vector-set! buffer buffer-in item)
          (set! buffer-in (modulo (+ buffer-in 1) buffer-size))
          (safe-print "Producer " id " produced: " item #\newline))
        (mutex-unlock! buffer-mutex)
        (items-available 'release)
        (thread-sleep! 0.05)
        (loop (+ i 1))))))

(define (bounded-consumer id count)
  (lambda ()
    (let loop ((i 0))
      (when (< i count)
        (items-available 'acquire)
        (mutex-lock! buffer-mutex)
        (let ((item (vector-ref buffer buffer-out)))
          (set! buffer-out (modulo (+ buffer-out 1) buffer-size))
          (safe-print "Consumer " id " consumed: " item #\newline))
        (mutex-unlock! buffer-mutex)
        (spaces-available 'release)
        (thread-sleep! 0.08)
        (loop (+ i 1))))))

;; Start bounded buffer producers and consumers
(define bp1 (thread-start! (make-thread (bounded-producer 1 5))))
(define bp2 (thread-start! (make-thread (bounded-producer 2 5))))
(define bc1 (thread-start! (make-thread (bounded-consumer 'A 5))))
(define bc2 (thread-start! (make-thread (bounded-consumer 'B 5))))

(thread-join! bp1)
(thread-join! bp2)
(thread-join! bc1)
(thread-join! bc2)

(safe-print "Producer-Consumer examples complete!" #\newline)
