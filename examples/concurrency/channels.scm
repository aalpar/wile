;;; channels.scm - Go-style channel messaging
;;;
;;; Demonstrates: Channel send/receive, synchronization
;;; Wile-specific: Go-backed channels extension
;;;
;;; Usage: ./dist/wile --file examples/concurrency/channels.scm
;;;
;;; Note: Some output may be interleaved due to concurrent execution.
;;; This is expected behavior demonstrating true concurrency.

;; Output mutex to prevent write errors
(define output-mutex (make-mutex))

(define (safe-print . args)
  (mutex-lock! output-mutex)
  (for-each display args)
  (mutex-unlock! output-mutex))

;; Simple send and receive
(safe-print "Basic channel send/receive:" #\newline)

(define ch (make-channel))

;; Producer thread
(define producer
  (thread-start!
    (make-thread
      (lambda ()
        (safe-print "Producer: sending values..." #\newline)
        (channel-send! ch 1)
        (channel-send! ch 2)
        (channel-send! ch 3)
        (channel-close! ch)
        (safe-print "Producer: done" #\newline)))))

;; Consumer thread
(define consumer
  (thread-start!
    (make-thread
      (lambda ()
        (let loop ()
          (let ((val (channel-receive ch)))
            (unless (channel-closed? ch)
              (safe-print "Consumer received: " val #\newline)
              (loop))))
        (safe-print "Consumer: done" #\newline)))))

(thread-join! producer)
(thread-join! consumer)
(safe-print #\newline)

;; Multiple producers, one consumer
(safe-print "Multiple producers, one consumer:" #\newline)

(define multi-ch (make-channel))

(define (make-producer id count)
  (lambda ()
    (let loop ((i 0))
      (when (< i count)
        (channel-send! multi-ch (cons id i))
        (thread-sleep! 0.05)
        (loop (+ i 1))))
    (safe-print "Producer " id " done" #\newline)))

;; Start multiple producers
(define p1 (thread-start! (make-thread (make-producer 'A 3))))
(define p2 (thread-start! (make-thread (make-producer 'B 3))))
(define p3 (thread-start! (make-thread (make-producer 'C 3))))

;; Consumer receives from all
(define multi-consumer
  (thread-start!
    (make-thread
      (lambda ()
        (let loop ((count 0))
          (when (< count 9)
            (let ((val (channel-receive multi-ch)))
              (safe-print "Received: ")
              (mutex-lock! output-mutex)
              (write val)
              (display #\newline)
              (mutex-unlock! output-mutex)
              (loop (+ count 1)))))
        (channel-close! multi-ch)))))

(thread-join! p1)
(thread-join! p2)
(thread-join! p3)
(thread-join! multi-consumer)
(safe-print #\newline)

;; Pipeline pattern (simplified to avoid output complexity)
(safe-print "Pipeline pattern:" #\newline)

(define (stage1 in-ch out-ch)
  (lambda ()
    (let loop ()
      (let ((val (channel-receive in-ch)))
        (unless (channel-closed? in-ch)
          (channel-send! out-ch (* val 2))
          (loop))))
    (channel-close! out-ch)))

(define (stage2 in-ch out-ch)
  (lambda ()
    (let loop ()
      (let ((val (channel-receive in-ch)))
        (unless (channel-closed? in-ch)
          (channel-send! out-ch (+ val 1))
          (loop))))
    (channel-close! out-ch)))

(define ch1 (make-channel))
(define ch2 (make-channel))
(define ch3 (make-channel))

;; Input generator
(define generator
  (thread-start!
    (make-thread
      (lambda ()
        (let loop ((i 1))
          (when (<= i 5)
            (channel-send! ch1 i)
            (loop (+ i 1))))
        (channel-close! ch1)))))

;; Pipeline stages
(define s1 (thread-start! (make-thread (stage1 ch1 ch2))))
(define s2 (thread-start! (make-thread (stage2 ch2 ch3))))

;; Output collector
(define collector
  (thread-start!
    (make-thread
      (lambda ()
        (let loop ()
          (let ((val (channel-receive ch3)))
            (unless (channel-closed? ch3)
              (safe-print "Pipeline result: " val #\newline)
              (loop))))))))

(thread-join! generator)
(thread-join! s1)
(thread-join! s2)
(thread-join! collector)

(safe-print "Channel examples complete!" #\newline)
