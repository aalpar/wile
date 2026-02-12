;;; mutex.scm - Mutual exclusion patterns
;;;
;;; Demonstrates: Mutexes, critical sections, race condition prevention
;;; Wile-specific: SRFI-18 mutex primitives
;;;
;;; Usage: ./dist/scheme --file examples/concurrency/mutex.scm

;; Output mutex to prevent interleaved output
(define output-mutex (make-mutex))

(define (safe-print . args)
  (mutex-lock! output-mutex)
  (for-each display args)
  (mutex-unlock! output-mutex))

;; Shared counter without synchronization (race condition)
(safe-print "Race condition example (no mutex):\n")

(define counter 0)

(define (increment-no-lock times)
  (lambda ()
    (let loop ((i 0))
      (when (< i times)
        (set! counter (+ counter 1))
        (loop (+ i 1))))
    counter))

;; Reset counter
(set! counter 0)

;; Create multiple threads incrementing shared counter
(define t1 (thread-start! (make-thread (increment-no-lock 1000))))
(define t2 (thread-start! (make-thread (increment-no-lock 1000))))
(define t3 (thread-start! (make-thread (increment-no-lock 1000))))

(thread-join! t1)
(thread-join! t2)
(thread-join! t3)

(safe-print "Expected: 3000, Got: " counter " (may differ due to race condition)\n\n")

;; Shared counter with mutex protection
(safe-print "Protected counter (with mutex):\n")

(set! counter 0)
(define counter-mutex (make-mutex))

(define (increment-with-lock times)
  (lambda ()
    (let loop ((i 0))
      (when (< i times)
        (mutex-lock! counter-mutex)
        (set! counter (+ counter 1))
        (mutex-unlock! counter-mutex)
        (loop (+ i 1))))
    counter))

;; Create threads with mutex protection
(define m1 (thread-start! (make-thread (increment-with-lock 1000))))
(define m2 (thread-start! (make-thread (increment-with-lock 1000))))
(define m3 (thread-start! (make-thread (increment-with-lock 1000))))

(thread-join! m1)
(thread-join! m2)
(thread-join! m3)

(safe-print "Expected: 3000, Got: " counter " (should match)\n\n")

;; Bank account with mutex
(safe-print "Bank account with synchronization:\n")

(define (make-safe-account initial-balance)
  (let ((balance initial-balance)
        (mutex (make-mutex)))
    (lambda (operation amount)
      (mutex-lock! mutex)
      (let ((result
              (cond
                ((eq? operation 'deposit)
                 (set! balance (+ balance amount))
                 balance)
                ((eq? operation 'withdraw)
                 (if (>= balance amount)
                     (begin
                       (set! balance (- balance amount))
                       balance)
                     (begin
                       (safe-print "Insufficient funds\n")
                       balance)))
                ((eq? operation 'balance)
                 balance)
                (else
                 (error "Unknown operation" operation)))))
        (mutex-unlock! mutex)
        result))))

(define account (make-safe-account 1000))

;; Multiple threads accessing account
(define (make-transaction account op amt count)
  (lambda ()
    (let loop ((i 0))
      (when (< i count)
        (account op amt)
        (thread-sleep! 0.01)
        (loop (+ i 1))))))

(safe-print "Initial balance: " (account 'balance 0) "\n")

(define deposit-thread
  (thread-start! (make-thread (make-transaction account 'deposit 10 10))))

(define withdraw-thread
  (thread-start! (make-thread (make-transaction account 'withdraw 5 10))))

(thread-join! deposit-thread)
(thread-join! withdraw-thread)

(safe-print "Final balance: " (account 'balance 0) " (expected: 1050)\n\n")

;; Mutex with timeout
(safe-print "Mutex with timeout:\n")

(define timeout-mutex (make-mutex))

;; Lock the mutex
(mutex-lock! timeout-mutex)

(define timeout-thread
  (thread-start!
    (make-thread
      (lambda ()
        (safe-print "Trying to lock mutex...\n")
        ;; Try to lock with timeout
        (let ((locked (mutex-lock! timeout-mutex 0.5)))
          (if locked
              (begin
                (safe-print "Acquired mutex\n")
                (mutex-unlock! timeout-mutex))
              (safe-print "Timeout: could not acquire mutex\n")))))))

;; Wait for timeout thread
(thread-sleep! 0.1)

;; Unlock so the thread can proceed
(mutex-unlock! timeout-mutex)

(thread-join! timeout-thread)

(safe-print "Mutex examples complete!\n")
