;;; coroutines.scm - Cooperative multitasking with channels
;;;
;;; Demonstrates: call/cc, define-record-type, mutation, closures,
;;;               goroutine-like concurrency in pure userspace Scheme
;;;
;;; Usage: ./dist/scheme --file examples/control/coroutines.scm

;; -----------------------------------------------------------------------
;; Scheduler
;;
;; A run queue of thunks. spawn adds work, yield suspends the current
;; coroutine and runs the next one. Everything built on call/cc.
;; -----------------------------------------------------------------------

(define *run-queue* '())
(define *scheduler-loop* #f)

(define (enqueue! thunk)
  (set! *run-queue* (append *run-queue* (list thunk))))

(define (dequeue!)
  (let ((next (car *run-queue*)))
    (set! *run-queue* (cdr *run-queue*))
    next))

(define (scheduler-run)
  (cond
    ((not (null? *run-queue*))
     ((dequeue!))
     (scheduler-run))
    (*scheduler-loop*
     (*scheduler-loop* 'done))))

(define (spawn thunk)
  (enqueue! (lambda () (thunk) (scheduler-run))))

(define (yield)
  (call/cc
   (lambda (k)
     (enqueue! (lambda () (k #f)))
     (scheduler-run))))

(define (scheduler-start)
  (call/cc
   (lambda (k)
     (set! *scheduler-loop* k)
     (scheduler-run))))

;; -----------------------------------------------------------------------
;; Channels — synchronous message passing (CSP-style)
;;
;; channel-put blocks if no reader is waiting.
;; channel-get blocks if no writer is waiting.
;; -----------------------------------------------------------------------

(define-record-type <channel>
  (make-channel-raw readers writers)
  channel?
  (readers channel-readers set-channel-readers!)
  (writers channel-writers set-channel-writers!))

(define (make-channel)
  (make-channel-raw '() '()))

(define (channel-put ch value)
  (let ((readers (channel-readers ch)))
    (if (null? readers)
        ;; No reader — suspend this writer
        (call/cc
         (lambda (k)
           (set-channel-writers! ch
             (append (channel-writers ch) (list (cons k value))))
           (scheduler-run)))
        ;; Reader waiting — deliver value and resume reader
        (let ((reader-k (car readers)))
          (set-channel-readers! ch (cdr readers))
          (enqueue! (lambda () (reader-k value)))))))

(define (channel-get ch)
  (let ((writers (channel-writers ch)))
    (if (null? writers)
        ;; No writer — suspend this reader
        (call/cc
         (lambda (k)
           (set-channel-readers! ch
             (append (channel-readers ch) (list k)))
           (scheduler-run)))
        ;; Writer waiting — take value and resume writer
        (let* ((pair (car writers))
               (writer-k (car pair))
               (value (cdr pair)))
          (set-channel-writers! ch (cdr writers))
          (enqueue! (lambda () (writer-k #t)))
          value))))

;; -----------------------------------------------------------------------
;; Demo 1: Simple round-robin coroutines
;; -----------------------------------------------------------------------

(display "=== Coroutines & Channels ===\n\n")

(display "--- Round-robin scheduling ---\n")
(set! *run-queue* '())

(spawn (lambda ()
         (display "  [A] step 1\n")
         (yield)
         (display "  [A] step 2\n")
         (yield)
         (display "  [A] step 3\n")))

(spawn (lambda ()
         (display "  [B] step 1\n")
         (yield)
         (display "  [B] step 2\n")))

(spawn (lambda ()
         (display "  [C] step 1\n")
         (yield)
         (display "  [C] step 2\n")
         (yield)
         (display "  [C] step 3\n")))

(scheduler-start)

;; -----------------------------------------------------------------------
;; Demo 2: Producer-consumer pipeline via channels
;; -----------------------------------------------------------------------

(display "\n--- Producer → transformer → consumer pipeline ---\n")
(set! *run-queue* '())

(let ((raw (make-channel))
      (processed (make-channel)))

  ;; Producer: generates numbers 1..5
  (spawn (lambda ()
           (let loop ((i 1))
             (if (<= i 5)
                 (begin
                   (channel-put raw i)
                   (loop (+ i 1)))))
           (channel-put raw 'done)))

  ;; Transformer: squares each number
  (spawn (lambda ()
           (let loop ()
             (let ((v (channel-get raw)))
               (if (eq? v 'done)
                   (channel-put processed 'done)
                   (begin
                     (channel-put processed (* v v))
                     (loop)))))))

  ;; Consumer: prints results
  (spawn (lambda ()
           (let loop ()
             (let ((v (channel-get processed)))
               (if (not (eq? v 'done))
                   (begin
                     (display "  received: ")
                     (display v)
                     (newline)
                     (loop)))))))

  (scheduler-start))

;; -----------------------------------------------------------------------
;; Demo 3: Fan-in (two producers, one consumer)
;; -----------------------------------------------------------------------

(display "\n--- Fan-in: two producers, one consumer ---\n")
(set! *run-queue* '())

(let ((ch (make-channel)))

  (spawn (lambda ()
           (for-each (lambda (x) (channel-put ch (list 'evens x)))
                     '(0 2 4 6 8))
           (channel-put ch 'done)))

  (spawn (lambda ()
           (for-each (lambda (x) (channel-put ch (list 'odds x)))
                     '(1 3 5 7 9))
           (channel-put ch 'done)))

  (spawn (lambda ()
           (let loop ((done-count 0))
             (if (< done-count 2)
                 (let ((v (channel-get ch)))
                   (if (eq? v 'done)
                       (loop (+ done-count 1))
                       (begin
                         (display "  ")
                         (display v)
                         (newline)
                         (loop done-count))))))))

  (scheduler-start))

(display "\nGoroutines and channels — in userspace, via continuations.\n")
