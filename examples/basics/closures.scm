;;; closures.scm - Lexical scoping and closures
;;;
;;; Demonstrates: Closures, captured variables, private state
;;;
;;; Usage: ./dist/wile --file examples/basics/closures.scm

;; Simple closure - function that captures a variable
(define (make-multiplier factor)
  (lambda (x)
    (* x factor)))

(define times3 (make-multiplier 3))
(define times5 (make-multiplier 5))

(display "3 * 7 = ")
(display (times3 7))
(newline)

(display "5 * 7 = ")
(display (times5 7))
(newline)

;; Counter with private state
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

(define counter1 (make-counter))
(define counter2 (make-counter))

(display "Counter1: ")
(display (counter1))
(display ", ")
(display (counter1))
(display ", ")
(display (counter1))
(newline)

(display "Counter2: ")
(display (counter2))
(display ", ")
(display (counter2))
(newline)

;; Bank account with private balance
(define (make-account initial-balance)
  (let ((balance initial-balance))
    (lambda (message . args)
      (cond
        ((eq? message 'deposit)
         (set! balance (+ balance (car args)))
         balance)
        ((eq? message 'withdraw)
         (if (>= balance (car args))
             (begin
               (set! balance (- balance (car args)))
               balance)
             (error "Insufficient funds")))
        ((eq? message 'balance)
         balance)
        (else
         (error "Unknown message" message))))))

(define account (make-account 100))

(display "Initial balance: ")
(display (account 'balance))
(newline)

(display "After deposit 50: ")
(display (account 'deposit 50))
(newline)

(display "After withdraw 30: ")
(display (account 'withdraw 30))
(newline)

;; Closure over multiple values
(define (make-accumulator)
  (let ((sum 0)
        (count 0))
    (lambda (value)
      (set! sum (+ sum value))
      (set! count (+ count 1))
      (cons sum count))))

(define acc (make-accumulator))

(display "Accumulator adds 10: ")
(let ((result (acc 10)))
  (display "(sum: ")
  (display (car result))
  (display ", count: ")
  (display (cdr result))
  (display ")"))
(newline)

(display "Accumulator adds 20: ")
(let ((result (acc 20)))
  (display "(sum: ")
  (display (car result))
  (display ", count: ")
  (display (cdr result))
  (display ")"))
(newline)

;; Closures for data abstraction
(define (make-point x y)
  (lambda (message)
    (cond
      ((eq? message 'x) x)
      ((eq? message 'y) y)
      ((eq? message 'distance-from-origin)
       (sqrt (+ (* x x) (* y y))))
      (else (error "Unknown message" message)))))

(define p (make-point 3 4))

(display "Point coordinates: (")
(display (p 'x))
(display ", ")
(display (p 'y))
(display ")")
(newline)

(display "Distance from origin: ")
(display (p 'distance-from-origin))
(newline)

;; Closure returning multiple functions
(define (make-stack)
  (let ((items '()))
    (lambda (operation)
      (cond
        ((eq? operation 'push)
         (lambda (item)
           (set! items (cons item items))))
        ((eq? operation 'pop)
         (lambda ()
           (if (null? items)
               (error "Stack underflow")
               (let ((item (car items)))
                 (set! items (cdr items))
                 item))))
        ((eq? operation 'peek)
         (lambda ()
           (if (null? items)
               (error "Stack empty")
               (car items))))
        ((eq? operation 'empty?)
         (lambda ()
           (null? items)))
        (else (error "Unknown operation" operation))))))

(define stack (make-stack))

((stack 'push) 10)
((stack 'push) 20)
((stack 'push) 30)

(display "Stack peek: ")
(display ((stack 'peek)))
(newline)

(display "Stack pop: ")
(display ((stack 'pop)))
(newline)

(display "Stack peek after pop: ")
(display ((stack 'peek)))
(newline)
