;;; recursion.scm - Recursive function examples
;;;
;;; Demonstrates: Recursion, base cases, tail recursion
;;;
;;; Usage: ./dist/wile --file examples/basics/recursion.scm

;; Classic recursive factorial
;; Not tail-recursive - builds up stack frames
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Tail-recursive factorial using accumulator
;; Can be optimized to iterative loop
(define (factorial-tail n)
  (define (fact-iter n acc)
    (if (<= n 1)
        acc
        (fact-iter (- n 1) (* n acc))))
  (fact-iter n 1))

;; Fibonacci - naive recursive version (exponential time)
(define (fibonacci n)
  (cond
    ((<= n 0) 0)
    ((= n 1) 1)
    (else (+ (fibonacci (- n 1))
             (fibonacci (- n 2))))))

;; Fibonacci - tail recursive with two accumulators
(define (fibonacci-tail n)
  (define (fib-iter n a b)
    (if (= n 0)
        a
        (fib-iter (- n 1) b (+ a b))))
  (fib-iter n 0 1))

;; Greatest common divisor using Euclid's algorithm
;; Naturally tail-recursive
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (modulo a b))))

;; Demonstrate the functions
(display "Factorial of 6: ")
(display (factorial 6))
(newline)

(display "Factorial of 6 (tail-recursive): ")
(display (factorial-tail 6))
(newline)

(display "Fibonacci(10): ")
(display (fibonacci 10))
(newline)

(display "Fibonacci(10) tail-recursive: ")
(display (fibonacci-tail 10))
(newline)

(display "GCD of 48 and 18: ")
(display (gcd 48 18))
(newline)

;; Compare performance on larger values
(display "Fibonacci(20) tail: ")
(display (fibonacci-tail 20))
(newline)

(display "Factorial(100) tail: ")
(display (factorial-tail 100))
(newline)
