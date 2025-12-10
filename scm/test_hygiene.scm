; Test hygiene with swap! macro

; Define the swap! macro
(define-syntax swap!
  (syntax-rules ()
    ((swap! x y)
     (let ((tmp x))
       (set! x y)
       (set! y tmp)))))

; Test case 1: Basic swap without conflict
(define a 1)
(define b 2)
(swap! a b)
(display "After swap: a = ")
(display a)
(display ", b = ")
(display b)
(newline)

; Test case 2: User has their own 'tmp' variable
; This should work correctly with hygiene
(define tmp 999)
(define c 3)
(define d 4)
(swap! c d)
(display "After swap: c = ")
(display c)
(display ", d = ")
(display d)
(display ", user's tmp = ")
(display tmp)
(display " (should still be 999)")
(newline)