;; stdlib/lib/srfi/14/dispatcher.scm
;; Phase 1: variadic char-set constructor (zero-arg dispatcher around
;; %char-set + %empty-char-set FFI primitives) and ->char-set coercing
;; constructor (Task 1.7).

(define (char-set . chars)
  (if (null? chars)
      (%empty-char-set)
      (apply %char-set chars)))

;; ->char-set: dispatching constructor that coerces strings, chars, and
;; existing char-sets to char-set. Per SRFI-14 spec.

(define (->char-set x)
  (cond ((char-set? x) x)
        ((string? x)   (string->char-set x))
        ((char? x)     (char-set x))
        (else (error "->char-set: not coercible to char-set" x))))
