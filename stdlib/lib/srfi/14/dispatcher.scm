;; stdlib/lib/srfi/14/dispatcher.scm
;; Phase 1: variadic char-set constructor (zero-arg dispatcher around
;; %char-set + %empty-char-set FFI primitives). Phase 1.7 will add ->char-set.

(define (char-set . chars)
  (if (null? chars)
      (%empty-char-set)
      (apply %char-set chars)))
