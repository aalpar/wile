;; stdlib/lib/srfi/14/iteration.scm
;;
;; Iteration / query layer built on %char-set-walk-ranges. Per design §6.C:
;; this layer plus the helper avoids ~140k FFI round-trips for
;; named-set walks (Phase 4).

(define (char-set-fold proc init cs)
  (%char-set-walk-ranges cs proc init))

(define (char-set-for-each proc cs)
  (%char-set-walk-ranges cs (lambda (ch _) (proc ch) #f) #f)
  (if #f #f))

(define (char-set-map proc cs)
  ;; proc :: char → char. Result is a char-set of the mapped chars.
  (list->char-set
    (%char-set-walk-ranges cs (lambda (ch acc) (cons (proc ch) acc)) '())))

(define char-set-filter
  (case-lambda
    ((pred cs) (char-set-filter pred cs (char-set)))
    ((pred cs base)
     (list->char-set
       (%char-set-walk-ranges cs
         (lambda (ch acc) (if (pred ch) (cons ch acc) acc))
         '())
       base))))

(define char-set-filter! char-set-filter)
