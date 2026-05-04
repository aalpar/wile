;; stdlib/lib/srfi/14/algebra.scm
;;
;; Set-algebra layer:
;;   - Derived: char-set-adjoin, char-set-delete (built on union/difference + singleton)
;;   - 10 ! aliases: per design Q2-A, all forms allocate fresh; ! suffix is permission
;;     to mutate, which the spec allows but doesn't require. We exercise the always-
;;     allocate option uniformly.
;;
;; The remaining 2 ! aliases (char-set-filter!, char-set-unfold!) ship in
;; stdlib/lib/srfi/14/iteration.scm where their referents are defined.

;; Derived set algebra
(define (char-set-adjoin cs . chars)
  (char-set-union cs (apply char-set chars)))

(define (char-set-delete cs . chars)
  (char-set-difference cs (apply char-set chars)))

;; ! aliases (set-algebra family + constructor family). All immutable/allocating.
(define char-set-adjoin!        char-set-adjoin)
(define char-set-delete!        char-set-delete)
(define char-set-complement!    char-set-complement)
(define char-set-union!         char-set-union)
(define char-set-intersection!  char-set-intersection)
(define char-set-difference!    char-set-difference)
(define char-set-xor!           char-set-xor)
(define list->char-set!         list->char-set)
(define string->char-set!       string->char-set)
(define ucs-range->char-set!    ucs-range->char-set)
