;; algebra.scm -- derived set algebra and ! aliases
;; Part of SRFI 14: Character-Set Library
;;
;; char-set-adjoin and char-set-delete are derived from union/difference + singleton.
;; All ! aliases allocate fresh; the spec permits mutation but does not require it,
;; so we exercise the always-allocate option uniformly.
;; The remaining ! aliases (char-set-filter!, char-set-unfold!) live in iteration.scm
;; where their referents are defined.

;; Derived set algebra
(define (char-set-adjoin cs . chars)
  "Return a new char-set containing all members of CS plus any additional CHARS.

Examples:
  (char-set-adjoin (char-set #\\a) #\\b #\\c)  =>  #<char-set: a-c>
  (char-set-adjoin (char-set) #\\x)           =>  #<char-set: x>

Parameters:
  cs : char-set
  chars : char ... (variadic)
Returns: char-set
Category: srfi-14
Keywords: adjoin, add, insert, union, char-set

See also: `char-set-delete', `char-set-union'."
  (char-set-union cs (apply char-set chars)))

(define (char-set-delete cs . chars)
  "Return a new char-set containing all members of CS except any of CHARS.

Examples:
  (char-set-delete (char-set #\\a #\\b #\\c) #\\b)  =>  #<char-set: a c>
  (char-set-delete (char-set #\\x) #\\x)           =>  #<char-set: empty>

Parameters:
  cs : char-set
  chars : char ... (variadic)
Returns: char-set
Category: srfi-14
Keywords: delete, remove, exclude, difference, char-set

See also: `char-set-adjoin', `char-set-difference'."
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
