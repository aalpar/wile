;; algebra.scm -- derived set algebra and ! aliases
;; Part of SRFI 14: Character-Set Library
;;
;; char-set-adjoin and char-set-delete are derived from union/difference + singleton.
;; All ! aliases allocate fresh; the spec permits mutation but does not require it,
;; so we exercise the always-allocate option uniformly.
;; The remaining ! aliases (char-set-filter!, char-set-unfold!) live in iteration.scm
;; where their referents are defined.

;; N-ary set algebra with zero-arg identities (SRFI-14 §"Set algebra").
;;
;; SRFI-14 specifies that union/intersection/xor accept any number of
;; char-sets, including zero, returning the identity element of the
;; operation when given no arguments:
;;   (char-set-union)        => the empty set       (identity for ∪)
;;   (char-set-intersection) => the full set        (identity for ∩)
;;   (char-set-xor)          => the empty set        (identity for △)
;; The %char-set-* FFI primitives fold one-or-more char-sets but cannot be
;; invoked with zero arguments (Wile's variadic convention requires at least
;; one fixed argument). These wrappers seed the identity on zero args and
;; delegate to the FFI fold otherwise.
;;
;; char-set-difference is NOT included: SRFI-14 requires at least one
;; argument (CS1), so it has no zero-arg identity. Its FFI primitive keeps
;; its name and arity unchanged.
;;
;; The full-set identity is built as the complement of the empty set so the
;; wrapper is independent of named-sets.scm load order.

(define (char-set-union . charsets)
  "Return the union of all CHARSETS. With no arguments returns the empty
char-set (the identity for union).

Examples:
  (char-set-union)                                  =>  #<char-set: empty>
  (char-set-union (char-set #\\a) (char-set #\\b))    =>  #<char-set: a b>

Parameters:
  charsets : char-set ... (variadic, zero or more)
Returns: char-set
Category: srfi-14
Keywords: union, set-algebra, char-set, identity

See also: `char-set-intersection', `char-set-xor', `char-set-difference'."
  (if (null? charsets)
      (char-set)
      (apply %char-set-union charsets)))

(define (char-set-intersection . charsets)
  "Return the intersection of all CHARSETS. With no arguments returns the
full char-set (the identity for intersection).

Examples:
  (char-set-intersection)                                   =>  #<char-set: full>
  (char-set-intersection (char-set #\\a #\\b) (char-set #\\b))  =>  #<char-set: b>

Parameters:
  charsets : char-set ... (variadic, zero or more)
Returns: char-set
Category: srfi-14
Keywords: intersection, set-algebra, char-set, identity

See also: `char-set-union', `char-set-xor', `char-set-difference'."
  (if (null? charsets)
      (char-set-complement (char-set))
      (apply %char-set-intersection charsets)))

(define (char-set-xor . charsets)
  "Return the symmetric difference of all CHARSETS. With no arguments returns
the empty char-set (the identity for symmetric difference).

Examples:
  (char-set-xor)                                          =>  #<char-set: empty>
  (char-set-xor (char-set #\\a #\\b) (char-set #\\b #\\c))   =>  #<char-set: a c>

Parameters:
  charsets : char-set ... (variadic, zero or more)
Returns: char-set
Category: srfi-14
Keywords: xor, symmetric-difference, set-algebra, char-set, identity

See also: `char-set-union', `char-set-intersection', `char-set-difference'."
  (if (null? charsets)
      (char-set)
      (apply %char-set-xor charsets)))

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
