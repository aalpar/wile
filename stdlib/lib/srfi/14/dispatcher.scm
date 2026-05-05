;; dispatcher.scm -- (char-set ...) zero-arg dispatcher and ->char-set coercer
;; Part of SRFI 14: Character-Set Library

(define (char-set . chars)
  "Construct a char-set containing exactly the given CHARS.
With no arguments returns the empty char-set.

Examples:
  (char-set)                    =>  #<char-set: empty>
  (char-set #\\a #\\b #\\c)       =>  #<char-set: a-c>
  (char-set #\\a #\\a)            =>  #<char-set: a>

Parameters:
  chars : char ... (variadic)
Returns: char-set
Category: srfi-14
Keywords: constructor, char-set, characters, build

See also: `->char-set', `list->char-set', `string->char-set'."
  (if (null? chars)
      (%empty-char-set)
      (apply %char-set chars)))

(define (->char-set x)
  "Coerce X to a char-set. Accepts a char-set (returned as-is), a string
(all characters become members), or a char (singleton set).

Examples:
  (->char-set #\\a)              =>  #<char-set: a>
  (->char-set \"abc\")             =>  #<char-set: a-c>
  (->char-set (char-set #\\x))   =>  #<char-set: x>

Parameters:
  x : char-set or string or char
Returns: char-set
Category: srfi-14
Keywords: coerce, constructor, char-set, string, char

See also: `char-set', `string->char-set', `list->char-set'."
  (cond ((char-set? x) x)
        ((string? x)   (string->char-set x))
        ((char? x)     (char-set x))
        (else (error "->char-set: not coercible to char-set" x))))
