;; concat.scm -- SRFI-13 string-concatenate, reverse-list->string
;; Part of SRFI 13: String Library

(define (string-concatenate string-list)
  "Concatenate every string in STRING-LIST in left-to-right order.
Equivalent to (apply string-append STRING-LIST), packaged as a single-
argument procedure (some Schemes have argument-count limits on apply).

Examples:
  (string-concatenate '(\"foo\" \"bar\" \"baz\"))  => \"foobarbaz\"
  (string-concatenate '())                      => \"\"
  (string-concatenate '(\"\"))                    => \"\"

Parameters:
  string-list : list of strings
Returns: string
Category: srfi-13
Keywords: concatenate, join, append, glue

See also: `string-append', `string-join', `reverse-list->string'."
  (apply string-append string-list))

(define (reverse-list->string char-list)
  "Build a string from CHAR-LIST in REVERSE order. Equivalent to
(list->string (reverse char-list)) but conventionally accepted as
the canonical building primitive when constructing a result by
consing chars onto a list right-to-left.

Examples:
  (reverse-list->string '(#\\c #\\b #\\a))  => \"abc\"
  (reverse-list->string '())                  => \"\"

Parameters:
  char-list : list of chars (in reversed order)
Returns: string
Category: srfi-13
Keywords: list->string, reverse, build, construct, accumulator

See also: `list->string', `string-concatenate'."
  (list->string (reverse char-list)))
