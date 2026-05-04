;; extras.scm -- Wile-specific string extras (not in SRFI-13)
;; Part of (wile strings)

(define (string-split s delim)
  "Split S at every occurrence of single-character delimiter DELIM.
Returns a list of substrings. Adjacent delimiters produce empty
substrings; a leading or trailing delimiter produces an empty
substring at that end.

Distinct from SRFI-13 `string-tokenize', which uses a criterion
applied to every char (and would skip empty fields).

Examples:
  (string-split \"a,b,c\" #\\,)       => (\"a\" \"b\" \"c\")
  (string-split \"hello\" #\\,)        => (\"hello\")
  (string-split \"\" #\\,)             => (\"\")
  (string-split \"a,,b\" #\\,)         => (\"a\" \"\" \"b\")
  (string-split \",a,\" #\\,)          => (\"\" \"a\" \"\")

Parameters:
  s : string
  delim : char
Returns: list of strings
Category: wile-strings
Keywords: split, tokenize, separator, delimiter, fields

See also: `string-tokenize', `string-join'."
  (let ((len (string-length s)))
    (let loop ((i 0) (start 0) (acc '()))
      (cond ((>= i len)
             (reverse (cons (substring s start len) acc)))
            ((char=? (string-ref s i) delim)
             (loop (+ i 1) (+ i 1) (cons (substring s start i) acc)))
            (else
             (loop (+ i 1) start acc))))))
