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

;; ============================================================
;; Phase 8 extras: string-replace-all, string-byte-length,
;;                 string-blank?, string-repeat.
;; ============================================================

(define (string-replace-all s from to)
  "Replace every non-overlapping occurrence of substring FROM in S
with TO. Matching is left-to-right and non-overlapping (after a match,
scanning resumes immediately after the matched range, not just past
the first matched char). Empty FROM returns S unchanged.

Distinct from SRFI-13 `string-replace' which is a splice (replace
S[i:j] with S2). This is substring find-and-replace.

Examples:
  (string-replace-all \"foo bar foo\" \"foo\" \"baz\")  => \"baz bar baz\"
  (string-replace-all \"hello\" \"x\" \"y\")             => \"hello\"
  (string-replace-all \"\" \"x\" \"y\")                  => \"\"
  (string-replace-all \"aaa\" \"\" \"X\")                => \"aaa\"   ; empty FROM
  (string-replace-all \"aaa\" \"aa\" \"b\")              => \"ba\"   ; left-to-right non-overlapping

Parameters:
  s    : string -- the input
  from : string -- the substring to find (empty -> S unchanged)
  to   : string -- the replacement
Returns: string
Category: wile-strings
Keywords: replace, gsub, substitute, find-replace, every

See also: `string-replace' (SRFI-13 splice form), `string-contains'."
  (let ((from-len (string-length from)))
    (cond ((zero? from-len) s)
          (else
           (let loop ((i 0) (parts '()))
             (let ((j (string-contains s from i)))
               (cond (j (loop (+ j from-len)
                              (cons to (cons (substring s i j) parts))))
                     (else
                      (apply string-append
                             (reverse (cons (substring s i (string-length s))
                                            parts)))))))))))

(define (string-byte-length s)
  "Return the UTF-8 byte length of S, distinct from its codepoint
length. Useful for buffer-size calculations when interfacing with
byte-oriented APIs.

(string-byte-length s) is exactly (bytevector-length (string->utf8 s)).
ASCII inputs satisfy (= (string-byte-length s) (string-length s)).

Examples:
  (string-byte-length \"abc\")           => 3   ; ASCII
  (string-byte-length \"é\")             => 2   ; 2-byte UTF-8 sequence
  (string-byte-length \"\")              => 0

Parameters:
  s : string
Returns: integer
Category: wile-strings
Keywords: bytes, utf-8, length, encoding, byte-count

See also: `string-length' (codepoint count), `string->utf8'."
  (bytevector-length (string->utf8 s)))

(define (string-blank? s)
  "Return #t if S is empty or every char is whitespace.

(string-blank? s) is exactly (string-every char-whitespace? s).

Examples:
  (string-blank? \"\")            => #t
  (string-blank? \"   \")         => #t
  (string-blank? \"hello\")      => #f
  (string-blank? \"  hi  \")     => #f

Parameters:
  s : string
Returns: boolean
Category: wile-strings
Keywords: blank, whitespace, empty, predicate, all-spaces

See also: `string-null?', `string-every', `char-whitespace?'."
  (string-every char-whitespace? s))

(define (string-repeat s n)
  "Return S concatenated with itself N times. (string-repeat s 0)
returns the empty string. N must be a non-negative integer.

Examples:
  (string-repeat \"ab\" 3)         => \"ababab\"
  (string-repeat \"x\" 0)          => \"\"
  (string-repeat \"\" 5)           => \"\"

Parameters:
  s : string
  n : integer (>= 0)
Returns: string
Category: wile-strings
Keywords: repeat, replicate, multiply, repetition

See also: `make-string', `string-append'."
  (cond ((negative? n)
         (error "string-repeat: count must be non-negative" n))
        ((zero? n) "")
        (else (apply string-append (make-list n s)))))
