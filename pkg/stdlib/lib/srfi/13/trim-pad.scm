;; trim-pad.scm -- SRFI-13 trim and pad
;; Part of SRFI 13: String Library

(define (%string-trim-impl s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let ((first-non-match (%string-skip s criterion a b)))
      (cond ((not first-non-match) "")
            (else (substring s first-non-match b))))))

(define (%string-trim-right-impl s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let ((last-non-match (%string-skip-right s criterion a b)))
      (cond ((not last-non-match) "")
            (else (substring s a (+ last-non-match 1)))))))

(define (%string-trim-both-impl s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let ((first-non-match (%string-skip s criterion a b)))
      (cond ((not first-non-match) "")
            (else
             (let ((last-non-match (%string-skip-right s criterion first-non-match b)))
               (substring s first-non-match (+ last-non-match 1))))))))

(define string-trim
  (case-lambda
    ((s)
     "Return S with leading chars matching CRITERION stripped.

CRITERION is a char (compared via char=?), a char-set (SRFI-14), or a
predicate procedure of one argument. Default is `char-whitespace?'.

Examples:
  (string-trim \"   hello\")              => \"hello\"
  (string-trim \"   hello   \")           => \"hello   \"
  (string-trim \"aaaaabc\" #\\a)           => \"bc\"
  (string-trim \"   \")                   => \"\"

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure (optional, default char-whitespace?)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: trim, strip, leading, whitespace, leftmost

See also: `string-trim-right', `string-trim-both', `string-trim-left'."
     (%string-trim-impl s char-whitespace? 0 (string-length s)))
    ((s criterion)
     (%string-trim-impl s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-trim-impl s criterion start (string-length s)))
    ((s criterion start end)
     (%string-trim-impl s criterion start end))))

(define string-trim-right
  (case-lambda
    ((s)
     "Return S with trailing chars matching CRITERION stripped.

Default CRITERION is `char-whitespace?'.

Examples:
  (string-trim-right \"hello   \")        => \"hello\"
  (string-trim-right \"abcaaa\" #\\a)      => \"abc\"

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure (optional, default char-whitespace?)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: trim, strip, trailing, whitespace, rightmost

See also: `string-trim', `string-trim-both'."
     (%string-trim-right-impl s char-whitespace? 0 (string-length s)))
    ((s criterion)
     (%string-trim-right-impl s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-trim-right-impl s criterion start (string-length s)))
    ((s criterion start end)
     (%string-trim-right-impl s criterion start end))))

(define string-trim-both
  (case-lambda
    ((s)
     "Return S with leading and trailing chars matching CRITERION
stripped.

Default CRITERION is `char-whitespace?'.

Examples:
  (string-trim-both \"  hello  \")       => \"hello\"
  (string-trim-both \"aaabcaaa\" #\\a)    => \"bc\"
  (string-trim-both \"   \")             => \"\"

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure (optional, default char-whitespace?)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: trim, strip, both, surround, whitespace

See also: `string-trim', `string-trim-right'."
     (%string-trim-both-impl s char-whitespace? 0 (string-length s)))
    ((s criterion)
     (%string-trim-both-impl s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-trim-both-impl s criterion start (string-length s)))
    ((s criterion start end)
     (%string-trim-both-impl s criterion start end))))

;; Alias for ergonomic symmetry with string-trim-right / string-trim-both.
;; SRFI-13 uses the unqualified `string-trim' name; the -left suffix is
;; provided for ergonomics. Phase-4 integration test verifies that
;; string-trim-left and string-trim refer to the same binding (eq? #t).
(define string-trim-left string-trim)

(define (%string-pad-impl s len char start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let ((src-len (- b a)))
      (cond ((= src-len len) (substring s a b))
            ((> src-len len) (substring s (- b len) b))
            (else
             (string-append (make-string (- len src-len) char)
                            (substring s a b)))))))

(define (%string-pad-right-impl s len char start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let ((src-len (- b a)))
      (cond ((= src-len len) (substring s a b))
            ((> src-len len) (substring s a (+ a len)))
            (else
             (string-append (substring s a b)
                            (make-string (- len src-len) char)))))))

(define string-pad
  (case-lambda
    ((s len)
     "Right-align S in a field of length LEN, padding on the left
with CHAR. If S is longer than LEN, the rightmost LEN chars are
kept (truncate from the left). Default CHAR is #\\space.

Examples:
  (string-pad \"42\" 5)              => \"   42\"
  (string-pad \"42\" 5 #\\0)          => \"00042\"
  (string-pad \"hello\" 3)           => \"llo\"   ; truncates from left
  (string-pad \"abc\" 3)             => \"abc\"

Parameters:
  s : string
  len : integer (>= 0)
  char : char (optional, default #\\space)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: pad, right-align, fill, fixed-width, leading

See also: `string-pad-right', `make-string'."
     (%string-pad-impl s len #\space 0 (string-length s)))
    ((s len char)
     (%string-pad-impl s len char 0 (string-length s)))
    ((s len char start)
     (%string-pad-impl s len char start (string-length s)))
    ((s len char start end)
     (%string-pad-impl s len char start end))))

(define string-pad-right
  (case-lambda
    ((s len)
     "Left-align S in a field of length LEN, padding on the right
with CHAR. If S is longer than LEN, the leftmost LEN chars are
kept (truncate from the right). Default CHAR is #\\space.

Examples:
  (string-pad-right \"42\" 5)        => \"42   \"
  (string-pad-right \"42\" 5 #\\.)    => \"42...\"
  (string-pad-right \"hello\" 3)     => \"hel\"   ; truncates from right
  (string-pad-right \"abc\" 3)       => \"abc\"

Parameters:
  s : string
  len : integer (>= 0)
  char : char (optional, default #\\space)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: pad, left-align, fill, fixed-width, trailing

See also: `string-pad', `make-string'."
     (%string-pad-right-impl s len #\space 0 (string-length s)))
    ((s len char)
     (%string-pad-right-impl s len char 0 (string-length s)))
    ((s len char start)
     (%string-pad-right-impl s len char start (string-length s)))
    ((s len char start end)
     (%string-pad-right-impl s len char start end))))
