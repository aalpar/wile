;; predicates.scm -- SRFI-13 string predicates
;; Part of SRFI 13: String Library

(define (string-null? s)
  "Return #t if S is the empty string.

Examples:
  (string-null? \"\")        => #t
  (string-null? \"hello\")   => #f

Parameters:
  s : string
Returns: boolean
Category: srfi-13
Keywords: empty, null, blank, predicate, length"
  (zero? (string-length s)))

(define (%string-every criterion s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (cond ((= a b) #t)
          (else
           (let loop ((i a) (last-result #t))
             (cond ((= i b) last-result)
                   (else
                    (let ((r (%match-char? criterion (string-ref s i))))
                      (and r (loop (+ i 1) r))))))))))

(define string-every
  (case-lambda
    ((criterion s)
     "Return non-#f if every char in S satisfies CRITERION.

CRITERION is a char (compared via char=?), a char-set (SRFI-14), or a
predicate procedure of one argument. For empty range returns #t. For
non-empty range and procedure CRITERION, returns the value returned for
the final char (any truthy value, not necessarily #t). Returns #f as
soon as a char fails.

Examples:
  (string-every char-alphabetic? \"abc\")    => #t
  (string-every char-alphabetic? \"ab1\")    => #f
  (string-every #\\a \"aaa\")                 => #t
  (string-every char-alphabetic? \"\")       => #t

Parameters:
  criterion : char, char-set (SRFI-14), or procedure
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: any
Category: srfi-13
Keywords: every, all, forall, predicate, satisfy

See also: `string-any', `string-count'."
     (%string-every criterion s 0 (string-length s)))
    ((criterion s start)
     (%string-every criterion s start (string-length s)))
    ((criterion s start end)
     (%string-every criterion s start end))))

(define (%string-any criterion s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((= i b) #f)
            (else
             (let ((r (%match-char? criterion (string-ref s i))))
               (or r (loop (+ i 1)))))))))

(define string-any
  (case-lambda
    ((criterion s)
     "Return the first non-#f value produced by applying CRITERION to
chars of S in left-to-right order, or #f if no char satisfies it.

CRITERION is a char (compared via char=?), a char-set (SRFI-14), or a
predicate procedure of one argument. For a procedure criterion, returns
the actual procedure return value (not necessarily #t).

Examples:
  (string-any char-numeric? \"abc1\")   => #t
  (string-any char-numeric? \"abcd\")   => #f
  (string-any #\\b \"abc\")              => #t

Parameters:
  criterion : char, char-set (SRFI-14), or procedure
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: any
Category: srfi-13
Keywords: any, some, exists, predicate, satisfy

See also: `string-every', `string-index'."
     (%string-any criterion s 0 (string-length s)))
    ((criterion s start)
     (%string-any criterion s start (string-length s)))
    ((criterion s start end)
     (%string-any criterion s start end))))
