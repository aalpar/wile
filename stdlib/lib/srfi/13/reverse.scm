;; reverse.scm -- SRFI-13 string-reverse and string-reverse!
;; Part of SRFI 13: String Library

(define (%string-reverse-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (list->string (reverse (string->list s a b)))))

(define (%string-reverse!-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (j (- b 1)))
      (cond ((>= i j) (if #f #f))
            (else
             (let ((c (string-ref s i)))
               (string-set! s i (string-ref s j))
               (string-set! s j c))
             (loop (+ i 1) (- j 1)))))))

(define string-reverse
  (case-lambda
    ((s)
     "Return a fresh string containing the characters of S in reverse order.

Optional [start [end]] indices restrict the reversal to that substring.
The result is a freshly allocated string of length (- end start).

Examples:
  (string-reverse \"hello\")       => \"olleh\"
  (string-reverse \"\")             => \"\"
  (string-reverse \"hello\" 1 4)   => \"lle\"

Parameters:
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: reverse, flip, mirror

See also: `string-reverse!', `reverse-list->string'."
     (%string-reverse-impl s 0 (string-length s)))
    ((s start)
     (%string-reverse-impl s start (string-length s)))
    ((s start end)
     (%string-reverse-impl s start end))))

(define string-reverse!
  (case-lambda
    ((s)
     "Reverse S in place via string-set! swaps. Optional [start [end]]
indices restrict the reversal to that substring. Return value is
unspecified.

Note: SRFI-13 mutating forms operate on simple chars only — no Unicode
case-fold-style length changes are possible because the string length
is fixed.

Examples:
  (let ((s (make-string 5 #\\x)))
    (string-set! s 0 #\\h)
    (string-set! s 1 #\\e)
    (string-set! s 2 #\\l)
    (string-set! s 3 #\\l)
    (string-set! s 4 #\\o)
    (string-reverse! s)
    s)
  => \"olleh\"

Parameters:
  s : mutable string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: unspecified
Category: srfi-13
Keywords: reverse, mutate, in-place, destructive

See also: `string-reverse', `string-set!'."
     (%string-reverse!-impl s 0 (string-length s)))
    ((s start)
     (%string-reverse!-impl s start (string-length s)))
    ((s start end)
     (%string-reverse!-impl s start end))))
