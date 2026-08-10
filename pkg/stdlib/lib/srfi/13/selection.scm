;; selection.scm -- SRFI-13 string selection (take/drop/tabulate)
;; Part of SRFI 13: String Library

(define (%check-take-drop-n who s n)
  (let ((len (string-length s)))
    (cond ((or (< n 0) (> n len))
           (error (string-append who ": n out of bounds") n len)))))

(define (string-take s n)
  "Return the first N chars of S.

Examples:
  (string-take \"hello\" 3)  => \"hel\"
  (string-take \"hello\" 0)  => \"\"
  (string-take \"hello\" 5)  => \"hello\"

Parameters:
  s : string
  n : integer (0 <= n <= (string-length s))
Returns: string
Category: srfi-13
Keywords: take, prefix, head, leftmost

See also: `string-drop', `string-take-right'."
  (%check-take-drop-n "string-take" s n)
  (substring s 0 n))

(define (string-drop s n)
  "Return all chars of S after the first N.

Examples:
  (string-drop \"hello\" 3)  => \"lo\"
  (string-drop \"hello\" 0)  => \"hello\"
  (string-drop \"hello\" 5)  => \"\"

Parameters:
  s : string
  n : integer (0 <= n <= (string-length s))
Returns: string
Category: srfi-13
Keywords: drop, skip, tail, rest

See also: `string-take', `string-drop-right'."
  (%check-take-drop-n "string-drop" s n)
  (substring s n (string-length s)))

(define (string-take-right s n)
  "Return the last N chars of S.

Examples:
  (string-take-right \"hello\" 3)  => \"llo\"
  (string-take-right \"hello\" 0)  => \"\"

Parameters:
  s : string
  n : integer (0 <= n <= (string-length s))
Returns: string
Category: srfi-13
Keywords: take, suffix, end, rightmost

See also: `string-take', `string-drop-right'."
  (%check-take-drop-n "string-take-right" s n)
  (let ((len (string-length s)))
    (substring s (- len n) len)))

(define (string-drop-right s n)
  "Return all chars of S except the last N.

Examples:
  (string-drop-right \"hello\" 3)  => \"he\"
  (string-drop-right \"hello\" 0)  => \"hello\"

Parameters:
  s : string
  n : integer (0 <= n <= (string-length s))
Returns: string
Category: srfi-13
Keywords: drop, prefix, beginning, leftmost

See also: `string-drop', `string-take-right'."
  (%check-take-drop-n "string-drop-right" s n)
  (substring s 0 (- (string-length s) n)))

(define substring/shared
  (case-lambda
    ((s start)
     "Return the substring of S from START to the end, or from START to END.

SRFI-13 permits but does not require the result to share storage with
S (\"compliant implementations are allowed, but not required, to provide
this kind of sharing\"), so this copies. The deviation that mattered was
arity: END is optional in SRFI-13, and a 2-argument call raised a
wrong-number-of-arguments error until 2026-08-09.

Examples:
  (substring/shared \"abcdef\" 2)    => \"cdef\"
  (substring/shared \"abcdef\" 2 4)  => \"cd\"

Parameters:
  s : string
  start : integer (0 <= start <= (string-length s))
  end : integer (optional, default (string-length s))
Returns: string (fresh; sharing is permitted, not required)
Category: srfi-13
Keywords: substring, share, slice, range

See also: `string-copy' (R7RS), `string-take', `string-drop'."
     (string-copy s start (string-length s)))
    ((s start end)
     (string-copy s start end))))

(define (string-tabulate proc len)
  "Return a string of length LEN whose i-th char is (PROC i).

The string is built right-to-left into a list, then converted via
list->string. Single allocation for the result; no mutation.

Examples:
  (string-tabulate (lambda (i) (integer->char (+ 65 i))) 5)
    => \"ABCDE\"
  (string-tabulate (lambda (i) #\\x) 3)
    => \"xxx\"

Parameters:
  proc : procedure -- (proc i) -> char for 0 <= i < len
  len : integer (>= 0)
Returns: string
Category: srfi-13
Keywords: build, construct, generate, fill, make

See also: `make-string', `string-map'."
  (let loop ((i (- len 1)) (acc '()))
    (cond ((< i 0) (list->string acc))
          (else (loop (- i 1) (cons (proc i) acc))))))

(define (%xsubstring-impl s from to start end)
  ;; Treat s[start..end) as an infinite string formed by cyclically
  ;; repeating that slice in both directions, then extract indices
  ;; [from, to). Index 0 of the cyclic string corresponds to s[start].
  ;; FROM and TO may be negative or exceed the slice length; they wrap.
  (let-values (((a b) (%string-range-check s start end)))
    (let ((slice-len (- b a)))
      (cond
       ((> from to)
        (error "xsubstring: from must be <= to" from to))
       ((= from to) "")
       ((<= slice-len 0)
        ;; A zero-length source cannot fill a non-empty request.
        (error "xsubstring: cannot replicate an empty substring" start end))
       (else
        (let ((out (make-string (- to from))))
          (let loop ((i from) (j 0))
            (cond ((>= i to) out)
                  (else
                   ;; modulo keeps the index in [0, slice-len) even for
                   ;; negative i, since Scheme modulo follows the divisor sign.
                   (string-set! out j (string-ref s (+ a (modulo i slice-len))))
                   (loop (+ i 1) (+ j 1)))))))))))

(define xsubstring
  (case-lambda
    ((s from)
     "Extract a substring from the infinite cyclic repetition of the
source slice S[start..end). The cyclic string's index 0 is S[start];
indices [FROM, TO) are returned. FROM and TO may be negative or exceed
the slice length and wrap cyclically.

TO defaults to FROM + (end - start), i.e. one full period. The source
slice must be non-empty whenever FROM /= TO.

Examples:
  (xsubstring \"abcdef\" 0)      => \"abcdef\"
  (xsubstring \"abc\" 0 7)       => \"abcabca\"
  (xsubstring \"abc\" -2 2)      => \"bcab\"
  (xsubstring \"abcdef\" 2 8)    => \"cdefab\"
  (xsubstring \"abc\" 1 1)       => \"\"

Parameters:
  s : string
  from : integer -- start index into the cyclic string (may be negative)
  to : integer (optional, default (+ from (- end start)))
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string (fresh), length (- to from)
Category: srfi-13
Keywords: xsubstring, cyclic, repeat, rotate, wrap, extended-substring

See also: `substring', `string-take', `string-pad'."
     (%xsubstring-impl s from (+ from (string-length s))
                       0 (string-length s)))
    ((s from to)
     (%xsubstring-impl s from to 0 (string-length s)))
    ((s from to start)
     (%xsubstring-impl s from to start (string-length s)))
    ((s from to start end)
     (%xsubstring-impl s from to start end))))
