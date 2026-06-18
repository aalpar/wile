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

(define substring/shared substring)

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
