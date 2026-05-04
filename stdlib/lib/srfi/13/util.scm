;; util.scm -- shared helpers for SRFI-13 implementations
;; Part of SRFI 13: String Library

(define (%string-range-check s start end)
  "Internal: validate that START and END form a valid subrange of
string S. Returns (values start end) when in range; raises an error
on out-of-bounds.

Parameters:
  s : string
  start : integer -- start index (inclusive)
  end : integer -- end index (exclusive)
Returns: values
Category: srfi-13
Keywords: validation, bounds, range, internal"
  (let ((len (string-length s)))
    (cond ((or (< start 0) (> start len))
           (error "string range: start out of bounds" start len))
          ((or (< end start) (> end len))
           (error "string range: end out of bounds" end len))
          (else (values start end)))))

(define (%match-char? criterion ch)
  "Internal: apply a SRFI-13 criterion to a single char.
A criterion is either a char (compared with char=?) or a predicate
procedure of one argument. char-set criteria are deferred until
SRFI-14 lands.

Parameters:
  criterion : char or procedure
  ch : char
Returns: boolean
Category: srfi-13
Keywords: criterion, predicate, char, internal"
  (cond ((char? criterion) (char=? criterion ch))
        ((procedure? criterion) (criterion ch))
        (else (error "string-* criterion must be char or procedure (char-set support deferred to SRFI-14)"
                     criterion))))
