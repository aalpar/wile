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
A criterion is a char (compared with char=?), a char-set (via
char-set-contains?), or a predicate procedure of one argument.

Parameters:
  criterion : char or char-set or procedure
  ch : char
Returns: boolean
Category: srfi-13
Keywords: criterion, predicate, char, char-set, internal"
  (cond ((char? criterion)      (char=? criterion ch))
        ((char-set? criterion)  (char-set-contains? criterion ch))
        ((procedure? criterion) (criterion ch))
        (else (error "string-* criterion must be char, char-set, or procedure"
                     criterion))))
