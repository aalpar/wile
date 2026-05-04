;; prefix-suffix.scm -- SRFI-13 prefix/suffix predicates
;; Part of SRFI 13: String Library

(define (%string-prefix? s1 start1 end1 s2 start2 end2)
  (let-values (((a1 b1) (%string-range-check s1 start1 end1))
               ((a2 b2) (%string-range-check s2 start2 end2)))
    (let ((len1 (- b1 a1))
          (len2 (- b2 a2)))
      (and (>= len2 len1)
           (let loop ((i 0))
             (cond ((= i len1) #t)
                   ((char=? (string-ref s1 (+ a1 i))
                            (string-ref s2 (+ a2 i)))
                    (loop (+ i 1)))
                   (else #f)))))))

(define (%string-suffix? s1 start1 end1 s2 start2 end2)
  (let-values (((a1 b1) (%string-range-check s1 start1 end1))
               ((a2 b2) (%string-range-check s2 start2 end2)))
    (let ((len1 (- b1 a1))
          (len2 (- b2 a2)))
      (and (>= len2 len1)
           (let loop ((i 1))
             (cond ((> i len1) #t)
                   ((char=? (string-ref s1 (- b1 i))
                            (string-ref s2 (- b2 i)))
                    (loop (+ i 1)))
                   (else #f)))))))

(define string-prefix?
  (case-lambda
    ((s1 s2)
     "Return #t if S1 (or its restricted substring) is a prefix of S2.

Optional [start1 [end1 [start2 [end2]]]] indices restrict the comparison
to the indicated substrings of S1 and S2.

Examples:
  (string-prefix? \"foo\" \"foobar\")        => #t
  (string-prefix? \"foo\" \"barfoo\")        => #f
  (string-prefix? \"\" \"anything\")         => #t
  (string-prefix? \"foo\" \"xfoobar\" 0 3 1 4) => #t

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: boolean
Category: srfi-13
Keywords: prefix, starts-with, beginning, predicate

See also: `string-suffix?', `string-prefix-ci?', `string-prefix-length'."
     (%string-prefix? s1 0 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1)
     (%string-prefix? s1 start1 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1 end1)
     (%string-prefix? s1 start1 end1 s2 0 (string-length s2)))
    ((s1 s2 start1 end1 start2)
     (%string-prefix? s1 start1 end1 s2 start2 (string-length s2)))
    ((s1 s2 start1 end1 start2 end2)
     (%string-prefix? s1 start1 end1 s2 start2 end2))))

(define string-suffix?
  (case-lambda
    ((s1 s2)
     "Return #t if S1 (or its restricted substring) is a suffix of S2.

Optional [start1 [end1 [start2 [end2]]]] indices restrict the comparison
to the indicated substrings of S1 and S2.

Examples:
  (string-suffix? \"bar\" \"foobar\")        => #t
  (string-suffix? \"bar\" \"barfoo\")        => #f
  (string-suffix? \"\" \"anything\")         => #t

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: boolean
Category: srfi-13
Keywords: suffix, ends-with, ending, predicate

See also: `string-prefix?', `string-suffix-ci?', `string-suffix-length'."
     (%string-suffix? s1 0 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1)
     (%string-suffix? s1 start1 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1 end1)
     (%string-suffix? s1 start1 end1 s2 0 (string-length s2)))
    ((s1 s2 start1 end1 start2)
     (%string-suffix? s1 start1 end1 s2 start2 (string-length s2)))
    ((s1 s2 start1 end1 start2 end2)
     (%string-suffix? s1 start1 end1 s2 start2 end2))))
