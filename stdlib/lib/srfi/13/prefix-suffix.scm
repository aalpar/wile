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

;; ------------------------------------------------------------
;; Length variants: count matching prefix/suffix chars.
;; ------------------------------------------------------------

(define (%string-prefix-length s1 start1 end1 s2 start2 end2)
  (let-values (((a1 b1) (%string-range-check s1 start1 end1))
               ((a2 b2) (%string-range-check s2 start2 end2)))
    (let ((max-len (min (- b1 a1) (- b2 a2))))
      (let loop ((i 0))
        (cond ((>= i max-len) i)
              ((char=? (string-ref s1 (+ a1 i))
                       (string-ref s2 (+ a2 i)))
               (loop (+ i 1)))
              (else i))))))

(define (%string-suffix-length s1 start1 end1 s2 start2 end2)
  (let-values (((a1 b1) (%string-range-check s1 start1 end1))
               ((a2 b2) (%string-range-check s2 start2 end2)))
    (let ((max-len (min (- b1 a1) (- b2 a2))))
      (let loop ((i 1))
        (cond ((> i max-len) (- i 1))
              ((char=? (string-ref s1 (- b1 i))
                       (string-ref s2 (- b2 i)))
               (loop (+ i 1)))
              (else (- i 1)))))))

(define string-prefix-length
  (case-lambda
    ((s1 s2)
     "Return the length of the longest common prefix of S1 and S2.

Optional [start1 [end1 [start2 [end2]]]] indices restrict the
comparison to the indicated substrings.

Examples:
  (string-prefix-length \"foobar\" \"foobaz\")  => 5
  (string-prefix-length \"abc\" \"xyz\")        => 0
  (string-prefix-length \"\" \"abc\")           => 0

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: integer
Category: srfi-13
Keywords: prefix, common, longest, length, count

See also: `string-prefix?', `string-suffix-length', `string-prefix-length-ci'."
     (%string-prefix-length s1 0 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1)
     (%string-prefix-length s1 start1 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1 end1)
     (%string-prefix-length s1 start1 end1 s2 0 (string-length s2)))
    ((s1 s2 start1 end1 start2)
     (%string-prefix-length s1 start1 end1 s2 start2 (string-length s2)))
    ((s1 s2 start1 end1 start2 end2)
     (%string-prefix-length s1 start1 end1 s2 start2 end2))))

(define string-suffix-length
  (case-lambda
    ((s1 s2)
     "Return the length of the longest common suffix of S1 and S2.

Optional [start1 [end1 [start2 [end2]]]] indices restrict the
comparison to the indicated substrings.

Examples:
  (string-suffix-length \"foobar\" \"goobar\")  => 4
  (string-suffix-length \"abc\" \"xyz\")        => 0

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: integer
Category: srfi-13
Keywords: suffix, common, longest, length, count

See also: `string-suffix?', `string-prefix-length', `string-suffix-length-ci'."
     (%string-suffix-length s1 0 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1)
     (%string-suffix-length s1 start1 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1 end1)
     (%string-suffix-length s1 start1 end1 s2 0 (string-length s2)))
    ((s1 s2 start1 end1 start2)
     (%string-suffix-length s1 start1 end1 s2 start2 (string-length s2)))
    ((s1 s2 start1 end1 start2 end2)
     (%string-suffix-length s1 start1 end1 s2 start2 end2))))

;; ------------------------------------------------------------
;; Case-insensitive variants. Per Phase-1 string-contains-ci, the
;; foldcase-then-delegate strategy is used. ASCII inputs (the common
;; case) yield identical indices; Unicode foldcase that changes length
;; is a documented v1 limitation.
;; ------------------------------------------------------------

(define string-prefix-ci?
  (case-lambda
    ((s1 s2)
     "Case-insensitive `string-prefix?'. Both inputs are folded with
`string-foldcase' before comparison.

Examples:
  (string-prefix-ci? \"FOO\" \"foobar\")  => #t
  (string-prefix-ci? \"BAR\" \"foobar\")  => #f

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: boolean
Category: srfi-13
Keywords: prefix, case-insensitive, ci, predicate

See also: `string-prefix?', `string-suffix-ci?'."
     (string-prefix? (string-foldcase s1) (string-foldcase s2)))
    ((s1 s2 start1)
     (string-prefix? (string-foldcase s1) (string-foldcase s2) start1))
    ((s1 s2 start1 end1)
     (string-prefix? (string-foldcase s1) (string-foldcase s2) start1 end1))
    ((s1 s2 start1 end1 start2)
     (string-prefix? (string-foldcase s1) (string-foldcase s2) start1 end1 start2))
    ((s1 s2 start1 end1 start2 end2)
     (string-prefix? (string-foldcase s1) (string-foldcase s2) start1 end1 start2 end2))))

(define string-suffix-ci?
  (case-lambda
    ((s1 s2)
     "Case-insensitive `string-suffix?'. Both inputs are folded with
`string-foldcase' before comparison.

Examples:
  (string-suffix-ci? \"BAR\" \"foobar\")  => #t
  (string-suffix-ci? \"FOO\" \"foobar\")  => #f

Parameters:
  s1 : string
  s2 : string
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: boolean
Category: srfi-13
Keywords: suffix, case-insensitive, ci, predicate

See also: `string-suffix?', `string-prefix-ci?'."
     (string-suffix? (string-foldcase s1) (string-foldcase s2)))
    ((s1 s2 start1)
     (string-suffix? (string-foldcase s1) (string-foldcase s2) start1))
    ((s1 s2 start1 end1)
     (string-suffix? (string-foldcase s1) (string-foldcase s2) start1 end1))
    ((s1 s2 start1 end1 start2)
     (string-suffix? (string-foldcase s1) (string-foldcase s2) start1 end1 start2))
    ((s1 s2 start1 end1 start2 end2)
     (string-suffix? (string-foldcase s1) (string-foldcase s2) start1 end1 start2 end2))))

(define string-prefix-length-ci
  (case-lambda
    ((s1 s2)
     "Case-insensitive `string-prefix-length'.

Examples:
  (string-prefix-length-ci \"FOObar\" \"foobaz\")  => 5

Parameters: see `string-prefix-length'.
Returns: integer
Category: srfi-13
Keywords: prefix, common, case-insensitive, ci, length

See also: `string-prefix-length', `string-suffix-length-ci'."
     (string-prefix-length (string-foldcase s1) (string-foldcase s2)))
    ((s1 s2 start1)
     (string-prefix-length (string-foldcase s1) (string-foldcase s2) start1))
    ((s1 s2 start1 end1)
     (string-prefix-length (string-foldcase s1) (string-foldcase s2) start1 end1))
    ((s1 s2 start1 end1 start2)
     (string-prefix-length (string-foldcase s1) (string-foldcase s2) start1 end1 start2))
    ((s1 s2 start1 end1 start2 end2)
     (string-prefix-length (string-foldcase s1) (string-foldcase s2) start1 end1 start2 end2))))

(define string-suffix-length-ci
  (case-lambda
    ((s1 s2)
     "Case-insensitive `string-suffix-length'.

Examples:
  (string-suffix-length-ci \"FOOBAR\" \"goobar\")  => 4

Parameters: see `string-suffix-length'.
Returns: integer
Category: srfi-13
Keywords: suffix, common, case-insensitive, ci, length

See also: `string-suffix-length', `string-prefix-length-ci'."
     (string-suffix-length (string-foldcase s1) (string-foldcase s2)))
    ((s1 s2 start1)
     (string-suffix-length (string-foldcase s1) (string-foldcase s2) start1))
    ((s1 s2 start1 end1)
     (string-suffix-length (string-foldcase s1) (string-foldcase s2) start1 end1))
    ((s1 s2 start1 end1 start2)
     (string-suffix-length (string-foldcase s1) (string-foldcase s2) start1 end1 start2))
    ((s1 s2 start1 end1 start2 end2)
     (string-suffix-length (string-foldcase s1) (string-foldcase s2) start1 end1 start2 end2))))
