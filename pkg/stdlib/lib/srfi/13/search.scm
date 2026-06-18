;; search.scm -- SRFI-13 substring search
;; Part of SRFI 13: String Library
;;
;; Phase 1 ships string-contains and string-contains-ci. The rest of the
;; search family (string-index, string-skip, string-count) is added in
;; Phase 3.

(define (%string-contains s1 start1 end1 s2 start2 end2)
  (let-values (((a1 b1) (%string-range-check s1 start1 end1))
               ((a2 b2) (%string-range-check s2 start2 end2)))
    (let ((needle-len (- b2 a2))
          (haystack-end b1))
      (cond ((zero? needle-len) a1)
            (else
             (let loop ((i a1))
               (cond ((> (+ i needle-len) haystack-end) #f)
                     ((let inner ((j 0))
                        (cond ((= j needle-len) #t)
                              ((char=? (string-ref s1 (+ i j))
                                       (string-ref s2 (+ a2 j)))
                               (inner (+ j 1)))
                              (else #f)))
                      i)
                     (else (loop (+ i 1))))))))))

(define string-contains
  (case-lambda
    ((s1 s2)
     "Return the lowest index in S1 at which S2 occurs as a substring,
or #f if no occurrence is found.

The optional indices [start1 [end1 [start2 [end2]]]] restrict the
search to substrings S1[start1:end1] and the pattern to S2[start2:end2].
The returned index is into the original S1 (not the slice).

A naive O(n*m) scan. v1 Scheme implementation; promote to a Go FFI
backed by strings.Index when profiling justifies it.

Examples:
  (string-contains \"hello world\" \"world\")  => 6
  (string-contains \"hello\" \"xyz\")           => #f
  (string-contains \"hello\" \"\")              => 0

Parameters:
  s1 : string -- the haystack
  s2 : string -- the needle
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: integer or #f
Category: srfi-13
Keywords: contains, search, find, substring, index

See also: `string-contains-ci', `string-index'."
     (%string-contains s1 0 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1)
     (%string-contains s1 start1 (string-length s1) s2 0 (string-length s2)))
    ((s1 s2 start1 end1)
     (%string-contains s1 start1 end1 s2 0 (string-length s2)))
    ((s1 s2 start1 end1 start2)
     (%string-contains s1 start1 end1 s2 start2 (string-length s2)))
    ((s1 s2 start1 end1 start2 end2)
     (%string-contains s1 start1 end1 s2 start2 end2))))

(define string-contains-ci
  (case-lambda
    ((s1 s2)
     "Case-insensitive variant of `string-contains'. Both inputs are
folded with `string-foldcase' once, then a regular substring search
is performed. The returned index is into the folded haystack — for
ASCII inputs (the common case) this matches the original. For
inputs whose Unicode case folding changes length, indices may
differ; this is a documented v1 limitation.

Examples:
  (string-contains-ci \"Hello World\" \"WORLD\")  => 6
  (string-contains-ci \"Hello\" \"world\")         => #f

Parameters:
  s1 : string -- the haystack
  s2 : string -- the needle
  start1 : integer (optional, default 0)
  end1 : integer (optional, default (string-length s1))
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: integer or #f
Category: srfi-13
Keywords: contains, search, find, case-insensitive, ci

See also: `string-contains'."
     (string-contains (string-foldcase s1) (string-foldcase s2)))
    ((s1 s2 start1)
     (string-contains (string-foldcase s1) (string-foldcase s2) start1))
    ((s1 s2 start1 end1)
     (string-contains (string-foldcase s1) (string-foldcase s2) start1 end1))
    ((s1 s2 start1 end1 start2)
     (string-contains (string-foldcase s1) (string-foldcase s2) start1 end1 start2))
    ((s1 s2 start1 end1 start2 end2)
     (string-contains (string-foldcase s1) (string-foldcase s2) start1 end1 start2 end2))))

;; ============================================================
;; Phase 3: index / skip / count family
;;
;; Argument order for the optional range arguments is [start end]
;; uniformly for all five procedures, including the -right variants.
;; Canonical SRFI-13 specifies [end start] for string-index-right and
;; string-skip-right; Wile uses [start end] for consistency with the
;; other SRFI-13 procedures and the R7RS-extras family. Document the
;; range explicitly when calling the right variants if your code is
;; intended to be portable to other SRFI-13 implementations.
;; ============================================================

(define (%string-index s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((>= i b) #f)
            ((%match-char? criterion (string-ref s i)) i)
            (else (loop (+ i 1)))))))

(define (%string-index-right s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i (- b 1)))
      (cond ((< i a) #f)
            ((%match-char? criterion (string-ref s i)) i)
            (else (loop (- i 1)))))))

(define (%string-skip s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((>= i b) #f)
            ((%match-char? criterion (string-ref s i)) (loop (+ i 1)))
            (else i)))))

(define (%string-skip-right s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i (- b 1)))
      (cond ((< i a) #f)
            ((%match-char? criterion (string-ref s i)) (loop (- i 1)))
            (else i)))))

(define (%string-count s criterion start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (count 0))
      (cond ((>= i b) count)
            ((%match-char? criterion (string-ref s i))
             (loop (+ i 1) (+ count 1)))
            (else (loop (+ i 1) count))))))

(define string-index
  (case-lambda
    ((s criterion)
     "Return the index of the first char in S matching CRITERION,
or #f if none match.

CRITERION is a char (compared via char=?), a char-set (SRFI-14), or a
predicate procedure of one argument.

Examples:
  (string-index \"hello world\" #\\space)         => 5
  (string-index \"hello\" char-numeric?)          => #f
  (string-index \"abc123\" char-numeric?)         => 3
  (string-index \"hello\" #\\l 3)                 => 3

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: integer or #f
Category: srfi-13
Keywords: index, find, search, locate, leftmost

See also: `string-index-right', `string-skip', `string-contains'."
     (%string-index s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-index s criterion start (string-length s)))
    ((s criterion start end)
     (%string-index s criterion start end))))

(define string-index-right
  (case-lambda
    ((s criterion)
     "Return the index of the rightmost char in S matching CRITERION,
or #f if none match. Wile uses [start end] argument order (rather than
the canonical SRFI-13 [end start]) for consistency.

Examples:
  (string-index-right \"hello\" #\\l)             => 3
  (string-index-right \"abc123\" char-numeric?)   => 5

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: integer or #f
Category: srfi-13
Keywords: index, find, search, rightmost, last

See also: `string-index', `string-skip-right'."
     (%string-index-right s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-index-right s criterion start (string-length s)))
    ((s criterion start end)
     (%string-index-right s criterion start end))))

(define string-skip
  (case-lambda
    ((s criterion)
     "Return the index of the first char in S that does NOT match
CRITERION, or #f if every char matches.

Examples:
  (string-skip \"   hello\" char-whitespace?)  => 3
  (string-skip \"   \" char-whitespace?)        => #f
  (string-skip \"abc\" #\\a)                    => 1

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: integer or #f
Category: srfi-13
Keywords: skip, find, search, non-match, first

See also: `string-skip-right', `string-index'."
     (%string-skip s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-skip s criterion start (string-length s)))
    ((s criterion start end)
     (%string-skip s criterion start end))))

(define string-skip-right
  (case-lambda
    ((s criterion)
     "Return the index of the rightmost char in S that does NOT match
CRITERION, or #f if every char matches. Wile uses [start end] argument
order (rather than canonical SRFI-13 [end start]) for consistency.

Examples:
  (string-skip-right \"hello   \" char-whitespace?)  => 4
  (string-skip-right \"   \" char-whitespace?)        => #f

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: integer or #f
Category: srfi-13
Keywords: skip, find, rightmost, non-match, last

See also: `string-skip', `string-index-right'."
     (%string-skip-right s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-skip-right s criterion start (string-length s)))
    ((s criterion start end)
     (%string-skip-right s criterion start end))))

(define string-count
  (case-lambda
    ((s criterion)
     "Return the number of chars in S that match CRITERION.

Examples:
  (string-count \"hello world\" #\\l)             => 3
  (string-count \"abc123\" char-numeric?)         => 3
  (string-count \"hello\" #\\z)                   => 0

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: integer
Category: srfi-13
Keywords: count, tally, frequency, number, predicate

See also: `string-index', `string-every'."
     (%string-count s criterion 0 (string-length s)))
    ((s criterion start)
     (%string-count s criterion start (string-length s)))
    ((s criterion start end)
     (%string-count s criterion start end))))
