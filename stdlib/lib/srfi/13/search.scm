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
