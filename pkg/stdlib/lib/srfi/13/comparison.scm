;; comparison.scm -- SRFI-13 binary comparisons (with optional ranges) +
;;                   3-way string-compare / string-compare-ci.
;; Part of SRFI 13: String Library
;;
;; Wile note: SRFI-13 binary comparisons are deliberately distinct from
;; the R7RS variadic forms (`string=?', `string<?', etc.). The R7RS forms
;; accept arbitrary arity and have no slicing arguments; the SRFI-13
;; forms accept exactly two strings and take optional [start1 end1
;; start2 end2] indices for slicing. Both families coexist in
;; (wile strings) (the kitchen-sink library).

(define (%string-cmp cmp s1 a1 b1 s2 a2 b2)
  (let-values (((aa1 bb1) (%string-range-check s1 a1 b1))
               ((aa2 bb2) (%string-range-check s2 a2 b2)))
    (cmp (substring s1 aa1 bb1) (substring s2 aa2 bb2))))

(define-syntax %define-srfi-13-cmp
  (syntax-rules ()
    ((_ name r7rs-cmp doc)
     (define name
       (case-lambda
         ((s1 s2) doc (r7rs-cmp s1 s2))
         ((s1 s2 start1)
          (%string-cmp r7rs-cmp s1 start1 (string-length s1) s2 0 (string-length s2)))
         ((s1 s2 start1 end1)
          (%string-cmp r7rs-cmp s1 start1 end1 s2 0 (string-length s2)))
         ((s1 s2 start1 end1 start2)
          (%string-cmp r7rs-cmp s1 start1 end1 s2 start2 (string-length s2)))
         ((s1 s2 start1 end1 start2 end2)
          (%string-cmp r7rs-cmp s1 start1 end1 s2 start2 end2)))))))

(define (%string<>?-impl s1 s2)
  (not (string=? s1 s2)))

(define (%string-ci<>?-impl s1 s2)
  (not (string-ci=? s1 s2)))

;; Boolean SRFI-13 comparisons. Each takes (s1 s2 [start1 [end1 [start2 [end2]]]]).
;; Documented as one block; the macro inlines the case-lambda so each public
;; binding gets its own arity dispatch.

(%define-srfi-13-cmp string=  string=?
  "SRFI-13 binary equality with optional [start1 [end1 [start2 [end2]]]]
ranges. Distinct from R7RS variadic `string=?' (which compares >= 2
strings with no slicing).

Examples:
  (string= \"abc\" \"abc\")            => #t
  (string= \"abc\" \"abcd\")           => #f
  (string= \"xabc\" \"abcy\" 1 4 0 3)  => #t

Parameters:
  s1, s2 : string
  start1 end1 start2 end2 : integer (optional)
Returns: boolean
Category: srfi-13
Keywords: equal, equality, comparison, slice, range

See also: `string=?', `string-ci=', `string-compare'.")

(%define-srfi-13-cmp string<  string<?
  "SRFI-13 binary less-than with optional ranges.

See `string=' for slicing semantics.

Returns: boolean
Category: srfi-13
Keywords: less-than, comparison, lexicographic, slice")

(%define-srfi-13-cmp string>  string>?
  "SRFI-13 binary greater-than with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: greater-than, comparison, lexicographic, slice")

(%define-srfi-13-cmp string<= string<=?
  "SRFI-13 binary less-or-equal with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: less-equal, comparison, lexicographic, slice")

(%define-srfi-13-cmp string>= string>=?
  "SRFI-13 binary greater-or-equal with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: greater-equal, comparison, lexicographic, slice")

(%define-srfi-13-cmp string<> %string<>?-impl
  "SRFI-13 binary inequality with optional ranges. Returns #t iff the
two slices are NOT string=? equal.

Returns: boolean
Category: srfi-13
Keywords: not-equal, inequality, comparison, slice")

(%define-srfi-13-cmp string-ci=  string-ci=?
  "SRFI-13 case-insensitive binary equality with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: equal, case-insensitive, ci, comparison, slice")

(%define-srfi-13-cmp string-ci<  string-ci<?
  "SRFI-13 case-insensitive binary less-than with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: less-than, case-insensitive, ci, comparison, slice")

(%define-srfi-13-cmp string-ci>  string-ci>?
  "SRFI-13 case-insensitive binary greater-than with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: greater-than, case-insensitive, ci, comparison, slice")

(%define-srfi-13-cmp string-ci<= string-ci<=?
  "SRFI-13 case-insensitive binary less-or-equal with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: less-equal, case-insensitive, ci, comparison, slice")

(%define-srfi-13-cmp string-ci>= string-ci>=?
  "SRFI-13 case-insensitive binary greater-or-equal with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: greater-equal, case-insensitive, ci, comparison, slice")

(%define-srfi-13-cmp string-ci<> %string-ci<>?-impl
  "SRFI-13 case-insensitive binary inequality with optional ranges.

Returns: boolean
Category: srfi-13
Keywords: not-equal, case-insensitive, ci, inequality, slice")

;; ============================================================
;; 3-way string-compare / string-compare-ci.
;;
;; Returns the value produced by applying proc<, proc=, or proc> to the
;; mismatch index (in s1's coordinate system). For two ranges that match
;; up through the shorter, the index passed is end1 when s1 is shorter
;; or equal, and the actual mismatch position when s1 is longer.
;; Matches the SRFI-13 reference implementation semantics.
;; ============================================================

(define (%string-compare-impl s1 a1 b1 s2 a2 b2 proc< proc= proc>)
  (let ((size1 (- b1 a1))
        (size2 (- b2 a2)))
    (let loop ((i 0))
      (cond
        ;; Both ranges fully exhausted - strings are equal.
        ((and (= i size1) (= i size2))
         (proc= b1))
        ;; s1 fully consumed, s2 has more chars - s1 is "less".
        ((= i size1)
         (proc< b1))
        ;; s2 fully consumed, s1 has more chars - s1 is "greater".
        ((= i size2)
         (proc> (+ a1 i)))
        ;; Both have chars at offset i; compare them.
        (else
         (let ((c1 (string-ref s1 (+ a1 i)))
               (c2 (string-ref s2 (+ a2 i))))
           (cond ((char<? c1 c2) (proc< (+ a1 i)))
                 ((char>? c1 c2) (proc> (+ a1 i)))
                 (else (loop (+ i 1))))))))))

(define (%string-compare-ci-impl s1 a1 b1 s2 a2 b2 proc< proc= proc>)
  ;; Foldcase the slices (not the whole strings) and recurse with full
  ;; ranges over the folded substrings. Index returned to proc</proc=/proc>
  ;; is the offset into the folded slice; for ASCII inputs this matches
  ;; the original-coordinate index, consistent with Phase-1 -ci semantics.
  (let-values (((aa1 bb1) (%string-range-check s1 a1 b1))
               ((aa2 bb2) (%string-range-check s2 a2 b2)))
    (let ((f1 (string-foldcase (substring s1 aa1 bb1)))
          (f2 (string-foldcase (substring s2 aa2 bb2))))
      ;; The mismatch index is reported relative to f1; add aa1 so callers
      ;; see indices in s1's original coordinate system (only valid for
      ;; ASCII / length-preserving foldcase).
      (%string-compare-impl
       f1 0 (string-length f1)
       f2 0 (string-length f2)
       (lambda (i) (proc< (+ aa1 i)))
       (lambda (i) (proc= (+ aa1 i)))
       (lambda (i) (proc> (+ aa1 i)))))))

(define string-compare
  (case-lambda
    ((s1 s2 proc< proc= proc>)
     "3-way string comparison. Walks chars of S1 and S2 to find the
mismatch index, then applies one of PROC<, PROC=, PROC> to that index
(in S1's coordinate system).

If S1's range exhausts first (equal prefix, S1 shorter), PROC< is
called with end1. If S2's range exhausts first, PROC> is called.
If both exhaust together, PROC= is called with end1.

Optional [start1 [end1 [start2 [end2]]]] indices restrict the comparison.

Examples:
  (string-compare \"abc\" \"abd\" (lambda (i) 'lt) (lambda (i) 'eq) (lambda (i) 'gt))
    => 'lt
  (string-compare \"abc\" \"abc\" (lambda (i) 'lt) (lambda (i) 'eq) (lambda (i) 'gt))
    => 'eq

Parameters:
  s1, s2 : string
  proc<, proc=, proc> : procedure of one integer argument
  start1 end1 start2 end2 : integer (optional)
Returns: any (the value produced by the selected procedure)
Category: srfi-13
Keywords: compare, three-way, lexicographic, mismatch, dispatch

See also: `string=', `string-compare-ci', `string-prefix-length'."
     (%string-compare-impl s1 0 (string-length s1)
                           s2 0 (string-length s2)
                           proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1)
     (%string-compare-impl s1 start1 (string-length s1)
                           s2 0 (string-length s2)
                           proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1)
     (%string-compare-impl s1 start1 end1
                           s2 0 (string-length s2)
                           proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1 start2)
     (%string-compare-impl s1 start1 end1
                           s2 start2 (string-length s2)
                           proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1 start2 end2)
     (%string-compare-impl s1 start1 end1
                           s2 start2 end2
                           proc< proc= proc>))))

(define string-compare-ci
  (case-lambda
    ((s1 s2 proc< proc= proc>)
     "Case-insensitive 3-way comparison. See `string-compare'.

Foldcases each slice once before comparing; for ASCII inputs the index
passed to PROC<, PROC=, or PROC> matches the original coordinate
system. For Unicode foldcase that changes length, indices may shift —
a documented v1 limitation shared with `string-contains-ci'.

Returns: any
Category: srfi-13
Keywords: compare, three-way, case-insensitive, ci, lexicographic

See also: `string-compare', `string-ci='."
     (%string-compare-ci-impl s1 0 (string-length s1)
                              s2 0 (string-length s2)
                              proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1)
     (%string-compare-ci-impl s1 start1 (string-length s1)
                              s2 0 (string-length s2)
                              proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1)
     (%string-compare-ci-impl s1 start1 end1
                              s2 0 (string-length s2)
                              proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1 start2)
     (%string-compare-ci-impl s1 start1 end1
                              s2 start2 (string-length s2)
                              proc< proc= proc>))
    ((s1 s2 proc< proc= proc> start1 end1 start2 end2)
     (%string-compare-ci-impl s1 start1 end1
                              s2 start2 end2
                              proc< proc= proc>))))

(define %string-hash-default-bound 4194304) ; 2^22; a fixed, modest default range

(define (%string-hash-impl s start end bound)
  ;; Deterministic polynomial (Java-style) rolling hash over the char
  ;; codepoints in [start, end). Uses bignum-safe arithmetic, then
  ;; reduces modulo BOUND so the result is always in [0, bound). The hash
  ;; is stable for a given string within and across runs (no randomized
  ;; seed), satisfying SRFI-13's requirement that equal strings under
  ;; string= hash equal.
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (h 0))
      (cond ((>= i b) (modulo h bound))
            (else
             (loop (+ i 1)
                   (+ (* h 31) (char->integer (string-ref s i)))))))))

(define string-hash
  (case-lambda
    ((s)
     "Return a non-negative integer hash of string S.

With no BOUND, the result lies in [0, 2^22). With BOUND, the result lies
in [0, BOUND). The hash is a deterministic polynomial over the character
codepoints (h := h*31 + codepoint), so equal strings (under string=)
always hash equal, both within a run and across runs.

Examples:
  (>= (string-hash \"abc\") 0)          => #t
  (< (string-hash \"abc\" 100) 100)     => #t
  (= (string-hash \"abc\") (string-hash \"abc\")) => #t

Parameters:
  s : string
  bound : positive integer (optional) -- result in [0, bound)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: non-negative integer
Category: srfi-13
Keywords: hash, digest, fingerprint, bound

See also: `string=', `string-hash-ci' (SRFI-13, not yet provided)."
     (%string-hash-impl s 0 (string-length s) %string-hash-default-bound))
    ((s bound)
     (cond ((or (not (integer? bound)) (<= bound 0))
            (error "string-hash: bound must be a positive integer" bound)))
     (%string-hash-impl s 0 (string-length s) bound))
    ((s bound start)
     (cond ((or (not (integer? bound)) (<= bound 0))
            (error "string-hash: bound must be a positive integer" bound)))
     (%string-hash-impl s start (string-length s) bound))
    ((s bound start end)
     (cond ((or (not (integer? bound)) (<= bound 0))
            (error "string-hash: bound must be a positive integer" bound)))
     (%string-hash-impl s start end bound))))
