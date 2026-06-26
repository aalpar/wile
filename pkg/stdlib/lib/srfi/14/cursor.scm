;; cursor.scm -- SRFI-14 cursor protocol, char-set-hash, char-set-diff+intersection
;; Part of SRFI 14: Character-Set Library
;;
;; The cursor protocol is SRFI-14's low-level, stateless iteration interface,
;; predating char-set-fold / char-set-for-each. A cursor is an opaque value
;; that designates a position within a char-set; the four operations
;; (char-set-cursor, char-set-ref, char-set-cursor-next, end-of-char-set?)
;; let a caller walk the members one at a time without callbacks.
;;
;; Representation: a cursor walks the canonical inversion-list ranges exposed
;; by char-set-ranges (the same single leak point used by util.scm). It is one
;; of two shapes:
;;
;;   the symbol 'end          -- exhausted, no more members
;;   (cp hi . rest-ranges)    -- cp  = current codepoint (an integer)
;;                               hi  = inclusive upper bound of cp's range
;;                               rest-ranges = ranges strictly after cp's range
;;
;; Storing hi and the remaining ranges in the cursor keeps each step O(1) and
;; avoids re-fetching char-set-ranges on every char-set-cursor-next. The cursor
;; never embeds the char-set itself, so char-set-ref / char-set-cursor-next
;; ignore their CS argument (kept only for SRFI-14 signature conformance).
;;
;; Surrogate handling: integer->char rejects the UTF-16 surrogate block
;; U+D800..U+DFFF (an invalid scalar value). char-set:full and any set built
;; over the whole codepoint space therefore contain "members" the cursor must
;; not hand to integer->char via char-set-ref. We skip the surrogate block
;; defensively during cursor advancement so char-set-ref always sees a valid
;; scalar value. This does NOT alter construction-time range logic (that is a
;; separate concern); it only guards iteration.

(define %char-set-surrogate-lo #xD800)
(define %char-set-surrogate-hi #xDFFF)

(define (%char-set-cursor-from-ranges ranges)
  "Internal: build a cursor positioned at the first valid (non-surrogate)
codepoint of RANGES, or 'end if no such codepoint exists."
  (if (null? ranges)
      'end
      (let* ((r (car ranges))
             (lo (car r))
             (hi (cdr r)))
        (%char-set-cursor-advance-to-valid lo hi (cdr ranges)))))

(define (%char-set-cursor-advance-to-valid cp hi rest)
  "Internal: return a cursor at CP within the range ending at HI (with REST
ranges following), skipping the surrogate block. If CP lands inside the
surrogate block, jump past it; if that exhausts the current range, descend
into REST. Returns 'end when nothing valid remains."
  (cond
    ;; CP exhausted the current range: move on to the next range.
    ((> cp hi)
     (%char-set-cursor-from-ranges rest))
    ;; CP is inside the surrogate block: jump to the first codepoint past it.
    ((and (>= cp %char-set-surrogate-lo) (<= cp %char-set-surrogate-hi))
     (%char-set-cursor-advance-to-valid (+ %char-set-surrogate-hi 1) hi rest))
    ;; CP is a valid scalar value: this is the cursor position.
    (else
     (cons cp (cons hi rest)))))

(define (char-set-cursor cs)
  "Return a cursor positioned at the first character of CS, or an
end-of-char-set cursor if CS is empty.

Examples:
  (let ((c (char-set-cursor (char-set #\\a))))
    (char-set-ref (char-set #\\a) c))  =>  #\\a

Parameters:
  cs : char-set
Returns: cursor (opaque)
Category: srfi-14
Keywords: cursor, iterate, traverse, char-set, srfi-14

See also: `char-set-ref', `char-set-cursor-next', `end-of-char-set?'."
  (%char-set-cursor-from-ranges (char-set-ranges cs)))

(define (char-set-ref cs cursor)
  "Return the character that CURSOR currently designates in CS.
It is an error to call this on an end-of-char-set cursor.

Examples:
  (char-set-ref (char-set #\\a) (char-set-cursor (char-set #\\a)))  =>  #\\a

Parameters:
  cs : char-set
  cursor : cursor (opaque, not at end)
Returns: char
Category: srfi-14
Keywords: cursor, ref, dereference, char-set, srfi-14

See also: `char-set-cursor', `char-set-cursor-next', `end-of-char-set?'."
  (if (eq? cursor 'end)
      (error "char-set-ref: cursor is at end of char-set" cs)
      (integer->char (car cursor))))

(define (char-set-cursor-next cs cursor)
  "Advance CURSOR to the next character of CS and return the new cursor.
The surrogate block U+D800..U+DFFF is skipped. It is an error to advance an
end-of-char-set cursor.

Examples:
  (let* ((cs (char-set #\\a #\\b))
         (c0 (char-set-cursor cs))
         (c1 (char-set-cursor-next cs c0)))
    (char-set-ref cs c1))  =>  #\\b

Parameters:
  cs : char-set
  cursor : cursor (opaque, not at end)
Returns: cursor (opaque)
Category: srfi-14
Keywords: cursor, next, advance, iterate, char-set, srfi-14

See also: `char-set-cursor', `char-set-ref', `end-of-char-set?'."
  (if (eq? cursor 'end)
      (error "char-set-cursor-next: cannot advance an end cursor" cs)
      (let ((cp (car cursor))
            (hi (cadr cursor))
            (rest (cddr cursor)))
        (%char-set-cursor-advance-to-valid (+ cp 1) hi rest))))

(define (end-of-char-set? cursor)
  "Return #t if CURSOR has walked past the last character of its char-set.

Examples:
  (end-of-char-set? (char-set-cursor (char-set)))  =>  #t

Parameters:
  cursor : cursor (opaque)
Returns: boolean
Category: srfi-14
Keywords: cursor, end, exhausted, predicate, char-set, srfi-14

See also: `char-set-cursor', `char-set-ref', `char-set-cursor-next'."
  (eq? cursor 'end))

;; char-set-hash -- stable, bounded content hash.
;;
;; The canonical inversion-list form is unique per set value (every
;; constructor enforces sorted/disjoint/non-adjacent ranges), so two
;; char-set= sets have identical char-set-ranges and therefore the same hash.
;; We fold over ranges (not individual codepoints) so the cost is O(#ranges),
;; independent of cardinality -- hashing char-set:full is cheap.
(define %char-set-hash-default-bound #x4000000) ; 2^26, fixnum-safe default

(define (%char-set-hash-impl cs bound)
  (let ((h (let loop ((ranges (char-set-ranges cs)) (acc 0))
             (if (null? ranges)
                 acc
                 (let ((r (car ranges)))
                   ;; Mix lo and hi with a multiplicative rolling hash.
                   (loop (cdr ranges)
                         (+ (* acc 31)
                            (* (car r) 131)
                            (cdr r))))))))
    (modulo h bound)))

(define char-set-hash
  (case-lambda
    ((cs)
     "Return a stable hash of CS in [0, bound). The hash depends only on the
set's membership: two char-set= sets hash equal. An optional BOUND (a positive
exact integer) caps the result; the default is implementation-defined.

Examples:
  (char-set-hash (char-set #\\a #\\b))                       =>  some integer
  (= (char-set-hash (char-set #\\a #\\b))
     (char-set-hash (char-set #\\b #\\a)))                   =>  #t

Parameters:
  cs : char-set
  bound : positive exact integer (optional)
Returns: exact integer in [0, bound)
Category: srfi-14
Keywords: hash, key, stable, bounded, char-set, srfi-14

See also: `char-set=', `char-set-ranges'."
     (%char-set-hash-impl cs %char-set-hash-default-bound))
    ((cs bound)
     (if (<= bound 0)
         (error "char-set-hash: bound must be a positive integer" bound)
         (%char-set-hash-impl cs bound)))))

(define (char-set-diff+intersection cs1 . charsets)
  "Return two values: the difference (CS1 minus the union of CHARSETS) and the
intersection (CS1 with the union of CHARSETS). Equivalent to calling
char-set-difference and char-set-intersection separately, but computed in a
single pass and returned together via `values'.

Examples:
  (call-with-values
    (lambda () (char-set-diff+intersection (char-set #\\a #\\b #\\c)
                                           (char-set #\\b #\\c #\\d)))
    (lambda (diff inter) (list diff inter)))
    =>  (#<char-set: a> #<char-set: b c>)

Parameters:
  cs1 : char-set
  charsets : char-set ... (variadic)
Returns: two values -- (difference intersection)
Category: srfi-14
Keywords: difference, intersection, diff, multiple-values, char-set, srfi-14

See also: `char-set-difference', `char-set-intersection'."
  (let ((others (apply char-set-union charsets)))
    (values (char-set-difference cs1 others)
            (char-set-intersection cs1 others))))

(define char-set-diff+intersection! char-set-diff+intersection)
