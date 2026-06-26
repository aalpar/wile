;; replace.scm -- SRFI-13 string-replace and tokenization
;; Part of SRFI 13: String Library
;;
;; Phase 1 ships only string-join. Phase 6 adds string-replace,
;; string-tokenize, string-filter, string-delete.

(define (%string-join-infix strings delim)
  ;; Build [s0, delim, s1, delim, s2, ...] then concatenate.
  (let loop ((acc (list (car strings))) (rest (cdr strings)))
    (cond ((null? rest) (apply string-append (reverse acc)))
          (else (loop (cons (car rest) (cons delim acc)) (cdr rest))))))

(define (%string-join-prefix strings delim)
  ;; Build [delim, s0, delim, s1, ...] then concatenate.
  (let loop ((acc '()) (lst strings))
    (cond ((null? lst) (apply string-append (reverse acc)))
          (else (loop (cons (car lst) (cons delim acc)) (cdr lst))))))

(define (%string-join-suffix strings delim)
  ;; Build [s0, delim, s1, delim, ...] then concatenate.
  (let loop ((acc '()) (lst strings))
    (cond ((null? lst) (apply string-append (reverse acc)))
          (else (loop (cons delim (cons (car lst) acc)) (cdr lst))))))

(define (%string-join strings delim grammar)
  (cond
    ((null? strings)
     (case grammar
       ((infix prefix suffix) "")
       ((strict-infix)
        (error "string-join: strict-infix grammar requires a non-empty list"
               strings))
       (else (error "string-join: unknown grammar" grammar))))
    (else
     (case grammar
       ((infix strict-infix) (%string-join-infix strings delim))
       ((prefix)             (%string-join-prefix strings delim))
       ((suffix)             (%string-join-suffix strings delim))
       (else (error "string-join: unknown grammar" grammar))))))

(define string-join
  (case-lambda
    ((strings)
     "Concatenate STRINGS with DELIMITER between elements. The default
DELIMITER is a single space \" \" per SRFI-13.

GRAMMAR controls how the delimiter is placed:
  infix         -- between elements; empty list -> \"\" (default)
  strict-infix  -- between elements; empty list -> error
  prefix        -- before each element
  suffix        -- after each element

Examples:
  (string-join '(\"a\" \"b\" \"c\") \",\")            => \"a,b,c\"
  (string-join '(\"a\" \"b\" \"c\"))                  => \"a b c\"
  (string-join '() \",\")                          => \"\"
  (string-join '(\"a\" \"b\") \",\" 'prefix)         => \",a,b\"
  (string-join '(\"a\" \"b\") \",\" 'suffix)         => \"a,b,\"

Parameters:
  strings : list of strings
  delimiter : string (optional, default \" \")
  grammar : symbol (optional, default 'infix)
Returns: string
Category: srfi-13
Keywords: join, concatenate, delimiter, separator, glue

See also: `string-split', `string-concatenate'."
     (%string-join strings " " 'infix))
    ((strings delim)
     (%string-join strings delim 'infix))
    ((strings delim grammar)
     (%string-join strings delim grammar))))

;; ============================================================
;; Phase 6: string-replace, string-tokenize, string-filter, string-delete
;; ============================================================

(define (%string-replace-impl s1 s2 i j start2 end2)
  (let-values (((a2 b2) (%string-range-check s2 start2 end2)))
    (cond ((or (< i 0) (> i (string-length s1)))
           (error "string-replace: i out of bounds" i (string-length s1)))
          ((or (< j i) (> j (string-length s1)))
           (error "string-replace: j out of bounds" j i (string-length s1)))
          (else
           (string-append (substring s1 0 i)
                          (substring s2 a2 b2)
                          (substring s1 j (string-length s1)))))))

(define string-replace
  (case-lambda
    ((s1 s2 i j)
     "Splice form: replace S1[i:j] with S2[start2:end2]. Returns the
freshly built string (S1[0:i] ++ S2-slice ++ S1[j:end1]).

Distinct from `string-replace-all' (which is a substring-find-and-replace
in (wile strings) Phase 8) -- this is the SRFI-13 splice operation.

Examples:
  (string-replace \"abcdef\" \"XYZ\" 1 3)        => \"aXYZdef\"
  (string-replace \"abc\" \"\" 1 2)              => \"ac\"  ; deletion via empty s2
  (string-replace \"\" \"hi\" 0 0)               => \"hi\"  ; insertion at end of empty
  (string-replace \"abc\" \"PQR\" 1 1)           => \"aPQRbc\"  ; insertion

Parameters:
  s1 : string -- target
  s2 : string -- replacement
  i, j : integer -- 0 <= i <= j <= (string-length s1)
  start2 : integer (optional, default 0)
  end2 : integer (optional, default (string-length s2))
Returns: string
Category: srfi-13
Keywords: replace, splice, substitute, insert, delete

See also: `string-replace-all', `substring', `string-append'."
     (%string-replace-impl s1 s2 i j 0 (string-length s2)))
    ((s1 s2 i j start2)
     (%string-replace-impl s1 s2 i j start2 (string-length s2)))
    ((s1 s2 i j start2 end2)
     (%string-replace-impl s1 s2 i j start2 end2))))

(define (%string-tokenize-impl criterion s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (acc '()))
      (cond ((>= i b) (reverse acc))
            (else
             ;; Find first index >= i where criterion matches (start of token).
             (let ((token-start (%string-index s criterion i b)))
               (cond ((not token-start) (reverse acc))
                     (else
                      ;; Find first index >= token-start where criterion does not match.
                      (let ((token-end (or (%string-skip s criterion token-start b) b)))
                        (loop token-end
                              (cons (substring s token-start token-end) acc)))))))))))

(define string-tokenize
  (case-lambda
    ((s)
     "Split S into a list of tokens. CRITERION identifies which characters
BELONG to a token (not which characters separate them); a token is a
maximal run of characters satisfying the criterion. Empty fields between
adjacent separators are NOT produced (this is the SRFI-13 sense, distinct
from `string-split').

CRITERION is a char (compared via char=?), a char-set (SRFI-14), or a
predicate procedure of one argument. The default is char-set:graphic
(SRFI-14): a token is a maximal run of graphic characters, so all
whitespace AND non-graphic control characters separate tokens.

Examples:
  (string-tokenize \"hello world\")              => (\"hello\" \"world\")
  (string-tokenize \"  many   spaces  \")        => (\"many\" \"spaces\")
  (string-tokenize \"\")                          => ()
  (string-tokenize \"a,b,c\" (lambda (ch) (not (char=? ch #\\,))))
    => (\"a\" \"b\" \"c\")

Parameters:
  s : string
  criterion : char, char-set (SRFI-14), or procedure (optional, default char-set:graphic)
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: list of strings
Category: srfi-13
Keywords: tokenize, split, words, fields, parse

See also: `string-split', `string-index', `string-skip'."
     (%string-tokenize-impl char-set:graphic s 0 (string-length s)))
    ((s criterion)
     (%string-tokenize-impl criterion s 0 (string-length s)))
    ((s criterion start)
     (%string-tokenize-impl criterion s start (string-length s)))
    ((s criterion start end)
     (%string-tokenize-impl criterion s start end))))

(define (%string-filter-impl criterion s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (acc '()))
      (cond ((>= i b) (list->string (reverse acc)))
            (else
             (let ((ch (string-ref s i)))
               (cond ((%match-char? criterion ch)
                      (loop (+ i 1) (cons ch acc)))
                     (else (loop (+ i 1) acc)))))))))

(define (%string-delete-impl criterion s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (acc '()))
      (cond ((>= i b) (list->string (reverse acc)))
            (else
             (let ((ch (string-ref s i)))
               (cond ((%match-char? criterion ch)
                      (loop (+ i 1) acc))
                     (else (loop (+ i 1) (cons ch acc))))))))))

(define string-filter
  (case-lambda
    ((criterion s)
     "Return a fresh string consisting of the chars of S that match
CRITERION (in order).

Examples:
  (string-filter char-numeric? \"abc123def\")  => \"123\"
  (string-filter #\\a \"banana\")                => \"aaa\"

Parameters:
  criterion : char, char-set (SRFI-14), or procedure
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: filter, keep, select, retain, predicate

See also: `string-delete', `string-count'."
     (%string-filter-impl criterion s 0 (string-length s)))
    ((criterion s start)
     (%string-filter-impl criterion s start (string-length s)))
    ((criterion s start end)
     (%string-filter-impl criterion s start end))))

(define string-delete
  (case-lambda
    ((criterion s)
     "Return a fresh string consisting of the chars of S that do NOT
match CRITERION (in order).

Examples:
  (string-delete char-numeric? \"abc123def\")  => \"abcdef\"
  (string-delete #\\a \"banana\")                => \"bnn\"

Parameters:
  criterion : char, char-set (SRFI-14), or procedure
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: delete, drop, remove, exclude, predicate

See also: `string-filter'."
     (%string-delete-impl criterion s 0 (string-length s)))
    ((criterion s start)
     (%string-delete-impl criterion s start (string-length s)))
    ((criterion s start end)
     (%string-delete-impl criterion s start end))))
