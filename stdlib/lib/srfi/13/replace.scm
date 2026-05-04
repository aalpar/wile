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
     "Concatenate STRINGS with DELIMITER between elements.

GRAMMAR controls how the delimiter is placed:
  infix         -- between elements; empty list -> \"\" (default)
  strict-infix  -- between elements; empty list -> error
  prefix        -- before each element
  suffix        -- after each element

Examples:
  (string-join '(\"a\" \"b\" \"c\") \",\")            => \"a,b,c\"
  (string-join '(\"a\" \"b\" \"c\"))                  => \"abc\"
  (string-join '() \",\")                          => \"\"
  (string-join '(\"a\" \"b\") \",\" 'prefix)         => \",a,b\"
  (string-join '(\"a\" \"b\") \",\" 'suffix)         => \"a,b,\"

Parameters:
  strings : list of strings
  delimiter : string (optional, default \"\")
  grammar : symbol (optional, default 'infix)
Returns: string
Category: srfi-13
Keywords: join, concatenate, delimiter, separator, glue

See also: `string-split', `string-concatenate'."
     (%string-join strings "" 'infix))
    ((strings delim)
     (%string-join strings delim 'infix))
    ((strings delim grammar)
     (%string-join strings delim grammar))))
