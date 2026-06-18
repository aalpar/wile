;; fold.scm -- SRFI-13 fold/map/for-each-index
;; Part of SRFI 13: String Library
;;
;; SRFI-13 string-map shadows R7RS string-map within (srfi 13) scope:
;; the SRFI-13 form takes optional [start [end]] arguments and a single
;; string, while R7RS string-map is variadic over multiple strings.
;; (wile strings) resolves the conflict by importing (scheme base) with
;; (except (scheme base) string-map) and re-exporting the SRFI-13 form
;; as the canonical string-map. Programs needing R7RS multi-string
;; string-map should import from (scheme base) directly.

(define (%string-fold-impl kons knil s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a) (acc knil))
      (cond ((>= i b) acc)
            (else (loop (+ i 1) (kons (string-ref s i) acc)))))))

(define (%string-fold-right-impl kons knil s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i (- b 1)) (acc knil))
      (cond ((< i a) acc)
            (else (loop (- i 1) (kons (string-ref s i) acc)))))))

(define (%string-for-each-index-impl proc s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((>= i b) (if #f #f))
            (else
             (proc i)
             (loop (+ i 1)))))))

(define (%string-map-impl proc s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i (- b 1)) (acc '()))
      (cond ((< i a) (list->string acc))
            (else (loop (- i 1) (cons (proc (string-ref s i)) acc)))))))

(define string-fold
  (case-lambda
    ((kons knil s)
     "Left-fold KONS over the chars of S, starting from initial value KNIL.
Each step computes (KONS char acc) and threads the result as the new acc.

Examples:
  (string-fold (lambda (ch acc) (cons ch acc)) '() \"abc\")
    => (#\\c #\\b #\\a)   ; reverse via fold
  (string-fold + 0 \"\")  => 0

Parameters:
  kons : procedure -- (kons char acc) -> any
  knil : any -- initial accumulator
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: any (the final accumulator)
Category: srfi-13
Keywords: fold, reduce, accumulate, leftward, foldl

See also: `string-fold-right', `string-for-each'."
     (%string-fold-impl kons knil s 0 (string-length s)))
    ((kons knil s start)
     (%string-fold-impl kons knil s start (string-length s)))
    ((kons knil s start end)
     (%string-fold-impl kons knil s start end))))

(define string-fold-right
  (case-lambda
    ((kons knil s)
     "Right-fold KONS over the chars of S, starting from initial value KNIL.

Examples:
  (string-fold-right cons '() \"abc\")  => (#\\a #\\b #\\c)

Parameters: see `string-fold'.
Returns: any
Category: srfi-13
Keywords: fold, reduce, accumulate, rightward, foldr

See also: `string-fold', `string-for-each'."
     (%string-fold-right-impl kons knil s 0 (string-length s)))
    ((kons knil s start)
     (%string-fold-right-impl kons knil s start (string-length s)))
    ((kons knil s start end)
     (%string-fold-right-impl kons knil s start end))))

(define string-for-each-index
  (case-lambda
    ((proc s)
     "Apply PROC to each integer index of S in left-to-right order.
Returns unspecified. Useful when PROC needs the index, not the char.

Examples:
  (let ((acc '()))
    (string-for-each-index (lambda (i) (set! acc (cons i acc))) \"abc\")
    (reverse acc))
  => (0 1 2)

Parameters:
  proc : procedure -- (proc i) called for side effects
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: unspecified
Category: srfi-13
Keywords: for-each, iterate, index, side-effect

See also: `string-for-each', `string-fold'."
     (%string-for-each-index-impl proc s 0 (string-length s)))
    ((proc s start)
     (%string-for-each-index-impl proc s start (string-length s)))
    ((proc s start end)
     (%string-for-each-index-impl proc s start end))))

(define string-map
  (case-lambda
    ((proc s)
     "Apply PROC to each char of S, returning a new string.

This is the SRFI-13 form: a single-string mapping with optional
[start [end]] range arguments. R7RS provides a variadic `string-map'
that maps over multiple strings simultaneously; the two forms are
distinct. Within (srfi 13) scope, the SRFI-13 form shadows R7RS.

(wile strings) imports (scheme base) with `(except ... string-map)'
and re-exports the SRFI-13 form. Programs needing R7RS multi-string
string-map should import (scheme base) directly.

Examples:
  (string-map char-upcase \"hello\")          => \"HELLO\"
  (string-map char-upcase \"hello\" 1 4)      => \"ELL\"

Parameters:
  proc : procedure -- (proc char) -> char
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string
Category: srfi-13
Keywords: map, transform, apply, char-by-char

See also: `string-fold', `string-tabulate'."
     (%string-map-impl proc s 0 (string-length s)))
    ((proc s start)
     (%string-map-impl proc s start (string-length s)))
    ((proc s start end)
     (%string-map-impl proc s start end))))
