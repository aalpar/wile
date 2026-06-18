;; case.scm -- SRFI-13 mutating case forms
;; Part of SRFI 13: String Library
;;
;; Note on Unicode: R7RS `string-upcase' (the non-mutating form, in
;; (scheme char)) performs FULL Unicode case mapping which can change
;; the string's character count -- e.g. German #\latin_small_letter_sharp_s
;; ('ß') uppercases to "SS", lengthening the string by one. The mutating
;; bang forms below cannot change the string's length, so they fall
;; back to SIMPLE (single-char) case mapping via (char-upcase ch). For
;; ASCII inputs this is identical to the non-bang form. Programs that
;; need full Unicode case mapping must use the non-bang R7RS forms.

(define (%string-upcase!-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((>= i b) (if #f #f))
            (else
             (string-set! s i (char-upcase (string-ref s i)))
             (loop (+ i 1)))))))

(define (%string-downcase!-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (let loop ((i a))
      (cond ((>= i b) (if #f #f))
            (else
             (string-set! s i (char-downcase (string-ref s i)))
             (loop (+ i 1)))))))

(define string-upcase!
  (case-lambda
    ((s)
     "Mutate S in place, replacing each char in [start, end) with
(char-upcase ch). Returns unspecified.

Uses simple (per-char) case mapping rather than R7RS string-upcase's
full Unicode mapping. Differences appear only on cross-codepoint
mappings such as 'ß' -> 'SS' which would change the string length.

Examples:
  (let ((s (string-copy \"hello\")))
    (string-upcase! s)
    s)
  => \"HELLO\"
  (let ((s (string-copy \"hello\")))
    (string-upcase! s 1 4)
    s)
  => \"hELLo\"

Parameters:
  s : mutable string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: unspecified
Category: srfi-13
Keywords: upcase, uppercase, mutate, in-place, simple-case-mapping

See also: `string-upcase' (R7RS, non-mutating, full Unicode mapping),
          `string-downcase!'."
     (%string-upcase!-impl s 0 (string-length s)))
    ((s start)
     (%string-upcase!-impl s start (string-length s)))
    ((s start end)
     (%string-upcase!-impl s start end))))

(define string-downcase!
  (case-lambda
    ((s)
     "Mutate S in place, replacing each char in [start, end) with
(char-downcase ch). Returns unspecified.

Uses simple (per-char) case mapping rather than R7RS string-downcase's
full Unicode mapping (see `string-upcase!' note).

Examples:
  (let ((s (string-copy \"HELLO\")))
    (string-downcase! s)
    s)
  => \"hello\"

Parameters:
  s : mutable string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: unspecified
Category: srfi-13
Keywords: downcase, lowercase, mutate, in-place, simple-case-mapping

See also: `string-downcase' (R7RS, non-mutating), `string-upcase!'."
     (%string-downcase!-impl s 0 (string-length s)))
    ((s start)
     (%string-downcase!-impl s start (string-length s)))
    ((s start end)
     (%string-downcase!-impl s start end))))
