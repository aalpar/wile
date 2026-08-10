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

(define (%string-upcase-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (%r7rs-string-upcase (string-copy s a b))))

(define (%string-downcase-impl s start end)
  (let-values (((a b) (%string-range-check s start end)))
    (%r7rs-string-downcase (string-copy s a b))))

(define string-upcase
  (case-lambda
    ((s)
     "Return a fresh string holding the chars of S in [start, end) upcased.

This is the SRFI-13 form: a single string with an optional [start [end]]
range. R7RS's string-upcase takes exactly one argument; the two are
distinct, and within (srfi 13) scope the SRFI-13 form shadows R7RS.
Importing (scheme char) alongside (srfi 13) therefore needs
(except (scheme char) string-upcase string-downcase).

The no-range call delegates to R7RS string-upcase, so it keeps FULL
Unicode case mapping and can change the string's length (German 'ß'
uppercases to \"SS\"). The ranged calls upcase a copy of the substring, so
they can too — unlike string-upcase!, which cannot change a string's
length and so falls back to simple per-char mapping.

Examples:
  (string-upcase \"hello\")      => \"HELLO\"
  (string-upcase \"hello\" 1 4)  => \"ELL\"

Parameters:
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string (fresh)
Category: srfi-13
Keywords: upcase, uppercase, case, range

See also: `string-upcase!' (mutating, simple mapping), `string-downcase',
          `string-map'."
     (%r7rs-string-upcase s))
    ((s start)
     (%string-upcase-impl s start (string-length s)))
    ((s start end)
     (%string-upcase-impl s start end))))

(define string-downcase
  (case-lambda
    ((s)
     "Return a fresh string holding the chars of S in [start, end) downcased.

The SRFI-13 form of R7RS's one-argument string-downcase, with an optional
[start [end]] range; see `string-upcase' for the shadowing note and for
why the no-range call keeps full Unicode mapping.

Examples:
  (string-downcase \"HELLO\")      => \"hello\"
  (string-downcase \"HELLO\" 1 4)  => \"ell\"

Parameters:
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string (fresh)
Category: srfi-13
Keywords: downcase, lowercase, case, range

See also: `string-downcase!' (mutating, simple mapping), `string-upcase'."
     (%r7rs-string-downcase s))
    ((s start)
     (%string-downcase-impl s start (string-length s)))
    ((s start end)
     (%string-downcase-impl s start end))))

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

(define (%string-titlecase-impl s start end)
  ;; Non-mutating: build a fresh string. A "word" is a maximal run of
  ;; cased characters; the first cased char of each run is uppercased and
  ;; the remaining cased chars are downcased. Non-cased chars (spaces,
  ;; punctuation, digits) reset the in-word state. Wile lacks a
  ;; char-titlecase primitive, so we approximate titlecase with
  ;; char-upcase -- identical for ASCII and for all characters whose
  ;; titlecase and uppercase mappings coincide.
  (let-values (((a b) (%string-range-check s start end)))
    (let ((out (string-copy s)))
      (let loop ((i a) (in-word #f))
        (cond ((>= i b) out)
              (else
               (let ((ch (string-ref s i)))
                 (cond
                  ((char-alphabetic? ch)
                   (string-set! out i
                                (if in-word
                                    (char-downcase ch)
                                    (char-upcase ch)))
                   (loop (+ i 1) #t))
                  (else
                   ;; preserve the original char, end the current word
                   (loop (+ i 1) #f))))))))))

(define string-titlecase
  (case-lambda
    ((s)
     "Return a fresh copy of S with each word titlecased: the first cased
character of every word is uppercased and the remaining cased characters
of that word are downcased. A word is a maximal run of alphabetic
characters; any non-alphabetic character ends the current word and is
copied unchanged.

Uses simple (per-char) case mapping. Wile has no char-titlecase
primitive, so the leading char is upcased via char-upcase; this is
identical to true titlecase for ASCII and for every character whose
titlecase and uppercase mappings coincide.

Examples:
  (string-titlecase \"hello world\")        => \"Hello World\"
  (string-titlecase \"--capitalize THIS!\")  => \"--Capitalize This!\"
  (string-titlecase \"hello world\" 6)       => \"hello World\"
  (string-titlecase \"\")                     => \"\"

Parameters:
  s : string
  start : integer (optional, default 0)
  end : integer (optional, default (string-length s))
Returns: string (fresh)
Category: srfi-13
Keywords: titlecase, capitalize, words, case

See also: `string-upcase' (R7RS), `string-downcase' (R7RS)."
     (%string-titlecase-impl s 0 (string-length s)))
    ((s start)
     (%string-titlecase-impl s start (string-length s)))
    ((s start end)
     (%string-titlecase-impl s start end))))

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
