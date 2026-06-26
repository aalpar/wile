;; SRFI-14 Phase 4: named char-sets integration tests.
;; The "consistency with built-in predicates" group is load-bearing per
;; design §10 — catches Q4 Unicode-reach decision being implemented
;; incorrectly. If char-set:letter were silently ASCII-only, every
;; consistency check on Greek/Cyrillic letters would fail loudly.

(import (scheme base) (chibi test) (srfi 14))

(test-begin "srfi-14-named-sets")

(test-group "basic membership"
  (test #t (char-set-contains? char-set:letter #\a))
  (test #t (char-set-contains? char-set:letter #\Z))
  (test #f (char-set-contains? char-set:letter #\1))
  (test #f (char-set-contains? char-set:letter #\space))

  (test #t (char-set-contains? char-set:lower-case #\a))
  (test #f (char-set-contains? char-set:lower-case #\A))
  (test #t (char-set-contains? char-set:upper-case #\A))
  (test #f (char-set-contains? char-set:upper-case #\a))

  (test #t (char-set-contains? char-set:digit #\5))
  (test #f (char-set-contains? char-set:digit #\a))

  (test #t (char-set-contains? char-set:hex-digit #\f))
  (test #t (char-set-contains? char-set:hex-digit #\F))
  (test #t (char-set-contains? char-set:hex-digit #\9))
  (test #f (char-set-contains? char-set:hex-digit #\g))

  (test #t (char-set-contains? char-set:whitespace #\space))
  (test #t (char-set-contains? char-set:whitespace #\tab))
  (test #t (char-set-contains? char-set:whitespace #\newline))
  (test #f (char-set-contains? char-set:whitespace #\a))

  (test #t (char-set-contains? char-set:iso-control (integer->char 9)))
  (test #f (char-set-contains? char-set:iso-control #\a)))

(test-group "fixed-size sets"
  (test 128    (char-set-size char-set:ascii))
  (test 0      (char-set-size char-set:empty))
  ;; full = every codepoint in [0, MaxCodepoint] EXCEPT the 2048 UTF-16
  ;; surrogates (U+D800..U+DFFF), which are not Unicode scalar values.
  (test (- #x110000 2048) (char-set-size char-set:full)))

(test-group "consistency with built-in predicates (load-bearing per design §10)"
  ;; Walk ASCII range 0..127 and verify cs membership matches the
  ;; corresponding Wile built-in predicate. Errors if any mismatch.
  (define (sample-ascii cs pred?)
    (let loop ((cp 0))
      (when (< cp 128)
        (if (eqv? (char-set-contains? cs (integer->char cp))
                  (pred? (integer->char cp)))
            (loop (+ cp 1))
            (error "consistency mismatch" cs cp pred?)))))

  (sample-ascii char-set:letter     char-alphabetic?)
  (sample-ascii char-set:digit      char-numeric?)
  (sample-ascii char-set:whitespace char-whitespace?)
  (sample-ascii char-set:lower-case char-lower-case?)
  (sample-ascii char-set:upper-case char-upper-case?)
  (test #t #t)) ;; If we reach here without error, the sweep passed.

(test-group "non-ASCII Unicode reach (Q4 design decision)"
  ;; Greek alpha should be in :letter
  (test #t (char-set-contains? char-set:letter (integer->char #x03B1)))  ; α
  ;; Cyrillic capital A should be in :upper-case
  (test #t (char-set-contains? char-set:upper-case (integer->char #x0410)))  ; А
  ;; Hebrew aleph is a letter but not lower or upper case
  (test #t (char-set-contains? char-set:letter (integer->char #x05D0)))  ; א
  (test #f (char-set-contains? char-set:lower-case (integer->char #x05D0)))
  (test #f (char-set-contains? char-set:upper-case (integer->char #x05D0))))

(test-end "srfi-14-named-sets")

(test-exit)
