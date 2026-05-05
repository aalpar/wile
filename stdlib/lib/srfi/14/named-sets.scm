;; stdlib/lib/srfi/14/named-sets.scm
;;
;; SRFI-14 named character sets, built once at library load time from
;; Go's unicode.RangeTable values via %make-named-charset (FFI primitive
;; in extensions/charsets/charsets.go). Caching at the Go layer means
;; repeated lookups return the same eq? value.

(define char-set:letter        (%make-named-charset 'letter))
(define char-set:lower-case    (%make-named-charset 'lower-case))
(define char-set:upper-case    (%make-named-charset 'upper-case))
(define char-set:title-case    (%make-named-charset 'title-case))
(define char-set:digit         (%make-named-charset 'digit))
(define char-set:letter+digit  (%make-named-charset 'letter+digit))
(define char-set:graphic       (%make-named-charset 'graphic))
(define char-set:printing      (%make-named-charset 'printing))
(define char-set:whitespace    (%make-named-charset 'whitespace))
(define char-set:iso-control   (%make-named-charset 'iso-control))
(define char-set:punctuation   (%make-named-charset 'punctuation))
(define char-set:symbol        (%make-named-charset 'symbol))
(define char-set:hex-digit     (%make-named-charset 'hex-digit))
(define char-set:blank         (%make-named-charset 'blank))

;; Direct construction (no %make-named-charset call needed)
(define char-set:empty (char-set))
(define char-set:full  (ucs-range->char-set 0 #x110000))
(define char-set:ascii (ucs-range->char-set 0 128))
