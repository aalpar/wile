;;; ansi-escape-test.scm - byte-level assertions on (chibi term ansi) escapes
;;;
;;; Four procedures emitted a RAW 0x1B byte followed by a stray ";" instead of
;;; the escaped "\x1B;[" form, so the string began ESC ";" — a complete two-byte
;;; private-use escape per ECMA-48. The CSI never formed and the remainder
;;; ("[38;5;196m") printed as literal text.
;;;
;;; The library was not untested: syntax_rules_dotted_tail_engine_test.go loads
;;; it transitively via (chibi diff). The gap was that nothing asserted on the
;;; escape BYTES, only on (string? ...). So these assertions index characters.
;;;
;;; WHAT THIS SUITE DOES NOT COVER: the remaining ~530 lines of the library.
;;; Everything outside the six escape emitters below still has no assertion of
;;; any kind, and the wrap procedures (rgb, rgb24, rgb-background,
;;; rgb24-background) are checked only indirectly through the escapes they call.

(import (scheme base) (chibi term ansi) (chibi test))

(test-begin "ansi-escape")

;; ── Every escape must begin with ESC then "[" ────────────────────
;;
;; Observed before the fix: the second character was #\; for all four of
;; rgb-escape, rgb24-escape, rgb-background-escape and rgb24-background-escape.

(define (first-two s)
  (list (string-ref s 0) (string-ref s 1)))

(test-group "CSI introducer is ESC then ["
  (test '(#\escape #\[) (first-two (rgb-escape 5 0 0)))
  (test '(#\escape #\[) (first-two (rgb24-escape 255 128 0)))
  (test '(#\escape #\[) (first-two (rgb-background-escape 0 0 5)))
  (test '(#\escape #\[) (first-two (rgb24-background-escape 0 0 128)))
  ;; The two that were already correct, kept so a future edit cannot regress
  ;; them silently while the four above are being watched.
  (test '(#\escape #\[) (first-two (gray-escape 12)))
  (test '(#\escape #\[) (first-two (gray-background-escape 12))))

;; ── Selector discipline: 5 is the 256-colour cube, 2 is truecolour ──
;;
;; The two are NOT interchangeable and the pairing is by colour depth, not by
;; foreground/background:
;;
;;   38;5;<index>      256-colour foreground   rgb-escape, gray-escape
;;   38;2;<r>;<g>;<b>  truecolour foreground   rgb24-escape
;;   48;5;<index>      256-colour background   rgb-background-escape, gray-background-escape
;;   48;2;<r>;<g>;<b>  truecolour background   rgb24-background-escape
;;
;; rgb24-background-escape emitted 48;5; — the 256-colour selector — while
;; feeding it three separate components. Its foreground twin rgb24-escape was
;; always correct at 38;2;.
;;
;; rgb-background-escape's 48;5; is CORRECT: it takes levels in [0,5] and emits
;; the single cube index (+ (* 36 r) (* 6 g) b 16). It must not be "fixed".

(define (contains? haystack needle)
  (let ((hn (string-length haystack))
        (nn (string-length needle)))
    (let loop ((i 0))
      (cond ((> (+ i nn) hn) #f)
            ((string=? (substring haystack i (+ i nn)) needle) #t)
            (else (loop (+ i 1)))))))

(test-group "256-colour procedures select with 5"
  (test #t (contains? (rgb-escape 5 0 0) "38;5;"))
  (test #t (contains? (gray-escape 12) "38;5;"))
  ;; Observed before the fix and after: 48;5;. Correct — do not change.
  (test #t (contains? (rgb-background-escape 0 0 5) "48;5;"))
  (test #t (contains? (gray-background-escape 12) "48;5;")))

(test-group "truecolour procedures select with 2"
  (test #t (contains? (rgb24-escape 255 128 0) "38;2;"))
  ;; Observed before the fix: "48;5;".
  (test #t (contains? (rgb24-background-escape 0 0 128) "48;2;"))
  (test #f (contains? (rgb24-background-escape 0 0 128) "48;5;")))

;; ── The cube index, so the 5-vs-2 assertions cannot pass vacuously ──

(test-group "256-colour index is the cube formula"
  ;; (+ (* 36 5) (* 6 0) 0 16) = 196
  (test #t (contains? (rgb-escape 5 0 0) "38;5;196m"))
  (test #t (contains? (rgb-background-escape 5 0 0) "48;5;196m")))

(test-group "truecolour components stay separate"
  (test #t (contains? (rgb24-escape 255 128 0) "38;2;255;128;0m"))
  (test #t (contains? (rgb24-background-escape 0 0 128) "48;2;0;0;128m")))

(test-end)
(test-exit)
