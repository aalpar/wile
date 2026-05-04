;; stdlib/lib/srfi/14/util.scm
;;
;; %char-set-walk-ranges is the SINGLE point where the inversion-list
;; representation (exposed via char-set-ranges from (wile charsets)) leaks
;; into the SRFI-14 Scheme layer. All iteration procedures in iteration.scm
;; build on this helper. If the FFI representation ever changes, exactly
;; one helper needs updating.

(define (%char-set-walk-ranges cs proc init)
  ;; proc :: char × accumulator → accumulator
  ;; Walks codepoints in canonical (ascending) order, calling proc once
  ;; per codepoint. Returns the final accumulator.
  (let loop ((ranges (char-set-ranges cs))
             (acc init))
    (if (null? ranges)
        acc
        (let* ((r (car ranges))
               (lo (car r))
               (hi (cdr r)))
          (let inner ((cp lo) (acc acc))
            (if (> cp hi)
                (loop (cdr ranges) acc)
                (inner (+ cp 1) (proc (integer->char cp) acc))))))))
