;; util.scm -- internal helpers (%char-set-walk-ranges)
;; Part of SRFI 14: Character-Set Library
;;
;; %char-set-walk-ranges is the single point where the inversion-list
;; representation (exposed via char-set-ranges from (wile charsets)) leaks
;; into the SRFI-14 Scheme layer. All iteration procedures in iteration.scm
;; build on this helper. If the FFI representation ever changes, exactly
;; one helper needs updating.

(define (%char-set-walk-ranges cs proc init)
  "Internal: walk every codepoint in CS in ascending order, folding PROC.
PROC receives each char and the current accumulator and returns the new
accumulator. Returns the final accumulator.

Parameters:
  cs : char-set
  proc : char × any → any
  init : any -- initial accumulator
Returns: any
Category: srfi-14
Keywords: walk, fold, ranges, internal, inversion-list"
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
