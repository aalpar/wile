;;; schelog-zebra-bench.scm - Timed Schelog zebra puzzle benchmark
;;;
;;; Outputs "Total time: Xs" for compatibility with run-extended.sh.
;;; Usage: SCHEME_INCLUDE_PATH=. ./dist/wile --file examples/benchmarks/schelog-zebra-bench.scm

(include "examples/logic/schelog/schelog.scm")
(include "examples/logic/schelog/puzzle.scm")
(include "examples/logic/schelog/houses.scm")

(set! *schelog-use-occurs-check?* #t)

(let* ((start (current-jiffy))
       (result (solve-puzzle %houses))
       (end (current-jiffy))
       (elapsed (exact->inexact (/ (- end start) (jiffies-per-second)))))
  (display "Total time: ")
  (display elapsed)
  (display "s\n")
  elapsed)
