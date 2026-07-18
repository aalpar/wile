;;; schelog-zebra-bench.scm - Timed Schelog zebra puzzle benchmark
;;;
;;; Outputs "Total time: Xs" for compatibility with run-extended.sh.
;;; Usage: SCHEME_INCLUDE_PATH=. ./dist/wile -q -i \
;;;            --file examples/benchmarks/schelog-zebra-bench.scm < /dev/null
;;;
;;; -i (mutable top level) is REQUIRED: schelog is unmodified upstream source
;;; that set!s its own top-level globals and redefines %append, both of which
;;; the immutable-top-level default rejects. `< /dev/null` feeds EOF so the
;;; REPL that -i leaves running exits 0 instead of waiting on stdin.

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
