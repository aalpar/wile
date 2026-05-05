;; SRFI-14 predicates and basic char-set? smoke tests.
;; Phase 0 covers only char-set? (always #f).
;; Phases 1+ extend coverage.

(import (scheme base)
        (chibi test)
        (srfi 14))

(test-begin "srfi-14-predicates")

(test-group "char-set? smoke (phase 0)"
  (test "symbol"    #f (char-set? 'foo))
  (test "string"    #f (char-set? "abc"))
  (test "char"      #f (char-set? #\a))
  (test "null"      #f (char-set? '()))
  (test "integer"   #f (char-set? 42)))

(test-end "srfi-14-predicates")

(test-exit)
