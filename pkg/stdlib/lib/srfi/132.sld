(define-library (srfi 132)
  (description "SRFI 132: Sort Libraries — sorting, merging, and related operations for lists and vectors.")
  (export
   ;; Predicates
   list-sorted? vector-sorted?
   ;; List sort
   list-sort list-stable-sort list-sort! list-stable-sort!
   ;; Vector sort
   vector-sort vector-stable-sort vector-sort! vector-stable-sort!
   ;; List merge
   list-merge list-merge!
   ;; Vector merge
   vector-merge vector-merge!
   ;; Neighbor dedup
   list-delete-neighbor-dups list-delete-neighbor-dups!
   vector-delete-neighbor-dups vector-delete-neighbor-dups!
   ;; Selection
   vector-select! vector-separate!
   ;; Median
   vector-find-median vector-find-median!)
  (import (scheme base) (scheme case-lambda))
  (include "132/predicates.scm"
           "132/list-merge.scm"
           "132/list-sort.scm"
           "132/vector-merge.scm"
           "132/vector-sort.scm"
           "132/dedup.scm"
           "132/select.scm"
           "132/median.scm"))
