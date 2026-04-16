;; SRFI-132 Sort Libraries integration tests
;; Tests all 20 exported procedures from (srfi 132)

(import (scheme base) (scheme write) (srfi 132) (chibi test))

(test-begin "SRFI-132")

;; ============================================================
;; Predicates
;; ============================================================

(test-begin "predicates")

;; list-sorted?
(test-begin "list-sorted?")
(test #t (list-sorted? < '()))
(test #t (list-sorted? < '(42)))
(test #t (list-sorted? < '(1 2 3 4 5)))
(test #f (list-sorted? < '(1 3 2 4 5)))
(test #t (list-sorted? > '(5 4 3 2 1)))
(test #t (list-sorted? < '(1 1 2 3)))  ;; equal neighbors: not less, so sorted
(test-end "list-sorted?")

;; vector-sorted?
(test-begin "vector-sorted?")
(test #t (vector-sorted? < #()))
(test #t (vector-sorted? < #(42)))
(test #t (vector-sorted? < #(1 2 3 4 5)))
(test #f (vector-sorted? < #(1 3 2 4 5)))
(test #t (vector-sorted? > #(5 4 3 2 1)))
;; subrange: #(5 1 2 3) indices 1..4 is (1 2 3), sorted
(test #t (vector-sorted? < #(5 1 2 3) 1 4))
;; subrange: #(1 3 2 4) indices 0..2 is (1 3), sorted
(test #t (vector-sorted? < #(1 3 2 4) 0 2))
;; subrange: #(1 3 2 4) indices 1..3 is (3 2), not sorted
(test #f (vector-sorted? < #(1 3 2 4) 1 3))
(test-end "vector-sorted?")

;; equal neighbors: all-same list
(test #t (list-sorted? < '(3 3 3 3)))
;; two elements
(test #t (list-sorted? < '(1 2)))
(test #f (list-sorted? < '(2 1)))

;; vector-sorted? with start only (2-arg optional)
(test #t (vector-sorted? < #(9 1 2 3) 1))

(test-end "predicates")

;; ============================================================
;; List sort
;; ============================================================

(test-begin "list-sort")

(test '() (list-sort < '()))
(test '(42) (list-sort < '(42)))
(test '(1 2 3 4 5) (list-sort < '(1 2 3 4 5)))
(test '(1 2 3 4 5) (list-sort < '(5 4 3 2 1)))
(test '(1 1 2 3 3 4 5) (list-sort < '(3 1 4 1 5 3 2)))
(test '(3 2 1) (list-sort > '(1 2 3)))

;; large list
(let ((big (let loop ((i 100) (acc '()))
             (if (= i 0) acc (loop (- i 1) (cons i acc))))))
  (test #t (list-sorted? < (list-sort < big))))

;; stability: sort pairs by car, equal keys retain original index order
(let* ((input '((1 . 0) (3 . 1) (1 . 2) (2 . 3) (3 . 4) (1 . 5)))
       (sorted (list-sort (lambda (a b) (< (car a) (car b))) input)))
  (test '((1 . 0) (1 . 2) (1 . 5) (2 . 3) (3 . 1) (3 . 4)) sorted))

;; non-mutation: original list unchanged
(let ((orig (list 3 1 4 1 5)))
  (let ((sorted (list-sort < orig)))
    (test '(3 1 4 1 5) orig)
    (test '(1 1 3 4 5) sorted)))

;; list-stable-sort alias
(test '(1 2 3) (list-stable-sort < '(3 2 1)))

;; Mixed ascending/descending runs exercise %collect-runs fully:
;; (3 2 1) descending, (4 5 6) ascending, (2 1) descending
(test '(1 1 2 2 3 4 5 6) (list-sort < '(3 2 1 4 5 6 2 1)))

;; All-equal input: single ascending run, no reversal
(test '(7 7 7 7) (list-sort < '(7 7 7 7)))

;; Two elements (smallest merge-pairs case)
(test '(1 2) (list-sort < '(2 1)))

;; Alternating high-low pattern exercises many short runs
(test '(1 2 3 4 5 6 7 8) (list-sort < '(2 1 4 3 6 5 8 7)))

(test-end "list-sort")

;; ============================================================
;; List sort!
;; ============================================================

(test-begin "list-sort!")

(test '() (list-sort! < (list)))
(test '(1 2 3) (list-sort! < (list 3 2 1)))
(test '(1 1 2 3 4 5) (list-sort! < (list 3 1 4 1 5 2)))

;; list-stable-sort! alias
(test '(1 2 3) (list-stable-sort! < (list 3 1 2)))

;; sort! on already-sorted (single run, no merging)
(test '(1 2 3 4 5) (list-sort! < (list 1 2 3 4 5)))

;; sort! on descending (single reversed run)
(test '(1 2 3 4 5) (list-sort! < (list 5 4 3 2 1)))

(test-end "list-sort!")

;; ============================================================
;; Vector sort
;; ============================================================

(test-begin "vector-sort")

(test #() (vector-sort < #()))
(test #(42) (vector-sort < #(42)))
(test #(1 2 3 4 5) (vector-sort < #(5 4 3 2 1)))
(test #(1 1 2 3 4 5) (vector-sort < #(3 1 4 1 5 2)))

;; subrange: sort only indices 1..4 of #(9 3 1 4 7)
(test #(1 3 4) (vector-sort < #(9 3 1 4 7) 1 4))

;; non-mutation
(let ((orig #(3 1 4 1 5)))
  (let ((sorted (vector-sort < orig)))
    (test #(3 1 4 1 5) orig)
    (test #(1 1 3 4 5) sorted)))

;; stability: sort tagged pairs by car
(let* ((input #((1 . 0) (3 . 1) (1 . 2) (2 . 3) (3 . 4)))
       (sorted (vector-sort (lambda (a b) (< (car a) (car b))) input)))
  (test #((1 . 0) (1 . 2) (2 . 3) (3 . 1) (3 . 4)) sorted))

;; vector-stable-sort alias
(test #(1 2 3) (vector-stable-sort < #(3 2 1)))

;; Power-of-2 length (even merge passes → result in original buffer)
(test #(1 2 3 4) (vector-sort < #(4 3 2 1)))

;; Non-power-of-2 length (odd merge passes → copy-back from temp)
(test #(1 2 3) (vector-sort < #(3 2 1)))
(test #(1 2 3 4 5) (vector-sort < #(5 4 3 2 1)))

;; Length 7: exercises the "one run left, copy as-is" branch in merge-runs
(test #(1 2 3 4 5 6 7) (vector-sort < #(7 6 5 4 3 2 1)))

;; All-equal vector
(test #(5 5 5 5) (vector-sort < #(5 5 5 5)))

;; Already sorted
(test #(1 2 3 4 5) (vector-sort < #(1 2 3 4 5)))

;; vector-sort with start-only arity
(test #(1 2 3) (vector-sort < #(9 3 1 2) 1))

(test-end "vector-sort")

;; ============================================================
;; Vector sort!
;; ============================================================

(test-begin "vector-sort!")

;; mutates in place
(let ((v (vector 5 3 1 4 2)))
  (vector-sort! < v)
  (test #(1 2 3 4 5) v))

;; subrange sort
(let ((v (vector 9 3 1 4 7)))
  (vector-sort! < v 1 4)
  (test #(9 1 3 4 7) v))

;; vector-stable-sort! alias
(let ((v (vector 3 1 2)))
  (vector-stable-sort! < v)
  (test #(1 2 3) v))

;; sort! subrange with start-only arity
(let ((v (vector 9 3 1 2)))
  (vector-sort! < v 1)
  (test #(9 1 2 3) v))

;; sort! on single element (base case, no temp allocation)
(let ((v (vector 42)))
  (vector-sort! < v)
  (test #(42) v))

;; sort! on empty (base case)
(let ((v (vector)))
  (vector-sort! < v)
  (test #() v))

(test-end "vector-sort!")

;; ============================================================
;; List merge
;; ============================================================

(test-begin "list-merge")

(test '(1 2 3 4 5 6) (list-merge < '(1 3 5) '(2 4 6)))
(test '(1 2) (list-merge < '(1 2) '()))
(test '(3 4) (list-merge < '() '(3 4)))
(test '() (list-merge < '() '()))

;; stability: equal elements from lis1 come first
;; Use tagged pairs: (1 . a) from lis1, (1 . b) from lis2
(let ((result (list-merge (lambda (a b) (< (car a) (car b)))
                          '((1 . a) (3 . a))
                          '((1 . b) (2 . b)))))
  (test '((1 . a) (1 . b) (2 . b) (3 . a)) result))

;; list-merge!
(test '(1 2 3 4 5 6) (list-merge! < (list 1 3 5) (list 2 4 6)))
(test '(1 2) (list-merge! < (list 1 2) '()))
(test '(3 4) (list-merge! < '() (list 3 4)))

;; merge! both empty
(test '() (list-merge! < '() '()))

;; merge interleaved: both branches of set-cdr! exercised
(test '(1 2 3 4) (list-merge! < (list 1 3) (list 2 4)))

;; merge with many duplicates (stability stress)
(test '(1 1 1 2 2 2) (list-merge < '(1 1 2 2) '(1 2)))

(test-end "list-merge")

;; ============================================================
;; Vector merge
;; ============================================================

(test-begin "vector-merge")

(test #(1 2 3 4 5 6) (vector-merge < #(1 3 5) #(2 4 6)))
(test #(1 2) (vector-merge < #(1 2) #()))
(test #(3 4) (vector-merge < #() #(3 4)))
(test #() (vector-merge < #() #()))

;; subranges: merge v1[1..4) with v2[1..4)
;; v1 = #(0 1 3 5 9), v2 = #(0 2 4 6 8) => merge (1 3 5) with (2 4 6)
(test #(1 2 3 4 5 6) (vector-merge < #(0 1 3 5 9) #(0 2 4 6 8) 1 4 1 4))

;; vector-merge! into target
(let ((target (make-vector 6 0)))
  (vector-merge! < target #(1 3 5) #(2 4 6))
  (test #(1 2 3 4 5 6) target))

;; vector-merge! with tstart offset
(let ((target (make-vector 8 0)))
  (vector-merge! < target #(1 3) #(2 4) 2)
  (test 0 (vector-ref target 0))
  (test 0 (vector-ref target 1))
  (test 1 (vector-ref target 2))
  (test 2 (vector-ref target 3))
  (test 3 (vector-ref target 4))
  (test 4 (vector-ref target 5)))

;; vector-merge stability: tagged pairs, equal keys from v1 first
(let ((result (vector-merge (lambda (a b) (< (car a) (car b)))
                            #((1 . a) (3 . a))
                            #((1 . b) (2 . b)))))
  (test #((1 . a) (1 . b) (2 . b) (3 . a)) result))

;; vector-merge! with subranges for both from1 and from2
(let ((target (make-vector 4 0)))
  (vector-merge! < target #(9 1 3 9) #(9 2 4 9) 0 1 3 1 3)
  (test #(1 2 3 4) target))

(test-end "vector-merge")

;; ============================================================
;; Neighbor dedup
;; ============================================================

(test-begin "dedup")

;; list-delete-neighbor-dups
(test-begin "list-delete-neighbor-dups")
(test '() (list-delete-neighbor-dups equal? '()))
(test '(1 2 3) (list-delete-neighbor-dups equal? '(1 2 3)))
(test '(1) (list-delete-neighbor-dups equal? '(1 1 1)))
(test '(1 2 3 4) (list-delete-neighbor-dups equal? '(1 1 2 3 3 3 4)))
(test '(7) (list-delete-neighbor-dups equal? '(7)))
;; non-consecutive duplicates are NOT removed
(test '(1 2 1) (list-delete-neighbor-dups equal? '(1 2 1)))
(test-end "list-delete-neighbor-dups")

;; list-delete-neighbor-dups!
(test-begin "list-delete-neighbor-dups!")
(test '(1 2 3 4) (list-delete-neighbor-dups! equal? (list 1 1 2 3 3 3 4)))
(test '() (list-delete-neighbor-dups! equal? (list)))
(test '(5) (list-delete-neighbor-dups! equal? (list 5)))
(test-end "list-delete-neighbor-dups!")

;; vector-delete-neighbor-dups
(test-begin "vector-delete-neighbor-dups")
(test #() (vector-delete-neighbor-dups equal? #()))
(test #(1 2 3 4) (vector-delete-neighbor-dups equal? #(1 1 2 3 3 3 4)))
(test #(1 2 3) (vector-delete-neighbor-dups equal? #(1 2 3)))
(test #(7) (vector-delete-neighbor-dups equal? #(7)))
;; subrange: #(9 1 1 2 2 3 9) indices 1..6 => (1 1 2 2 3) => dedup => #(1 2 3)
(test #(1 2 3) (vector-delete-neighbor-dups equal? #(9 1 1 2 2 3 9) 1 6))
(test-end "vector-delete-neighbor-dups")

;; vector-delete-neighbor-dups!
(test-begin "vector-delete-neighbor-dups!")
(test 0 (vector-delete-neighbor-dups! equal? #()))
(let ((v (vector 1 1 2 3 3 3 4)))
  (let ((end (vector-delete-neighbor-dups! equal? v)))
    (test 4 end)
    ;; compacted elements are in v[0..end)
    (test 1 (vector-ref v 0))
    (test 2 (vector-ref v 1))
    (test 3 (vector-ref v 2))
    (test 4 (vector-ref v 3))))
(let ((v (vector 1 2 3)))
  (test 3 (vector-delete-neighbor-dups! equal? v)))
(test-end "vector-delete-neighbor-dups!")

;; vector dedup with subrange: start-only arity
(test #(2 3) (vector-delete-neighbor-dups equal? #(1 1 2 3 3) 2))

;; vector dedup! on subrange
(let ((v (vector 0 1 1 2 2 0)))
  (let ((end (vector-delete-neighbor-dups! equal? v 1 5)))
    ;; compacted [1,2] in v[1..end), returns end=3
    (test 3 end)
    (test 1 (vector-ref v 1))
    (test 2 (vector-ref v 2))))

;; list dedup with custom equality
(test '("a" "B" "c")
      (list-delete-neighbor-dups string-ci=? '("a" "A" "B" "b" "c")))

(test-end "dedup")

;; ============================================================
;; Selection
;; ============================================================

(test-begin "select")

;; vector-select!: k=0 is minimum
(let ((v (vector 5 3 1 4 2)))
  (test 1 (vector-select! < v 0)))

;; vector-select!: k=n-1 is maximum
(let ((v (vector 5 3 1 4 2)))
  (test 5 (vector-select! < v 4)))

;; vector-select!: middle element
(let ((v (vector 5 3 1 4 2)))
  (test 3 (vector-select! < v 2)))

;; vector-select! with subrange
(let ((v (vector 9 8 7 6 5 4 3 2 1)))
  ;; subrange indices 2..7: elements (7 6 5 4 3), k=0 => min = 3
  (test 3 (vector-select! < v 0 2 7)))

;; vector-separate!: first k elements should be the k smallest
(let ((v (vector 5 3 1 4 2)))
  (vector-separate! < v 3)
  ;; first 3 positions contain {1,2,3} in some order
  (let ((front (list (vector-ref v 0) (vector-ref v 1) (vector-ref v 2))))
    (test '(1 2 3) (list-sort < front))))

;; vector-separate! with k=0 (no-op)
(let ((v (vector 5 3 1)))
  (vector-separate! < v 0)
  ;; vector should still contain same elements
  (test 3 (vector-length v)))

;; select on 2-element vector (minimal quickselect)
(test 1 (let ((v (vector 2 1))) (vector-select! < v 0)))
(test 2 (let ((v (vector 2 1))) (vector-select! < v 1)))

;; select with many duplicates (exercises the equal partition in partition3!)
(let ((v (vector 3 3 3 1 3 3)))
  (test 3 (vector-select! < v 5)))  ;; max is 3
(let ((v (vector 3 3 3 1 3 3)))
  (test 1 (vector-select! < v 0)))  ;; min is 1

;; separate with all-equal (entire vector is equal partition)
(let ((v (vector 5 5 5 5)))
  (vector-separate! < v 2)
  ;; all 5s, nothing to reorder
  (test #(5 5 5 5) v))

;; select on large vector (exercises recursion depth)
(let ((v (let ((v (make-vector 100)))
           (let loop ((i 0))
             (when (< i 100)
               (vector-set! v i (- 99 i))
               (loop (+ i 1))))
           v)))
  (test 0 (vector-select! < v 0))
  (test 99 (vector-select! < (vector-copy v) 99)))

(test-end "select")

;; ============================================================
;; Median
;; ============================================================

(test-begin "median")

;; empty vector returns knil
(test 42 (vector-find-median < #() 42))
(test 42 (vector-find-median! < (vector) 42))

;; singleton
(test 7 (vector-find-median < #(7) 0))
(test 7 (vector-find-median! < (vector 7) 0))

;; odd length: middle element
(test 3 (vector-find-median < #(3 1 4 1 5) 0))
(test 3 (vector-find-median! < (vector 3 1 4 1 5) 0))

;; even length with default mean (returns lesser of two middles)
;; elements sorted: 1 1 3 4 5 9, two middles: 3 and 4, default returns 3
(test 3 (vector-find-median < #(3 1 4 1 5 9) 0))
(test 3 (vector-find-median! < (vector 3 1 4 1 5 9) 0))

;; even length with custom arithmetic mean
(test 7/2 (vector-find-median < #(3 1 4 1 5 9) 0
                              (lambda (a b) (/ (+ a b) 2))))
(test 7/2 (vector-find-median! < (vector 3 1 4 1 5 9) 0
                               (lambda (a b) (/ (+ a b) 2))))

;; vector-find-median doesn't mutate
(let ((v #(5 3 1 4 2)))
  (let ((med (vector-find-median < v 0)))
    (test 3 med)
    (test #(5 3 1 4 2) v)))

;; find-median! returns correct answer
(test 3 (vector-find-median! < (vector 3 1 4 1 5) 0))

;; two elements: even length, exercises upper-middle scan
(test 1 (vector-find-median < #(2 1) 0))  ;; default mean: lesser
(test 3/2 (vector-find-median < #(2 1) 0
            (lambda (a b) (/ (+ a b) 2))))

;; even length with all-equal elements
(test 5 (vector-find-median < #(5 5 5 5) 0))

;; large odd-length vector
(let ((v (let ((v (make-vector 101)))
           (let loop ((i 0))
             (when (< i 101)
               (vector-set! v i (- 100 i))
               (loop (+ i 1))))
           v)))
  (test 50 (vector-find-median < v -1)))

(test-end "median")

;; ============================================================

(test-end "SRFI-132")

(test-exit)
