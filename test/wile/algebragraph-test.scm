;;; algebragraph-test.scm — (wile algebragraph) FFI primitive tests

(import (scheme base)
        (chibi test)
        (wile algebragraph))

(test-begin "algebragraph")

;; --- count-paths-in-dag ---

(test-group "count-paths-in-dag"
  ;; Single node, no edges.
  (test #(1) (count-paths-in-dag 1 '() 0))

  ;; Linear chain 0 → 1 → 2 → 3: one path to each.
  (test #(1 1 1 1)
        (count-paths-in-dag 4 '((0 . 1) (1 . 2) (2 . 3)) 0))

  ;; Diamond DAG: two paths from 0 to 3.
  (test #(1 1 1 2)
        (count-paths-in-dag 4 '((0 . 1) (0 . 2) (1 . 3) (2 . 3)) 0))

  ;; Cyclic input — primitive returns #f to signal the kernel's
  ;; cycle-detection precondition failure (counting on cycles diverges).
  (test #f (count-paths-in-dag 2 '((0 . 1) (1 . 0)) 0))

  ;; Self-loop at source is a cycle.
  (test #f (count-paths-in-dag 1 '((0 . 0)) 0)))

;; --- count-paths-cyclic ---

;; Helper: destructure the 3-value return into a list for `test` equality.
(define (cyclic-result n edges src)
  (call-with-values
    (lambda () (count-paths-cyclic n edges src))
    list))

(test-group "count-paths-cyclic"
  ;; Single cycle 0 → 1 → 2 → 0: one non-trivial SCC, source count 1.
  (test '(#(0 0 0) #(1) #(#t))
        (cyclic-result 3 '((0 . 1) (1 . 2) (2 . 0)) 0))

  ;; Mutual recursion with parallel call sites:
  ;;   main(0) → f(1), main → g(2),
  ;;   f → helper(3) ×2, f → g, g → f, g → helper.
  ;; Three SCCs in reverse-topological order: {main} (id 0),
  ;; {f, g} (id 1, non-trivial), {helper} (id 2).
  ;; Counts: 1, 2 (entry count), 6 (= 2 × 3 inter-SCC edges).
  (let ((result (cyclic-result 4
                  '((0 . 1) (0 . 2) (1 . 3) (1 . 3)
                    (1 . 2) (2 . 1) (2 . 3))
                  0)))
    (test #(0 1 1 2) (car result))         ; SCC vector
    (test #(1 2 6)   (cadr result))        ; counts per SCC
    (test #(#f #t #f) (caddr result))))    ; non-trivial flags

;; --- Error contracts ---
;;
;; Mirror the Go-side TestCountPathsInDAG_ErrorOnInvalidInput and
;; TestCountPathsCyclic_ErrorOnInvalidInput tests at the Scheme
;; boundary. The Go primitives raise (not return #f) for invalid
;; input; the FFI bridge surfaces that as a Scheme exception. These
;; tests pin the user-visible contract: bad input raises, cyclic
;; input to count-paths-in-dag returns #f (the documented sentinel).

(test-group "count-paths-in-dag error contracts"
  ;; Non-integer num-nodes
  (test-error (count-paths-in-dag "two" '() 0))
  ;; Negative num-nodes
  (test-error (count-paths-in-dag -1 '() 0))
  ;; Source out of range
  (test-error (count-paths-in-dag 2 '() 5))
  ;; Malformed edge: cdr is not an integer
  (test-error (count-paths-in-dag 2 '((0 . "x")) 0))
  ;; Edge endpoint out of range
  (test-error (count-paths-in-dag 2 '((0 . 5)) 0)))

(test-group "count-paths-cyclic error contracts"
  (test-error (count-paths-cyclic "two" '() 0))
  (test-error (count-paths-cyclic -1 '() 0))
  (test-error (count-paths-cyclic 2 '() 5))
  (test-error (count-paths-cyclic 2 '((0 . 5)) 0)))

(test-end)
(test-exit)
