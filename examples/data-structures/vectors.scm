;;; vectors.scm - Vector operations
;;;
;;; Demonstrates: Vectors, random access, mutation, vector operations
;;; Wile-specific: Full R7RS vector support
;;;
;;; Usage: ./dist/scheme --file examples/data-structures/vectors.scm

;; Vectors provide O(1) random access to elements.
;; Unlike lists, vectors have constant-time indexing and mutation.

(display "=== Vectors in Wile ===\n")
(newline)

;; Example 1: Creating vectors
(display "Example 1: Creating vectors\n")

(define v1 (vector 1 2 3 4 5))
(define v2 (make-vector 5 0))
(define v3 '#(a b c d))

(display "  (vector 1 2 3 4 5) = ")
(display v1)
(newline)
(display "  (make-vector 5 0) = ")
(display v2)
(newline)
(display "  '#(a b c d) = ")
(display v3)
(newline)
(newline)

;; Example 2: Accessing elements
(display "Example 2: Random access\n")

(display "  v1 = ")
(display v1)
(newline)
(display "  (vector-ref v1 0) = ")
(display (vector-ref v1 0))
(newline)
(display "  (vector-ref v1 2) = ")
(display (vector-ref v1 2))
(newline)
(display "  (vector-length v1) = ")
(display (vector-length v1))
(newline)
(newline)

;; Example 3: Mutation
(display "Example 3: Mutating vectors\n")

(define v4 (vector 10 20 30))
(display "  Before: ")
(display v4)
(newline)
(vector-set! v4 1 99)
(display "  After (vector-set! v4 1 99): ")
(display v4)
(newline)
(newline)

;; Example 4: Conversion between lists and vectors
(display "Example 4: List/vector conversion\n")

(define lst '(a b c d))
(define vec (list->vector lst))
(display "  List: ")
(display lst)
(newline)
(display "  As vector: ")
(display vec)
(newline)
(display "  Back to list: ")
(display (vector->list vec))
(newline)
(newline)

;; Example 5: Iterating over vectors
(display "Example 5: Vector iteration\n")

(define v5 '#(1 2 3 4 5))
(display "  Vector: ")
(display v5)
(newline)
(display "  Elements: ")
(do ((i 0 (+ i 1)))
    ((>= i (vector-length v5)))
  (display (vector-ref v5 i))
  (display " "))
(newline)
(newline)

;; Example 6: vector-map and vector-for-each
(display "Example 6: vector-map and vector-for-each\n")

(define v6 '#(1 2 3 4))
(display "  Original: ")
(display v6)
(newline)
(display "  Squared (vector-map): ")
(display (vector-map (lambda (x) (* x x)) v6))
(newline)
(display "  Print each (vector-for-each): ")
(vector-for-each
 (lambda (x)
   (display "[")
   (display x)
   (display "] "))
 v6)
(newline)
(newline)

;; Example 7: Dynamic array pattern
(display "Example 7: Growing a vector (dynamic array)\n")

(define (vector-append vec item)
  ;; Functional append - creates new vector
  (let* ((len (vector-length vec))
         (new-vec (make-vector (+ len 1))))
    (do ((i 0 (+ i 1)))
        ((>= i len))
      (vector-set! new-vec i (vector-ref vec i)))
    (vector-set! new-vec len item)
    new-vec))

(define v7 '#(1 2 3))
(display "  Original: ")
(display v7)
(newline)
(set! v7 (vector-append v7 4))
(display "  After appending 4: ")
(display v7)
(newline)
(set! v7 (vector-append v7 5))
(display "  After appending 5: ")
(display v7)
(newline)
(newline)

;; Example 8: Sorting vectors
(display "Example 8: Sorting a vector\n")

(define v8 (vector 5 2 8 1 9 3))
(display "  Original: ")
(display v8)
(newline)

;; Simple insertion sort (sort is not in R7RS-small)
(define (insert-sorted item lst less-than?)
  (cond
   ((null? lst) (list item))
   ((less-than? item (car lst)) (cons item lst))
   (else (cons (car lst) (insert-sorted item (cdr lst) less-than?)))))

(define (insertion-sort lst less-than?)
  (let loop ((remaining lst) (sorted '()))
    (if (null? remaining)
        sorted
        (loop (cdr remaining)
              (insert-sorted (car remaining) sorted less-than?)))))

;; Convert to list, sort, convert back
(define v8-sorted (list->vector (insertion-sort (vector->list v8) <)))
(display "  Sorted: ")
(display v8-sorted)
(newline)
(newline)

;; Example 9: Matrix representation
(display "Example 9: 2D matrix using vector of vectors\n")

(define (make-matrix rows cols init)
  (let ((matrix (make-vector rows)))
    (do ((i 0 (+ i 1)))
        ((>= i rows) matrix)
      (vector-set! matrix i (make-vector cols init)))))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref matrix row) col))

(define (matrix-set! matrix row col value)
  (vector-set! (vector-ref matrix row) col value))

(define m (make-matrix 3 3 0))
(matrix-set! m 0 0 1)
(matrix-set! m 1 1 2)
(matrix-set! m 2 2 3)

(display "  3×3 matrix (diagonal):\n")
(do ((i 0 (+ i 1)))
    ((>= i 3))
  (display "    ")
  (do ((j 0 (+ j 1)))
      ((>= j 3))
    (display (matrix-ref m i j))
    (display " "))
  (newline))
(newline)

;; Example 10: Binary search on sorted vector
(display "Example 10: Binary search\n")

(define (vector-binary-search vec value)
  (let loop ((low 0) (high (- (vector-length vec) 1)))
    (if (> low high)
        #f
        (let* ((mid (quotient (+ low high) 2))
               (mid-val (vector-ref vec mid)))
          (cond
           ((= mid-val value) mid)
           ((< mid-val value) (loop (+ mid 1) high))
           (else (loop low (- mid 1))))))))

(define sorted-vec '#(1 3 5 7 9 11 13 15 17 19))
(display "  Vector: ")
(display sorted-vec)
(newline)
(display "  Search for 7: index ")
(display (vector-binary-search sorted-vec 7))
(newline)
(display "  Search for 15: index ")
(display (vector-binary-search sorted-vec 15))
(newline)
(display "  Search for 8: ")
(display (vector-binary-search sorted-vec 8))
(newline)
(newline)

;; Example 11: Vector fill and copy
(display "Example 11: vector-fill! and vector-copy\n")

(define v9 (make-vector 5))
(display "  New vector: ")
(display v9)
(newline)
(vector-fill! v9 42)
(display "  After (vector-fill! v9 42): ")
(display v9)
(newline)

(define v10 (vector-copy v9))
(display "  Copy: ")
(display v10)
(newline)
(vector-set! v10 2 99)
(display "  After modifying copy: ")
(display v10)
(newline)
(display "  Original unchanged: ")
(display v9)
(newline)
(newline)

;; Example 12: Circular buffer
(display "Example 12: Circular buffer\n")

(define (make-circular-buffer size)
  (cons (make-vector size #f)
        (cons 0 0)))  ; (vector . (write-pos . count))

(define (cb-write! cb value)
  (let* ((vec (car cb))
         (write-pos (cadr cb))
         (count (cddr cb))
         (size (vector-length vec)))
    (vector-set! vec write-pos value)
    (set-car! (cdr cb) (modulo (+ write-pos 1) size))
    (set-cdr! (cdr cb) (min (+ count 1) size))))

(define (cb-read cb)
  (let* ((vec (car cb))
         (write-pos (cadr cb))
         (count (cddr cb))
         (size (vector-length vec))
         (read-pos (modulo (- write-pos count) size)))
    (if (= count 0)
        '()
        (let loop ((i 0) (result '()))
          (if (>= i count)
              (reverse result)
              (let ((idx (modulo (+ read-pos i) size)))
                (loop (+ i 1) (cons (vector-ref vec idx) result))))))))

(define cb (make-circular-buffer 3))
(display "  Buffer size 3:\n")
(cb-write! cb 'a)
(cb-write! cb 'b)
(cb-write! cb 'c)
(display "    After writing a,b,c: ")
(display (cb-read cb))
(newline)
(cb-write! cb 'd)
(display "    After writing d (overwrites a): ")
(display (cb-read cb))
(newline)
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Vectors provide:\n")
(display "  • O(1) random access via index\n")
(display "  • O(1) element mutation with vector-set!\n")
(display "  • Fixed size (grow by copying to new vector)\n")
(display "  • Better cache locality than lists\n")
(display "  • Use for: arrays, matrices, buffers, indexed data\n")
(newline)
(display "Choose vectors for random access, lists for sequential processing!\n")
