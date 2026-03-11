;;; records.scm - Record types with define-record-type
;;;
;;; Demonstrates: define-record-type, constructors, accessors, predicates
;;; Wile-specific: Full R7RS record support
;;;
;;; Usage: ./dist/wile --file examples/data-structures/records.scm

;; Records provide user-defined structured data types.
;; define-record-type creates a new disjoint type.

(display "=== Records in Wile ===\n")
(newline)

;; Example 1: Basic record definition
(display "Example 1: Defining a simple record type\n")

(define-record-type <point>
  (make-point x y)
  point?
  (x point-x)
  (y point-y))

(define p1 (make-point 3 4))

(display "  Created point: ")
(display "(")
(display (point-x p1))
(display ", ")
(display (point-y p1))
(display ")\n")
(display "  (point? p1) = ")
(display (point? p1))
(newline)
(display "  (point? 42) = ")
(display (point? 42))
(newline)
(newline)

;; Example 2: Mutable fields
(display "Example 2: Mutable record fields\n")

(define-record-type <person>
  (make-person name age)
  person?
  (name person-name)
  (age person-age set-person-age!))

(define alice (make-person "Alice" 30))

(display "  Initial: ")
(display (person-name alice))
(display ", age ")
(display (person-age alice))
(newline)

(set-person-age! alice 31)

(display "  After birthday: ")
(display (person-name alice))
(display ", age ")
(display (person-age alice))
(newline)
(newline)

;; Example 3: Computed fields via custom constructor
(display "Example 3: Custom constructor with validation\n")

(define-record-type <rectangle>
  (%make-rectangle width height)
  rectangle?
  (width rectangle-width)
  (height rectangle-height))

(define (make-rectangle width height)
  (when (or (<= width 0) (<= height 0))
    (error "Rectangle dimensions must be positive"))
  (%make-rectangle width height))

(define (rectangle-area rect)
  (* (rectangle-width rect) (rectangle-height rect)))

(define r1 (make-rectangle 5 3))
(display "  Rectangle 5×3:\n")
(display "    Width: ")
(display (rectangle-width r1))
(newline)
(display "    Height: ")
(display (rectangle-height r1))
(newline)
(display "    Area: ")
(display (rectangle-area r1))
(newline)
(newline)

;; Example 4: Nested records
(display "Example 4: Records containing other records\n")

(define-record-type <circle>
  (make-circle center radius)
  circle?
  (center circle-center)
  (radius circle-radius))

(define c1 (make-circle (make-point 0 0) 5))

(display "  Circle at origin with radius 5:\n")
(display "    Center: (")
(display (point-x (circle-center c1)))
(display ", ")
(display (point-y (circle-center c1)))
(display ")\n")
(display "    Radius: ")
(display (circle-radius c1))
(newline)
(newline)

;; Example 5: Binary tree using records
(display "Example 5: Binary tree implementation\n")

(define-record-type <tree>
  (make-tree value left right)
  tree?
  (value tree-value)
  (left tree-left)
  (right tree-right))

(define (leaf value)
  (make-tree value '() '()))

(define (tree-insert tree value)
  (if (null? tree)
      (leaf value)
      (if (< value (tree-value tree))
          (make-tree (tree-value tree)
                     (tree-insert (tree-left tree) value)
                     (tree-right tree))
          (make-tree (tree-value tree)
                     (tree-left tree)
                     (tree-insert (tree-right tree) value)))))

(define (tree-contains? tree value)
  (cond
   ((null? tree) #f)
   ((= value (tree-value tree)) #t)
   ((< value (tree-value tree)) (tree-contains? (tree-left tree) value))
   (else (tree-contains? (tree-right tree) value))))

(define (tree-inorder tree)
  (if (null? tree)
      '()
      (append (tree-inorder (tree-left tree))
              (list (tree-value tree))
              (tree-inorder (tree-right tree)))))

(define my-tree
  (tree-insert
   (tree-insert
    (tree-insert
     (tree-insert '() 5)
     3)
    7)
   1))

(display "  Tree contents (inorder): ")
(display (tree-inorder my-tree))
(newline)
(display "  (tree-contains? my-tree 3) = ")
(display (tree-contains? my-tree 3))
(newline)
(display "  (tree-contains? my-tree 99) = ")
(display (tree-contains? my-tree 99))
(newline)
(newline)

;; Example 6: Queue using records
(display "Example 6: Queue data structure\n")

(define-record-type <queue>
  (%make-queue front rear)
  queue?
  (front queue-front set-queue-front!)
  (rear queue-rear set-queue-rear!))

(define (make-queue)
  (%make-queue '() '()))

(define (queue-empty? q)
  (and (null? (queue-front q))
       (null? (queue-rear q))))

(define (enqueue! q item)
  (let ((new-pair (cons item '())))
    (cond
     ((queue-empty? q)
      (set-queue-front! q new-pair)
      (set-queue-rear! q new-pair))
     (else
      (set-cdr! (queue-rear q) new-pair)
      (set-queue-rear! q new-pair)))))

(define (dequeue! q)
  (if (queue-empty? q)
      (error "Queue underflow")
      (let ((result (car (queue-front q))))
        (set-queue-front! q (cdr (queue-front q)))
        (when (null? (queue-front q))
          (set-queue-rear! q '()))
        result)))

(define q (make-queue))
(display "  Created empty queue\n")
(enqueue! q 'a)
(enqueue! q 'b)
(enqueue! q 'c)
(display "  Enqueued: a b c\n")
(display "  Dequeue: ")
(display (dequeue! q))
(newline)
(display "  Dequeue: ")
(display (dequeue! q))
(newline)
(enqueue! q 'd)
(display "  Enqueued: d\n")
(display "  Dequeue: ")
(display (dequeue! q))
(newline)
(newline)

;; Example 7: Record equality
(display "Example 7: Record identity vs structural equality\n")

(define p2 (make-point 3 4))
(define p3 (make-point 3 4))

(display "  p1: (3, 4)\n")
(display "  p2: (3, 4) [different instance]\n")
(display "  p3: (3, 4) [different instance]\n")
(display "  (eq? p1 p2) = ")
(display (eq? p1 p2))
(display " (different objects)\n")

(define (point-equal? p1 p2)
  (and (point? p1)
       (point? p2)
       (= (point-x p1) (point-x p2))
       (= (point-y p1) (point-y p2))))

(display "  (point-equal? p1 p2) = ")
(display (point-equal? p1 p2))
(display " (same values)\n")
(newline)

;; Example 8: Stack using records
(display "Example 8: Stack data structure\n")

(define-record-type <stack>
  (make-stack-internal items)
  stack?
  (items stack-items set-stack-items!))

(define (make-stack)
  (make-stack-internal '()))

(define (stack-push! s item)
  (set-stack-items! s (cons item (stack-items s))))

(define (stack-pop! s)
  (if (null? (stack-items s))
      (error "Stack underflow")
      (let ((result (car (stack-items s))))
        (set-stack-items! s (cdr (stack-items s)))
        result)))

(define (stack-peek s)
  (if (null? (stack-items s))
      (error "Stack empty")
      (car (stack-items s))))

(define (stack-empty? s)
  (null? (stack-items s)))

(define stk (make-stack))
(display "  Created empty stack\n")
(stack-push! stk 1)
(stack-push! stk 2)
(stack-push! stk 3)
(display "  Pushed: 1 2 3\n")
(display "  Peek: ")
(display (stack-peek stk))
(newline)
(display "  Pop: ")
(display (stack-pop! stk))
(newline)
(display "  Pop: ")
(display (stack-pop! stk))
(newline)
(display "  Empty? ")
(display (stack-empty? stk))
(newline)
(newline)

;; Summary
(display "=== Summary ===\n")
(display "Records provide:\n")
(display "  • User-defined data types\n")
(display "  • Automatic constructor, predicate, accessors\n")
(display "  • Optional mutators for mutable fields\n")
(display "  • Disjoint types (unique identity)\n")
(display "  • Foundation for data structures (trees, queues, stacks)\n")
(newline)
(display "Use records for structured, typed data!\n")
