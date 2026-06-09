;;; (wile algebra tree) — Ordered tree edit distance (Zhang & Shasha 1989).
;;;
;;; The AST-level sibling of (wile algebra combinatorial-graph)'s
;;; maximum-common-subgraph. Where MCS finds the largest shared substructure
;;; of two unrooted graphs, tree edit distance measures how far apart two
;;; *ordered rooted labeled* trees are: the minimum-cost sequence of node
;;; relabel / insert / delete operations transforming T1 into T2.
;;;
;;; ASTs are ordered — (- a b) ≠ (- b a) — so child order is significant.
;;; The ordered case is polynomial (Zhang-Shasha); unordered tree edit
;;; distance is NP-hard and out of scope.
;;;
;;; Node navigation goes through the (wile algebra rewrite) <term-protocol>:
;;; a node's LABEL is (term-get-operator proto node) when compound, else the
;;; node itself; its CHILDREN are (term-get-operands proto node) when compound,
;;; else (). The same abstraction unification.scm / symbolic.scm already use.
;;;
;;; References:
;;;   Zhang, K. & Shasha, D. (1989). "Simple fast algorithms for the editing
;;;   distance between trees and related problems." SIAM J. Comput. 18(6).

;;; -- Preprocessed tree --
;;;
;;; A <ted-tree> is the Zhang-Shasha working representation of one input tree:
;;; every node numbered 1..n in left-to-right postorder, with the derived
;;; tables the forest-distance DP indexes into. Index 0 is an unused sentinel
;;; so all vectors are 1-based, matching the recurrence's FD[l-1][·] base case.
;;;
;;;   n        : node count
;;;   nodes    : vector 1..n — the original node at each postorder index
;;;   labels   : vector 1..n — (term-get-operator proto node) if compound else node
;;;   children : vector 1..n — list of child postorder indices, left-to-right
;;;   l        : vector 1..n — l(i), postorder index of i's leftmost-leaf descendant
;;;   keyroots : ascending list of LR-keyroot indices (the subtree roots the DP loops over)

(define-record-type <ted-tree>
  (%make-ted-tree n nodes labels children l keyroots)
  ted-tree?
  (n        ted-tree-n)
  (nodes    ted-tree-nodes)
  (labels   ted-tree-labels)
  (children ted-tree-children)
  (l        ted-tree-l)
  (keyroots ted-tree-keyroots))

(define (%tree-postorder proto t)
  "Number T's nodes 1..n in left-to-right postorder under PROTO.
Returns a <ted-tree> with n/nodes/labels/children filled and l/keyroots
left #f (filled by %tree-l and %tree-keyroots). Child order is preserved
exactly — this is what makes the distance ordered."
  (let ((counter 0)
        (nodes-acc '())     ; reverse-postorder accumulators (id n first)
        (labels-acc '())
        (children-acc '()))
    (define (label-of node)
      (if (term-compound? proto node)
          (term-get-operator proto node)
          node))
    (define (kids-of node)
      (if (term-compound? proto node)
          (term-get-operands proto node)
          '()))
    ;; Walk children strictly left-to-right: bind each child's id before
    ;; recursing on the rest, so postorder numbering does not depend on the
    ;; evaluation order of `map` (Wile is L→R, but this is order-explicit).
    (define (walk-children kids)
      (if (null? kids)
          '()
          (let ((first-id (walk (car kids))))
            (cons first-id (walk-children (cdr kids))))))
    (define (walk node)
      (let ((child-ids (walk-children (kids-of node))))
        (set! counter (+ counter 1))
        (set! nodes-acc (cons node nodes-acc))
        (set! labels-acc (cons (label-of node) labels-acc))
        (set! children-acc (cons child-ids children-acc))
        counter))
    (walk t)
    (let ((n counter)
          (nodes-vec (make-vector (+ counter 1) #f))
          (labels-vec (make-vector (+ counter 1) #f))
          (children-vec (make-vector (+ counter 1) '())))
      ;; Accumulators are in reverse postorder (highest id at the head).
      (let loop ((id counter) (ns nodes-acc) (ls labels-acc) (cs children-acc))
        (when (>= id 1)
          (vector-set! nodes-vec id (car ns))
          (vector-set! labels-vec id (car ls))
          (vector-set! children-vec id (car cs))
          (loop (- id 1) (cdr ns) (cdr ls) (cdr cs))))
      (%make-ted-tree n nodes-vec labels-vec children-vec #f #f))))

(define (%tree-l base)
  "Compute the l(i) leftmost-leaf-descendant table for preprocessed BASE.
l(leaf) = leaf; l(internal) = l(leftmost child). A single ascending sweep
suffices because every child's postorder index is smaller than its parent's,
so l(first-child) is already filled when node i is reached."
  (let ((n (ted-tree-n base))
        (children (ted-tree-children base)))
    (let ((l (make-vector (+ n 1) 0)))
      (let loop ((i 1))
        (when (<= i n)
          (let ((kids (vector-ref children i)))
            (vector-set! l i
              (if (null? kids)
                  i
                  (vector-ref l (car kids)))))
          (loop (+ i 1))))
      l)))

(define (%tree-keyroots l)
  "LR-keyroots from the l-table: { k : no k' > k has l(k') = l(k) }.
Equivalently the largest-id node per distinct l-value — always including the
root (id n). Returned ascending. These are the subtree roots the tree-distance
DP iterates over; the rest are covered as the forest-distance fall-through."
  (let ((n (- (vector-length l) 1)))
    (let ((seen (make-vector (+ n 1) #f))
          (acc '()))
      ;; Descend k from n: the first (largest) k with a given l-value is its keyroot.
      ;; Consing while descending leaves acc ascending.
      (let loop ((k n))
        (when (>= k 1)
          (let ((lk (vector-ref l k)))
            (unless (vector-ref seen lk)
              (vector-set! seen lk #t)
              (set! acc (cons k acc))))
          (loop (- k 1))))
      acc)))

(define (%ted-preprocess proto t)
  "Run the full Zhang-Shasha preprocessing pipeline on T under PROTO,
returning a complete <ted-tree> (n/nodes/labels/children/l/keyroots)."
  (let ((base (%tree-postorder proto t)))
    (let ((l (%tree-l base)))
      (%make-ted-tree (ted-tree-n base)
                      (ted-tree-nodes base)
                      (ted-tree-labels base)
                      (ted-tree-children base)
                      l
                      (%tree-keyroots l)))))

;;; -- Public entry (Phase 1: preprocessing only; DP lands in Phase 2) --

(define (tree-edit-distance t1 t2 proto . opts)
  (error "tree-edit-distance: not implemented"
         (list 'fix "forward DP lands in Phase 2")))
