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

;;; -- 2D table helpers (vector of row-vectors, 0-based) --

(define (%make-2d rows cols init)
  (let ((m (make-vector rows)))
    (let loop ((i 0))
      (when (< i rows)
        (vector-set! m i (make-vector cols init))
        (loop (+ i 1))))
    m))

(define (%2d-ref m i j)
  (vector-ref (vector-ref m i) j))

(define (%2d-set! m i j x)
  (vector-set! (vector-ref m i) j x))

;;; -- Forest-distance recurrence (Zhang & Shasha 1989) --
;;;
;;; treedist(i, j): fill the forest-distance table FD for the keyroot pair
;;; (i, j) and write out tree-distances TD[di][dj] for every (di, dj) that sits
;;; at its own subtree root relative to (i, j). The cost closures are
;;; index-based: (relabel di dj), (delete di), (insert dj).
;;;
;;; FD is windowed to the absolute index range [l(i)-1 .. i] × [l(j)-1 .. j];
;;; row/col 0 of the window (absolute l-1) is the empty forest, seeded by
;;; cumulative delete / insert.

;; Returns (values fd roff coff): the windowed forest-distance table plus the
;; row/col offsets that map an absolute postorder index into the window. Also
;; writes the root-branch cells into the shared TD table as a side effect, so
;; the forward pass (which discards fd) and the backtracker (which walks it)
;; share one recurrence.
(define (%ted-forest-window t1 t2 i j td relabel delete insert)
  (let ((l1 (ted-tree-l t1))
        (l2 (ted-tree-l t2)))
    (let ((li (vector-ref l1 i))
          (lj (vector-ref l2 j)))
      (let ((roff (- li 1))                 ; absolute di → window row (di - roff)
            (coff (- lj 1))                  ; absolute dj → window col (dj - coff)
            (fd (%make-2d (+ (- i li) 2) (+ (- j lj) 2) 0)))
        (define (fd-ref a b)
          (%2d-ref fd (- a roff) (- b coff)))
        (define (fd-set! a b v)
          (%2d-set! fd (- a roff) (- b coff) v))
        ;; Base: empty forest vs empty forest, then cumulative delete / insert.
        (fd-set! (- li 1) (- lj 1) 0)
        (let loop ((di li))
          (when (<= di i)
            (fd-set! di (- lj 1) (+ (fd-ref (- di 1) (- lj 1)) (delete di)))
            (loop (+ di 1))))
        (let loop ((dj lj))
          (when (<= dj j)
            (fd-set! (- li 1) dj (+ (fd-ref (- li 1) (- dj 1)) (insert dj)))
            (loop (+ dj 1))))
        ;; Body: the three-way min. The keyroot branch condition
        ;; (l(di)=l(i) ∧ l(dj)=l(j)) decides relabel-vs-compose.
        (let oloop ((di li))
          (when (<= di i)
            (let iloop ((dj lj))
              (when (<= dj j)
                (let ((cost-del (+ (fd-ref (- di 1) dj) (delete di)))
                      (cost-ins (+ (fd-ref di (- dj 1)) (insert dj))))
                  (if (and (= (vector-ref l1 di) li)
                           (= (vector-ref l2 dj) lj))
                      ;; Both at subtree root → relabel/match the two roots,
                      ;; and this forest distance IS the tree distance.
                      (let ((m (min cost-del
                                    cost-ins
                                    (+ (fd-ref (- di 1) (- dj 1)) (relabel di dj)))))
                        (fd-set! di dj m)
                        (%2d-set! td di dj m))
                      ;; Otherwise compose: distance of the forest left of these
                      ;; subtrees, plus the (already computed) sub-tree distance.
                      (let ((m (min cost-del
                                    cost-ins
                                    (+ (fd-ref (- (vector-ref l1 di) 1)
                                               (- (vector-ref l2 dj) 1))
                                       (%2d-ref td di dj)))))
                        (fd-set! di dj m))))
                (iloop (+ dj 1))))
            (oloop (+ di 1))))
        (values fd roff coff)))))

(define (%ted-tree-distance-table t1 t2 relabel delete insert)
  "Fill the full TD[0..n1][0..n2] tree-distance table by running the
forest-distance recurrence over every (keyroot₁ × keyroot₂) pair, ascending.
Ascending order guarantees each composed sub-tree distance is ready when read.
TD[n1][n2] is the edit distance between the whole trees."
  (let ((n1 (ted-tree-n t1))
        (n2 (ted-tree-n t2)))
    (let ((td (%make-2d (+ n1 1) (+ n2 1) 0)))
      (for-each
        (lambda (i)
          (for-each
            (lambda (j)
              ;; fd window discarded here; only its TD side effects persist.
              (%ted-forest-window t1 t2 i j td relabel delete insert))
            (ted-tree-keyroots t2)))
        (ted-tree-keyroots t1))
      td)))

;;; -- Backtracking → node mapping --
;;;
;;; Recover the node correspondence by re-walking the forest windows. The full
;;; TD table is already populated, so each window is recomputed on demand and
;;; walked from its top-right corner toward the empty-forest base, picking the
;;; branch that achieved the minimum:
;;;   delete   → (node1 . #f)        insert  → (#f . node2)
;;;   relabel  → (node1 . node2)     compose → recurse into the matched subtree
;;; Branch order (delete, insert, then diagonal/compose) matches the canonical
;;; Zhang-Shasha recovery; any minimizer path yields a mapping whose summed
;;; cost equals the scalar distance.

(define (%ted-backtrack t1 t2 td relabel delete insert)
  (let ((l1 (ted-tree-l t1))
        (l2 (ted-tree-l t2))
        (nodes1 (ted-tree-nodes t1))
        (nodes2 (ted-tree-nodes t2))
        (acc '()))
    (define (emit! pair)
      (set! acc (cons pair acc)))
    (define (rec i j)
      (call-with-values
        (lambda () (%ted-forest-window t1 t2 i j td relabel delete insert))
        (lambda (fd roff coff)
          (define (fd-ref a b)
            (%2d-ref fd (- a roff) (- b coff)))
          (let ((li (vector-ref l1 i))
                (lj (vector-ref l2 j)))
            (let loop ((di i) (dj j))
              (cond
                ((and (< di li) (< dj lj))
                 #t)                          ; reached the empty forest
                ((and (>= di li)
                      (= (fd-ref di dj) (+ (fd-ref (- di 1) dj) (delete di))))
                 (emit! (cons (vector-ref nodes1 di) #f))
                 (loop (- di 1) dj))
                ((and (>= dj lj)
                      (= (fd-ref di dj) (+ (fd-ref di (- dj 1)) (insert dj))))
                 (emit! (cons #f (vector-ref nodes2 dj)))
                 (loop di (- dj 1)))
                ((and (= (vector-ref l1 di) li)
                      (= (vector-ref l2 dj) lj))
                 ;; relabel/match the two subtree roots
                 (emit! (cons (vector-ref nodes1 di) (vector-ref nodes2 dj)))
                 (loop (- di 1) (- dj 1)))
                (else
                 ;; compose: di,dj root a matched subtree — recurse, then jump
                 ;; the forest pointer left of both subtrees.
                 (rec di dj)
                 (loop (- (vector-ref l1 di) 1) (- (vector-ref l2 dj) 1)))))))))
    (rec (ted-tree-n t1) (ted-tree-n t2))
    (reverse acc)))

;;; -- Index-based cost closures (default unit costs) --
;;;
;;; The DP works in postorder indices; these closures translate to the
;;; user-visible nodes/labels. Default unit model: relabel = 0 when labels are
;;; equal else 1, delete = insert = 1. Phase 4 swaps in custom closures.

(define (%unit-relabel t1 t2 label-equal?)
  (let ((lab1 (ted-tree-labels t1))
        (lab2 (ted-tree-labels t2)))
    (lambda (di dj)
      (if (label-equal? (vector-ref lab1 di) (vector-ref lab2 dj))
          0
          1))))

(define (%unit-delete)
  (lambda (di)
    1))

(define (%unit-insert)
  (lambda (dj)
    1))

;;; -- Custom cost wrapping --
;;;
;;; User-supplied cost procedures operate on the visible NODES; the DP works in
;;; postorder indices, so each is wrapped to translate index → node. relabel-fn
;;; takes (node-a node-b); delete-fn / insert-fn take a single node.

(define (%wrap-relabel fn tt1 tt2)
  (let ((nodes1 (ted-tree-nodes tt1))
        (nodes2 (ted-tree-nodes tt2)))
    (lambda (di dj)
      (fn (vector-ref nodes1 di) (vector-ref nodes2 dj)))))

(define (%wrap-delete fn tt1)
  (let ((nodes1 (ted-tree-nodes tt1)))
    (lambda (di)
      (fn (vector-ref nodes1 di)))))

(define (%wrap-insert fn tt2)
  (let ((nodes2 (ted-tree-nodes tt2)))
    (lambda (dj)
      (fn (vector-ref nodes2 dj)))))

(define (%cost-alist? spec)
  (and (pair? spec)
       (every
         (lambda (e)
           (and (pair? e) (symbol? (car e)) (procedure? (cdr e))))
         spec)))

(define (%proc-list? spec)
  (and (pair? spec)
       (every procedure? spec)))

;; Resolve the `cost` option into the three index-based DP cost closures,
;; returned as (values relabel delete insert). SPEC is #f (unit default), an
;; alist ((relabel . fn) (insert . fn) (delete . fn)) with any subset present,
;; or a positional list (relabel-fn insert-fn delete-fn).
(define (%resolve-costs spec tt1 tt2 label-equal?)
  (cond
    ((not spec)
     (values (%unit-relabel tt1 tt2 label-equal?)
             (%unit-delete)
             (%unit-insert)))
    ((%cost-alist? spec)
     (let ((rl (assv-or spec 'relabel #f))
           (ins (assv-or spec 'insert #f))
           (del (assv-or spec 'delete #f)))
       (values (if rl (%wrap-relabel rl tt1 tt2) (%unit-relabel tt1 tt2 label-equal?))
               (if del (%wrap-delete del tt1) (%unit-delete))
               (if ins (%wrap-insert ins tt2) (%unit-insert)))))
    ((%proc-list? spec)
     (unless (= 3 (length spec))
       (error "tree-edit-distance: positional cost spec needs exactly 3 procedures"
              (list 'fix "pass (relabel-fn insert-fn delete-fn), or an alist ((relabel . fn) (insert . fn) (delete . fn))")
              spec))
     (values (%wrap-relabel (list-ref spec 0) tt1 tt2)
             (%wrap-delete (list-ref spec 2) tt1)
             (%wrap-insert (list-ref spec 1) tt2)))
    (else
     (error "tree-edit-distance: invalid cost spec"
            (list 'fix "pass (relabel-fn insert-fn delete-fn), or an alist ((relabel . fn) (insert . fn) (delete . fn))")
            spec))))

;;; -- Public entry --

(define (tree-edit-distance t1 t2 proto . opts)
  "Ordered tree edit distance between term trees T1 and T2 under PROTO.
Returns (values COST MAPPING): the minimum total cost of relabel / insert /
delete operations transforming T1 into T2, and the node correspondence
realizing it. Child order is significant (Zhang & Shasha 1989) — ASTs are
ordered, so (- a b) and (- b a) are distance 2 apart, not 0.

PROTO is a <term-protocol> (from (wile algebra rewrite)): a node's label is
(term-get-operator proto node) when compound, else the node itself; its
children are (term-get-operands proto node) when compound, else (). The same
trees that flow through AC-matching flow through edit distance.

MAPPING is an alist of node correspondences:
  (a . b)   a in T1 matched/relabeled to b in T2
  (a . #f)  a deleted from T1
  (#f . b)  b inserted into T2
The cost summed over MAPPING equals COST.

Options (trailing alist):
  (cost . SPEC)        override unit costs. SPEC is an alist
                       ((relabel . fn) (insert . fn) (delete . fn)) with any
                       subset present (missing ops keep unit cost), or a
                       positional list (relabel-fn insert-fn delete-fn).
                       relabel-fn takes (node-a node-b); insert-fn / delete-fn
                       take one node. Each returns a non-negative number.
  (label-equal? . fn)  equality used by the default relabel cost (0 when equal,
                       else 1). Defaults to equal?. Ignored when a custom
                       relabel cost is given.

The result is a metric only when the override is itself a metric; the default
unit costs are.

Examples:
  (define proto
    (make-term-protocol pair? car cdr
      (lambda (t a) (cons (car t) a)) (lambda (a b) #f)))
  (tree-edit-distance '(f a b) '(f a c) proto)   => 1, ((... ) (b . c) (a . a))
  (tree-edit-distance '(f a b) '(f b a) proto)   => 2  (ordered!)

Parameters:
  t1 : any
  t2 : any
  proto : term-protocol
  opts : alist
Returns: two values — a number and an alist
Category: algebra
Keywords: tree edit distance, Zhang-Shasha, ordered tree, AST diff, structural diff, edit script

See also: `make-term-protocol', `graph-maximum-common-subgraph'."
  (unless (term-protocol? proto)
    (error "tree-edit-distance: proto must be a term-protocol"
           (list 'fix "construct one with make-term-protocol from (wile algebra rewrite)")
           proto))
  (validate-opts-keys "tree-edit-distance" opts '(cost label-equal?))
  (let ((label-equal? (assv-or opts 'label-equal? equal?)))
    (assert-procedure "tree-edit-distance" label-equal?)
    (let ((tt1 (%ted-preprocess proto t1))
          (tt2 (%ted-preprocess proto t2)))
      (call-with-values
        (lambda ()
          (%resolve-costs (assv-or opts 'cost #f) tt1 tt2 label-equal?))
        (lambda (relabel delete insert)
          (let ((td (%ted-tree-distance-table tt1 tt2 relabel delete insert)))
            (values (%2d-ref td (ted-tree-n tt1) (ted-tree-n tt2))
                    (%ted-backtrack tt1 tt2 td relabel delete insert))))))))
