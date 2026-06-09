;;; algebra-tree-test.scm — black-box tests for (wile algebra tree).
;;; Ordered tree edit distance (Zhang & Shasha 1989) via the public API.
;;; White-box preprocessing tests live in algebra-tree-internals-test.scm.

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra rewrite)
        (wile algebra tree))

;;; Built-in s-expression term protocol: (op c1 c2 ...) is a compound node
;;; with label op and children c1..; an atom is a leaf labeled by itself.
(define proto
  (make-term-protocol
    pair?
    car
    cdr
    (lambda (term new-args) (cons (car term) new-args))
    (lambda (a b) #f)))

;; Scalar distance only (drops the mapping).
(define (ted t1 t2)
  (call-with-values
    (lambda () (tree-edit-distance t1 t2 proto))
    (lambda (cost mapping) cost)))

;; Scalar distance with trailing options (cost spec, label-equal?, ...).
(define (ted-cost t1 t2 . opts)
  (call-with-values
    (lambda () (apply tree-edit-distance t1 t2 proto opts))
    (lambda (cost mapping) cost)))

;; Node mapping only (drops the scalar cost).
(define (ted-map t1 t2)
  (call-with-values
    (lambda () (tree-edit-distance t1 t2 proto))
    (lambda (cost mapping) mapping)))

;; Node mapping with trailing options.
(define (ted-map-opts t1 t2 . opts)
  (call-with-values
    (lambda () (apply tree-edit-distance t1 t2 proto opts))
    (lambda (cost mapping) mapping)))

;; Label of a node under the s-expression protocol.
(define (label x)
  (if (pair? x) (car x) x))

;; Unit cost implied by one mapping entry.
(define (entry-cost p)
  (let ((a (car p)) (b (cdr p)))
    (cond
      ((not a) 1)                              ; insert
      ((not b) 1)                              ; delete
      ((equal? (label a) (label b)) 0)         ; match
      (else 1))))                              ; relabel

(define (mapping-cost m)
  (apply + (map entry-cost m)))

(define (count-deletes m)
  (count (lambda (p) (not (cdr p))) m))

(define (count-inserts m)
  (count (lambda (p) (not (car p))) m))

(define (count-relabels m)
  (count
    (lambda (p)
      (and (car p) (cdr p) (not (equal? (label (car p)) (label (cdr p))))))
    m))

(test-begin "tree-edit-distance")

;;; --- §5 hand-verifiable fixtures (unit costs) --------------------------
(test 0 (ted '(f a b) '(f a b)))      ; identical
(test 1 (ted 'a 'b))                  ; relabel a→b (two leaves)
(test 1 (ted '(f a b) '(f a c)))      ; relabel b→c
(test 1 (ted '(f a b) '(f a)))        ; delete b
(test 1 (ted '(f a) '(f a b)))        ; insert b
(test 1 (ted '(f (g a) b) '(f (g c) b))) ; deep relabel a→c
(test 1 (ted '(f a b) '(g a b)))      ; relabel root f→g

;;; The ordered-ness discriminator: an *unordered* distance would call these
;;; equal (0); ordered tree edit distance pays 2 (relabel a→b AND b→a). This
;;; pins the "ordered" design decision.
(test 2 (ted '(f a b) '(f b a)))

;;; --- §6 metric-property regression -------------------------------------
;;; Under unit costs, ordered tree edit distance is a metric. A wrong
;;; forest-distance recurrence breaks one of these even when the point values
;;; above happen to look right — the triangle inequality is the sharp test.
(define fixtures
  (list 'a 'b
        '(f a b) '(f a c) '(g a b) '(f b a)
        '(f a) '(f a b c)
        '(f (g a) b) '(f (g c) b)
        '(h (f a b) (g c))
        ;; ≥3-deep, two internal-node children — forces multi-level compose
        ;; recursion in %ted-backtrack and the keyroot/non-keyroot branch with
        ;; non-flat operands. Flows through identity/symmetry/triangle/cost.
        '(h (f (g a) b) (k c))))

;; Identity: d(T, T) = 0.
(for-each
  (lambda (t)
    (test 0 (ted t t)))
  fixtures)

;; Symmetry: d(T1, T2) = d(T2, T1).
(for-each
  (lambda (t1)
    (for-each
      (lambda (t2)
        (test (ted t1 t2) (ted t2 t1)))
      fixtures))
  fixtures)

;; Triangle inequality: d(T1, T3) <= d(T1, T2) + d(T2, T3).
(for-each
  (lambda (t1)
    (for-each
      (lambda (t2)
        (for-each
          (lambda (t3)
            (test-assert
              (<= (ted t1 t3)
                  (+ (ted t1 t2) (ted t2 t3)))))
          fixtures))
      fixtures))
  fixtures)

;;; --- Phase 3: node mapping ---------------------------------------------

;; d(T,T): every node matched to its counterpart, no inserts/deletes, and
;; every matched pair has equal labels (identity mapping).
(let ((m (ted-map '(h (f a b) (g c)) '(h (f a b) (g c)))))
  (test 0 (count-deletes m))
  (test 0 (count-inserts m))
  (test 0 (count-relabels m))
  (test 0 (mapping-cost m)))

;; Single relabel: one differing-label pair, no inserts/deletes.
(let ((m (ted-map '(f a b) '(f a c))))
  (test 0 (count-deletes m))
  (test 0 (count-inserts m))
  (test 1 (count-relabels m))
  ;; the relabel pairs the differing leaves b and c
  (test-assert (member '(b . c) m)))

;; Delete: exactly one (_ . #f), no inserts.
(let ((m (ted-map '(f a b) '(f a))))
  (test 1 (count-deletes m))
  (test 0 (count-inserts m))
  (test-assert (member '(b . #f) m)))

;; Insert: exactly one (#f . _), no deletes.
(let ((m (ted-map '(f a) '(f a b))))
  (test 0 (count-deletes m))
  (test 1 (count-inserts m))
  (test-assert (member '(#f . b) m)))

;; Deep relabel: the differing leaf deep in the tree is the only relabel.
(let ((m (ted-map '(f (g a) b) '(f (g c) b))))
  (test 0 (count-deletes m))
  (test 0 (count-inserts m))
  (test 1 (count-relabels m))
  (test-assert (member '(a . c) m)))

;; Mapping-consistency invariant: the cost implied by the mapping equals the
;; returned scalar distance, for every fixture pair. A backtracker that picks
;; a non-minimizing branch would break this even with a correct scalar DP.
(for-each
  (lambda (t1)
    (for-each
      (lambda (t2)
        (call-with-values
          (lambda () (tree-edit-distance t1 t2 proto))
          (lambda (cost m)
            (test cost (mapping-cost m)))))
      fixtures))
  fixtures)

;;; --- Phase 4: cost model -----------------------------------------------

;; Custom delete cost via the alist form (missing ops keep unit cost).
(test 10
      (ted-cost '(f a b) '(f a)
                (cons 'cost (list (cons 'delete (lambda (n) 10))))))

;; Positional cost spec (relabel-fn insert-fn delete-fn) with asymmetric
;; insert/delete costs, so the two assertions below pin the positional ORDER
;; (a swapped parse would flip the 2 and the 7).
(define asym-cost
  (cons 'cost
        (list (lambda (a b) (if (equal? (label a) (label b)) 0 1)) ; relabel
              (lambda (n) 2)                                        ; insert
              (lambda (n) 7))))                                     ; delete
(test 2 (ted-cost '(f a) '(f a b) asym-cost))   ; pure insert of b → 2
(test 7 (ted-cost '(f a b) '(f a) asym-cost))   ; pure delete of b → 7

;; label-equal? override: treat all labels as equal → only structural
;; insert/delete remain, and same-shape trees collapse to distance 0.
(test 0
      (ted-cost '(f a b) '(g a c)
                (cons 'label-equal? (lambda (a b) #t))))

;; Partial-subset alist forms. relabel-only: a free relabel closure drives the
;; leaf distance to 0 (the unit default would give 1) — proves the relabel
;; subset is applied. insert-only: insert costs 9 while root/a match free.
(test 0                                  ; relabel always free → d(x,y)=0
      (ted-cost 'x 'y
                (cons 'cost (list (cons 'relabel (lambda (a b) 0))))))
(test 9                                  ; insert b costs 9; root/a matched free
      (ted-cost '(f a) '(f a b)
                (cons 'cost (list (cons 'insert (lambda (n) 9))))))

;; %wrap-relabel contract: relabel-fn receives the NODES (not labels) in the
;; order (t1-node t2-node). This relabel is free ONLY for the exact ordered
;; pair (x . y); a swapped-argument bug would miss it and fall back to 5,
;; making delete+insert (cost 2) the minimizer instead of 0.
(test 0
      (ted-cost 'x 'y
                (cons 'cost
                      (list (lambda (a b) (if (and (equal? a 'x) (equal? b 'y)) 0 5))
                            (lambda (n) 1)
                            (lambda (n) 1)))))

;; Asymmetric-cost mapping (not just scalar): the cheapest path under
;; insert=2/delete=7 still recovers a delete of b, and the mapping is consistent.
(let ((m (ted-map-opts '(f a b) '(f a) asym-cost)))
  (test-assert (member '(b . #f) m))
  (test 1 (count-deletes m))
  (test 0 (count-inserts m)))

;;; --- Compose/recurse mapping content (multi-level subtree match) -------
;; Identical 3-deep tree: the backtracker's compose branch must pair each
;; matched subtree ROOT with its counterpart (not merely sum to cost 0). A
;; backtracker that mis-attributes subtree roots could still sum to 0 and pass
;; the cost-consistency loop — only an explicit content assertion catches it.
(let ((m (ted-map '(h (f (g a) b) (k c)) '(h (f (g a) b) (k c)))))
  (test 0 (mapping-cost m))
  (test 0 (count-deletes m))
  (test 0 (count-inserts m))
  (test-assert (member (cons '(f (g a) b) '(f (g a) b)) m))  ; f-subtree root ↔ itself
  (test-assert (member (cons '(g a) '(g a)) m))              ; deep g-subtree root ↔ itself
  (test-assert (member (cons '(k c) '(k c)) m)))             ; k-subtree root ↔ itself

;;; --- Phase 4: edge cases -----------------------------------------------

;; Two leaves: 0 when equal, 1 when not.
(test 0 (ted 'a 'a))
(test 1 (ted 'a 'b))

;; Leaf vs larger tree. d('f, '(f a b)): match the leaf to the root f (label
;; equal, cost 0) then insert a and b → 2. Symmetric.
(test 2 (ted 'f '(f a b)))
(test 2 (ted '(f a b) 'f))
;; d('z, '(f a b)): relabel z→f (1) + insert a + insert b (2) = 3, cheaper than
;; deleting z and inserting all three (4).
(test 3 (ted 'z '(f a b)))

;; Identical larger tree → 0.
(test 0 (ted '(h (f a b) (g c)) '(h (f a b) (g c))))

;;; --- Phase 4: input validation -----------------------------------------

;; PROTO must be a term-protocol.
(test-error (tree-edit-distance 'a 'b 42))
;; Unknown option keys are rejected (typo guard).
(test-error (tree-edit-distance 'a 'b proto '(bogus . 1)))
;; A non-procedure label-equal? is rejected up front.
(test-error (tree-edit-distance 'a 'b proto '(label-equal? . 7)))
;; A malformed cost spec is rejected.
(test-error (tree-edit-distance 'a 'b proto '(cost . 99)))
;; A typo'd cost-alist key must surface, not silently fall back to unit cost
;; (options-alist discipline — the inner cost alist is key-validated too).
(test-error
  (tree-edit-distance 'a 'b proto
                      (cons 'cost (list (cons 'delte (lambda (n) 1))))))
;; A positional cost spec of the wrong arity (not 3 procedures) is rejected.
(test-error
  (tree-edit-distance 'a 'b proto
                      (cons 'cost (list (lambda (a b) 0) (lambda (n) 1)))))
;; #f is the reserved no-node mapping sentinel, so a tree node valued #f is
;; rejected at the boundary (enforced, not merely documented) — both as a bare
;; leaf and nested inside a compound.
(test-error (tree-edit-distance #f 'a proto))
(test-error (tree-edit-distance '(f #f) '(f a) proto))
(test-error (tree-edit-distance '(f a) '(f #f) proto))

(test-end)
(test-exit)
