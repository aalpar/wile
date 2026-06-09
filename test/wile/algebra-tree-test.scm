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

;; Node mapping only (drops the scalar cost).
(define (ted-map t1 t2)
  (call-with-values
    (lambda () (tree-edit-distance t1 t2 proto))
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
        '(h (f a b) (g c))))

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

(test-end)
(test-exit)
