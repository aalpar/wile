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

;; Scalar distance only (drops the mapping, which arrives in Phase 3).
(define (ted t1 t2)
  (call-with-values
    (lambda () (tree-edit-distance t1 t2 proto))
    (lambda (cost mapping) cost)))

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

(test-end)
(test-exit)
