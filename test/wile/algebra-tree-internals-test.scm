;;; algebra-tree-internals-test.scm — white-box tests for (wile algebra tree)
;;; Zhang-Shasha preprocessing (%tree-postorder / %tree-l / %tree-keyroots).
;;;
;;; These helpers are %-private (not exported), so this file `include`s the
;;; implementation directly to reach them. The include path is repo-root
;;; relative — run-all.sh / cover-scm.sh cd to the repo root before invoking,
;;; matching how CI runs the suite. Black-box tests of the public
;;; tree-edit-distance contract live in algebra-tree-test.scm.

(import (scheme base)
        (srfi 1)
        (chibi test)
        (wile algebra setoid)
        (wile algebra rewrite))

(include "pkg/stdlib/lib/wile/algebra/tree.scm")

;;; Built-in s-expression protocol: (op c1 c2 ...) is compound with label op
;;; and children c1..; an atom is a leaf labeled by itself.
(define sexp-proto
  (make-term-protocol
    pair?
    car
    cdr
    (lambda (term new-args) (cons (car term) new-args))
    (lambda (a b) #f)))

;; Read the (1-based) postorder data out of a preprocessed tree as plain lists
;; (dropping the slot-0 sentinel) for easy comparison in tests.
(define (vec->list-1based v n)
  (let loop ((i 1) (acc '()))
    (if (> i n)
        (reverse acc)
        (loop (+ i 1) (cons (vector-ref v i) acc)))))

(define (labels-of t)
  (let ((tree (%ted-preprocess sexp-proto t)))
    (vec->list-1based (ted-tree-labels tree) (ted-tree-n tree))))

(define (l-of t)
  (let ((tree (%ted-preprocess sexp-proto t)))
    (vec->list-1based (ted-tree-l tree) (ted-tree-n tree))))

(define (keyroots-of t)
  (ted-tree-keyroots (%ted-preprocess sexp-proto t)))

(define (n-of t)
  (ted-tree-n (%ted-preprocess sexp-proto t)))

(test-begin "tree-internals")

;;; --- A single leaf -----------------------------------------------------
;;; postorder: a=1.  l(1)=1.  keyroots={1} (the root).
(test 1 (n-of 'a))
(test '(a) (labels-of 'a))
(test '(1) (l-of 'a))
(test '(1) (keyroots-of 'a))

;;; --- A binary node (f a b) ---------------------------------------------
;;; postorder: a=1, b=2, f=3.  labels [a b f].
;;; l(1)=1, l(2)=2, l(3)=l(firstchild=1)=1.
;;; keyroots: root f=3, plus b=2 (has a left sibling a). a=1 has none. -> {2 3}.
(test 3 (n-of '(f a b)))
(test '(a b f) (labels-of '(f a b)))
(test '(1 2 1) (l-of '(f a b)))
(test '(2 3) (keyroots-of '(f a b)))

;;; --- Deep / left-leaning  (f (g a) b) ----------------------------------
;;; postorder: a=1, g=2, b=3, f=4.  labels [a g b f].
;;; children: a->(), g->(1), b->(), f->(2 3).
;;; l(1)=1, l(2)=l(1)=1, l(3)=3, l(4)=l(2)=1.
;;; keyroots (descend 4..1): 4 (l=1,new), 3 (l=3,new), 2 (l=1 seen), 1 (l=1 seen)
;;;   -> {3 4}.  (root f, and b which has a left sibling (g a))
(test 4 (n-of '(f (g a) b)))
(test '(a g b f) (labels-of '(f (g a) b)))
(test '(1 1 3 1) (l-of '(f (g a) b)))
(test '(3 4) (keyroots-of '(f (g a) b)))

;;; --- Right-leaning  (f a (g b)) ----------------------------------------
;;; postorder: a=1, b=2, g=3, f=4.  labels [a b g f].
;;; children: a->(), b->(), g->(2), f->(1 3).
;;; l(1)=1, l(2)=2, l(3)=l(2)=2, l(4)=l(1)=1.
;;; keyroots (descend): 4 (l=1,new), 3 (l=2,new), 2 (l=2 seen), 1 (l=1 seen)
;;;   -> {3 4}.  (root f, and (g b) which has a left sibling a)
(test 4 (n-of '(f a (g b))))
(test '(a b g f) (labels-of '(f a (g b))))
(test '(1 2 2 1) (l-of '(f a (g b))))
(test '(3 4) (keyroots-of '(f a (g b))))

;;; --- Ternary  (f a b c) ------------------------------------------------
;;; postorder: a=1, b=2, c=3, f=4.
;;; l = [1 2 3 1].  keyroots: 4 (l=1 new), 3 (l=3 new), 2 (l=2 new), 1 (seen)
;;;   -> {2 3 4}.  (root + the two right siblings b,c; leftmost child a excluded)
(test '(1 2 3 1) (l-of '(f a b c)))
(test '(2 3 4) (keyroots-of '(f a b c)))

(test-end)
(test-exit)
