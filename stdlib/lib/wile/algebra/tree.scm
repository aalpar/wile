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

;;; -- Public entry (Phase 0 stub) --

(define (tree-edit-distance t1 t2 proto . opts)
  (error "tree-edit-distance: not implemented"
         (list 'fix "implementation lands in Phases 1-4")))
