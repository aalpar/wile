;;; (wile algebra cfl) — context-free-language reachability.
;;;
;;; A path counts iff its edge-label string is in L(A) for a nonterminal A.
;;; Solver: Melski-Reps / Reps-Horwitz-Sagiv (1995) worklist over (s,A,t)
;;; triples — terminates on finite graphs (finite triple set), O(n^3 * |G|).
;;;
;;; Grammars are normalized BY CONSTRUCTION: the four production kernels are
;;; exactly the four normal forms (eps, terminal, unary A->B, binary A->B C).

;; ─── Productions ──────────────────────────────────────────────────────────
(define-record-type <cfl-production>
  (%make-cfl-production kind lhs rhs1 rhs2)
  cfl-production?
  (kind cfl-production-kind)   ; 'epsilon | 'terminal | 'unary | 'binary
  (lhs  cfl-production-lhs)    ; nonterminal (symbol)
  (rhs1 cfl-production-rhs1)   ; terminal label | nonterminal B | #f
  (rhs2 cfl-production-rhs2))  ; nonterminal C (binary only) | #f

(define (cfl-epsilon a)
  (%make-cfl-production 'epsilon  a #f #f))

(define (cfl-terminal a t)
  (%make-cfl-production 'terminal a t  #f))

(define (cfl-unary a b)
  (%make-cfl-production 'unary    a b  #f))

(define (cfl-binary a b c)
  (%make-cfl-production 'binary   a b  c))

;; ─── Grammar ──────────────────────────────────────────────────────────────
(define-record-type <cfl-grammar>
  (%make-cfl-grammar start productions)
  cfl-grammar?
  (start       cfl-grammar-start)
  (productions cfl-grammar-productions))

(define (make-cfl-grammar start productions)
  "Construct a context-free grammar from a START nonterminal and a list of
PRODUCTIONS (each built by cfl-epsilon/cfl-terminal/cfl-unary/cfl-binary).
The grammar is normalized by construction. Use validate-cfl-grammar to check
well-formedness (terminal/nonterminal disjointness, start has a production,
RHS nonterminals are defined).
Parameters:
  start : symbol
  productions : list of cfl-production
Returns: cfl-grammar
Category: algebra
Keywords: grammar, context-free, CFL, reachability"
  (%make-cfl-grammar start productions))

;; Nonterminals = all LHS symbols. Terminals = all cfl-terminal rhs1 labels.
(define (cfl-grammar-nonterminals g)
  "Return the deduplicated list of nonterminal symbols of grammar G (all
production left-hand sides, in first-occurrence order).
Parameters:
  g : cfl-grammar
Returns: list of symbols
Category: algebra
Keywords: grammar, context-free, CFL, nonterminals"
  (let loop ((ps (cfl-grammar-productions g)) (acc '()))
    (cond ((null? ps) (reverse acc))
          ((member (cfl-production-lhs (car ps)) acc) (loop (cdr ps) acc))
          (else (loop (cdr ps) (cons (cfl-production-lhs (car ps)) acc))))))

(define (cfl-grammar-terminals g)
  "Return the deduplicated list of terminal labels of grammar G (the label T
in every cfl-terminal production A→T, in first-occurrence order).
Parameters:
  g : cfl-grammar
Returns: list
Category: algebra
Keywords: grammar, context-free, CFL, terminals"
  (let loop ((ps (cfl-grammar-productions g)) (acc '()))
    (cond ((null? ps) (reverse acc))
          ((and (eq? (cfl-production-kind (car ps)) 'terminal)
                (not (member (cfl-production-rhs1 (car ps)) acc)))
           (loop (cdr ps) (cons (cfl-production-rhs1 (car ps)) acc)))
          (else (loop (cdr ps) acc)))))

;; ─── Graph ────────────────────────────────────────────────────────────────
(define-record-type <cfl-graph>
  (%make-cfl-graph nodes edges)
  cfl-graph?
  (nodes cfl-graph-nodes)
  (edges cfl-graph-edges))   ; edges: list of (from label to)

(define (make-cfl-graph nodes edges)
  "Construct a labeled directed graph from a NODES list and an EDGES list of
(from label to) triples. Nodes and labels must be hashable atoms (symbols,
strings, numbers) for v1.
Parameters:
  nodes : list
  edges : list of (from label to)
Returns: cfl-graph
Category: algebra
Keywords: graph, labeled, directed, CFL"
  (%make-cfl-graph nodes edges))
