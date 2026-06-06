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

;; ─── Solution record ──────────────────────────────────────────────────────
(define-record-type <cfl-solution>
  (%make-cfl-solution nodes n nt->idx node->idx start-idx R outx)
  cfl-solution?
  (nodes     sol-nodes)      ; vector of node values, index = node-idx
  (n         sol-n)          ; node count
  (nt->idx   sol-nt->idx)    ; hashtable: nonterminal symbol -> idx
  (node->idx sol-node->idx)  ; hashtable: node -> idx
  (start-idx sol-start-idx)  ; idx of the grammar start nonterminal
  (R         sol-R)          ; hashtable: encoded-triple -> #t  (membership)
  (outx      sol-outx))      ; hashtable: encoded-pair(s,A) -> list of t-idx

(define (cfl-solve grammar graph)
  "Close the (s,A,t) derivation relation for GRAMMAR over GRAPH and return a
cfl-solution queryable by cfl-reachable?/-from/-pairs and cfl-derives?.
Runs the Reps-Horwitz-Sagiv worklist; terminates on finite graphs.
Parameters:
  grammar : cfl-grammar
  graph : cfl-graph
Returns: cfl-solution
Category: algebra
Keywords: CFL, reachability, worklist, context-sensitive"
  (let* ((node-list (cfl-graph-nodes graph))
         (nodes     (list->vector node-list))
         (n         (vector-length nodes))
         (node->idx (make-hashtable))
         (nts       (cfl-grammar-nonterminals grammar))
         (m         (length nts))
         (nt->idx   (make-hashtable))
         (prods     (cfl-grammar-productions grammar))
         (unary-rhs (make-hashtable))   ; A-idx -> list of B-idx          (B -> A)
         (bin-rhs1  (make-hashtable))   ; A-idx -> list of (B-idx . C-idx) (B -> A C)
         (bin-rhs2  (make-hashtable))   ; A-idx -> list of (B-idx . C-idx) (B -> C A)
         (R         (make-hashtable))
         (outx      (make-hashtable))
         (inx       (make-hashtable))   ; encoded-pair(A,t) -> list of s-idx
         (work      '()))
    ;; index nodes and nonterminals
    (let loop ((i 0)) (when (< i n) (hashtable-set! node->idx (vector-ref nodes i) i) (loop (+ i 1))))
    (let loop ((i 0) (xs nts)) (unless (null? xs) (hashtable-set! nt->idx (car xs) i) (loop (+ i 1) (cdr xs))))
    (let ((nidx  (lambda (v)   (hashtable-ref node->idx v #f)))
          (ntidx (lambda (sym) (hashtable-ref nt->idx sym #f))))
      ;; encoders (all keys are integers; pairs are not hashable)
      (define (enc3 s a t) (+ (* (+ (* s m) a) n) t))   ; triple (s,A,t)
      (define (encSA s a)  (+ (* s m) a))               ; pair (s,A) for outx
      (define (encAt a t)  (+ (* a n) t))               ; pair (A,t) for inx
      (define (push-list! ht k v) (hashtable-set! ht k (cons v (hashtable-ref ht k '()))))
      (define (add! s a t)
        (let ((key (enc3 s a t)))
          (unless (hashtable-ref R key #f)
            (hashtable-set! R key #t)
            (push-list! outx (encSA s a) t)
            (push-list! inx  (encAt a t) s)
            (set! work (cons (vector s a t) work)))))
      ;; build production indices by RHS
      (for-each
        (lambda (p)
          (case (cfl-production-kind p)
            ((unary)
             (push-list! unary-rhs (ntidx (cfl-production-rhs1 p)) (ntidx (cfl-production-lhs p))))
            ((binary)
             (let ((b (ntidx (cfl-production-lhs p)))
                   (a (ntidx (cfl-production-rhs1 p)))
                   (c (ntidx (cfl-production-rhs2 p))))
               (push-list! bin-rhs1 a (cons b c))
               (push-list! bin-rhs2 c (cons b a))))
            (else #f)))
        prods)
      ;; seed: epsilon self-loops and terminal edges
      (for-each
        (lambda (p)
          (case (cfl-production-kind p)
            ((epsilon)
             (let ((a (ntidx (cfl-production-lhs p))))
               (let loop ((i 0)) (when (< i n) (add! i a i) (loop (+ i 1))))))
            ((terminal)
             (let ((a (ntidx (cfl-production-lhs p)))
                   (lbl (cfl-production-rhs1 p)))
               (for-each
                 (lambda (e)        ; e = (from label to)
                   (when (equal? (cadr e) lbl)
                     (add! (nidx (car e)) a (nidx (caddr e)))))
                 (cfl-graph-edges graph))))
            (else #f)))
        prods)
      ;; propagate to fixpoint
      (let loop ()
        (unless (null? work)
          (let* ((tr (car work)) (s (vector-ref tr 0)) (a (vector-ref tr 1)) (t (vector-ref tr 2)))
            (set! work (cdr work))
            (for-each (lambda (b) (add! s b t)) (hashtable-ref unary-rhs a '()))
            (for-each
              (lambda (bc)                 ; B -> A C : (t,C,e) => (s,B,e)
                (for-each (lambda (e) (add! s (car bc) e))
                          (hashtable-ref outx (encSA t (cdr bc)) '())))
              (hashtable-ref bin-rhs1 a '()))
            (for-each
              (lambda (bc)                 ; B -> C A : (e,C,s) => (e,B,t)
                (for-each (lambda (e) (add! e (car bc) t))
                          (hashtable-ref inx (encAt (cdr bc) s) '())))
              (hashtable-ref bin-rhs2 a '())))
          (loop)))
      (%make-cfl-solution nodes n nt->idx node->idx (ntidx (cfl-grammar-start grammar)) R outx))))

;; ─── Queries ──────────────────────────────────────────────────────────────
(define (%sol-enc3 sol s a t) (+ (* (+ (* s (hashtable-size (sol-nt->idx sol))) a) (sol-n sol)) t))

(define (cfl-reachable? sol s t)
  "True iff T is reachable from S deriving the grammar's START symbol.
Parameters:
  sol : cfl-solution
  s : node
  t : node
Returns: boolean
Category: algebra
Keywords: CFL, reachability, query"
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (ti (hashtable-ref (sol-node->idx sol) t #f)))
    (and si ti (sol-start-idx sol)
         (hashtable-ref (sol-R sol) (%sol-enc3 sol si (sol-start-idx sol) ti) #f)
         #t)))

(define (cfl-reachable-from sol s)
  "List of nodes T with (S, START, T) — START-reachable targets from S.
Parameters:
  sol : cfl-solution
  s : node
Returns: list of nodes
Category: algebra
Keywords: CFL, reachability, query"
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (m  (hashtable-size (sol-nt->idx sol))))
    (if (and si (sol-start-idx sol))
        (map (lambda (ti) (vector-ref (sol-nodes sol) ti))
             (hashtable-ref (sol-outx sol) (+ (* si m) (sol-start-idx sol)) '()))
        '())))

(define (cfl-reachable-pairs sol)
  "All (s . t) pairs reachable under START.
Parameters:
  sol : cfl-solution
Returns: list of (s . t)
Category: algebra
Keywords: CFL, reachability, query"
  (let loop ((i 0) (acc '()))
    (if (>= i (sol-n sol))
        (reverse acc)
        (let* ((s (vector-ref (sol-nodes sol) i))
               (ts (cfl-reachable-from sol s)))
          (loop (+ i 1) (append (reverse (map (lambda (t) (cons s t)) ts)) acc))))))

(define (cfl-derives? sol s a t)
  "True iff (S, A, T) is derivable for nonterminal A (the full relation).
Parameters:
  sol : cfl-solution
  s : node
  a : nonterminal symbol
  t : node
Returns: boolean
Category: algebra
Keywords: CFL, derives, query"
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (ai (hashtable-ref (sol-nt->idx sol) a #f))
        (ti (hashtable-ref (sol-node->idx sol) t #f)))
    (and si ai ti (hashtable-ref (sol-R sol) (%sol-enc3 sol si ai ti) #f) #t)))

;; ─── Validators ──────────────────────────────────────────────────────
(define (validate-cfl-grammar g)
  "Return #t if G is well-formed, else a list of violation descriptions.
Checks: (1) the start symbol has at least one production; (2) the
terminal and nonterminal symbol sets are disjoint; (3) every RHS symbol
in unary and binary productions is a defined nonterminal.
Parameters:
  g : cfl-grammar
Returns: any
Category: algebra
Keywords: validation, grammar, CFL, well-formed"
  (let* ((fail! (make-violation-reporter))
         (nts   (cfl-grammar-nonterminals g))
         (terms (cfl-grammar-terminals g))
         (nt?   (lambda (x) (and (memv x nts) #t))))
    (unless (nt? (cfl-grammar-start g))
      (fail! 'start-undefined (cfl-grammar-start g)))
    (for-each
      (lambda (t)
        (when (nt? t)
          (fail! 'terminal-nonterminal-collision t)))
      terms)
    (for-each
      (lambda (p)
        (case (cfl-production-kind p)
          ((unary)
           (unless (nt? (cfl-production-rhs1 p))
             (fail! 'rhs-not-nonterminal (cfl-production-rhs1 p))))
          ((binary)
           (unless (nt? (cfl-production-rhs1 p))
             (fail! 'rhs-not-nonterminal (cfl-production-rhs1 p)))
           (unless (nt? (cfl-production-rhs2 p))
             (fail! 'rhs-not-nonterminal (cfl-production-rhs2 p))))
          (else #f)))
      (cfl-grammar-productions g))
    (fail!)))

(define (validate-cfl-graph G)
  "Return #t if every edge in G references declared nodes, else a
violation list. Each violation identifies an undeclared from- or
to-node.
Parameters:
  G : cfl-graph
Returns: any
Category: algebra
Keywords: validation, graph, CFL, well-formed"
  (let ((fail!  (make-violation-reporter))
        (nodes  (cfl-graph-nodes G)))
    (for-each
      (lambda (e)
        (unless (member (car e) nodes)
          (fail! 'edge-from-undeclared (car e)))
        (unless (member (caddr e) nodes)
          (fail! 'edge-to-undeclared (caddr e))))
      (cfl-graph-edges G))
    (fail!)))

;; ─── Dyck preset ──────────────────────────────────────────────────────
(define (dyck-grammar bracket-pairs)
  "Build the Dyck (matched-delimiter) grammar over BRACKET-PAIRS, a list of
(open-label . close-label) pairs. The start symbol S derives exactly the
balanced strings. This is the program-analysis entry point: one pair per call
site (call/return) or per field (open/close) yields interprocedural /
field-sensitive reachability.

For start S and each pair i this generates the normalized productions
S -> eps, S -> S S, S -> Oi Ti, Ti -> S Ci, Oi -> open_i, Ci -> close_i.
The internal nonterminal names (O<i>/T<i>/C<i>) are deterministic but are not
a public contract — query results via cfl-reachable?, not internal names.
Parameters:
  bracket-pairs : list of (open . close)
Returns: cfl-grammar
Category: algebra
Keywords: Dyck, balanced, brackets, interprocedural, context-sensitive"
  (define (nm prefix i)
    (string->symbol (string-append prefix (number->string i))))
  (let loop ((pairs bracket-pairs) (i 0)
             (prods (list (cfl-epsilon 'S) (cfl-binary 'S 'S 'S))))
    (if (null? pairs)
        (make-cfl-grammar 'S prods)
        (let ((o (nm "O" i)) (tt (nm "T" i)) (c (nm "C" i))
              (open (caar pairs)) (close (cdar pairs)))
          (loop (cdr pairs) (+ i 1)
                (append (list (cfl-binary 'S o tt)
                              (cfl-binary tt 'S c)
                              (cfl-terminal o open)
                              (cfl-terminal c close))
                        prods))))))
