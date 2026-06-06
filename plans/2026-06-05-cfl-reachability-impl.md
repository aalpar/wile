# CFL-Reachability `(wile algebra cfl)` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `(wile algebra cfl)` — context-free-language reachability over a labeled directed graph — closing the one demand-justified wile-side algebra gap (wile-goast Track C4 context-sensitive analysis).

**Architecture:** A pure-Scheme library with three record types (`<cfl-production>`, `<cfl-grammar>`, `<cfl-graph>`), a Melski–Reps / Reps–Horwitz–Sagiv (1995) worklist solver that closes the `(s, A, t)` derivation relation, query functions over the closed relation, and a `dyck-grammar` preset for matched-delimiter (interprocedural / field-sensitive) analysis. Grammars are normalized **by construction** via four typed production kernels, so there is no CFG-normalization step.

**Tech Stack:** R7RS Scheme; wile built-in `[hashtables]` (`make-hashtable`/`hashtable-set!`/`hashtable-ref`, `equal?`-keyed); `(wile algebra setoid)` validation helpers (`make-violation-reporter`, `assert-procedure`); `(chibi test)` for the test suite; tutorial `lib/check.scm` for the quick-tour.

**Design:** `plans/2026-06-05-cfl-reachability-design.md` (read it first — the normative solver rules and the canary derivation live there).

**Key implementation constraint (verified 2026-06-05):** wile's `make-hashtable` is `equal?`-based but **rejects pair/list keys** ("key is not hashable"). Symbols, strings, and integers ARE hashable. Therefore the solver indexes nodes `0..n-1` and nonterminals `0..m-1` and encodes every compound key as a single integer. Nodes and edge labels must be hashable atoms (symbols/strings/numbers) for v1 — true for the program-analysis consumer; a node-setoid generalization is deferred.

---

## File Structure

- Create `stdlib/lib/wile/algebra/cfl.sld` — library definition (exports + imports + include).
- Create `stdlib/lib/wile/algebra/cfl.scm` — records, constructors, validators, solver, queries, `dyck-grammar` preset.
- Create `test/wile/algebra-cfl-test.scm` — `(chibi test)` suite incl. the canary (auto-discovered by `test/run-all.sh`'s `find ... -name '*-test.scm'`).
- Modify `stdlib/lib/wile/algebra.sld` — add `(wile algebra cfl)` to the umbrella `import` list and append cfl's exports to the umbrella `export` list.
- Create `examples/algebra/tutorial/quick-tour/cfl.scm` — tutorial quick-tour.
- Modify `examples/algebra/tutorial/README.md`, `docs/algebra/tutorial.md`, `docs/algebra/overview.md` — quick-tour index rows.
- Modify `docs/algebra/reference.md` — `## CFL Reachability` section + cross-reference row.
- Modify `TODO.md` — mark the CFL-reachability Tier-A item done; reference the shipped plan.

---

## Task 1 — Records + typed production kernels

**Files:**
- Create: `stdlib/lib/wile/algebra/cfl.sld`
- Create: `stdlib/lib/wile/algebra/cfl.scm`
- Test: `test/wile/algebra-cfl-test.scm`

- [ ] **Step 1: Write the library skeleton (`cfl.sld`).**

```scheme
(define-library (wile algebra cfl)
  (description "Context-free-language reachability over a labeled directed graph. A path counts iff its edge-label string lies in the language of a context-free grammar. Generalizes semiring path-algebra (Boolean reachability, tropical shortest-path) to grammar-constrained composition — the basis of context-sensitive (interprocedural, field-sensitive) program analysis. Reps-Horwitz-Sagiv (1995).")
  (export
    ;; Productions (typed kernels)
    cfl-epsilon cfl-terminal cfl-unary cfl-binary
    cfl-production? cfl-production-kind cfl-production-lhs
    cfl-production-rhs1 cfl-production-rhs2
    ;; Grammar
    make-cfl-grammar cfl-grammar?
    cfl-grammar-start cfl-grammar-productions
    cfl-grammar-nonterminals cfl-grammar-terminals
    validate-cfl-grammar
    ;; Graph
    make-cfl-graph cfl-graph?
    cfl-graph-nodes cfl-graph-edges
    validate-cfl-graph
    ;; Solver + query
    cfl-solve cfl-solution?
    cfl-reachable? cfl-reachable-from cfl-reachable-pairs cfl-derives?
    ;; Preset
    dyck-grammar)
  (import (scheme base)
          (wile algebra setoid))   ; validation-helper idiom (validate-* siblings)
  (include "cfl.scm"))
```

- [ ] **Step 2: Write the failing record/constructor test.**

Append to `test/wile/algebra-cfl-test.scm` (create the file with this header first):

```scheme
;;; algebra-cfl-test.scm — (wile algebra cfl)

(import (scheme base)
        (chibi test)
        (wile algebra cfl))

(test-begin "cfl")

(test-group "productions — typed kernels"
  (test 'epsilon  (cfl-production-kind (cfl-epsilon 'S)))
  (test 'S        (cfl-production-lhs  (cfl-epsilon 'S)))
  (test 'terminal (cfl-production-kind (cfl-terminal 'O 'open)))
  (test 'open     (cfl-production-rhs1 (cfl-terminal 'O 'open)))
  (test 'unary    (cfl-production-kind (cfl-unary 'A 'B)))
  (test 'binary   (cfl-production-kind (cfl-binary 'S 'A 'B)))
  (test 'B        (cfl-production-rhs2 (cfl-binary 'S 'A 'B)))
  (test #t        (cfl-production? (cfl-epsilon 'S))))

(test-group "grammar + graph records"
  (define g (make-cfl-grammar 'S (list (cfl-epsilon 'S) (cfl-terminal 'S 'a))))
  (test #t  (cfl-grammar? g))
  (test 'S  (cfl-grammar-start g))
  (test 2   (length (cfl-grammar-productions g)))
  (define G (make-cfl-graph '(n0 n1) '((n0 a n1))))
  (test #t  (cfl-graph? G))
  (test '(n0 n1)     (cfl-graph-nodes G))
  (test '((n0 a n1)) (cfl-graph-edges G)))
```

- [ ] **Step 3: Run it; verify it fails (library not found / unbound).**

Run: `dist/$(uname -s | tr A-Z a-z)/*/wile --file test/wile/algebra-cfl-test.scm` (or `make build` first).
Expected: FAIL — `(wile algebra cfl)` unresolved or `cfl-epsilon` unbound.

- [ ] **Step 4: Implement records + kernels in `cfl.scm`.**

```scheme
;;; (wile algebra cfl) — context-free-language reachability.
;;;
;;; A path counts iff its edge-label string is in L(A) for a nonterminal A.
;;; Solver: Melski-Reps / Reps-Horwitz-Sagiv (1995) worklist over (s,A,t)
;;; triples — terminates on finite graphs (finite triple set), O(n^3 * |G|).
;;;
;;; Grammars are normalized BY CONSTRUCTION: the four production kernels are
;;; exactly the four normal forms (eps, terminal, unary A->B, binary A->B C).

;; ─── Productions ──────────────────────────────────────────────────────
(define-record-type <cfl-production>
  (%make-cfl-production kind lhs rhs1 rhs2)
  cfl-production?
  (kind cfl-production-kind)   ; 'epsilon | 'terminal | 'unary | 'binary
  (lhs  cfl-production-lhs)    ; nonterminal (symbol)
  (rhs1 cfl-production-rhs1)   ; terminal label | nonterminal B | #f
  (rhs2 cfl-production-rhs2))  ; nonterminal C (binary only) | #f

(define (cfl-epsilon a)     (%make-cfl-production 'epsilon  a #f #f))
(define (cfl-terminal a t)  (%make-cfl-production 'terminal a t  #f))
(define (cfl-unary a b)     (%make-cfl-production 'unary    a b  #f))
(define (cfl-binary a b c)  (%make-cfl-production 'binary   a b  c))

;; ─── Grammar ──────────────────────────────────────────────────────────
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
  (let loop ((ps (cfl-grammar-productions g)) (acc '()))
    (cond ((null? ps) (reverse acc))
          ((memv (cfl-production-lhs (car ps)) acc) (loop (cdr ps) acc))
          (else (loop (cdr ps) (cons (cfl-production-lhs (car ps)) acc))))))

(define (cfl-grammar-terminals g)
  (let loop ((ps (cfl-grammar-productions g)) (acc '()))
    (cond ((null? ps) (reverse acc))
          ((and (eq? (cfl-production-kind (car ps)) 'terminal)
                (not (memv (cfl-production-rhs1 (car ps)) acc)))
           (loop (cdr ps) (cons (cfl-production-rhs1 (car ps)) acc)))
          (else (loop (cdr ps) acc)))))

;; ─── Graph ────────────────────────────────────────────────────────────
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
```

- [ ] **Step 5: Run the record/constructor tests; verify PASS.**

Run: `make build && dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: the two groups pass (8 + 6 assertions).

- [ ] **Step 6: Commit.**

```bash
git add stdlib/lib/wile/algebra/cfl.sld stdlib/lib/wile/algebra/cfl.scm test/wile/algebra-cfl-test.scm
git commit -m "feat(algebra/cfl): records + typed production kernels"
```

---

## Task 2 — The worklist solver + queries (the core; canary first)

**Files:**
- Modify: `stdlib/lib/wile/algebra/cfl.scm` (append solver + queries)
- Test: `test/wile/algebra-cfl-test.scm` (append the canary)

- [ ] **Step 1: Write the canary test FIRST (it will fail).**

Append to `test/wile/algebra-cfl-test.scm`. (NOTE: this uses a hand-built Dyck grammar so it does not depend on Task 3's `dyck-grammar` preset — Task 2 is self-contained.)

```scheme
(test-group "canary — CFL is more precise than Boolean reachability"
  ;; Procedure p reached from two call sites.
  ;;   caller A:  a1 --call1--> p --return1--> a2
  ;;   caller B:  b1 --call2--> p --return2--> b2
  (define G
    (make-cfl-graph
      '(a1 a2 b1 b2 p)
      '((a1 call1 p) (p return1 a2)
        (b1 call2 p) (p return2 b2))))
  ;; Dyck grammar for two bracket pairs, hand-built:
  ;;   S -> eps | S S | O1 T1 | O2 T2
  ;;   T1 -> S C1 ;  T2 -> S C2
  ;;   O1 -> call1 ; C1 -> return1 ; O2 -> call2 ; C2 -> return2
  (define dyck
    (make-cfl-grammar 'S
      (list (cfl-epsilon 'S)
            (cfl-binary 'S 'S 'S)
            (cfl-binary 'S 'O1 'T1) (cfl-binary 'T1 'S 'C1)
            (cfl-binary 'S 'O2 'T2) (cfl-binary 'T2 'S 'C2)
            (cfl-terminal 'O1 'call1)   (cfl-terminal 'C1 'return1)
            (cfl-terminal 'O2 'call2)   (cfl-terminal 'C2 'return2))))
  (define sol (cfl-solve dyck G))
  ;; Matched (interprocedurally feasible) paths ARE start-reachable:
  (test #t (cfl-reachable? sol 'a1 'a2))
  (test #t (cfl-reachable? sol 'b1 'b2))
  ;; THE CANARY: mismatched brackets. A directed path a1->p->b2 EXISTS (so
  ;; Boolean reachability would say #t), but call1/return2 do not balance, so
  ;; CFL reachability is precise and returns #f:
  (test #f (cfl-reachable? sol 'a1 'b2))
  (test #f (cfl-reachable? sol 'b1 'a2))
  ;; Reflexive: epsilon makes every node S-reach itself.
  (test #t (cfl-reachable? sol 'p 'p))
  ;; from / pairs / derives? surfaces:
  (test #t (member 'a2 (cfl-reachable-from sol 'a1)))
  (test #f (and (member 'b2 (cfl-reachable-from sol 'a1)) #t))
  (test #t (cfl-derives? sol 'a1 'S 'a2)))
```

- [ ] **Step 2: Run it; verify it fails (`cfl-solve` unbound).**

Run: `dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: FAIL — `cfl-solve` unbound.

- [ ] **Step 3: Implement the solver + queries in `cfl.scm`.**

Append. The solver integer-indexes nodes and nonterminals and encodes all
hashtable keys as integers (pairs are not hashable in wile).

```scheme
;; ─── Solution record ──────────────────────────────────────────────────
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
               (push-list! bin-rhs2 c (cons b c))))
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

;; ─── Queries ──────────────────────────────────────────────────────────
(define (%sol-enc3 sol s a t) (+ (* (+ (* s (hashtable-size (sol-nt->idx sol))) a) (sol-n sol)) t))

(define (cfl-reachable? sol s t)
  "True iff T is reachable from S deriving the grammar's START symbol."
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (ti (hashtable-ref (sol-node->idx sol) t #f)))
    (and si ti (sol-start-idx sol)
         (hashtable-ref (sol-R sol) (%sol-enc3 sol si (sol-start-idx sol) ti) #f)
         #t)))

(define (cfl-reachable-from sol s)
  "List of nodes T with (S, START, T) — START-reachable targets from S."
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (m  (hashtable-size (sol-nt->idx sol))))
    (if (and si (sol-start-idx sol))
        (map (lambda (ti) (vector-ref (sol-nodes sol) ti))
             (hashtable-ref (sol-outx sol) (+ (* si m) (sol-start-idx sol)) '()))
        '())))

(define (cfl-reachable-pairs sol)
  "All (s . t) pairs reachable under START."
  (let loop ((i 0) (acc '()))
    (if (>= i (sol-n sol))
        (reverse acc)
        (let* ((s (vector-ref (sol-nodes sol) i))
               (ts (cfl-reachable-from sol s)))
          (loop (+ i 1) (append (reverse (map (lambda (t) (cons s t)) ts)) acc))))))

(define (cfl-derives? sol s a t)
  "True iff (S, A, T) is derivable for nonterminal A (the full relation)."
  (let ((si (hashtable-ref (sol-node->idx sol) s #f))
        (ai (hashtable-ref (sol-nt->idx sol) a #f))
        (ti (hashtable-ref (sol-node->idx sol) t #f)))
    (and si ai ti (hashtable-ref (sol-R sol) (%sol-enc3 sol si ai ti) #f) #t)))
```

- [ ] **Step 4: Run the canary; verify PASS.**

Run: `make build && dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: the canary group passes — crucially `(test #f (cfl-reachable? sol 'a1 'b2))` and `(test #f (cfl-reachable? sol 'b1 'a2))`. If either is `#t`, the solver is over-approximating (treating derivation as free/associative) — re-check the binary propagation `bin-rhs1`/`bin-rhs2` directions against the design's normative rules.

- [ ] **Step 5: Commit.**

```bash
git add stdlib/lib/wile/algebra/cfl.scm test/wile/algebra-cfl-test.scm
git commit -m "feat(algebra/cfl): worklist solver + queries; context-sensitivity canary"
```

---

## Task 3 — `dyck-grammar` preset

**Files:**
- Modify: `stdlib/lib/wile/algebra/cfl.scm`
- Test: `test/wile/algebra-cfl-test.scm`

- [ ] **Step 1: Write the failing preset test.**

```scheme
(test-group "dyck-grammar preset"
  (define G
    (make-cfl-graph '(a1 a2 b2 p)
      '((a1 call1 p) (p return1 a2) (p return2 b2))))
  (define sol (cfl-solve (dyck-grammar '((call1 . return1) (call2 . return2))) G))
  (test #t (cfl-reachable? sol 'a1 'a2))    ; matched
  (test #f (cfl-reachable? sol 'a1 'b2))    ; mismatched
  ;; nested: [[ ]] over a line graph
  (define Gn
    (make-cfl-graph '(x0 x1 x2 x3 x4)
      '((x0 open x1) (x1 open x2) (x2 close x3) (x3 close x4))))
  (define soln (cfl-solve (dyck-grammar '((open . close))) Gn))
  (test #t (cfl-reachable? soln 'x0 'x4))   ; open open close close — balanced
  (test #f (cfl-reachable? soln 'x0 'x3)))  ; open open close — unbalanced
```

- [ ] **Step 2: Run it; verify it fails (`dyck-grammar` unbound).**

Run: `dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: FAIL — `dyck-grammar` unbound.

- [ ] **Step 3: Implement `dyck-grammar` in `cfl.scm`.**

Generates, for start `S` and each bracket pair i, the normalized productions
`S -> eps`, `S -> S S`, `S -> Oi Ti`, `Ti -> S Ci`, `Oi -> open_i`,
`Ci -> close_i`. Internal nonterminal names are gensym-free and deterministic
(`O<i>`/`T<i>`/`C<i>` interned via `string->symbol`), but are NOT a public
contract — query via `cfl-reachable?`, not internal names.

```scheme
(define (dyck-grammar bracket-pairs)
  "Build the Dyck (matched-delimiter) grammar over BRACKET-PAIRS, a list of
(open-label . close-label) pairs. The start symbol S derives exactly the
balanced strings. This is the program-analysis entry point: one pair per call
site (call/return) or per field (open/close) yields interprocedural /
field-sensitive reachability.
Parameters:
  bracket-pairs : list of (open . close)
Returns: cfl-grammar
Category: algebra
Keywords: Dyck, balanced, brackets, interprocedural, context-sensitive"
  (define (nm prefix i) (string->symbol (string-append prefix (number->string i))))
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
```

- [ ] **Step 4: Run the preset test; verify PASS.**

Run: `make build && dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: PASS (matched `#t`, mismatched `#f`, nested balanced `#t`, unbalanced `#f`).

- [ ] **Step 5: Commit.**

```bash
git add stdlib/lib/wile/algebra/cfl.scm test/wile/algebra-cfl-test.scm
git commit -m "feat(algebra/cfl): dyck-grammar preset for matched-delimiter reachability"
```

---

## Task 4 — Validation + general (non-Dyck) grammar

**Files:**
- Modify: `stdlib/lib/wile/algebra/cfl.scm`
- Test: `test/wile/algebra-cfl-test.scm`

- [ ] **Step 1: Write failing validation + general-grammar tests.**

```scheme
(test-group "validation"
  ;; well-formed grammar/graph -> #t
  (define ok-g (make-cfl-grammar 'S (list (cfl-epsilon 'S) (cfl-terminal 'S 'a))))
  (test #t (validate-cfl-grammar ok-g))
  (test #t (validate-cfl-graph (make-cfl-graph '(n0 n1) '((n0 a n1)))))
  ;; terminal/nonterminal collision: 'a is both a terminal and a nonterminal LHS
  (define bad-g
    (make-cfl-grammar 'S (list (cfl-terminal 'S 'a) (cfl-epsilon 'a))))
  (test #t (pair? (validate-cfl-grammar bad-g)))   ; returns violation list
  ;; start with no production
  (test #t (pair? (validate-cfl-grammar (make-cfl-grammar 'Q (list (cfl-epsilon 'S))))))
  ;; binary RHS that is not a nonterminal (undefined symbol Z)
  (test #t (pair? (validate-cfl-grammar
                    (make-cfl-grammar 'S (list (cfl-binary 'S 'Z 'S) (cfl-epsilon 'S))))))
  ;; edge to undeclared node
  (test #t (pair? (validate-cfl-graph (make-cfl-graph '(n0) '((n0 a n1)))))))

(test-group "general (non-Dyck) grammar — a^n b^n"
  ;; S -> A B?  Use S -> eps | a S b  encoded in CNF:
  ;;   S -> eps | Oa Tb ;  Tb -> S Cb ;  Oa -> a ; Cb -> b   (this IS Dyck on a/b)
  ;; A genuinely non-Dyck check: S -> X X over a single terminal, requiring an
  ;; even-length path of 'a' edges.
  (define even-a
    (make-cfl-grammar 'S
      (list (cfl-epsilon 'S) (cfl-binary 'S 'P 'S) (cfl-binary 'P 'A 'A) (cfl-terminal 'A 'a))))
  (define G (make-cfl-graph '(n0 n1 n2 n3) '((n0 a n1) (n1 a n2) (n2 a n3))))
  (define sol (cfl-solve even-a G))
  (test #t (cfl-reachable? sol 'n0 'n2))   ; two a-edges: even
  (test #f (cfl-reachable? sol 'n0 'n3))   ; three a-edges: odd
  (test #t (cfl-reachable? sol 'n0 'n0)))  ; zero: even (epsilon)
```

- [ ] **Step 2: Run; verify it fails (`validate-cfl-grammar` returns wrong shape / unbound).**

Run: `dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: FAIL.

- [ ] **Step 3: Implement validators in `cfl.scm`.**

Uses `make-violation-reporter` from `(wile algebra setoid)` (returns a closure
pair: a `report!` and a `result` thunk — confirm the exact protocol against
`setoid.scm:102` and follow it; the sketch below assumes
`(make-violation-reporter)` returns `(values report! finish)` where `finish`
returns `#t` or the accumulated list).

```scheme
(define (validate-cfl-grammar g)
  "Return #t if G is well-formed, else a list of violation descriptions.
Checks: terminal and nonterminal symbol sets are disjoint; START has at least
one production; every unary/binary RHS symbol is a defined nonterminal."
  (let* ((nts   (cfl-grammar-nonterminals g))
         (terms (cfl-grammar-terminals g))
         (nt?   (lambda (x) (and (memv x nts) #t)))
         (vs    '()))
    (define (bad! msg) (set! vs (cons msg vs)))
    (unless (nt? (cfl-grammar-start g))
      (bad! (list 'start-undefined (cfl-grammar-start g))))
    (for-each (lambda (t) (when (nt? t) (bad! (list 'terminal-nonterminal-collision t)))) terms)
    (for-each
      (lambda (p)
        (case (cfl-production-kind p)
          ((unary)  (unless (nt? (cfl-production-rhs1 p)) (bad! (list 'rhs-not-nonterminal (cfl-production-rhs1 p)))))
          ((binary) (unless (nt? (cfl-production-rhs1 p)) (bad! (list 'rhs-not-nonterminal (cfl-production-rhs1 p))))
                    (unless (nt? (cfl-production-rhs2 p)) (bad! (list 'rhs-not-nonterminal (cfl-production-rhs2 p)))))
          (else #f)))
      (cfl-grammar-productions g))
    (if (null? vs) #t (reverse vs))))

(define (validate-cfl-graph G)
  "Return #t if every edge references declared nodes, else a violation list."
  (let ((nodes (cfl-graph-nodes G)) (vs '()))
    (for-each
      (lambda (e)
        (unless (member (car e) nodes)   (set! vs (cons (list 'edge-from-undeclared (car e)) vs)))
        (unless (member (caddr e) nodes) (set! vs (cons (list 'edge-to-undeclared (caddr e)) vs))))
      (cfl-graph-edges G))
    (if (null? vs) #t (reverse vs))))
```

> NOTE for the implementer: the sketch above hand-rolls accumulation. Before
> finalizing, check `stdlib/lib/wile/algebra/setoid.scm:102` (`make-violation-reporter`)
> and the sibling `validate-*` functions (e.g. `validate-lattice`,
> `validate-semiring`) and follow whichever idiom they use, so cfl's validators
> are consistent with the rest of the algebra suite. If the suite convention is
> the reporter closure, replace the `set!`/`bad!` accumulation with it.

- [ ] **Step 4: Run; verify PASS.**

Run: `make build && dist/*/*/wile --file test/wile/algebra-cfl-test.scm`
Expected: all groups pass.

- [ ] **Step 5: Commit.**

```bash
git add stdlib/lib/wile/algebra/cfl.scm test/wile/algebra-cfl-test.scm
git commit -m "feat(algebra/cfl): grammar/graph validators + general-grammar tests"
```

---

## Task 5 — Umbrella re-export

**Files:**
- Modify: `stdlib/lib/wile/algebra.sld`

- [ ] **Step 1: Add the import.** In the `(import …)` block (ends at `(wile algebra sat)))`), add `(wile algebra cfl)` after `(wile algebra sat)`.

- [ ] **Step 2: Add the exports.** Append a block to the umbrella `(export …)` list:

```scheme
    ;; CFL reachability
    cfl-epsilon cfl-terminal cfl-unary cfl-binary
    cfl-production? cfl-production-kind cfl-production-lhs
    cfl-production-rhs1 cfl-production-rhs2
    make-cfl-grammar cfl-grammar?
    cfl-grammar-start cfl-grammar-productions
    cfl-grammar-nonterminals cfl-grammar-terminals
    validate-cfl-grammar
    make-cfl-graph cfl-graph? cfl-graph-nodes cfl-graph-edges
    validate-cfl-graph
    cfl-solve cfl-solution?
    cfl-reachable? cfl-reachable-from cfl-reachable-pairs cfl-derives?
    dyck-grammar
```

- [ ] **Step 3: Verify the umbrella imports cleanly.**

Run: `make build && dist/*/*/wile -e "(import (wile algebra)) (display (cfl-reachable? (cfl-solve (dyck-grammar '((o . c))) (make-cfl-graph '(x y z) '((x o y) (y c z)))) 'x 'z)) (newline)"`
Expected: `#t`.

- [ ] **Step 4: Commit.**

```bash
git add stdlib/lib/wile/algebra.sld
git commit -m "feat(algebra): re-export (wile algebra cfl) from the umbrella"
```

---

## Task 6 — Quick-tour + reference docs (land documented from day one)

**Files:**
- Create: `examples/algebra/tutorial/quick-tour/cfl.scm`
- Modify: `examples/algebra/tutorial/README.md`, `docs/algebra/tutorial.md`, `docs/algebra/overview.md`
- Modify: `docs/algebra/reference.md`

- [ ] **Step 1: Write the quick-tour** (tutorial harness: `check=`/`check-true`/`check-false` from `lib/check.scm`).

```scheme
;; quick-tour: (wile algebra cfl)
;;
;; Context-free-language reachability: a path from s to t "counts" only when
;; its edge-label string is balanced under a context-free grammar. This is
;; how interprocedural analysis stays precise -- a call must return to its
;; own call site, not someone else's. You reach for it when plain (Boolean)
;; reachability over-approximates because it ignores call/return or field
;; open/close matching.

(import (scheme base) (wile algebra cfl))
(include "../lib/check.scm")

;; -- Two call sites into one procedure --
(define g
  (make-cfl-graph '(a1 a2 b1 b2 p)
    '((a1 call1 p) (p return1 a2)
      (b1 call2 p) (p return2 b2))))
(define sol (cfl-solve (dyck-grammar '((call1 . return1) (call2 . return2))) g))

;; -- Matched call/return is reachable --
(check-true  (cfl-reachable? sol 'a1 'a2) "a1 -> a2: call1 matched by return1")
(check-true  (cfl-reachable? sol 'b1 'b2) "b1 -> b2: call2 matched by return2")

;; -- Mismatched is NOT (this is the precision plain reachability lacks) --
(check-false (cfl-reachable? sol 'a1 'b2) "a1 -> b2: call1 / return2 do not balance")

;; -- General grammars too: balanced brackets on a line graph --
(define gn (make-cfl-graph '(x0 x1 x2 x3 x4)
              '((x0 open x1) (x1 open x2) (x2 close x3) (x3 close x4))))
(define soln (cfl-solve (dyck-grammar '((open . close))) gn))
(check-true  (cfl-reachable? soln 'x0 'x4) "[[ ]] balances")
(check-false (cfl-reachable? soln 'x0 'x3) "[[ ] does not")

(display "cfl tour complete") (newline)
```

- [ ] **Step 2: Add quick-tour index rows** (match existing format exactly):
  - `examples/algebra/tutorial/README.md` quick-tour table: `| \`quick-tour/cfl.scm\` | \`cfl\` (context-free-language reachability) |`
  - `docs/algebra/tutorial.md` quick-tour table: `| [\`quick-tour/cfl.scm\`](../../examples/algebra/tutorial/quick-tour/cfl.scm) | \`cfl\` | CFL/Dyck reachability; interprocedural call/return matching vs plain Boolean reachability |`
  - `docs/algebra/overview.md` Learning-Path quick-tour list: `- [\`cfl.scm\`](../../examples/algebra/tutorial/quick-tour/cfl.scm)`

- [ ] **Step 3: Add the `reference.md` section** before the "Cross-Reference" table, and a cross-ref row `| CFL Reachability | \`(wile algebra cfl)\` |`:

```markdown
## CFL Reachability -- `(wile algebra cfl)`

Context-free-language reachability: a path counts iff its edge-label string lies in `L(A)` for nonterminal `A`. Generalizes semiring path-algebra (Boolean reachability, tropical shortest-path) to grammar-constrained composition — the basis of context-sensitive program analysis (interprocedural call/return matching, field-sensitivity). Solver: Reps–Horwitz–Sagiv (1995) worklist over `(s, A, t)` triples; terminates on finite graphs, `O(n³·|G|)`.

### Grammar (typed production kernels)
- `(cfl-epsilon A)` / `(cfl-terminal A t)` / `(cfl-unary A B)` / `(cfl-binary A B C)` -- the four normal-form productions; grammars are normalized by construction
- `(make-cfl-grammar start productions)` / `cfl-grammar?` / `cfl-grammar-start` / `cfl-grammar-productions`
- `(cfl-grammar-nonterminals g)` / `(cfl-grammar-terminals g)` -- derived symbol sets
- `(validate-cfl-grammar g)` -- `#t` or violation list; checks terminal/nonterminal disjointness, defined start, RHS-nonterminal

### Graph
- `(make-cfl-graph nodes edges)` -- edges are `(from label to)` triples; nodes/labels are hashable atoms
- `cfl-graph?` / `cfl-graph-nodes` / `cfl-graph-edges` / `(validate-cfl-graph G)`

### Solve + query
- `(cfl-solve grammar graph)` -- closes the relation; returns `<cfl-solution>`
- `(cfl-reachable? sol s t)` -- start-symbol reachability
- `(cfl-reachable-from sol s)` -- start-reachable targets
- `(cfl-reachable-pairs sol)` -- all `(s . t)` under start
- `(cfl-derives? sol s A t)` -- full relation, any nonterminal

### Dyck preset
- `(dyck-grammar bracket-pairs)` -- balanced-delimiter grammar from `(open . close)` pairs; the interprocedural / field-sensitive entry point

### References
- Reps, Horwitz, Sagiv (1995). "Precise interprocedural dataflow analysis via graph reachability." POPL.
- Melski, Reps (2000). "Interconvertibility of set constraints and CFL-reachability." TCS.
```

- [ ] **Step 4: Verify the quick-tour runs.**

Run: `dist/*/*/wile --file examples/algebra/tutorial/quick-tour/cfl.scm`
Expected: all `ok` lines + `cfl tour complete`.

- [ ] **Step 5: Commit.**

```bash
git add examples/algebra/tutorial/quick-tour/cfl.scm examples/algebra/tutorial/README.md docs/algebra/tutorial.md docs/algebra/overview.md docs/algebra/reference.md
git commit -m "docs(algebra/cfl): quick-tour + reference section + index rows"
```

---

## Task 7 — Verification, TODO close-out, PR

**Files:**
- Modify: `TODO.md`

- [ ] **Step 1: Mark the TODO item done.** In `TODO.md` Tier A, change the `- [ ] **CFL-reachability path algebra**` line to `- [x]` and append `Shipped — \`(wile algebra cfl)\`; design+impl \`plans/2026-06-05-cfl-reachability-{design,impl}.md\`.`

- [ ] **Step 2: Full suite + lint + coverage.**

Run: `make tutorial-test && make ci`
Expected: `All tutorial files passed` (incl. `cfl tour complete`), Scheme suite green (incl. `cfl` group), lint 0 issues, coverage gate met.

- [ ] **Step 3: Commit + push + open PR.**

```bash
git add TODO.md
git commit -m "docs(todo): close CFL-reachability — (wile algebra cfl) shipped"
git push -u origin feat/algebra-cfl
gh pr create --title "feat(algebra): (wile algebra cfl) — CFL-reachability" \
  --body "Closes wile-goast Track C4's wile-side gap. General CFG engine (typed kernels) + Dyck preset + Reps-Horwitz-Sagiv worklist solver. Canary proves CFL reachability is strictly more precise than Boolean reachability. Design+impl in plans/2026-06-05-cfl-reachability-*.md."
gh pr edit --add-reviewer copilot-pull-request-reviewer
```

- [ ] **Step 4: Dual review per `plans/CLAUDE.md`** — dispatch `/crosscheck:crosscheck all` on the diff; address Copilot + crosscheck findings; do NOT merge without explicit instruction.

---

## Self-Review (spec coverage)

- Grammar typed kernels + record + nonterminal/terminal derivation → Task 1. ✔
- Labeled-edge graph → Task 1. ✔
- Worklist solver (seed + 3 propagation rules) + query surface (`cfl-reachable?`/`-from`/`-pairs`/`cfl-derives?`) → Task 2. ✔
- The canary (matched `#t`, mismatched `#f`, under one solve) → Task 2 Step 1. ✔
- Dyck preset → Task 3. ✔
- Validation (terminal/nonterminal disjointness, defined start, RHS-nonterminal, dangling edge) + general non-Dyck grammar → Task 4. ✔
- Umbrella re-export → Task 5. ✔
- Quick-tour + reference + index tables (documented from day one) → Task 6. ✔
- Verification + TODO close-out + PR → Task 7. ✔

**Open implementer checkpoints (flagged inline, not placeholders):** (a) confirm `make-violation-reporter`'s exact protocol at `setoid.scm:102` and match the suite's `validate-*` idiom in Task 4 Step 3; (b) `dist/*/*/wile` glob resolves to the host `os/arch` build — substitute the concrete path if the glob is ambiguous.
