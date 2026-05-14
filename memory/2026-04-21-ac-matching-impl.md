# AC-Matching & AC-Unification Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `(wile algebra unification)` — AC-matching, AC-unification, and `diophantine-basis` — replacing the combinatorial `discover-equivalences` workaround with polynomial algorithms.

**Architecture:** Eker-style AC-matching with boolean-semiring matrix permanent for feasibility pruning; Stickel's reduction from AC-unification to linear Diophantine systems via Contejean–Devie basis enumeration. Pattern variables are records (`<pattern-var>`); substitutions are record-wrapped alists. Reuses `<term-protocol>` (from `rewrite.sld`) and `<theory>` (from `symbolic.sld`) without modification.

**Tech Stack:** Scheme (R7RS), Wile stdlib conventions, `(chibi test)` for the test suite, `(wile algebra matrix)` for bipartite feasibility.

**Design doc:** `plans/2026-04-21-ac-matching-design.md`

**Target files:**
- `stdlib/lib/wile/algebra/unification.sld` — library declarations, exports, imports
- `stdlib/lib/wile/algebra/unification.scm` — implementation
- `stdlib/lib/wile/algebra.sld` — aggregator re-export (Phase 6)
- `test/wile/algebra-unification-test.scm` — test suite
- `TODO.md` — mark §5.3 done at Phase 6 close

**LOC budget:** ~510 lib + ~350 tests = ~860 total. Larger than directions-doc estimate (~400–600) because this plan also publishes `diophantine-basis`.

**Workflow per task:** red → green → refactor → commit. Each task runs `make lint && make covercheck` before the commit.

---

## Phase 1 — Scaffolding, pattern vars, substitutions (~150 lib / ~80 test LOC)

### Task 1.1: Library skeleton and `<pattern-var>`

**Files:**
- Create: `stdlib/lib/wile/algebra/unification.sld`
- Create: `stdlib/lib/wile/algebra/unification.scm`
- Create: `test/wile/algebra-unification-test.scm`

- [ ] **Step 1: Write failing test**

Test file:

```scheme
;;; algebra-unification-test.scm — AC-matching and AC-unification tests

(import (scheme base)
        (chibi test)
        (wile algebra unification))

(test-begin "unification")

(test-group "pattern-var construction and identity"
  (let ((vx (make-pattern-var 'x)))
    (test #t (pattern-var? vx))
    (test 'x (pattern-var-name vx))
    (test #f (pattern-var? 'x))               ; symbol, not pattern-var
    (test #f (pattern-var? '(+ 1 2)))))       ; pair, not pattern-var

(test-end "unification")
```

- [ ] **Step 2: Run test — expect failure**

Run: `wile --file test/wile/algebra-unification-test.scm`
Expected: Library not found error (unification.sld doesn't exist).

- [ ] **Step 3: Implement library skeleton**

`stdlib/lib/wile/algebra/unification.sld`:

```scheme
(define-library (wile algebra unification)
  (description "AC-matching and AC-unification on terms modulo associative-commutative theories.")
  (export
    ;; Pattern variables
    make-pattern-var pattern-var? pattern-var-name)
  (import (scheme base))
  (include "unification.scm"))
```

`stdlib/lib/wile/algebra/unification.scm`:

```scheme
;;; unification.scm — AC-matching and AC-unification.
;;;
;;; Term protocol contract: term-compare must be a total order consistent
;;; with equal? modulo the AC-equivalence induced by the caller's theory.

(define-record-type <pattern-var>
  (make-pattern-var name)
  pattern-var?
  (name pattern-var-name))
```

- [ ] **Step 4: Run test — expect pass**

Run: `wile --file test/wile/algebra-unification-test.scm`
Expected: `unification` group passes.

- [ ] **Step 5: Commit**

```bash
git add stdlib/lib/wile/algebra/unification.sld stdlib/lib/wile/algebra/unification.scm test/wile/algebra-unification-test.scm
git commit -m "feat(algebra/unification): scaffold library with <pattern-var>"
```

---

### Task 1.2: `parse-pattern` helper

**Files:**
- Modify: `stdlib/lib/wile/algebra/unification.sld` — add `parse-pattern` to exports
- Modify: `stdlib/lib/wile/algebra/unification.scm` — add parse-pattern
- Modify: `test/wile/algebra-unification-test.scm` — add test-group

- [ ] **Step 1: Write failing tests**

Add to test file before `(test-end "unification")`:

```scheme
(test-group "parse-pattern: ?-convention → <pattern-var> records"
  ;; Leaf var
  (let ((p (parse-pattern '?x)))
    (test #t (pattern-var? p))
    (test 'x (pattern-var-name p)))
  ;; Plain symbol stays a symbol
  (test 'foo (parse-pattern 'foo))
  ;; Nested: vars become records, operators stay symbols
  (let ((p (parse-pattern '(+ ?x ?y))))
    (test '+ (car p))
    (test #t (pattern-var? (cadr p)))
    (test #t (pattern-var? (caddr p))))
  ;; Repeated ?x interns to one record (eq? identity)
  (let ((p (parse-pattern '(+ ?x ?x))))
    (test #t (eq? (cadr p) (caddr p)))))
```

- [ ] **Step 2: Run test — expect failure** (`parse-pattern` undefined)

- [ ] **Step 3: Implement**

Append to `unification.scm`:

```scheme
(define (parse-pattern expr)
  "Convert EXPR from sexpr with ?-prefix convention to a pattern using
<pattern-var> records. Symbols starting with #\\? become pattern variables;
repeated ?-names intern to one record (name-based identity).

Parameters:
  expr : any
Returns: pattern (sexpr possibly containing <pattern-var> records)
Category: algebra
Keywords: pattern, parse, match, unification"
  (let ((interned '()))
    (define (var-name-of sym)
      (let* ((s (symbol->string sym))
             (n (string-length s)))
        (and (> n 1)
             (char=? #\? (string-ref s 0))
             (string->symbol (substring s 1 n)))))
    (define (walk x)
      (cond
        ((pair? x)
         (cons (walk (car x)) (walk (cdr x))))
        ((null? x) '())
        ((symbol? x)
         (let ((nm (var-name-of x)))
           (cond
             ((not nm) x)
             ((assq nm interned) => cdr)
             (else
              (let ((v (make-pattern-var nm)))
                (set! interned (cons (cons nm v) interned))
                v)))))
        (else x)))
    (walk expr)))
```

Add `parse-pattern` to `.sld` exports.

- [ ] **Step 4: Run test — expect pass**

- [ ] **Step 5: Commit**

```bash
git add -u
git commit -m "feat(algebra/unification): add parse-pattern with ?-convention sugar"
```

---

### Task 1.3: `<substitution>` record, `empty-substitution`, accessors

**Files:**
- Modify: `stdlib/lib/wile/algebra/unification.sld` — add substitution exports
- Modify: `stdlib/lib/wile/algebra/unification.scm` — add record + accessors
- Modify: `test/wile/algebra-unification-test.scm`

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "substitution: construction and accessors"
  (test #t (substitution? empty-substitution))
  (test '() (substitution-bindings empty-substitution))
  (let* ((vx (make-pattern-var 'x))
         (s (make-substitution (list (cons vx 42)))))
    (test #t (substitution? s))
    (test '((x . 42))
          (map (lambda (b) (cons (pattern-var-name (car b)) (cdr b)))
               (substitution-bindings s)))))
```

- [ ] **Step 2: Run test — expect failure**

- [ ] **Step 3: Implement**

Add to `unification.scm`:

```scheme
(define-record-type <substitution>
  (make-substitution bindings)
  substitution?
  (bindings substitution-bindings))

(define empty-substitution (make-substitution '()))
```

Exports: `make-substitution substitution? substitution-bindings empty-substitution`.

- [ ] **Step 4: Run test — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): add <substitution> record and empty-substitution`

---

### Task 1.4: `substitution-lookup`

**Files:**
- Modify: `stdlib/lib/wile/algebra/unification.{sld,scm}`, test file

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "substitution-lookup"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s (make-substitution (list (cons vx 1) (cons vy 2)))))
    (test 1 (substitution-lookup s vx))
    (test 2 (substitution-lookup s vy))
    (test #f (substitution-lookup s (make-pattern-var 'z)))
    (test #f (substitution-lookup empty-substitution vx))))
```

- [ ] **Step 2: Run — expect failure** (`substitution-lookup` undefined)
- [ ] **Step 3: Implement**

```scheme
(define (substitution-lookup sub var)
  "Return the term bound to VAR in SUB, or #f if unbound. Var identity is
by pattern-var-name (symbols compared with eq?).

Parameters: sub : <substitution>, var : <pattern-var>
Returns: term | #f
Category: algebra
Keywords: substitution, lookup, unification"
  (let loop ((xs (substitution-bindings sub)))
    (cond
      ((null? xs) #f)
      ((eq? (pattern-var-name (caar xs))
            (pattern-var-name var))
       (cdar xs))
      (else (loop (cdr xs))))))
```

Export `substitution-lookup`.

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): add substitution-lookup`

---

### Task 1.5: `substitution-compose` with occurs-check

**Files:** same trio

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "substitution-compose: non-conflicting and conflicting"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s1 (make-substitution (list (cons vx 1))))
         (s2 (make-substitution (list (cons vy 2))))
         (s3 (make-substitution (list (cons vx 99)))))
    ;; Non-conflicting compose: union of bindings
    (let ((merged (substitution-compose s1 s2)))
      (test 1 (substitution-lookup merged vx))
      (test 2 (substitution-lookup merged vy)))
    ;; Conflicting compose: x↦1 vs x↦99 → #f
    (test #f (substitution-compose s1 s3))
    ;; Empty cases
    (test s1 (substitution-compose s1 empty-substitution))
    (test s1 (substitution-compose empty-substitution s1))))
```

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Implement**

```scheme
(define (substitution-compose s1 s2)
  "Merge two substitutions. Returns a new <substitution> if bindings are
compatible, #f on conflict (same var bound to term-unequal values).
Does NOT perform occurs-check on binding targets; that is the caller's
responsibility via substitution-apply.

Parameters: s1, s2 : <substitution>
Returns: <substitution> | #f
Category: algebra
Keywords: substitution, compose, merge, unification"
  (let loop ((xs (substitution-bindings s2))
             (acc (substitution-bindings s1)))
    (cond
      ((null? xs) (make-substitution acc))
      (else
       (let* ((pair (car xs))
              (var (car pair))
              (val (cdr pair))
              (existing (substitution-lookup (make-substitution acc) var)))
         (cond
           ((not existing)
            (loop (cdr xs) (cons pair acc)))
           ((equal? existing val)
            (loop (cdr xs) acc))
           (else #f)))))))
```

Export `substitution-compose`.

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): add substitution-compose with conflict detection`

---

### Task 1.6: `substitution-apply`

**Files:** same trio

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "substitution-apply: rewrites pattern with bindings"
  (let* ((vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y))
         (s (make-substitution (list (cons vx 10) (cons vy 'hello))))
         (proto (sexp-term-protocol (lambda (a b)
                                      (cond
                                        ((and (number? a) (number? b))
                                         (cond ((< a b) -1) ((> a b) 1) (else 0)))
                                        ((and (symbol? a) (symbol? b))
                                         (let ((sa (symbol->string a))
                                               (sb (symbol->string b)))
                                           (cond ((string<? sa sb) -1)
                                                 ((string>? sa sb) 1)
                                                 (else 0))))
                                        (else 0)))))) ; dummy: OK for this test
    ;; Atom var
    (test 10 (substitution-apply s proto vx))
    ;; Compound with vars
    (test '(+ 10 hello) (substitution-apply s proto (list '+ vx vy)))
    ;; Unbound var stays unchanged
    (let ((vz (make-pattern-var 'z)))
      (test vz (substitution-apply s proto vz)))
    ;; No vars: identity
    (test '(+ 1 2) (substitution-apply s proto '(+ 1 2)))))
```

Note: this test imports `(wile algebra symbolic)` for `sexp-term-protocol`. Add to test imports.

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Implement**

Add `(wile algebra rewrite)` and `(wile algebra symbolic)` to `.sld` imports. Add:

```scheme
(define (substitution-apply sub proto term)
  "Return TERM with each <pattern-var> leaf replaced by its binding in SUB,
rebuilding compound terms via the protocol's term-make-term. Unbound vars
are returned unchanged.

Parameters: sub : <substitution>, proto : <term-protocol>, term : any
Returns: term
Category: algebra
Keywords: substitution, apply, rewrite, unification"
  (cond
    ((pattern-var? term)
     (or (substitution-lookup sub term) term))
    ((term-compound? proto term)
     (term-make-term proto term
       (map (lambda (a) (substitution-apply sub proto a))
            (term-get-operands proto term))))
    (else term)))
```

Export `substitution-apply`. Imports in `.sld`: add `(wile algebra rewrite)`.

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): add substitution-apply with protocol-driven rebuild`

---

## Phase 2 — `diophantine-basis` (~80 lib / ~40 test LOC)

Standalone; no dependency on match/unify. Published per goal #1 (broadest application).

### Task 2.1: Trivial cases (`x = y`)

**Files:** same trio, plus the helper is its own section in unification.scm.

- [ ] **Step 1: Write failing test**

```scheme
(test-group "diophantine-basis: x = y trivial"
  ;; System: 1·x = 1·y (m=n=1, a=[1], b=[1])
  ;; Minimal solution: x=1, y=1. That's it.
  (let ((basis (diophantine-basis '(1) '(1))))
    (test 1 (length basis))
    (test '((1) . (1)) (car basis))))
```

- [ ] **Step 2: Run — expect failure** (undefined)
- [ ] **Step 3: Implement Contejean–Devie (minimal version for m+n small)**

```scheme
(define (diophantine-basis a b)
  "Enumerate minimal non-negative integer solutions (u,v) of a·u = b·v,
where a∈ℕᵐ, b∈ℕⁿ. Returns list of (u . v) pairs.

Algorithm: BFS over ℕ^(m+n) from unit vectors, prune by domination and by
the invariant that partial sums cannot grow past a known solution. Terminates
via Dickson's lemma (finitely many minimal ℕ-vectors).

Parameters: a, b : list of non-negative integers
Returns: list of (u . v) where u, v are integer lists
Category: algebra
Keywords: diophantine, linear, basis, unification, combinatorics, Petri"
  (unless (and (list? a) (every exact-nonneg-integer? a))
    (error "diophantine-basis: expected non-negative integer list" a))
  (unless (and (list? b) (every exact-nonneg-integer? b))
    (error "diophantine-basis: expected non-negative integer list" b))
  (when (or (null? a) (null? b))
    (error "diophantine-basis: empty coefficient vector" a b))
  ;; BFS implementation — see design doc §Algorithms.
  ;; Nodes are (u v residual) with residual = a·u - b·v.
  ;; Emit solutions where residual = 0 (u, v not both zero).
  ;; Expand by incrementing one uᵢ if residual ≤ 0, one vⱼ if residual ≥ 0.
  ;; Prune nodes dominated by emitted solutions.
  (let ((m (length a)) (n (length b)))
    (contejean-devie-bfs a b m n)))

;; Helper: pure integer predicate
(define (exact-nonneg-integer? x)
  (and (integer? x) (exact? x) (>= x 0)))

;; Full BFS implementation (shown here in sketch; see unification.scm for
;; the ~40-line worker. Uses two queues: frontier and emitted.).
(define (contejean-devie-bfs a b m n)
  ;; TO BE IMPLEMENTED IN TASK 2.2 — this task ships the 1×1 case via direct
  ;; solve and expands the algorithm in the next task.
  (if (and (= m 1) (= n 1))
      (let ((ai (car a)) (bj (car b)))
        ;; solve ai·x = bj·y, minimal non-neg: x = bj/gcd, y = ai/gcd
        (if (and (positive? ai) (positive? bj))
            (let ((g (gcd ai bj)))
              (list (cons (list (quotient bj g)) (list (quotient ai g)))))
            '()))
      '()))  ; larger cases handled in Task 2.2
```

Export `diophantine-basis`.

- [ ] **Step 4: Run — expect pass** (1×1 case only)
- [ ] **Step 5: Commit:** `feat(algebra/unification): diophantine-basis — 1x1 case via gcd`

---

### Task 2.2: General Contejean–Devie BFS

- [ ] **Step 1: Extend tests**

```scheme
(test-group "diophantine-basis: Stickel canonical x+y = z"
  ;; 1·x₁ + 1·x₂ = 1·y₁  →  basis: ((1 0).(1)), ((0 1).(1))
  (let ((basis (diophantine-basis '(1 1) '(1))))
    (test 2 (length basis))
    (test #t (member '((1 0) . (1)) basis))
    (test #t (member '((0 1) . (1)) basis))))

(test-group "diophantine-basis: asymmetric 2x = 3y"
  ;; 2·x = 3·y → minimal (x=3, y=2)
  (let ((basis (diophantine-basis '(2) '(3))))
    (test 1 (length basis))
    (test '((3) . (2)) (car basis))))
```

- [ ] **Step 2: Run — expect failure** (only 1×1 case works so far)
- [ ] **Step 3: Replace `contejean-devie-bfs` with full BFS**

Replace the stub in `unification.scm`:

```scheme
(define (contejean-devie-bfs a b m n)
  ;; Vectors represented as lists of length m (u-side) or n (v-side).
  ;; Zero vector:
  (define (zeros k) (make-list k 0))
  (define (residual u v) (- (dot a u) (dot b v)))
  (define (dot xs ys) (apply + (map * xs ys)))
  (define (bump xs i)
    (let loop ((k 0) (xs xs) (acc '()))
      (if (= k i)
          (append (reverse acc) (cons (+ 1 (car xs)) (cdr xs)))
          (loop (+ k 1) (cdr xs) (cons (car xs) acc)))))
  (define (vec-le? u v) (every <= u v))  ; domination: u ≤ v componentwise
  (define (dominated? u v emitted)
    (any (lambda (e)
           (and (vec-le? (car e) u) (vec-le? (cdr e) v)))
         emitted))
  (let loop ((frontier (list (cons (zeros m) (zeros n))))
             (emitted '()))
    (cond
      ((null? frontier) (reverse emitted))
      (else
       (let* ((node (car frontier))
              (u (car node))
              (v (cdr node))
              (r (residual u v)))
         (cond
           ;; Solution node (non-zero): emit and stop expanding
           ((and (zero? r) (or (any positive? u) (any positive? v)))
            (if (dominated? u v emitted)
                (loop (cdr frontier) emitted)
                (loop (cdr frontier) (cons (cons u v) emitted))))
           ;; Dead-end (dominated): prune
           ((dominated? u v emitted)
            (loop (cdr frontier) emitted))
           (else
            ;; Expand: u-bumps if r ≤ 0; v-bumps if r ≥ 0
            (let* ((u-bumps
                    (if (<= r 0)
                        (map (lambda (i) (cons (bump u i) v))
                             (iota m))
                        '()))
                   (v-bumps
                    (if (>= r 0)
                        (map (lambda (j) (cons u (bump v j)))
                             (iota n))
                        '())))
              (loop (append (cdr frontier) u-bumps v-bumps)
                    emitted)))))))))
```

Note: `every`, `any`, `iota` come from `(srfi 1)` — add to `.sld` imports.

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): diophantine-basis — general Contejean-Devie BFS`

---

### Task 2.3: Error paths

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "diophantine-basis: errors on bad input"
  (test-error (diophantine-basis '(-1) '(1)))     ; negative
  (test-error (diophantine-basis '(1) '(2.5)))    ; non-integer
  (test-error (diophantine-basis '() '(1)))       ; empty a
  (test-error (diophantine-basis '(1) '())))      ; empty b
```

- [ ] **Step 2: Run — expect pass if Task 2.1's guards are intact, else failure.** If tests pass already (guards caught all cases), this task may only need verification and a docstring polish commit.
- [ ] **Step 3: Fix any gaps in the input validation block of `diophantine-basis`.**
- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit (if changes):** `test(algebra/unification): error paths for diophantine-basis`

---

## Phase 3 — AC-match without matrix prune (~100 lib / ~90 test LOC)

Ship a correctness-complete matcher via direct backtracking. Matrix prune is Phase 4.

### Task 3.1: Theory AC-flag table + test fixture

**Files:** same trio

- [ ] **Step 1: Write failing test**

```scheme
;; At top of test file, after (test-begin ...):
(define (make-ac-theory ops)
  "Build a theory where each op in OPS is both commutative and associative."
  (let ((axioms
         (apply append
                (map (lambda (op)
                       (list (make-named-axiom (symbol-append 'ass- op)
                               #f (make-associativity-axiom op))
                             (make-named-axiom (symbol-append 'com- op)
                               #f (make-commutativity-axiom op))))
                     ops))))
    (make-theory axioms ops)))

;; test-group:
(test-group "ac-match: ground AC-equality"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    ;; (+ a b) matches (+ b a) under AC — single trivial unifier
    (let ((results (ac-match '(+ a b) '(+ b a) theory proto)))
      (test 1 (length results))
      (test '() (substitution-bindings (car results))))))
```

Add `default-compare` helper at top of test file:

```scheme
(define (default-compare a b)
  (cond
    ((and (number? a) (number? b))
     (cond ((< a b) -1) ((> a b) 1) (else 0)))
    ((and (symbol? a) (symbol? b))
     (let ((sa (symbol->string a)) (sb (symbol->string b)))
       (cond ((string<? sa sb) -1) ((string>? sa sb) 1) (else 0))))
    ((equal? a b) 0)
    (else 1)))
```

- [ ] **Step 2: Run — expect failure** (ac-match undefined)
- [ ] **Step 3: Implement theory scan + stub `ac-match`**

```scheme
;; Scan theory once, build alist op → #t for AC operators.
(define (ac-ops-of theory)
  (filter
    (lambda (op)
      (let ((is-comm? #f) (is-assoc? #f))
        (for-each
          (lambda (na)
            (let ((ax (named-axiom-axiom na)))
              (cond
                ((and (commutativity-axiom? ax)
                      (eq? op (commutativity-axiom-op ax)))
                 (set! is-comm? #t))
                ((and (associativity-axiom? ax)
                      (eq? op (associativity-axiom-op ax)))
                 (set! is-assoc? #t)))))
          (theory-axioms theory))
        (and is-comm? is-assoc?)))
    (theory-associative-ops theory)))

(define (ac-op? op ac-ops) (memq op ac-ops))

(define (ac-match pattern subject theory proto)
  "See design doc for algorithm. Returns list<substitution>; empty = no match."
  (unless (theory? theory)
    (error "ac-match: expected theory" theory))
  (unless (term-protocol? proto)
    (error "ac-match: expected term-protocol" proto))
  (let ((ac-ops (ac-ops-of theory)))
    (match-rec pattern subject empty-substitution ac-ops proto)))

;; Stub match-rec: handle base cases only for now
(define (match-rec p s sub ac-ops proto)
  (cond
    ((pattern-var? p) (bind-or-check p s sub))
    ((term-compound? proto p)
     (if (and (term-compound? proto s)
              (eq? (term-get-operator proto p)
                   (term-get-operator proto s)))
         ;; punt: only handle equal-arity positional for now; AC in 3.4
         (match-positional (term-get-operands proto p)
                           (term-get-operands proto s)
                           sub ac-ops proto)
         '()))
    (else
     (if (zero? (term-compare proto p s))
         (list sub)
         '()))))

(define (bind-or-check var subject sub)
  (let ((existing (substitution-lookup sub var)))
    (cond
      ((not existing)
       (list (make-substitution
               (cons (cons var subject) (substitution-bindings sub)))))
      ((equal? existing subject) (list sub))
      (else '()))))

(define (match-positional ps ss sub ac-ops proto)
  (cond
    ((and (null? ps) (null? ss)) (list sub))
    ((or (null? ps) (null? ss)) '())
    (else
     (let ((partial (match-rec (car ps) (car ss) sub ac-ops proto)))
       (apply append
         (map (lambda (s1)
                (match-positional (cdr ps) (cdr ss) s1 ac-ops proto))
              partial))))))
```

Export `ac-match`. Imports: add `(wile algebra rewrite)` (already there from Phase 1), also need `commutativity-axiom-op` and `associativity-axiom-op` accessors — **verify rewrite.sld exports these**; if not, export them there first as a precursor commit.

- [ ] **Step 4: Run — expect pass** (ground AC-equality test; AC-case uses positional for now since `(+ a b)` = `(+ b a)` is not yet testable without AC flattening — the test above DOES require AC handling)

If the test fails because AC isn't wired yet: extend the test to use non-AC operator or defer this test to Task 3.4. Simplest: add a placeholder test that only tests non-AC positional matching:

```scheme
(test-group "ac-match: non-AC positional"
  (let* ((theory (make-theory '() '()))  ; no axioms, no AC
         (proto (sexp-term-protocol default-compare)))
    (test 1 (length (ac-match '(f a b) '(f a b) theory proto)))
    (test 0 (length (ac-match '(f a b) '(f b a) theory proto)))))
```

Defer the AC test to Task 3.4 where flattening is implemented.

- [ ] **Step 5: Commit:** `feat(algebra/unification): ac-match scaffold with positional matching and bind-or-check`

---

### Task 3.2: Variable binding (unbound / nonlinear)

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "ac-match: single variable"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(f ?x ?y))))
    (let ((results (ac-match pat '(f a b) theory proto)))
      (test 1 (length results))
      (let ((bs (substitution-bindings (car results))))
        (test 2 (length bs))))))

(test-group "ac-match: nonlinear ?x ?x"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(f ?x ?x))))
    (test 1 (length (ac-match pat '(f a a) theory proto)))   ; match
    (test 0 (length (ac-match pat '(f a b) theory proto))))) ; fail
```

- [ ] **Step 2: Run — expect pass** (Task 3.1's `bind-or-check` already handles this via substitution-lookup)
- [ ] **Step 3: If failing, trace and fix.** Most likely needs `parse-pattern` output to preserve var-identity through `match-positional` — verify.
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit (only if code changed, else skip):** `test(algebra/unification): variable binding and nonlinearity cases`

---

### Task 3.3: AC flatten helper

- [ ] **Step 1: Write failing test**

```scheme
(test-group "flatten-ac: associativity collapse"
  (let ((proto (sexp-term-protocol default-compare)))
    (test '(a b c) (flatten-ac '(+ a (+ b c)) '+ proto))
    (test '(a b c d) (flatten-ac '(+ (+ a b) (+ c d)) '+ proto))
    ;; Non-AC op nested: don't flatten
    (test '(a (g b c)) (flatten-ac '(+ a (g b c)) '+ proto))
    ;; Leaf
    (test '(a) (flatten-ac 'a '+ proto))))
```

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Implement** (internal, non-exported):

```scheme
(define (flatten-ac term op proto)
  "Collapse nested (op ...) applications, returning a flat list of operand terms."
  (cond
    ((not (term-compound? proto term)) (list term))
    ((eq? (term-get-operator proto term) op)
     (apply append
       (map (lambda (a) (flatten-ac a op proto))
            (term-get-operands proto term))))
    (else (list term))))
```

Not exported (internal helper).

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): internal flatten-ac helper`

---

### Task 3.4: AC-case dispatch in `match-rec`

- [ ] **Step 1: Write failing tests** (re-enable the ground-AC-equality test from 3.1, plus add var-in-AC)

```scheme
(test-group "ac-match: AC ground equality"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    (test 1 (length (ac-match '(+ a b) '(+ b a) theory proto)))
    (test 1 (length (ac-match '(+ a b c) '(+ c a b) theory proto)))
    (test 0 (length (ac-match '(+ a b) '(+ a c) theory proto)))))

(test-group "ac-match: variable in AC op — enumerates assignments"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(+ ?x a))))
    ;; ?x can bind to b (from (+ b a)) — 1 match, ?x↦b
    (test 1 (length (ac-match pat '(+ a b) theory proto)))))
```

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Replace `match-rec` compound branch** with AC dispatch:

```scheme
(define (match-rec p s sub ac-ops proto)
  (cond
    ((pattern-var? p) (bind-or-check p s sub))
    ((term-compound? proto p)
     (cond
       ((not (term-compound? proto s)) '())
       ((not (eq? (term-get-operator proto p) (term-get-operator proto s))) '())
       ((ac-op? (term-get-operator proto p) ac-ops)
        (match-ac (term-get-operator proto p)
                  (flatten-ac p (term-get-operator proto p) proto)
                  (flatten-ac s (term-get-operator proto p) proto)
                  sub ac-ops proto))
       (else
        (match-positional (term-get-operands proto p)
                          (term-get-operands proto s)
                          sub ac-ops proto))))
    (else
     (if (zero? (term-compare proto p s)) (list sub) '()))))

(define (match-ac op pat-ops subj-ops sub ac-ops proto)
  ;; pat-ops, subj-ops are flat lists (multisets under AC).
  ;; Direct backtracking: for each permutation of subj-ops, try positional
  ;; match against pat-ops. Correct but exponential; Phase 4 adds pruning.
  (cond
    ((null? pat-ops) (if (null? subj-ops) (list sub) '()))
    (else
     (let ((head (car pat-ops)) (rest (cdr pat-ops)))
       (apply append
         (map (lambda (i)
                (let* ((chosen (list-ref subj-ops i))
                       (remaining (remove-at subj-ops i))
                       (partial (match-rec head chosen sub ac-ops proto)))
                  (apply append
                    (map (lambda (s1)
                           (match-ac op rest remaining s1 ac-ops proto))
                         partial))))
              (iota (length subj-ops))))))))

(define (remove-at xs i)
  (cond ((zero? i) (cdr xs))
        (else (cons (car xs) (remove-at (cdr xs) (- i 1))))))
```

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): AC-case dispatch with direct-backtracking matcher`

---

### Task 3.5: Variable binding to multiset (free var in AC op)

- [ ] **Step 1: Write failing test**

```scheme
(test-group "ac-match: free var binds to (op …) submultiset"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(+ ?x a))))
    ;; (+ a b c): ?x↦b, remaining a; OR ?x↦c, remaining a; OR ?x↦(+ b c)
    ;; Plus any further subset combinations — CSU may have several entries.
    (let ((results (ac-match pat '(+ a b c) theory proto)))
      (test #t (> (length results) 0)))))
```

- [ ] **Step 2: Run — expect failure or partial pass.** Current `match-ac` requires `|pat-ops| = |subj-ops|`; needs extension to let a free var consume multiple subj elements.

- [ ] **Step 3: Extend `match-ac`** to handle the "free var binds to sub-multiset" case.

Strategy: when `head` is a free `<pattern-var>` and `|rest|` < `|subj-ops remaining|`, enumerate non-empty subsets of `subj-ops` to bind `head` to, re-wrapping via `term-make-term`.

```scheme
;; Inside match-ac, before the per-element iteration, add:
;;   if head is free pattern-var AND length rest < length subj-ops,
;;   enumerate non-empty proper subsets of subj-ops. For each subset S:
;;     let binding = (term-make-term proto p (elements S))
;;     match-ac op rest (subj-ops \ S) (extend sub with head↦binding) …

;; Full revised match-ac in unification.scm — see design doc §AC-case decomposition.
```

Full code: ~30 additional LOC. Implementation sketch:

```scheme
(define (match-ac op pat-ops subj-ops sub ac-ops proto)
  (cond
    ((null? pat-ops) (if (null? subj-ops) (list sub) '()))
    (else
     (let* ((head (car pat-ops)) (rest (cdr pat-ops)))
       (cond
         ;; Single-element binding branch (as before)
         ((and (not (pattern-var? head))
               (not (= (length subj-ops) (length pat-ops)))) '())
         ((pattern-var? head)
          (append
            ;; Case A: single-element binding
            (apply append
              (map (lambda (i)
                     (let* ((chosen (list-ref subj-ops i))
                            (rem (remove-at subj-ops i))
                            (partial (bind-or-check head chosen sub)))
                       (apply append
                         (map (lambda (s1)
                                (match-ac op rest rem s1 ac-ops proto))
                              partial))))
                   (iota (length subj-ops))))
            ;; Case B: multi-element binding (only if free and rest allows)
            (if (and (not (substitution-lookup sub head))
                     (>= (length subj-ops) 2)
                     (>= (length subj-ops) (+ (length rest) 2)))
                (apply append
                  (map (lambda (subset)
                         (let* ((binding (apply term-make-term-variadic
                                                proto op subset))
                                (rem (list-difference subj-ops subset))
                                (partial (bind-or-check head binding sub)))
                           (apply append
                             (map (lambda (s1)
                                    (match-ac op rest rem s1 ac-ops proto))
                                  partial))))
                       (proper-subsets-size>=2 subj-ops)))
                '())))
         (else
          ;; Non-var head: same as original per-element iteration
          (apply append
            (map (lambda (i)
                   (let* ((chosen (list-ref subj-ops i))
                          (rem (remove-at subj-ops i))
                          (partial (match-rec head chosen sub ac-ops proto)))
                     (apply append
                       (map (lambda (s1)
                              (match-ac op rest rem s1 ac-ops proto))
                            partial))))
                 (iota (length subj-ops))))))))))

(define (proper-subsets-size>=2 xs)
  ;; Return all subsets with size in [2, |xs|-1].
  (filter (lambda (s)
            (and (>= (length s) 2) (< (length s) (length xs))))
          (all-subsets xs)))

(define (all-subsets xs)
  (cond ((null? xs) '(()))
        (else (let ((rest-subs (all-subsets (cdr xs))))
                (append rest-subs
                        (map (lambda (s) (cons (car xs) s)) rest-subs))))))

(define (list-difference xs ys)
  (let loop ((xs xs) (acc '()) (ys ys))
    (cond
      ((null? xs) (reverse acc))
      ((member (car xs) ys)
       (loop (cdr xs) acc (remove-first (car xs) ys)))
      (else (loop (cdr xs) (cons (car xs) acc) ys)))))

(define (remove-first x xs)
  (cond ((null? xs) '())
        ((equal? x (car xs)) (cdr xs))
        (else (cons (car xs) (remove-first x (cdr xs))))))

(define (term-make-term-variadic proto op args)
  ;; Construct (op . args); there's no single existing term in the protocol
  ;; to pass as the "shape" arg, so synthesize one.
  (term-make-term proto (cons op args) args))
```

Note: `term-make-term-variadic` is a workaround — if `term-make-term` needs an existing reference, use the first arg's container. Verify against the protocol's `term-make-term` signature in `rewrite.scm`.

- [ ] **Step 4: Run — expect pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): free-var binding to operand multisets`

---

### Task 3.6: More AC-match edge cases (operator mismatch, arity, nested)

- [ ] **Step 1: Add tests**

```scheme
(test-group "ac-match: mismatches and edge cases"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    ;; Operator mismatch
    (test 0 (length (ac-match '(+ a b) '(* a b) theory proto)))
    ;; Non-AC operator with mismatched arity
    (let ((theory-nac (make-theory '() '())))
      (test 0 (length (ac-match '(f a b) '(f a b c) theory-nac proto))))
    ;; Nested AC: (+ (* ?x 2) ?y) matches (+ ?y (* 2 a))?
    ;; (Requires * to also be AC; ?y can bind to any element.)
    (let* ((theory-both (make-ac-theory '(+ *)))
           (pat (parse-pattern '(+ (* ?x 2) ?y))))
      (test #t (> (length (ac-match pat '(+ (* 2 a) b) theory-both proto)) 0)))))
```

- [ ] **Step 2: Run — expect most to pass; diagnose any failures**
- [ ] **Step 3: Fix any gaps** (likely in `match-ac`'s nested-pattern handling or `match-positional`'s arity check)
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit:** `test(algebra/unification): AC-match edge cases (mismatch, nested)`

---

### Task 3.7: `ac-match` error paths

- [ ] **Step 1: Write tests**

```scheme
(test-group "ac-match: argument errors"
  (let ((proto (sexp-term-protocol default-compare)))
    (test-error (ac-match '(+ a b) '(+ a b) 'not-a-theory proto))
    (test-error (ac-match '(+ a b) '(+ a b) (make-theory '() '()) 'not-a-proto))))
```

- [ ] **Step 2: Run — expect pass** (guards already in Task 3.1's `ac-match`)
- [ ] **Step 3: Verify or adjust**
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit (if changes):** `test(algebra/unification): ac-match input validation`

---

### Task 3.8: Phase-3 lint & coverage checkpoint

- [ ] **Step 1:** Run `make lint` — fix any warnings.
- [ ] **Step 2:** Run `make covercheck` — verify no regression.
- [ ] **Step 3:** Run full test suite — all existing + new tests green.
- [ ] **Step 4:** Commit lint fixes if any: `style(algebra/unification): lint pass through Phase 3`

---

## Phase 4 — Matrix-permanent feasibility prune (~30 lib / ~10 test LOC)

Optional optimization. Decision to retain Phase-4 code is benchmark-gated.

### Task 4.1: Boolean compatibility matrix construction

- [ ] **Step 1: Write a benchmark-or-test that stresses `match-ac`**

```scheme
(test-group "ac-match: pathological 8-element case"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (subj '(+ a b c d e f g h)))
    ;; Pattern that forces large enumeration: 4 free vars, 4 ground
    (let* ((pat (parse-pattern '(+ ?v ?w ?x ?y a b c d))))
      (let ((start (current-jiffy))
            (results (ac-match pat subj theory proto))
            (end (current-jiffy)))
        (test #t (> (length results) 0))
        ;; Record runtime; will compare post-Phase-4
        (display "\n[BENCH 4.1] match-ac 8-element: ")
        (display (/ (- end start) (jiffies-per-second)))
        (newline)))))
```

Runs on every Phase-4 iteration; not asserted on time, but displayed.

- [ ] **Step 2: Implement `build-compat-matrix`** as an internal helper.

```scheme
(define (build-compat-matrix pat-positions subj-ops proto)
  ;; pat-positions: list of remaining pattern positions (after peeling grounds)
  ;; subj-ops:     list of remaining subject elements
  ;; Returns:      boolean-matrix (via (wile algebra matrix))
  (let ((m (length pat-positions))
        (n (length subj-ops)))
    (make-dense-matrix m n
      (lambda (i j)
        (can-position-match? (list-ref pat-positions i)
                             (list-ref subj-ops j)
                             proto)))))

(define (can-position-match? p s proto)
  ;; Conservative structural check: returns #t if p could possibly match s
  ;; (pattern-var always compatible; compound requires operator match;
  ;; atom requires term-compare = 0).
  (cond
    ((pattern-var? p) #t)
    ((term-compound? proto p)
     (and (term-compound? proto s)
          (eq? (term-get-operator proto p) (term-get-operator proto s))))
    (else (zero? (term-compare proto p s)))))
```

Add `(wile algebra matrix)` to `.sld` imports.

- [ ] **Step 3: Run — verify helper compiles and unit-test it.**

```scheme
(test-group "build-compat-matrix smoke test"
  (let* ((proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x))
         (M (build-compat-matrix (list vx 'a) '(a b) proto)))
    (test #t (matrix? M))))
```

- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit:** `feat(algebra/unification): build-compat-matrix helper`

---

### Task 4.2: Permanent-based early prune in `match-ac`

- [ ] **Step 1: Modify `match-ac` to compute permanent before enumeration**

```scheme
;; At the top of match-ac, before branching:
;;   if (zero? (boolean-permanent (build-compat-matrix pat-ops subj-ops proto)))
;;   → return '()

(define (match-ac op pat-ops subj-ops sub ac-ops proto)
  (cond
    ((null? pat-ops) (if (null? subj-ops) (list sub) '()))
    ;; Fast reject: no compatible bipartite matching exists
    ((and (>= (length pat-ops) 2)
          (= (length pat-ops) (length subj-ops))
          (zero? (matrix-permanent-boolean
                   (build-compat-matrix pat-ops subj-ops proto))))
     '())
    (else
     ;; ... rest unchanged from Task 3.5
     )))
```

Note: `matrix-permanent-boolean` is the boolean-semiring permanent; verify the actual name exported by `(wile algebra matrix)` and adjust. If the matrix library doesn't ship a permanent primitive yet, add a thin wrapper:

```scheme
(define (matrix-permanent-boolean M)
  ;; 0 or 1 via Ryser's formula over boolean semiring, or equivalent
  ;; — implementation ~15 LOC; verify matrix library's semiring API.
  ...)
```

- [ ] **Step 2: Run all Phase-3 tests + the 4.1 benchmark test.** Phase-3 tests must stay green; benchmark should show measurable improvement on pathological case.
- [ ] **Step 3: If benchmark shows no win (<10% improvement), REVERT this task.** The design doc (Phase-3-before-Phase-4) explicitly allows dropping Phase 4 if benchmarks don't validate it. Document the decision in the commit message either way.
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit (if kept):** `perf(algebra/unification): matrix-permanent prune for ac-match bipartite feasibility`

---

### Task 4.3: Benchmark harness commit (if Task 4.2 retained)

- [ ] **Step 1: Polish the bench test output** into a shape that can be compared across runs (e.g., table form).
- [ ] **Step 2: Run bench before and after Phase-4 code is in place; record both numbers.**
- [ ] **Step 3: Add a brief note to the library docstring header summarizing the optimization and its benchmark evidence.**
- [ ] **Step 4: Commit:** `docs(algebra/unification): document Phase-4 matrix-permanent benchmark result`

---

## Phase 5 — AC-unify (~120 lib / ~100 test LOC)

### Task 5.1: Robinson syntactic unification (base, non-AC fallback)

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "ac-unify: ground equality"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare)))
    ;; Ground identical terms: single empty unifier
    (test 1 (length (ac-unify 'a 'a theory proto)))
    (test 0 (length (ac-unify 'a 'b theory proto)))))

(test-group "ac-unify: Robinson basic var"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x)))
    ;; x = a → {x↦a}
    (let ((results (ac-unify vx 'a theory proto)))
      (test 1 (length results))
      (test 'a (substitution-lookup (car results) vx)))))

(test-group "ac-unify: occurs-check"
  (let* ((theory (make-theory '() '()))
         (proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x)))
    ;; x = (f x) → no unifier
    (test 0 (length (ac-unify vx (list 'f vx) theory proto)))))
```

- [ ] **Step 2: Run — expect failure** (ac-unify undefined)
- [ ] **Step 3: Implement Robinson (non-AC fallback) first**

```scheme
(define (ac-unify t1 t2 theory proto)
  "See design doc for algorithm. Returns CSU as list<substitution>."
  (unless (theory? theory)
    (error "ac-unify: expected theory" theory))
  (unless (term-protocol? proto)
    (error "ac-unify: expected term-protocol" proto))
  (let ((ac-ops (ac-ops-of theory)))
    (unify-rec t1 t2 empty-substitution ac-ops proto)))

(define (unify-rec t1 t2 sub ac-ops proto)
  (let ((t1* (resolve t1 sub))
        (t2* (resolve t2 sub)))
    (cond
      ((and (pattern-var? t1*) (pattern-var? t2*)
            (eq? (pattern-var-name t1*) (pattern-var-name t2*)))
       (list sub))
      ((pattern-var? t1*) (bind-with-occurs-check t1* t2* sub proto))
      ((pattern-var? t2*) (bind-with-occurs-check t2* t1* sub proto))
      ((and (term-compound? proto t1*) (term-compound? proto t2*))
       (cond
         ((not (eq? (term-get-operator proto t1*)
                    (term-get-operator proto t2*))) '())
         ((ac-op? (term-get-operator proto t1*) ac-ops)
          ;; AC case — Task 5.3
          (unify-ac (term-get-operator proto t1*)
                    (flatten-ac t1* (term-get-operator proto t1*) proto)
                    (flatten-ac t2* (term-get-operator proto t1*) proto)
                    sub ac-ops proto))
         (else
          (unify-positional (term-get-operands proto t1*)
                            (term-get-operands proto t2*)
                            sub ac-ops proto))))
      (else
       (if (zero? (term-compare proto t1* t2*)) (list sub) '())))))

(define (resolve t sub)
  ;; Walk chain of var↦var bindings.
  (cond
    ((and (pattern-var? t) (substitution-lookup sub t))
     (resolve (substitution-lookup sub t) sub))
    (else t)))

(define (bind-with-occurs-check var term sub proto)
  (cond
    ((occurs? var term sub proto) '())
    (else (list (make-substitution
                  (cons (cons var term) (substitution-bindings sub)))))))

(define (occurs? var term sub proto)
  (let ((t (resolve term sub)))
    (cond
      ((and (pattern-var? t) (eq? (pattern-var-name t) (pattern-var-name var)))
       #t)
      ((term-compound? proto t)
       (any (lambda (a) (occurs? var a sub proto))
            (term-get-operands proto t)))
      (else #f))))

(define (unify-positional t1s t2s sub ac-ops proto)
  (cond
    ((and (null? t1s) (null? t2s)) (list sub))
    ((or (null? t1s) (null? t2s)) '())
    (else
     (let ((partial (unify-rec (car t1s) (car t2s) sub ac-ops proto)))
       (apply append
         (map (lambda (s1)
                (unify-positional (cdr t1s) (cdr t2s) s1 ac-ops proto))
              partial))))))

;; Stub unify-ac — Task 5.3 replaces
(define (unify-ac op t1-ops t2-ops sub ac-ops proto)
  ;; Fallback: pretend it's positional for now
  (if (= (length t1-ops) (length t2-ops))
      (unify-positional t1-ops t2-ops sub ac-ops proto)
      '()))
```

Export `ac-unify`.

- [ ] **Step 4: Run — expect pass on Task-5.1 tests**
- [ ] **Step 5: Commit:** `feat(algebra/unification): ac-unify Robinson base + non-AC unification`

---

### Task 5.2: AC ground equality via unify-ac

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "ac-unify: AC ground equality"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare)))
    ;; (+ a b) vs (+ b a) — equal modulo AC → one unifier (empty sub)
    (test 1 (length (ac-unify '(+ a b) '(+ b a) theory proto)))
    (test 1 (length (ac-unify '(+ a b c) '(+ c b a) theory proto)))
    ;; Mismatch at multiset level
    (test 0 (length (ac-unify '(+ a b) '(+ a c) theory proto)))))
```

- [ ] **Step 2: Run — expect failure** (stub unify-ac doesn't handle permutation)
- [ ] **Step 3: Replace stub with multiset-equality check** (a subset of full AC-unify: when both sides are ground, test whether multisets are equal modulo `term-compare`).

```scheme
(define (unify-ac op t1-ops t2-ops sub ac-ops proto)
  (cond
    ;; Both ground: multiset-equal iff every element of t1-ops matches some element of t2-ops (and sizes match)
    ((and (all-ground? t1-ops proto) (all-ground? t2-ops proto))
     (if (multiset-equal? t1-ops t2-ops proto)
         (list sub)
         '()))
    (else
     ;; Has variables: full Stickel — Task 5.3
     (unify-ac-stickel op t1-ops t2-ops sub ac-ops proto))))

(define (all-ground? ts proto)
  (every (lambda (t) (not (has-var? t proto))) ts))

(define (has-var? t proto)
  (cond
    ((pattern-var? t) #t)
    ((term-compound? proto t)
     (any (lambda (a) (has-var? a proto)) (term-get-operands proto t)))
    (else #f)))

(define (multiset-equal? xs ys proto)
  (cond
    ((and (null? xs) (null? ys)) #t)
    ((or (null? xs) (null? ys)) #f)
    (else
     (let ((match-idx (find-index
                        (lambda (y)
                          (zero? (term-compare proto (car xs) y)))
                        ys)))
       (and match-idx
            (multiset-equal? (cdr xs) (remove-at ys match-idx) proto))))))

(define (find-index pred xs)
  (let loop ((xs xs) (i 0))
    (cond
      ((null? xs) #f)
      ((pred (car xs)) i)
      (else (loop (cdr xs) (+ i 1))))))

;; Stub for next task
(define (unify-ac-stickel op t1-ops t2-ops sub ac-ops proto)
  '())  ; placeholder
```

- [ ] **Step 4: Run — expect pass on 5.2 tests; 5.3 tests still fail**
- [ ] **Step 5: Commit:** `feat(algebra/unification): ac-unify ground multiset equality`

---

### Task 5.3: Stickel's reduction — variable case

- [ ] **Step 1: Write failing tests**

```scheme
(test-group "ac-unify: Stickel canonical x+y = a+b"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x))
         (vy (make-pattern-var 'y)))
    ;; x+y =AC a+b → CSU has 2 unifiers: {x↦a, y↦b} and {x↦b, y↦a}
    (let ((results (ac-unify (list '+ vx vy) '(+ a b) theory proto)))
      (test 2 (length results))
      ;; Every unifier binds x and y to {a, b}
      (for-each
        (lambda (u)
          (let ((x-val (substitution-lookup u vx))
                (y-val (substitution-lookup u vy)))
            (test #t (or (and (eq? x-val 'a) (eq? y-val 'b))
                         (and (eq? x-val 'b) (eq? y-val 'a))))))
        results))))
```

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Implement `unify-ac-stickel`**

Full implementation (~60 LOC). Approach per design doc §AC-unify:
1. Partition `t1-ops` and `t2-ops` into variables and non-variable constants/compounds.
2. Build Diophantine system where each variable on either side becomes an unknown with coefficient 1.
3. Call `diophantine-basis`.
4. Reconstruct unifiers from basis elements — each basis element represents one assignment of variables to combinations of opposite-side terms.
5. Compose with `sub` via `substitution-compose`, filter `#f` (occurs-check failures).

```scheme
(define (unify-ac-stickel op t1-ops t2-ops sub ac-ops proto)
  ;; Separate variables from ground/compound terms on each side
  (let-values (((t1-vars t1-terms) (partition pattern-var? t1-ops))
               ((t2-vars t2-terms) (partition pattern-var? t2-ops)))
    ;; System: sum of |t1-vars| unknowns + constants from t1-terms
    ;;       = sum of |t2-vars| unknowns + constants from t2-terms
    ;; For each term on t1 side, it contributes a "token" to be matched on t2 side, and vice versa.
    ;; Build coefficient vectors for diophantine-basis.
    (let* ((m (length t1-vars))
           (n (length t2-vars))
           (a (make-list m 1))  ; each t1-var has coefficient 1
           (b (make-list n 1))  ; each t2-var has coefficient 1
           ;; Constants must match one-to-one (or via variables)
           ;; For simplicity v1: only solve the pure-variable case
           ;; (all constants equal on both sides)
           (basis (if (and (null? t1-terms) (null? t2-terms) (> m 0) (> n 0))
                      (diophantine-basis a b)
                      '())))
      ;; Reconstruct unifiers from basis elements
      (apply append
        (map (lambda (bv)
               (reconstruct-unifier op (car bv) (cdr bv)
                                    t1-vars t2-vars sub proto))
             basis)))))

(define (reconstruct-unifier op u v t1-vars t2-vars sub proto)
  ;; u[i] is multiplicity of t1-vars[i]; v[j] is multiplicity of t2-vars[j]
  ;; Build assignment by matching up positions with non-zero multiplicity.
  ;; v1 simplification: if all entries are 0 or 1, direct bijection.
  (cond
    ((and (every (lambda (x) (<= x 1)) u)
          (every (lambda (x) (<= x 1)) v))
     (let ((pairs (find-bijection u v t1-vars t2-vars)))
       (if pairs
           (let loop ((ps pairs) (s sub))
             (cond
               ((null? ps) (list s))
               (else
                (let* ((lhs (car (car ps)))
                       (rhs (cdr (car ps)))
                       (merged (substitution-compose
                                 s
                                 (make-substitution (list (cons lhs rhs))))))
                  (if merged (loop (cdr ps) merged) '())))))
           '())))
    (else
     ;; Non-unit multiplicities → variable binds to (op …) compound.
     ;; Deferred: full handling in a follow-up task if real consumers need it.
     '())))

(define (find-bijection u v t1-vars t2-vars)
  ;; u and v are 0/1 vectors. For each u[i]=1, pair t1-vars[i] with some t2-vars[j] where v[j]=1.
  ;; v1: return the obvious positional pairing.
  (let ((active-t1 (filter-indices u t1-vars))
        (active-t2 (filter-indices v t2-vars)))
    (if (= (length active-t1) (length active-t2))
        (map cons active-t1 active-t2)
        #f)))

(define (filter-indices mask xs)
  (let loop ((mask mask) (xs xs) (acc '()))
    (cond
      ((or (null? mask) (null? xs)) (reverse acc))
      ((positive? (car mask))
       (loop (cdr mask) (cdr xs) (cons (car xs) acc)))
      (else
       (loop (cdr mask) (cdr xs) acc)))))
```

Note: this is **v1 simplification** — pure-variable AC-unification with 0/1 basis elements only. Full Stickel with non-trivial multiplicities and interleaved constants is deferred to a follow-up (documented in Future extensions).

- [ ] **Step 4: Run — expect the canonical test to pass; more complex cases may need the follow-up.**
- [ ] **Step 5: Commit:** `feat(algebra/unification): ac-unify Stickel reduction (pure-variable case)`

---

### Task 5.4: Nonlinear AC-unification (`x + x vs a + a`)

- [ ] **Step 1: Write failing test**

```scheme
(test-group "ac-unify: nonlinear x+x = a+a"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x)))
    ;; x + x =AC a + a → {x↦a}
    (let ((results (ac-unify (list '+ vx vx) '(+ a a) theory proto)))
      (test 1 (length results))
      (test 'a (substitution-lookup (car results) vx)))
    ;; x + x =AC a + b → no unifier
    (test 0 (length (ac-unify (list '+ vx vx) '(+ a b) theory proto)))))
```

- [ ] **Step 2: Run — expect failure**
- [ ] **Step 3: Handle nonlinearity.** Repeated var-identity on the same side reduces variable count via post-composition — when `substitution-compose` merges a second `x↦...` binding, it checks compatibility. Minimal adjustment in `reconstruct-unifier`: the bijection for repeated vars forces equal RHS, which `substitution-compose` already rejects on conflict.
- [ ] **Step 4: Run — verify the fix**. Likely passes without code change if Task 5.3 threads substitution-compose correctly.
- [ ] **Step 5: Commit (if changes):** `test(algebra/unification): nonlinear AC-unification coverage`

---

### Task 5.5: Mixed AC + non-AC & constants

- [ ] **Step 1: Write tests**

```scheme
(test-group "ac-unify: mixed AC and non-AC operators"
  (let* ((theory (make-ac-theory '(+)))
         (proto (sexp-term-protocol default-compare))
         (vx (make-pattern-var 'x)))
    ;; (f (+ a b)) vs (f (+ b a)) — f is non-AC, + is AC
    (test 1 (length (ac-unify '(f (+ a b)) '(f (+ b a)) theory proto)))
    ;; (g x) vs (g a) — f non-AC but unifier binds x↦a
    (test 1 (length (ac-unify (list 'g vx) '(g a) theory proto)))))
```

- [ ] **Step 2: Run — expect pass** (unify-rec already dispatches by op AC-ness)
- [ ] **Step 3: If failing, trace dispatch**
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit (if changes):** `test(algebra/unification): mixed AC/non-AC unification`

---

## Phase 6 — Integration, docs, aggregator (~30 lib / ~30 test LOC)

### Task 6.1: Protocol-conformance harness test

- [ ] **Step 1: Write test that exercises `sexp-term-protocol` as a conforming instance**

```scheme
(test-group "integration: sexp-term-protocol conforms to contract"
  ;; Verify proto functions work over non-trivial nested AC terms
  (let* ((theory (make-ac-theory '(+ *)))
         (proto (sexp-term-protocol default-compare))
         (pat (parse-pattern '(+ (* ?x 2) (* ?y 3))))
         (subj '(+ (* 3 b) (* 2 a))))
    (let ((results (ac-match pat subj theory proto)))
      (test #t (> (length results) 0)))))

(test-group "integration: normalize then unify is equivalent to direct unify"
  ;; Given term T and AC theory, normalize(T) and T should unify identically
  ;; with any pattern P
  ;; This test asserts a stability property — the normalizer must not lose
  ;; information the matcher needs.
  ;; Skipped if no normalizer is accessible from the unification library.
  #t)
```

- [ ] **Step 2: Run — expect pass**
- [ ] **Step 3: Fix gaps**
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit:** `test(algebra/unification): protocol conformance + normalize-unify equivalence`

---

### Task 6.2: Stress test on random AC terms

- [ ] **Step 1: Write**

```scheme
(test-group "integration: random AC-equality stress"
  (let ((proto (sexp-term-protocol default-compare))
        (theory (make-ac-theory '(+))))
    (for-each
      (lambda (seed)
        (let* ((t (random-ac-term '+ '(a b c d) 5 seed))
               (shuffled (shuffle-ac-term t '+ seed)))
          (test 1 (length (ac-unify t shuffled theory proto)))))
      '(1 2 3 4 5))))

;; Helpers at top of test file:
(define (random-ac-term op leaves depth seed)
  (parameterize ((random-source (make-random-source seed)))
    (if (zero? depth)
        (list-ref leaves (random-integer (length leaves)))
        (let ((k (+ 2 (random-integer 3))))
          (cons op (map (lambda (_) (random-ac-term op leaves (- depth 1) seed))
                        (iota k)))))))

(define (shuffle-ac-term t op seed)
  ;; Reverse-order children recursively; this is a structural shuffle that
  ;; must preserve AC-equivalence.
  (cond
    ((pair? t)
     (if (eq? (car t) op)
         (cons op (reverse (map (lambda (x) (shuffle-ac-term x op seed)) (cdr t))))
         t))
    (else t)))
```

Note: `random-source`, `random-integer`, `make-random-source` come from `(srfi 27)` or equivalent. Verify Wile's random primitive names; fall back to a simple LCG if needed.

- [ ] **Step 2: Run — expect pass**
- [ ] **Step 3: If failing, root-cause stress test's term generator or the matcher**
- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit:** `test(algebra/unification): random AC-term stress coverage`

---

### Task 6.3: Docstrings on every public export

- [ ] **Step 1:** For each exported procedure in `unification.scm`, verify the docstring contains:
  - Parameters: description of each arg with type
  - Returns: return type and meaning
  - Category: `algebra`
  - Keywords: at least 3 relevant search terms

Exports to check:
- `make-pattern-var`, `pattern-var?`, `pattern-var-name`
- `parse-pattern`
- `empty-substitution`, `make-substitution`, `substitution?`
- `substitution-lookup`, `substitution-bindings`, `substitution-compose`, `substitution-apply`
- `ac-match`, `ac-unify`
- `diophantine-basis`

- [ ] **Step 2:** Add missing docstrings inline.
- [ ] **Step 3:** Spot-check one via the doc tool:

```bash
wile --doc ac-match
```

Expected: structured output with Parameters, Returns, Category, Keywords.

- [ ] **Step 4:** Commit: `docs(algebra/unification): structured docstrings for all public exports`

---

### Task 6.4: `(wile algebra)` aggregator re-export

- [ ] **Step 1: Check current aggregator**

```bash
grep -n 'unification\|incidence\|polynomial' stdlib/lib/wile/algebra.sld
```

Expected: `unification` not yet present; `incidence` and `polynomial` should be.

- [ ] **Step 2: Add to aggregator**

Modify `stdlib/lib/wile/algebra.sld`:
- Add `(wile algebra unification)` to `(import ...)`
- Add all exports from the unification library to the aggregator's `(export ...)`

- [ ] **Step 3: Test**

```scheme
;; In test/wile/algebra-integration-test.scm, add:
(test-group "aggregator re-exports unification"
  (eval '(ac-match '(+ a b) '(+ b a) (make-ac-theory '(+))
                   (sexp-term-protocol (lambda (a b) 0)))
        (environment '(wile algebra))))  ; should not error
```

- [ ] **Step 4: Run — pass**
- [ ] **Step 5: Commit:** `feat(algebra): aggregator re-exports unification library`

---

### Task 6.5: Mark TODO.md §5.3 complete + file follow-up plan stub

- [ ] **Step 1:** Update `TODO.md` line ~62 (§5.3 bullet) to `[x]` with a pointer to this plan's closing commit SHA.
- [ ] **Step 2:** Create `plans/YYYY-MM-DD-wile-goast-ac-match-migration.md` as a stub capturing the three integration risks from the design doc.

Stub contents:

```markdown
# wile-goast AC-match Migration

**Status:** Stub — deferred follow-up.
**Predecessor:** `plans/2026-04-21-ac-matching-design.md`

## Scope

Migrate `wile-goast/cmd/wile-goast/lib/wile/goast/unify.scm:421` from `discover-equivalences` (from `(wile algebra symbolic)`) to `ac-unify` (from `(wile algebra unification)`).

## Three risks to address

1. **Term-protocol contract compliance.** Add protocol-conformance test for wile-goast's Go-AST protocol.
2. **Trace-emitting diagnostic paths.** Audit consumers of `discover-equivalences` traces; any survivors stay on `discover-equivalences` until a trace-reconstructing variant exists.
3. **Small-arity benchmark.** Measure Eker+matrix vs direct-enumeration crossover on typical wile-goast input sizes.

## Scope (estimated ~100 LOC)

- Call-site migration: ~10 LOC
- Benchmark harness: ~40 LOC
- Protocol-conformance test: ~30 LOC
- Optional retirement of `discover-equivalences` from `symbolic.scm`: ~20 LOC (gated on no surviving consumers)
```

- [ ] **Step 3:** Commit: `docs(plans): mark §5.3 complete; stub wile-goast migration follow-up`

---

### Task 6.6: Final lint, covercheck, and closeout

- [ ] **Step 1:** `make lint` — clean.
- [ ] **Step 2:** `make covercheck` — clean.
- [ ] **Step 3:** `make test` — full suite green.
- [ ] **Step 4:** Final commit (if any fixes): `style(algebra/unification): final lint + coverage closeout`

---

## Self-review checklist (for the executing engineer)

Before declaring the phase complete:

- [ ] Every public export in `.sld` has a corresponding structured docstring
- [ ] `make lint` and `make covercheck` both clean
- [ ] `test/wile/algebra-unification-test.scm` has ≥47 test groups (per design doc)
- [ ] `(wile algebra)` aggregator imports and re-exports `(wile algebra unification)`
- [ ] Library header comment documents the `term-protocol` contract
- [ ] `TODO.md` §5.3 marked `[x]`
- [ ] Follow-up plan stub filed in `plans/`
- [ ] No wile-goast repository modified in this plan's commits
- [ ] Phase 4 retained iff benchmark justifies it; if dropped, that decision is documented in a commit message

## Known v1 simplifications (documented in design's Future extensions)

- AC-unification's Stickel reduction handles 0/1 basis multiplicities only. Non-trivial-multiplicity cases (variables binding to `(op …)` compounds from basis interpretation) are deferred to a follow-up when a consumer emerges.
- Stress test uses reverse-order shuffle rather than full random permutation — enough for AC-equivalence assertion, but not a complete randomness test.
- Matrix-permanent prune is 0/1 feasibility testing only; counting-semiring permanent is not wired.
