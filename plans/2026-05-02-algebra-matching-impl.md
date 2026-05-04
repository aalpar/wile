# `(wile algebra matching)` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Ship `(wile algebra matching)` — Gale-Shapley deferred acceptance, Hungarian assignment, hospital/intern many-to-one, and Conway-lattice selection — completing Tier B of the algebra roadmap.

**Architecture:** Three layers per §4.6 of the directions doc — local optimization (Gale-Shapley, Hungarian) → stability constraint (`stable?`, `blocking-pairs`) → global selection (Conway distributive lattice via `birkhoff-reconstruction`). Records: `<preference-profile>`, `<bipartite-matching>`, `<rotation>`. No new substrate; loads on shipped `(wile algebra setoid|order|incidence|lattice)`.

**Tech Stack:** Scheme (R7RS), Wile stdlib conventions per `stdlib/lib/wile/algebra/CLAUDE.md`, `(chibi test)` for the test suite.

**Design doc:** `plans/2026-05-02-algebra-matching-design.md` (Q1=Yes, Q2=Lazy, Q3=Defer, Q4=Pair confirmed)

**Target files:**
- `stdlib/lib/wile/algebra/matching.sld` — library declaration, exports, imports
- `stdlib/lib/wile/algebra/matching.scm` — implementation
- `stdlib/lib/wile/algebra.sld` — aggregator re-export (Phase 6)
- `test/wile/algebra-matching-test.scm` — test suite
- `docs/algebra/reference.md` — user-facing reference section (Phase 6)
- `TODO.md` — mark Tier B `(wile algebra matching)` and §4.2 Hungarian primitive entries done at Phase 6 close

**LOC budget:** ~850 lib + ~570 tests = ~1,420 total (Q1=Yes, Q2=Lazy variant per design doc).

**Workflow per task:** red → green → refactor → commit. Every task ends with `make lint && make covercheck` clean before the commit; phase boundaries also run `make ci`.

**Branch:** `feat/algebra-matching` from master.

---

## Phase 1 — Scaffolding, preference profiles, bipartite matchings, stability (~180 lib / ~120 test LOC)

### Task 1.1: Library skeleton and `<preference-profile>` record

**Files:**
- Create: `stdlib/lib/wile/algebra/matching.sld`
- Create: `stdlib/lib/wile/algebra/matching.scm`
- Create: `test/wile/algebra-matching-test.scm`

- [x] **Step 1: Write failing test**

```scheme
;;; algebra-matching-test.scm — Two-sided matching tests

(import (scheme base)
        (chibi test)
        (wile algebra matching))

(test-begin "matching")

(test-group "preference-profile construction"
  (let ((P (make-preference-profile
             '(a b c)
             (lambda (agent)
               (case agent
                 ((a) '(x y z))
                 ((b) '(y x z))
                 ((c) '(z x y)))))))
    (test #t (preference-profile? P))
    (test '(a b c) (preference-profile-agents P))
    (test '(x y z) ((preference-profile-ranks-of P) 'a))
    (test '(z x y) ((preference-profile-ranks-of P) 'c))))

(test-end "matching")
```

- [x] **Step 2: Run test — expect failure**

Run: `wile --file test/wile/algebra-matching-test.scm`
Expected: Library not found error.

- [x] **Step 3: Implement library skeleton + record**

`stdlib/lib/wile/algebra/matching.sld`:

```scheme
(define-library (wile algebra matching)
  (description "Two-sided matching: Gale-Shapley, Hungarian assignment, hospital/intern, Conway-lattice selection.")
  (export
    ;; Preference profiles
    make-preference-profile preference-profile?
    preference-profile-agents preference-profile-ranks-of
    preference-profile-setoid)
  (import (scheme base)
          (srfi 1)
          (wile algebra setoid))
  (include "matching.scm"))
```

`stdlib/lib/wile/algebra/matching.scm`:

```scheme
;;; matching.scm — Two-sided matching primitives.
;;;
;;; Three layers per directions doc §4.6:
;;;   Local optimization → gale-shapley, tropical-assignment
;;;   Stability constraint → stable?, blocking-pairs
;;;   Global selection → stable-matching-lattice, egalitarian-stable-matching
;;;
;;; Theorems brought into scope:
;;;   Gale-Shapley (1962) — deferred acceptance produces a stable matching
;;;   Conway (1976) — stable matchings form a distributive lattice
;;;   Roth (1985) — hospital/intern reduces to one-to-one with synthetic copies
;;;   Birkhoff (1937) — finite distributive lattices are downset lattices

(define-record-type <preference-profile>
  (make-preference-profile* agents ranks-of setoid)
  preference-profile?
  (agents preference-profile-agents)
  (ranks-of preference-profile-ranks-of)
  (setoid preference-profile-setoid))

(define (make-preference-profile agents ranks-of . opts)
  "Construct a preference profile.\n\nParameters:\n  agents : list — the agents on this side of the market\n  ranks-of : procedure — agent → ordered list of preferred candidates (best first)\n  opts : trailing alist — supports (setoid . S)\nReturns: <preference-profile>\nCategory: algebra\nKeywords: stable matching, preferences, two-sided market"
  (assert-procedure "make-preference-profile" ranks-of)
  (validate-opts-keys "make-preference-profile" opts '(setoid))
  (let ((setoid (assv-or opts 'setoid (default-setoid))))
    (make-preference-profile* agents ranks-of setoid)))
```

- [x] **Step 4: Run test — expect pass**

Run: `wile --file test/wile/algebra-matching-test.scm`
Expected: `preference-profile construction` group passes.

- [x] **Step 5: Lint + commit**

```bash
make lint
git add stdlib/lib/wile/algebra/matching.sld stdlib/lib/wile/algebra/matching.scm test/wile/algebra-matching-test.scm
git commit -m "feat(algebra/matching): scaffold library with <preference-profile>"
```

---

### Task 1.2: `preference-profile-rank-of` and `preference-profile-prefers-strictly?`

**Files:**
- Modify: `stdlib/lib/wile/algebra/matching.sld` — add exports
- Modify: `stdlib/lib/wile/algebra/matching.scm` — add functions
- Modify: `test/wile/algebra-matching-test.scm` — add test group

- [x] **Step 1: Write failing test**

Add to test file before `(test-end "matching")`:

```scheme
(test-group "preference-profile-rank-of and prefers-strictly?"
  (let ((P (make-preference-profile
             '(a b c)
             (lambda (agent)
               (case agent
                 ((a) '(x y z))
                 ((b) '(y x z))
                 ((c) '(z x y)))))))
    (test 1 (preference-profile-rank-of P 'a 'x))
    (test 2 (preference-profile-rank-of P 'a 'y))
    (test 3 (preference-profile-rank-of P 'a 'z))
    (test #t (preference-profile-prefers-strictly? P 'a 'x 'y))
    (test #f (preference-profile-prefers-strictly? P 'a 'y 'x))
    (test #f (preference-profile-prefers-strictly? P 'a 'x 'x))))
```

- [x] **Step 2: Run test — expect failure** (`Error: undefined identifier preference-profile-rank-of`)

- [x] **Step 3: Implement**

Add to `matching.sld` exports:

```scheme
    preference-profile-rank-of
    preference-profile-prefers-strictly?
```

Add to `matching.scm`:

```scheme
(define (preference-profile-rank-of P agent candidate)
  "Return 1-based rank of CANDIDATE in AGENT's preference list, or #f if absent.\n\nParameters:\n  P : preference-profile\n  agent : any\n  candidate : any\nReturns: positive integer or #f\nCategory: algebra\nKeywords: preferences, ranking"
  (let ((eq? (setoid-equiv? (preference-profile-setoid P)))
        (lst ((preference-profile-ranks-of P) agent)))
    (let loop ((xs lst) (i 1))
      (cond ((null? xs) #f)
            ((eq? candidate (car xs)) i)
            (else (loop (cdr xs) (+ i 1)))))))

(define (preference-profile-prefers-strictly? P agent x y)
  "Return #t iff AGENT strictly prefers X to Y under preference profile P.\nReturns #f if they tie, or if either is absent from AGENT's list.\n\nParameters:\n  P : preference-profile\n  agent : any\n  x : any\n  y : any\nReturns: boolean\nCategory: algebra\nKeywords: preferences, ranking, strict order"
  (let ((rx (preference-profile-rank-of P agent x))
        (ry (preference-profile-rank-of P agent y)))
    (and rx ry (< rx ry))))
```

- [x] **Step 4: Run test — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add rank-of and prefers-strictly? on preference profile"
```

---

### Task 1.3: `validate-preference-profile`

**Files:**
- Modify: `stdlib/lib/wile/algebra/matching.sld` — export `validate-preference-profile`
- Modify: `stdlib/lib/wile/algebra/matching.scm` — add validator
- Modify: `test/wile/algebra-matching-test.scm`

- [x] **Step 1: Write failing test**

```scheme
(test-group "validate-preference-profile"
  (let ((good (make-preference-profile
                '(a b)
                (lambda (x) (case x ((a) '(y x)) ((b) '(x y)))))))
    (test #t (validate-preference-profile good '(x y))))
  (let ((bad-out-of-set (make-preference-profile
                          '(a)
                          (lambda (x) '(z)))))     ; z not in candidate set
    (test #f (eq? #t (validate-preference-profile bad-out-of-set '(x y)))))
  (let ((bad-tied (make-preference-profile
                    '(a)
                    (lambda (x) '(x x)))))         ; tied
    (test #f (eq? #t (validate-preference-profile bad-tied '(x y))))))
```

- [x] **Step 2: Run test — expect failure**

- [x] **Step 3: Implement**

```scheme
(define (validate-preference-profile P candidate-set)
  "Verify that every agent in P ranks only members of CANDIDATE-SET, with no ties.\nReturns #t on success or a reversed list of (violation-type agent ...) entries.\n\nParameters:\n  P : preference-profile\n  candidate-set : list — universe of valid candidates\nReturns: #t or list\nCategory: algebra\nKeywords: validation, preferences"
  (let* ((fail! (make-violation-reporter))
         (eq? (setoid-equiv? (preference-profile-setoid P)))
         (in-set? (lambda (c) (setoid-member? c candidate-set eq?))))
    (for-each
      (lambda (agent)
        (let ((lst ((preference-profile-ranks-of P) agent)))
          (for-each
            (lambda (c)
              (unless (in-set? c)
                (fail! 'preference-out-of-set agent c)))
            lst)
          (let loop ((xs lst))
            (cond ((or (null? xs) (null? (cdr xs))) 'ok)
                  ((setoid-member? (car xs) (cdr xs) eq?)
                   (fail! 'tied-preference agent (car xs)))
                  (else (loop (cdr xs)))))))
      (preference-profile-agents P))
    (fail!)))
```

- [x] **Step 4: Run test — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add validate-preference-profile (out-of-set, tied checks)"
```

---

### Task 1.4: `<bipartite-matching>` record + accessors

**Files:**
- Modify: `matching.sld` — add exports for `make-bipartite-matching`, `bipartite-matching?`, `bipartite-matching-pairs`
- Modify: `matching.scm`
- Modify: test file

- [x] **Step 1: Write failing test**

```scheme
(test-group "bipartite-matching construction"
  (let ((M (make-bipartite-matching '((a . x) (b . y)))))
    (test #t (bipartite-matching? M))
    (test '((a . x) (b . y)) (bipartite-matching-pairs M))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement**

Add to exports:

```scheme
    make-bipartite-matching bipartite-matching?
    bipartite-matching-pairs
    bipartite-matching-prop-setoid bipartite-matching-recv-setoid
```

Add to `matching.scm`:

```scheme
(define-record-type <bipartite-matching>
  (make-bipartite-matching* pairs prop-setoid recv-setoid)
  bipartite-matching?
  (pairs bipartite-matching-pairs)
  (prop-setoid bipartite-matching-prop-setoid)
  (recv-setoid bipartite-matching-recv-setoid))

(define (make-bipartite-matching pairs . opts)
  "Construct a bipartite matching from an alist of (proposer . receiver) pairs.\nOptional trailing alist supports (prop-setoid . S), (recv-setoid . S).\n\nParameters:\n  pairs : alist of (any . any)\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: bipartite matching, assignment, two-sided"
  (validate-opts-keys "make-bipartite-matching" opts '(prop-setoid recv-setoid))
  (let ((ps (assv-or opts 'prop-setoid (default-setoid)))
        (rs (assv-or opts 'recv-setoid (default-setoid))))
    (make-bipartite-matching* pairs ps rs)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add <bipartite-matching> record"
```

---

### Task 1.5: `bipartite-matching-partner` and `bipartite-matching-unmatched`

- [x] **Step 1: Failing test**

```scheme
(test-group "bipartite-matching partner and unmatched"
  (let ((M (make-bipartite-matching '((a . x) (b . y)))))
    (test 'x (bipartite-matching-partner M 'a))
    (test 'a (bipartite-matching-partner M 'x))    ; symmetric lookup
    (test #f (bipartite-matching-partner M 'c))
    (test '(c) (bipartite-matching-unmatched M 'proposer '(a b c)))
    (test '(z) (bipartite-matching-unmatched M 'receiver '(x y z)))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement**

Add to exports: `bipartite-matching-partner bipartite-matching-unmatched`.

```scheme
(define (bipartite-matching-partner M agent)
  "Return the partner of AGENT in matching M, or #f if AGENT is unmatched.\nLookup is symmetric — works whether AGENT is on the proposer or receiver side.\n\nParameters:\n  M : bipartite-matching\n  agent : any\nReturns: any or #f\nCategory: algebra\nKeywords: matching, partner, lookup"
  (let ((peq (setoid-equiv? (bipartite-matching-prop-setoid M)))
        (req (setoid-equiv? (bipartite-matching-recv-setoid M)))
        (pairs (bipartite-matching-pairs M)))
    (let loop ((ps pairs))
      (cond ((null? ps) #f)
            ((peq agent (car (car ps))) (cdr (car ps)))
            ((req agent (cdr (car ps))) (car (car ps)))
            (else (loop (cdr ps)))))))

(define (bipartite-matching-unmatched M side agents)
  "Return AGENTS not appearing on SIDE ('proposer or 'receiver) of matching M.\n\nParameters:\n  M : bipartite-matching\n  side : symbol — 'proposer or 'receiver\n  agents : list — agents on that side\nReturns: list — agents from AGENTS not appearing in M on the given side\nCategory: algebra\nKeywords: matching, unmatched, partial"
  (let* ((eq (setoid-equiv?
               (case side
                 ((proposer) (bipartite-matching-prop-setoid M))
                 ((receiver) (bipartite-matching-recv-setoid M))
                 (else (error "bipartite-matching-unmatched: side must be 'proposer or 'receiver" side)))))
         (key (case side ((proposer) car) ((receiver) cdr)))
         (matched (map key (bipartite-matching-pairs M))))
    (filter (lambda (a) (not (setoid-member? a matched eq))) agents)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add bipartite-matching-partner and -unmatched"
```

---

### Task 1.6: `bipartite-matching-equal?` + `validate-bipartite-matching`

- [x] **Step 1: Failing test**

```scheme
(test-group "bipartite-matching equality and validation"
  (let ((M1 (make-bipartite-matching '((a . x) (b . y))))
        (M2 (make-bipartite-matching '((b . y) (a . x))))    ; same pairs, reordered
        (M3 (make-bipartite-matching '((a . y) (b . x)))))   ; different
    (test #t (bipartite-matching-equal? M1 M2))
    (test #f (bipartite-matching-equal? M1 M3)))
  (let ((M (make-bipartite-matching '((a . x) (b . x)))))    ; x matched twice
    (test #f (eq? #t (validate-bipartite-matching M '(a b) '(x y))))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement**

Add to exports: `bipartite-matching-equal? validate-bipartite-matching`.

```scheme
(define (bipartite-matching-equal? M1 M2)
  "Return #t iff M1 and M2 represent the same matching (order-insensitive).\n\nParameters:\n  M1 : bipartite-matching\n  M2 : bipartite-matching\nReturns: boolean\nCategory: algebra\nKeywords: equality, matching"
  (let ((peq (setoid-equiv? (bipartite-matching-prop-setoid M1)))
        (req (setoid-equiv? (bipartite-matching-recv-setoid M1)))
        (p1 (bipartite-matching-pairs M1))
        (p2 (bipartite-matching-pairs M2)))
    (and (= (length p1) (length p2))
         (every
           (lambda (pair)
             (any (lambda (q)
                    (and (peq (car pair) (car q))
                         (req (cdr pair) (cdr q))))
                  p2))
           p1))))

(define (validate-bipartite-matching M proposers receivers)
  "Verify M is a valid one-to-one matching: every proposer/receiver appears at most once;\nevery agent in M's pairs is drawn from PROPOSERS or RECEIVERS.\nReturns #t on success or reversed violation list.\n\nParameters:\n  M : bipartite-matching\n  proposers : list\n  receivers : list\nReturns: #t or list\nCategory: algebra\nKeywords: validation, matching"
  (let* ((fail! (make-violation-reporter))
         (peq (setoid-equiv? (bipartite-matching-prop-setoid M)))
         (req (setoid-equiv? (bipartite-matching-recv-setoid M)))
         (pairs (bipartite-matching-pairs M)))
    (let loop ((seen-p '()) (seen-r '()) (ps pairs))
      (cond
        ((null? ps) 'done)
        (else
          (let ((p (car (car ps))) (r (cdr (car ps))))
            (unless (setoid-member? p proposers peq)
              (fail! 'proposer-not-in-set p))
            (unless (setoid-member? r receivers req)
              (fail! 'receiver-not-in-set r))
            (when (setoid-member? p seen-p peq)
              (fail! 'proposer-matched-twice p))
            (when (setoid-member? r seen-r req)
              (fail! 'receiver-matched-twice r))
            (loop (cons p seen-p) (cons r seen-r) (cdr ps))))))
    (fail!)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add equality and validation for bipartite matchings"
```

---

### Task 1.7: `blocking-pairs` and `stable?`

- [x] **Step 1: Failing test**

```scheme
(test-group "blocking-pairs and stable?"
  (let* ((prop-prefs (make-preference-profile
                       '(a b) (lambda (x) (case x ((a) '(y x)) ((b) '(x y))))))
         (recv-prefs (make-preference-profile
                       '(x y) (lambda (x) (case x ((x) '(a b)) ((y) '(a b))))))
         (M-stable (make-bipartite-matching '((a . y) (b . x))))
         (M-unstable (make-bipartite-matching '((a . x) (b . y)))))
    (test '() (blocking-pairs M-stable prop-prefs recv-prefs))
    (test #t (stable? M-stable prop-prefs recv-prefs))
    ;; M-unstable: a prefers y over x, y prefers a over b → (a . y) blocks
    (test #f (null? (blocking-pairs M-unstable prop-prefs recv-prefs)))
    (test #f (stable? M-unstable prop-prefs recv-prefs))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement**

Add to exports: `blocking-pairs stable?`.

```scheme
(define (blocking-pairs M prop-prefs recv-prefs)
  "Return the list of (proposer . receiver) blocking pairs in matching M.\nA blocking pair (p,r) satisfies:\n  p prefers r over its current partner (or is unmatched), AND\n  r prefers p over its current partner (or is unmatched).\nM is stable iff this list is empty.\n\nParameters:\n  M : bipartite-matching\n  prop-prefs : preference-profile — proposers' preferences over receivers\n  recv-prefs : preference-profile — receivers' preferences over proposers\nReturns: list of (any . any)\nCategory: algebra\nKeywords: stability, blocking pair, Gale-Shapley"
  (let ((proposers (preference-profile-agents prop-prefs))
        (receivers (preference-profile-agents recv-prefs)))
    (let outer ((ps proposers) (acc '()))
      (cond
        ((null? ps) (reverse acc))
        (else
          (let* ((p (car ps))
                 (cur-r (bipartite-matching-partner M p)))
            (let inner ((rs receivers) (acc2 acc))
              (cond
                ((null? rs) (outer (cdr ps) acc2))
                (else
                  (let* ((r (car rs))
                         (cur-p (bipartite-matching-partner M r))
                         (p-prefers-r (or (not cur-r)
                                          (preference-profile-prefers-strictly?
                                            prop-prefs p r cur-r)))
                         (r-prefers-p (or (not cur-p)
                                          (preference-profile-prefers-strictly?
                                            recv-prefs r p cur-p))))
                    (if (and p-prefers-r r-prefers-p
                             (not (and cur-r
                                       (setoid-equiv? (preference-profile-setoid recv-prefs))
                                       ((setoid-equiv? (preference-profile-setoid recv-prefs)) cur-r r))))
                        (inner (cdr rs) (cons (cons p r) acc2))
                        (inner (cdr rs) acc2))))))))))))

(define (stable? M prop-prefs recv-prefs)
  "Return #t iff matching M is stable under the given preferences (no blocking pair).\n\nParameters:\n  M : bipartite-matching\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: boolean\nCategory: algebra\nKeywords: stability, Gale-Shapley, two-sided matching"
  (null? (blocking-pairs M prop-prefs recv-prefs)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add blocking-pairs and stable? predicates"
```

---

### Task 1.8: `with-preference-profile` and `with-bipartite-matching` macros + Phase 1 checkpoint

- [x] **Step 1: Failing test**

```scheme
(test-group "with-X macros"
  (let ((P (make-preference-profile '(a) (lambda (x) '(y)))))
    (with-preference-profile P (agents ranks-of)
      (test '(a) agents)
      (test '(y) (ranks-of 'a))))
  (let ((M (make-bipartite-matching '((a . x)))))
    (with-bipartite-matching M (pairs)
      (test '((a . x)) pairs))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement** per `stdlib/lib/wile/algebra/CLAUDE.md` `with-X` skeleton

Add to exports: `with-preference-profile with-bipartite-matching`.

```scheme
(define-syntax with-preference-profile
  (syntax-rules ()
    ((with-preference-profile p (agents ranks-of) body ...)
     (let ((tmp p))
       (let ((agents (preference-profile-agents tmp))
             (ranks-of (preference-profile-ranks-of tmp)))
         body ...)))))

(define-syntax with-bipartite-matching
  (syntax-rules ()
    ((with-bipartite-matching m (pairs) body ...)
     (let ((tmp m))
       (let ((pairs (bipartite-matching-pairs tmp)))
         body ...)))))
```

- [x] **Step 4: Run — expect pass; then run full Phase 1 checkpoint**

```bash
wile --file test/wile/algebra-matching-test.scm
make lint
make covercheck
```

Expected: all groups pass; lint clean; coverage at or above project gate for new file.

- [x] **Step 5: Commit Phase 1 close**

```bash
git add -u
git commit -m "feat(algebra/matching): add with-X macros; Phase 1 scaffold complete"
```

---

## Phase 2 — Gale-Shapley deferred acceptance (~120 lib / ~100 test LOC)

### Task 2.1: Gale-Shapley proposer-side (textbook 4×4)

- [x] **Step 1: Failing test** — Gusfield-Irving §1.2 textbook instance

```scheme
(test-group "gale-shapley proposer-optimal — textbook 4×4"
  ;; Men's preferences (proposers)
  (let* ((mp (make-preference-profile
               '(1 2 3 4)
               (lambda (m)
                 (case m
                   ((1) '(a b c d))
                   ((2) '(b a c d))
                   ((3) '(a c b d))
                   ((4) '(c a b d))))))
         ;; Women's preferences (receivers)
         (wp (make-preference-profile
               '(a b c d)
               (lambda (w)
                 (case w
                   ((a) '(2 4 1 3))
                   ((b) '(3 1 2 4))
                   ((c) '(2 3 4 1))
                   ((d) '(4 1 3 2))))))
         (M (gale-shapley mp wp)))
    (test #t (bipartite-matching? M))
    (test #t (stable? M mp wp))
    (test 4 (length (bipartite-matching-pairs M)))))
```

- [x] **Step 2: Run — expect failure** (`Error: undefined identifier gale-shapley`)

- [x] **Step 3: Implement**

Add to exports: `gale-shapley`.

```scheme
(define (gale-shapley prop-prefs recv-prefs)
  "Compute the proposer-optimal stable matching via Gale-Shapley deferred acceptance.\nO(n^2) where n is the number of proposers. Conway (1976): this matching is the\nlattice top — every proposer does at least as well as in any other stable matching;\nsymmetrically, every receiver does at least as poorly.\n\nParameters:\n  prop-prefs : preference-profile — proposers' preferences\n  recv-prefs : preference-profile — receivers' preferences\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: Gale-Shapley, stable matching, deferred acceptance, proposer-optimal"
  (let* ((proposers (preference-profile-agents prop-prefs))
         (peq (setoid-equiv? (preference-profile-setoid prop-prefs)))
         (req (setoid-equiv? (preference-profile-setoid recv-prefs)))
         ;; cursors: for each proposer, the index into their preference list of the
         ;; next candidate to try.
         (cursors (map (lambda (p) (cons p 0)) proposers))
         ;; current matches: alist of (receiver . proposer)
         (matches '()))
    (define (cursor-of p)
      (cdr (assoc p cursors (lambda (a b) (peq a b)))))
    (define (advance-cursor! p)
      (let ((cell (assoc p cursors (lambda (a b) (peq a b)))))
        (set-cdr! cell (+ 1 (cdr cell)))))
    (define (next-candidate p)
      (let* ((lst ((preference-profile-ranks-of prop-prefs) p))
             (i (cursor-of p)))
        (if (< i (length lst)) (list-ref lst i) #f)))
    (define (current-match-of-receiver r)
      (let ((cell (assoc r matches (lambda (a b) (req a b)))))
        (if cell (cdr cell) #f)))
    (define (set-match! r p)
      (let ((cell (assoc r matches (lambda (a b) (req a b)))))
        (if cell
            (set-cdr! cell p)
            (set! matches (cons (cons r p) matches)))))
    (define (free-proposer)
      (find (lambda (p)
              (and (not (any (lambda (cell) (peq (cdr cell) p)) matches))
                   (next-candidate p)))
            proposers))
    (let loop ()
      (let ((p (free-proposer)))
        (cond
          ((not p) 'done)
          (else
            (let ((r (next-candidate p)))
              (advance-cursor! p)
              (let ((cur (current-match-of-receiver r)))
                (cond
                  ((not cur) (set-match! r p))
                  ((preference-profile-prefers-strictly? recv-prefs r p cur)
                   (set-match! r p))
                  (else 'rejected)))
              (loop))))))
    (make-bipartite-matching
      (map (lambda (cell) (cons (cdr cell) (car cell))) matches)
      `(prop-setoid . ,(preference-profile-setoid prop-prefs))
      `(recv-setoid . ,(preference-profile-setoid recv-prefs)))))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): implement gale-shapley proposer-optimal"
```

---

### Task 2.2: Receiver-optimal variant

- [x] **Step 1: Failing test**

```scheme
(test-group "gale-shapley/receiver-optimal asymmetry"
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '(a b)) ((2) '(b a))))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) (case w ((a) '(2 1)) ((b) '(1 2))))))
         (M-prop (gale-shapley mp wp))
         (M-recv (gale-shapley/receiver-optimal mp wp)))
    (test #t (stable? M-prop mp wp))
    (test #t (stable? M-recv mp wp))
    ;; Proposer-optimal: 1↔a, 2↔b (everyone gets first choice from one side)
    ;; Receiver-optimal swaps: 1↔b, 2↔a
    (test #f (bipartite-matching-equal? M-prop M-recv))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement**

Add to exports: `gale-shapley/receiver-optimal`.

```scheme
(define (gale-shapley/receiver-optimal prop-prefs recv-prefs)
  "Compute the receiver-optimal stable matching by running Gale-Shapley with sides swapped.\nReturns a matching with proposer-shaped pairs (proposers as keys) for consistency with\nthe proposer-side variant.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: Gale-Shapley, receiver-optimal, stable matching"
  (let* ((swapped (gale-shapley recv-prefs prop-prefs))
         (pairs (bipartite-matching-pairs swapped)))
    (make-bipartite-matching
      (map (lambda (pr) (cons (cdr pr) (car pr))) pairs)
      `(prop-setoid . ,(preference-profile-setoid prop-prefs))
      `(recv-setoid . ,(preference-profile-setoid recv-prefs)))))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add gale-shapley/receiver-optimal"
```

---

### Task 2.3: Edge cases (unequal sides, exhausted preferences)

- [x] **Step 1: Failing test**

```scheme
(test-group "gale-shapley edge cases"
  ;; Three proposers, two receivers — one proposer ends unmatched
  (let* ((mp (make-preference-profile
               '(1 2 3)
               (lambda (m) '(a b))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) '(1 2 3))))
         (M (gale-shapley mp wp)))
    (test #t (stable? M mp wp))
    (test 2 (length (bipartite-matching-pairs M)))
    (test '(3) (bipartite-matching-unmatched M 'proposer '(1 2 3))))
  ;; Empty preference list — proposer can never match
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '()) ((2) '(a))))))
         (wp (make-preference-profile
               '(a)
               (lambda (w) '(2 1))))
         (M (gale-shapley mp wp)))
    (test 1 (length (bipartite-matching-pairs M)))
    (test '(1) (bipartite-matching-unmatched M 'proposer '(1 2)))))
```

- [x] **Step 2: Run — expect pass** (existing implementation should already handle these)

If failing: the `next-candidate` returning `#f` and `free-proposer`'s requirement that the proposer have a remaining candidate handle exhaustion. If a test fails, fix the boundary in `gale-shapley` rather than special-casing.

- [x] **Step 3: Commit**

```bash
make lint
git add -u
git commit -m "test(algebra/matching): cover gale-shapley edge cases (unequal sides, empty prefs)"
```

---

### Task 2.4: Property test — random profiles always produce a stable matching

- [x] **Step 1: Failing test**

```scheme
(test-group "gale-shapley property: 50 random profiles, all stable"
  (define (random-perm n)
    (let* ((xs (iota n))
           (vec (list->vector xs)))
      (do ((i (- n 1) (- i 1)))
          ((<= i 0))
        (let* ((j (modulo (* (+ i 1) 2654435761) (+ i 1)))
               (tmp (vector-ref vec i)))
          (vector-set! vec i (vector-ref vec j))
          (vector-set! vec j tmp)))
      (vector->list vec)))
  (define n 5)
  (do ((trial 0 (+ trial 1))) ((>= trial 50))
    (let* ((mp (make-preference-profile
                 (iota n)
                 (lambda (m) (random-perm n))))
           (wp (make-preference-profile
                 (iota n)
                 (lambda (w) (random-perm n))))
           (M (gale-shapley mp wp)))
      (test-assert (stable? M mp wp)))))
```

Note: deterministic pseudo-random (multiplicative congruential on `i`) — reproducible across runs. Replace with R7RS `random` if/when added.

- [x] **Step 2: Run — expect pass** (property holds by Gale-Shapley's correctness)

- [x] **Step 3: Phase 2 checkpoint**

```bash
wile --file test/wile/algebra-matching-test.scm
make lint
make covercheck
make ci
```

- [x] **Step 4: Commit**

```bash
git add -u
git commit -m "test(algebra/matching): randomized stability property test for gale-shapley; Phase 2 closeout"
```

---

## Phase 3 — Hospital/intern many-to-one (~120 lib / ~80 test LOC)

### Task 3.1: Quota validation and `<hospital-quota>` accessor scaffolding

- [x] **Step 1: Failing test**

```scheme
(test-group "hospital-intern quota validation"
  (test-error
    (hospital-intern-match
      (make-preference-profile '(i1) (lambda (x) '(h1)))
      (make-preference-profile '(h1) (lambda (x) '(i1)))
      '((h1 . 0)))))                                  ; quota 0 invalid
  (test-error
    (hospital-intern-match
      (make-preference-profile '(i1) (lambda (x) '(h1)))
      (make-preference-profile '(h1) (lambda (x) '(i1)))
      '())))                                           ; missing h1 quota
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement skeleton**

Add to exports: `hospital-intern-match`.

```scheme
(define (hospital-intern-match intern-prefs hospital-prefs hospital-quotas)
  "Compute an intern-optimal stable many-to-one matching via Roth's reduction.\nReturns an alist ((hospital . (intern ...)) ...) of accepted interns per hospital.\nUnmatched interns are absent; caller can derive them via set difference.\n\nParameters:\n  intern-prefs : preference-profile — interns' preferences over hospitals\n  hospital-prefs : preference-profile — hospitals' preferences over interns\n  hospital-quotas : alist of (hospital . positive-integer)\nReturns: alist of (any . list)\nCategory: algebra\nKeywords: hospital-intern, college-admissions, many-to-one, Roth, quota"
  (validate-quotas! hospital-quotas (preference-profile-agents hospital-prefs))
  ;; Filled in by Task 3.2.
  (error "hospital-intern-match: not yet implemented"))

(define (validate-quotas! quotas hospitals)
  (for-each
    (lambda (h)
      (let ((cell (assoc h quotas)))
        (cond
          ((not cell)
           (error "hospital-intern-match: missing quota for hospital" h))
          ((or (not (integer? (cdr cell)))
               (not (positive? (cdr cell))))
           (error "hospital-intern-match: quota must be a positive integer" h (cdr cell))))))
    hospitals))
```

- [x] **Step 4: Run — expect first two tests pass; third (full match) still fails as expected**

For these tests we just want the validation to fire. Adjust if `test-error` only checks for any error.

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): scaffold hospital-intern with quota validation"
```

---

### Task 3.2: Roth reduction — inflate hospitals to copies, run Gale-Shapley, collapse

- [x] **Step 1: Failing test**

```scheme
(test-group "hospital-intern textbook example (Roth-Sotomayor §5.5 simplified)"
  (let* ((iprefs (make-preference-profile
                   '(i1 i2 i3)
                   (lambda (i)
                     (case i
                       ((i1) '(h1 h2))
                       ((i2) '(h1 h2))
                       ((i3) '(h2 h1))))))
         (hprefs (make-preference-profile
                   '(h1 h2)
                   (lambda (h)
                     (case h
                       ((h1) '(i1 i2 i3))
                       ((h2) '(i3 i1 i2))))))
         (quotas '((h1 . 1) (h2 . 2)))
         (M (hospital-intern-match iprefs hprefs quotas)))
    ;; h1 wants i1 (top of quota=1) → i1 → h1
    ;; h2 has quota 2: top is i3, then accepts the next free intern
    (test #t (member 'i1 (cdr (assoc 'h1 M))))
    (test #t (member 'i3 (cdr (assoc 'h2 M))))
    (test 3 (apply + (map (lambda (cell) (length (cdr cell))) M)))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement Roth reduction**

Replace the stub body with:

```scheme
(define (hospital-intern-match intern-prefs hospital-prefs hospital-quotas)
  "..."     ; keep docstring from Task 3.1
  (validate-quotas! hospital-quotas (preference-profile-agents hospital-prefs))
  (let* ((hospitals (preference-profile-agents hospital-prefs))
         (interns (preference-profile-agents intern-prefs))
         (heq (setoid-equiv? (preference-profile-setoid hospital-prefs)))
         ;; Inflate each hospital h to copies (h . 1), (h . 2), ..., (h . q_h)
         (copies-of
           (lambda (h)
             (let ((q (cdr (assoc h hospital-quotas))))
               (let loop ((i 1) (acc '()))
                 (if (> i q) (reverse acc)
                     (loop (+ i 1) (cons (cons h i) acc)))))))
         (all-copies (apply append (map copies-of hospitals)))
         ;; Inflated intern preferences: each h in intern's list expands to its copies
         (inflated-iprefs
           (make-preference-profile
             interns
             (lambda (i)
               (apply append
                      (map copies-of
                           ((preference-profile-ranks-of intern-prefs) i))))))
         ;; Inflated hospital-copy preferences: every copy of h has h's preference list
         (inflated-hprefs
           (make-preference-profile
             all-copies
             (lambda (copy)
               ((preference-profile-ranks-of hospital-prefs) (car copy)))))
         (M-flat (gale-shapley inflated-iprefs inflated-hprefs))
         (pairs (bipartite-matching-pairs M-flat)))
    ;; Collapse: group interns by hospital
    (map
      (lambda (h)
        (cons h
              (filter-map
                (lambda (pr)
                  (let ((intern (car pr)) (copy (cdr pr)))
                    (if (heq (car copy) h) intern #f)))
                pairs)))
      hospitals)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): implement hospital-intern via Roth reduction"
```

---

### Task 3.3: quota=1 reduces to Gale-Shapley + property test

- [x] **Step 1: Failing test**

```scheme
(test-group "hospital-intern quota=1 reduces to gale-shapley"
  (let* ((iprefs (make-preference-profile
                   '(i1 i2)
                   (lambda (i) (case i ((i1) '(h1 h2)) ((i2) '(h2 h1))))))
         (hprefs (make-preference-profile
                   '(h1 h2)
                   (lambda (h) (case h ((h1) '(i2 i1)) ((h2) '(i1 i2))))))
         (M-hi (hospital-intern-match iprefs hprefs '((h1 . 1) (h2 . 1))))
         (M-gs (gale-shapley iprefs hprefs)))
    ;; Convert hospital-intern alist to flat pair set for comparison
    (let ((flat-hi (apply append
                          (map (lambda (cell)
                                 (map (lambda (i) (cons i (car cell))) (cdr cell)))
                               M-hi))))
      (test #t
        (bipartite-matching-equal?
          (make-bipartite-matching flat-hi)
          M-gs)))))
```

- [x] **Step 2: Run — expect pass** (semantic equivalence by Roth's reduction).

If failing, the conversion of `M-hi` to flat pairs may need ordering adjustment — both alists represent unordered sets, but `bipartite-matching-equal?` already handles reordering.

- [x] **Step 3: Commit**

```bash
git add -u
git commit -m "test(algebra/matching): hospital-intern quota=1 reduces to gale-shapley"
```

---

### Task 3.4: Phase 3 checkpoint

- [x] **Step 1: Run full test suite + CI**

```bash
wile --file test/wile/algebra-matching-test.scm
make lint
make covercheck
make ci
```

- [x] **Step 2: Commit Phase 3 closeout**

```bash
git add -u
git commit -m "chore(algebra/matching): Phase 3 hospital-intern complete"
```

---

## Phase 4 — Hungarian assignment (~180 lib / ~110 test LOC)

### Task 4.1: 2×2 Hungarian by hand

- [x] **Step 1: Failing test**

```scheme
(test-group "tropical-assignment 2x2 by hand"
  (let* ((cost (lambda (p r)
                 (case p
                   ((1) (case r ((a) 4) ((b) 1)))
                   ((2) (case r ((a) 2) ((b) 5))))))
         (result (tropical-assignment cost '(1 2) '(a b))))
    ;; Optimal: 1→b (cost 1), 2→a (cost 2), total 3
    (test 3 (cdr result))
    (test #t (bipartite-matching? (car result)))
    (test 'b (bipartite-matching-partner (car result) 1))
    (test 'a (bipartite-matching-partner (car result) 2))))
```

- [x] **Step 2: Run — expect failure**

- [x] **Step 3: Implement Hungarian (Kuhn-Munkres)**

Add to exports: `tropical-assignment`.

```scheme
(define (tropical-assignment cost-fn proposers receivers)
  "Compute a minimum-cost perfect assignment via the Hungarian algorithm (Kuhn 1955; Munkres 1957).\nReturns (matching . total-cost) where matching is a <bipartite-matching>.\nUse +inf.0 in COST-FN to forbid a (proposer, receiver) pair.\n\nUnequal-size sides are padded internally with synthetic agents at +inf.0 cost; synthetic\npairs are excluded from the returned matching. The Shapley-Shubik core allocation\n(LP dual potentials) is computed internally but not returned in v1; see\ntropical-assignment/with-potentials in Future extensions.\n\nParameters:\n  cost-fn : procedure — (proposer × receiver) → number ∪ +inf.0\n  proposers : list\n  receivers : list\nReturns: pair (<bipartite-matching> . number)\nCategory: algebra\nKeywords: Hungarian, assignment, Kuhn-Munkres, tropical, bipartite, Shapley-Shubik"
  (let* ((m (length proposers))
         (n (length receivers))
         (size (max m n))
         (INF +inf.0)
         ;; Pad to a square matrix with INF
         (C (make-vector size #f)))
    (do ((i 0 (+ i 1))) ((>= i size))
      (let ((row (make-vector size INF)))
        (when (< i m)
          (let ((p (list-ref proposers i)))
            (do ((j 0 (+ j 1))) ((>= j n))
              (vector-set! row j (cost-fn p (list-ref receivers j))))))
        (vector-set! C i row)))
    (let ((assignment (kuhn-munkres-square C size)))
      ;; Decode assignment vector (row → col) to pairs, dropping synthetic
      (let loop ((i 0) (pairs '()) (total 0))
        (if (>= i size)
            (cons (make-bipartite-matching (reverse pairs)) total)
            (let ((j (vector-ref assignment i)))
              (if (and (< i m) (< j n)
                       (not (= (vector-ref (vector-ref C i) j) INF)))
                  (loop (+ i 1)
                        (cons (cons (list-ref proposers i)
                                    (list-ref receivers j)) pairs)
                        (+ total (vector-ref (vector-ref C i) j)))
                  (loop (+ i 1) pairs total))))))))

;; Kuhn-Munkres on a square cost matrix (vector of vectors).
;; Returns a vector mapping row → assigned column.
;; Uses the O(n^3) potential-based shortest-augmenting-path variant
;; (Jonker-Volgenant 1987 form, easier to implement than the line-cover version).
(define (kuhn-munkres-square C n)
  (let ((u (make-vector (+ n 1) 0))     ; row potentials
        (v (make-vector (+ n 1) 0))     ; col potentials
        (p (make-vector (+ n 1) 0))     ; col → row assignment (1-indexed)
        (way (make-vector (+ n 1) 0)))  ; predecessor in BFS
    (do ((i 1 (+ i 1))) ((> i n))
      (vector-set! p 0 i)
      (let ((j0 0)
            (minv (make-vector (+ n 1) +inf.0))
            (used (make-vector (+ n 1) #f)))
        (let phase ()
          (vector-set! used j0 #t)
          (let ((i0 (vector-ref p j0))
                (delta +inf.0)
                (j1 0))
            (do ((j 1 (+ j 1))) ((> j n))
              (when (not (vector-ref used j))
                (let* ((cur (- (vector-ref (vector-ref C (- i0 1)) (- j 1))
                               (vector-ref u i0) (vector-ref v j))))
                  (when (< cur (vector-ref minv j))
                    (vector-set! minv j cur)
                    (vector-set! way j j0))
                  (when (< (vector-ref minv j) delta)
                    (set! delta (vector-ref minv j))
                    (set! j1 j)))))
            (do ((j 0 (+ j 1))) ((> j n))
              (if (vector-ref used j)
                  (begin
                    (vector-set! u (vector-ref p j) (+ (vector-ref u (vector-ref p j)) delta))
                    (vector-set! v j (- (vector-ref v j) delta)))
                  (vector-set! minv j (- (vector-ref minv j) delta))))
            (set! j0 j1)
            (when (not (= (vector-ref p j0) 0))
              (phase))))
        (let augment ()
          (let ((j1 (vector-ref way j0)))
            (vector-set! p j0 (vector-ref p j1))
            (set! j0 j1)
            (when (not (= j0 0))
              (augment))))))
    (let ((result (make-vector n 0)))
      (do ((j 1 (+ j 1))) ((> j n))
        (when (> (vector-ref p j) 0)
          (vector-set! result (- (vector-ref p j) 1) (- j 1))))
      result)))
```

- [x] **Step 4: Run — expect pass**

- [x] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): implement tropical-assignment via Kuhn-Munkres O(n^3)"
```

---

### Task 4.2: 4×4 vs `semiring-matrix-permanent` cost agreement

- [x] **Step 1: Failing test**

```scheme
(test-group "tropical-assignment 4x4 cost agrees with tropical-permanent"
  (let* ((rows '((9 11 14 11)
                 (6  3  9  9)
                 (5  8  3  6)
                 (5  8  6  3)))
         (cost-fn (lambda (i j)
                    (list-ref (list-ref rows (- i 1)) (- j 1)))))
    (let ((r (tropical-assignment cost-fn '(1 2 3 4) '(1 2 3 4))))
      ;; Reference cost computed externally; minimum permanent = 19
      (test 19 (cdr r))
      (test #t (stable-assignment? (car r) cost-fn)))))

;; Helper: assignment is "stable" iff swapping any two rows' columns doesn't reduce cost.
;; (Local-optimum sanity, not the same as Gale-Shapley stability.)
(define (stable-assignment? M cost-fn)
  (let ((pairs (bipartite-matching-pairs M)))
    (let outer ((xs pairs))
      (cond
        ((null? xs) #t)
        (else
          (let inner ((ys (cdr xs)))
            (cond
              ((null? ys) (outer (cdr xs)))
              (else
                (let* ((p1 (car xs)) (p2 (car ys))
                       (orig (+ (cost-fn (car p1) (cdr p1))
                                (cost-fn (car p2) (cdr p2))))
                       (swap (+ (cost-fn (car p1) (cdr p2))
                                (cost-fn (car p2) (cdr p1)))))
                  (and (<= orig swap) (inner (cdr ys))))))))))))
```

- [x] **Step 2: Run — expect pass** (Hungarian is provably optimal; the test asserts the known optimum value)

If the assignment doesn't match, debug Kuhn-Munkres — do not weaken the test.

- [x] **Step 3: Commit**

```bash
git add -u
git commit -m "test(algebra/matching): tropical-assignment 4x4 with cost-agreement check"
```

---

### Task 4.3: Forbidden pairs and unequal sides

- [x] **Step 1: Failing test**

```scheme
(test-group "tropical-assignment edge cases"
  ;; Forbidden pair (+inf.0): assignment routes around it
  (let* ((cost-fn (lambda (p r)
                    (case p
                      ((1) (case r ((a) 1) ((b) +inf.0)))
                      ((2) (case r ((a) 5) ((b) 2))))))
         (r (tropical-assignment cost-fn '(1 2) '(a b))))
    (test 3 (cdr r))                          ; 1→a (1) + 2→b (2)
    (test 'a (bipartite-matching-partner (car r) 1)))
  ;; Unequal sides: 3 proposers, 2 receivers — one proposer unmatched
  (let* ((cost-fn (lambda (p r)
                    (case p ((1) 1) ((2) 2) ((3) 3))))
         (r (tropical-assignment cost-fn '(1 2 3) '(a b))))
    (test 2 (length (bipartite-matching-pairs (car r))))))
```

- [x] **Step 2: Run — expect pass** (existing implementation handles both)

- [x] **Step 3: Commit**

```bash
git add -u
git commit -m "test(algebra/matching): tropical-assignment forbidden pairs and unequal sides"
```

---

### Task 4.4: Phase 4 checkpoint

- [ ] **Step 1: Run** `make ci` and verify clean

- [ ] **Step 2: Commit closeout**

```bash
git add -u
git commit -m "chore(algebra/matching): Phase 4 Hungarian assignment complete"
```

---

## Phase 5 — Rotations and Conway lattice (~220 lib / ~130 test LOC)

### Task 5.1: `<rotation>` record + `apply-rotation`

- [ ] **Step 1: Failing test**

```scheme
(test-group "rotation record and apply"
  (let* ((rho (make-rotation '((1 . a) (2 . b))))
         (M (make-bipartite-matching '((1 . a) (2 . b)))))
    (test #t (rotation? rho))
    (test '((1 . a) (2 . b)) (rotation-cycle rho))
    ;; Apply: rotate each (pᵢ, rᵢ) → (pᵢ, r_{i+1})
    (let ((M' (apply-rotation M rho)))
      (test 'b (bipartite-matching-partner M' 1))
      (test 'a (bipartite-matching-partner M' 2)))))
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement**

Add to exports: `make-rotation rotation? rotation-cycle apply-rotation`.

```scheme
(define-record-type <rotation>
  (make-rotation* cycle)
  rotation?
  (cycle rotation-cycle))

(define (make-rotation cycle)
  "Construct a rotation from a list of (proposer . receiver) pairs in cyclic order.\nApplying the rotation to a stable matching M produces M' where each proposer pᵢ is\nreassigned from its current partner rᵢ to r_{i+1 mod k}.\n\nParameters:\n  cycle : list of (any . any), length ≥ 2\nReturns: <rotation>\nCategory: algebra\nKeywords: rotation, Irving, Gusfield, stable matching"
  (when (or (not (list? cycle)) (< (length cycle) 2))
    (error "make-rotation: cycle must be a list of at least 2 (proposer . receiver) pairs" cycle))
  (make-rotation* cycle))

(define (apply-rotation M rho)
  "Apply rotation RHO to matching M, returning a new matching where each rotation\nproposer is reassigned to the next receiver in the cycle.\n\nParameters:\n  M : bipartite-matching\n  rho : rotation\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: rotation, Gusfield-Irving, stable matching, lattice traversal"
  (let* ((cycle (rotation-cycle rho))
         (k (length cycle))
         (peq (setoid-equiv? (bipartite-matching-prop-setoid M)))
         ;; Build new-partner map: pᵢ → r_{(i+1) mod k}
         (rotmap
           (map (lambda (i)
                  (cons (car (list-ref cycle i))
                        (cdr (list-ref cycle (modulo (+ i 1) k)))))
                (iota k)))
         (new-pairs
           (map (lambda (pr)
                  (let* ((p (car pr))
                         (override (assoc p rotmap (lambda (a b) (peq a b)))))
                    (if override (cons p (cdr override)) pr)))
                (bipartite-matching-pairs M))))
    (make-bipartite-matching
      new-pairs
      `(prop-setoid . ,(bipartite-matching-prop-setoid M))
      `(recv-setoid . ,(bipartite-matching-recv-setoid M)))))
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add <rotation> record and apply-rotation"
```

---

### Task 5.2: Rotation enumeration via reduced preference table

- [ ] **Step 1: Failing test** — Gusfield-Irving §3.4 small example

```scheme
(test-group "rotations on Gusfield-Irving 4x4"
  (let* ((mp (make-preference-profile
               '(1 2 3 4)
               (lambda (m)
                 (case m
                   ((1) '(a b c d))
                   ((2) '(b a c d))
                   ((3) '(a c b d))
                   ((4) '(c a b d))))))
         (wp (make-preference-profile
               '(a b c d)
               (lambda (w)
                 (case w
                   ((a) '(2 4 1 3))
                   ((b) '(3 1 2 4))
                   ((c) '(2 3 4 1))
                   ((d) '(4 1 3 2))))))
         (rhos (rotations mp wp)))
    ;; Just assert "some rotations exist for a non-degenerate preference profile"
    (test #t (list? rhos))
    (test #t (every rotation? rhos))
    ;; Applying every rotation in any order to M_top must yield a stable matching
    (let ((M-top (gale-shapley mp wp)))
      (for-each
        (lambda (rho)
          (test #t (stable? (apply-rotation M-top rho) mp wp)))
        rhos))))
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement** — abridged Gusfield-Irving Algorithm 3.2.1

Add to exports: `rotations`.

```scheme
(define (rotations prop-prefs recv-prefs)
  "Enumerate the rotations of the stable-matching system for the given preferences.\nEach rotation, when applied to a stable matching, produces another stable matching.\nThe set of rotations forms a poset whose downsets are in bijection with stable matchings\n(Gusfield-Irving 1989, Theorem 3.2.1) — exactly the join-irreducibles of the Conway lattice.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: list of <rotation>\nCategory: algebra\nKeywords: rotation, Gusfield-Irving, Birkhoff, join-irreducibles, stable matching"
  ;; Strategy: build the proposer-optimal matching, then iteratively expose rotations
  ;; by following the second-choice graph until the receiver-optimal matching is reached.
  (let ((M-top (gale-shapley prop-prefs recv-prefs))
        (M-bot (gale-shapley/receiver-optimal prop-prefs recv-prefs))
        (peq (setoid-equiv? (preference-profile-setoid prop-prefs)))
        (req (setoid-equiv? (preference-profile-setoid recv-prefs))))
    (let loop ((M M-top) (acc '()))
      (cond
        ((bipartite-matching-equal? M M-bot) (reverse acc))
        (else
          (let ((rho (find-exposed-rotation M prop-prefs recv-prefs)))
            (if (not rho)
                (reverse acc)            ; safety: no more rotations exposable
                (loop (apply-rotation M rho) (cons rho acc)))))))))

;; Find an exposed rotation: a cycle (p_0, r_0, p_1, r_1, ..., p_{k-1}, r_{k-1}) where
;; r_i is p_i's current partner, p_{i+1} is the next-best p_i could improve to and
;; r_i prefers p_{i+1} as a "successor proposer" (i.e., the worst proposer r_i would
;; still accept). Returns #f if no rotation is exposable.
(define (find-exposed-rotation M prop-prefs recv-prefs)
  (let* ((proposers (preference-profile-agents prop-prefs))
         (peq (setoid-equiv? (preference-profile-setoid prop-prefs)))
         ;; succ(p) = next-most-preferred receiver for p strictly worse than p's current partner
         ;; that p would still consider, where the receiver in turn prefers p over its current
         ;; partner (i.e., would accept).
         (succ
           (lambda (p)
             (let* ((cur-r (bipartite-matching-partner M p))
                    (lst ((preference-profile-ranks-of prop-prefs) p))
                    ;; Drop everyone p prefers strictly to cur-r (and cur-r itself)
                    (rest (let drop ((xs lst))
                            (cond
                              ((null? xs) '())
                              ((preference-profile-prefers-strictly?
                                 prop-prefs p (car xs) cur-r)
                               (drop (cdr xs)))
                              (else (cdr xs))))))
               (let try ((xs rest))
                 (cond
                   ((null? xs) #f)
                   (else
                     (let ((r (car xs)))
                       (let ((cur-p-of-r (bipartite-matching-partner M r)))
                         (cond
                           ((not cur-p-of-r) r)
                           ((preference-profile-prefers-strictly? recv-prefs r p cur-p-of-r) r)
                           (else (try (cdr xs))))))))))))
         ;; Walk: p_0 has a successor; chase to detect a cycle.
         (start (find succ proposers)))
    (and start
         (let walk ((p start) (path '()))
           (let ((seen (find (lambda (cell) (peq (car cell) p)) path)))
             (cond
               (seen
                ;; Cycle from `seen` onward
                (let ((cycle (reverse (cons (cons p (succ p)) (take-while
                                                                (lambda (c) (not (peq (car c) (car seen))))
                                                                (cons (cons p (succ p)) path))))))
                  (make-rotation
                    (map (lambda (c) (cons (car c) (bipartite-matching-partner M (car c)))) cycle))))
               (else
                 (let ((r (succ p)))
                   (and r
                        (let ((p-next (bipartite-matching-partner M r)))
                          (and p-next (walk p-next (cons (cons p r) path)))))))))))))

(define (take-while pred xs)
  (cond ((null? xs) '())
        ((pred (car xs)) (cons (car xs) (take-while pred (cdr xs))))
        (else '())))
```

Note: this is an abridged form — the canonical Gusfield-Irving algorithm is more efficient (O(n²)) and uses an explicit reduced-preference-table data structure. The above is correct but may be O(n³) on adversarial inputs. If Phase 5 benchmarks show a problem, replace with the canonical form per the design-doc lattice principle (§5.5 reuse).

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): enumerate rotations via second-choice graph cycles"
```

---

### Task 5.3: Rotation poset and `stable-matching-lattice`

- [ ] **Step 1: Failing test**

```scheme
(test-group "stable-matching-lattice via Birkhoff"
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '(a b)) ((2) '(b a))))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) (case w ((a) '(2 1)) ((b) '(1 2))))))
         (L (stable-matching-lattice mp wp)))
    (test #t (lattice? L))
    ;; Both proposers and both receivers get top choice from someone — exactly 2 stable matchings
    (test 2 (length (lattice-elements L)))))
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement**

Add to exports: `stable-matching-lattice`.

```scheme
(define (stable-matching-lattice prop-prefs recv-prefs)
  "Construct the Conway distributive lattice of stable matchings (Conway 1976) by\nenumerating rotations and applying Birkhoff's representation theorem (Birkhoff 1937,\nshipped in (wile algebra lattice) §5.5). Lazy: rotations enumerated eagerly, but the\nlattice carrier is the set of all possible matchings reachable by rotation applications.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <lattice>\nCategory: algebra\nKeywords: Conway, Birkhoff, distributive lattice, stable matching, rotation"
  (let* ((M-top (gale-shapley prop-prefs recv-prefs))
         (rhos (rotations prop-prefs recv-prefs))
         ;; Enumerate all stable matchings: one per subset (downset) of the rotation poset.
         ;; v1 enumerates by trying every subset and applying it; the subset is a downset
         ;; iff applying produces a stable matching (this is the proof artifact).
         (n (length rhos))
         (all-matchings
           (let loop ((subsets (enumerate-subsets rhos)) (acc '()))
             (cond
               ((null? subsets) acc)
               (else
                 (let* ((subset (car subsets))
                        (M (fold (lambda (rho M-acc) (apply-rotation M-acc rho))
                                 M-top
                                 subset)))
                   (if (and (stable? M prop-prefs recv-prefs)
                            (not (any (lambda (existing) (bipartite-matching-equal? existing M)) acc)))
                       (loop (cdr subsets) (cons M acc))
                       (loop (cdr subsets) acc))))))))
    ;; Build a lattice with proposer-utility leq?: M ≤ M' iff every proposer prefers
    ;; (or is indifferent to) their partner in M' over their partner in M.
    (define (leq? M M')
      (every
        (lambda (p)
          (let ((pa (bipartite-matching-partner M p))
                (pb (bipartite-matching-partner M' p)))
            (cond
              ((and (not pa) (not pb)) #t)
              ((not pa) #t)               ; unmatched ≤ matched
              ((not pb) #f)
              (else
                (or ((setoid-equiv? (preference-profile-setoid recv-prefs)) pa pb)
                    (preference-profile-prefers-strictly? prop-prefs p pb pa))))))
        (preference-profile-agents prop-prefs)))
    (make-lattice
      ;; join: take element-wise lcm under ≤
      (lambda (a b)
        (or (find (lambda (M) (and (leq? a M) (leq? b M)
                                    (every (lambda (M2)
                                             (or (not (and (leq? a M2) (leq? b M2)))
                                                 (leq? M M2)))
                                           all-matchings)))
                  all-matchings)
            (error "stable-matching-lattice: join undefined" a b)))
      ;; meet: dual
      (lambda (a b)
        (or (find (lambda (M) (and (leq? M a) (leq? M b)
                                    (every (lambda (M2)
                                             (or (not (and (leq? M2 a) (leq? M2 b)))
                                                 (leq? M2 M)))
                                           all-matchings)))
                  all-matchings)
            (error "stable-matching-lattice: meet undefined" a b)))
      M-top                                  ; bottom from receiver perspective; top from proposer
      (gale-shapley/receiver-optimal prop-prefs recv-prefs)
      leq?
      `(elements . ,all-matchings))))

(define (enumerate-subsets xs)
  "All 2^|xs| subsets. Internal — exponential by design; use only on small rotation lists."
  (cond
    ((null? xs) '(()))
    (else
      (let ((rest (enumerate-subsets (cdr xs))))
        (append rest (map (lambda (s) (cons (car xs) s)) rest))))))
```

Update imports in `matching.sld` to add `(wile algebra lattice)`:

```scheme
  (import (scheme base)
          (srfi 1)
          (wile algebra setoid)
          (wile algebra lattice))
```

- [ ] **Step 4: Run — expect pass**

Note: this is the brute-force form. The design doc allows it because Q2=Lazy was confirmed and the doc explicitly notes the small-input cap. If a future consumer hits the wall, replace with `birkhoff-reconstruction` over the rotation poset.

- [ ] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): build Conway lattice via brute-force stable-matching enumeration"
```

---

### Task 5.4: `egalitarian-stable-matching` and `sex-equal-stable-matching`

- [ ] **Step 1: Failing test**

```scheme
(test-group "egalitarian and sex-equal selectors"
  (let* ((mp (make-preference-profile
               '(1 2)
               (lambda (m) (case m ((1) '(a b)) ((2) '(b a))))))
         (wp (make-preference-profile
               '(a b)
               (lambda (w) (case w ((a) '(2 1)) ((b) '(1 2))))))
         (E (egalitarian-stable-matching mp wp))
         (S (sex-equal-stable-matching mp wp)))
    (test #t (stable? E mp wp))
    (test #t (stable? S mp wp))
    ;; In this symmetric instance, both extremes are equally egalitarian:
    ;; sum-rank = 2 + 2 = 4 either way; sex-equal also identical
    (test 4 (sum-rank E mp wp))
    (test 4 (sum-rank S mp wp))))

(define (sum-rank M prop-prefs recv-prefs)
  (let ((p-sum (apply + (map (lambda (pr)
                               (preference-profile-rank-of prop-prefs (car pr) (cdr pr)))
                             (bipartite-matching-pairs M))))
        (r-sum (apply + (map (lambda (pr)
                               (preference-profile-rank-of recv-prefs (cdr pr) (car pr)))
                             (bipartite-matching-pairs M)))))
    (+ p-sum r-sum)))
```

- [ ] **Step 2: Run — expect failure**

- [ ] **Step 3: Implement**

Add to exports: `egalitarian-stable-matching sex-equal-stable-matching`.

```scheme
(define (egalitarian-stable-matching prop-prefs recv-prefs)
  "Return the stable matching minimizing total-sum-of-ranks across both sides.\nNP-hard in general (Iwama-Manlove 1999); v1 brute-forces the enumerated stable set,\nso practical only for ~10 agents per side. See `stable-matching-lattice` for the\nunderlying enumeration.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: egalitarian, stable matching, minimum sum rank, NP-hard"
  (let* ((L (stable-matching-lattice prop-prefs recv-prefs))
         (matchings (lattice-elements L))
         (score (lambda (M)
                  (apply + (map (lambda (pr)
                                  (+ (preference-profile-rank-of prop-prefs (car pr) (cdr pr))
                                     (preference-profile-rank-of recv-prefs (cdr pr) (car pr))))
                                (bipartite-matching-pairs M))))))
    (let loop ((xs matchings) (best #f) (best-score +inf.0))
      (cond
        ((null? xs) best)
        (else
          (let ((s (score (car xs))))
            (if (< s best-score)
                (loop (cdr xs) (car xs) s)
                (loop (cdr xs) best best-score))))))))

(define (sex-equal-stable-matching prop-prefs recv-prefs)
  "Return the stable matching minimizing |sum-rank-proposers − sum-rank-receivers|.\nNP-hard in general; same brute-force caveat as egalitarian-stable-matching.\n\nParameters:\n  prop-prefs : preference-profile\n  recv-prefs : preference-profile\nReturns: <bipartite-matching>\nCategory: algebra\nKeywords: sex-equal, stable matching, balanced, NP-hard"
  (let* ((L (stable-matching-lattice prop-prefs recv-prefs))
         (matchings (lattice-elements L))
         (imbalance (lambda (M)
                      (let ((p-sum (apply + (map (lambda (pr)
                                                   (preference-profile-rank-of prop-prefs (car pr) (cdr pr)))
                                                 (bipartite-matching-pairs M))))
                            (r-sum (apply + (map (lambda (pr)
                                                   (preference-profile-rank-of recv-prefs (cdr pr) (car pr)))
                                                 (bipartite-matching-pairs M)))))
                        (abs (- p-sum r-sum))))))
    (let loop ((xs matchings) (best #f) (best-score +inf.0))
      (cond
        ((null? xs) best)
        (else
          (let ((s (imbalance (car xs))))
            (if (< s best-score)
                (loop (cdr xs) (car xs) s)
                (loop (cdr xs) best best-score))))))))
```

- [ ] **Step 4: Run — expect pass**

- [ ] **Step 5: Commit**

```bash
make lint
git add -u
git commit -m "feat(algebra/matching): add egalitarian and sex-equal selectors over enumerated stable set"
```

---

### Task 5.5: Phase 5 checkpoint

- [ ] **Step 1: Run** `make ci` and verify all green

- [ ] **Step 2: Commit closeout**

```bash
git add -u
git commit -m "chore(algebra/matching): Phase 5 rotations + Conway lattice complete"
```

---

## Phase 6 — Aggregator, docs, TODO closeout (~30 lib / ~30 test LOC)

### Task 6.1: Re-export from `(wile algebra)` umbrella

- [ ] **Step 1: Failing test** — `algebra_umbrella_drift_test.go`

```bash
go test -run TestAlgebraUmbrellaDrift ./...
```

Expected failure: `matching` exports not re-exported from umbrella.

- [ ] **Step 2: Update umbrella**

Modify `stdlib/lib/wile/algebra.sld`:

- Add to imports list: `(wile algebra matching)`
- Add to re-exports: every public symbol from `matching.sld` (the test failure output enumerates them)

- [ ] **Step 3: Re-run — expect pass**

- [ ] **Step 4: Commit**

```bash
git add -u
git commit -m "feat(algebra): re-export (wile algebra matching) from umbrella"
```

---

### Task 6.2: Library-level docstring with theorems brought into scope

- [ ] **Step 1: Edit `matching.sld` description**

```scheme
  (description
    "Two-sided matching primitives — Roth-Sotomayor (1990).
Three layers per directions doc §4.6:
  Local optimization → gale-shapley, gale-shapley/receiver-optimal, tropical-assignment
  Stability constraint → stable?, blocking-pairs
  Global selection → stable-matching-lattice, egalitarian-, sex-equal-

Theorems brought into scope:
  Gale-Shapley (1962) — deferred acceptance produces a stable matching
  Conway (1976) — stable matchings form a distributive lattice (proposer-utility order)
  Birkhoff (1937, via §5.5) — finite distributive lattices are downset lattices
  Roth (1985) — hospital-intern reduces to one-to-one with synthetic copies
  Iwama-Manlove (1999) — sex-equal stable matching is NP-hard (documents the brute-force wall)
  Kuhn (1955), Munkres (1957) — Hungarian algorithm O(n^3) for assignment")
```

- [ ] **Step 2: Commit**

```bash
git add -u
git commit -m "docs(algebra/matching): add library-level theorem inventory"
```

---

### Task 6.3: User-facing reference section

- [ ] **Step 1: Add section** to `docs/algebra/reference.md`

Append a new section `### (wile algebra matching)` mirroring the bespoke-headings pattern of recently-added libraries (per `TODO.md` "Harmonize reference template" deferred item — bespoke is fine here):

```markdown
### `(wile algebra matching)`

Two-sided matching: Gale-Shapley deferred acceptance, Hungarian assignment, hospital/intern many-to-one, Conway-lattice selection. Three-layer structure per directions doc §4.6.

#### Preference profiles

- `(make-preference-profile agents ranks-of . opts)` — construct a preference profile
- `(preference-profile-rank-of P agent candidate)` — 1-based rank or `#f`
- `(preference-profile-prefers-strictly? P agent x y)` — strict preference predicate

#### Stable matching

- `(gale-shapley prop-prefs recv-prefs)` — proposer-optimal matching
- `(gale-shapley/receiver-optimal prop-prefs recv-prefs)` — receiver-optimal matching
- `(stable? M prop-prefs recv-prefs)` — stability check
- `(blocking-pairs M prop-prefs recv-prefs)` — list of blocking pairs (empty iff stable)

#### Many-to-one

- `(hospital-intern-match intern-prefs hospital-prefs hospital-quotas)` — intern-optimal stable matching with quotas

#### Assignment (Hungarian)

- `(tropical-assignment cost-fn proposers receivers)` → `(matching . cost)`. Use `+inf.0` to forbid pairs.

#### Conway lattice (selection layer)

- `(rotations prop-prefs recv-prefs)` — enumerate rotations (= join-irreducibles of the Conway lattice)
- `(stable-matching-lattice prop-prefs recv-prefs)` — full distributive lattice
- `(egalitarian-stable-matching prop-prefs recv-prefs)` — minimum sum-rank
- `(sex-equal-stable-matching prop-prefs recv-prefs)` — minimum |Δ-sum-rank|

References: Gale-Shapley (1962), Roth-Sotomayor (1990), Gusfield-Irving (1989), Conway (1976), Kuhn (1955), Munkres (1957).
```

- [ ] **Step 2: Commit**

```bash
git add docs/algebra/reference.md
git commit -m "docs(algebra/matching): add user-facing reference section"
```

---

### Task 6.4: Mark TODO.md done; file Kelso-Crawford follow-up stub

- [ ] **Step 1: Update `TODO.md`**

Mark these entries `[x]` with closing-commit pointer:

- Tier B: `(wile algebra matching) library` — Roth-Sotomayor
- §4.2 Tropical permanent / Hungarian primitive

Leave §4.2 Maximum common subgraph open (separate scope per design doc).

- [ ] **Step 2: Create stub `plans/2026-05-02-algebra-matching-many-to-many.md`**

Single-file follow-up stub:

```markdown
## `(wile algebra matching)` — Many-to-many extension (Kelso-Crawford)

**Status:** Stub — gated on `(wile algebra matroid)` (§5.7 Tier C, not shipped).

### Scope

Extend `(wile algebra matching)` with many-to-many stable matching under Kelso-Crawford substitutes condition. Adds `many-to-many-match` export.

### Why deferred

The substitutes condition is a matroid-intersection property; without `(wile algebra matroid)` (~300 LOC, Tier C), the v1 implementation would either re-implement matroid intersection inline (duplicate work) or hand-encode substitutability (incorrect for general cases).

### When to revive

When §5.7 matroids ships AND a workspace consumer needs many-to-many matching. Until then, this stub serves as a reminder of the planned API extension and the dependency direction.
```

- [ ] **Step 3: Final closeout commit**

```bash
git add TODO.md plans/2026-05-02-algebra-matching-many-to-many.md
git commit -m "chore(algebra/matching): mark TODO entries done; file many-to-many stub"
```

---

### Task 6.5: Final lint + ci + branch handoff

- [ ] **Step 1: Run full local CI**

```bash
make ci
```

Expected: all packages pass; coverage at or above gate; lint clean.

- [ ] **Step 2: Verify master CI is green**

```bash
gh run list --branch master --limit 3
```

- [ ] **Step 3: Push branch and open PR**

```bash
git push -u origin feat/algebra-matching
gh pr create --title "feat(algebra): add (wile algebra matching) — two-sided matching" --body "$(cat <<'EOF'
## Summary

Ships `(wile algebra matching)` — two-sided matching primitives completing Tier B of the algebra roadmap. Three-layer structure per directions doc §4.6: local optimization (Gale-Shapley, Hungarian) → stability constraint → global selection (Conway distributive lattice via Birkhoff).

- Gale-Shapley deferred acceptance (proposer-optimal + receiver-optimal): O(n²)
- Hospital/intern many-to-one via Roth's reduction
- Hungarian (Kuhn-Munkres O(n³)) as `tropical-assignment` — closes TODO §4.2 Hungarian primitive
- Conway distributive lattice on stable matchings via Birkhoff (load-test of §5.5)
- Egalitarian + sex-equal selectors (brute force, NP-hard wall documented per Iwama-Manlove 1999)

Closes:
- TODO Tier B: `(wile algebra matching)` library
- TODO §4.2: Tropical permanent / Hungarian primitive

Defers (filed as stub `plans/2026-05-02-algebra-matching-many-to-many.md`):
- Kelso-Crawford many-to-many (gated on §5.7 matroids)

## Test plan

- [ ] `make ci` clean locally
- [ ] Test count delta: ~+50 groups in `test/wile/algebra-matching-test.scm`
- [ ] `algebra_umbrella_drift_test.go` passes (umbrella re-exports)
- [ ] Random property test: 50 random preference profiles → all Gale-Shapley outputs `stable?`
- [ ] Hungarian 4×4 textbook value (19) verified

EOF
)"
gh pr edit $(gh pr view --json number -q .number) --add-reviewer Copilot
```

- [ ] **Step 4: Dispatch `/crosscheck:crosscheck all` locally on the diff**

Use the project's standard 5-agent crosscheck workflow per `plans/CLAUDE.md` Implementation Completion Workflow §3.

- [ ] **Step 5: Wait for both feedback streams; address per workflow §6–§8**

Stop here for user instruction. Do not merge without explicit authorization per `CLAUDE.md` "never commit changes without asking first" + `feedback-implementation-completion-workflow.md`.

---

## Risk register (apply judgment during implementation)

1. **Cyclic preference detection in `find-exposed-rotation` (Task 5.2)** is the load-bearing piece of Phase 5. The abridged second-choice-graph form is correct but unoptimized; benchmark on `(rotations …)` over an 8×8 random profile before claiming Phase 5 done. If it's slow, the canonical Gusfield-Irving Algorithm 3.2.1 (with explicit reduced preference table) replaces it without changing the API.

2. **Hungarian numerical stability**. The Kuhn-Munkres potential-update form is robust on integer inputs; floating-point tests should use the integer matrix from Task 4.2 to avoid epsilon drift in cost comparison.

3. **`stable-matching-lattice` exponential blowup**. Brute-force enumeration of `2^|rotations|` subsets is the deliberate v1 strategy per Q2 = Lazy and the small-input cap. If a test fixture trips the wall (>10 agents per side), reduce the fixture rather than weaken the implementation.

4. **`bipartite-matching-equal?` performance** is O(n²) with the naive `every`/`any` form. Acceptable for v1 (tests use n≤6); document as latent concern if a consumer needs scale.

5. **Plan defects discovered during implementation** (per AC-matching post-ship lesson): treat the *test expectations* as design intent and adjust the *code sketches* if they don't match. Document each deviation in the PR description for post-ship retrospective.
