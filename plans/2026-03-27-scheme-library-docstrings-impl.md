# Scheme Library Docstrings Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add Guile-style docstrings to all ~300 eligible `define`/`lambda` procedures across `stdlib/lib/`.

**Architecture:** No code changes — only content additions. Each eligible `define` gets a leading string literal in its body. The existing compiler docstring extraction (PR #579) handles the rest. Single-expression bodies become two-expression bodies (docstring + original), which is semantically identical.

**Tech Stack:** Scheme (stdlib source files). Go test suite for regression verification.

**Design doc:** `plans/2026-03-27-scheme-library-docstrings-design.md`

---

## Conventions (apply to ALL tasks)

**Eligibility:** Only `(define (name ...) body ...)` and `(define name (lambda (...) body ...))` forms with at least one body expression. NOT eligible: `define-syntax`, `define-record-type`, `(define name value)` aliases.

**Docstring format:**
- First sentence: standalone summary (works in a listing)
- Parameters in UPPER CASE: `"Return the inverse of A in group G."`
- Math concepts explained inline — no assumed domain knowledge
- Newlines via `\n` within the string literal
- No trailing period after the last line

**Placement:** Docstring is the FIRST expression in the body, before any existing code:
```scheme
;; Before:
(define (foo x)
  (+ x 1))

;; After:
(define (foo x)
  "Add one to X and return the result."
  (+ x 1))
```

**Internal helpers** (prefixed with `%` or nested `define` inside `let` bodies): Include docstrings. They're still callable and introspectable.

**`case-lambda`**: Docstring goes in the FIRST clause only (that's what `procedure-documentation` reads):
```scheme
(define fixpoint
  (case-lambda
    ((L f x)
     "Compute the least fixpoint of F starting from X in lattice L.\n..."
     (let loop ...))
    ((L f x fuel)
     (let loop ...))))
```

---

## Task 1: Phase 1a — `wile/algebra/` libraries

**Files to modify:**
- `stdlib/lib/wile/algebra/monoid.scm`
- `stdlib/lib/wile/algebra/group.scm`
- `stdlib/lib/wile/algebra/order.scm`
- `stdlib/lib/wile/algebra/semiring.scm`
- `stdlib/lib/wile/algebra/ring.scm`
- `stdlib/lib/wile/algebra/lattice.scm`
- `stdlib/lib/wile/algebra/galois.scm`
- `stdlib/lib/wile/algebra/rewrite.scm`

**Procedure count:** ~80

**Voice:** Self-contained mathematical descriptions. Explain what the algebraic concept means, not just what the code does. Reference the algebraic laws being checked in validators.

**Exemplar docstrings (match this voice for all algebra procedures):**

```scheme
;; monoid.scm
(define (monoid-op M a b)
  "Apply monoid M's binary operation to A and B.\nA monoid operation is associative: combining A with the result\nof combining B and C gives the same result as combining the\nresult of A and B with C."
  ((monoid-op-fn M) a b))

(define (monoid-fold M lst)
  "Fold LST using monoid M's operation, starting from M's identity.\nCombines all elements of LST left-to-right. Returns the identity\nelement for an empty list."
  (let loop ...))

(define (validate-monoid M samples)
  "Spot-check that M satisfies the monoid laws on SAMPLES.\nTests left identity, right identity, and associativity for all\nelements and triples in SAMPLES. Returns #t if all laws hold,\nor a list of (violation-type element ...) entries describing failures."
  (let ...))

;; lattice.scm
(define (flat-lattice elements equal?)
  "Construct a flat lattice over ELEMENTS using EQUAL? for comparison.\nIn a flat lattice, all elements are incomparable to each other\nbut sit between a bottom element (less than everything) and a top\nelement (greater than everything). The lattice join of two unequal\nelements is top; their meet is bottom."
  (let ...))

;; rewrite.scm
(define (axiom->rules axiom proto)
  "Compile AXIOM into a list of rewrite-rule procedures using term protocol PROTO.\nEach rule is a procedure (term -> value-or-*no-match*) that attempts\none rewriting step. Identity axioms produce two rules (left and right),\ncommutativity produces one rule that normalizes by term ordering,\nand involution produces one rule that collapses f(f(x)) to x."
  (cond ...))
```

**Step 1:** Read each file, add docstrings to all eligible `define` forms following the exemplar voice.

**Step 2:** Run tests to verify no regressions.

Run: `make test`
Expected: PASS

**Step 3:** Commit (do NOT push).

```
docs(stdlib): add docstrings to wile/algebra libraries
```

---

## Task 2: Phase 1b — `wile/control.scm`, `wile/kanren.scm`, `wile/microkanren.scm`

**Files to modify:**
- `stdlib/lib/wile/control.scm`
- `stdlib/lib/wile/kanren.scm`
- `stdlib/lib/wile/microkanren.scm`

**Procedure count:** ~25

**Voice:** Explain the programming-language-theory concepts (continuations, logic programming, unification) for someone who hasn't read the papers.

**Exemplar docstrings:**

```scheme
;; microkanren.scm
(define (var c)
  "Create a logic variable identified by the integer C.\nLogic variables are represented as single-element vectors.\nTwo variables are the same if they have the same identifier."
  (vector c))

(define (unify u v s)
  "Attempt to make U and V equal under substitution S.\nWalks both values to their current bindings, then extends S\nwith new associations as needed. Returns the extended substitution\non success, or #f if U and V cannot be made equal.\nHandles pairs recursively and uses eqv? for atoms."
  (let ...))

(define (mplus $1 $2)
  "Interleave two answer streams $1 and $2.\nIf $1 is empty, return $2. If $1 is a suspension (procedure),\nreturn a suspension that swaps the arguments, ensuring fair\nenumeration of both branches. Otherwise cons the first answer\nof $1 and interleave the rest with $2."
  (cond ...))

;; kanren.scm
(define (walk* v s)
  "Deeply walk value V under substitution S.\nLike walk, but also recursively resolves any pairs found after\nwalking. Produces a fully resolved value with no remaining\nsubstitutable variables except unbound ones."
  (let ...))

;; control.scm
(define (%prompt-reinstall tag thunk)
  "Install a continuation prompt tagged TAG, run THUNK, and\nreinstall the same prompt if an abort delivers a new thunk.\nUsed internally by prompt-at, reset-at, prompt, and reset to\nprovide handler-reinstalling behavior."
  (call-with-continuation-prompt ...))
```

**Step 1:** Read each file, add docstrings to all eligible `define` forms.

**Step 2:** Run tests.

Run: `make test`
Expected: PASS

**Step 3:** Commit.

```
docs(stdlib): add docstrings to wile/control, kanren, microkanren
```

---

## Task 3: Phase 2 — `srfi/1/` list library

**Files to modify:**
- `stdlib/lib/srfi/1/constructors.scm`
- `stdlib/lib/srfi/1/predicates.scm`
- `stdlib/lib/srfi/1/selectors.scm`
- `stdlib/lib/srfi/1/fold.scm`
- `stdlib/lib/srfi/1/search.scm`
- `stdlib/lib/srfi/1/deletion.scm`
- `stdlib/lib/srfi/1/alists.scm`
- `stdlib/lib/srfi/1/lset.scm`
- `stdlib/lib/srfi/1/misc.scm`
- `stdlib/lib/srfi/1.sld` (any inline `define`s)

**Procedure count:** ~87

**Voice:** SRFI-1 spec language, adapted. These are well-known list operations — describe what they do precisely, including edge cases for empty lists, circularity, and equality predicates.

**Exemplar docstrings:**

```scheme
;; constructors.scm
(define (xcons a b)
  "Construct a pair with B as car and A as cdr.\nLike cons with arguments reversed. Useful as a combiner in\nfold where the accumulator position is swapped."
  (cons b a))

(define (iota count . o)
  "Return a list of COUNT numbers starting from START with step STEP.\nSTART defaults to 0, STEP defaults to 1. For example,\n(iota 5) produces (0 1 2 3 4) and (iota 3 1 2) produces (1 3 5)."
  (let ...))

;; fold.scm
(define (fold kons knil lis1 . lists)
  "Apply KONS across the elements of one or more lists, accumulating\nfrom KNIL. For a single list, computes\n  (kons eN ... (kons e1 knil))\nwhere e1..eN are the list elements. For multiple lists, KONS\nreceives one element from each list plus the accumulator.\nProcesses elements left-to-right. Unspecified behavior on\ncircular lists."
  ...)

;; search.scm
(define (find pred list)
  "Return the first element of LIST satisfying predicate PRED,\nor #f if no element matches. Note: cannot distinguish between\n#f as a found value and #f as not-found. Use find-tail when\nthe list may contain #f."
  ...)
```

**Step 1:** Read each file, add docstrings to all eligible `define` forms.

**Step 2:** Run tests.

Run: `make test`
Expected: PASS

**Step 3:** Commit.

```
docs(stdlib): add docstrings to SRFI-1 list library
```

---

## Task 4: Phase 3 — `chibi/` libraries

**Files to modify:**
- `stdlib/lib/chibi/diff.scm`
- `stdlib/lib/chibi/diff.sld` (inline defines)
- `stdlib/lib/chibi/test.scm`
- `stdlib/lib/chibi/test.sld` (inline defines)
- `stdlib/lib/chibi/term/ansi.scm`
- `stdlib/lib/chibi/optional.scm`

**Procedure count:** ~80

**Voice:** Document observed behavior. These are third-party ports — describe what the code does, referencing Chibi-Scheme origins where relevant. For `chibi/test`, reference SRFI-64 where applicable.

**Exemplar docstrings:**

```scheme
;; diff.scm
;; (Read the actual code to understand what each procedure does,
;;  then document the observed behavior.)

;; test.scm
;; Focus on the public API procedures — test-begin, test-end,
;; test-assert, test-equal, test-error, etc.
;; Internal helpers get brief one-liners.

;; term/ansi.scm — procedures that generate ANSI escape sequences
;; Document what escape sequence is produced and what visual effect it has.
```

**Step 1:** Read each file carefully (these are larger and less familiar), add docstrings.

**Step 2:** Run tests.

Run: `make test`
Expected: PASS

**Step 3:** Commit.

```
docs(stdlib): add docstrings to chibi libraries (diff, test, ansi, optional)
```

---

## Task 5: Phase 4 — `scheme/cxr.sld`

**File to modify:**
- `stdlib/lib/scheme/cxr.sld`

**Procedure count:** 28

**Voice:** Mechanical — describe the composition of car/cdr operations. Use a consistent pattern.

**Pattern for all 28 CxR procedures:**

```scheme
(define (caar x)
  "Return (car (car X)).\nEquivalent to taking the car of the car of a nested pair structure."
  (car (car x)))

(define (cadr x)
  "Return (car (cdr X)).\nExtracts the second element of a proper list."
  (car (cdr x)))

(define (caddr x)
  "Return (car (cdr (cdr X))).\nExtracts the third element of a proper list."
  (car (cdr (cdr x))))

(define (cddddr x)
  "Return (cdr (cdr (cdr (cdr X)))).\nEquivalent to four successive cdr operations."
  (cdr (cdr (cdr (cdr x)))))
```

The pattern: first line is the expansion, second line is a human-readable interpretation (e.g., "nth element of a list" for `ca+r` patterns, "composition of N operations" for `cd+r` patterns).

**Step 1:** Add docstrings to all 28 CxR procedures.

**Step 2:** Run tests.

Run: `make test`
Expected: PASS

**Step 3:** Commit.

```
docs(stdlib): add docstrings to scheme/cxr library
```

---

## Task 6: Final verification and wile/er-macro-test.scm

**Step 1:** Check if `wile/er-macro-test.scm` has any eligible `define` forms.

Note: This file contains only `define-syntax` forms — NOT eligible. Skip.

**Step 2:** Run full verification.

Run: `make lint && make covercheck`
Expected: PASS

**Step 3:** Run integration tests if any exist for docstrings.

Run: `make test`
Expected: PASS

**Step 4:** Verify docstrings are accessible at runtime (manual spot-check).

Run a quick REPL test:
```scheme
(import (wile algebra monoid))
(procedure-documentation monoid-op)
;; Should return the docstring as a string, not #f
```

**Step 5:** Final commit if any fixups needed.

```
fix: address any docstring regressions
```
