# Procedure Unification Detection

**Status**: Proposed
**Foundation**: [wile-goast](https://github.com/aalpar/wile-goast) — all five goast layers (see `plans/GO-STATIC-ANALYSIS.md`)
**Dependencies**: None beyond existing goast infrastructure
**Implementation**: Pure Scheme rule using `(wile goast)` primitives

## Problem

Two procedures that share most of their structure but differ in a few parameterizable ways are a unification candidate — they could be replaced by a single procedure with parameters capturing the differences. Detecting these candidates mechanically is useful for maintaining codebases where duplication evolves gradually.

## Objective Precondition for Unification

There is one objective precondition: **agreement on the shared domain** — where the input spaces overlap, the procedures must produce identical results. If `f(x) != g(x)` for some `x` in both domains, what you have is dispatch behind a flag, not unification.

Everything beyond that is a design judgment: does unification reduce total complexity or increase it? The rule's job is to identify candidates where the complexity equation favors unification, not to make the decision.

## Complexity Model

```
cost_before  = size(f) + size(g)
cost_after   = size(h) + size(param_types) + call_sites * size(param_passing)
```

Unification reduces complexity when `cost_after < cost_before`. The shared structure cancels out — what remains is whether the **parameter overhead** is less than the **duplicated code**.

## Why AST Layer, Not SSA

For unification detection, **AST with type annotations** (via `go-typecheck-package`) is the right level:

- **SSA normalizes too aggressively.** Two functions doing "the same thing with different types" produce completely different SSA instruction sequences because types flow through every instruction. Structural similarity is obscured.
- **AST preserves structure.** Type differences are localized to specific nodes (the `inferred-type` annotation). This makes it easy to identify "same structure, different types" — the core signal for unification.
- **AST retains programmer intent.** Variable names, function names, and structural choices are preserved. SSA erases these, making it harder to classify differences as semantic vs. accidental.

SSA would be more appropriate for detecting *semantic* equivalence (do these compute the same thing?). Unification detection needs *structural* similarity with *parameterizable* differences — that's an AST question.

The SSA and CFG layers could enhance later versions (e.g., "do these functions have isomorphic control flow graphs?"), but they are not needed for v1.

## Detectable Signals

Three signals, all mechanically computable from the AST:

### 1. Structural Similarity Ratio

Recursive comparison of two function ASTs as s-expressions. Each node is a tagged alist `(tag (key . val) ...)` — same tag means compare children; different tag means structural divergence.

Metric: `shared_nodes / total_nodes`. High ratio = unification candidate.

This is the primary filter. It eliminates pairs that happen to share a name or a few lines but are structurally different.

### 2. Difference Classification

Not all AST differences are equal. Each differing node falls into a category:

| Diff kind | Example | Parameterizable as | Unification cost |
|-----------|---------|-------------------|------------------|
| Type name | `int64` vs `uint64` (in `inferred-type` or ident nodes) | Type parameter | Low |
| Literal value | `0` vs `1`, `"add"` vs `"remove"` | Value parameter | Low |
| Identifier (local) | `x` vs `y` (variable names) | Rename (free) | Zero |
| Called function | `f.Inc()` vs `f.Dec()` | Callback / method value | Medium |
| Control flow | `if` present in one, absent in other | Flag parameter + branch | High |
| Structural | Different loop structure, different return paths | Not parameterizable | Reject |

Leaf-only edits (types, literals, identifiers) make cheap unification. Structural edits mean the procedures are genuinely different.

The `inferred-type` annotation from `go-typecheck-package` is key for distinguishing type-name diffs from identifier diffs — without type info, `int64` and `myVar` are both just `(ident (name . "..."))`.

### 3. Difference Regularity

If all diffs are the *same kind* (e.g., every difference is a type substitution `int64` -> `uint64`), a single parameter covers all of them. If diffs are heterogeneous (a type here, a literal there, a callback somewhere else), each needs its own parameter, inflating the unified signature.

Metric: `parameter_count / shared_node_count`. Low = good candidate.

## Rule Architecture

### Pass 0: Candidate Enumeration

```scheme
;; Load all packages, extract func-decl nodes
(define pkgs (go-typecheck-package target))

;; Extract all func-decls with their signature shape
;; Shape = (param-count . return-count)
(define (signature-shape func-decl)
  (let* ((ftype (nf func-decl 'type))
         (params (nf ftype 'params))
         (results (nf ftype 'results))
         (param-count (if (pair? params) (length params) 0))
         (result-count (if (pair? results) (length results) 0)))
    (cons param-count result-count)))

;; Group functions by signature shape to avoid O(n^2) full comparison
;; Only compare functions within the same shape group
```

**Pre-filter rationale:** Two functions with different signature shapes (different param/return counts) cannot be unified without adding/removing parameters, which is a structural difference. Grouping by shape cuts pair enumeration from O(n^2) to O(sum of k_i^2) where k_i is group size.

Additional pre-filters to consider:
- Same receiver type (methods) or both free functions
- Same package (cross-package unification is architecturally suspect)
- Minimum function size (skip trivial 1-3 line functions)

### Pass 1: Structural Comparison (AST Diff)

Core algorithm: recursive s-expression comparison.

```scheme
;; Compare two AST nodes, returning (shared-count diff-count diffs)
;;
;; Two nodes match when:
;;   - Same tag AND all children match (recursively)
;;   - Both are identical atoms (strings, symbols, numbers, booleans)
;;
;; A diff is: (path node-a node-b category)
;;   where path is a list of field keys from root to the diff point

(define (ast-diff node-a node-b path)
  (cond
    ;; Both tagged alists with same tag -> compare field-by-field
    ((and (pair? node-a) (pair? node-b)
          (symbol? (car node-a)) (symbol? (car node-b))
          (eq? (car node-a) (car node-b)))
     (fields-diff (cdr node-a) (cdr node-b) path))

    ;; Both tagged alists with different tags -> structural diff
    ((and (pair? node-a) (pair? node-b)
          (symbol? (car node-a)) (symbol? (car node-b)))
     (make-diff 'structural path node-a node-b))

    ;; Both lists (child node sequences) -> element-wise comparison
    ((and (pair? node-a) (pair? node-b)
          (not (symbol? (car node-a))))
     (list-diff node-a node-b path 0))

    ;; Both atoms, equal -> match
    ((equal? node-a node-b)
     (make-match 1))

    ;; Both atoms, different -> classify the leaf diff
    (else
     (make-diff (classify-leaf node-a node-b) path node-a node-b))))
```

**List diff strategy for v1:** Element-wise positional comparison (zip). If lists differ in length, the extra elements are structural diffs. This is simpler than full tree edit distance and sufficient for functions with the same control flow — their statement/expression lists will be the same length.

Full tree edit distance (handling insertions/deletions) is a v2 enhancement for functions that differ by one added/removed statement.

### Pass 2: Difference Classification

```scheme
;; Classify a leaf diff using AST context and type annotations
(define (classify-leaf node-a node-b)
  (cond
    ;; Both are type-position idents with inferred-type
    ;; (the inferred-type differs -> type parameter candidate)
    ((and (tag? node-a 'ident) (tag? node-b 'ident)
          (nf node-a 'inferred-type) (nf node-b 'inferred-type)
          (not (equal? (nf node-a 'inferred-type)
                       (nf node-b 'inferred-type))))
     'type-name)

    ;; Both are idents without type info, or with same type
    ;; (variable rename -> free unification)
    ((and (tag? node-a 'ident) (tag? node-b 'ident))
     'identifier)

    ;; Both are literals with same kind but different value
    ((and (tag? node-a 'lit) (tag? node-b 'lit)
          (equal? (nf node-a 'kind) (nf node-b 'kind)))
     'literal-value)

    ;; Both are literals with different kind
    ((and (tag? node-a 'lit) (tag? node-b 'lit))
     'type-name)

    ;; Both are symbols (operators, tokens) -> operator diff
    ((and (symbol? node-a) (symbol? node-b))
     'operator)

    ;; Fallback
    (else 'structural)))
```

### Pass 3: Scoring and Reporting

```scheme
;; Weights for different diff categories
(define diff-weights
  '((type-name . 1)      ;; one type parameter covers all
    (literal-value . 1)   ;; one value parameter per distinct literal
    (identifier . 0)      ;; free: just a rename
    (operator . 2)        ;; callback or flag parameter
    (structural . 100)))  ;; effectively rejects the pair

;; Score a diff result
(define (unification-score shared-count diffs)
  (let* ((diff-cost (apply + (map (lambda (d)
                                     (cdr (assoc (diff-category d) diff-weights)))
                                   diffs)))
         ;; Count distinct parameter types needed
         (type-params (unique (filter-map
                                (lambda (d)
                                  (and (eq? (diff-category d) 'type-name)
                                       (cons (diff-value-a d) (diff-value-b d))))
                                diffs)))
         (value-params (filter (lambda (d) (eq? (diff-category d) 'literal-value)) diffs))
         (param-count (+ (length type-params) (length value-params)))
         ;; Similarity ratio
         (total (+ shared-count (length diffs)))
         (similarity (if (> total 0) (/ shared-count total) 0)))
    (list similarity param-count diff-cost diffs)))

;; Threshold: report pairs with >= 70% structural similarity
;; and no structural diffs
(define similarity-threshold 0.70)

(define (report-candidate func-a func-b score)
  (let ((name-a (nf func-a 'name))
        (name-b (nf func-b 'name))
        (similarity (car score))
        (param-count (cadr score))
        (diffs (cadddr score)))
    (display "  Candidate: ") (display name-a)
    (display " <-> ") (display name-b)
    (display "  similarity=") (display similarity)
    (display "  params-needed=") (display param-count)
    (newline)
    ;; Detail the diffs
    (for-each
      (lambda (d)
        (display "    ") (display (diff-category d))
        (display " at ") (display (diff-path d))
        (display ": ") (display (diff-value-a d))
        (display " -> ") (display (diff-value-b d))
        (newline))
      diffs)))
```

## Existing Prior Art in This Codebase

The `state-trace-full.scm` example (in [wile-goast](https://github.com/aalpar/wile-goast) `examples/goast-query/`) demonstrates the exact multi-layer analysis pattern:

- **Pass 1 (AST):** `walk` + `tag?` + `nf` for structural pattern detection (boolean clusters)
- **Pass 2 (AST):** If-chain field sweep detection via recursive condition extraction
- **Pass 3 (SSA):** `go-ssa-build` + field-addr/store correlation for mutation independence
- **Pass 4 (CFG):** `go-cfg` + `go-cfg-dominators` + `go-cfg-dominates?` for ordering

The `walk` function, `nf` (node-field), `tag?`, `filter-map`, and `flat-map` utilities from that example are directly reusable. The unification rule adds a new visitor pattern (pairwise tree diff) but the traversal infrastructure is identical.

## Known Concrete Example

In the sibling `crdt` project, `pncounter` and `gcounter` share the find-own-dot/replace/build-delta pattern (`int64` vs `uint64`). This rule should detect them as candidates with a small number of type-name diffs. However, gcounter has a grow-only constraint (no decrement) — a semantic difference that manifests as a missing method, not an AST diff within shared methods. The rule would correctly identify the *shared* methods as unification candidates while the *absent* method would not appear in the comparison.

This is a good test case: the rule should flag the similarity, and the human should decide that the grow-only constraint is a domain-agreement violation that prevents full unification. Partial unification (shared helper for the increment path) might still be valuable.

## Limitations

### What this rule cannot detect

1. **Semantic equivalence.** Two functions that compute the same result through different algorithms will not be flagged — their ASTs are structurally different.
2. **Domain agreement violations.** The objective precondition (agreement on shared domain) is a semantic property that cannot be checked statically from AST structure alone.
3. **Cross-function duplication.** A pattern repeated *within* different functions (not the entire function body) requires sub-tree matching, not whole-function comparison. This is a different problem (clone detection at the fragment level).
4. **Macro-generated duplication.** Go doesn't have macros, but code generators (`go generate`) can produce structural clones that this rule would flag. Whether that's useful depends on whether the generator should be fixed.

### What the scoring cannot capture

The scoring function approximates "does unification reduce complexity?" but cannot fully answer it:

- **Call-site cost** is not measured (how many callers would need to change?)
- **Cognitive cost** of the unified function's signature is subjective
- **Package boundary effects** (would unification create a new dependency?) are not checked

These require human judgment. The rule identifies candidates; the human decides.

## Future Enhancements (v2+)

- **SSA-level comparison:** For functions that pass the AST filter, compare their SSA representations to detect "same control flow, different types" at a normalized level. This catches cases where syntactic differences (variable naming, expression ordering) mask structural similarity.
- **CFG isomorphism:** Compare control flow graphs to detect functions with identical branching structure but different computations. Combined with the AST diff, this distinguishes "same algorithm, different types" from "different algorithm, same types."
- **Sub-tree matching:** Detect duplicated code *fragments* within different functions, not just whole-function similarity. Requires sliding-window or suffix-tree approaches on the s-expression representation.
- **Call graph context:** Use `go-callgraph` to find functions that call the same set of dependencies — a pre-filter that narrows candidates to functions with similar "purpose signatures."
- **Cross-package analysis:** Compare functions across packages to detect library-level duplication. Requires care to avoid false positives from intentional encapsulation.
