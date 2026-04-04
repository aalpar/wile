# SRFI-42 Syntax Expansion Bugs

**Status:** Bugs B+C fixed. Bug A validator fixed, environment layer analysis in
`2026-04-03-syntax-rules-ellipsis-hygiene-design.md` § "Bug A: Environment Layer".

## Background

SRFI-42 (Eager Comprehensions) is a pure `define-syntax`/`syntax-rules` library that
stress-tests macro expansion. Using the reference implementation as a diagnostic tool
against Wile's expander revealed three bugs in `syntax-rules` template expansion.

## Bug A: Template-introduced identifiers not hygienically distinct

**Severity:** High — silent correctness failures, defeats purpose of hygiene

**Symptom:** When a `syntax-rules` template introduces an identifier (e.g., `i` in
`(:string cc var arg ...) -> (:string cc var (index i) arg ...)`), that identifier
collides with identically-named user-supplied identifiers at the macro use site.

**Minimal repro:**
```scheme
;; :parallel merges :do forms from :range and :string
;; :range binds user var 'i', :string internally introduces 'i' via (index i)
(do-ec (:parallel (:range i 3) (:string ch "abc"))
       (display (list i ch)))
;; => ERROR: duplicate binding name "i" in let form
```

**Expected behavior:** The template-introduced `i` should carry an intro scope making
it distinct from the user-supplied `i`. After merging, the `let` should have two
distinct bindings that happen to print as `i` but are hygienically separate.

**SRFI-42 impact:** Blocks `:parallel` with generators that internally use `(index i)`.
Also affects `vector-of-length-ec` which introduces `(i 0)` in its template.

**Workaround:** User avoids variable names that collide with template-introduced names.

## Bug B: Cross-group ellipsis variables not recognized in shared template repetition

**Severity:** Critical — blocks `:generator-proc` and the `:` dispatch generator

**Symptom:** When pattern variables from different ellipsis groups appear under the
same template ellipsis, only the first group's variables are substituted. Variables
from other groups lose pattern-variable status and fall through to normal resolution.

**Minimal repro:**
```scheme
(define-syntax cross
  (syntax-rules ()
    ((cross (a ...) (b ...))
     (list (cons a b) ...))))

(cross (1 2 3) (10 20 30))
;; => ERROR: no such binding "b" with compatible scopes
```

**Expected behavior:** Both `a` and `b` are depth-1 pattern variables. R7RS 4.3.2
allows pattern variables from different subpatterns at the same ellipsis depth to
appear together in a template repetition, provided the repetition counts match.

**SRFI-42 impact:** Blocks `:generator-proc` which uses `(set! lv ls) ...` where
`lv` is from `((lv li) ...)` and `ls` is from `(ls ...)`. This blocks the entire
runtime dispatch system (`:`, `make-initial-:-dispatch`).

**No workaround** — the pattern is fundamental to SRFI-42's generator protocol.

## Bug C: Nested ellipsis (depth > 1) produces empty expansion

**Severity:** Medium — R7RS conformance gap, low SRFI-42 impact

**Symptom:** Patterns with ellipsis depth > 1 match correctly but the outer ellipsis
iterates zero times during template expansion.

**Minimal repro:**
```scheme
(define-syntax nested
  (syntax-rules ()
    ((nested ((a ...) ...))
     (list (list a ...) ...))))

(nested ((1 2 3) (4 5)))
;; => () — expected ((1 2 3) (4 5))
```

**Expected behavior:** Outer ellipsis should iterate twice, producing two sublists.

**SRFI-42 impact:** None — SRFI-42 doesn't use depth-2 ellipsis.

## Root cause analysis

### Bug B: `findMatchingEllipsisID` assumes single-group ownership

**File:** `internal/match/match.go:492-540`

The function `findMatchingEllipsisID(vars)` receives the set of all pattern variables
found in a template sub-expression and tries to find **one** ellipsis ID that captured
**all** of them. For cross-group usage like `(cons a b) ...` where `a ∈ group 0` and
`b ∈ group 1`, no single ID contains both. It falls back to partial match (first ID
containing any variable), so expansion iterates `ctx.children[0]` only. Within each
child context from group 0, `b` has no binding → falls through to `applyHygieneToSymbol`
→ treated as free identifier → "no such binding."

**Fix direction:** When template variables span multiple ellipsis IDs at the same
depth, the expander must:
1. Detect the multi-group case
2. Verify all groups have equal iteration counts (R7RS error if not)
3. Zip the child contexts: iteration `k` merges bindings from `children[id0][k]`,
   `children[id1][k]`, etc.

**Key constraint:** The merged context must be temporary — don't mutate the capture
tree. Build a synthetic `captureContext` per iteration with bindings from all groups.

**File:** `internal/match/syntax_expand.go:370-438` (`expandSyntaxEllipsis`)

This is where the fix lands. After calling `findMatchingEllipsisID` (or its
replacement), the iteration loop `for _, childCtx := range children` must iterate
the zipped contexts instead of a single group's children.

### Bug C: Nested ellipsis — outer ID's children not traversed

**File:** `internal/match/syntax_expand.go:370-438`

For pattern `((a ...) ...)`, the compiler assigns:
- ID 0 (inner `...`): captures `{a}` — child contexts hold individual `a` values
- ID 1 (outer `...`): captures `{a}` — child contexts hold entire inner sublists

`findSyntaxPatternVariables` on template `(list a ...)` finds `{a}`.
`findMatchingEllipsisID({a})` returns ID 0 (first match, sorted order).
`ctx.children[0]` holds the inner captures from only the **first** outer iteration
(because only the first outer iteration's inner captures are stored as direct children
of the root context).

**Fix direction:** The expander needs to understand ellipsis nesting depth. For a
template `(list a ...) ...`, the inner `...` should use ID 0 within each outer
iteration context, and the outer `...` should use ID 1 on the root context. This
requires tracking which ellipsis depth each template `...` corresponds to — the
current code treats all `...` uniformly.

This is a deeper fix than Bug B. The compiler may need to annotate template ellipsis
with their corresponding pattern ellipsis IDs, or the expander must infer depth from
the capture tree structure.

### Bug A: Template-introduced identifiers — intro scope present but ineffective

**File:** `internal/match/syntax_expand.go:280-290` (`applyHygieneToSymbol`)

The intro scope IS created fresh per macro invocation (`operation_syntax_rules_transform.go:192`)
and IS added to template-introduced identifiers (`syntax_expand.go:287`). But the
downstream `let` compilation apparently doesn't use scope-aware comparison when checking
for duplicate binding names — it likely compares by string key only.

**Confirmed location:** `internal/validate/validate_let.go:390-401`

```go
seen := make(map[string]bool, len(nameSyms))
for _, ns := range nameSyms {
    key := ns.Sym.Key  // bare string — ignores scopes!
    if seen[key] { ... "duplicate binding name" ... }
}
```

Same pattern at line 558-565 for named-let.

**Fix direction:** The duplicate-name check must compare (key, scopes) pairs, not bare
keys. Two identifiers with the same string key but different scope sets are distinct
bindings in a hygienic macro system. The `seen` map key should incorporate scope
identity — e.g., a struct of `{Key string, ScopeID uint64}` or use the existing
`syntax.ScopesMatch` for pairwise comparison.

## Files involved

| File | Bug | Role |
|------|-----|------|
| `internal/match/match.go` | B | `findMatchingEllipsisID` — needs multi-group support |
| `internal/match/syntax_expand.go` | B, C | `expandSyntaxEllipsis` — iteration over zipped/nested contexts |
| `internal/match/syntax_compiler.go` | C | May need depth annotations on ellipsis IDs |
| `internal/validate/validate_let.go` | A | Duplicate binding check at lines 390-401 and 558-565 — uses bare string key, ignores scopes |
| `machine/operation_syntax_rules_transform.go` | — | Intro scope creation (already correct) |

## Test plan

After fixes, the full SRFI-42 reference implementation should load and pass:
1. Bug B fix: `:generator-proc` works → `:` dispatch generator works
2. Bug A fix: `:parallel` with any variable names works → `(index i)` forms work everywhere
3. Bug C fix: nested ellipsis produces correct results
4. End-to-end: `(list-ec (: i 5) (* i i))` → `(0 1 4 9 16)` via dispatch
