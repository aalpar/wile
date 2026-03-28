# Scheme Library Docstrings — Design

**Goal:** Add Guile-style docstrings to all 300 eligible `define`/`lambda` procedures across `stdlib/lib/`.

## Mechanism

Already implemented (PR #579, #581). A string literal as the first body expression, when the body has >1 expression, is treated as documentation. Retrieved via `(procedure-documentation proc)`.

## Scope

**In scope:** 300 `(define (name ...) body ...)` procedures across 29 files.

**Excluded:**
- `define-syntax` macros (67) — no compiler support; deferred
- `define name value` aliases (106) — no body
- `define-record-type` forms (14) — generated accessors

## Docstring Conventions

Full convention documented in `CODING_STYLE.md` § "Scheme Docstring Conventions".

Key formatting elements (Emacs Lisp-inspired, within plain strings):

- **First sentence:** Standalone summary
- **Parameters:** UPPER CASE (`"Return the inverse of A in group G."`)
- **Cross-references:** `` `procedure-name' `` (backtick + straight quote)
- **Paragraphs:** Separated by `\n\n`
- **Examples:** `Examples:` header with indented code lines
- **See also:** `See also:` at end with comma-separated `` `name' `` references
- **Pre-formatted:** Indent with 2 spaces after `\n`
- **Math:** Self-contained, no assumed domain knowledge
- **No trailing period** after the last line

Example:
```scheme
(define (group-inverse G a)
  "Return the element that, when combined with A using G's\noperation, yields the identity element. That is,\n(group-op G A (group-inverse G A)) = (group-identity G).\n\nSee also: `group-op', `group-identity'."
  ((group-inverse-fn G) a))
```

## Phases

| Phase | Files | ~Procs | Focus |
|-------|-------|--------|-------|
| 1 | `wile/algebra/*`, `wile/control.scm`, `wile/kanren.scm`, `wile/microkanren.scm` | 100 | Original code. Self-contained math descriptions |
| 2 | `srfi/1/*.scm`, `srfi/1.sld` | 87 | SRFI-1 list library. Lean on spec language |
| 3 | `chibi/diff.scm`, `chibi/test.scm`, `chibi/term/ansi.scm`, `chibi/optional.scm` + `.sld` stubs | 80 | Third-party. Document observed behavior |
| 4 | `scheme/cxr.sld` | 28 | Mechanical CxR compositions |

Each phase: write docstrings → `make test` → review diff → next phase.

## Verification

After each phase: `make test` confirms no regressions. Adding a leading string to a multi-expression body is safe. Single-expression bodies become two-expression (string + original), semantically identical.

## Out of Scope

- `define-syntax` docstrings (needs new compiler mechanism)
- Library-level documentation
- `,doc` REPL command integration
- Documentation generation tooling
