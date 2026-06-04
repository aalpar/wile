# SRFI-204 `match` — Design

**Status**: Design draft. Implementation plan to follow in a separate `-impl.md` file.
**Date**: 2026-06-04.
**Related work**: `internal/match/` — the *syntax-rules* pattern matcher (compile-time syntax-object matching for the macro expander). This work is a **separate engine**; it does not modify or unify with `internal/match` (see §3). `internal/validate/register.go` — the 7-site "ADDING A NEW SPECIAL FORM" recipe this work follows. `machine/compilation/compile_syntax_case.go` — precedent for a special form that runs a matcher and binds captured locals. `extensions/algebragraph/` / `2026-05-30-sat-solver-design.md` — precedent for a Go kernel with a Scheme frontend.

---

## 1. Motivation

Wile has no `match` form. SRFI-204 (the Wright–Cartwright–Shinn pattern matcher, standardizing Alex Shinn's `match`) is high-value for two reasons:

1. **Ergonomics.** wile-goast's analysis code destructures Go-AST-shaped nested lists pervasively: **101 nested `car`/`cadr`/`caddr`/`cddr` accessor uses** across its `.scm` files, plus dense `cond`/`case` dispatch (`belief.scm` 42 destructuring idioms, `unify-detect-pkg.scm` 33, `split.scm` 26, `unify.scm` 22). `match` collapses these car/cdr/cond chains into declarative patterns. wile-goast consumes `match` as a **Scheme form** — it needs no Go API.

2. **A reusable Go matcher core.** A Go-callable `Match(pattern, value) → bindings` lets embedders (and, prospectively, internal Go code) destructure `values.Value` structurally instead of by hand-rolled type switches. This is Wile's stated product direction ("embedding is the product"). The internal/embedding consumer is currently *prospective*, not demonstrated — this is an accepted, eyes-open bet (see §2 non-goals and §7).

### 1.1 Why not port the reference macro (verified, not assumed)

The conventional way to get `match` is to drop in Shinn's portable `syntax-rules` reference (the basis SRFI-204 standardizes). We **tested this against Wile's actual expander** (chibi `match.scm`, 1232 lines) rather than assuming. It does **not** run unmodified:

- **Reader gap.** Wile rejects the bare identifier `@` (`'@` → parse error). The reference uses `@` as the `object`-pattern keyword. Wile is *correct* here per R7RS — `@` is not a valid identifier `<special initial>` (`! $ % & * / : < = > ? ^ _ ~`). So the canonical source is non-portable on this token.
- **Expansion failure.** After renaming `@`→`at` (4 source lines), the file parses and expands, but the core `match → match-next → match-one → match-two` dispatch throws **"no matching clause" on even the simplest pattern** `(match '(1 2 3) ((a b c) ...))`. Root cause not pinned; it is *not* the `(... ...)` ellipsis-escape (Wile supports that — verified — and the reference does not use it). It lies somewhere in chibi's literal-identifier dispatch idiom.

**Conclusion:** the "zero-Go, proven" appeal of an all-Scheme port is undercut — it is not a drop-in and would require expander debugging or nontrivial macro surgery before it runs at all. A Go implementation sidesteps the expander entirely.

A Go-side *expander-to-core-forms* approach (compile each pattern to nested `if`/`let`/`car`/`cdr`) was also considered. It gives optimal runtime and avoids expander stress, but the matching logic lives in code generation, not in a value matcher — so it yields **no reusable Go API**, which is requirement (2). Rejected on that basis.

---

## 2. Scope and non-goals

**In scope (eventual — full SRFI-204):** `_`, identifier (bind), literal (`equal?`/`eqv?`/`quote`), list/dotted, `... ` ellipsis with fixed tails, vectors, `and`/`or`/`not`, `?` predicate, `=` accessor, `$`/`struct`/`object` records, quasiquote patterns, `***` tree, `set!`/`get!`, `..1`/`___`/`=..`/`*..` bounded ellipsis, `=>` failure continuation. Derived forms `match-lambda`, `match-lambda*`, `match-let`, `match-let*`, `match-letrec`, `match-named-let`.

**Phase 1 (this work — the high-value structural 80%):** see §6.

**Non-goals:**

- **No unification with the syntax-rules matcher.** `internal/match` stays the syntax-object engine; this is a separate value-matcher engine (§3). Shared structural helpers may be extracted *later*, only if duplication is demonstrated.
- **No `@` keyword.** The chibi `@` alias is unreadable in Wile and non-portable per R7RS. SRFI-204's `object` keyword covers the same need. Documented deviation in `docs/reference/r7rs-differences.md`.
- **No bytecode-specialized pattern compilation in v1.** Patterns are parsed to a Go AST and interpreted at runtime (§4.3). The AST is the IR a future bytecode backend would consume; specialize only if a profile demands it.

---

## 3. Two engines, not one (the central architectural decision)

A natural instinct is to make this new matcher *replace* `internal/match`. **Rejected, with reasons.** The two share only surface shape ("walk pairs/vectors, handle ellipsis, capture variables"); their contracts diverge on five **semantic** axes:

| Axis | `internal/match` (syntax-rules) | this engine (SRFI-204) |
|---|---|---|
| Subject | syntax objects (carry scope sets) | runtime `values.Value` |
| Timing | compile/expansion time | runtime |
| Literal semantics | hygienic binding resolution (`LiteralMatcher` callback, `match.go`) | `equal?`/`eqv?` on data |
| Output contract | depth-keyed capture tree → drives template expansion (`captureStack`, `ellipsisVars`, `ellipsisDepths`) | flat value bindings + control dispatch |
| Control | deterministic single-pass | backtracking (`or`, `***`) + evaluates `?`/`=` |

Applying the project's own refactoring test — *"verify by substitution: can every call site use the unified function?"* — it **fails**: the six syntax-case call sites need depth-keyed hygienic output a value matcher would never produce; conversely this engine needs predicate evaluation and backtracking the syntax matcher never performs. A unified engine would be the *union* of both contracts, dominated by mode-selecting conditionals.

The risk is also asymmetric. `internal/match` is the most correctness-sensitive component in the language — a hygiene regression breaks *every* macro. Coupling greenfield code to it to win DRY trades a small dedup gain for a blast radius of "the entire macro system." Two engines is the correct boundary.

---

## 4. Architecture

### 4.1 Package boundary

New package **`internal/patmatch`** (deliberately not `internal/match`). Depends only on `values/` and `werr/` — **no VM dependency**. VM-requiring operations (applying a `?` predicate or `=` accessor procedure) enter through an injected interface, keeping the package unit-testable and Go-callable without a running machine:

```go
// Evaluator abstracts the single VM-dependent operation the matcher needs:
// applying a Scheme callable. Pure-Go callers supply an implementation that
// wraps Go funcs (or errors for ?/= if unsupported); the `match` form supplies
// a VM-backed one.
type Evaluator interface {
    Apply(proc values.Value, args ...values.Value) (values.Value, error)
}
```

### 4.2 Pattern AST + parser

```go
// Parse turns a pattern datum into a Pattern AST. Reports malformed patterns.
func Parse(datum values.Value) (Pattern, error)

// PatternVars returns, in left-to-right order, the identifiers a pattern binds.
// The compiler uses this to pre-allocate locals; the parser uses it to enforce
// SRFI-204's rule that every `or` branch binds the same variable set.
func PatternVars(p Pattern) []string
```

AST node kinds (Phase 1 marked ✓): `Wild` ✓ (`_`), `Var` ✓, `Lit` ✓ (quote/self-eval), `PairPat` ✓ (incl. dotted tail), `Ellipsis` ✓ (sub-pattern + fixed-length tail), `VectorPat` ✓, `And`/`Or`/`Not` ✓, `Pred` ✓ (`?`); `Field` (`=`), `Record` (`$`/`struct`/`object`), `Quasi`, `Tree` (`***`), bounded-ellipsis counts — Phase 2.

### 4.3 The matcher

```go
// Match interprets pat against v, recording captures in b. Returns (true,nil)
// on match, (false,nil) on clean mismatch, (false,err) on a structural error
// (e.g. ctx cancellation surfaced via ev, malformed runtime input). ev is
// consulted only for ?/= sub-patterns.
func Match(pat Pattern, v values.Value, b *Bindings, ev Evaluator) (ok bool, err error)
```

Interprets the AST against a runtime value: structural walk of pairs/vectors, ellipsis collection **into flat lists** (not depth-keyed trees — that is the syntax engine's job), `equal?`/`eqv?` literals, `and`/`or`/`not` with backtracking, and `ev.Apply` for `?`/`=`. **Bodies and guards are not its concern** — they are compiled bytecode that runs after a successful match binds the captures (§4.4).

The runtime-interpreted design (vs compiling each pattern to inline bytecode) is the explicit v1 trade: slightly slower per match than inlined `car`/`cdr`, but it is the same core the Go API exposes, and far cheaper at compile time than the reference macro's expansion bloat.

### 4.4 The `match` special form

Follows the 7-site recipe in `internal/validate/register.go`:

- **Validator** (`internal/validate/validate_match.go`): shape-checks `(match expr clause ...)`; each clause is `(pattern body ...)`, `(pattern (=> fail) body ...)`, or `(pattern guard body ...)`.
- **Compiler** (`machine/compilation/compile_match.go`): compiles `expr` once to a temp; for each clause, **parses the pattern at compile time** (the resulting AST becomes a bytecode constant — no per-execution re-parse), allocates locals for `PatternVars`, and emits code that runs the matcher, on success binds captures + evaluates guard/body, on failure falls through to the next clause, and raises `werr.ErrNotAMatch` (wrapped) when no clause matches.
- **Matcher invocation mechanism** (dedicated opcode vs VM hook) is settled in the impl plan. The binding constraint: `?`/`=`/`or`-backtracking must be able to call back into the VM mid-match via the evaluator. `compile_syntax_case.go` is the precedent for "run a matcher, then bind captured locals into the clause body."

### 4.5 Derived forms = thin Scheme sugar

`match-lambda`, `match-lambda*`, `match-let`, `match-let*`, `match-letrec`, `match-named-let` ship as **one-level `syntax-rules` macros** in `stdlib/lib/srfi/204.scm` that expand to `match`. This is the correct division of labor: the expander-stressing part (`match` itself) is Go; the trivial sugar is Scheme. A one-level `(match-lambda c ...) → (lambda (x) (match x c ...))` does not stress the expander. (Not yet verified on Wile — in the §8 premise tests the derived-form checks failed *transitively* because the reference `match` itself failed; they will be validated against our Go `match` in Phase 1.)

---

## 5. Error handling

- Reuse `werr.ErrNotAMatch` (already a sentinel, re-exported by `internal/match`) for "no clause matched", wrapped with context via `werr.WrapForeignErrorf`.
- Malformed patterns at compile time: reuse an existing `werr` sentinel if one fits; add `werr.ErrMalformedPattern` only if none does (decision deferred to the impl audit of `werr/werr.go`).
- Never bare sentinels; never `fmt.Errorf`. `errors.Is`/`errors.As` only.

---

## 6. Phasing

**Phase 1 — structural core + the high-value 80%:**
`internal/patmatch` package, AST + parser, matcher for `_`, var, literal, pair/dotted, `p ...` ellipsis (with fixed tails), vector, `and`/`or`/`not`, `? pred`. The `match` special form (validator + compiler). The Go `Match` API. `match-lambda`/`match-let`/`match-let*` sugar. Go unit tests (table-driven) + `integration/testdata/srfi-204-tests.scm`.

**Phase 2 — full SRFI-204:**
`= accessor`, `$`/`struct`/`object` records, quasiquote patterns, `***` tree, `set!`/`get!`, `..1`/`___`/`=..`/`*..` counts, `=>` failure continuation, `match-letrec`/`match-named-let`. Library registration `(srfi 204)` and any aliases.

---

## 7. Open decisions (for user review)

- **D1 — Runtime-interpreted matcher (recommended, baked into §4.3).** Accept the slight per-match cost for the Go API + cheap compile time. Alternative (bytecode-specialized patterns) deferred behind the AST as IR.
- **D2 — `? pred` in Phase 1 (recommended, baked into §6).** It is high-value and needs only `Evaluator.Apply`. Could be deferred to Phase 2 with `=`/`$` to keep the Phase-1 core purely structural; chosen to include it.
- **D3 — `@` keyword permanently excluded (§2 non-goals).** Confirm `object` suffices and we document the deviation rather than extending the reader to accept `@`.

---

## 8. Verification appendix (premise tests run 2026-06-04)

| Check | Method | Result |
|---|---|---|
| Reference macro runs on Wile's expander? | Ran chibi `match.scm` (1232 lines) via the `wile` CLI | **No** — reader rejects `@`; after fix, core dispatch fails "no matching clause" on trivial input |
| `(... ...)` ellipsis-escape supported? | Minimal `define-syntax` generating a macro using `(... ...)` | **Yes** — Wile supports it; not the cause |
| `@` a valid identifier? | `'@` datum read | **No** — parse error; correct per R7RS `<special initial>` |
| wile-goast ergonomics need | Counted destructuring idioms across `.scm` | 101 nested car/cdr; cond/case-dense files (42/33/26/22) |
| Internal Go consumer for a `Match` API? | grep'd consumers of `internal/match` | None for a *value* matcher; the 6 syntax-case consumers want the *syntax* engine (kept separate) |
