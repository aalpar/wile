# MCP Triggering Rewrite (Lever A) — Wile

**Status:** Design — awaiting user review
**Date:** 2026-04-18
**Scope:** wile MCP server only (wile-goast deferred)
**Type:** Text-only change — no code logic, no new tools, no architectural change

---

## Summary

Rewrite three text artifacts of the wile MCP server — `WithInstructions`, the
nine per-tool descriptions, and the `wile-scheme.md` prompt — to *trigger* LLM
tool use on the domains where Wile reliably outperforms LLM-native arithmetic.
The split between loaded and available libraries is preserved; the misleading
`libraries` tool description that hides the catalog is corrected.

## Motivation

The `algebra-accuracy` benchmark (`~/ClaudeProjects/LLMAccuracy/algebra-accuracy/`)
shows the Wile MCP tools dramatically improve Sonnet 4.6 accuracy on algebraic
problems — overall 58/125 → 119/125 correct, with deltas of +80–100 percentage
points at break-zone difficulties for `modular_arithmetic`, `monoid_power`, and
`rational_field`. The tools work *when used*.

However, `powerset_lattice` regresses under treatment (−10% at hard, −20% at
extra-hard). Trace analysis (`docs/mcp-documentation-tools.md`) identifies the
failure: the LLM calls `doc("(wile algebra)")`, receives "not loaded," and
falls back to manual computation that exhausts the 10-round budget before
producing an answer. The root cause is *not* the loaded-vs-available split —
the split is an intentional architectural decision the maintainer wants to
preserve. The root cause is that the LLM never discovers the catalog.

The `libraries` tool *does* return the full catalog — verified by direct call
on 2026-04-18: `Loaded libraries (11)` + `Available libraries (45)`, each with
a rich one-line description. But the tool's MCP description claims it lists
*"libraries currently loaded in the session"* — a lie of omission that gives
the LLM no reason to call it during discovery. Three artifacts misrepresent or
under-sell the runtime in similar ways.

## Goals

- Make LLMs reach for Wile MCP tools on the right problems (modular arithmetic,
  exact rationals, polynomials, lattices, algebraic structures, symbolic ops).
- Correct the `libraries` description so the LLM treats it as the discovery
  entry point.
- Apply sibling-tool disambiguation per MCP best practice: each description
  says when to use the *other* tool.
- Replace descriptive language ("eval evaluates Scheme") with triggering
  language ("eval — when you would otherwise compute by hand, do this").
- Validate via the existing `algebra-accuracy` benchmark — re-run treatment
  with the new strings and confirm the `powerset_lattice` regression closes.

## Non-Goals

- **Lever B** — new tools (`library-doc`), educational error messages, cross-library
  apropos. Tracked separately; this design is text-only.
- **Lever C** — extending the benchmark to new domains (linear algebra, symbolic
  differentiation, number theory). Tracked separately.
- **wile-goast** — same triggering treatment to be applied after wile validates.
- **Architectural change** — the loaded-vs-available library split stays
  (maintainer decision; reasons not enumerated here). The split's user-visible
  cost is a one-round discovery overhead per session, which is acceptable in
  production (no round cap) and only painful in benchmarks (10-round cap turns
  tokens-wasted into rounds-exhausted).

## Design

Three artifacts revised in `cmd/wile/`:

| Artifact | File | Lines (approx) |
|---|---|---|
| Server instructions | `mcp.go:69–89` (`WithInstructions(...)` arg) | ~20 |
| Per-tool descriptions | `mcp.go:92–216` (9 `mcp.NewTool(...)` calls) | ~120 |
| Wile-scheme prompt | `prompts/wile-scheme.md` | ~150 |

All three use a single, coordinated triggering vocabulary so the LLM gets the
same message at every entry point: server-instructions level, per-tool level,
and prompt-explicit level.

### Artifact 1 — `WithInstructions`

Replace the current "When to use / When NOT to use / Session model" structure
with: lead-with-triggering, then discovery workflow, then tool roster, then
session model, then "when NOT to use".

#### Proposed text

```
# The Wile Scheme MCP server

Wile is a Scheme runtime with extensive libraries for math, algebra, and
symbolic computation. Reach for these tools whenever the task involves
arithmetic or structure where LLM-native reasoning is unreliable:

- Modular arithmetic, exact rationals, big-integer ops, polynomial arithmetic
- Algebraic structures: lattices, groups, rings, semirings, monoids, fields
- Symbolic manipulation, term rewriting, differential algebra
- Combinatorics, number theory, exact-answer problems

The `eval` tool is more reliable than your own arithmetic for these domains.

## Discovery workflow

1. Call `libraries` to see the full catalog — both loaded and available libraries
   with one-line descriptions. This is the entry point for unfamiliar tasks.
2. Pick a library from the descriptions.
3. Use `eval` with `(import (library name))` to load it.
4. Use `doc` and `apropos` to introspect the library's bindings.
5. Use `eval` to compute the answer.

## Tools

- `eval` — Evaluate Scheme. Reach for this *instead of computing by hand* on
  the domains above.
- `libraries` — Catalog of all libraries (loaded + available) with descriptions.
  **Call this first** on any unfamiliar task.
- `doc <name>` — Documentation for a binding or loaded library. For unloaded
  libraries, see `libraries` for descriptions, then import first.
- `apropos <pattern>` — Substring search across loaded bindings only. For
  cross-library discovery, use `libraries`.
- `topics` / `topic` — Browse loaded bindings by category.
- `disassemble` — Bytecode listing of a defined procedure.
- `reset` — Discard session state.
- `set-timeout` — Override the default eval timeout in seconds.

## Session model

The `eval` tool runs in a persistent session. Definitions, imports, and state
carry across calls. Multiple top-level definitions in a single eval can
reference each other. Use `reset` for a clean slate.

## When NOT to use

- Go static analysis (AST queries, call graphs, SSA, belief checks) → wile-goast
- Go code navigation (find symbol, references, diagnostics) → gopls
```

### Artifact 2 — Per-tool descriptions

Each description: one triggering sentence, one functional sentence, one sibling
disambiguation sentence (where applicable).

#### `eval` (current ~10 lines → triggering version)

```
Evaluate Scheme expression(s) in a persistent session. Reach for this when you
would otherwise compute by hand on a problem involving exact arithmetic, modular
operations, polynomials, lattices, algebraic structures, or symbolic
manipulation — eval is more reliable than LLM-native reasoning for these
domains. Definitions, imports, and state carry across calls; multiple
top-level definitions in a single call can reference each other. Returns JSON
{"output":"...", "value":"..."} where output is captured stdout (display/write)
and value is the last expression's result. Pass `timeout` to override the
session default; pass 0 to disable.
```

#### `libraries` (CRITICAL fix — current description is misleading)

```
List the full catalog of Scheme libraries — both loaded and available-but-not-
loaded — with a one-line description for each. Output has two sections: "Loaded
libraries" (already imported, ready to call) and "Available libraries" (use eval
with `(import (library name))` to load). **Call this first** on any unfamiliar
task: the descriptions tell you which library to import for math, algebra,
lists, lattices, or other specialized domains.
```

#### `doc`

```
Show documentation for a Scheme binding or loaded library. For bindings, pass
the name (e.g. "car", "map"). For libraries, pass the parenthesized name (e.g.
"(scheme base)", "(wile algebra)"). Returns signature, parameter types,
description, and category. For libraries that haven't been imported yet, doc
reports "not loaded" — use the `libraries` tool to see the catalog of unloaded
libraries with descriptions, then `(import ...)` before calling `doc` again.
```

#### `apropos`

```
Substring search (case-insensitive) over the names and one-line descriptions of
*currently loaded* bindings. Returns matching names with category and short
description. Cannot find symbols in unloaded libraries — for cross-library
discovery, call `libraries` first to find the right library to import.
```

#### `topics`

```
List documentation categories of currently loaded bindings with entry counts.
Use with the `topic` tool to browse a category. Only categories from loaded
libraries appear — use `libraries` to see what else is available to import.
```

#### `topic`

```
List all loaded bindings in a documentation category, each with a one-line
description. Use `topics` first to see available category names. Only loaded
libraries contribute — use `libraries` to discover unloaded libraries by topic.
```

#### `disassemble`, `reset`, `set-timeout` — no functional change

These are operational rather than discovery-driven; current descriptions are
adequate. Minor copy-edits only:

- `disassemble`: keep as-is.
- `reset`: keep as-is.
- `set-timeout`: keep as-is.

### Artifact 3 — `cmd/wile/prompts/wile-scheme.md`

Insert a new top section, "When to reach for this server," before "Session
Model." Keep all existing content unchanged below it. Cross-reference the
high-value domain list (Section "Domains" below).

#### New top section (proposed)

```markdown
## When to Reach for This Server

LLMs are unreliable at exact arithmetic, modular operations, polynomial work,
and structural algebra. The Wile eval tool is reliable on these domains. If
your task touches any of the categories below, **stop reasoning by hand and
use eval**.

| Domain | Trigger | Worked example |
|---|---|---|
| Modular arithmetic | `mod`, `expt` over integers, gcd/lcm of large numbers, modular inverse | `(modulo (expt 23 1000) 1009)` |
| Exact Arithmetic | <one-line trigger> | <eval call> |
| Polynomials | <one-line trigger> | <eval call> |
| Structural Algebra | <one-line trigger> | <eval call> |
| <DOMAIN 5> | <one-line trigger> | <eval call> |

## Discovery Workflow

When the task domain is unfamiliar:

1. Call `libraries` first — see the full catalog (loaded + available) with
   descriptions.
2. Pick a relevant library from the descriptions.
3. Run `(import (library name))` via `eval` to load it.
4. Use `doc <name>` or `apropos <pattern>` to introspect what loaded.
5. Compute via `eval`.
```

(Existing "Session Model," "Result Format," "Importing Libraries," "Common
Patterns," "Instructions" sections retained verbatim below this insertion.)

## High-Value Domain List

Worked example below; **3–5 additional rows owned by user.** The format is
deliberately tight (trigger + example) so the LLM can scan it in-context.

### Worked example: Modular arithmetic

| Field | Value |
|---|---|
| **Domain** | Modular arithmetic |
| **LLM failure mode** | Computing large modular exponents (e.g. `23^1000 mod 1009`) by hand; LLMs accumulate sign errors on negative bases and silently truncate big-int intermediate results. |
| **Trigger** | When the problem involves `mod`, `expt` over integers, primality, gcd/lcm of large numbers, or modular inverse — call `eval`. |
| **Example call** | `(modulo (expt 23 1000) 1009)` → exact integer. |
| **Required libraries** | `(scheme base)` (always loaded). For modular ring algebra: `(import (wile algebra ring))`. |

### TODOs (user-owned)

Pick 3–5 additional domains from the candidates below and write trigger rows
for them in the same format as the worked example. The chosen rows go into
*both* artifacts:

- The prompt table (full row format with worked example).
- The server-instructions bullet list (terser — domain name + one-clause hook).

Both must agree on the same 4–6 domains. Candidate domains, drawn from the
`algebra-accuracy` benchmark categories and the libraries listed by `,libraries`:

- [ ] **Exact rationals / fractions** (compare `algebra-accuracy/rational_field` results)
- [ ] **Polynomial arithmetic** (if `(wile algebra ring)` covers this)
- [ ] **Lattice / order operations** — directly addresses the `powerset_lattice` regression
- [ ] **Big-integer arithmetic** (factorials, binomials, large primes)
- [ ] **Combinatorics** (partitions, permutations — check what `(srfi 1)` and `(wile algebra)` expose)
- [ ] **Symbolic differentiation / term rewriting** (`(wile algebra symbolic)`, `(wile algebra rewrite)`)
- [ ] **Number theory** (primes, factorization)
- [ ] **Boolean / Heyting algebra** (`(wile algebra boolean)`, `(wile algebra heyting)`)

The domain list lives in two places (server instructions + the prompt). Format
the prompt version as the table above; format the server-instructions version
as a tighter bullet list. The two should agree on the same 4–6 domains.

## Validation

The success criterion is empirical: re-run `algebra-accuracy/evaluate.py` in
treatment mode with the new MCP strings and confirm:

1. The `powerset_lattice` regression at hard / extra-hard closes (target:
   matches or exceeds control's 90% / 100%).
2. No regression on the categories that already win (modular_arithmetic,
   monoid_power, rational_field).
3. Treatment cost-per-correct does not materially increase. The expectation is
   it *decreases* slightly because the LLM no longer wastes rounds on failed
   discovery.

Secondary check: a fresh transcript-trace audit on 5 powerset_lattice problems
should show the LLM calling `libraries` on round 1 (the discovery hint
working), not jumping straight to `doc("(wile algebra)")`.

## Out of Scope / Future Work

- **Lever B** — `library-doc` tool that parses unloaded libraries on demand;
  enriched error messages that suggest imports on unbound-variable errors.
  Could close the discovery cost further but requires touching parser code.
- **Lever C** — Benchmark extension to symbolic / linear algebra / number theory
  domains. Should follow the same `algebra-accuracy` template.
- **wile-goast triggering pass** — Same pattern (instructions + tool descriptions
  + prompts), but the triggering language is different ("before grepping, ask
  the AST"). Apply after wile pass validates.
- **`readOnlyHint: true` annotations** on read-only tools (`doc`, `apropos`,
  `topics`, `topic`, `libraries`, `disassemble`) — would let MCP hosts
  auto-approve them and remove a UX friction point. Worth doing but bundling
  it in this PR would contaminate the benchmark measurement; ship separately.

## Implementation Notes

- All changes are content-only. No new functions, no schema changes, no
  behavioral changes.
- The `mcp.go` `WithInstructions` argument is a single string literal; replace
  it. Each tool's `mcp.WithDescription` is also a string literal; replace each.
- `prompts/wile-scheme.md` is embedded via `embeddedPrompts` (`go:embed`); a
  rebuild is required for the prompt change to take effect.
- Existing `cmd/wile/mcp_test.go` tests behavior, not description content. No
  test updates required for this PR.
- After implementation, this design doc should be updated with the final
  domain list (filled in by user).
