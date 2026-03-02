# Staff Engineer Tech Debt Assessment — Wile Codebase

Date: 2026-02-28
Scope: Full codebase (664 Go files, ~177K LOC)
Method: Four parallel agents with orthogonal mandates + manual verification
Prior reviews: Structural (complete, remediated), Signals (complete, remediated), [Tech Debt](TECH_DEBT_REVIEW.md) (verified same day)

## Relationship to TECH_DEBT_REVIEW.md

This review ran independently and rediscovered most findings from TECH_DEBT_REVIEW.md (F1–F14).
Findings below are **net-new** — items not covered by the existing tech debt review.
Cross-references to overlapping findings are noted where relevant.

---

## New Findings

### N1 [Priority: High] [COMPLETE] — Global binding access triplicated in VM loop

**Where**: `machine/machine_context.go:840-856` (OpLoadGlobal), `:861-878` (OpStoreGlobal), `:911-927` (OpPushGlobal)
**What**: Three opcodes repeat the identical sequence: extract literal → validate → type-assert to `*GlobalIndex` → conditional dispatch on `gi.Env != nil` → binding lookup → nil check → error. The only difference is the final action (SetValue vs Store vs evals.Push). Same duplication exists for locals (OpLoadLocal:888 vs OpPushLocal:932).
**Why it matters**: Any change to global binding resolution logic (caching, metrics, new binding kinds) must be applied 3 times. The Load+Push fusion pattern (driven by peephole.go) mechanically doubles every load opcode.
**Suggested fix**: Extract a `resolveGlobal(mc, instr) (*Binding, error)` helper; each case calls it then applies the one-line action.
**Effort**: S
**Relation**: Partially overlaps TECH_DEBT_REVIEW.md F2 (opcode friction), but F2 focuses on file-count tax for *new* opcodes. This finding is about duplication within *existing* opcodes.

---

### N2 [Priority: Medium] [COMPLETE] — VM errors bypass sentinel+wrap pattern

**Where**: `machine/machine_context.go` — 10 instances of `mc.Error(fmt.Sprintf(...))`
**Evidence** (verified):
```
843: return mc.Error(fmt.Sprintf("literal index %v does not exist", instr.Arg))
847: return mc.Error(fmt.Sprintf("literal %v is not a global index", o))
856: return mc.Error(fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
864: return mc.Error(fmt.Sprintf("literal index %v does not exist", instr.Arg))
868: return mc.Error(fmt.Sprintf("literal %v is not a global index", o))
892: return mc.Error(fmt.Sprintf("no such local binding %d:%d", slot, depth))
914: return mc.Error(fmt.Sprintf("literal index %v does not exist", instr.Arg))
918: return mc.Error(fmt.Sprintf("literal %v is not a global index", o))
927: return mc.Error(fmt.Sprintf("no such global binding for %s", gi.SchemeString()))
936: return mc.Error(fmt.Sprintf("no such local binding %d:%d", slot, depth))
```
**What**: These produce bare errors without sentinels. Callers can't match with `errors.Is`. The rest of the codebase follows sentinel+wrap per the documented invariant in CLAUDE.local.md.
**Why it matters**: Embedders catching VM errors can only inspect message strings. If the engine ever needs to distinguish "binding not found" from "invalid literal index" programmatically, there's no sentinel to match against.
**Suggested fix**: Define sentinels (`ErrBindingNotFound`, `ErrInvalidLiteral`, etc.) and wrap at each site.
**Effort**: S

---

### N3 [Priority: Medium] — BigComplex part operations: 4 × 3×3 nested type switches

**Where**: `values/big_complex.go:138-260` — `addParts()`, `subtractParts()`, `multiplyParts()`, `divideParts()`
**What**: Four package-level functions with identical structure: outer switch on `a`'s type (BigInteger/Rational/BigFloat), inner switch on `b`'s type (same 3). The four functions differ only in which arithmetic method is called.
**Why it matters**: Adding a new BigComplex-compatible part type requires updating all 4 functions × both switches = 8 edit sites. The functions can't trivially delegate to `a.Add(b)` because the dispatch tables handle cross-type promotion to Float/Complex, not to the BigComplex-compatible subset. Two promotion models coexist.
**Suggested fix**: Extract a generic `applyPartOp(a, b Number, op func(Number, Number) Number) Number` that handles the promotion and delegates.
**Effort**: M
**Relation**: Complements TECH_DEBT_REVIEW.md F1 (dispatch tables). These functions exist *because* the dispatch tables don't serve BigComplex's needs — unifying dispatch (F1) would eliminate these.

---

### N4 [Priority: Medium] [COMPLETE] — PrimExpt: ~230-line function with nested type dispatch

**Where**: `extensions/math/prim_math.go:229+`
**What**: `PrimExpt` handles integer/rational/float/complex exponentiation with nested type switches on both base and exponent. Manual type checking instead of `helpers.RequireArg[T]()`. Also `PrimMakeRectangular` (~100 lines) with similar patterns and repeated error messages (~8 instances of identical WrapForeignErrorf with only field name changes).
**Why it matters**: Each new numeric type requires adding cases to both switches. The function does too many things (exact integer powers, rational exponents, float fallback, complex domain).
**Suggested fix**: Split into `exptExact(base, exp)` and `exptInexact(base, exp)`, each handling a subset of types.
**Effort**: M

---

### N5 [Priority: Medium] [COMPLETE] — Literal deduplication is O(n²)

**Where**: `machine/native_template.go` — `AppendLiteralUnique()`
**What**: Each literal addition does a linear scan of the existing pool via `lit.EqualTo(v)`. For a template with N unique literals, total work is O(N²).
**Why it matters**: Only visible in large programs (1000+ literals per template), but Scheme macros can generate large template bodies. This is a scaling wall that won't announce itself — compilation just gets gradually slower.
**Suggested fix**: Add a hash-based index alongside the literal pool for O(1) dedup lookups on `Hashable` values, falling back to linear scan for non-hashable ones.
**Effort**: S

---

### N6 [Priority: Medium] [COMPLETE] — Compiler/expander parallel structure divergence risk

**Where**: `machine/compile_time_continuation.go` (454 lines + 2053 in include/library/quasiquote supplements), `machine/expander_time_continuation.go` (1526 lines)
**What**: Both implement scope-aware binding resolution with nearly-parallel entry points (`CompileExpression`/`ExpandExpression`, `CompileSymbol`/`ExpandSymbol`). They share the Flatt model but for different purposes (codegen vs. macro expansion). CLAUDE.local.md acknowledges this.
**Why it matters**: Bug fixes in scope resolution logic must be verified in both files. Subtle semantic differences (compiler checks empty scope sets for perf; expander checks binding scopes for hygiene) make it easy to apply a fix to one and miss the other.
**Suggested fix**: Not consolidation (the difference is real). Add a shared test suite that exercises scope resolution through both paths with identical inputs, catching divergence.
**Effort**: M
**Relation**: Partially overlaps TECH_DEBT_REVIEW.md F8 (two compilation dispatch paths), but F8 focuses on the validated vs. registry compilation split. This focuses on the compiler vs. expander parallel structure.

---

### N7 [Priority: Low] [COMPLETE] — security/ package uses errors.New/fmt.Errorf

**Where**: `security/authorizer.go:22` (`errors.New`), `security/filesystem_root.go:54,59` (`fmt.Errorf`)
**What**: The security package defines `ErrAccessDenied` with `errors.New` instead of `values.NewStaticError`, and wraps it with `fmt.Errorf` instead of `values.WrapForeignErrorf`.
**Why it matters**: Minor consistency issue. May be intentional to avoid importing `values/` and keep security low in the dependency graph.
**Suggested fix**: Either document the deliberate layering choice, or if security already imports values, switch to `NewStaticError`/`WrapForeignErrorf`.
**Effort**: S
**Relation**: TECH_DEBT_REVIEW.md F7 covers security/ test coverage but not its error pattern.

---

### N8 [Priority: Low] [COMPLETE] — Peephole optimizer opcode knowledge is fragile

**Where**: `machine/peephole.go:56-63` — `writesValueRegister()`
**What**: The optimizer manually enumerates load opcodes that write the value register. Adding a new load opcode and forgetting to update this list causes silent missed optimizations (dead LoadVoid not eliminated).
**Why it matters**: Low probability of bug, but high cost if it occurs — missed optimization shows up only in benchmark regressions, not test failures.
**Suggested fix**: Generate `writesValueRegister` from opcode metadata rather than hand-maintaining the list.
**Effort**: S
**Relation**: Subsumed by TECH_DEBT_REVIEW.md F2 (opcode friction), but highlights a specific failure mode F2 doesn't call out.

---

### N9 [Priority: Low] [COMPLETE] — Magic number boundaries undocumented

**Where**: `values/string.go:34` (`stringInternMaxLen = 64`), `values/integer.go:33-36` (`intCacheMin/Max = -32768/32767`)
**What**: Performance-tuning constants lack documented rationale. 64 bytes for interning, int16 range for integer cache.
**Why it matters**: Someone tuning performance can't tell whether these are empirically validated or arbitrary.
**Suggested fix**: Add a one-line comment with rationale for each constant.
**Effort**: S

---

## Items Confirmed from TECH_DEBT_REVIEW.md

These findings were independently rediscovered and verified:

| TECH_DEBT Finding | Confirmed | Notes |
|---|---|---|
| F1: 41 dispatch tables | Yes | 41 arrays verified across 7 files |
| F2: Opcode 7-file tax | Yes | 12 Load/Store/Push cases confirmed in machine_context.go |
| F3: Complex/BigComplex HashCode | Not re-checked | Deferred to existing finding |
| F5: Makefile duplicate test-scheme | Not re-checked | Deferred to existing finding |
| F9: BigComplex missing LessThan | Yes | 5 vs 6 arrays confirmed |
| F10: MachineContext size | Yes | Confirmed via opcode case count |
| F14: ByteVectorBufferdOutputPort typo | Not re-checked | Deferred to existing finding |

---

## Areas Found Clean

| Area | Status | Notes |
|---|---|---|
| Package layering | Clean | No circular imports, clean DAG |
| Extension registration | Excellent | All 7 extensions follow identical pattern |
| Error handling (non-VM) | Excellent | Ruleguard enforcement, near-universal compliance |
| Linter directives | Well-controlled | 155 suppressions, 94% justified |
| Build/CI | Robust | 461-line Makefile, clean CI pipeline |
| Documentation | Current | TODO.md, CLAUDE.md, plan files all maintained |
| internal/ decoupling | Good | Minimal cross-reaches, appropriate aggregation points |

---

## Closing Summary

The codebase is architecturally sound. The prior structural and signals reviews remediated the most critical issues. What this review adds: (1) the VM dispatch loop has its own duplication problem (N1) independent of the numeric tower's (F1) — same pattern, different domain; (2) the VM violates its own error handling convention in 10 places (N2); (3) BigComplex's part operations are a consequence of the dispatch table design and would be eliminated by F1 remediation (N3). The remaining items are medium-priority consistency and evolution-risk issues.

## Top 3 New Items to Tackle

| # | Item | Rationale |
|---|---|---|
| 1 | **N1: VM global binding resolution helper** | Smallest effort, biggest safety improvement — eliminates 3x duplication in the hottest code path. One function, one test, done in an hour. |
| 2 | **N2: VM error sentinels** | 10 bare errors violate the project's own documented invariant. Quick fix (define sentinels + wrap), unblocks programmatic error handling for embedders. |
| 3 | **N5: Literal dedup O(n²)** | Small fix with real scaling impact. Hash index for Hashable literals, linear fallback for the rest. |
