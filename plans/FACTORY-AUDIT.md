# Primitive Factory Audit

**Date:** 2026-06-01
**Status:** **Phases 1-2 SHIPPED** (`cleanup/code-volume-phase5`, 2026-06-03). Phase 1 (|C|≥4):
`makeCharSetFold` (char-set union/intersection/difference/xor → `extensions/charsets`) and
`makeWriteVariant` (write/display/write-simple/write-shared → `internal/extensions/io`). Phase 2
(borderline |C|=3, factored for consistency per maintainer decision): `makeBinaryPredicate`
(eq?/eqv?/equal? → `registry/core`) and `makeSyntaxLocAccessor` (syntax-line/column/position →
`registry/core`); the two registry/core families keep exported `var Prim*` for API stability
(func→var per open-question 1). `make lint` 0 issues + `make covercheck` 41/41 ≥80% + axis-b
manifest verified (464 entries, set unchanged — fn=/loc= shifts only). **Census finding:** the
obvious |C|≥4 families were already factored (math `makeRealNumberPrimitive`/`makeComplexPrimitive`
×15, char/type/sequence `Make*` factories); the four seed families above were the genuine
holdouts. **Phase 0 census + Phase 3 (ArgShape) SHIPPED** (`cleanup/argshape-unification`, 2026-06-03). The
full wile-goast AST-diff census ran over all 490 registered primitives (399 fingerprinted, 60 files;
77 already factory-closures). It found the codebase already factors every same-package family
(division→`realDivision`, assoc/member→`MemberLookup`/`AssocLookup`, file-open→`openFilePort`,
predicates/transforms/sequence→existing `Make*`). The one surviving systemic family — the cross-type
"extract typed arg → project/mutate → set" accessor pattern — exceeded the 7-variant trigger
(clusters of 13/10/10/6/4), so Phase 3 fired: three generics in `registry/helpers`
(`MakeUnaryAccessor`, `MakeUnarySideEffect`, `MakeBinarySetter`) collapsed **42 sites** across
`registry/core`, `extensions/{threads,gointerop,process,charsets}`. Exported `Prim*` kept as `var`
for embedder API stability. **`set-car!`/`set-cdr!` deliberately NOT converted** — measured ~2%
regression on a set-car!-dominated microbench (factory adds one indirect call; reachable from
Larceny destruc/maze), per the hot-path exclusion. `make lint` 0 + `make covercheck` 41/41 +
axis-b manifest 464 (set unchanged). Cluster 7 (constants: eof-object, char-ready?, ...) left as
distinct constants. The special-form registration tables (`internal/validate`,
`machine/compilation`) were out of census scope (open-question 3 — primitive registry only).
Originally deferred behind `CODE-VOLUME-REDUCTION.md` Phases 2-4 (which reshaped
`registry/helpers`, the factory sink).
**Type:** Systematic sweep — not a fixed finding list. Discovers the full set of
"N near-identical primitives → one factory" families across ~448 registration entries.
**Lineage (this is a continuation, not a novel idea):**
- `2026-05-08-dispatch-axis-as-data.md` — names the exact pattern this audit hunts:
  *"a multi-axis dispatch problem encoded by replicating structure along one axis instead
  of treating that axis as data."* Read it first; this plan is its registry-layer instance.
- `2026-05-29-car-cdr-consolidation.md` — COMPLETE exemplar (~600 LOC consolidated via a
  closed helper API). Reuse its phase shape, sentinel-preservation rule, and TDD cadence.
- `2026-05-18-registry-structural-reduction.md` — deferred **Phase 5 (ArgShape)** and
  **Phase 6 (unification)** are gated on a "7th-variant trigger." This audit is that trigger:
  it produces the variant census that decides whether those phases fire.
- `CODE-VOLUME-REDUCTION.md` — the workflow audit that seeded the known candidates below.

## The pattern (recap from dispatch-axis-as-data)

A parametric family `{F(c) : c ∈ C}` of primitives indexed by some axis `C` can be encoded
as **data** (`dispatch[c]`, or a factory closure over `c`) or as **code** (one named
`Prim<C₁>`, `Prim<C₂>`, … per inhabitant). The code form is wrong when `C` has **≥4
inhabitants**, when adding to `C` needs **multi-site edits**, or when **consistency across
`c` is invariant** (all variants must validate args / wrap errors / set values the same way).
Wile already chose the data form in several places; this audit finds where it didn't.

## Existing factory vocabulary (verified precedents — follow these, don't invent)

| Factory | File | Shape |
|---------|------|-------|
| `MakeTypePredicate(check)` | `registry/helpers/type.go:25` | `Value→bool` predicate |
| `MakeNumericPredicate[T](…)` | `registry/helpers/type.go:40` | generic numeric predicate |
| `MakeCharPredicate(name, test)` | `registry/helpers/type.go:57` | `rune→bool` |
| `MakeCharTransform(name, transform)` | `registry/helpers/type.go:70` | `rune→rune` |
| `SequenceLength/Ref/Set[T]` | `registry/helpers/sequence.go:32,49,70` | generic seq accessors (vector/bytevector share) |
| `makeRealNumberPrimitive(op)` | `extensions/math/prim_rounding.go:35` | real-number unary op |
| `makeComplexPrimitive(name, fn)` | `extensions/math/prim_transcendental.go:32` | `complex128→complex128` |

**Placement rule (per package layering):** a factory lives in the lowest package that owns
its operand type. Type/char/sequence factories → `registry/helpers`. Operand-specific
factories (e.g. `*values.CharSet` fold) → that operand's extension package. Never push an
operand-type dependency *up* into `registry/helpers`.

## Method

1. **Census.** Enumerate all ~448 `PrimitiveSpec` registration entries (`registry/core/*.go`,
   `extensions/*/*.go`, `internal/extensions/*/*.go`). For each, capture name, param count,
   variadic flag, return type, and the implementing `Prim*` function.
2. **Cluster by structure, not name.** Use wile-goast AST-diff (`(wile goast)`) on the
   `Prim*` bodies to find families that differ only along one axis (the operand op, the
   projected field, the comparator). Do NOT eyeball — confirm structurally. This is the same
   tool the audit used.
3. **Classify each family** against the decision rule below. Record axis size (`|C|`),
   whether adding an inhabitant needs multi-site edits, and whether cross-variant consistency
   is invariant.
4. **For factor-worthy families:** name the factory, pick its package (placement rule),
   write it test-first (TDD per car-cdr-consolidation), then convert call sites one commit at
   a time.
5. **No silent caps.** If the census is truncated (time/scope), `log` what packages were not
   swept — a partial audit must not read as complete.

## Decision rule

**Factor when** `|C| ≥ 4` **OR** adding an inhabitant requires edits at 2+ sites **OR**
all variants must stay consistent (same arg validation / error sentinel / value-set shape).
**Leave alone when** `|C| ≤ 3` and the variants are genuinely distinct, **or** the family is
a hot-path dispatch (see Hard exclusions). A 3-member family that's a closed set and unlikely
to grow (e.g. before/after/around) is a judgment call — prefer leaving it.

## Seed candidates (confirmed by the workflow audit — start here)

These are the families the audit already confirmed; the census will surface more.

| Family | Sites | `\|C\|` | Proposed factory | Package |
|--------|-------|------:|------------------|---------|
| char-set union/intersect/diff/xor | `extensions/charsets/charsets.go:313,326,339,352` | 4 | `makeCharSetFold(name, op)` | `extensions/charsets` (operand `*CharSet`) |
| write/display/write-simple/write-shared | `internal/extensions/io/prim_write.go:48,85,118,138` | 4 | one factory keyed on cycle/label mode | `internal/extensions/io` |
| `eq?`/`eqv?`/`equal?` | `registry/core/prim_equality.go:26,41,50` | 3 | factory over the comparator fn | `registry/core` (borderline — `\|C\|`=3) |
| char/string comparators | `registry/core/prim_characters.go:69`; `prim_strings.go:302` | — | already have `makeChar/StringComparePrimitive` — they're single-use forwarders (inline or generalize) | `registry/core` |
| `syntax-line`/`-column`/`-position` | `registry/core/prim_syntax_loc.go:66,78,90` | 3 | factory over the source-context int projection | `registry/core` (borderline) |

The `eq?`/`eqv?`/`equal?` and `syntax-*` families are `|C|`=3 — apply the decision rule
deliberately; they qualify only because cross-variant consistency (identical arg-validation
prologue) is invariant. Document the call either way.

## Hard exclusions (do NOT factor — correctness/perf invariants)

- **VM dispatch loop** (`machine/machine_context.go` `Run()` switch) and **numeric-tower
  per-kind switches.** Memory records a **1.5% regression** from collapsing switch→table
  (promoted ops) and multiple reverted table-dispatch attempts. A Go `switch` on a hot axis
  is the *correct* data encoding here; do not "factory" it. (`2026-05-08-dispatch-axis-as-data.md`
  explicitly carves out "hand-unrolled dispatch intentionally duplicated for performance.")
- **Tokenizer inner-loop char dispatch.** Same reasoning.

## Risk controls (every conversion)

- **Multi-line factory bodies.** The no-single-line-function rule applies to the returned
  closures too — model on the existing `Make*` factories. This caps net LOC savings; that's
  expected.
- **Preserve sentinels, not message strings.** Thread the primitive `name` through the
  factory so per-site error context survives; tests assert via `errors.Is`.
- **Regenerate `plans/axis-b-manifest.scm`** after each conversion — the manifest records
  `fn=` and `loc=` per primitive, both of which change when a hand-written `PrimX` becomes a
  factory closure. `WILE_AXIS_B_UPDATE=1 go test -run TestBuildAxisBManifest .`. Verify the
  primitive *set* (count) is unchanged.
- **`func → var` public-API note.** Converting an exported `func PrimX` to a `var PrimX =
  factory(...)` changes what `&PrimX` means to embedders. Precedent exists in-package; accept
  but flag for the maintainer in the PR.
- **`make lint && make covercheck` green per phase**; `make bench-gabriel` if any converted
  family is reachable from a benchmark hot path.

## Phasing

- **Phase 0 — Census + classification.** Produce the variant table (method steps 1-3). No
  code changes. Output: this file's seed table, extended with every family found. Decide
  whether registry-SR Phases 5/6 should now fire.
- **Phase 1 — Unambiguous `|C|≥4` families.** char-set fold, write-family, and any census
  finds at `|C|≥4`. One branch per family.
- **Phase 2 — Borderline `|C|=3` families.** eq/eqv/equal, syntax-loc, comparators — only
  those where consistency-is-invariant justifies it. Document each decision.
- **Phase 3 — Fold into registry-SR Phase 5/6** if the census shows the deferred ArgShape /
  unification work is now warranted.

## Open questions (for the maintainer)

1. **`func→var` tolerance:** acceptable to convert exported `Prim*` to factory-produced
   `var`s wholesale, or keep thin named wrappers for API stability? (Affects whether the
   factory output can be registered directly.)
2. **Borderline `|C|=3` families:** factor for consistency, or leave for readability? Set a
   project-wide default so Phase 2 isn't re-litigated per family.
3. **Census scope:** registry/core + extensions only, or also the expand/compile-time
   special-form registration tables (`internal/validate`, `machine/compilation`)?
