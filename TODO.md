TODO
----

**Last Updated**: 2026-04-23

### Current Project Status

**Version**: v1.13.16 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower, core-compiled let forms
**Extensions**: 12 extension packages — 8 public (files, math, process, system, threads, gointerop, introspection, charsets), 4 internal (io, eval, namespace, all); all importable as R7RS `(wile <name>)` libraries. Extension API contracts (ValueType enum, PrimitiveSpec type declarations) in Phase 1. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI uses public Engine API; embedded stdlib via `stdlib.FS` (`go:embed` + `fs.Sub`); named profiles (`Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink`) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); `Engine.AvailableLibraries()` API for library discovery.
**Documentation**: Complete documentation system — `procedure-documentation`, `,doc`, `,apropos`, `,topics`, `,topic`, library-level `(description)` clause, docstring examples, examples filtering. All 397 primitive specs + ~243 stdlib procedures + 29 special forms + 15 macros + 11 syntax compilers + 34 library descriptions documented.
**MCP Server**: Built-in MCP server mode (`wile --mcp`) with eval, doc, apropos, topic, libraries, and reset tools.
**Examples**: 75 examples across 13 categories, 23 benchmarks (16 Gabriel + Larceny R7RS + Schelog + miniKanren)
**Tests**: Go test suite comprehensive; Scheme test suite: 3,852 lines across 19 files (11 scheme + 8 algebra) + R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1), (srfi 13), (srfi 14), (srfi 132), (wile strings), (wile charsets), (wile algebra) complete including `(wile algebra polynomial)` (ring-parameterized univariate polynomials); stdlib embedded in binary

### Ordering

Items ordered by perceived priority for the project's success as an embedding product. Tiers: Security/Correctness → Embedding API → Tooling/DX → Performance → Tech Debt → Deferred → Nice-to-Haves. Completed items at the bottom for reference.

---

## Tier 1 — Security & Correctness

Items that block production embedded use or prevent silent state corruption.

- [x] **vmState field coverage test** [High, S]: Reflection-based test enumerating vmState fields, asserting each appears in a coverage table keyed by operation. Prevents silent state corruption when fields are added. See [FCA Assessment](#fca-assessment) below.
- [x] **Error type identity** [Medium]: Determined: `CompilationError` and `RuntimeError` are **public boundary types** — they translate internal errors (`werr.ForeignError`, `machine.SchemeError`, `machine.ErrExceptionEscape`) to the embedder API. They should NOT implement `SchemeError` or `ForeignError`. Embedders use `errors.As` to match them. `RuntimeError` already has `Source`/`StackTrace`; `CompilationError` lacks source because the compiler doesn't propagate `SourceContext` into its errors — fix belongs in "error stack traces" below.
- [x] **Exceptions and error stack traces** [Medium, Done]: `SourcedError` type in `compilation/`, `CompileExpression` wraps errors with source context, `CompilationError.Source` field populated from cause chain. Phases 1-4 complete (PR #657 + precision fix in `processLibraryImport`). Datum-level functions (`import_set_datum.go`, `library_bindings.go`) operate on `values.Value` without syntax context — callers wrap. Foreign stack trace entries for Native → Foreign → Native callback crossings (P3) remain deferred.
- [x] **MCP eval fails on schelog `include`** [Not a bug]: Original report was missing `puzzle.scm` include and `(set! *schelog-use-occurs-check?* #t)`. Without occurs check, the puzzle infinite-loops and hits MCP timeout. With correct setup, MCP eval produces the correct answer.
- [x] **`read` mid-parse EOF should raise read-error, not return EOF** [Done]: Phase 4 exceptions audit G.1 — `(read "(foo")` returned `#!eof` instead of raising. Fixed by `wrapMidParseEOF` helper in `internal/parser/parser.go` that converts `io.EOF` to a `ParserError` wrapping `io.ErrUnexpectedEOF` at all mid-parse sites (readList, readLabeledList, readVector, readByteVector). `PrimRead`/`PrimReadSyntax`/`PrimReadToken` unchanged — the existing `errors.Is(err, io.EOF)` check correctly rejects the new `io.ErrUnexpectedEOF` and falls through to `WrapForeignReadErrorf`, producing a `ForeignReadError` that `goErrorToSchemeException` maps to `NativeErrorKindRead`, making `(read-error? e) → #t`. Test in `registry/core/prim_read_mid_parse_eof_test.go` covers 6 mid-parse cases + 2 clean-EOF regressions.
- [x] **Audit PrimitiveSpec `ReturnType` and `ParamTypes` annotations** [High, L] — **complete**: Phase 1 axis-A (docs ↔ ReturnType) clean. Phase 3 axis-B (ReturnType ↔ impl, 5 tightened in PR #675). Phase 4 axis-C R7RS sweep clean. Phase 5 ParamTypes audit: 5.A (manifest, PR #678), 5.B (analyzer + inventory, PR #679), 5.C (sidecar, `plans/2026-04-20-paramtypes-annotation-bugs.md`), 5.D (one partial narrowing: `get-output-bytevector` → `TypeBinaryOutputPort`), 5.E (`plans/2026-04-20-paramtypes-axis-c-findings.md` R7RS sweep) all complete. **Finding**: declared-too-narrow bucket empty (3 FPs confirmed); declared-too-wide dominated by TypeConstraint-vocabulary gaps (~85 entries, cross-referenced with axis-B Category C's 28 return-side gaps); ~25 sub-domain refinement-type candidates (R7RS "exact non-negative integer", "byte in [0,255]", etc.) below ValueType granularity. Four-axis framework closed. Next forward work: vocabulary-extension design (Extension API contracts Phase 2+), separately scoped. Original scoping rationale below:
  1. **Annotation vs implementation** (mechanical, tool-assistable via `wile-goast` belief or fuzz harness): for each primitive, verify the annotation is the narrowest *sound* type covering every return path; flag dead branches.
  2. **Implementation vs published standard** (R7RS-small first, then R6RS, SRFI, Racket, Chibi/Guile/Chicken where applicable): for each primitive whose name appears in a standard, verify the Wile implementation's domain (accepted params) and codomain (return shape) match the spec. Catches non-standard extensions masquerading as standard primitives (the open-output-bytevector one-arg branch).
  3. **Annotation vs standard**: cross-check ensures we don't document non-standard behavior as if it were standard.
  Wile-specific primitives (no entry in any adopted standard) need a **local spec** written before they can be audited — name, intended domain, intended codomain, error cases, any invariants. Without a spec there is nothing to drift *from*. Produce the spec inventory as a deliverable; treat missing specs as a debt sub-item. This audit becomes load-bearing the moment "Extension API contracts Phase 2+" (Tier 2) ships compile-time checking — unsound annotations then turn into wrongly-rejected programs, and the R7RS-compliance-as-baseline product claim starts to depend on evidence rather than assertion.

---

## Tier 2 — Embedding API & Product Value

The embedding experience that differentiates Wile.

- [ ] **Extension API contracts Phase 2+** [Embedding, High]: Compile-time (compiler consults `ParamTypes` for static call sites — error before execution, zero runtime cost) and runtime (`buildValidator` wires `ParamTypes` → `SetValidator`). Integration with linter. Prerequisite vocabulary-extension design at `plans/2026-04-21-type-constraint-extension-design.md` (Julia-subset nominal lattice, `OpaqueTypeConstraint`, `Subtype` as primary operation; excludes refinement and union types per invertibility/no-duplication principles). Original parent: `plans/2026-03-26-extension-contracts-design.md`
- [x] **Environment profiles** [Embedding, Done]: Named profiles (Tiny, Console, ConsoleWithLoad, Small, KitchenSink) via `WithProfile`; orthogonal `WithSandbox` modifier; virtual env map (`WithEnv`, `WithEnvMap`); Scheme-level `(environment '(wile <profile>))` support; `SafeExtensions`/`AllExtensions` removed. `plans/2026-03-26-environment-profiles-impl.md`
- [x] **Eager documentation index** [Tooling, Done]: Shipped as lazy-build-and-cache rather than eager scan. `LibraryExportIndex` is built on first `apropos`/`doc` query and cached on `Namespace`; Scheme-level `(apropos)`, REPL `,apropos`, and MCP share the same index, so LLMs can discover unloaded-library procedures from the first query. See PRs #623–625 (`memory/LIBRARY-EXPORT-INDEX.md`) and post-#623 asymmetry fix (`memory/PRIM-APROPOS-EXPORT-INDEX.md`). Original eager-scan design (`2026-04-08-eager-doc-index-design.md`) was superseded before any code shipped.
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [ ] **MCP triggering rewrite (Lever A)** [Embedding, Text-only]: Rewrite `cmd/wile/mcp.go` `WithInstructions`, 9 tool descriptions, and `prompts/wile-scheme.md` to trigger LLM tool use on algebra/modular/polynomial domains. Correct misleading `libraries` description (currently claims "loaded only" but tool returns full catalog). Validation via `algebra-accuracy` benchmark: closes `powerset_lattice` regression. No code logic changes. `plans/2026-04-18-mcp-triggering-rewrite.md`

### Algebra & Analytics Roadmap

Directions documents — identify prioritized capability extensions. Priority sequence per 2026-04-22 decision: **wile-goast-first** (Tier A — named consumers in wile-goast analysis code, giving wile-goast a complete algebraic palette without digressions into wile), **matching-second** (Tier B — Roth-Sotomayor two-sided matching), then **§5.7 lower-priority** (Tier C).

- [x] **Algebra library roadmap** [Directions]: `plans/2026-04-17-algebra-foundations-directions.md` identifies 6 prioritized directions extending `(wile algebra ...)`. §5.1 `(wile algebra matrix)` shipped via Path D (PRs #684–#691, #695, #696). §5.2 Möbius / incidence algebra — shipped (commit `4ff8a314`, `plans/2026-04-21-incidence-algebra-impl.md`). §5.3 AC-matching shipped via `(wile algebra unification)`, `plans/2026-04-21-ac-matching-impl.md` (Phase 6 closeout). §5.4 Group actions & Burnside shipped as extension of `(wile algebra group)` (`plans/2026-04-22-group-actions-burnside-impl.md`). §5.5 Distributive/modular lattices + Birkhoff shipped as extension of `(wile algebra lattice)` (`plans/2026-04-22-lattice-birkhoff-impl.md`). §5.6–§5.7 broken out as individual items below.
- [ ] **Benchmark statistics (gonum)** [Directions]: `plans/2026-04-18-gonum-integration-directions.md` §5.2 identifies a benchmark-statistics gap in wile. Ships `bench-stats/` ~100–150 LOC; pure Go, no CGo, one `go.mod` entry. Independent track from the companion wile-goast `goastgraph/` work (see wile-goast TODO). Distinct algebraic setting from `(wile algebra matrix)`: gonum is field-valued (ℝ/ℂ), not semiring-parameterized.

#### Tier A — wile-goast-first (named consumers in Appendix A)

- [x] **§5.4 Group actions & Burnside** [Algebra, wile-goast, High]: Shipped as extension of `(wile algebra group)` in place (D1 — not a new `(wile algebra group-action)` library). Extends `<group>` record with optional metadata (element?, setoid, order, elements, generators); adds `<group-action>` record, BFS-from-generators `orbit`/`stabilizer`/`fixed-points`, `orbit-representative` with documented tie-breaker, `burnside-count` with divisibility validation, presets (`trivial`/`cyclic`/`symmetric`/`product` groups; `trivial`/`permutation`/`regular`/`conjugation`/`product` actions). 124 tests; end-to-end verified via Burnside on conjugation-action of S_3 = 3 conjugacy classes. Available for wile-goast migration of register-renaming (`goastssa/prim_canonicalize.go`), binop commutativity (`ssa-normalize.scm` `ssa-rule-commutative`), and `boolean-simplify.scm`. Plan: `plans/2026-04-22-group-actions-burnside-impl.md`.
- [x] **§5.5 Distributive/modular lattice + Birkhoff** [Algebra, wile-goast, Matching, High]: Shipped as extension of `(wile algebra lattice)` in place (not a new library). Extends `<lattice>` record with three optional metadata fields (setoid, cardinality, elements); ships `distributive?` / `modular?` exhaustive axiom-check predicates + sample-based `validate-*[/setoid]` siblings; `join-irreducibles` / `meet-irreducibles` via lower/upper cover counting; `birkhoff-representation` / `birkhoff-reconstruction` roundtrip with smart O(|downsets(P)|) enumerator; `lattice->locally-finite-poset` projection; five presets (chain, boolean, diamond/M3, pentagon/N5, free-distributive). Also extends `<locally-finite-poset>` with optional `elements` field + `lf-poset-elements` accessor. Dedekind numbers verified through D(5) = 7581 (~1.5s). 155 tests. Available for wile-goast migration of `dataflow.scm` `run-analysis` MOP=MFP certification and `domains.scm` precision annotations. Plan: `plans/2026-04-22-lattice-birkhoff-impl.md`.
- [x] **§5.6 Combinatorial graph** [Algebra, wile-goast]: Shipped as new `(wile algebra combinatorial-graph)` — distinct from `graph.sld` (which remains as semiring-Bellman-Ford). 1-WL color refinement + individualization-refinement backtracking for complete graph isomorphism (Weisfeiler–Leman 1968; McKay–Piperno 2014), spanning-tree count via deletion-contraction with fast paths (Cayley, C_n, tree, empty), chromatic and Tutte polynomials via deletion-contraction (Read 1968, Tutte 1954) with |V|+|E|≤20 size cap, Hopcroft-Karp O(E·√V) bipartite matching, six preset fixtures (K_n, C_n, P_n, K_{m,n}, empty, Petersen). Setoid-carried vertex equality, tier-1/tier-2/tier-3 finiteness per §5.4 pattern. 225 tests including Petersen backtracking-correctness canary and C_6 vs 2K_3 cospectral non-iso canary. `plans/2026-04-22-combinatorial-graph-impl.md`.
- [x] **§2.2 Free Boolean algebra on atoms** [Algebra, wile-goast, Done]: Shipped via extraction from wile-goast's `boolean-simplify.scm` L23-69. Named entry points `symbolic-boolean-normalize` / `symbolic-boolean-equivalent?` in `(wile algebra symbolic)` — normalize under the axioms currently exposed by `boolean->theory` (commutativity, associativity, identity, idempotence, absorption of join/meet; complement-involution). Not applied: De Morgan, complement-law (x ∧ ¬x ⇒ ⊥), bound identities — extend `boolean->theory` if consumers need them. Also shipped Tier 2+3 of the same extraction plan: `(wile algebra abstract-domain)` with `sign-lattice` + `abstract-sign` + `sign-binop`; `(wile algebra dataflow)` with `<cfg-protocol>` record + `run-analysis` MFP worklist solver + `reverse-postorder` + `analysis-in/out/states`. `plans/2026-04-22-wile-goast-algebra-extraction-design.md` + `-impl.md`.

#### Tier B — Two-sided matching (Roth-Sotomayor)

- [x] **`(wile algebra matching)` library** [Algebra, Matching, Done]: Two-sided matching per Roth & Sotomayor (1990). Gale-Shapley deferred acceptance (proposer + receiver optimal), hospital/intern many-to-one via Roth's reduction, Conway distributive lattice on stable matchings via Birkhoff (load-tests §5.5), Irving rotations enumeration, egalitarian + sex-equal selectors. Many-to-many (Kelso-Crawford) deferred to follow-up gated on §5.7 matroids (`plans/2026-05-02-algebra-matching-many-to-many.md`). `plans/2026-05-02-algebra-matching-design.md`, `plans/2026-05-02-algebra-matching-impl.md`.
- [x] **§4.2 Tropical permanent / Hungarian primitive** [Algebra, Matching, Done]: `tropical-assignment` shipped in `(wile algebra matching)` — Kuhn-Munkres O(n³) Jonker-Volgenant 1987 form. Returns `(matching . cost)`. Forbidden pairs via `+inf.0`. Unequal sides via padding. Sanity-checked on a 4×4 textbook fixture against brute-force optimum.
- [ ] **§4.2 Maximum common subgraph** [Algebra, Matching]: True code clone detection — bipartite matching between candidate node pairs, branch-and-bound with assignment relaxation. Overlaps §5.6 combinatorial-graph. `plans/2026-04-17-algebra-foundations-directions.md` §4.2.

#### Tier C — §5.7 lower priority

- [ ] **§5.7 Matroids** [Algebra, Low]: `(wile algebra matroid)` — rank function, circuits, duality, Tutte polynomial, matroid intersection. ~300 LOC. Blocks Kelso-Crawford substitutes for many-to-many matching; also unlocks matroid-intersection framing of register allocation and scheduling. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Integer partitions & Young's lattice** [Algebra, Low]: `(wile algebra partition)` — `partitions-of`, conjugate partition, dominance order, Young's lattice as a poset. ~150 LOC. Natural addition given `order.sld`. `plans/2026-04-17-algebra-foundations-directions.md` §2.6 + §5.7.
- [ ] **§5.7 Category theory extensions** [Algebra, Low]: Functors, natural transformations, general adjunctions beyond `galois.sld`'s Galois-connection special case. Formalizes abstract-interpretation composition (Cousot & Cousot 1977). ~400 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Connes-Kreimer Hopf algebra on rooted trees** [Algebra, Low]: Coproduct cuts subtrees — matches `ast-transform`/`ast-splice` primitive operation in wile-goast's `utils.scm`. Formalizes rewrite-rule composition. ~300 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Submodular optimization** [Algebra, Low]: Greedy approximation framework. Applies to program slicing, test-suite selection, import minimization (submodular-maximization-under-cardinality). ~200 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.
- [ ] **§5.7 Symmetric functions / RSK** [Algebra, Research, Low]: Research-tier. Small consumer: LCS→LIS→RSK connection for statement/parameter-list diff in `unify.scm`. ~500 LOC. `plans/2026-04-17-algebra-foundations-directions.md` §5.7.

#### Follow-ups (deferred from shipped plans)

- [ ] **wile-goast AC-match migration** [Algebra, Follow-up]: Migrate `wile-goast/.../unify.scm:421` from `discover-equivalences` to `ac-unify`. Three risks: (1) term-protocol contract compliance, (2) trace-emitting diagnostic paths (`ac-unify` produces no rewrite trace), (3) small-arity benchmark before crossover claim. Scope ~100 LOC. `plans/2026-04-21-wile-goast-ac-match-migration.md`.
- [ ] **AC-matching v2 deferred decisions** [Algebra, Follow-up]: 8 decisions deferred in `plans/2026-04-21-ac-matching-design.md` "Open questions" — non-unit-multiplicity Stickel, sort-typed pattern-vars, E-matching scope. Re-open when a consumer surfaces.
- [ ] **Incidence algebra future extensions** [Algebra, Follow-up]: Items in `plans/2026-04-21-incidence-algebra-impl.md` "Future extensions (deferred)" section.

> Explicitly excluded as Part 7 non-goals in `plans/2026-04-17-algebra-foundations-directions.md` (no prospective consumer; documented here so the exclusion is visible rather than mistaken for oversight): tropical algebraic geometry, simplicial complexes / persistent homology, vector spaces as algebraic objects, holographic algorithms / Pfaffians, spectral graph matching, symmetric-function machinery beyond the LIS connection already tracked above.

---

## Tier 3 — Tooling & Developer Experience

- [ ] **Scheme linter** [Tooling, High, Needs Scoping]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. Potential checks: unused bindings, arity mismatches, type mismatches, unreachable code, style warnings. Research needed: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much at expand time vs separate pass? Interaction with type system is a key design question.
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [ ] **Scheme-side line coverage** [Tooling, M]: `executed []bool` on `NativeTemplate`, `WithCoverage` engine option, `--cover PATH` CLI flag, Go cover v1 output consumable by `go tool cover -html`. Design locked-in. Active branch: `feat/scheme-coverage`. `plans/2026-04-18-scheme-line-coverage.md`
- [ ] **Source file tracking in Syntax Objects** [Tooling]: Utilities for finding source locations and providing source lines.
- [ ] **`make doclint` target** [Tooling, S]: Extract `foo.go:N` citations from `docs/**/*.md` and `plans/**/*.md`; assert each file exists and `N` is within `wc -l file`. Cheap version catches the bulk of drift. Existing `check-readme-links.sh` only validates markdown link targets, not prose citations. Past multi-commit doc sweeps (PRs #707, #710, #711, #712, #713) are evidence the check would pay for itself. Stronger form would `go/ast`-parse the cited line and verify the enclosing decl name matches a nearby identifier in the doc.
- [ ] **POSIX API / SRFI-170 remaining phases** [Standard library, 9 phases]: Phases 2-10 not started. Phase 1 (directory ops + process extension) completed in PR #565.

---

## Tier 4 — Performance

- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`
- [ ] **B3 effective capture refinement** [Performance, Research]: Propagate B2 escape results back into B1 capture status. A binding marked `Captured` by B1 is effectively non-captured if every lambda that references it is stored in a non-escaping binding (B2). Cross-binding analysis over B1+B2 results.
- [ ] **Benchmark coverage gaps** [Performance, S-M]: No benchmarks for compiler, expander (syntax-rules expansion), library import resolution, or continuation capture/restore cycle. Existing benchmarks cover VM dispatch, fibonacci, tokenizer, parser, environment, and symbol interning.
- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`

---

## Tier 5 — Tech Debt

### FCA-Derived

- [ ] **Structural reduction roadmap** [Top priority, planning-only]: Selects the next packages to subject to `/structural-reduction` and **gates** the implementation plans below. Tier A targets in priority order: `values/` (Ca=33, 11K LOC, numeric tower + port hierarchy), `environment/` (Ca=16, binding-resolution algebra, recent namespace migration seams), `registry/` (Ca=19, contract surface for ~500 primitives). Tier B: root `wile/` (API design quality), `repl/`, `registry/helpers/`. Tier C uses different lenses (`scheme-conformance` for `registry/core`, `extensions/math`; `signals-engineer` for `security/`; batch `staff-engineer` sweep for `extensions/{eval,files,threads,gointerop,charsets,system,process,introspection}`). Why gating: `internal/` Phase 7 references `values/` as migration precedent; `machine/` Phase 7 boundaries depend on `environment/` frame shape. Run Tier A analyses BEFORE implementing the plans below. `plans/2026-05-07-structural-reduction-roadmap.md`. **Tier A complete (2026-05-09): A.1 (`values/`) and A.3 (`registry/`) cross-cutting findings consolidated in `plans/2026-05-08-dispatch-axis-as-data.md`; A.2 (`environment/`) findings recorded in `plans/2026-05-09-environment-structural-reduction.md`.**
- [ ] **vmCore sub-struct extraction** [High, M]: Extract always-transfer fields (env, template, pc, callDepth) into sub-struct. Converts 4 hand-copied assignments to 1 struct assignment at each of 6 copy sites. See [FCA Assessment](#fca-assessment).
- [ ] **Machine package structural reduction** [High, mixed S/M/L]: 7 findings + 3 opportunities from `/structural-reduction ./machine` (2026-05-06). Highest-impact items: (1) replace `syntaxCase any` field with sealed marker interface (S, eliminates universe-typed back-channel from `compilation/`); (2) push `maxStackSize` guard into `Stack.Push` (S, deletes 6 inlined guards in `Run()`); (3) replace `0=unlimited` sentinel on `maxCallDepth`/`maxStackSize` with `*uint64` and align `callDepth` int/uint64 mismatch (S); (4) remove the empty `Operation` interface contract (S); (5) consolidate `singleValue`/`multiValues` accessors to enforce mutex invariant (S, no perf change); (6) collapse 28 tail/non-tail opcode pairs to 14 via `instr.Arg` sign-bit encoding — gated on benchmark re-run against the 2026-04-05 baseline (M); (7) named sub-records for correlated `MachineContext` fields (timer/sub-context/expansion clusters) (L). Sequence smaller findings first; Finding 7 absorbs them. `plans/2026-05-06-machine-structural-reduction.md`
- [ ] **Internal package structural reduction** [Medium-High, mixed XS/S/M]: 7 findings + 4 opportunities from `/structural-reduction ./internal` (2026-05-07). Dependency graph is a clean DAG (0 cycles). Highest-impact items: (1) finish `*SyntaxPair`/`SyntaxEmptyList` migration — drop dual empty-list representation, deletes 7 defensive guards, type precision 60%→100% (M, touches `syntax/` which has 14 dependents); (2) extract `parseLetBindingPairs` helper to unify 3 hand-unrolled parse loops in let/let*/letrec validators (S, ~80 LOC removed); (3) extract `detectDuplicateSymbols` fold to unify 5 inlined `seen[bindingIdentity]` loops in validate/ (S); (4) extract `bindLocalSymbol` + `extendEnvWithSymbols` to unify 5 child-env construction sites (S); (5) generalize binding-reference walker — `WalkBindingRefs` higher-order traversal collapses `markCaptured` + `markEscaped` boilerplate, enables future analyses (S–M); (6) replace `match.NewMatcher{,WithEllipsisVars,Full,FullWithDepths}` telescoping constructors with option-functions (S, aligns with `wile.Engine` idiom); (7) delete dead `SyntaxObject.IsPair()` / `IsEmptyList()` (XS, unreachable by construction). Sequence Phase 7 (the syntax/ migration) last; Phases 1–6 independent. `plans/2026-05-07-internal-structural-reduction.md`
- [ ] **Environment package structural reduction** [Medium-High, mixed XS/S/M]: 10 findings + 4 opportunities from `/structural-reduction ./environment` (2026-05-09). Closes Tier A.2 of the roadmap. Internal SCC `{Namespace, EnvironmentFrame, GlobalEnvironmentFrame}` is irreducible (R7RS first-class envs require it) but one of its three edges is dead. Highest-impact items: (1) delete `GlobalEnvironmentFrame.namespace` — written but never read; aliases `EnvironmentFrame.namespace`; cuts one SCC edge (XS); (2) delete four dead `EqualTo`/`SchemeString`/`IsVoid` clusters on `Binding`, `Local-/Global-/EnvironmentFrame` (only `Namespace` has `var _ values.Value` check; `EnvironmentFrame.EqualTo` is also semantically wrong — recursively walks parent chain) (~100 LOC, S); (3) extract `Namespace.root()` to collapse the 7-site hand-unrolled `if p.parent != nil { return p.parent.X(...) }` delegation pattern; pair with explicit field-policy doc-table (snapshot vs delegate) — resolves two TODOs at `namespace.go:535-536` (S); (4) collapse 5 Namespace constructors to 2 + 1 specialty via option-functions, aligning with the `wile.Engine` / `match.Matcher` (Finding 6 of internal/) pattern (S–M); (5) extract `bestOf[T]` Flatt-resolution reducer used twice in `GetBinding` and `GetLocalIndex` (S); (6) collapse 10 `Binding` accessors to direct `Meta()` / `EnsureMeta()` (S); (7) delete dead `Binding.SetBindingType` (XS); (8) document or remove transient `BindingTypeUnknown` state (XS doc-only, M for full removal); (9) deprecate or document `EnvironmentFrame`'s 5-method delegation surface to `Namespace` (XS); (10) audit `*LocalIndex` allocation outside the hot path — 40 sites; the unboxed `slot, depth int` fast path already exists (M, benchmark-gated). Sequence: dead-code (1, 2, 3, 7, 9) first; structural (5, 6, 4, 8) next; allocation audit (10) last. `plans/2026-05-09-environment-structural-reduction.md`
- [ ] **Bidirectional opcode conversion test** [Medium, S]: Verify `operationToInstruction` and `instructionToOperation` cover the same opcode set.
- [ ] **LocalEnvironmentFrame pointer ambiguity** [Low, S]: Doc comment on `NewLocalEnvironment` explaining lifecycle (value-vs-pointer ownership).

### Tech Debt Plan (remaining)

- [ ] **Task 6.2: Replace `context.TODO()` in tests** [Low, S]: 431 occurrences across 39 test files. Mechanical `→ context.Background()`.
- [ ] **Task 6.4: Add `typeswitchlint` to value type guide** [Low, S]: Guide comment missing step for `cmd/typeswitchlint/main.go:knownValueTypes`.
- [x] **Task 8.1: Extract `machine/compilation/resolver/`** [Done]: FileResolver implementations extracted. `LibraryEnumerator` replaced with `FileEnumerator.EnumerateFiles` (returns paths, not `LibraryName`). Type aliases in compilation for backward compat. `plans/2026-04-13-resolver-extraction-impl.md`
- [ ] **Task 8.2: Evaluate `wile.Value` wrapper** [Low, M]: Wrapper provides minimal methods beyond `Internal()` escape hatch.
- [ ] **Task 8.4: Make `DefaultBigFloatPrecision` configurable** [Low, M]: 256-bit precision hardcoded across 12 call sites. No engine option.
- [ ] **Error sentinel grouping** [Low, S]: ~109 sentinels in flat list. Consider category-specific files if count exceeds ~150.
- [ ] **Namespace registry typing** [Low, S]: Namespace's registry should have a type instead of `any`.
- [ ] **ValueType refactoring** [Low]: ValueType doesn't have grounding in Scheme or Go — determine use and scope of type domains.
- [ ] **Evaluate need for Primitive Annotation Enforcement** [Low]: Enforcement may not be needed.

### Algebra library consistency (2026-04-23 staff-engineer audit)

- [x] **Promote setoid collection helpers** [High, S, Done]: `setoid-member?`, `setoid-assoc`, `setoid-dedup` now public in `(wile algebra setoid)`; private `%`-copies deleted from `group.scm` and `combinatorial-graph.scm`. All call sites updated.
- [x] **Promote options-alist helpers** [High, S, Done]: `assv-or` and `validate-opts-keys` folded into `(wile algebra setoid)` as public helpers (noted as "plumbing, not setoid ops" with a section comment; move to a dedicated module later if a third category accumulates). Scope was larger than audit showed: duplication existed in FOUR libraries (group, combinatorial-graph, **incidence, lattice**). All four call sites updated; `incidence.sld` gained a `(wile algebra setoid)` import.
- [x] **Drift-check test for umbrella `algebra.sld`** [High, S, Done — option (c)]: `algebra_umbrella_drift_test.go` parses every leaf `.sld` export clause and asserts umbrella coverage. First run caught real drift (`rewrite.sld: associativity-axiom-op commutativity-axiom-op`; `semiring.sld: tropical-inf`) — added to umbrella. Deferred decisions (a) delete and (b) generate until test-driven drift frequency justifies either path.
- [x] **Documented convention for structure API** [Medium, S, Done — supersedes "extract with-X macro skeleton"]: Reviewing the audit's own premise: a `define-with-binder` meta-macro would save ~10 lines across 15 libraries at the cost of indirection. Not worth it. Replaced with `stdlib/lib/wile/algebra/CLAUDE.md` documenting the five-part structure API (`make-X` / `X?` / accessors / `with-X` / `validate-X`), the shared plumbing in `(wile algebra setoid)`, options-alist discipline, validator body shape, and the `with-X` skeleton. New structures and reviewers have an anchor; duplication stays mechanical.
- [x] **Extract `validate-X` violation-collector idiom** [Medium, S, Done]: `make-violation-reporter` added to `(wile algebra setoid)` — two-mode procedure (call with type + args to record, call with no args to finalize). Retrofitted every `validate-X` across 14 libraries (setoid, monoid, group, ring, field, semiring, lattice + /distributive + /modular, boolean, heyting, category, closure, differential, partial-order + /setoid, galois's gc-sound?, combinatorial-graph). Parent-validator delegation patterns (field→ring, boolean→lattice, heyting→lattice, partial-order/setoid→partial-order, differential→ring) now use `(for-each (lambda (v) (apply fail! v)) parent-result)` instead of `(set! violations (append ...))`.
- [x] **Resolve `assert-X` asymmetry** [Medium, S, Done — dissolved via generic helper]: Rather than add 18 `assert-X` symbols, added `assert-validation` as a syntax-rules macro in `(wile algebra setoid)`. `(assert-validation (validate-group G s))` raises if the result isn't `#t`, with the source expression preserved in the error datum. Existing `assert-group`/`assert-graph` kept as thin conveniences; new structures should use the generic helper. Net API-surface cost: +1 symbol instead of +18.
- [x] **Refactor `combinatorial-graph.scm` monolith — first cut** [Medium, M, Done (partial)]: Replaced `%list-sort`/`%insert` (custom insertion sort) with `list-sort` from `(srfi 132)` — 14 lines removed, new library import added. File is now 1,726 lines (down from 1,787 including the setoid/options helper promotion earlier this session). Remaining `%`-prefixed helpers (`%sig<`, `%key<`, `%refine-step`, `%nat-*`, `%backtrack-canonical`, etc.) are genuinely graph-specific or WL/isomorphism-specific — splitting into sub-files (`-isomorphism`, `-polynomials`, `-matching`) is deferred until that provides tangible review-scope benefit.
- [x] **Normalize `make-X` constructor validation discipline** [Low, S, Done]: Added `assert-procedure` macro to `(wile algebra setoid)` — uses `syntax-rules` to capture the source identifier, so `(assert-procedure "make-ring" plus)` raises `"make-ring: plus must be a procedure"` on non-procedure input. Retrofitted 11 non-validating constructors: `make-setoid`, `make-monoid`, `make-partial-order`, `make-closure-operator` (split into `make-X*` record-type ctor + wrapper), `make-semiring`, `make-ring`, `make-field`, `make-boolean-algebra`, `make-heyting-algebra`, `make-category`, `make-differential-ring`, `make-galois-connection`. The audit noted `make-lattice` as non-validating — it was already validating. Convention documented in `stdlib/lib/wile/algebra/CLAUDE.md` "Procedural-argument discipline for `make-X`" section.
- [ ] **Watch `matrix.scm` for split pressure** [Low, S, Deferred]: 1,302 lines with two record types (`<semiring-matrix>` at 839, `<sparse-semiring-matrix>` at 1137) in one file. Shared helpers justify co-location today. Revisit once a third representation (banded, symmetric, etc.) appears — no action needed now.
- [ ] **Harmonize `docs/algebra/reference.md` section template** [Low, M, Deferred; 2026-04-23 crosscheck consistency finding]: First 15 sections use a fixed 5-heading template (Constructors → Predicates → Operations → Validation → Destructuring). The 11 sections added in PR #706 (matrix, polynomial, incidence, interval, graph, combinatorial-graph, unification, fca, pareto, abstract-domain, dataflow) use bespoke headings because their library shapes don't match the 5-part structure pattern (e.g. dataflow has no "law checker"; unification has pattern-vars, substitutions, matching as three parallel concerns). Decision at the time: keep bespoke headings since forcing the template would obscure real structural differences. Revisit if either (a) the template gets extended to cover the new shapes cleanly, or (b) a reader reports navigation trouble across sections.

### Helpers TypeName Encoding (PR #725 deferred items)

Items surfaced by /crosscheck adversarial review on PR #725 (helpers
typeName encoding refactor). Deferred per scope or design choice.

- [ ] **Distinct `*TypeSentinel` type for compile-time enforcement** [Tech debt, M, Deferred per Q1=A]: Type-design analyzer recommended splitting `*StaticError` into two types: `*StaticError` for non-type sentinels and `*TypeSentinel` for type-mismatch sentinels (embedding or wrapping `*StaticError`). Helpers like `RequireArg`/`RequireType` would take `*TypeSentinel` directly, making "passing a non-type sentinel to a type helper" a compile error. Current design uses runtime sum-as-struct discriminant (empty `expectedType` = non-type) plus `TestTypeSentinelsCarryTypeName` allowlist as the guard. Future cleanup once a real misuse incident motivates the rename across the codebase. See PR #725 review.
- [ ] **Store bare noun in `expectedType`, apply `articleFor` at format time** [Tech debt, S, Deferred]: Currently `NewTypeSentinel("string")` stores `expectedType: "a string"` (with article baked in). Type analyzer recommended storing `noun: "string"` and applying `articleFor` during `Error()`/`TypeName()`. Would let the article rule evolve (e.g., switch to phonetic) without regenerating sentinels, and would isolate the orthographic rule from the data. Pass-through irregulars ("a once") would need a separate `irregularArticle` field or override map.
- [ ] **`TypeNamer` interface for `typeNameFromSentinel`** [Tech debt, S, Deferred]: Currently `typeNameFromSentinel` matches on concrete `*werr.StaticError` via `errors.As`. Type analyzer recommended an open-extensible `interface { TypeName() string }` so any future error type could opt in. Trade-off: opens to accidental participation by unrelated types adding `TypeName() string`. Address when a second carrier of TypeName actually appears.
- [ ] **`Lengthable` rename to `IndexedSequence`** [Bikeshed, S, Deferred]: Type analyzer noted the helpers use the constraint as "indexed finite sequence" but the name `Lengthable` promises only `Length() int`. `*String`, `*Pair`, and `emptyListType` accidentally satisfy `Lengthable` but cannot meaningfully participate in `SequenceRef`/`SequenceSet`. Rename when the asymmetry causes real confusion.
- [ ] **Reflection-based `TestTypeSentinelsCarryTypeName`** [Test debt, S, Deferred]: Currently the inventory test enumerates ~55 type sentinels by hand. Test analyzer recommended a reflection-based variant that walks all exported `*StaticError` vars in `werr/` and asserts any whose `Error()` starts with `"not "` has a non-empty `TypeName()`. Self-maintaining, ~20 lines replacing ~60. Add when a contributor adds a new sentinel and forgets the inventory entry.
- [ ] **Extension-level message-content tests for new sentinels** [Test debt, M, Deferred]: Test analyzer flagged that no extension-level test asserts the user-visible "expected an integer/namespace/once" message content. Helper-level tests in `registry/helpers/args_test.go` pin the plumbing end-to-end through `TestRequireType_ErrorMessageContainsTypeName`, but a regression that, say, swaps `ErrNotAnInteger` back to `ErrNotANumber` in `make-vector` would not be caught by a test. Belt-and-suspenders coverage; add per primitive when message wording becomes load-bearing for users.
- [ ] **`ParseOptionalStartEnd` / `ParseOptionalArg` literal phrases** [Tech debt, S, Deferred]: Silent-failure hunter flagged that these helpers hardcode "improper argument list" / "too many arguments" rather than reading from sentinels. These are *shape* errors (proper-list, arity), not type errors, so the design choice to skip TypeName plumbing is defensible — but the file mixes two conventions. Either add a comment noting "shape errors don't need TypeName plumbing" or migrate to a parallel mechanism.
- [ ] **`read-line` / `peek-char` `UnreadRune` errcheck** [Bug, S, Deferred — pre-existing]: `internal/extensions/io/prim_read_write.go:337` swallows `UnreadRune` error with `//nolint:errcheck`. If `UnreadRune` fails, the next read sees corrupt data with no error signal. Pre-existing pattern; surfaced by but not introduced by PR #725.
- [ ] **`peek-char` error classification** [Bug, S, Deferred — pre-existing]: Same file uses `WrapForeignErrorf` rather than `WrapForeignReadErrorf`, so the resulting error doesn't satisfy `(read-error? e)` per R7RS §6.11. Pre-existing.
- [ ] **Library-binding installation swallows errors silently** [Bug, S, Deferred — pre-existing]: `machine/compilation/library_bindings.go:281-289` (the propagation branch in `CopyLibraryBindingsToEnvAtPhase`) and `library_bindings.go:328-336` (the syntax-binding branch in `copyLibraryBindingsDirect`) discard return values from `MaybeCreateOwnGlobalBinding` and `SetOwnGlobalValue` via `_, _ =` / `_ =`. A failed `SetOwnGlobalValue` in the syntax-binding branch means a macro is silently not installed in the expand environment; subsequent macro expansion mysteriously fails. The non-propagation branch (lines 270-273) wraps and returns errors correctly — the asymmetry is "evolved separately." Pre-existing; surfaced by but not introduced by PR #728. Fix: wrap and return per the existing convention; while there, validate `targetPhase + sourcePhase` against int8 overflow.

### Postponed

Items deferred for stated reasons. Re-evaluate when preconditions change.

- [ ] **F11: Promote internal extensions** [Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. Structural difference is semantic, not accidental.
- [ ] **VM dispatch loop extraction** [Postponed]: `MachineContext.Run()` is 547 lines with 65 inlined opcode cases. Go has no computed goto; method dispatch adds measurable overhead on hot path. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Postponed]: Requires design decision on canonical pattern.
- [ ] **Schemeutil: grab-bag reorganization** [Postponed]: Moving functions risks import cycle issues.

---

## Tier 6 — Nice-to-Haves

No demand signal. Speculative or research-only.

### Tooling
- [ ] **Hygiene debugging** [Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Programmatic tokenization/parsing**: Expose tokenizer/parser to Scheme. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Event callbacks**: Hooks for expansion, compilation, debugging. IDE integration, profiling.

### Standard Library
- [ ] **Hashtable SRFI compliance**: Current custom API (10 primitives) doesn't conform to any SRFI. Gaps vs SRFI-125: no custom hash/equality, no `hash-table-update!`, no fold/map, no immutable variant, naming uses `hashtable-*` not `hash-table-*`. Decide: SRFI-125 or keep custom.
- [ ] **Logging library**: Levels, structured output, handlers.
- [ ] **Go AST Phase 3 — Comments & generics** [S]: `Comment`/`CommentGroup` attachment, `BadExpr`/`BadStmt`/`BadDecl` error recovery, `IndexListExpr` for generics. `plans/GO-AST.md`
- [ ] **`continuation-mark-set-first` accepts `#f` for mark-set** [XS, Racket-compat]: Racket lets `#f` stand in for "current continuation's marks" as the first argument; Wile's `PrimContinuationMarkSetFirst` (`registry/core/prim_cont_marks.go:54`) hard-requires `*machine.ContinuationMarkSet` via `RequireType`. One-branch fix: check `values.FalseValue` before the type check and substitute `mc.CollectContinuationMarks(machine.DefaultPromptTag)`. Surfaced by the audit findings crosscheck on PR #673; no demand signal yet. Defer until the audit's Phase 4 (axis C — Racket compliance sweep) or a real consumer asks.

### Core Language
- [ ] **Type system**: Covers base types, expandable. Discover useful type properties. Types as distinct top-level concept.
- [ ] **let-syntax*** [S]: Implement `let-syntax*`.
- [ ] **Scribble-style `@` reader notation** [Reader extension]: Racket-style at-expressions for rich documentation markup. `@cmd[datum ...]{text ...}` desugars to S-expressions.

### Architecture
- [ ] **Dialect system** [Proposed]: De-globalize forms registry, `WithDialect()`, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined.
- [ ] **User labels/tags for FS resolvers**: Distinguish bootstrap from include/library loaders in fileResolver.

### Testing & Quality
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`stdlib/lib/*/test/`), new test cases.
- [ ] **Parser unit tests**: Unit tests for parser.

### Content
- [ ] **Blog area in repo**: Git blog area.
- [ ] **Finish blog article**: Scheme for sandboxing.

---

## Documented Exceptions

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Investigated & Rejected

These items were investigated and determined not to warrant changes:

- [x] **Promoted op table**: Table-driven dispatch regressed ~1.5% geo mean (15/16 Gabriel benchmarks slower). Go compiles contiguous-integer switches to jump tables; table-driven adds overhead. `plans/2026-04-05-structural-reduction.md`
- [x] **PrimitiveSpec dead fields (D1)**: Originally 5%/2% usage. Extension contracts Phase 1 populated both broadly. No longer dead.
- [x] **ForeignClosure redundant fields (D2)**: `doc` duplicates `PrimitiveSpec.Doc` but costs ~3.2KB total, set once, cannot diverge. Removing requires circular import workarounds.
- [x] **Namespace root/child state waste (D3)**: ~6 nil fields in children. Not worth splitting — zero-value costs nothing, children are rare.
- [x] **LocalIndex / BindingID overlap (D4)**: `LocalIndex` is relative (slot+depth), `BindingID` is absolute (frame pointer + slot). Both needed.
- [x] **Binding/BindingMeta (FCA)**: Clean lazy-initialization pattern. FCA false positive.
- [x] **PrimitiveRegistration/PrimitiveSpec (FCA)**: Orthogonal concerns properly separated. FCA false positive.
- [x] **CompileTimeCallContext (FCA)**: 2-field value type parameter, not coupling. FCA false positive.
- [x] **Opcode resource limits**: Per-category limits (match steps, expand steps, continuation copy depth). Existing mechanisms sufficient: `WithMaxCallDepth` bounds recursion, `WithMaxStackSize` bounds stack growth, `context.WithTimeout` checked every 1024 ops in VM and match loops. Deterministic per-category budgets are niche; timeout is an adequate proxy.

---

## FCA Assessment

Detailed staff-engineer assessment of cross-boundary coupling. Actionable items extracted into Tier 1 and Tier 5 above.

<details>
<summary>Full FCA findings (click to expand)</summary>

**[Priority: High] — vmState field-addition has 6 unguarded copy sites**

Where:
- machine/machine_context_continuation.go:31-224 (Restore, RestoreAndRelease, PopContinuation, SaveContinuation)
- machine/machine_continuation.go:96-113 (NewMachineContinuationFromMachineContext)
- machine/machine_continuation.go:157-183 (Copy)
- machine/machine_context.go:93-110 (NewMachineContext)

What: Adding a field to vmState requires updating 6 functions across 3 files, each with different copy semantics (transfer, clone, skip, force-false). No compile-time guard ensures all sites are updated. The documentation table at vm_state.go:78-93 is the only safety net — and it's comments.

Why it matters: Every field added has to be reasoned about independently at each site. The envPooled column alone has four different behaviors. The marks field uses cloneMarks in some paths and direct assignment in others. Miss one site → silent state corruption.

---

**[Priority: High] — No two transfer operations agree on which fields to copy**

Where: machine/machine_context_continuation.go — all four operations

What: Save, Restore, RestoreAndRelease, Pop each copy a different subset of vmState. The non-uniformity isn't accidental — each deviation is a semantic decision documented only in comments. vmState is treated as three implicit partitions (always-transfer, conditionally-transfer, never-transfer) but there's no type-level encoding.

The deeper issue: The evals field alone has four distinct ownership modes across the four operations.

---

**[Priority: Medium] — Opcode extension requires 7 coordinated edits**

Where:
- machine/opcode.go (constant + table entry)
- machine/machine_context.go:305-329 (dispatch switch)
- machine/native_template.go:129+, 256+ (both conversion directions)
- machine/operation_*.go (new Operation type)
- machine/compilation/*.go (compiler emission)
- machine/peephole.go (if fused)

What: Adding a new opcode touches 7 mandatory sites. The bidirectional conversion switches must stay synchronized.

---

**[Priority: Medium] — LocalEnvironmentFrame pointer ambiguity**

Where: environment/local_environment_frame.go:29-33, environment/environment_frame.go:93-108

What: LocalEnvironmentFrame is embedded by value in EnvironmentFrame (for heap savings), but NewLocalEnvironment() returns *LocalEnvironmentFrame (heap-allocated). Same type, two ownership semantics.

---

**State of the Code**: Wile's machine package is well-documented and intentionally designed, but carries real evolution risk in vmState transfer operations. The CESK architecture is sound. The debt isn't in the abstraction — it's in hand-unrolled field copying where each of 6 functions implements a different subset of a 12-field copy with different ownership semantics, guarded only by a comment table.

</details>

---

## Completed

<details>
<summary>Completed items (click to expand)</summary>

### Bugs & Correctness
- [x] **Peephole optimizer double-restore** [Fixed]: `savedCont` pointer-identity guard. `plans/OPTIMIZER-FIX.md`
- [x] **Degenerate form pipeline tests** [Done]: Full-pipeline tests for all core special forms. PR #571.
- [x] **Sub-context winding stack inheritance hazard** [Fixed]: Constructor parameter requirement. `machine/machine_context_subcontext.go`.
- [x] **`cond-expand (library ...)` bypasses FileResolver** [Fixed]: `machine/compilation/features.go`.
- [x] **syntax-rules ellipsis and hygiene bugs** [Fixed]: Three bugs — scope-aware duplicate binding detection (PR #607), cross-group ellipsis zipping, nested ellipsis depth tracking (PR #606).

### Refactoring
- [x] **`WalkSubExprs` for validated expression traversal** [Done]: `ChildRole` enum, B1 capture analysis migrated.
- [x] **Extract interface types from `environment/` `any` fields** [Done]: 15 type assertions removed across 7 files. `plans/2026-03-31-environment-any-fields.md`
- [x] **`Stack.Pull()` O(1) replacement** [Done]: `PullDrain()` in `OpPullApply`. `plans/2026-03-31-pulldrain-design.md`
- [x] **Split `ffi.go` by concern** [Done]: 1010 lines → 4 files. PR #599.
- [x] **Engine initialization order invariant** [Done]: 6-step DAG documented. `plans/2026-04-01-engine-init-order.md`
- [x] **`machine/` mega-package decomposition** [Done]: PRs #592, #593. `plans/2026-03-30-machine-decomposition-design.md`
- [x] **`file_resolver.go` chain of responsibility** [Done]: 541 → 469 lines.
- [x] **Timing-dependent concurrency tests** [Done]: PR #602. `plans/2026-04-01-timing-dependent-tests.md`
- [x] **ExpanderTimeContinuation convention deviations** [Done]: 18 deviations fixed.
- [x] **Opcode metadata consolidation (D5)** [Done]: `OperandKind` enum. `plans/2026-04-05-structural-reduction.md`

### Tech Debt
- [x] Task 1.1: `uint16` source table index overflow → `uint32`
- [x] Task 1.2: Opcode round-trip exhaustiveness test (already existed)
- [x] Task 1.3: Extension list consistency test (already existed)
- [x] Task 1.4: Eval stack size limit — `WithMaxStackSize(n)`. `plans/2026-04-11-eval-stack-limit-design.md`
- [x] Task 4.2: Security gate integration tests (already existed)
- [x] Task 5.1: `NamedCallable` interface
- [x] Task 5.2: `StringOrFalse` helper. PR #609.
- [x] Task 5.3: `ForEachList` for proper-list enforcement. PR #609.
- [x] Task 5.4: `requireSourceContext` helper. PR #609.
- [x] Task 5.5: `RequireArg[T]` migration (5 sites, 3 intentional deviations). PR #609.
- [x] Task 6.1: Delete `runtime/` package
- [x] Task 6.3: Receiver naming normalized. PR #609.
- [x] Task 7.1: Unified `machine/testutil` into `registry/testhelpers`. PR #609.
- [x] Task 8.3: REPL decoupled from `machine/compilation`. PR #639. `plans/2026-04-11-repl-decoupling-design.md`
- [x] Task 8.5: `prim_eval.go` funneled through `NewSubContext`. PR #637. `plans/2026-04-11-eval-subcontext-design.md`

### Performance
- [x] **GC pressure reduction** [Done]: -8.9% geo mean. PRs #562-563. `plans/GC-PRESSURE-REDUCTION.md`
- [x] **Core-let compilation** [Done]: PR #570. `plans/CORE-LET-IMPL.md`
- [x] **Procedure inlining** [Done]: PR #605. `plans/PROCEDURE-INLINING.md`
- [x] **B2 escape analysis** [Done]: PR #604. `plans/ESCAPE-ANALYSIS.md`

### Features
- [x] **Algebra library** [Done]: `(wile algebra)`. 158 tests. `plans/2026-03-25-algebra-library-design.md`
- [x] **`(wile algebra polynomial)` library** [Done]: Ring-parameterized univariate polynomials. 12/12 tasks. 60 tests passing. poly-plus/negate/minus/times, Horner evaluation, formal derivative (characteristic-safe, O(n) via accumulator threading), divmod (field-required), GCD (Euclidean, monic-normalized), polynomial-ring capstone (enables recursive rings R[x][y]), `with-polynomial` macro. Commits `69b98203`..`78bb7e2f`. `plans/2026-04-18-polynomial-library.md`
- [x] **`(wile algebra matrix)` library** [Done]: Semiring-parameterized matrix algebra (§5.1 of foundations). Path D implementation across 10 phases: sparse/dense representations, dispatch-table rep-tags, bang-first arithmetic, aliasing enforcement. Test count 112→303. PRs #684–#691 (P2–P10), #695 (error attribution), #696 (N1–N9 crosscheck follow-ups). `plans/2026-04-20-algebra-matrix-impl.md`, `plans/2026-04-21-matrix-path-d-impl.md`.
- [x] **`(wile algebra incidence)` library** [Done]: Möbius/incidence algebra on locally-finite posets per Rota (1964) (§5.2 of foundations). Formalizes ad-hoc direct-vs-transitive handling across four wile-goast posets (dominator trees, subtype lattices, call-graph reachability, import DAGs) and belief-DSL overlap normalization. `<locally-finite-poset>` with `(leq? interval-proc)`, ring-parameterized with `(integer-ring)` default, lazy memoization via `equal?`-keyed hashtable. ~200 LOC, ~25 tests. Commit `4ff8a314`. `plans/2026-04-21-incidence-algebra-impl.md`.
- [x] **`(wile algebra unification)` library** [Done]: AC-matching and AC-unification per Eker/Stickel/Contejean–Devie (§5.3 of foundations). `ac-match`, `ac-unify`, `<pattern-var>` records, substitution suite, `diophantine-basis`. `ac-unify` returns CSU (finitary-not-unitary per Fages–Huet 1986). PR #698 (30 commits on `feat/algebra-unification`). `plans/2026-04-21-ac-matching-design.md`, `plans/2026-04-21-ac-matching-impl.md`.
- [x] **SRFI-14 + `(wile charsets)`** [Done]: 17 FFI primitives + 23 derived Scheme procedures + 17 named char-sets (`char-set:letter`, `char-set:digit`, `char-set:whitespace`, etc.) sourced from Go's `unicode` tables; inversion-list representation, fully immutable. Char-set criteria enabled across 7 SRFI-13 procedures (`string-index`, `string-skip`, `string-count`, `string-trim*`, `string-tokenize`, `string-filter`, `string-delete`). `(wile charsets)` exposes `char-set-ranges` for efficient iteration. PR #TBD. `plans/2026-05-04-srfi-14-design.md`, `plans/2026-05-04-srfi-14-impl.md`.
  - **Deferred SRFI-14 names** (7 total; re-add cost per §11 of design):
    - `char-set-hash` — spec is loose; defer until someone needs char-sets as hash keys. ~30 LOC to re-add.
    - `char-set-cursor`, `char-set-ref`, `char-set-cursor-next`, `end-of-char-set?` — pre-1995 iteration protocol, redundant with for-each/fold. ~60 LOC to re-add.
    - `char-set-diff+intersection`, `char-set-diff+intersection!` — niche micro-optimization. ~40 LOC to re-add.
- [x] **SRFI-13 + `(wile strings)`** [Done]: 60 SRFI-13 procedures + `string-trim-left` alias + 5 `(wile strings)` extras (`string-split`, `string-replace-all`, `string-byte-length`, `string-blank?`, `string-repeat`); 309 integration tests across 8 phases. All pure Scheme; FFI promotion deferred (profile-driven §6 of design). `(wile strings)` resolves the SRFI-13 vs R7RS `string-map` shadowing via `(except (scheme base) string-map)`. Char-set criteria enabled by SRFI-14; `string-titlecase`, `string-hash`, `string-unfold`, `xsubstring`, `*/shared` forms also deferred per §11 of design. PR #721. `memory/2026-05-03-string-primitives-design.md`, `memory/2026-05-03-string-primitives-impl.md`.
- [x] **Documentation system** [Done]: Full infrastructure — `,doc`, `,apropos`, `,topics`, `,topic`, library descriptions, docstring examples. PRs #579-591.
- [x] **MCP server** [Done]: `wile --mcp`. PR #588. `plans/2026-03-26-wile-mcp-server-design.md`
- [x] **`(available-libraries)` primitive** [Done]: PR #590. `plans/AVAILABLE-LIBRARIES.md`
- [x] **OpaqueValue type** [Done]: Generic opaque wrapper for Go objects in Scheme.
- [x] **Disassembler** [Done]: `(disassemble proc)`, `,dis`, MCP tool. PR #603.
- [x] **Go AST Phase 2** [Done]: 13 node types. PR #480. `plans/GO-AST.md`

### Other
- [ ] **Important refactoring**
    - When few fields are referenced from a struct within a function, pass in the field - do not pass in the struct or a reference to the struct

- [x] **Promote `eval` extension to public** [Done]: Moved `internal/extensions/eval/` → `extensions/eval/`, importable as `github.com/aalpar/wile/extensions/eval`. Required by wile-goast and any embedder wanting sandboxed `(eval ...)` / `(load ...)`. The naive composition `WithProfile(Console) + WithExtension(eval.Extension)` does **not** work — `ConsoleAuthorizer` denies `code:load`, so `(load ...)` fails. The fix is a baked `ConsoleWithLoad` profile (extensions + matching authorizer that allows `code:load` under `/tmp`), now part of `plans/2026-03-26-environment-profiles-impl.md`.

</details>


