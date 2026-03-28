TODO
----

**Last Updated**: 2026-03-25

### Current Project Status

**Version**: v1.9.9 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower, core-compiled let forms
**Extensions**: 11 extension packages — 7 public (files, math, process, system, threads, gointerop, introspection), 4 internal (io, eval, namespace, all); all importable as R7RS `(wile <name>)` libraries. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Embedding**: CLI uses public Engine API; embedded stdlib via `stdlib.FS` (`go:embed` + `fs.Sub`); `AllExtensions()`/`WithAllExtensions()` convenience options.
**Examples**: 75 examples across 12 categories, 23 benchmarks (16 Gabriel + Larceny R7RS + Schelog + miniKanren)
**Tests**: Go test suite comprehensive; Scheme test suite: 3,248 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 924-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete; stdlib embedded in binary

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- [x] **Peephole optimizer double-restore** [High, Bug, Fixed]: `callForeignCached` and `applyForeign` would double-restore when PrimCallCC inline mode called `ApplyCallable` with a `ForeignClosure` (e.g., `(call/cc procedure?)`). Fixed with `savedCont` pointer-identity guard. `plans/OPTIMIZER-FIX.md`
- [x] **Degenerate form pipeline tests** [Correctness, Done]: Full-pipeline tests (string → tokenize → parse → expand → compile → run) for degenerate forms of all core special forms and macro-based derived forms. PR #571.
- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned

---

## Refactoring

### Low Priority

- [ ] **ExpanderTimeContinuation convention deviations** [Low, M]: wile-goast call-clustering found 40 deviations across 4 conventions (`SourceContext` 76%, `WrapForeignErrorf` 68%, `NewSyntaxCons` 64%, `IsSyntaxEmptyList` 60%). Some explained (let-syntax/letrec-syntax delegate to impl which wraps), others not yet checked.
- [ ] **Error sentinel grouping** [Low, S]: ~109 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.

### Postponed

- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Low, Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol.
- [ ] **VM dispatch loop extraction** [Low, Postponed]: `MachineContext.Run()` is 547 lines with 65 inlined opcode cases. Extraction adds indirection without clear benefit — Go has no computed goto, and method dispatch adds measurable overhead on the hot path. The two-tier model (inlined ops + `OpComplex` side table for 16 complex ops) already extracts the most complex operations. Intentional performance-over-readability trade-off.
- [ ] **Match: consolidate bytecode type files** [Low, Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Low, Postponed]: Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring.
- [ ] **Schemeutil: grab-bag reorganization** [Low, Postponed]: Moving functions risks import cycle issues. Needs careful dependency analysis.

---

## Performance & CI

### Completed

- [x] **GC pressure reduction** [Performance, Done]: FreeList migration for continuation/stack pools, pre-sized binding arrays in env frame pool, env frame leak fix in context release. -8.9% geo mean. PRs #562-563. `plans/GC-PRESSURE-REDUCTION.md`
- [x] **Core-let compilation** [Performance, Done]: `let`, `let*`, `letrec`, `letrec*` compiled as core forms with `ValidatedLet` + `OpPushEnv`, eliminating lambda overhead for all binding forms. PR #570. `plans/CORE-LET-IMPL.md`

### Actionable

- [ ] **Procedure inlining** [Performance, Research]: Explore peephole inlining of known procedures at compile time. `plans/PERFORMANCE.md`
- [ ] **Environment frame slimming** [Performance]: Reduce `EnvironmentFrame` struct for closure bodies that only need local bindings. `plans/PERFORMANCE.md`
- [ ] **B2 escape analysis for let-bound closures** [Performance, Research]: Track whether a closure stored in a let binding escapes the let scope. Enables `!Captured` for patterns like `(let ((f (lambda () x))) (f))` where the closure is only called locally. Requires data flow analysis within the let body. Blocked by: B1 capture analysis (`plans/CAPTURE-ANALYSIS.md`).

### Research

- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. 6-phase incremental plan written. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`

---

## Features

- [ ] **Opcode resource limits** [Security, Design]: Per-category limits for match/expand/continuation copy. Completes defense-in-depth for embedded use. `plans/SECURITY.md`
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [ ] **POSIX API / SRFI-170** [Standard library, 10 phases]: Comprehensive OS access — stat, permissions, links, temp files, env vars, subprocess, signals, user/group, terminal, error handling. Phase 1 (directory ops + process extension) completed in PR #565; remaining phases not started.
- [x] **Algebra library** [Standard library]: General-purpose algebraic structures as an R7RS library `(wile algebra)`. Partial orders, lattices (flat, powerset, product, map), fixpoint (Kleene + widening), monoids, semirings (boolean, tropical, counting), groups, rings (integer, modular), fields (rational), Galois connections. Design: `plans/2026-03-25-algebra-library-design.md`. 158 tests across 8 test files.

- [ ] **Documentation system** [Tooling, High, Partially Complete]: REPL-accessible documentation for all Scheme bindings. Completed: `procedure-documentation` primitive (PR #579, #581) for MachineClosure, ForeignClosure, CaseLambdaClosure; all 397 `PrimitiveSpec.Doc` fields populated; ~243 stdlib library procedure docstrings (PR #582); `,doc` REPL command via `DocProvider`. Remaining gaps: (1) bootstrap procedures lack docstrings, (2) `,doc` and `procedure-documentation` are separate paths, (3) special forms/macros have no doc mechanism, (4) no `apropos`/search, (5) no library-level or cross-reference docs.
  - [x] **Bootstrap procedure docstrings** [Content, Done]: 17 procedures in `registry/core/bootstrap_procedures.scm` (`map`, `for-each`, `member`, `assoc`, `not`, `zero?`, `positive?`, `negative?`, `list?`, `boolean=?`, `symbol=?`, `square`, vector/string map variants) now have docstrings. CxR accessors (28) intentionally skipped as self-documenting. Branch `docs/bootstrap-docstrings`.
  - [ ] **Unify `,doc` with `procedure-documentation`** [Unification, Medium]: The `,doc` REPL command uses `DocProvider` (registry lookup by name string). `procedure-documentation` reads from the closure object's `Doc()` field. These are parallel paths that don't see each other's data: `,doc` can't see Scheme-defined docstrings, and `procedure-documentation` can't see special-form docs. Unify so `,doc foo` calls `(procedure-documentation (eval 'foo))` for runtime bindings, falling back to `DocProvider` for compile-time-only bindings (special forms, auxiliary syntax).
  - [ ] **Docstring validation in validator** [Separation of concerns]: Move docstring detection from compiler to `internal/validate/` — produce a `ValidatedDocstring` node or attach doc metadata to `ValidatedLambda`/`ValidatedDefine`. Cleaner separation: the validator recognizes syntax, the compiler just reads a field. Prerequisite for Scribble-style `@` notation, which will need richer doc AST nodes beyond plain strings.
  - [x] **Docstrings for special forms and macros** [Infrastructure, Done]: Added `BindingMeta.Doc` field, `BindingSpec`/`DocEntry` registry types, and `ApplyDocs` post-bootstrap injection. ~29 special forms and ~15 bootstrap macros now documented via `,doc` in the REPL. Design: `plans/2026-03-27-special-form-macro-docstrings-design.md`.
  - [ ] **`apropos` / documentation search** [Usability]: No way to search for procedures by keyword. Guile has `(apropos "string")`, Chicken has `,a`. Should search across primitive names, `PrimitiveSpec.Doc` strings, and Scheme procedure docstrings. REPL command (`,apropos pattern`) and Scheme primitive (`(apropos "pattern")`) both needed.
  - [ ] **Library-level documentation** [Infrastructure, Needs Design]: No mechanism to document what a library provides. `(scheme base)`, `(wile algebra group)`, etc. have no runtime-queryable description, export summary, or purpose statement. `.sld` files are pure `define-library` forms with no body to hang a docstring on. Needs either a convention within `define-library` (e.g., a `(description ...)` clause) or an external doc registry keyed by library name.
  - [ ] **Cross-referencing and topic browsing** [Usability, Low Priority]: No way to list all procedures in a category (`,doc arithmetic`), follow "see also" links, or browse by topic. The `PrimitiveSpec.Category` field exists but isn't exposed through any user-facing interface. Low priority until the core documentation gaps are filled.
  - [ ] **Scribble-style `@` reader notation** [Reader extension, Low Priority]: Racket-style at-expressions for rich documentation markup. Reader recognizes `@cmd[datum ...]{text ...}` and desugars to S-expressions. Enables structured doc content beyond plain strings. Depends on docstring validation being in the validator layer.
- [ ] **Scheme linter** [Tooling, High, Needs Scoping]: Static analysis for Wile Scheme code — catch "plausible but wrong" before execution. Scope, design, and feasibility TBD. Potential checks: unused bindings, arity mismatches on known procedures, type mismatches at call sites, unreachable code after tail calls, style warnings. Research: what do Racket (Check Syntax), Guile, CHICKEN lint tools actually check? How much can be done at expand time vs requiring a separate pass? Interaction with the type system (if built) is a key design question.
- [ ] **Extension API contracts** [Embedding, High, Needs Design]: Stronger type/contract declarations on extension APIs so callers (including LLMs) know what types flow where. Currently `ForeignFunction` receives `MachineContext` and does its own type assertions via `helpers.RequireArg`. Design question: can contracts be declared in `PrimitiveSpec` (param types, return type) and validated automatically, or is this better as documentation-only metadata? Feeds into documentation system and linter.
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.
- [x] **OpaqueValue type** [Values, Embedding]: Generic opaque wrapper for Go objects in Scheme. `SchemeString()` → `#<tag:id>`, identity-based equality, `opaque?` and `opaque-tag` predicates. Enables wile-goast shared sessions (Track A1) and other Go-object-wrapping use cases.

---

## Nice-to-Haves

No demand signal. Speculative or research-only.

- [ ] **Hygiene debugging** [Tooling, Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Tooling, Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Dialect system** [Architecture, Proposed]: De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Architecture, Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Programmatic tokenization/parsing** [Tooling]: Expose tokenizer/parser to Scheme code. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [ ] **Logging library** [Standard library]: Levels, structured output, handlers.
- [ ] **Event callbacks** [Tooling]: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined. No demand signal yet.
- [ ] **Scribble syntax (@-expressions)** [Syntax]: Racket-style text processing. No demand signal yet.
- [ ] **Hashtable SRFI compliance** [Standard library]: Current implementation is a custom API (10 primitives, fixed FNV-1a hash, fixed `EqualTo` comparison). Not R7RS-small (hashtables aren't in the spec) but doesn't conform to any SRFI either. Gaps vs SRFI-125: no custom hash/equality functions in constructor, no `hash-table-update!`, no `hash-table-fold`/`hash-table-map`, no immutable variant, no `hash-table->alist`/`alist->hash-table` conversion, naming uses `hashtable-*` not `hash-table-*`. Decide: target SRFI-125 (broader ecosystem compat) or keep custom API. Internal design issue: bucket chaining over `map[uint64][]entry` could be replaced with native Go map.
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`stdlib/lib/*/test/`), new test cases for features not covered by Go test extraction.
- [ ] **Type system**: type system that covers all the base types and can be expanded.  Discover useful properties of types to track (if any).  Types should be a distinct type (exists at the top of the hierarchy) - except for maybe some generic object type.
- [ ] **Parser unit tests**: unit tests for parser.
- [ ] **Source file tracking in Syntax Objects**: need some utilities around finding source locations and providing source lines.
- [ ] **Exceptions and Error stack traces**: Both Foreign and Native errors should track stacktraces with source code references.
- [ ] **Foreign Stack trace entry in stack traces that cross from Native -> Foreign -> Native callback.**
- [ ] **Area for blog articles** Git blog area in repo
- [ ] **Finish blog article** Finish blog article on appropriateness of Scheme for sandboxing.
- [x] **Go AST extension Phase 2 — Advanced** [Standard library, S]: Concurrency (`GoStmt`, `DeferStmt`, `SelectStmt`, `CommClause`), switch (`SwitchStmt`, `TypeSwitchStmt`, `CaseClause`), `SliceExpr`, `TypeAssertExpr`, `ChanType`, `Ellipsis`, `LabeledStmt`. 13 node types. PR #480. `plans/GO-AST.md`
- [ ] **Go AST extension Phase 3 — Comments & generics** [Standard library, S]: `Comment`/`CommentGroup` attachment for round-trip structural fidelity. `BadExpr`/`BadStmt`/`BadDecl` for error recovery. `IndexListExpr` for generics. `plans/GO-AST.md`
- [ ] **Implement let-syntax*** [Core language, S]: Implement `let-syntax*`.
- [ ] **User labels/tags to distinguish FS resolvers** Use tags or labels to distinguish bootstrap loadee from include/library loaders in fileResolver.
- [ ] **Disassembler** Implement a disassembler for Wile
- [ ] **CompilationError** does not have source location, nor does it have an identity as a SchemeError and no Wrap* constructor.  Look into CompilationError and determine where it sits between Scheme and Foreign errors
- [ ] **RuntimeError** does not have an identity as SchemeError or ForeignError.  It also does not have a constructor
- [ ] **cond-expand (library ...) with fs.FS** `FindLibraryFile` in `features.go` uses `os.Stat` directly; `cond-expand (library ...)` cannot detect libraries in a virtual `fs.FS`. Requires passing `FileResolver` into the `FeatureRequirement` interface.
- [ ] **Scheme Dissasembly** "What does this compile to?" — When debugging or implementing compiler changes, I reason about bytecode by reading Go compiler code. A disassemble command that shows the opcode sequence for a Scheme expression would let me verify compilation directly instead of tracing through `compile_*.go` by hand. The disassembly one would have the highest impact — I'd use it constantly when working on the compiler, optimizer, or debugging macro expansion.
- [ ] **Primitive Search** "What primitives exist matching X?" — When implementing features or checking coverage, I grep across `registry/core/prim_*.go` files. A queryable primitive registry ("show me all string primitives" or "what's the signature of assoc?") would be faster.
- [ ] **Expression Evaluation**  "Does this expression raise or return?" — Quick smoke tests. The eval tool covers this, but I'm not sure how much I actually use it vs. reading code. Honest question — does eval return error details, or just success/failure?
- [ ] **Proposal**  Proposal: The simplest high-impact change would be renaming/re-describing the tool so it's clear it's a general Scheme evaluator (that also happens to have goast loaded). Something like: "Evaluate Scheme expressions in a persistent session. All R7RS features available. Also loads Go static analysis libraries: (wile goast), ..." That's a documentation fix, not a code change. The tool already does what I need — I just didn't reach for it because the description undersold it.
- [ ] **Consider ValueType Refactoring** ValueType does not seem to have a grounding in Scheme or Go, which begs the question of what sorts of type domains is it attempting to describe?  Ask specific questions of AI to determine the use and scope of the type domains
- [ ] **Evaluate Need for Primitive Annotation Enforcement** — enforcement may not be needed.
