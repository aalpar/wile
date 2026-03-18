TODO
----

**Last Updated**: 2026-03-18

### Current Project Status

**Version**: v1.5.0 (released)
**Core Language**: R7RS-small complete with hygienic macros, composable continuations, numeric tower
**Extensions**: 10 extension packages — 7 public (files, math, system, threads, exceptions, gointerop, introspection), 3 internal (io, eval, all); all importable as R7RS `(wile <name>)` libraries. Go static analysis extensions extracted to [wile-goast](https://github.com/aalpar/wile-goast).
**Examples**: 76 examples across 12 categories, 21 Gabriel benchmarks, Schelog
**Tests**: Go test suite comprehensive; Scheme test suite: 3,248 lines across 11 files (strings, characters, ports, numbers, exceptions, lazy, records, eval, control, macros) + 915-test R7RS conformance suite
**Libraries**: (chibi test), (chibi optional), (chibi diff), (chibi term ansi), (srfi 1) complete

### Ordering

Sections are ordered: bugs/correctness first, then performance, refactoring (by dependency), features, nice-to-haves last. Within refactoring, high-priority items that block other work come first; low-priority cosmetic items come last.

---

## Bugs & Correctness

- L7 (`char-ready?`/`u8-ready?` always `#t`) — documented semantic difference, no fix planned
- [x] **`==` error comparison in REPL** [Low, S]: Fixed — now uses `errors.Is(err, readline.ErrInterrupt)`.
- [x] **`guard` body drops multiple values** [Medium, S]: Fixed in #395 — body now uses `call-with-values` + `list` capture.
- [x] **Tuple ForEach nil returns Void instead of EmptyList** [Medium, S]: Fixed in #394 — nil guards and loop exits now return `EmptyList`/`SyntaxEmptyList`.
- [x] **Variadic math primitives accept excess args** [Low, S]: Fixed in #446 — `log`, `atan`, `number->string`, `string->number` now reject extra arguments. Also added missing radix validation to `string->number`.

---

## Refactoring

Ordered by dependency — items that unblock others or carry divergence risk come first. MachineContext decomposition is last (depends on other refactorings settling).

### High Priority

- [x] **Remaining raw sentinel panics** [High, S]: 20 `panic(werr.ErrXxx)` calls without `WrapForeignErrorf` wrapping. Sites: `values/pair.go:139,169,236`, `values/empty_list.go:83,88`, `values/promotion.go:308`, `internal/syntax/syntax_pair.go:151,168,172,181,198,212,338,357`, `internal/syntax/syntax_empty_list.go:91,96,101,106`, `machine/stack.go:43,55`. Wrapped with site-specific context. Ruleguard rule `noBareSentinelPanic` added to prevent regression.
- [x] **Bare division-by-zero returns** [High, S]: 7 numeric `Divide` methods return bare `werr.ErrDivisionByZero` without wrapping. Wrapped with `"Type.Divide: division by exact zero"` at each site.
- [ ] **internal/validate test coverage** [High, M]: 17 source files, 2 test files (1,724 source lines). 15 validator modules (`validate_lambda.go`, `validate_define.go`, `validate_macro.go`, etc.) have zero dedicated unit tests — coverage comes entirely from integration tests. Validators are the first defense against malformed syntax; silent validation failures produce confusing compiler errors or incorrect bytecode. `validate_macro.go` (247 lines), `validate_define.go` (185 lines), `validate_lambda.go` (84 lines) carry the most edge cases — start there.
- [ ] **Tokenizer `readNan` refactoring** [High, M]: 4 stacked TODOs in `internal/tokenizer/tokenizer_numbers.go:186,192,226,446` describe tangled state: "refactor readNan", "do not remove line below... should not be needed, but current readNan implementation sets p.state to Symbol on error". `readNan` incorrectly mutates tokenizer state on error, requiring callers to compensate. `//nolint:errcheck` on lines 193 and 225 silence error returns because callers work around the bug. Refactor `readNan` to return errors cleanly without state mutation side effects.

### Medium Priority

- [x] **Extract `parser.readSyntax()` dispatcher** [Medium, M]: Extracted `readCharacter()`, `readExactnessMarker()`, and 6 numeric methods to `parser_number.go`. Fixed `readQuoteForm` source location (pre-advance token) and unified Quote case. Switch reduced from ~238 lines to ~120 lines.
- [x] **Environment resolve: unify parent-chain walks** [Medium, M]: `GetLocalIndexWithScopes` was the only search walk duplicating `resolveLocal`'s loop. Rewritten to delegate to `resolveLocal` with `checkScopes=true`; visitor handles candidate collection and perfect-match early exit. All parent-chain search walks now use `resolveLocal`.
- [x] **`syntax_adapter.go` responsibility split** [Medium, M]: Split into `syntax_adapter.go` (~337 lines, matching + interfaces) and `syntax_expand.go` (~505 lines, template expansion). Two files instead of three — hygiene validation is inseparable from matching/expansion.
- [x] **Numeric tower type-switch checklist** [Medium, S]: Added 11-item checklist comment in `values/numeric_kind.go` listing every file that must be updated when adding a new numeric type. Added `TestTypeSwitchFunctionsHandleAllTypes` in `numeric_dispatch_test.go` covering `NumberToFloat64`, `NumberToComplex128`, `Simplify`, `ExactnessOf` across all 7 types.
- [x] **Opcode extension checklist** [Medium, S]: 7-item checklist comment already exists in `machine/opcode.go:22-29` listing all files that must be updated when adding a new opcode.
- [x] **Document ffi.go callback converter inversion** [Medium, S]: Already documented in `ffi.go:541-543` (direction inversion summary) and `ffi.go:559-561` (callback return explanation).
- [x] **Document optional argument patterns** [Medium, S]: Added decision tree to `registry/CLAUDE.md` covering `OptionalArg[T]` (typed default), `ParseOptionalArg` (presence check), and `ParseSubrange` (positional start/end).
- [x] **I/O port extraction helper** [Medium, S]: Extracted generic `extractPort[T]` in #424.
- [x] **Optional fill argument extraction**: 3 `make-*` primitives (`PrimMakeVector`, `PrimMakeBytevector`, `PrimMakeString`) independently extract optional fill arguments with slightly different patterns. Share a helper.
- [x] **Machine package tech debt** [Medium, M]: 6 phases — arity dedup, closure extraction, expander decomposition, letrec* unification, library import dedup, stale alias removal. Complete in #444. See `plans/MACHINE-TECH-DEBT.md`.
- [x] **Math extension file split** [Low, S]: Split 1,292-line `prim_math.go` into 5 files by R7RS section (transcendental, rounding, rational, complex, conversion). Complete in #446.
- [x] **Raw string panics** [Medium, S]: 4 panics used raw strings instead of `werr.WrapForeignErrorf(sentinel, ...)` in `extensions/math/prim_complex.go`, `machine/edit_plan.go`, `machine/native_template.go`. Wrapped each with the appropriate sentinel.
- [x] **Bare sentinel panics** [Medium, S]: `NumberToFloat64`, `NumberToComplex128` (`values/promotion.go`) and `ExactnessOf` (`values/numeric_tower.go`) panicked with bare `werr.ErrNotANumber` — no `WrapForeignErrorf` wrapping. Added call-site context.
- [x] **internal/forms tests** [Medium, S]: `internal/forms/form_spec.go` (105 lines) is the only internal package with zero test files. Central registry used by `validate` and `machine`. Add `form_spec_test.go` covering registration, duplicate detection, and lookup miss behavior. Complete in #452.
- [x] **Expander time continuation decomposition** [Medium, M]: Split 1,327-line `expander_time_continuation.go` into 4 files: `expander_let_syntax.go` (let-syntax/letrec-syntax), `expander_primitive_forms.go` (if, begin, set!, define, import, etc.), `expander_lambda.go` (lambda, case-lambda, helpers), core dispatch remaining in original file.
- [x] **Quasiquote/quasisyntax duplication** [Medium, M]: Extracted shared `expandQuasi`, `expandQuasiList`, `expandQuasiListWithSplice` into `machine/quasi_expand.go` with `quasiKeywords` config struct. Fixed latent `list*` bug in quasisyntax improper list expansion (replaced with nested `cons`).
- [x] **Pooling contract documentation** [Medium, S]: `machine/pool.go` has 4 global pools (stack, sub-context, continuation, env-frame). Continuation frames pooled only on normal return; `call/cc` escapes leave frames for GC via `MarkChainShared`. Documented in `docs/dev/POOLING.md`.
- [ ] **`context.TODO()` in production code** [Medium, S]: `values/pair.go:185` (`Length`) and `values/pair.go:388` (`AsVector`) pass `context.TODO()` to `ForEach`. These are the only production `context.TODO()` calls. `ForEach` accepts `context.Context` for cancellation, but these methods bypass it — infinite circular structures have no escape hatch. Either accept a `context.Context` parameter or use `context.Background()` to make the design choice explicit.
- [ ] **Numeric kind checklist incomplete** [Medium, S]: `values/numeric_kind.go:8-23` checklist for adding a new numeric type is missing `registry/helpers/value_conv.go` (contains `ToComplex128`, `ToFloat64` conversions). Also, item 9 (`wile-goast/goast/mapper.go`) is in an external repo — add a note flagging this.
- [ ] **extensions/math/ test coverage** [Medium, M]: 1,553 source lines across 7 files, 928 test lines in a single `prim_math_test.go` (0.59 ratio). `prim_complex.go` (275 lines), `prim_conversion.go` (310 lines), `prim_rational.go` (260 lines), `prim_rounding.go` (225 lines), `prim_transcendental.go` (320 lines) each lack dedicated test files. R7RS §6.2.6 mandates detailed edge cases for these operations. Split tests per module and add edge cases for complex construction and exact/inexact conversion.
- [ ] **ruleguard/ has zero tests** [Medium, S]: `ruleguard/rules.go` enforces the project's error handling invariants (`noErrorsNew`, `noFmtErrorf`, `noBareSentinelPanic`) but has no test file. If a rule has a regex bug or a Go version changes AST structure, violations pass lint silently. Add `ruleguard/rules_test.go` with positive and negative cases for each rule.
- [ ] **Missing checklists for extensions and special forms** [Medium, S]: Adding a new opcode (7 items) and numeric type (11 items) both have documented checklists. Adding a new extension (4-6 files) or special form (5-7 files) has no checklist. Extensions require touching `internal/bootstrap/environment_tiny.go:54` (hardcoded `allExtensions` slice) which is easy to miss. Special forms require coordinating validators and compilers across `internal/validate/register.go` and `machine/register.go`. Add checklist comments following the opcode checklist pattern.

### Low Priority

- [x] **Port type boilerplate** [Low, S]: `portBase` now provides `EqualTo()` and `SchemeString()` via embedding promotion. `EqualTo` uses `kind + datum` identity comparison (Go's `any` equality checks both dynamic type and value, preventing false matches across port types that share a kind). `IsVoid()` remains per-type (nil-receiver contract requires concrete type method). 10 `EqualTo` methods removed from 10 port files.
- [x] **Machine: document implicit PC contract** [Low, S]: Added doc comment on `pc` field in `vmState` listing all write sites (NewMachineContext, Apply, Restore, SaveContinuation, opcodes) and the raise-continuable rationale. Added defensive `pc < 0` bounds assertion at top of `Run()` with `ErrInvalidProgramCounter` sentinel. Test in `machine_context_test.go`.
- [x] **Compound if-init in OperationSaveContMark** [Low, S]: `machine/operation_cont_mark.go:87` used `if old := mc.GetMark(key); old != nil {` — extracted assignment before conditional per project style.
- [ ] **Error sentinel grouping** [Low, S]: ~103 sentinels in flat list with comment grouping only. Consider category-specific files or typed constant blocks if count exceeds ~150.
- [ ] **`int16` instruction argument limit undocumented** [Low, S]: Branch offsets in `machine/instruction.go` use `int16`, limiting jump distances to ±32,767 instructions. A single Scheme function with >32K bytecode instructions would fail with no clear error. Add a comment documenting the limit and a compile-time bounds check that panics with a descriptive message if a branch target exceeds `int16` range.
- [x] **Operation file consolidation** [Low, M]: Consolidated 24 single-method operation files into 6 family files: `operations_stack.go` (Push/Pop/Pull/Drop/PeekK), `operations_load_store.go` (Load*/Store*), `operations_control.go` (Branch/BranchOnFalse/SaveCont/RestoreCont), `operations_call.go` (Apply/ForeignFunctionCall/UnpackListToStack), `operations_closure.go` (MakeClosure/MakeCaseLambdaClosure), `operations_winding.go` (PushWind/PopWind/PopEnv). 5 larger files kept separate (build_syntax, syntax_rules_transform, syntax_case, cont_mark, helpers).
- [x] **Rename `AddSearchPath` to `PrependSearchPath`** [Low, S]: Renamed in library_registry.go and all call sites.
- [x] **Unify library/include path resolution** [Low, S]: `findFile` now consults library registry search paths as fallback dirs, sharing the same paths as `import`.
- [x] **Tokenizer test file consolidation** [Low, M]: Consolidated 14 coverage-goal-named test files into 10 behavior-oriented files mirroring source structure. All 191 tests preserved. PR #448.
- [x] **REPL deprecated wrappers** [Low, S]: Deleted `Compile`, `Run`, `Load` wrappers from `internal/repl/repl.go`. Internal call sites now use `wileruntime` directly.
- [x] **String utility duplication** [Low, S]: Unified `TrimPrefixFolded`/`TrimSuffixFolded` (parser) and `TrimPrefixCI`/`TrimSuffixCI` (tokenizer) into `internal/schemeutil/stringutil.go`. ASCII-only implementation (all call sites use ASCII prefixes). Deleted `parser_string.go` and `tokenizer/utils.go`. Fixed redundant double-trim of `#z`/`#Z` prefix in `parseBigIntegerWithBase`.
- [x] **Hand-rolled predicates in extensions** [Low, S]: Converted 9 hand-rolled type predicates (4 in threads, 5 in gointerop) to use `helpers.MakeTypePredicate` factory, matching core primitive pattern.
- [x] **Duplicated import set parsing** [Medium, M]: Already resolved — all three import paths (compile-time, library-internal, expand-time) call `.UnwrapAll()` to strip syntax wrappers, then delegate to the single `ParseImportSetFromDatum` in `import_set_datum.go`. No duplication exists.
- [x] **`forms` package type erasure** [Medium, S]: Moved `ValidatedExpr` interface to `forms` package (depends only on `*syntax.SourceContext`). Type alias in `validate` preserves all existing references. `ValidatorFunc` now typed: `env *environment.EnvironmentFrame`, `pair *syntax.SyntaxPair`, returns `ValidatedExpr`. `CompilerFunc` `expr` param typed as `ValidatedExpr`. Remaining `any` params (`result`, `ctc`, `ctctx`) are genuinely uncrossable — `validate` and `machine` import `forms`, so `forms` can't import them back.
- [x] **Machine: naming — `declareDefineBinding` vs `predeclareDefineBindingFromValidated`** [Low, S]: Investigated — semantically different. `declareDefineBinding` (compile_validated.go) compiles a single define form and returns the symbol for immediate use. `predeclareDefineBindingFromValidated` (compile_closure.go) pre-declares all defines in a body (letrec* Pass 1) with void return. Different phases, different return types, no unification needed.

### Postponed

- [ ] **F11: Promote internal extensions** [Low, Postponed]: `internal/extensions/{io,eval,all}` invisible to embedders. Promote to `extensions/{io,eval}/` when extension API stabilizes and external consumers exist.
- [ ] **Parser: unify readList + readLabeledList** [Low, Postponed]: High risk — datum labels require in-place mutation of placeholder pairs. The structural difference is semantic, not accidental. Unifying requires careful design to handle the placeholder protocol.
- [ ] **Match: extract opcode handlers from VM interpreter** [Low, Postponed]: 264-line switch is large but stable. Extraction adds indirection without clear benefit until new opcodes are needed.
- [ ] **Match: consolidate bytecode type files** [Low, Postponed]: Pure cosmetic reorganization.
- [ ] **Extensions: standardize registration patterns** [Low, Postponed]: Requires design decision on the canonical pattern. Worth a separate discussion, not a mechanical refactoring.
- [ ] **Schemeutil: grab-bag reorganization** [Low, Postponed]: Moving functions risks import cycle issues. Needs careful dependency analysis.
- [x] **F10: MachineContext decomposition** [Medium, Postponed]: Split 1,639-line `machine_context.go` into 5 files by responsibility: `machine_context_continuation.go` (286 lines), `machine_context_winding.go` (149 lines), `machine_context_subcontext.go` (106 lines), `machine_context_apply.go` (320 lines), core reduced to 857 lines.

---

## Performance & CI

- [x] **CI benchmark tracking** [CI, S]: Gabriel suite (16 benchmarks, 3 runs) runs in CI; results uploaded as 90-day artifacts. PR #430. Baseline regenerated from CI hardware in #431 — apparent 20% regression was a measurement environment mismatch (local vs CI hardware), not a code regression.

### Actionable

- [x] **Remove symbol interning** [Performance, M]: Removed `InternSymbol` canonicalization; symbols compared by `.Key` string via `helpers.EqIdentity`. ~50 call sites removed, `symbolInterns` map deleted from `TopLevelEnvironment`. `plans/REMOVE-SYMBOL-INTERNING.md`
- [x] **Optimize hot-path ForeignFunction calls** [Performance, M]: Promoted opcodes approach (A). Phase 1: list predicates/accessors (`null?`, `pair?`, `car`, `cdr`) — #497. Phase 2: binary arithmetic/comparisons (`+`, `-`, `<`, `<=`, `>`, `>=`, `=`) — #498. Phase 3: `cons`, `*`, `/` — bypasses dispatch for 2-arg calls; variadic falls back to `CallForeignCached`. `plans/OPCODE-PROMOTION.md`.
- [ ] **Procedure inlining** [Performance, Research]: Explore peephole inlining of known procedures at compile time. No plan yet.

### Architectural (Tier 3)

- [x] **Flat closures** [Performance, L, Reverted]: Implemented (PRs #514, #515, #516) and reverted. +7.4% geo-mean regression across 31 benchmarks — new `freeVars` slice allocation exceeded savings from eliminated parent-chain walks. `plans/FLAT-CLOSURES.md`
- [x] **Stack frames replacing continuation chains** [Performance, L, Closed]: Implemented and reverted (PR #518). Dispatch improved 5% on fib but regressed continuation-heavy benchmarks 10-20%. Pool-based `MachineContinuation` linked list retained. `plans/STACK-FRAMES.md`
- [ ] **NaN-boxing / tagged pointers** [Performance, L]: Encode small values (fixnums, booleans, chars) in 64 bits instead of 16-byte Go interface. Halves stack/binding sizes. Massive change, awkward in Go. `plans/PERFORMANCE.md`

### Research

- [ ] **Fused lexing/parsing** [Performance, Research]: Flap paper (PLDI 2023) — fuse tokenizer and parser into single character-level pass, eliminating per-token heap allocation. 6-phase incremental plan written. Gated on profiling confirming tokenizer is a bottleneck. `plans/PERFORMANCE.md`

---

## Features

- [ ] **Opcode resource limits** [Security, Design]: Per-category limits for match/expand/continuation copy. Completes defense-in-depth for embedded use. `plans/SECURITY.md`
- [ ] **Module decomposition Phase 1** [Architecture]: Decompose `internal/extensions/all/` into records, promises, core. Enables future module extraction. `plans/ARCHITECTURE.md`
- [x] **ER macro transformer** [Macro system]: Unlocks Chibi library ecosystem. Matters after Go-side adoption creates demand for Scheme library porting. `plans/MACRO_SYSTEM.md`
- [ ] **Network libraries** [Standard library]: TCP/UDP, HTTP, TLS, DNS. Required for real-world embedded use cases.
  - TCP/UDP sockets (tcp-connect, tcp-listen, tcp-accept, tcp-close)
  - HTTP client/server primitives
  - SSL/TLS support
  - DNS resolution
- [ ] **Debugger / DAP integration** [Tooling]: Debug Adapter Protocol. Inline traps + snap-to-next designs ready in `plans/DEBUGGER.md`
- [ ] **POSIX API / SRFI-170** [Standard library, 10 phases]: Comprehensive OS access — stat, permissions, links, temp files, env vars, subprocess, signals, user/group, terminal, error handling.
- [ ] **Go FFI Phase 3 — Plugin support** [Embedding]: Dynamic extension loading via registry pattern.

---

## Nice-to-Haves

No demand signal. Speculative or research-only.

- [ ] **Doc metadata for Scheme-defined procedures** [Tooling]: 44 primitives migrated to Scheme lost their `PrimitiveSpec` doc/params/category metadata (REPL `,doc`, extension library exports). Need a mechanism to register documentation for `define`-based procedures — either a doc-only `PrimitiveSpec` entry or a parallel doc registry that macro sources can populate.
- [ ] **Security context** Authorizer rides on `context.valueContext`.  This is considered to be bad style - create a custom security context that implements `context.Context`.
- [ ] **Hygiene debugging** [Tooling, Planned]: Scope introspection for macro authors. `plans/MACRO_SYSTEM.md`
- [ ] **Macro expansion tracing** [Tooling, Planned]: Trace generated code back to macro invocation/template. `plans/MACRO_SYSTEM.md`
- [ ] **Dialect system** [Architecture, Proposed]: De-globalize forms registry, `WithDialect()` option, extract R7RS as default dialect. `plans/ARCHITECTURE.md`
- [ ] **Plugin shadowing** [Architecture, Proposed]: Extension primitive shadowing. Depends on public extensions. `plans/ARCHITECTURE.md`
- [ ] **Programmatic tokenization/parsing** [Tooling]: Expose tokenizer/parser to Scheme code. 4 phases: token introspection, syntax introspection, EOF handling, advanced reader control.
- [x] **Continuation marks** [Runtime]: Racket-style per-frame key→value annotations. Phase 1: `marks` field on `vmState`, `with-continuation-mark` special form (#508, #509). Phase 2: `ContinuationMarkSet` type, `current-continuation-marks`, `continuation-mark-set->list`, `continuation-mark-set-first`, prompt-delimited collection (#510). Phase 3: `call-with-immediate-continuation-mark`, `continuation-marks` on captured continuations via `CapturedContinuation` type, `continuation?` predicate (#511). `plans/CONTINUATION_MARKS.md`.
- [ ] **Logging library** [Standard library]: Levels, structured output, handlers.
- [ ] **Event callbacks** [Tooling]: Hooks for expansion, compilation, debugging. IDE integration, profiling.
- [ ] **Feature flags (3-tier)** [Runtime]: Compile-time, runtime global, extension-defined. No demand signal yet.
- [ ] **Scribble syntax (@-expressions)** [Syntax]: Racket-style text processing. No demand signal yet.
- [ ] **Hashtable SRFI compliance** [Standard library]: Current implementation is a custom API (10 primitives, fixed FNV-1a hash, fixed `EqualTo` comparison). Not R7RS-small (hashtables aren't in the spec) but doesn't conform to any SRFI either. Gaps vs SRFI-125: no custom hash/equality functions in constructor, no `hash-table-update!`, no `hash-table-fold`/`hash-table-map`, no immutable variant, no `hash-table->alist`/`alist->hash-table` conversion, naming uses `hashtable-*` not `hash-table-*`. Decide: target SRFI-125 (broader ecosystem compat) or keep custom API. Internal design issue: bucket chaining over `map[uint64][]entry` could be replaced with native Go map.
- [ ] **Unit testing expansion**: Regression test files (`test/regression/`), library-specific tests (`lib/*/test/`), new test cases for features not covered by Go test extraction.
- [ ] **Type system**: type system that covers all the base types and can be expanded.  Discover useful properties of types to track (if any).  Types should be a distinct type (exists at the top of the hierarchy) - except for maybe some generic object type.
- [ ] **Parser unit tests**: unit tests for parser.
- [ ] **Source file tracking in Syntax Objects**: need some utilities around finding source locations and providing source lines.
- [ ] **Exceptions and Error stack traces**: Both Foreign and Native errors should track stacktraces with source code references.
- [ ] **Foreign Stack trace entry in stack traces that cross from Native -> Foreign -> Native callback.**
- [ ] **Area for blog articles** Git blog area in repo
- [ ] **Finish blog article** Finish blog article on appropriateness of Scheme for sandboxing.
- [ ] **Implement let-syntax*** [Core language, S]: Implement `let-syntax*`.
- [x] **Native forms migration** [Refactoring, M]: 44 of 52 done. Phase 3 complete: `call-with-port` migrated to Scheme in io extension (capturable continuation frames); `callWithFile` single-value bug fixed; all 6 list algorithms benchmarked and kept in Go (4-9× slower on short lists; all exceed 20% gate — per-element Scheme VM dispatch dominates); `call-with-input-file`/`call-with-output-file` kept in Go (files extension must load without io). Benchmark data in `plans/NATIVE-FORMS-MIGRATION.md`.
- [ ] **User labels/tags to distinguish FS resolvers** Use tags or labels to distinguish bootstrap loadee from include/library loaders in fileResolver.
- [x] **Benchmark** benchmark result of moving primitives to Scheme — measured in Phase 3 of native forms migration. All 6 list algorithms 4-9× slower in Scheme (per-element ForeignFunction dispatch dominates on short lists). Data in `plans/NATIVE-FORMS-MIGRATION.md`.
- [ ] **Disassembler** Implement a disassembler for Wile
- [x] **Reflection primitives** [Runtime]: `procedure-arity`, `procedure-name`, `procedure-source-location`, `procedure-bound-symbols`, `procedure-type` in `registry/core/`.
