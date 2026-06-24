# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/),
and this project adheres to [Semantic Versioning](https://semver.org/).

> **Note on versions.** Only the dated, tagged headings below correspond to
> releases. `VERSION` (and `wile --version`) changes only at release-cut time
> (via `make bump-{patch,minor,major}`) and stays fixed between releases. Its
> current value may sit ahead of the latest release heading below: a per-commit
> auto-bump used until 2026-06-24 left the patch number ahead of the last tag,
> and it is reset to the released version at the next release-cut. A build whose
> `--version` has no matching heading here is an unreleased development build of
> the most recent release shown.

## [Unreleased]

## [1.17.0] - 2026-06-17

### Added

- **`(wile algebra cfl)` — context-free-language reachability.** New algebra
  sub-library: a path over a labeled directed graph "counts" iff its edge-label
  string lies in the language of a context-free grammar — generalizing semiring
  path-algebra (Boolean/tropical reachability) to grammar-constrained
  composition, the basis of context-sensitive (interprocedural, field-sensitive)
  program analysis. Typed production kernels (`cfl-epsilon`/`-terminal`/`-unary`/
  `-binary`), a Reps–Horwitz–Sagiv worklist solver (`cfl-solve` +
  `cfl-reachable?`/`-from`/`-pairs`/`cfl-derives?`), a `dyck-grammar` preset for
  matched-delimiter analysis, and `validate-cfl-grammar`/`-graph`.
- **REPL startup version header.** Entering the interactive REPL now prints
  a `Wile Scheme <version> (<sha>)` header line. Suppressed by `-q`/`--quiet`;
  file and `-e` execution remain header-free so script output stays clean.
- **`,version` REPL meta-command.** Prints the interpreter version and build
  identifier on demand without leaving the session. Available in MCP mode too.

### Changed

- **BREAKING: the public `wile` package moved to `github.com/aalpar/wile/pkg/wile`.**
  The module path (`github.com/aalpar/wile`) and the package name (`wile`) are
  unchanged, so call sites (`wile.NewEngine`, `wile.Engine`, `wile.WithProfile`,
  …) are untouched. Only the import line changes. Migration: replace
  `import "github.com/aalpar/wile"` with `import "github.com/aalpar/wile/pkg/wile"`
  (subpackage imports like `.../values`, `.../security`, `.../extensions/...` are
  unaffected). This empties the module root of loose `.go` files so the
  repository view is scannable. `wile.StdLibFS` is preserved exactly, now
  re-exporting the internal `stdlib` package's embed rather than embedding the
  library tree a second time.
- **`--version` output drops the empty `()` suffix** when no build SHA is
  available (e.g. some `go install` builds): prints `Wile Scheme <version>`
  instead of `Wile Scheme <version> ()`. The `--version` flag, the REPL header,
  and `,version` now share a single `versionString()` formatter.
- **SRFI-18 threads no longer inherit `parameterize` bindings from the spawning
  thread.** A spawned thread now starts from the top-level dynamic environment.
  The previous inheritance was an unsynchronized live read of the parent thread's
  state rather than a snapshot taken at creation, so it was both racy and
  unspecified by SRFI-18; a correct creation-time snapshot may be reintroduced
  later if a use case warrants it. (#772)

### Fixed

- **Data race when one SRFI-18 thread terminates another.** Capturing the
  terminated thread's exception backtrace could read that thread's still-running
  VM state from another goroutine (detectable under the Go race detector). Thread
  contexts are now independent execution roots, so backtrace and dynamic-state
  walks stop at the thread boundary instead of crossing into the concurrently
  running parent. (#772)
- **Algebra solvers: caller-reachable silent non-termination now raises.** Six
  paths that could loop forever with no output and no error now raise a
  remedy-pointing error, extending the cap-guard discipline of
  `(wile algebra dataflow)`/`graph` to the rest of the algebra libraries:
  - `(wile algebra group)` — `subgroup-generated`, `enumerate-finite-group`,
    and `orbit` on an infinite or infinitely-generated group (e.g. the integers
    under `+`). `orbit` gains an optional `(max-size . N)` matching
    `subgroup-generated`; both fall back to a large default cap when none is
    given.
  - `(wile algebra lattice)` — the 3-arg `fixpoint` (Kleene iteration) on an
    infinite-height lattice; the error points at `fixpoint/widen` or the 4-arg
    fuel form.
  - `(wile algebra matching)` — `tropical-assignment` / `kuhn-munkres-square`
    on a cost matrix with no finite perfect assignment (every completion forced
    through a `+inf.0` pair).
  - `(wile algebra combinatorial-graph)` — `enumerate-finite-graph` on a tier-2
    neighbor-fn that describes an infinite graph.
  - `(wile algebra polynomial)` — `poly-divmod` / `poly-gcd` when `F`'s
    reciprocal is not a true inverse (a "field" built over a non-field such as
    the integers); the error points at `validate-field`.
  `(wile algebra matrix)`'s already-guarded `semiring-matrix-closure` error now
  names a cycle-safe-semiring remedy. Caveat: the group/graph element-count caps
  use O(n) membership, so on a genuinely infinite structure the *default* cap
  raises only after ~cap² work; pass an explicit small `(max-size . N)` for a
  fast, precise failure.

## [1.16.0] - 2026-05-19

### Removed

- **13 `Datum()` accessor methods deleted from concrete `*values.X` types.**
  Affected types: `Integer`, `Float`, `Boolean`, `Character`, `Byte`,
  `Complex`, `String`, `Symbol`, `Box`, `ByteVector`, `Vector`, `Pair`,
  `NativeError`. Each method was a pure field accessor with no shared
  interface contract — the shared name was a false signal of
  polymorphism, not a real protocol. Callers migrate to direct field
  access (`v.Value`, `v.Key`) or, for `NativeError`, the
  identical-bodied `Unwrap()` method (standard Go convention). For
  `Vector` / `ByteVector` / `Pair` (slice/array-typed values), callers
  dereference with `*v` or use the existing typed accessors
  (`Length()`, `Car()`, `Cdr()`, `Get(i)`).

  **Breaking** for any external embedder depending on these methods.
  Sanctioned under the v1.x zero-consumers policy
  (`CLAUDE.md` versioning section: "break freely in minor versions
  — no v2 module path ceremony until real users exist"). Surviving
  `Datum()` methods on `*SyntaxObject` and `*SyntaxSymbol`
  (in `internal/syntax/`) are unrelated — they form a separate
  syntax-level protocol and are out of scope.

### Changed

- **FFI `float64` parameter conversion is now precision-aware.** Numeric arguments
  passed across the FFI to Go `float64` parameters are checked for lossless
  representability under the default ("strict") mode:
  - `*BigFloat` is now accepted when the value fits `float64` losslessly;
    previously the FFI rejected it via `werr.ErrTypeConversion`. Lossy
    `*BigFloat` (mantissa beyond 53 bits, or magnitude beyond `float64` range)
    now errors with `werr.ErrLossyConversion`.
  - `*BigInteger` overflow newly errors. Previously the FFI silently truncated
    to `±Inf` via `(*big.Int).Float64()`'s discarded accuracy bit; now returns
    `ErrLossyConversion` with the direction (`Above` / `Below`) named in the
    message.
  - `*Rational` non-representable newly errors. Previously `(/ 1 3)` passed to
    a `float64` parameter silently rounded to `0.333…`; now errors with
    `ErrLossyConversion`.
  - Passing `*Complex` or `*BigComplex` to a Go `float64` parameter now returns
    `ErrLossyConversion` (via the new `!isReal` branch of
    `values.ToFloat64Lossless`) instead of the previous `ErrTypeConversion`.
    Embedders matching on `errors.Is(err, ErrTypeConversion)` to catch
    "complex passed where real expected" should add `errors.Is(err,
    ErrLossyConversion)`.

  Embedders relying on the previous silent-truncation path can recover it via
  the new `WithLossyConversionsAllowed()` engine option.

- **FFI `complex128` parameter conversion is now supported.** Go functions
  taking `complex128` parameters can now be registered. Previously, registration
  failed with `ErrFFIRegistration`. `*Complex` and `*BigComplex` arguments
  convert with per-component precision tracking; under strict mode, any
  component that rounds returns `ErrLossyConversion`. Complex *return* values
  and complex callback parameters remain unsupported (`makeRetConverter` has no
  `complex128` arm).

- **`registry/helpers/value_conv.ToFloat64` tightened.** Previously silently
  truncated `*BigFloat` overflow, `*BigInteger` overflow, and `*Rational` with
  non-representable denominators (e.g., `1/3`). Now errors with
  `werr.ErrLossyConversion` on loss. Same-precision inputs (`*Integer`,
  `*Float`, exact-power-of-2 `*Rational`, etc.) continue to succeed
  unchanged. Migration: callers needing the silent-truncation behavior should
  call `values.ToFloat64WithAccuracy` and discard the accuracy slot. The only
  in-tree caller affected was `(atan y x)`, which now uses the lossy-allowed
  path directly (R7RS §6.2.6 inherently returns inexact, so silent loss is
  load-bearing there).

### Added

- **`wile.WithLossyConversionsAllowed()` engine option** — opt-in flag
  suppressing `ErrLossyConversion` returns from FFI converters. When set, the
  Float64 converter calls `values.ToFloat64WithAccuracy` and discards the
  accuracy / `isReal` flags; the Complex128 converter projects the value slot
  and discards per-component accuracies. Per-engine; the flag is captured at
  `RegisterFunc` time so changes after registration do not affect already-built
  FFI closures.

- **`werr.ErrLossyConversion` sentinel** — new static error distinct from
  `ErrNotAReal` (real-vs-complex domain mismatch) and `ErrTypeConversion`
  (`reflect.Kind` mismatch). Callers can `errors.Is` against it to detect
  precision-loss specifically.

- **`values.ToFloat64WithAccuracy`, `values.ToFloat64Lossless`,
  `values.ToComplex128WithAccuracy`, `values.ToComplex128Lossless`** — public
  helpers surfacing Go's `big.Accuracy` three-valued enum (`big.Below` /
  `big.Exact` / `big.Above`) at the cross-package boundary. `WithAccuracy`
  forms return the raw value plus accuracy slots; `Lossless` forms return
  `ErrLossyConversion` when any component would round. See
  `values/conversion.go`.

- **Four loss-signal-aware Scheme primitives in the math extension** — surface
  Go's `big.Accuracy` to Scheme via `'below` / `'exact` / `'above` symbols.
  R7RS `(exact->inexact)` continues to silently saturate per §6.2.6; these
  primitives **expose** the rounding direction:

  | Primitive | Returns | Purpose |
  |-----------|---------|---------|
  | `inexact-lossless?` | boolean | `#t` iff `(exact->inexact n)` would be lossless. For complex N, both components must be exact. |
  | `inexact-accuracy` | 1 sym (real) or 2 syms (complex) | Predicts accuracy without performing the conversion. |
  | `inexact-with-accuracy` | 2 values (real) or 3 values (complex) | Performs conversion and returns the inexact result with its accuracy. |
  | `complex-inexact-with-accuracy` | always 3 values | Uniform 3-value variant — `(values complex-c real-acc imag-acc)` regardless of input domain. |

  Domain dispatch (real-vs-complex) uses the `values.ComplexNumber` interface,
  matching the `Hashable` / `Tuple` / `Indexable` precedent. Available
  unconditionally when the math extension is loaded (profile `Small` and
  above). See `docs/numeric/tower.md` §"Conversion to Fixed-Precision Go
  Types".

### Fixed

- `syntax-case` now propagates non-`ErrNotAMatch` matcher errors instead of silently translating them to "no matching clause". Context cancellations and malformed-input errors during pattern matching surface with the actual diagnostic instead of the misleading no-match message. (#732)
- `syntax-case` no-match diagnostic now includes the input form being expanded (`syntax-case: no matching clause for input <form>`), where previously the message was the generic `no matching clause`. Macro authors can identify which input fell through without trial-and-error. (#732)
- `MachineContext.syntaxCase` field readers now distinguish "field unset" from "wrong concrete type" with field-specific diagnostics naming which operation should have populated each piece of state, where previously both produced the same misleading `no input available` message. (#732)

## [1.15.0] - 2026-05-04

### Added

- `(srfi 14)` Character-Set Library — 17 SRFI-14 FFI primitives, 23 derived Scheme procedures, 17 named char-sets (`char-set:letter`, `char-set:digit`, `char-set:whitespace`, etc.) sourced from Go's `unicode` tables for full Unicode reach. Inversion-list representation, fully immutable (`!`-suffix forms always allocate fresh per spec permission). Unlocks char-set criteria across 7 SRFI-13 procedures (`string-index`, `string-skip`, `string-count`, `string-trim*`, `string-tokenize`, `string-filter`, `string-delete`). 7 names deferred from spec: `char-set-hash`, cursor protocol (4 names), `char-set-diff+intersection` and `!`. New `(wile charsets)` library exposes `char-set-ranges` for efficient iteration. (#723)
- `(srfi 13)` String Library — 60 SRFI-13 procedures plus the `string-trim-left` alias, all pure Scheme. Predicates (`string-null?`, `string-every`, `string-any`), selection (take/drop family, `string-tabulate`, `substring/shared`), prefix/suffix (predicates, lengths, ci variants), search (`string-index(/-right)`, `string-skip(/-right)`, `string-count`, `string-contains(-ci)`), trim (`string-trim(/-right/-both/-left)`), pad (`string-pad(/-right)`), comparison (12 boolean binary forms with optional `[start1 end1 start2 end2]` slicing + 3-way `string-compare(/-ci)`), reverse (`string-reverse(/!)`), splice/tokenize (`string-replace`, `string-tokenize`, `string-filter`, `string-delete`), concat (`string-concatenate`, `reverse-list->string`), fold/map (`string-fold(/-right)`, `string-for-each-index`, SRFI-13 `string-map`), mutating case (`string-upcase!`, `string-downcase!`). Char-set criteria enabled by SRFI-14; v1 accepted char or predicate criteria only. (#721)
- `(wile strings)` kitchen-sink convenience library — re-exports R7RS comparison forms (`string=?`, `string-ci=?`, `string-upcase`, etc.) alongside the SRFI-13 surface, plus five Wile-specific extras: `string-split` (split on a single delimiter character; distinct from SRFI-13 `string-tokenize`), `string-replace-all` (left-to-right non-overlapping substring replace), `string-byte-length` (UTF-8 byte count), `string-blank?`, `string-repeat`. Resolves the SRFI-13 vs R7RS `string-map` shadowing by importing `(scheme base)` with `(except ... string-map)` and re-exporting the SRFI-13 form. (#721)
- `(wile algebra matrix)` Path D polymorphic API — 19 new primitives that dispatch across dense and sparse representations: `matrix?`, `matrix-ref`, `matrix-rows`, `matrix-cols`, `matrix-shape`, `matrix-semiring`, `matrix-rep-tag`, `matrix-for-each-entry`, `matrix-fold-entries`, `matrix-add`, `matrix-add!`, `matrix-mul`, `matrix-mul!`, `matrix-op-supported?`, `matrix-power`, `matrix-closure`, `matrix-permanent`, `matrix-copy`, `matrix-copy!`. `add` and `mul` are implemented for every rep-pair combination; `power`, `closure`, `permanent` remain dense-only and raise a typed error on sparse input with conversion advice (#684–#691).
- Bang-form semantics follow OQ4 (strict destination-rep) and OQ5 (no-hazard aliasing for `add!`; incremental-write aliasing rejection for `mul!`).
- `matrix-op-supported?` gives callers a programmatic capability query so they can branch on support rather than catching errors.
- `<sparse-semiring-matrix>` records gain an `ssmat-entries-set!` mutator so bang forms can replace the entries alist in place (additive; getter preserved).

### Removed

- `sparse-semiring-matrix-entries` — the alist shape is no longer part of the public API. Iterate via `matrix-for-each-entry` or `matrix-fold-entries` instead. No deprecation period (zero consumers) (#685).

## [1.14.14] - 2026-04-15

### Added

- Implement SRFI-132 sort libraries — 20 procedures for list/vector sorting, merging, deduplication, selection, and median with 142 tests (#655)
- Constant bindings — mark imported bindings with `Imported` and `Constant` flags, reject `set!` on imported bindings per R7RS 5.2 (#651)
- Add `ErrImmutableBinding` sentinel for immutability enforcement (#651)

### Fixed

- Clear `Imported`/`Constant` flags when top-level `define` supersedes import (#651)
- SRFI-132 spec compliance and safety fixes from crosscheck review (#655)

### Documentation

- Reorganize documentation by topic with `INDEX.md` and `TOC.md` (#652)
- Add R5RS/R6RS keyword aliases to procedure docstrings (#653)
- Add SRFI-132 design and implementation plan (#655)

## [1.14.0] - 2026-04-14

### Added

- Add opaque record types for abstract data type support — record types with hidden constructors/accessors for encapsulation (#650)
- Add four algebra library modules: formal concept analysis, pareto dominance, interval arithmetic, and semiring graph algorithms
- Add `sourceload` package for unified fs.FS-based file search with `LoadStack`, `Walk`, and `Finder` (#647)
- Add `PathTracker` interface to environment package for decoupled path management (#647)
- Add algebra library documentation, examples, and API reference (#649)

### Fixed

- Guard `NewRecord` against nil record type (#650)
- Harden opaque record types with validation and doc corrections (#650)
- Resolve Tier 1 security/correctness items (#648)
- Resolve Tier 1 error reporting and source location items (#648)
- Absolutize OS search dirs before passing to sourceload.Finder (#647)

### Refactored

- Extract `machine/compilation/resolver` package — unified library file extension handling and tighter API (#645, #646)
- Extract `machine/compilation/sourceload` package — migrate Namespace to PathTracker interface, wire resolvers to sourceload.Finder, deduplicate isHidden (#647)
- Narrow `setupLibrarySystem` and `newFileResolver` parameters (#648)

## [1.13.21] - 2026-04-12

### Added

- Add theory projections for group, semiring, ring, field, and Heyting algebra in `(wile algebra symbolic)` (#633)
- Add `discover-equivalences` — explore distinct normal forms by running terms through single-axiom sub-theories (#633)
- Add `WithMaxStackSize` engine option for eval stack limits (#636)
- Add SageMath oracle for algebra library validation (#643)

### Fixed

- Widen `sourceRefs` to `uint32`, add `NamedCallable` interface (#634)
- Crosscheck review findings for sage algebra validation (#643)
- Restore `DefaultInlineThreshold` at `ExpandAndCompile` call sites (#642)

### Refactored

- Extract `ExpandAndCompile` and unify expand→compile pipeline (#642)
- Move expansion operations to `machine/compilation` subpackage (#644)
- Complete `RequireArg[T]` migration across primitives (tech debt 5.5) (#638)
- Decouple `repl/` from `machine/` internals (task 8.3) (#639)
- Funnel `prim_eval.go` through `NewSubContext` (tech debt 8.5) (#637)

## [1.13.0] - 2026-04-10

### Added

- Add Heyting and Boolean algebra types — bounded distributive lattices, pseudo-complement, powerset/map constructors, ring bridge projections (#630)
- Add orthogonal algebra types — setoid, category, closure, differential with validation and projections (#631)
- Add symbolic algebra library — theory projections from operational structures, recursive bottom-up normalizer, traced rewriting with human-readable explanations (#632)
- Add absorption and associativity axiom types to rewrite library (#632)

## [1.12.0] - 2026-04-09

### Added

- Add `Keywords` field to primitives for LLM discovery — `apropos` finds operations by alternate names (#626, #627, #628)
- Add extensible type constraints — `TypeConstraint` interface replaces `ValueType` enum for parameter validation (#629)
- Add library export index for unloaded library discovery in `apropos` (#623)

### Fixed

- Unify documentation conventions and output format across special forms and primitives (#620, #621)
- Propagate Keywords through doc-only primitive registration (#622)
- Read library registry dynamically in RegistryDocProvider (#624)
- Search unloaded library names and descriptions in `apropos` (#625)
- Nil guards, stale comment, empty-string edge case (crosscheck findings)

## [1.11.0] - 2026-04-07

### Added

- Add public `repl/` package — Engine-centric REPL with completer, meta-command handler, debug support, and doc provider for embedders (#617)
- Add public `docparse/` package — structured metadata parsing for Guile-style docstrings
- Add `Engine.SetDebugger` for session-scoped debugging
- Add `Engine.ReadExpression` for reader-based single-expression parsing (no EOF required)
- Add `IsIncompleteInput` helper for REPL multi-line input detection

### Fixed

- Use live registry for doc provider — documentation now reflects runtime state including imported libraries (#619)
- Show categories for special forms and macros in `,apropos` (#618)
- Register docstrings from imported libraries at import time (#616)

### Refactored

- Migrate `internal/repl` to public `repl/` package, delete `internal/repl`
- Promote `docparse` from internal to public package

## [1.10.0] - 2026-04-01

### Added

- Add bytecode disassembler (#603)
- Add escape analysis for let-bound closures (#604)
- Add inline let-bound lambda calls as synthetic let forms (#605)
- Add unified scoped binding API with multi-slot keys to environment package (#607)
- Add structured docstring metadata for Scheme-defined procedures and stdlib libraries (#613, #614)
- Add `CallContext` interface for extension decoupling from machine internals (#610)
- Add opcode metadata consolidation with `OperandKind` enum (#612)

### Fixed

- Fix cross-group and nested ellipsis in syntax-rules expansion (#606)
- Fix syntax-case pattern binding and compilation coverage (#608)
- Fix recursive let* binding
- Eliminate duplicate primitives in topic/apropos listings (#615)

### Refactored

- Tech debt April 2026: Phases 1-7 — machine decomposition, typed Namespace fields, compilation subpackage, expander conventions, PullDrain optimization, winding stack inheritance, FileResolver helpers (#592-#600, #609)
- Thread outcome sum type — eliminate impossible states (#611)

## [1.10.3] - 2026-03-29

### Added

- Add `procedure-documentation` primitive — Guile-style docstrings for all procedures (#579, #581)
- Add `apropos`, `doc-topics`, `doc-topic` for documentation search and browsing (#585)
- Add library-level documentation with `(description)` clause (#586)
- Add `(available-libraries)` primitive and `Engine.AvailableLibraries` API (#590)
- Add extension API contract system with contract validator infrastructure (#577, #578)
- Add Wile MCP server with `eval`, `doc`, `apropos`, `libraries`, `reset` tools and prompts (#588)
- Add examples to all primitive and library docstrings (#589)
- Filter examples from `,doc` output by default (#591)

### Refactored

- Move docstring detection from compiler to validator (#584)
- Extract body annotation passes from machine package (#580)
- Add Guile-style docstrings to bootstrap procedures, stdlib libraries, and CxR accessors (#582, #583, #587)

## [1.9.11] - 2026-03-25

### Added

- Add `(wile algebra rewrite)` term rewriting library (#576)
- Add capture analysis for let bindings (#575)
- Add `StdLibFS` — exported embedded standard library filesystem for embedders
- Add embedded stdlib with `fs.Sub`-based path resolution, CLI migration to public Engine API, optimizer double-restore fix (#573)

### Changed

- Wire process extension into `AllExtensions` and bootstrap (#574)

## [1.9.5] - 2026-03-25

### Added

- Add `(wile algebra)` composable algebraic structures library (#572)
- Make `let`, `let*`, `letrec`, `letrec*` core compiled forms with `ValidatedLet`/`LetKind` enum (#570)
- Add opaque value system for Go object wrapping (#566)
- Add OS primitives — directory operations and process extension (#565)
- Add full-pipeline degenerate form tests (#571)

### Fixed

- Harden I/O port resolution, error wrapping, and load path handling (#568)
- Reject malformed bindings in expander (#570)
- Predeclare defines from begin blocks in let bodies (#570)
- Address crosscheck findings across codebase (#567)

### Performance

- Replace `sync.Pool` with `FreeList` for env frames — -14% geo mean (#563)
- Reduce GC pressure via FreeList migration and pre-sized bindings — -8.9% geo mean (#563)

## [1.9.0] - 2026-03-22

### Changed (Breaking)

- Remove `NoCopyApply` engine option to prevent SRFI-18 thread data races — argument copying is now always performed (#561)
- Remove dead old compilation path (pre-template bytecode compiler) (#558)

### Changed

- Modernize atomic counters from `atomic.AddUint64`/`atomic.LoadUint64` to `atomic.Uint64`/`atomic.Uint32` (#562)

## [1.8.0] - 2026-03-22

### Added

- Add `Expression` type — opaque wrapper for parsed Scheme expressions, enforces single-expression constraint at parse time (#555)
- Add `Engine.Parse(ctx, code)` and `ParseWithSource(ctx, code, source)` for creating `*Expression` values (#555)
- Add `Engine.MustParse(ctx, code)` and `MustParseWithSource` for test/example convenience (#555)

### Changed (Breaking)

- Single-expression APIs (`Eval`, `Compile`, `EvalIn`) now accept `*Expression` instead of `string` (#555)
- Remove `EvalWithSource` and `CompileWithSource` — source context lives on `*Expression` via `ParseWithSource` (#555)

## [1.7.2] - 2026-03-21

### Added

- Add `ChainFileResolver` for multi-layer source loading — searches multiple `FileResolver` instances in order, falling through on file-not-found while propagating security denials and I/O errors immediately (#554)
- Add `WithSourceOS()` engine option to explicitly include the OS filesystem in the resolver chain (#554)

### Changed

- `WithSourceFS(fsys)` is now additive — multiple calls build a resolver chain searched in call order. Previously it was exclusive (replaced OS filesystem entirely). To get the old exclusive behavior, use `WithSourceFS(fsys)` without `WithSourceOS()` (#554)
- `WithSourceFS(nil)` now panics eagerly at option creation time with a clear message (#554)
- Internal: replace `sourceFS fs.FS` config field with `resolverFactories []resolverFactory` — each `WithSourceFS`/`WithSourceOS` call appends a factory closure (#554)

## [1.7.1] - 2026-03-20

### Added

- Add `WithSourceFS(fs.FS)` engine option — route all source loading (include, load, library import) through a virtual filesystem (#553)
- Add `FSFileResolver` for virtual filesystem source resolution with load-path-stack, search paths, and FS root fallback (#553)

### Changed

- `LoadLibrary` routes through `FileResolver` interface instead of calling `os.Open` directly (#553)
- `LoadPathStack` accepts relative paths (relaxed from absolute-only) (#553)

## [1.7.0] - 2026-03-19

### Added

- Add namespace system — `Namespace` type owns syntax interning, phases, library registry, primitive registry, authorizer, and module instances (#544)
- Add `NewNamespace(ctx, opts...)`, `WithNamespace(ns)`, `Engine.EvalIn(ctx, code, ns)` for namespace management (#544)
- Add Scheme namespace API — 10 primitives for runtime namespace creation and manipulation (#544)
- Add Tier 1 Racket primitives and `(wile control)` library (#547)
- Add marks-based `parameterize` for composable continuation correctness (#542)

### Changed

- Rename `TopLevelEnvironment` to `Namespace`; move registry and authorizer from Engine to Namespace (#544)
- `extractPort` returns `(T, Tuple, bool, error)` instead of taking a thunk (#543)
- Eliminate `skipCore` flag from `engineConfig` (#552)
- Add `RuntimeError` constructors (#551)
- Extract Go static analysis extensions to [wile-goast](https://github.com/aalpar/wile-goast) — removes `golang.org/x/tools` dependency (#492)

### Fixed

- Fix marks-based `parameterize` — `isolatedMarks` flag prevents stale parent marks in `call/cc` escape sub-contexts (#542)

### Refactored

- Tech debt sweep — remove string interning, add bounds checks and tests (#529)
- Fix `readNan` fall-through and separate match from error in tokenizer (#530)
- Comprehensive doc fixes across 18+ files (#531-#540)

## [1.6.1] - 2026-03-12

### Fixed

- Fix release pipeline — move kanren benchmark out of example test path to avoid 30s timeout on CI runners (#486)

## [1.6.0] - 2026-03-12

### Added

- Add `(wile goast lint)` extension — Scheme-programmable Go static analysis with pattern-based AST linting rules (#484)
- Add `(wile goast ssa)` and `(wile goast cfg)` extensions — SSA-form and control-flow-graph analysis for Go packages (#478)
- Add `go-typecheck-package` with type and package annotations on Go AST nodes (#477)
- Add Go AST mapping for 13 node types: `GoStmt`, `DeferStmt`, `SendStmt`, `LabeledStmt`, `SwitchStmt`, `TypeSwitchStmt`, `CaseClause`, `SelectStmt`, `CommClause`, `TypeAssertExpr`, `SliceExpr`, `ChanType`, `Ellipsis` (#480)
- Add Go AST support for `BadExpr`, `BadStmt`, `BadDecl`, `IndexListExpr`, and full comment round-trip preservation (#481)

### Changed

- Migrate 28 CxR accessors (`caar`, `cddr`, etc.), 9 type predicates, and 6 higher-order functions (`map`, `for-each`, `string-for-each`, `vector-map`, `vector-for-each`, `string-map`) from Go to Scheme — enables correct `call/cc` behavior through these procedures (#456, #460, #462)
- Move exception primitives from extension to core for availability in all engine configurations (#461)
- Wrap bare sentinel panics and error returns with call-site context for improved error diagnostics (#470)

### Fixed

- Fix Go AST file-level comment rebuild — standalone comments survive round-trip (#482)
- Fix Go AST `ChanDir` handling — panic on unknown direction replaced with error return
- Harden error handling in Go AST comment rebuild and consolidate tests (#483)

### Performance

- Opcode fusion and primitive promotion — fused opcodes for common instruction sequences, hot primitives promoted to inline VM dispatch (#476)

## [1.5.0] - 2026-03-05

### Added

- Add security package — `security.Authorizer` interface with K8s-style Resource/Action vocabulary, `security.Check()` gating, and four built-in authorizers (`DenyAll`, `ReadOnly`, `FilesystemRoot`, `AllowAll`); wire into engine via `WithAuthorizer()` option
- Gate privileged primitives (`eval`, `load`, filesystem, system) with `security.Check` for fine-grained runtime authorization
- Add sandboxing convenience API — `SafeExtensions()` provides a zero-config safe sandbox excluding filesystem, eval, system, and thread extensions
- Add REPL enhancements — tab completion (`SchemeCompleter`), primitive documentation (`DocProvider` + `,doc` meta-command), `,help`/`,edit` meta-commands, pager for long output
- Add five reflection primitives — `procedure-name`, `procedure-arity`, `procedure-source`, `procedure-formals`, `procedure-body` (#427)
- Add environment introspection primitives and extract to safe extension (`environment-bindings`, `environment-bound-names`, `environment-parent`)
- Add `values.Callable` interface for type-safe procedure handling across closures, foreign functions, continuations, and parameter objects
- Add `-e`/`--eval` CLI flag for command-line expression evaluation (#374)
- Add `Pool[T]` generic pool and environment frame pooling (#325)
- Thread source context into binding creation for improved error locations (#324)
- Add `MaxCallDepth` (default 10000) for embedded safety — prevents runaway recursion from exhausting Go stack (#P3)
- Add `SyntaxWalk` convenience wrapper in `internal/syntax`
- Add registry filtering, library name in factory, and import observer for sandbox enforcement
- Add peephole optimization pass for compiled bytecode — dead-code elimination, fused push/call opcodes, `LoadVoid` removal
- Add `ForeignClosure` type for Go callbacks with proper bytecode-path recursion support
- Add Scheme-domain test suites — strings, characters, ports, numbers, exceptions, lazy evaluation, records, eval, control flow, and macros
- Add shebang support (`#!/usr/bin/env scheme`) and R7RS `(command-line)` argument access (#321)

### Changed

- Rename `AddSearchPath` to unified include/library path resolution API (#426)
- Extract generic `OptionalArg[T]` for typed fill extraction, replacing ad-hoc optional parameter parsing (#425)
- Extract generic port extraction helper, eliminating per-primitive port boilerplate (#424)
- Unexport `NewEnvironmentFrame` — use `Pool[T]` allocation instead (#422)
- Collapse match constructor telescopes into opts pattern, reducing `SyntaxMatcher` constructor complexity (#420)
- Consolidate port `SchemeString` into `portBase`, eliminating per-port-type formatting duplication (#419)
- Unify escape mechanisms under `ErrPromptAbort` — removes three redundant sentinel errors (#418)
- Split `library.go` into bindings and registry files for focused concerns (#417)
- Split `parser.go` into 4 concern-specific files (#404)
- Split `compile_time_continuation_library.go` into 4 concern-specific files (#400)
- Consolidate opcode metadata into single table — eliminates scattered switch statements (#384)
- Eliminate hand-unrolled type dispatch in `PrimExpt` and `PrimMakeRectangular` (#382)
- Eliminate `BigComplex` `*Parts` type-switch duplication (#378)
- Extract error infrastructure into `werr/` package — separates error types from value types (#379)
- VM binding helpers, error sentinels, and literal dedup O(1) (#377)
- Consolidate test helpers into `registry/testhelpers` (#372)
- Unify expander callable dispatch (#370)
- Remove `ArrayList` type — convert all `NewCons` loops to block-allocated `List()` (#316)
- Convert numeric dispatch to table-driven with same-type hot paths (#317)
- Structural consolidation of compiler and expander internals (#319)
- Table-drive `char-ci` and `string-ci` comparison primitives
- Tighten `SyntaxSymbol.ResolvedBinding` from `any` to `ResolvedRef`
- Remove `context.Context` from `ForeignFunction` signature — context propagated via VM, not per-call
- Generate `Subtract`/`Multiply`/`Divide`/`LessThan`/`Compare` dispatch tables
- Migrate `*wile.Error` to sentinel+wrap pattern (#320)
- Change `vmState.callDepth` from `uint64` to `int`
- Unexport `NewBoolean` — use `BoolToBoolean` instead
- Remove 17 unused sentinel errors (#410)
- Remove 8 write-only struct fields across tokenizer, match, machine (#411)
- Remove dead `Indexable` interface (#413)
- Unexport `Promise.Thunk`/`.Result`, add accessor methods (#412)

### Fixed

- Fix R7RS `syntax-rules` vector pattern matching — vector subpatterns with ellipsis now track element positions correctly
- Fix R7RS `guard` re-raise dynamic extent — guard clauses now re-raise in the correct dynamic environment per R7RS §4.2.7
- Fix `eval` multi-value propagation — `values` form results now propagate through `eval` correctly
- Fix R7RS conformance across numeric tower, parser, I/O, display, and predicates (multiple passes)
- Fix `BigFloat` Inf/NaN handling — `BigFloat` is now Inf/NaN-capable; rounding primitives (`floor`, `ceiling`, `truncate`, `round`) handle `BigFloat` correctly
- Fix `BigComplex` predicates and preserve imaginary part when `Float(Inf/NaN)` operates with `BigComplex`
- Fix `Complex.HashCode` crash on NaN/±Inf components
- Fix `NaN` guard in `Float.EqualTo` for IEEE 754 compliance
- Fix `BigFloat.SchemeString` — integer-valued BigFloats now append `.0` for correct inexact representation
- Fix circular structure crash in compiler and pair display
- Fix `apply` under-arity now caught at compile time
- Fix shared acyclic datum labels — `deduplicatePair` memoized, `internSymbolsInValue` handles shared structures
- Fix `guard` body not propagating multiple values (#395)
- Fix `ForEach` nil guards — return `EmptyList` instead of `Void` for proper list termination
- Fix `FilesystemRoot.Authorize` — preserve root cause error through path resolution
- Fix import observer not firing during expand-phase imports (#398)
- Fix `PopContinuation` underflow — convert panic to error return with distinct sentinel
- Fix winding stack update — `unwindStackTo` now updates incrementally instead of slice aliasing
- Fix `LoadPathStack` not populated from CLI file loading
- Fix `call/cc` escapes not handled in `Engine.Call()` API (#328)
- Fix recursive foreign closure dispatch — restore bytecode path (#335)
- Fix arity errors not catchable by Scheme exception handlers (#339)
- Fix `import-set` processing — deduplicate loop, use map-based name filtering (#406, #407, #416)
- Fix `buildPCRemap` — dead positions now map forward correctly (#304)
- Fix `ToFloat64` — now covers the full real numeric tower
- Fix double-printed error in `WrapForeignFileError` (#408)
- Fix `SelectCase` — replace bool pair with `SelectCaseKind` enum for clarity (#415)
- Fix defensive copy from `LocalEnvironmentFrame.Keys()` (#414)
- Eliminate all compound if-assignments per style guide (#403)

### Performance

- Compile `apply` as special form for proper tail recursion — `apply` in tail position now reuses the current frame (#H1)
- Block-allocate pairs via `PairBlock` and add fused push/call opcodes (#311)
- Add fused `PushLiteral`, `PushGlobal`, `PushLocal` opcodes — eliminate separate push+load instruction pairs (#308)
- Add fused push/call opcodes and promote `MakeClosure` to inlined op (#309)
- Inline continuation evals + direct-call opcode for primitives (#387)
- `Stack.Drain` eliminates `PopAll` allocation in VM hot path (#396)
- Reuse rest-arg buffer for foreign variadic calls (#333)
- Move compile-time fields behind `BindingMeta` pointer — reduces per-binding runtime size (#314)
- Replace field-by-field binding copy with `copy()` (#313)
- Fix escape analysis artifacts in numeric fold helpers (#312)
- Dead-`LoadVoid` elimination for all value-register writers (#307)
- 2-argument fast path for numeric helpers
- Enable `noCopyApply` for foreign closures
- Environment frame pooling and number error returns (#386)

## [1.4.0] - 2026-02-20

### Added

- Unify `call/cc` via composable-continuation-then-abort model — adds escape continuations, continuation barriers, and `call-with-composable-continuation`; full continuations now compose correctly across barrier boundaries (#293)
- Expose extensions as importable R7RS libraries — extensions register as `(wile <name>)` libraries, loadable via `(import (wile regex))` etc. (#297 follow-up)
- Add `WithLibraryPaths` engine option to enable R7RS library system with configurable search paths
- Add `Engine.RegisterFuncs` for batch registration of Go functions (map-based variant of `RegisterFunc`)
- Add stage-isolated benchmarks for VM and environment subsystems
- Add coverage tests for math, exceptions, eval, and CLI (#296)

### Changed

- Split VM value register into single-value fast path (`singleValue values.Value`) and multi-value slow path (`multiValues MultipleValues`), eliminating a `[]values.Value{v}` heap allocation on every bytecode instruction — reduces allocations by ~20% and wall time by ~8% on call-heavy workloads
- Split `Operation` into base `Operation` and `InlinedOperation` interfaces — inlined ops carry their operand directly, simplifying dispatch (#292)
- Move 6 Tier 1 extensions from `internal/extensions/` to public `extensions/` package for direct embedder access
- Remove `ApplyContext` interface — `InitFunc` now takes `*registry.Registry` directly, simplifying extension authoring
- Add `Registry.AddGlobalValue` for registering non-function global values; eliminate `ApplyContext.Environment()` usage
- Move bool predicates (`IsTrue`, `IsFalse`, `BoolToBoolean`) from `internal/schemeutil` to `values/`
- Move `SchemeEquals` to `valuestest/` package, externalize `values/` tests (#297)
- Extract `validatedBase` helper and `SourceContext.Clone` method (#295)
- Move `LibraryEnvFactory` from package global to `TopLevelEnvironment` field (#282)
- Migrate `SyntaxEmptyList` to pointer singleton (#278)
- Use `IsEmptyList` in `PrimListQ` instead of direct `== EmptyList` (#279)
- Consolidate benchmarks into table-driven Eval/Run format
- Enable `modernize` linter and apply all fixes

### Fixed

- Fix winding stack not inherited by sub-contexts in `PrimApply`, `PrimCallWithValues`, and `applyParameter` — `dynamic-wind` before/after thunks now execute correctly through apply chains (#294)
- Fix `callDepth` `uint64` underflow — derive depth from parent pointer instead of decrementing, preventing wrap-around panic in deeply nested contexts
- Fix multi-extension primitive leakage — extensions loaded after engine creation no longer pollute earlier engines' environments
- Fix evals stack leak in `ReleaseSubContext` — pooled sub-contexts now properly clear the eval stack
- Fix version display falling back to `(unknown)` when ldflags are absent — now reads version from `debug.ReadBuildInfo`

### Performance

- Compile-time escape analysis to skip `CopyForApply` — the compiler marks non-escaping closures, avoiding unnecessary environment copies in the common case (#291)
- Shared-flag continuation optimization for `call/cc` path — continuation capture skips deep-copy when no mutation has occurred since the last capture (#290)
- Embed `LocalEnvironmentFrame` by value in `EnvironmentFrame`, reducing pointer indirection and GC pressure (#289)
- Retain stack backing array across `PopAll` cycles instead of reallocating (#288)
- Change `LocalEnvironmentFrame.bindings` from `[]*Binding` to `[]Binding`, eliminating per-binding heap allocations (#287)
- Eliminate `*LocalIndex` heap allocation in VM hot path (#286)
- Migrate 8 zero-operand ops to switch dispatch, eliminating interface method call overhead (Phase 6) (#284, #285)
- Compiler optimizations: ops slice preallocation, peephole optimization, constant folding (Phase 5) (#283)
- Pool `MachineContext` for macro expansion call sites (#281)
- Structural sharing in syntax tree scope propagation (Phase 4.1–4.2) — `AddScope`/`WithScope` now return the receiver unchanged when the scope is already present (#280)
- Environment copy-on-write for `Apply` hot path (Phase 3) (#274)
- Copy-on-write for environment frame keys and shallow binding copies (#271)
- Continuation frame pooling via `sync.Pool` (Phase 2) (#270)
- `sync.Pool` for `Stack` and `MachineContext` sub-contexts
- Cache ASCII characters (0–127) to avoid allocation in `NewCharacter`
- Cache `callDepth` and ellipsis tail count for O(1) access instead of chain traversal
- Batch `ctx.Done()` check every 1024 ops in VM loop instead of every instruction
- Eliminate `PopAll` clone by swapping backing array ownership

## [1.3.0] - 2026-02-14

### Added

- Add load path stack for relative file resolution — `(load "helper.scm")` now resolves relative to the file containing the `load` call, not the working directory; nested loads resolve correctly through a per-VM LIFO path stack
- Add new primitives: `(current-load-path)`, `(current-load-directory)`, `(current-load-depth)` for inspecting the load stack at runtime
- Add 73 examples across 12 categories (basics, numeric tower, macros, control flow, data structures, I/O, concurrency, applications, logic programming, embedding, benchmarks)
- Add Gabriel benchmark suite with 21 benchmarks (tak, takl, ctak, cpstak, fib, triangl, sum, sumfp, sumloop, diviter, divrec, deriv, destruct, browse, ackermann, sieve, nqueens, primes, peval, puzzle, puzzle-debug) comparable across Scheme implementations
- Add Schelog logic programming system (Prolog-style relational programming in Scheme)
- Add benchmark infrastructure: `make bench-gabriel` (canonical), `make bench-gabriel-all` (all benchmarks), `make bench-gabriel-compare` (cross-implementation comparison)
- Add R6RS compatibility shim (`examples/lib/r6rs-compat.scm`) for `error` procedure signature differences — accepts both R6RS `(error who message ...)` and R7RS `(error message ...)` forms
- Create convenience symlink `dist/wile` → `dist/{os}/{arch}/wile` during build for easier manual invocation (Makefile targets use explicit platform paths)
- Add Apache 2.0 NOTICE file
- Add CLI package subprocess tests (coverage 9.8% → 75%)

### Changed

- Enforce two-layer error convention (sentinel + wrap) across ~80 call sites — all production errors now wrap a sentinel for programmatic matching via `errors.Is`/`errors.As`
- Unexport `NewForeignError` — callers must use `WrapForeignErrorf` with a sentinel; enforced by ruleguard lint
- Convert 14 `panic` sites to return sentinel errors, improving error recovery in embedding scenarios
- Convert read-only `*Pair` call sites to `Tuple` interface across import set parsing and helpers
- Consolidate `[start [end]]` optional position parsing into `helpers.ParseSubrange`
- Centralize parser/tokenizer cache eviction into `evictPortCache()`
- Embed `OperationBase` in all 34 VM operation types — default `String`/`IsVoid` provided by base struct; `EqualTo` uses generic helpers (`sameType`, `fieldMatches`)
- Make pattern compiler and analyzer work directly with `syntax.SyntaxValue`, eliminating `ConstructPatternTree` and `fromPatternValue` conversion layer
- Consolidate 6 `SyntaxMatcher.Expand*` methods into single `Expand(template, ExpandOptions)` with options struct
- Consolidate tokenizer number parsing — extract `readOptionalDecimalPart`, delete `scanForImaginaryNumberSpecials`, extract `signedState` helper, unify string/extended-symbol scanning via `readDelimited`
- Deduplicate unwind logic between `UnwindTo` and `RestoreWithWindingFrom`
- Deduplicate validator prologues with `formPrologue` helper
- Deduplicate port guard-and-delegate methods
- Split `compile_time_continuation.go` by domain into focused files
- Consolidate match bytecode instructions by category
- Extract `ParseOptionalArg` helper for `make-*` fill parameter extraction
- Standardize empty list handling on check-first pattern
- Extract `ValidateByteValue` helper for byte range checks with `ErrNotAByte` sentinel

### Fixed

- Fix `set!` in hygienic macros using name-based lookup instead of scope-aware lookup, causing incorrect variable binding in macro-generated code (M1)
- Fix winding stack slice aliasing — cap-limited slices now prevent `dynamic-wind` before/after thunks from sharing backing arrays between contexts (M2)
- Fix exception handlers not inherited by sub-contexts, violating R7RS dynamic extent semantics for `with-exception-handler` (M3)
- Fix `SyntaxVector.AddScope` not propagating scopes to vector elements, causing macro hygiene failures on vector patterns (M4)
- Fix `BigInteger.Compare` precision loss when comparing against `Float` — now promotes to `BigFloat` instead of truncating to `float64` (M5)
- Fix string interning mutation bug — interned strings are now marked immutable; `string-set!` copies-on-write to prevent aliased mutation (M6)
- Fix goroutine leak in `ConditionVariable.Wait` with timeout — the wait goroutine now properly exits when the condition is signaled before timeout (M7)
- Fix dead code in `parseComplex` sign validation (M8)
- Fix `string-ci=?`, `string-ci<?`, `char-ci=?`, and related predicates to use Unicode case folding instead of simple lowercasing per R7RS §6.7 (M9)
- Fix `read-string` and `read-bytevector` unbounded allocation vulnerability — added 100 MB per-call allocation limit (M10)
- Fix `read-bytevector` and `read-bytevector!` dropping partial reads at EOF instead of returning available bytes per R7RS §6.13.3 (M11)
- Fix `string->utf8` using byte indices instead of character indices for start/end parameters (R7RS §6.9 specifies character positions)
- Fix cross-goroutine `MachineContext` access in thread creation — thread-start! now deep-copies required state (T4)
- Fix `nextScopeID` counter not being atomic, causing potential data races under concurrent macro expansion (T5)
- Fix `with-input-from-file` and `with-output-to-file` thread safety by converting from primitives to macros wrapping `call-with-*-file` (T3)
- Fix `context.Context` not propagated through `call/cc` restoration, `thread-start!`, and tail-call frames — cancellation now reaches all VM execution paths
- Fix `eval` and `load` not inheriting thread identity from parent context
- Fix `ChannelSelect` separating `recover()` assignment from condition check (Go spec requires same expression)
- Fix reachable panic in quasiquote expansion when quasiquoted improper lists contain unquotes (e.g., `` `(a ,b . c) ``)
- Fix cross-type numeric hash consistency — `Integer`, `BigInteger`, and `Rational` now share canonical exact hashes; `Float` and `BigFloat` share canonical inexact hashes (restores `Hashable` contract: `a.EqualTo(b)` implies `a.HashCode() == b.HashCode()`)
- Fix broken output in `examples/concurrency/mutex.scm` (used `#\newline` character literals instead of printing newlines)
- Fix compilation error in `examples/data-structures/association-lists.scm` (undefined `sort` procedure; added insertion sort implementation)
- Fix compilation error in `examples/data-structures/vectors.scm` (undefined `sort` procedure; added insertion sort implementation)
- Fix compilation error in `examples/macros/simple-macros.scm` (`else` not listed as literal in user-defined `cond-with-arrow` macro)
- Fix `examples/basics/higher-order.scm` using undefined `filter` (not in R7RS-small; added local definition)
- Fix `examples/io/file-io.scm` using `with-exception-handler` where `guard` is needed (handler returned from non-continuable exception)
- Fix `string-append` with zero arguments returning an immutable string instead of a mutable one
- Fix `ValidateByteValue` error messages losing argument-role context after helper extraction

## [1.2.0] - 2026-02-11

### Added

- Add `NewRational`, `NewComplex`, `NewVector` value constructors to public embedding API
- Add Scheme-level test infrastructure (`scheme-test` executable built from `tests/run-tests.scm`)

### Changed

- Rename `CreateLocalBinding` to `EnsureLocalBinding` on `EnvironmentFrame` and `LocalEnvironmentFrame` — the method has get-or-create semantics (returns existing binding if key exists), and the new name reflects actual behavior (breaking API change for embedders)

### Fixed

- Fix `(list? syntax-obj)` returning `#t` instead of `#f` for syntax objects (R7RS: syntax objects are not lists)
- Fix `EqualTo` comparison for empty syntax lists (previously compared unequal to themselves)
- Fix `ArrayList.ForEach` violating `Tuple` interface contract (previously mutated during iteration)
- Fix 32 missing R7RS library exports across `scheme/char`, `scheme/complex`, `scheme/inexact`, and `scheme/lazy`
- Fix bytevector parser and `NewByteVectorFromIntegers` accepting out-of-range integers (now rejects values outside 0-255)
- Fix flaky `TestMutexAbandoned` test by replacing timing-based synchronization with polling loop

## [1.1.0] - 2026-02-08

### Added

- Enforce cross-thread continuation rejection: continuations captured in one thread now raise `ErrCrossThreadContinuation` when invoked from a different thread, preventing VM state corruption
- Run `dynamic-wind` after-thunks on thread termination (both normal exit and `thread-terminate!`)
- Mark owned mutexes as abandoned when a thread terminates, unblocking waiters per SRFI-18 semantics
- Add thread identity to `MachineContext` so `current-thread` returns the actual thread object inside spawned threads (previously always returned `'primordial`)
- Add `CompilationError` and `RuntimeError` structured error types with `Unwrap()` support for programmatic error handling via `errors.As`
- Add `RuntimeError.Condition` field carrying the Scheme raised value when errors originate from `raise`/`raise-continuable`
- Add 16 value-inspection helpers for embedding: `IsList`, `IsPair`, `IsVector`, `IsSymbol`, `Car`, `Cdr`, `ToSlice`, `ToGoInt`, `ToGoFloat`, `ToGoString`, `ToGoBool`, `ToGoBytes`, `ListLength`, `VectorRef`, `VectorLength`, `VectorToSlice`
- Add context cancellation support: engine operations respect `context.Context` through VM execution and macro expansion
- Add VM performance counters to `MachineContext` for runtime introspection
- Add `ComplexNumber` sub-interface with `RealPart()`, `ImagPart()`, `IsReal()` for interface-based complex number dispatch
- Add `RealNumber` sub-interface with `IsPositive()`, `IsNegative()`, `Sign()` for interface-based real number dispatch
- Add `Abs()`, `ToExact()`, `ToInexact()` methods to the `Number` interface
- Add `IsInteger()`, `IsRational()`, `IsFinite()`, `IsNaN()` predicate methods to the `Number` interface
- Add `EvalWithSource`, `EvalMultipleWithSource`, and `CompileWithSource` methods for source-tracked evaluation — source locations appear in `RuntimeError.Source` and `RuntimeError.StackTrace`
- Add `RegisterFunc` for registering Go functions with natural signatures — supports `int64`, `int`, `float64`, `string`, `bool`, `[]byte`, `Value`, `context.Context`, variadic parameters, and `(T, error)` returns
- Extend `RegisterFunc` with composite type support: `[]T` ↔ Scheme lists, `map[K]V` ↔ hashtables, structs ↔ alists, and `func(...)` callback parameters accepting Scheme procedures (including `make-parameter` objects)
- Add `ErrTypeConversion` sentinel for FFI runtime type mismatch errors
- Wire `ErrExceptionEscape` to carry source location and stack trace from per-operation source tracking

### Changed

- Optimize scope matching hot paths: add size guard early return in `ScopesMatch`, cache `Scopes()` calls in `GetBindingWithScopes`, and add perfect-match early termination in `GetLocalIndexWithScopes`

- Centralize ~190 type assertion sites into `RequireArg[T]` and `RequireType[T]` generic helpers, reducing boilerplate across 22 primitive files
- `Engine.Call` now dispatches case-lambda, parameter objects, and composable continuations in addition to plain closures
- `EmptyList` is now a dedicated singleton type (not `*Pair`), enforcing `(pair? '()) → #f` at the type level
- `String` implements the `Indexable` interface with `Length()`, `Get()`, `Set()` methods

### Removed

- Remove unused `Tower*` dispatch functions from the numeric tower

### Fixed

- Fix void-returning primitives (`display`, `newline`, `vector-set!`, etc.) silently dropping argument slots when used as function arguments
- Fix `CurrentSource()` not walking the continuation chain when the current template has no source info
- Fix `CaptureStackTrace` using wrong PC for continuation frames (return addresses pointed past the call site)
- Fix `(pair? '())` returning `#t` instead of `#f` (R7RS §6.4: the empty list is not a pair)
- Fix `Engine.Call` and `runCompiled` leaking internal `ErrMachineHalt` sentinel to callers
- Fix parameter converter errors returned without context (now wrapped with "parameter: converter error")
- Use `errors.Is` for all sentinel error comparisons (`io.EOF`, `ErrMachineHalt`) to handle wrapped errors correctly
- Fix empty list `()` in expression position causing "empty application in call form" compiler error (R7RS §4.1.2)
- Fix `(exact-integer? 1+0i)` returning `#f` instead of `#t` for exact complex with zero imaginary part (R7RS §6.2.6)
- Fix `(rational? z)` returning `#f` for real `BigComplex` values
- Fix `(integer? z)` for large inexact floats outside int64 range
- Fix `RegisterFunc` silently producing empty slices/structs when a non-list value is passed where a proper list is expected
- Fix `RegisterFunc` panicking on named scalar types (e.g., `type MyInt int64`) due to `reflect.Call` type mismatch

## [1.0.4] - 2026-02-05

### Removed

- Remove unused `*Same` methods from numeric types (dead code from pre-direct-dispatch architecture)

## [1.0.3] - 2026-02-05

### Fixed

- Use SPDX canonical Apache-2.0 license text for pkg.go.dev license detection

## [1.0.2] - 2026-02-05

### Fixed

- Fix asymmetric precision loss in mixed BigFloat/Complex arithmetic (`BigFloat + Complex` now returns `BigComplex` to preserve arbitrary precision, matching `Complex + BigFloat` behavior)

## [1.0.1] - 2026-02-04

### Added

- Automated release builds with prebuilt binaries for darwin/linux on arm64/amd64

## [1.0.0] - 2026-02-04

### Added

- R7RS-small Scheme interpreter with bytecode compiler and stack-based virtual machine
- Hygienic macros via `syntax-rules` using the sets-of-scopes model (Flatt 2016)
- First-class syntax objects preserving source location and scope information
- First-class continuations with `call/cc` and `dynamic-wind`
- Delimited continuations with prompts and composable capture
- Proper tail-call optimization
- R7RS standard libraries: `scheme/base`, `scheme/char`, `scheme/complex`, `scheme/cxr`, `scheme/eval`, `scheme/file`, `scheme/inexact`, `scheme/lazy`, `scheme/load`, `scheme/read`, `scheme/write`, `scheme/repl`, `scheme/process-context`, `scheme/time`, `scheme/case-lambda`, `scheme/r5rs`
- Full numeric tower: integers, rationals, floats, complex numbers, with exact/inexact distinction
- Arbitrary precision integers (`BigInteger`) with automatic overflow promotion
- R7RS §7.1.1 inexact digit placeholders (`1.2###`) in numeric literals
- Non-decimal base fractions (`#x10/2`, `#o11/2`, `#b101/10`)
- Hashtable primitives with `Hashable` key interface
- Box primitives (`box`, `box?`, `unbox`, `set-box!`)
- Go embedding API via the `wile` package: `Engine`, `Eval`, `Compile`, `Run`, `Define`, `Get`, `Call`, `RegisterPrimitive`
- Value constructors for Go interop: `NewInteger`, `NewFloat`, `NewString`, `NewSymbol`, `NewBoolean`, `NewList`
- Library system with `define-library`, `import`, `export` and configurable search paths
- Interactive REPL with readline support and debug commands
- File execution mode with positional argument and `--file` flag
- SIGQUIT handler for goroutine stack dumps
- Multi-platform builds: `dist/{os}/{arch}/wile` layout with targets for darwin/linux on arm64/amd64
- Docker build support with `TARGETOS`/`TARGETARCH` platform awareness
- CI builds all four OS/architecture combinations
- R7RS conformance test suite running in CI

[Unreleased]: https://github.com/aalpar/wile/compare/v1.17.0...HEAD
[1.17.0]: https://github.com/aalpar/wile/compare/v1.16.0...v1.17.0
[1.16.0]: https://github.com/aalpar/wile/compare/v1.15.0...v1.16.0
[1.15.0]: https://github.com/aalpar/wile/compare/v1.14.258...v1.15.0
[1.14.0]: https://github.com/aalpar/wile/compare/v1.13.21...v1.14.0
[1.13.21]: https://github.com/aalpar/wile/compare/v1.13.0...v1.13.21
[1.13.0]: https://github.com/aalpar/wile/compare/v1.12.0...v1.13.0
[1.12.0]: https://github.com/aalpar/wile/compare/v1.11.0...v1.12.0
[1.11.0]: https://github.com/aalpar/wile/compare/v1.10.0...v1.11.0
[1.10.0]: https://github.com/aalpar/wile/compare/v1.10.3...v1.10.0
[1.10.3]: https://github.com/aalpar/wile/compare/v1.9.11...v1.10.3
[1.9.11]: https://github.com/aalpar/wile/compare/v1.9.5...v1.9.11
[1.9.5]: https://github.com/aalpar/wile/compare/v1.9.0...v1.9.5
[1.9.0]: https://github.com/aalpar/wile/compare/v1.8.0...v1.9.0
[1.8.0]: https://github.com/aalpar/wile/compare/v1.7.2...v1.8.0
[1.7.2]: https://github.com/aalpar/wile/compare/v1.7.1...v1.7.2
[1.7.1]: https://github.com/aalpar/wile/compare/v1.7.0...v1.7.1
[1.7.0]: https://github.com/aalpar/wile/compare/v1.6.1...v1.7.0
[1.6.1]: https://github.com/aalpar/wile/compare/v1.6.0...v1.6.1
[1.6.0]: https://github.com/aalpar/wile/compare/v1.5.0...v1.6.0
[1.5.0]: https://github.com/aalpar/wile/compare/v1.4.0...v1.5.0
[1.4.0]: https://github.com/aalpar/wile/compare/v1.3.0...v1.4.0
[1.3.0]: https://github.com/aalpar/wile/compare/v1.2.0...v1.3.0
[1.2.0]: https://github.com/aalpar/wile/compare/v1.1.0...v1.2.0
[1.1.0]: https://github.com/aalpar/wile/compare/v1.0.4...v1.1.0
[1.0.4]: https://github.com/aalpar/wile/compare/v1.0.3...v1.0.4
[1.0.3]: https://github.com/aalpar/wile/compare/v1.0.2...v1.0.3
[1.0.2]: https://github.com/aalpar/wile/compare/v1.0.1...v1.0.2
[1.0.1]: https://github.com/aalpar/wile/compare/v1.0.0...v1.0.1
[1.0.0]: https://github.com/aalpar/wile/releases/tag/v1.0.0

