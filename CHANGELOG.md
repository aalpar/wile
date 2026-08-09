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

### Added

- **R6RS `(rnrs hashtables)`.** The hash moves from the KEY to the TABLE, so
  **any object can be a hashtable key** — lists, vectors, records, nested tables.
  Which objects count as one key is now the constructor's choice.

  New: `make-eq-hashtable`, `make-eqv-hashtable`, `make-equal-hashtable`,
  `hashtable-contains?`, `hashtable-entries`, `hashtable-update!`,
  `hashtable-mutable?`, `hashtable-equivalence-function`,
  `hashtable-hash-function`, and the hash procedures `equal-hash`,
  `string-hash`, `string-ci-hash`, `symbol-hash`. `equal-hash` hashes a bounded
  prefix of a value's *unfolding*, so it terminates on cycles as R6RS requires
  and agrees with `equal?` even on bisimilar cycles of different node counts.

  `(rnrs hashtables)` is the first `(rnrs ...)` library in the tree. It is
  registered VERSIONLESS — R6RS's `(rnrs hashtables (6))` does not resolve,
  since versioned library names are unimplemented — and it deliberately does not
  re-export the four hash procedures; see `docs/reference/r7rs-differences.md`
  items 12-15.

  This retires `graph.scm`'s atomic-node-identifier restriction entirely: a
  graph keyed on lists or vectors now interns *and* reaches the Go counting
  kernel, where before it raised from the SCC side queries and was denied the
  fast path.

### Changed

- **An import no longer overwrites a definition of the same name.** Imported
  bindings used to be installed at the very coordinates a top-level `define`
  writes, so the two shared one slot and whichever ran second won *by
  assignment*. `(define map 1)` followed by `(import (scheme base))` therefore
  left `map` bound to the library's procedure, with the user's value gone and
  the binding stamped as imported. Imports now install one tier below, so a
  definition **shadows** an import in either order and the import stays visible
  when no definition exists.

  Two consequences worth knowing. `set!` on an import is still refused (R7RS
  §5.2) and `namespace-undefine!` still removes an import — it recognises one by
  provenance rather than by tier, so removing the definition reveals the import
  underneath instead of leaving the name unbound. And a `define` after an import
  no longer *drops* the import provenance, because it no longer touches the
  import's binding at all; the name simply resolves to the definition.

  Macro imports (the phase-1 half) deliberately keep the old placement: the
  coordinate the relocation targets is occupied at phase 1 by bootstrap macros,
  and moving them there would let an imported macro overwrite a bootstrap
  transformer engine-wide. So `define-syntax` over an imported macro still
  supersedes in place.

- **`error` is now capture-safe-annotated.** It runs the installed exception
  handler on the live chain, like `raise`, but was not marked as doing so — so a
  self-recursive procedure calling `error` was compiled with an in-place
  parameter rebind that corrupted any continuation captured inside the handler.
  Replaying such a continuation twice now continues the loop identically. The
  same annotation lands on `thread-join!` and `mutex-lock!`, which signal their
  SRFI-18 conditions the same way. Each costs a small optimization at those call
  sites.

- **BREAKING — hashtables.** `(make-hashtable)` is gone; use
  `(make-equal-hashtable)` or the R6RS `(make-hashtable equal-hash equal?)`.
  `hashtable-ref`'s `default` is now **required** and an absent key never
  errors. `hashtable-keys` returns a **vector**. `hashtable-values` is
  **removed** — `hashtable-entries` subsumes it and is the only way to get keys
  and values paired reliably. `(hashtable-copy ht)` with no second argument now
  returns an **immutable** table, per R6RS; pass `#t` to copy-and-mutate. This
  last one is silent — it changes behaviour rather than failing to compile.

  `equal?` on two hashtables is structural only when both use the same key
  equivalence and every key is a non-container; otherwise it is identity. No
  table that was constructible before changes its answer.

- **`wile --check` compiles a program without running it.** Parses, expands, and
  compiles every input, reports the first error as `file:line:col: ...` and exits 1,
  or exits 0 in silence — the `go build` of a Scheme program. It reaches code a test
  run would have to execute to discover, such as the body of a procedure that is
  never called. Files are checked in order against one namespace, so a later file
  resolves names an earlier one defines. Cannot be combined with `-i` or `--mcp`, and
  requires a file or `-e` expression. Embedders get the same seam as
  `Engine.CheckProgram`.

  Two caveats are inherent and documented rather than fixed: `(import ...)` executes
  the imported library's body at compile time, so checking a program that imports a
  side-effecting library runs those effects; and compiling a top-level `define`
  registers its binding, so checking is not read-only with respect to the engine's
  namespace.

- **Call-site arity is checked at compile time.** A call passing an argument count
  the callee cannot accept is now a compile error rather than a run-time one, wherever
  the callee is statically known and cannot be rebound to a different arity: ambient
  primitives, imported library procedures, and `define`s in the same compilation unit
  (including forward references). The soundness gate is the existing
  `Binding.IsStable()` rebind-stability proof for the first two, and the validator's
  `StableInUnit` evidence for the third; the accept/reject decision is delegated
  entirely to `values.Callable.AcceptsArity`, the same predicate the VM's apply path
  uses. Calls through a parameter, through `apply`, or under a mutable top level are
  not statically decidable and remain the run-time check's business.

  This fires in **all** compiles, not only under `--check`, on the grounds that
  reporting a guaranteed run-time failure earlier never changes what a correct program
  does. It does change one thing: a test that deliberately calls a known procedure
  with the wrong argument count in order to assert the *run-time* error no longer
  compiles. Write such a call through `apply` to keep the argument count hidden until
  run time.

## [1.19.1] - 2026-07-29

A compiler release: environment-frame elimination for `or`, frame release for
mutually recursive `letrec` bindings and internal defines, and the escape/capture
analysis relaxations those depend on. One soundness fix in the capture-safe
analysis. No language-visible semantic change.

### Changed

- **`or` no longer allocates an environment frame per operand.** `or` expands to
  `(let ((t E)) (if t t B))`, one frame per operand beyond the first, and that frame is
  unobservable: `t` is bound only to be tested and returned. `OpBranchOnFalseValue` reads
  the value register without writing it, so the value `E` already left there *is* the
  consequent's value — the form now compiles to `E`, a branch, and `B`, with no slot, no
  `OpPushEnv`/`OpPopEnv` pair and no reload. `(or x y)` goes from 9 instructions to 5, and
  an `or` in a tail loop drops from 2.0 to 0.0 env-frame allocations per iteration while
  its self call now earns `OpSelfTailCall`. Result values are unchanged: the register
  passthrough returns `E`'s own value, so `(or 5 1)` is still `5` rather than `#t`. A
  hand-written `let` of the same shape whose body references the bound temp, and the
  sibling shape `(cond (test => f))` whose consequent *consumes* the bound value, are both
  refused; matching is by name **and** binder-scope containment, through the same
  `syntax.ScopesCompatible` the environment's local resolution uses. 340 such frames exist
  across 181 stdlib and Larceny sources. No interleaved A/B was run for this change, so
  there is no end-to-end suite figure to quote.
- **A `letrec` binding whose tail call goes to a sibling now releases its frame.** Previously
  only a *self* tail call qualified, so a mutually recursive group allocated a frame per
  iteration (measured 2.0, now 0.0). Because clearing a call to sibling `o` rests on `o` not
  capturing — the very property being proven for `o` — the safety predicate is co-inductive
  over the whole `letrec` group and answers uniformly: one unsafe member refuses the group.
  Self-tail reuse keeps precedence, since rebinding in place beats releasing and
  re-acquiring. `examples/benchmarks` contains no `letrec` at all, so the corpus prices this
  at zero; named lets do desugar to `letrec` but carry a depth-0 self call and take the
  self-tail path instead. The lever is generality, not measured throughput.
- **Internal defines get the same frame release.** R7RS §5.3.2 gives internal defines
  `letrec*` semantics, but they are never rewritten into a `let` — they stay define nodes
  compiled in the enclosing lambda's frame — so they reached the release decision by a
  different path and the `letrec` predicate never saw them. Same shape, same 2.0 → 0.0
  allocations per iteration. Six armed sites corpus-wide (`compiler.scm` 522 → 527,
  `gcbench.scm` 2 → 3); timing is correspondingly flat. Mutually recursive internal defines
  whose sibling call sits in tail position are rare even in code carrying 255 internal
  defines.
- **A lexically-bound call operator is now proven capture-safe instead of refused.** Since a
  named `let` validates to a `letrec` whose single binding is the loop lambda, the blanket
  refusal covered every loop-shaped procedure in the language and propagated to each of
  their callers. The capture-safe stamp count across `examples/benchmarks` goes 19/68 →
  42/68. This is the change with a measured end-to-end win: the stamp travels one edge up
  through non-tail callee resolution, so `primes.scm` improves **−4.23%** (interleaved
  min-of-5 across both binaries, reproducing a −4.94% eight-round reading), and
  `(primes-upto 1000)` drops 17570 → 15574 allocations per op — ~2 per iteration, the
  binding frame. A recursive formulation of the proof was built first and deleted: it
  produced exactly the same 42/68.
- **Escape analysis no longer treats every loop-shaped procedure as escaping.** A
  `let`/`letrec`-bound lambda's frame cannot outlive the call iff every reference to the
  binding sits in call-operator position; called is not escaping, while stored, returned,
  passed as an argument, or `set!` is. Frame-reclaim verdicts over `examples/benchmarks` go
  84.6% → 90.7% aggregate, with `diviter`, `nqueens`, `primes` and `sumfp` each 0.0% →
  100.0%. **This one changes zero emitted instructions on its own** — opcode counts are
  identical in both arms — and is recorded as a precondition for later work rather than as a
  win.

### Removed

- **`examples/benchmarks/puzzle.scm` and `puzzle-debug.scm`.** `puzzle.scm` raised a type
  error on every run since it was introduced: its search target `'(1 2 3 4)` is a flat list
  of integers while the generator yields permutations of *sublists*, so the fit test always
  handed `=` a pair. Both the shape and the length were wrong, and there is no input for
  which it reaches its base case. No runner script referenced it, so the failure had no path
  to a red build — it compiled fine, which is why static-analysis harnesses globbing
  `examples/benchmarks/*.scm` accepted it as a corpus member. `puzzle-debug.scm` was a
  leftover diagnostic written to isolate exactly this mismatch. The canonical Gabriel
  `puzzle` (Baskett's 3D piece-placement search) is unaffected and still runs from
  `benchmarks/larceny/src/puzzle.scm` via `make bench-extended`.

### Fixed

- **A parameter sharing a capture-safe global's name no longer inherits that global's
  stamp.** The capture-safe body walk ran with an empty shadow set against a flat
  environment with no local frames, so in `(define (f car x) (car x))` the operator `car`
  resolved through the global environment to the capture-safe primitive, and `f` was stamped
  capture-safe — even though `car` is bound to whatever the caller passes, including
  `call/cc`. The analysed procedure's own parameters are now seeded into the shadow set,
  which is what local resolution means in that walk.

### Added

- **`,doc` now reports where an imported binding is defined.** A trailing
  `From: (srfi 1)` line, or `From: (srfi 1) as fold` when the name you typed differs
  from the name it is defined under (an export or import rename anywhere along the
  chain). It reads the import-provenance root recorded on the binding, so it names the
  **defining** library rather than the one a program happened to import from: a binding
  taken through a re-exporting library still reports its origin, not the intermediary.
  Bindings never reached by an import — a top-level `define`, or an ambient bootstrap
  name usable without importing anything — have no root, and get no line rather than a
  guess. Embedders building their own doc UI get the same data as
  `repl.DocInfo.Origin` (an `*environment.OriginRef`, nil when absent). This is the
  first consumer of the provenance the binding-identity work below recorded.

### Removed

- **`values.ChannelSelect` and its `SelectCase`/`SelectCaseKind` types are gone.**
  Exported from `values/` (a public embedding package) but reachable from Go only —
  no `channel-select` primitive was ever registered, so no Scheme program could
  invoke it. It had also drifted out from under the channel lifecycle: since the
  done-channel rewrite the data channel is never closed, so `ChannelSelect` built
  its `reflect.SelectCase` set on channels that cannot report closure, and a peer
  closing a channel mid-block was invisible to a blocked select. It took no
  `context.Context` either, where `Send`/`Receive` both do, so exposing it as-is
  would have reintroduced the parked-goroutine leak those two were fixed for.
  Removed rather than repaired: nothing consumes it. Wiring it back is a bounded
  job (a `done` arm and a ctx arm per channel in the `reflect.Select` set, an
  arity guard for `reflect.Select`'s 65536-case ceiling now that the list would
  come from Scheme), and worth doing when a `channel-select` surface has an actual
  consumer.

### Fixed

- **An ambiguous hygienic reference now raises instead of silently resolving to the
  innermost binding.** When two same-name bindings carry equal-cardinality, mutually
  incomparable scope sets — both maximal subsets of the reference's scope set — no single
  binding is *the* maximal match, so the reference is ambiguous (Racket raises here per
  Flatt's set-of-scopes model). The binding resolver ranked candidates by scope-set
  cardinality alone and kept the first-seen (innermost) on a tie, silently picking one.
  Resolution now detects the incomparable tie (`scopedBestOf`) and raises
  `werr.ErrAmbiguousBinding` at all three resolver sites, surfaced as a `CompilationError`
  (matchable via `errors.Is`) on `Eval`/`EvalMultiple`/`Compile`. No bundled program
  produces the tie today — the whole Go and Scheme example/test corpus stays green with the
  raise live — so this is a conformance guard for the incomparable-tie case rather than a
  fix to a reachable regression.
- **Internal `define-syntax` now works in a function-shorthand `define` body.**
  `(define (f) (define-syntax m …) (m))` raised `no such local or global binding "m"`;
  the same body under `let`/`lambda`/named-`let` worked. The shorthand-`define` body was
  expanded as a flat argument list, so internal macros were never registered and body
  identifiers carried no scope (violating the compiler's "a local always carries a scope"
  invariant). The shorthand body now gets the identical treatment a lambda body gets. The
  form is preserved (not desugared), so the self-tail-call and frame-reclaim optimizations
  are unaffected.
- **A local `define-syntax` now shadows a same-named imported macro.** Importing a macro and
  then defining your own macro of that name used the *imported* one — at the top level and
  inside a library, in both the export surface and the body — while the variable analogue
  shadowed correctly. Imported macros were mirrored into the runtime frame, which macro
  resolution and the export probe consulted first; imported macros now install into the
  expand frame only (on both the library-internal and top-level import paths). Imported
  macros stay usable.
- **A free identifier in a macro template now resolves definition-site on the macro path,
  matching values (R7RS §4.3.2).** A top-level `(define-syntax guard-aux …)` captured
  `guard`'s private helper — `(guard (e (else 'x)) (raise 'y))` returned the user's
  transformer instead of `x`. Bootstrap macros/expanders now live in a per-namespace
  immutable **sealed expand base** (phase 1), so a use-site `define-syntax`,
  `let-syntax`/`letrec-syntax`, or `import` of a same-named macro **shadows** in the mutable
  expand child instead of overwriting the pinned binding in place; and macro dispatch consults
  the template's definition-site pin (after the local `let-syntax` arm, before the use-site
  arms), so the helper resolves definition-site while a co-introduced keyword still shadows it.
  The guarantee also covers a helper's **own recursion**: a recursive macro's self-reference
  (a multi-clause `guard`, a `define-record-type` with fields) is pinned to its own binding, so
  a use-site redefinition cannot capture the recursion either. This is the private-helper /
  definition-site guarantee documented for values in `docs/reference/r7rs-differences.md` (Chez
  two-environment model), now extended to macros; public-macro redefinition is unchanged by
  intent (a directly-typed reference carries no pin).
- **Redefining a core special form now shadows cleanly instead of bricking it.**
  `(define-syntax let-syntax …)` overwrote the installed primitive-expander slot in place, so
  every subsequent `(let-syntax …)` failed to compile (`let-syntax` has no fallback). The
  special-form expanders now install in the sealed expand base, so a user redefine becomes a
  shadow in the mutable expand child and the user's transformer runs; the original form stays
  intact for code that does not redefine it.
- **`free-identifier=?` and an ER-macro's `compare` now decide identity by binding
  provenance, not by pointer or by value.** Both were wrong, on complementary cases
  (the conformant answers verified against Racket and Chez). `free-identifier=?`
  compared `*Binding` pointers, so two rename-imports of one binding — taken directly,
  through a re-exporting library, or under a renamed export — reported *different*.
  ER-compare fell back to comparing the bindings' *values*, so two distinct defines
  that happen to hold one value (`(define a car)` `(define b car)`) reported *same*.
  Each binding now carries an import-provenance root (`environment.OriginRef{RootLib,
  RootName}`), folded at import and keyed on the defining name so export/import
  renaming cannot fork identity, and both predicates compare that root via
  `environment.SameBinding`. A library's own exports are stamped with their own root
  at library finalization, so a library-internal binding and an import of itself
  compare equal too — that is where ER-compare resolves a rename, and
  `free-identifier=?` now agrees with it. Impact is narrow: `free-identifier=?` is
  R6RS rather than R7RS-small, and no bundled program reaches either case.
- **Import-gated `fold`/`fold-right` inlining could inline the wrong template.** The
  compiler inlines a curated higher-order primitive when the binding comes from the
  library that owns it, but it selected the inline template by the *call-site* name —
  so importing one curated HOF renamed onto another curated HOF's name ran the other's
  body, a silently wrong result. Dispatch now selects by the canonical name stamped on
  the binding at import time (`BindingMeta.InlineHOFName`). The gate itself follows the
  binding's provenance root rather than the immediate import library, so the real
  `(srfi 1)` `fold` is still inlined through a re-export chain while a same-named HOF
  from elsewhere is refused; and the stamp is cleared on every re-import, so a
  last-import-wins replacement cannot leave a stale template on the new value.
- **An imported macro's docstring could outlive the macro it described.** Two
  libraries exporting one name conflate under the by-name import diamond (R7RS §5.6
  last-import-wins), so a later import replaces the earlier one's value in the shared
  slot — but the docstring carried across the import boundary was written only when
  non-empty, so nothing could ever clear it. Importing a documented macro and then an
  undocumented macro of that name left `,doc` reporting the displaced macro's
  documentation for the macro that actually expands. The docstring is now assigned
  unconditionally at import, so it tracks the current value the same way the
  inline-HOF stamp does. Procedures were never affected: a closure carries its
  docstring on its own template, so it travels with the value; the imported binding's
  metadata is the macro path's only carrier, and so the only one that could go stale.
- **Release note correction (1.18.0).** The 1.18.0 entry "`channel-select` is
  deterministic when a closed send races a ready receive" described `channel-select`
  as a Scheme primitive. There has never been one; the fix it describes was to the
  unexported-in-practice Go helper removed above. No Scheme-visible behavior changed
  in 1.18.0, and nothing a program could call was affected.

### Changed

- **Top-level bindings are now scope-keyed (hygienic global storage).** A top-level name
  owns one binding slot per distinct hygiene scope set rather than a single shared slot,
  which corrects several macro-hygiene cases at global scope. User-visible consequences:
  a macro-generating macro expanded twice now produces two independent binders instead of
  silently sharing one; a library that re-exports a template-introduced identifier is now
  rejected eagerly at definition rather than exporting an unhygienic binding; and
  `namespace-undefine!` now removes the single scope-matched binding (the one
  `namespace-ref` resolves) instead of every slot the name owns, so it can no longer
  destroy a macro-introduced binding the reader reports as unbound. (Landed across the
  `8afeb66a…4f73936d` arc; architecture in `docs/environment/system.md`, Invariant 5.)
- **SRFI-18: `thread-join!` wraps an uncaught exception.** A thread that terminates via an
  exception it did not handle is now surfaced to the joiner as an `uncaught-exception`
  object rather than by re-raising the bare condition. Recover the original with the new
  `uncaught-exception-reason` (and test with `uncaught-exception?`); the reason is the same
  object the thread raised. This is the strict SRFI-18 behavior; a joiner that matched the
  bare condition inside a `guard` must now unwrap via `uncaught-exception-reason`.
- **Exactness contagion (R7RS §6.2.2) now actually contaminates.** An exact operand
  meeting an inexact one is absorbed into the inexact operand's representation, on
  **both** the real and the complex axis: `(+ 1.5 2)` is a `Float` (it was a 256-bit
  `BigFloat`) and `(+ 1.0+2.0i 1)` is a `Complex` (it was a `BigComplex`). The old
  behavior promoted "to preserve precision", on the theory that `Simplify` would demote
  afterwards; per-op demotion was never wired, so ordinary arithmetic minted bignums
  that never came back down. **This is user-visible and lossy by design**: an exact
  value too large for `float64` now overflows, so `(+ 1.5 (expt 2 2000))` is `+inf.0`,
  which is what Chez gives. A program that needs the precision must stay exact or ask
  for it with a `#m` literal, which is preserved.

  The complex half was not a free choice. The promotion table is a join-semilattice,
  and with `exact ⊔ Float = Float` and `Float ⊔ Complex = Complex`, associativity
  *forces* `exact ⊔ Complex = Complex`. Escalating instead broke the law on 12 of its
  343 triples, and because the result kind is observable through `eqv?`/`equal?`, that
  produced values that were `=`, printed identically, and were not `eqv?`:
  `(eqv? (+ 1 1.5 2.0+0.0i) (+ 2.0+0.0i 1 1.5))` answered `#f`, and
  `(equal? (* 1.0+2.0i 1) 1.0+2.0i)` — multiplying by exact `1` — answered `#f`.
  Signed zero is unaffected: `(/ 10 2.0+0.0i)` is still `5.0-0.0i`, now preserved at the
  *operation* (a real operand contributes no imaginary component, so `real ⊕ complex` is
  computed part-wise) rather than by escalating the promotion.
- **Inexact representation is observable, so `Float` and `BigFloat` are never `eqv?`.**
  Per R7RS §6.1, a `float64` `1.0` and a 256-bit `#m1.0` are distinguishable by
  arithmetic and are therefore distinct numbers. They no longer compare equal, and — the
  practical consequence — a `BigFloat` key no longer finds a `Float` entry in a
  hashtable. Exact cross-representations are unaffected: `1`, `#e1`, and `1/1` remain
  `eqv?` and hash alike.
- **`values.PromotionResultKind` / `values.ComparisonResultKind` are no longer public.**
  Both had zero production callers and existed only for the external test package. As
  public API they were a raw-index panic (`NumericKind` is exported, the bound is not)
  and a footgun: nothing in the type system distinguished "kind for arithmetic" from
  "kind for comparison", and picking the wrong one silently rounds an operand. They now
  live in `export_test.go`.
- **Strict-mode FFI precision loss is detected at the boundary, not through arithmetic.**
  With contagion fixed, `(+ 1.0 (expt 10 60))` is an ordinary lossy `Float`, so a strict
  `float64` parameter accepts it. It used to be rejected — but only because the old
  `BigFloat` contagion kept the precision alive past the point where it should have been
  lost. Strict mode was detecting Wile's own non-conformance. Use a `#m` literal to keep
  precision through mixed arithmetic.

### Fixed

- **The `values.Value` Go-comparability contract is real and enforced.** Every `Value`
  implementor must be Go-comparable: `values.EqIdentity` — backing `eq?`, `memq`, `assq`
  and the literal pool — is a bare `a == b` on the interface, and Go *panics* with
  "comparing uncomparable type" when the dynamic type is a slice, map, or func. In an
  embedded engine that panic lands in the host's process. Three violators had shipped
  (`machine.Operations`, `machine.MultipleValues`, `machine.boxedValues`), none found by
  reading code. The first two were not Scheme data at all and lost the conformance; the
  third is genuinely a `Value` and became pointer-shaped. Enforcement is now module-wide
  via `go/types` (`TestValue_AllImplementorsAreGoComparable`), which checks all ~130
  implementors across all seven implementing packages and needs no roster to keep current.
- **Trichotomy across the exact/inexact boundary.** For any two reals exactly one of
  `<`, `=`, `>` holds. It did not: the comparison dispatchers carried an IEEE
  special-value guard that, when either operand was an `Inf`/`NaN` `Float`, routed
  **both** operands through `float64`. An exact bignum too large for `float64` became
  `+Inf` and compared *equal* to the infinity it was tested against, so
  `(< (expt 10 400) +inf.0)` was `#f` **and** `(>= (expt 10 400) +inf.0)` was `#t`,
  simultaneously. The guard's premise (that `BigFloat`/`BigComplex` cannot hold
  `Inf`/`NaN`) went stale when they were made IEEE-capable. Comparison never rounds an
  operand now — that is the whole reason it uses a different promotion table from
  arithmetic.
- **The literal pool had two dedup predicates that disagreed.** `AddLiteral` used
  `literalIdentical` (which refuses to merge across concrete types) while
  `deduplicateLiteral` used a bare `EqualTo` (which does not), so the pool could hand
  back a pooled `Integer 1` in place of a `BigInteger 1` and silently re-type the
  literal. Both paths now use one predicate, and its hand-rolled `Float` arm — named in
  `eqv.go` as one of three drifted copies of the numeric rule, and by the end disagreeing
  with `EqualTo` on NaN — is gone in favor of `EqvNumber`.
- **`BigComplex.HashCode` no longer hashes a NaN component as zero.** It hashed the
  component's raw backing `big.Float`, bypassing `BigFloat.HashCode` and so bypassing
  NaN canonicalization; a NaN-valued `BigFloat` stores its flag alongside a *zero* value,
  so a `BigComplex` with a NaN real part hashed identically to one with `0.0`. Equality
  recurses into the components, so hashing does too.
- **`eqv?` and `=` no longer contradict each other on an exact complex.** An exact
  `BigComplex` with an exact-zero imaginary part *is* a real number, and R7RS §6.1
  requires two exact numbers that are `=` to be `eqv?`. `=` said `#t` and `eqv?` said
  `#f`. Not reachable from Scheme (`make-rectangular` canonicalizes) but
  `values.NewBigComplex` is public API and does not. Inexact does not collapse:
  `(eqv? 1.0 1.0+0.0i)` is still `#f`, matching Chez.
- **`EqvNumber` no longer conflates distinct typed-nils.** A nil `*Float` and a nil
  `*BigInteger` are both void and are not the same value; the centralized void guard
  answered `#t` for the pair, widening what the per-type guards it replaced had done.

- **`equal?` is no longer finer than `eqv?`.** R7RS §6.1 orders the equivalence
  predicates by coarseness — `eq?` ⊆ `eqv?` ⊆ `equal?` — and each must answer `#t`
  wherever the finer one does. `equal?` fell *below* `eqv?` on NaN: `eqv?` settles
  identity before inspecting the value, but `equal?` went straight to
  `Float.EqualTo`, which compares values, and IEEE-754 says `NaN != NaN`. The
  damage was concrete, not philosophical — `member`/`assoc` are `equal?`-based
  while `memv`/`assv` are `eqv?`-based, so `(member x (list 1 x 2))` could not find
  the very object it was handed. `equal?` also disagreed with itself by nesting
  depth: `(equal? x x)` was `#f` while `(equal? (list x) (list x))` was `#t`.
  Both predicates now route through the single authority `values.EqvNumber`, so they
  agree on numbers by construction rather than by coincidence, and **`(eqv? +nan.0
  +nan.0)` is `#t`** (R7RS §6.1 makes this explicitly unspecified; Wile follows Chez
  and Racket). `equal?` remains coarser than `eqv?` on strings and lists, and numeric
  `=` keeps IEEE-754 semantics unchanged — `(= +nan.0 +nan.0)` is still `#f`. It is a
  different predicate, and conflating it with the equivalence relation was the bug.
- **A recovered Go panic now carries its site's sentinel.** `werr.RecoverAsError`
  returned an error-typed panic value unchanged, so the sentinel only ever reached
  *non-error* panics. A Go runtime fault inside foreign code (nil dereference, index
  out of range) arrives as a `runtime.Error`, which satisfies `error` — it therefore
  carried no `ErrPanicRecovery` and no site attribution, and reached Scheme
  indistinguishable from a deliberate `(error "…")`. `errors.Is(err,
  ErrPanicRecovery)` could not match a real panic. The value is now chained as a
  cause, which preserves the `errors.As` routing that VM signal types (prompt abort,
  exception escape, timer interrupt, continuation resume) depend on.
- **`equal?` no longer panics on a non-comparable operand.** `values.Equal` formed
  its visited-set key before validating the right-hand operand. The key is a map
  key, and hashing a non-comparable dynamic type panics — a stronger requirement
  than the identity compare above it, which only faults when *both* types match. A
  container compared against a slice-backed `Value` therefore panicked inside the
  map lookup. `step()` now reaches both the identity compare and the visited-set key
  only once *both* operands are known to be `DeepEqualer`s. (It was reachable in-tree
  at the time via `machine.Operations`, a `[]Operation` that then implemented `Value`;
  see the comparability contract below, which removed that conformance.)
- **`syntax-local-introduce` fails honestly.** Nothing sets an introduction scope, so
  the primitive could never succeed, yet its docstring advertised working behavior
  with a worked example. It now reports `werr.ErrNotImplemented` (a new sentinel;
  the previous `ErrNoCaptureContext` misdescribed a wiring gap as a runtime
  condition) and says so in its documentation.
- **`SyntaxVector.ForEach` honors context cancellation.** It accepted a `ctx` it never
  read — the same shape that let `apply` ignore cancellation.
- **Stale doc comments** on `Vector.EqualTo` and `Hashtable.EqualTo`, which still
  described the recursive comparison that the iterative worklist replaced.

## [1.18.0] - 2026-07-09

### Added

- **Optional docstring on `define-syntax`.** A macro definition may now carry a
  Guile-style docstring between the keyword and the transformer —
  `(define-syntax NAME "doc…" TRANSFORMER)` — mirroring the leading-string
  docstring `define` already accepts on procedure bodies. The docstring is
  surfaced by the REPL's `,doc` command and the doc tooling, and it survives
  `import`, so a documented library macro keeps its documentation at the use
  site. All 22 `(wile control)` delimited-continuation macros (`prompt`/
  `control`, `reset`/`shift`, the `0`-variants, `spawn`, `set`/`cupto`) are now
  documented.

### Changed

- **BREAKING: the remaining public packages moved under `pkg/`.** v1.17.0 moved
  only the root `wile` package to `github.com/aalpar/wile/pkg/wile`; this
  completes the relocation. The library and infrastructure packages — `values`,
  `werr`, `registry`, `security`, `repl`, `docparse`, `machine`, `environment`,
  `syntax`, `parser`, `schemeutil` — now live at `github.com/aalpar/wile/pkg/<name>`.
  Migration: insert `/pkg/` into the import path, e.g.
  `github.com/aalpar/wile/values` → `github.com/aalpar/wile/pkg/values`. The
  module path (`github.com/aalpar/wile`) is unchanged. The `extensions/*`
  packages keep their `github.com/aalpar/wile/extensions/<name>` paths (the `io`
  extension is the lone exception, now `github.com/aalpar/wile/pkg/extensions/io`),
  and repo tooling moved under `tools/`. This supersedes the v1.17.0 note that
  subpackage imports were unaffected. (#773)
- **REPL: `Ctrl-C` is contained to the running form.** An interrupt during
  evaluation now cancels just that form and returns to the prompt instead of
  tearing down the session with `context canceled: REPL error`. `main` owns
  `SIGTERM`; the REPL owns `SIGINT` and runs each form under its own cancellable
  context. Runtime-error rendering no longer double-prints the source location
  and stack trace or the redundant `runtime error:` prefix, and the REPL input
  source is now injectable for embedders via `repl.WithInput(io.Reader)`. (#784)
- **Embedding hardening: a panic at the VM boundary is contained as an error.**
  `RunWithEscapeHandling` now converts every panic into a returned `*SchemeError`
  rather than re-raising a `runtime.Error`, so malformed input or an internal
  fault stays within the VM boundary instead of crashing the host process or
  REPL. (#784)
- **`write`/`display`/`write-shared` bound their nesting depth.** A new
  `DefaultMaxWriteDepth` caps writer recursion — the fourth depth bound beside
  the call, parse, and expand limits — so pathologically deep or cyclic
  structure raises `ErrWriteDepthExceeded` instead of overflowing the host
  stack. Flat lists of any length still write (the spine is walked iteratively).
  (#782)

### Fixed

- **The datum reader no longer crashes the host on malformed input.** `read`
  accepts untrusted input (R7RS §6.13.2), yet several inputs took the process
  down with an unrecoverable Go panic or silently mis-parsed — e.g. `#u8(1 2]`
  (nil-deref), `(( . ))` and `#0=(1 . )` (nil-cdr panic), `( . 5)` (silently
  dropped the `5`). Every malformed input now returns a located `*ParserError`,
  with a boundary catch-all that lifts any stray non-`*ParserError` to a located
  error. The reader also gained the repo's first Go native fuzz targets (clean
  at 180s / 20M+ executions). (#779, #782)
- **Datum labels (`#n=`/`#n#`) and circular vectors read correctly.** An
  undefined or forward `#n#` reference silently became the integer `n`; a
  self-reference inside a labeled vector (`#0=#(1 #0#)`) silently became `0`
  because vectors — unlike lists — were not pre-registered before their elements
  were read; and a genuine circular vector literal overflowed the compile-time
  validator, which cycle-guarded pairs but not vectors. Forward/undefined
  references now raise `ErrDatumLabelUndefined`, labeled vectors resolve
  self-references, and the literal validator cycle-guards vectors. (#780)
- **`syntax-rules`: a depth-0 pattern variable now broadcasts into an ellipsis
  sub-template.** A template such as `(list (+ x e) ...)` with `x` bound at
  ellipsis depth 0 raised a spurious unbound-reference error (R7RS §4.3.2);
  per-iteration capture contexts now chain to their parent, so a lower-depth
  variable resolves at any nesting depth.
- **Integer overflow corners promote to bignum.** `(* MinInt64 -1)` and
  `(quotient MinInt64 -1)` silently returned the wrapped `MinInt64` instead of
  promoting to `+2^63`, violating R7RS §6.2.6 (exact integer arithmetic is
  unbounded). Both boundaries now promote, matching every other overflow path.
- **`channel-select` is deterministic when a closed send races a ready
  receive.** Selection previously relied on `reflect.Select` panicking on a send
  to a closed channel; when a send case's channel was closed *and* a receive
  case was ready, identical inputs could return either the received value or the
  closed-send error (~50/50). Closed send cases are now detected explicitly
  before blocking, so the result is stable.

### Performance

- **Tail-call frame reclamation and capture-safe HOF inline-reclaim.** Two
  related optimizations cut env-frame allocation, which dominates the heap on
  recursive and call-heavy workloads. The compiler now proves when a tail call's
  environment frame is dead and reclaims it — closing the mutual-recursion frame
  leak — driven by an interprocedural capture-safety capability and proof that
  replaced the previous hand-maintained whitelist (#775, #776). The six curated
  tail higher-order procedures (`map`, `for-each`, `fold`, `vector-map`/
  `vector-for-each`, `string-map`/`string-for-each`) inline-reclaim their
  per-element callback frames at capture-safe call sites, sharply reducing
  allocation in folds and traversals (#778).
- **Lock-free per-thread object pools.** The env-frame, continuation, and stack
  pools now use a per-thread freelist without the previously uncontended
  mutex/atomics, improving single-thread call-bound throughput and SRFI-18
  thread scaling (#777).
- **Compound-argument tail primitive calls are promoted.** A tail call to a
  primitive with compound arguments (e.g. the `+` in `fib`'s tail position) is
  inlined, gated on a preceding frame-reclaim proof so re-entrant continuations
  stay correct (#774).

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

[Unreleased]: https://github.com/aalpar/wile/compare/v1.19.1...HEAD
[1.19.1]: https://github.com/aalpar/wile/compare/v1.19.0...v1.19.1
[1.19.0]: https://github.com/aalpar/wile/compare/v1.18.0...v1.19.0
[1.18.0]: https://github.com/aalpar/wile/compare/v1.17.0...v1.18.0
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

