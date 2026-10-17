# Tier 1 — Security & Correctness

> One of the files [`TODO.md`](../TODO.md) is split into (2026-10-17). `TODO.md` holds the
> status, ordering, conventions, Plan Index and Top Priority table; this file holds rows.
> Cite a row by its bold text, never by line number. `make indexlint` and `make planlint`
> read `TODO.md` and `todo/*.md` as one corpus.

## Tier 1 — Security & Correctness

Items that block production embedded use or prevent silent state corruption.

### Redefining `if` — the three capture defects CLOSED 2026-10-14 (P3.3 `6f5b9f6a`/`1f380c15`, P4.3 `35e8866a`/`c4442f23`); two open rows below

Found while probing whether a program can override core forms. The three entries below were filed
2026-09-19 with *hypothesised* mechanisms; both hypotheses were **wrong**, and both root causes were
located by instrumentation on 2026-09-20. Design and the two candidate fixes:
`plans/2026-09-20-core-form-override-and-import-portability-design.md` §3.

**Two independent sites, not one.** The recap called defects 1 and 3 "the same root cause seen from
two sides". Measured: a 3-line change at the D2 pin arm fixes 1 and 2 and leaves 3; a 6-line change
in library export resolution fixes 3 and leaves 1. Neither touches the other's file.

**Oracle table**, run 2026-09-20 on HEAD `e64c43a2` against Racket 9.2 and Chez (`petite`, R6RS
libraries). Program: a user `(define-syntax if (syntax-rules () ((_ a b c) 'mine)))` over
`(list (cond (#f 1) (else 2)) (and 1 2) (or #f 7) (if #t 1 2) (when #t 5) (case 1 ((1) 'one) (else 'other)))`.

| | Racket 9.2 | Chez | Wile `e64c43a2` | Wile `956604f0` |
|---|---|---|---|---|
| defect 1 | `(2 2 7 mine 5 one)` | `(2 2 7 mine 5 one)` | every slot `mine`; `when` raises "no matching clause" | `(2 2 7 mine 5 one)` |
| defect 2 | `1` | `1` | SIGKILL at 20 s | `1` |
| defect 3 | `lib-if` | `lib-if` | `1` | `lib-if` |

Racket and Chez agree byte for byte on all three, so this is not a place where the report is silent.
The 2026-09-19 filing listed three capturing forms; `or`, `case` and named `let` capture too.
Controls that stay correct: a library macro named `zif` imports and dispatches fine, and a user
`(define if 5)` or `(define (if a b c) …)` (a *variable*) leaves `cond`/`and`/`when` alone. The fault
is a **macro** named after a special form.

- [x] **A top-level `define-syntax` of `if` captures the `if` inside stdlib macros** [Correctness /
  hygiene, M, filed 2026-09-19, Done 2026-10-14, P4.3/I132, `35e8866a`, merged `c4442f23`: a
  Primitive pin stops `lookupMacroBinding`'s search and is handed to head dispatch;
  `validate.resolveFormHead` reads the pin first. Gate `integration/core_form_denotation_test.go`.
  The record below is as filed]: after
  `(import (scheme base) (scheme write)) (define-syntax if (syntax-rules () ((_ a b c) 'mine)))`,
  every derived form in the oracle table above answers `mine`. Racket and Chez both answer
  `(2 2 7 mine 5 one)`.
  **Root cause (verified, not hypothesised).** `lookupMacroBinding`'s D2 arm,
  `pkg/machine/compilation/expander_time_continuation.go:393-401`. The 2026-09-19 filing guessed the
  template's `if` "gets no pin". It gets one, and it resolves — instrumented at HEAD while expanding
  `(cond (#f 1) (else 2))`: `hasPin=true pinned=true type=BindingTypePrimitive`. D2 then **discards**
  it, because the arm filters `pinned.BindingType() == BindingTypeSyntax` and a special-form keyword
  is `BindingTypePrimitive`. The arm's own doc states the exclusion as deliberate. Control then falls
  to ARM 2, `p.env.NextPhase()`, which is exactly where a top-level `define-syntax if` deposits, so
  the user's transformer captures the stdlib template.
  **Candidate fix, measured.** Three lines: a pin resolving to `BindingTypePrimitive` is an *answer*,
  not a miss, so the macro search stops. Wile then prints `(2 2 7 mine 5 one)`, identical to both
  oracles, and `go test ./...` is green (all 26 reported failures were "scheme binary not found — run
  make build first" in the probe worktree; after building `dist/`, `./integration/...` passes too).
  Two things to settle before shipping it, in the plan as Q2: the shipped form should return the
  **pinned expander** rather than `nil`, so head dispatch does not re-resolve by use-site spelling;
  and `lookupMacroBinding` has three callers (`:551`, `:860`, `expander_context.go:86` `MacroValue`),
  of which `MacroValue` backs `(expand-once …)` and is untested under a pinned special form.
  Under `WILE_SYNTAX_FORMS=scheme` the `define-syntax` itself still fails, "not a closure:
  values.voidType" — separate, unverified, and out of scope until the layer flip.
- [x] **`(except (scheme base) if)` plus a user `if` written with `cond` expands for effectively
  ever** [Correctness, S, filed 2026-09-19, Done 2026-10-14, P4.3/I133, `35e8866a`, merged
  `c4442f23`: died with the capture above; answers `1`. Gate
  `TestCoreFormDenotationExceptIfTerminates` (`integration/core_form_denotation_test.go`)]: `(if #t 1 2)` with
  `(define-syntax if (syntax-rules () ((_ a b c) (cond (a b) (else c)))))` recurses `if` → `cond` →
  `if` through the capture above. Racket and Chez both give `1`; Wile is SIGKILLed at 20 s.
  **Same root cause, and it dies with it**: under the candidate fix the program returns `1` in
  **32 ms**. The superlinear expansion cost the 2026-09-19 filing bundled in here is a **separate
  fault with its own entry below**; it is not fixed by this and does not need this to fire.
- [x] **An imported macro named `if` loses to the core form** [Correctness / conformance, S, filed
  2026-09-19, Done 2026-09-30, P3.3/I134, `6f5b9f6a`, merged `1f380c15`: `findLibraryBinding`
  prefers the library's own Expand-phase syntax row over the inherited core form; answers `lib-if`.
  Pinned by `pkg/wile/library_own_syntax_export_test.go`]: library `(myif)` exports
  `(define-syntax if (syntax-rules () ((_ a b c) 'lib-if)))`; a program
  `(import (except (scheme base) if) (myif))` gets `1` from `(if #t 1 2)` where Racket and Chez give
  `lib-if`.
  **Root cause (verified).** Not keyword dispatch, as filed. **Library export resolution.**
  `findLibraryBinding`, `pkg/machine/compilation/library_bindings.go:554-557`, probes phase 0 first
  and accepts *anything that is not* `BindingTypeSyntax`. A library's `define-syntax` writes at the
  Expand phase, so the phase-0 probe finds the **sealed core `if`** the library inherited from its
  own base and returns that as the export. The phase-1 keyword probe at `:562` is never reached.
  Three measurements rule out the spelling hypothesis: (a) a *renamed* import,
  `(rename (myif) (if qif))`, misbehaves identically — a 1-argument library macro called as `(qif 7)`
  raises *"if requires at least 2 arguments"*, the core form's arity error under a name the core form
  does not have; (b) the library's own body calling its own `if` answers correctly, so the macro
  exists and only the export is wrong; (c) an identical library exporting `zif` works.
  **Candidate fix, measured.** Six lines: when the phase-0 hit is a `BindingTypePrimitive`, prefer the
  library's own Expand-phase `BindingTypeSyntax` row. Answers `lib-if`; full suite green with this and
  the defect-1 fix both active. Note the asymmetry the code already carries — the phase-0 arm accepts
  "anything not Syntax", the Expand arm "anything not Variable". **Read the `findLibraryBinding`
  entry below (2026-09-08, wrong phase for a two-phase name) first: same function, same probe, and
  they may be one fix** (plan Q3).

- [ ] **Expander cost grows superlinearly with recursion depth, with no shadowing involved**
  [Performance, M, split out and re-measured 2026-09-20; **re-measured 2026-10-16, now quadratic**]:
  at `956604f0` the same shape (`(cnt a …)`, rule `((_ x rest ...) (+ 1 (cnt rest ...)))`) takes
  0.03 / 0.07 / 0.26 / 0.99 / 3.87 s wall at depth 250 / 500 / 1000 / 2000 / 4000, ≈3.9× per
  doubling. The cubic term went with the persistent scope chain, bisected to `7d4192b7` (depth 2000:
  9.4 s at its parent, 1.2 s at the merge). Not a DoS: the closeout design's appendix
  (`plans/2026-09-20-defect-closeout-design.md`) retracts that framing (plain nested `let` shows the
  same curve; Racket is quadratic too; expansion polls `ctx.Done()`, so time is bounded and only
  memory is not). Remaining work is the constant, not the shape. The record below is as filed: filed 2026-09-19 as part of the
  `if` → `cond` recursion above, which made it look like a consequence of the capture. It is not.
  Measured at HEAD with an ordinary recursive `syntax-rules` macro over a list of *n* elements — no
  `except`, nothing redefined, nothing shadowed:

  | depth | wall | ratio per doubling |
  |---|---|---|
  | 250 | 0.111 s | |
  | 500 | 0.395 s | 3.6× |
  | 1000 | 2.506 s | 6.3× |
  | 2000 | 23.33 s | 9.3× |

  Between quadratic and cubic (≈ 2^3.2 by the last step). `WithMaxExpandDepth` *does* fire, but
  `DefaultMaxExpandDepth` is 50000, so the guard is reached only long after the process is unusable.
  **A sandboxed program can pin a CPU in a dozen lines with no privileged operation**, which is why
  this sits in Tier 1 rather than Tier 4. No design yet; the first step is a profile of the
  per-level cost, not an edit.

- [ ] **A top-level `include` loses the `set!` evidence and refuses legal R7RS** [Correctness, S,
  repro constructed 2026-09-20]: this is the missing repro for **`validateInclude` never reads the
  included file** (filed 2026-09-15 below, "unpinned — no repro constructed"). Three lines:

  ```scheme
  ;; inc.scm
  (define y 1)
  (define (bump) (set! y 2))
  ;; main.scm
  (import (scheme base) (scheme write)) (include "inc.scm") (bump) (write y)
  ```

  raises *`set!: cannot mutate immutable top-level binding "y"`*. The identical two forms written
  directly in `main.scm` answer `2`, as do Racket and Chez. `finalizeStability`
  (`pkg/internal/validate/errors.go:107-118`) stamps `StableInUnit` per validation unit; an included
  file is not that unit, so the `set!` is invisible, the define is stamped `Stable`, and the
  immutable-top-level default refuses the assignment. So the consequence is not only the stale-inline
  hazard that entry describes: **it is a hard refusal of conforming code.** This is what breaks
  `examples/logic/schelog/run-all-tests.scm` at `schelog.scm:641`
  (`set!: cannot mutate immutable top-level binding "schelog:*more-k*"`), which is a *different*
  failure from the one recorded against issue #820.

### plans/ sweep — conformance and phase deltas not previously filed (2026-09-20)

Whole-`plans/` sweep, 72 files plus `WORK.local.md`, run against HEAD `e64c43a2`. Every row was
verified against source and `git log` rather than read off a plan's own status line. Only rows that
are **not** filed anywhere else in this file appear here; simplification and performance deltas from
the same sweep are under the matching headings in Tiers 5 and 4.

Most of the phase rows come from `plans/2026-07-10-climbing-tower-design.md` §6.8, which collects
what the Tier-2 analysis found *in passing*. They are independent of the Tier-2 decision: each is a
defect at HEAD whether or not Tier 2 is ever built.

- [x] **Extension-library exports resolve against the live root environment** [Done 2026-10-02,
  P3.13/I137, branch `fix/extension-library-export-env`]: a top-level `(define sqrt …)` reached an
  extension library's export, including under a rename the user never wrote.
  `registerExtensionLibraries` (`pkg/wile/engine.go`) now resolves each export ONCE, at
  registration, through `SealedBindingAt` — the startup-set tier, which neither a user define nor
  an import can reach — and records the binding on the `CompiledLibrary`
  (`SetResolvedExport`/`resolvedExports`). `findLibraryBinding` answers from that table and
  **refuses on a miss** rather than falling through to the name lookup; the refusal is the fix, not
  the table.
  **The shape matters, and the plan's own two options were both worse.** Shape (a), widening the
  `level > strictLevelOff && len(snapshots) > 0` conjunct so every engine gets a second base
  environment, is correct and costs **+29.5% ns/op, +29.9% B/op, +30.3% allocs/op** on
  `BenchmarkEngineStartupWithImport/small` (interleaved A/B, 6 pairs, two `go test -c` binaries
  from one tree) — measured, not inherited, and the right order of magnitude for the plan's +38%
  CLI figure. Shape (b) as filed ("out of the registry snapshot … no second base environment at
  all") needs new exported registry API to mint primitive bindings outside a frame. The shipped
  shape resolves out of the environment the task already had and costs **+0.23% ns/op, +0.10%
  B/op, +49 allocs/op** — ns/op inside the ±1.2% same-binary drift band, so only the alloc numbers
  are resolvable at all. Tolerance was +5% / +2% / +2%. Labelled observation, not a gate: CLI
  `-e '(display 1)'` 10.39 → 10.49 ms (30 interleaved pairs, median of best 20).
  **So `strictLevel` and its conjunct STAY**, where the task's Change paragraph had them deleted.
  The conjunct was never the shadow defence the paragraph read it as — it is a capability test,
  because the extension primitives are not in a strict engine's sealed base at all. That
  distinction is now written where it lives (`setupLibrarySystem`), and `finishEngine`'s warning
  about moving `strictLevel` onto the `Namespace` if the second conjunct ever goes is still live.
  **Gate:** `TestExtensionLibraryExportIgnoresUserShadow` (`pkg/wile/engine_library_test.go`), two
  RED rows on master. It must use `EvalProgram`, not `EvalMultiple`: the CLI's unit is one
  `(begin …)`, whose top-level hoist creates the define's slot before the import resolves, so the
  define-AFTER-import row is red only in that shape. Under `EvalMultiple` master answers it
  correctly by accident. **The §5.2 matrix P3.17 is sequenced on:** plain `(import (wile math))`
  under a prior `(define (sqrt x) 'hijacked)` still answers `hijacked`, on both sides — the user
  asked for `sqrt` and defined `sqrt`, so §1.3.2's latitude does apply and Racket agrees. Only the
  renamed shapes changed. Found during Tier-2 T2.0a, so this is where that sub-step landed.
- [x] **A module-level `define-syntax` RHS is evaluated twice** [Correctness, M, Done 2026-10-16,
  P4.6/I138, `e6267d8d` + `c7219b11` + `787a3fea`, merged `cb4a74f3`]: the expander now
  evaluates the RHS when it reaches the form (`expandDefineSyntax`) and `CompileDefineSyntax` only
  re-checks the shape and that the keyword is bound. Pinned by `TestDefineSyntaxRHSRunsOnce`. One
  double run remains, filed below as the `expand` row. Racket evaluates
  it once. Both `compileDefineSyntaxFromSyntax` and `CompileDefineSyntax` ran it, so a transformer
  with a side effect fired twice per definition. Observable, and it made any
  compile-time-state reasoning arithmetically wrong by a factor of two. Reproduced at `956604f0`:
  `(import (scheme base) (scheme write) (for-syntax (scheme base) (scheme write))) (define-syntax m
  (begin (display "X") (er-macro-transformer (lambda (f r c) 1)))) (display (m))` prints `XX1`.
- [ ] **`expand` of a `define-syntax` runs its RHS, and the result runs it again** [Correctness, S,
  filed 2026-10-16 from P4.6's crosscheck]: since P4.6 the expander evaluates a define-syntax RHS
  wherever it reaches the form, so `expand` does too. A transformer that returns `(expand
  #'(define-syntax n (begin (display "X") …)))` prints X twice: once inside `expand`, once when the
  outer expander reaches the returned form. A transformer that calls `expand` only to inspect such a
  form now binds `n` in the caller's environment, where master left it unbound; likewise a
  top-level `(expand (datum->syntax #f '(define-syntax q …)))` rebinds `q`. Racket's top-level
  `expand` behaviour is unchecked. Options: an already-expanded marker that skips the RHS, or
  accept and pin.
- [x] **`er-macro-transformer` inside a `let`** [INVERTED — the filed behaviour does not reproduce;
  measured and pinned 2026-09-26]: this row claimed such a transformer body reads the `let`'s own
  local as `#<void>`, and blamed `CompileERMacroTransformerExpr` for evaluating the lambda before
  the locals exist. **There is no `#<void>` anywhere.** `(let ((c 0)) (define-syntax m
  (er-macro-transformer (lambda (x r c*) c))) (m))` refuses, exit 1, `errors.Is(err,
  werr.ErrNoSuchBinding)`, naming `c`; adding `(define-for-syntax c 99)` above the `let` answers 99,
  exit 0. The refusal is the correct hygiene answer — `c` is a phase-0 runtime binding and the
  transformer body runs at phase 1 — and Racket agrees, erroring on the same shape with "c:
  undefined; cannot reference an identifier before its definition". So acting on this row as filed
  would have replaced a correct refusal with a silent wrong answer: a hygiene regression that no
  gate in the tree would have caught. `pkg/wile/er_macro_let_local_test.go` now pins both halves
  (the sentinel plus `no such binding "c"`, and the `define-for-syntax` escape hatch at 99). It is
  green the day it is written and is **not** a ratchet; its reason for existing is this row. Two
  things it deliberately does not assert, because the provenance wave rewrites both and a pin that
  breaks when a diagnostic improves invites its own deletion: the rendered wrap chain (nine
  `failed to …` links) and the `line:col` offset. That makes the pin **not** independent of the
  provenance work — its second assertion is coupled to the message text even in this narrowed form
  — so a change that stops naming the offending identifier lands there as a deliberate edit.
  `TestUnboundDiagnosticNamesThePhase` and `TestPhase1_ProceduralTransformerUnboundWithoutImport`
  already pin the same mechanism for a *top-level* binder; only the let-local shape was uncovered.
- [x] **`define-for-syntax` binds under the empty scope set** [Done 2026-10-02, P3.6/I140, with
  I170, branch `fix/for-syntax-binder-scopes-and-pruning`]: `runDefineForSyntax` wrote with ∅
  scopes, so a binder a macro TEMPLATE introduced was reachable from every scope set and leaked to
  any later transformer spelling it the same way. `defineForSyntaxParts` now returns the
  `*syntax.SyntaxSymbol` rather than its unwrapped `*values.Symbol` — the spelling was never the
  question, the scopes were — and `createPhaseBindingUnlessStable` takes `nameStx.Scopes()`.
  Racket and Chez both refuse the leaking reference.
  **THIS HALF ALONE IS A REGRESSION, measured three ways**, and that is why it shipped with I170
  rather than before it: a macro-SUPPLIED name wears the use-site scope the invocation stamped, so
  under the scopes argument alone a later reference stops resolving. The use-site pruner strips
  that scope back off, and it only prunes forms on its list, so `pruneDefinitionBinder` gained
  `define-for-syntax` in the same change. Measured on three builds — master fails r1/r4b, the
  scopes argument alone fails **r2**/r4b, both pass — which is the evidence the halves are
  inseparable. Gate `TestForSyntaxBinderScopes` (`pkg/wile`), six rows, two RED on master.
  **Read every row through a procedural transformer, never a `syntax-rules` template**: the
  template channel is the phase-0 emission leak in the row below, and a gate written that way
  asserts Wile keeps it.
- [x] **`cond-expand (library X)` instantiates X as a side effect** [Correctness, Done 2026-10-02,
  P3.14/I141, branch `fix/cond-expand-library-probe-purity`]: the probe was a real `LoadLibrary`,
  so asking whether a library was available printed its output and performed its mutations. R7RS
  §4.2.1 asks whether the library is available, not for its state. Compile and instantiate are now
  separate steps: `CompiledLibrary` carries a `sync.Once` plus `EnsureInstantiated`, `LoadLibrary`
  is a wrapper over `loadLibrary(…, instantiate bool)`, and the probe calls the new
  `LibraryAvailable`, which compiles and stops. **Two consequences worth knowing before touching
  it.** (1) **A criterion change, deliberate and documented** (`r7rs-differences.md` item 19): a
  library that compiles but whose BODY raises now answers *available*, where it used to answer
  *not-available* because the probe ran it and took the raise as evidence. The failure surfaces at
  the `import`. (2) **Registration now precedes instantiation**, so a probe leaves a compiled
  library findable with its body pending, and the first real import flushes it — exactly once,
  however many clauses asked first, which is what the `sync.Once` is for. **Residual, pinned as a
  labelled row:** the purity is one level deep. Probing X still instantiates the libraries X
  imports, because a library's own `import` clauses go through the ordinary loader
  (`resolveImportSets` is the shared prefix for the top-level and library-internal paths, and only
  the probe's own call is parameterised). Gate
  `TestCondExpandLibraryProbeDoesNotRunTheBody` (`pkg/wile`), five rows, **three RED on master**:
  leaf purity, probe-then-import-runs-once, and the criterion row.
- [ ] **`compileTransformerValue` / `CompileERMacroTransformerExpr` ignore engine configuration**
  [Correctness, S]: both hardcode `DefaultInlineThreshold` and `DefaultMaxExpandDepth`
  (`compile_transformer.go:68`), so `WithInlineThreshold` and `WithMaxExpandDepth` do not reach
  transformer compilation. An embedder's expansion-depth cap is therefore not honoured on exactly
  the path that can run away — see the cubic-cost entry above.
- [x] **Go map-range order decides export-map slot numbering and conflict-report order**
  [Phase / determinism, M, Done]: `CopyLibraryBindingsToEnvAtPhase` / `copyLibraryBindingsDirect`
  iterated a Go map, so slot numbers and the identifier a conflict diagnostic named varied run to
  run. A determinism leak of exactly the kind the project's own concurrency orientation names: not
  routed through a seed, so not reproducible from one. Both loops now walk `sortedExportKeys`
  (`library_registry.go`), ordered **phase before name** — a name exported at two phases denotes two
  bindings, and grouping a library's phase-0 installs ahead of its phase-1 ones keeps the install
  order the phase-shift arithmetic assumes. Measured: 20 fresh engines importing two libraries that
  export ten conflicting names named **9 distinct identifiers → 1** (`aa`, the lexicographic first),
  pinned by `TestImportConflictNamesTheSameIdentifierEveryTime` (pkg/wile), red before the sort.
  THIRD SITE DELIBERATELY NOT SORTED, so this does not get re-filed as an omission:
  `stampLibraryExportOrigins` also ranges the export map, but its body is a nil-guarded idempotent
  `UpdateMeta`, so map order is observable only if two of one library's export keys reach ONE
  `*Binding` with different `exportRoot` answers. The base arm provably cannot differ (no
  `RootPhase`); only the library-scoped arm can. Paid for with a measurement instead of a sort:
  `TestExportRootIsOneAnswerPerBinding` (pkg/wile) sweeps every library the stdlib loads — 76
  libraries, 2574 resolved export keys, 992 of them library-scoped — and asserts no two keys
  resolve to one binding with unequal roots. Green with and without the sort, so it pins the
  position rather than the fix; if it ever goes red, sort the third site. The sweep needs the
  loaded stdlib, so it lives in pkg/wile and reaches the predicate through a new exported
  `CompiledLibrary.ExportedBindingRoot`; an in-package test cannot load the stdlib, because
  bootstrap imports compilation.
- [x] **`unquote` / `unsyntax` / `with-syntax` operands are never expanded** [Correctness, M,
  Done 2026-10-15, P4.5/I144, `2882da27` + `76e81aac`, merged `956604f0`: operands expand in the
  expander pass (`expander_quasi.go`); pinned by `pkg/wile/quasi_operand_expansion_test.go`. Residual
  positions (cond-expand clause, include, syntax-case clause bodies) are tracked in the opaque-scan
  row below]:
  `` `(1 ,(when #t 2)) `` fails. R7RS §4.2.8 requires the unquoted expression to be an ordinary
  expression, macro uses included. Found in the renamed-inner-positions review, noted in the
  2026-09-19 recap as "found in review, not filed".
- [ ] **`eval` into an `(environment …)` namespace is mutable; R7RS makes it immutable**
  [Correctness, M, from `WORK.local.md:155`]: R7RS §6.12 specifies the environment returned by
  `environment` / `scheme-report-environment` as immutable, and Wile permits definition into it.
  Unmeasured against Racket and Chez; measure both before designing, because this is the kind of
  restriction §1.3.2 lets an implementation relax deliberately.
- [x] **§5.2's ordering clause is unenforced and unmeasured** [Correctness, S, Done 2026-10-06 in P3.17, merged `4ea9b5b5`; from
  `WORK.local.md:150`]: "refer to an identifier before it is imported". Distinct from the shadowing
  clause, which `plans/2026-09-20-core-form-override-and-import-portability-design.md` §4 handles;
  nothing checks the ordering half and nobody has measured what Wile does. Measure first.
  **MEASURED 2026-10-05** (P3.17 record carries the table): a program unit (`EvalProgram`)
  scopes an import over the whole unit, so a reference written before it answers the library's
  value, as a Racket module and a Chez `--program` do; `EvalMultiple` refuses it, but only
  because it refuses every forward reference, so that is not a §5.2 check. Define-before-import
  keeps the user's define in Wile and Racket; Chez `--program` REFUSES it ("multiple
  definitions"), and Chez `--script` lets the import win. **Documented** as unchecked in
  `r7rs-differences.md` (*Import order is not checked*), pinned by
  `TestImportOrderingIsNotChecked`.
- [ ] **`buildRestArg` buffer reuse is unverified against a rest-Tuple-retaining primitive**
  [Correctness, S, from `2026-07-17-pair-gc-investigation.md` Q1]: no test references `restArgBuf`
  or `buildRestArg`. If a variadic primitive retains the rest list, reuse mutates it under the
  caller. Cheap adversarial test, never written.

Scheme-syntax-forms layer (`WILE_SYNTAX_FORMS=scheme`, not the default). These do not reach a
default engine today, but they are owed before the flip and three of them are plain gaps rather
than decisions:

- [ ] **No box (`#&`) arm in `%gen-match` / `%gen-template`** [Correctness, S]: a box pattern never
  binds and a box template stays a verbatim constant. The Go layer has
  `TestBoxInMacroTemplateAndPattern`; the Scheme layer has no twin.
- [ ] **An imported `syntax-rules` is called as a closure: "not a closure: \*compilation.SyntaxCompiler"**
  [Correctness, M, filed 2026-10-16 from P5.9's crosscheck; cause NOT established]: every
  `syntax-rules` reached through an import fails under the layer, at `a23c4543` and on
  `f3c0910b` alike. Measured with `wile -q -e`:
  - `(eval '(define-syntax m (syntax-rules () ((_) 42))) (environment '(scheme base)))`, a LONE
    define-syntax, raises `not a closure: *compilation.SyntaxCompiler`, so such a namespace
    cannot define a `syntax-rules` macro at all. P5.9's five dispatch rows in
    `TestImportSetNamespaceDispatchesSpecialForms` fail the same way.
  - At phase 1 via `(for-syntax (scheme base))`: `(define-syntax m (begin 1 (syntax-rules () ((_)
    1))))` fails the same way, which is why `TestDefineSyntaxRHSRunsOnce` (P4.6) is red under the
    layer on master too.
  - Control: a top-level `(define-syntax m (syntax-rules () ((_) 1)))` with no import answers 1.

  Hypothesis, unverified: an import copies the Go `syntax-rules` keyword row, whose value is the
  `*SyntaxCompiler`, while the layer's macro lives only in the engine's own sealed phase-1 tier, so
  macro dispatch finds a syntax-typed binding that is not a transformer. If so, it is the I171
  shape for the eight `schemeSyntaxFormNames`, which P5.9's root probe deliberately skips (it
  returns only a `*PrimitiveExpander`). Repro both layers before choosing a site.
- [x] **`%gen-ellipsis-map` raises on a constant subtemplate followed by `...`** [Correctness, S,
  Done]: the two layers now AGREE, by moving the Go layer rather than the Scheme one. The row's
  premise — "the Go layer and Wile's reading of R7RS §4.3.2 both answer `()`" — was half right and
  the reading was wrong. §4.3.2 states only the ∀ direction (every driver must be bound at least as
  deep as the template ellipsis depth) and is silent on the ∃ direction; both reference
  implementations resolve that silence by refusing — petite "extra ellipsis in syntax form", exit
  255; racket "no pattern variables before ellipsis in template", exit 1; `racket -I r5rs` the
  same. Two oracles agreeing where the spec is silent is this project's standard for taking their
  answer, and Wile's Go layer was alone in answering `()`. `checkEllipsisGroupDriver` no longer
  exempts the zero-variable case: a sub-template with no pattern variable of sufficient depth has no
  driver whether it has zero variables or only shallow ones, and the zero case was never an
  exemption.
  TWO SITES, not the one the plan named. `syntax-case` does not route through
  `checkEllipsisGroupDriver` at all — its template is walked at use, by
  `expandSyntaxEllipsis` (`pkg/internal/match`), which had the same silent drop. Refusing only the
  `syntax-rules` site leaves the `syntax-case` shape answering `()`. Note for P2: this puts the task
  in TWO of the seven freeze-window packages, not one.
  TWO SENTINELS, deliberately: `werr.ErrInvalidSyntax` from the compile path and `werr.ErrExpansion`
  from the expansion path, each the dominant sentinel of its own package — `ErrExpansion` is what
  the adjacent arms of `expandSyntaxEllipsis` already raise.
  REFUSAL IS AT CONSTRUCTION for `syntax-rules`, so a bad definition is refused even when the macro
  is never used, which is what petite does with a definition-only script. The plan's stated bootstrap
  risk (one such template anywhere in `pkg/stdlib/lib` would break the image for every engine) was
  measured and did not materialize: `go test ./...`, `test/run-all.sh` (all Scheme suites) and
  `make test-examples` (56 passed, 2 skipped, 0 failed) are all green with the refusal in.
  Pinned by `TestI149_ConstantSubtemplateFollowedByEllipsisIsRefused` (pkg/wile), four shapes, each
  verified red with the two production edits reverted and green with them in. The fourth shape is
  definition-only. The `constant template followed by ellipsis` row in
  `syntax_rules_depth_test.go`'s no-false-positives table, which pinned `()` with a §4.3.2 rationale
  comment, is removed there and inverted into that gate — it was a true positive, not a false one.
  RESIDUAL, not this row's: the `syntax-case` refusal renders as the generic "syntax: template
  expansion error" because `operation_syntax_case.go`'s `mc.WrapError` does not surface its cause's
  message. The `Unwrap` edge is intact (`errors.Is` reaches `ErrExpansion`, which is what the gate
  asserts), so this is a rendering gap affecting every syntax-case template error, not a lost chain.
  Filed for the provenance wave rather than fixed here.
- [x] **Scheme layer instantiates phase 3 at startup where Go stops at phase 2** [Phase, S, decided
  and Done 2026-10-06 in P3.17, merged `4ea9b5b5`]: `TestStartupBindsNothingAtPhaseTwo` pinned the Go layer only. Needs a decision, not a
  fix. **Decision proposed in P3.17 (re-measured 2026-10-05):** the phase-3 frame is a footprint
  fact, not a binding one. No SLOT exists above `PhaseExpand` on either layer (`ExactBindingAt`;
  the dialect's macro-phase rows answer at every macro phase by design and are excluded), so the
  `PresentPhases` DeepEquals, RED only under `WILE_SYNTAX_FORMS=scheme`,
  becomes a layer-independent binding invariant plus a per-layer footprint bound (Go <= 3,
  Scheme <= 4). **Taken:** `TestStartupBindsNothingAboveExpand` builds both layers explicitly and
  passes on each. Its binding probe is `ExactBindingAt` with a phase-1 control: the old
  `OwnGlobalIndex` probe read the mutable tier only and found nothing at ANY phase, so it could
  never fail.
- [ ] **Cross-library pattern-literal residual: the Scheme layer refuses what Go accepts** [Phase /
  correctness, S, undecided]: `TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding`. Which
  answer is right is the open question; the two layers disagreeing is not acceptable either way.

### Defects from the 2026-09-14 doc-audit sweep

Found while checking docs against code (`99b8c99d`, `032728ab`) and while fixing what that pass
reproduced. Every fixed item has a regression test that fails on `032728ab`.

**Fixed, MERGED 2026-09-14 (`032728ab..6ce2f4c1`)**, all eleven plus
`fix/phase-shifted-expander-root` and `fix/let-merge-capture-safe` (entry below). Branches deleted.

| Branch | Fixes |
|---|---|
| `fix/environment-authorizer-under-evalin` | SECURITY. `environment`, `scheme-report-environment`, `null-environment`, `make-namespace`, `namespace-require` built from the registering namespace, so a stricter child authorizer under `EvalIn` was escaped. Also `checkProfileWidening` now reads `EffectiveAuthorizer` |
| `fix/containment-symlink-dotdot` | SECURITY. `<root>/link/..` passed `containedInRoot` by spelling and `set-current-directory!` left the root. A `..` that backs out of any symlink is now refused, even one that stays inside the root (the `os.Root` rule) |
| `fix/composable-callcc-pooled-frame` | `GraftContinuation` hung a shared segment on an unshared chain; `call/cc` inside a 2+-frame segment stopped `MarkChainShared` early and a pooled frame was recycled (index-out-of-range panic, or silent wrong values) |
| `fix/continuation-marks-under-tail-prompt` | A mark was recorded twice under a tail-position prompt/barrier/`call-with-values` (`RunBodyUnderFrame`), and `appendChainMarks` kept the prompt frame's own marks |
| `fix/read-shares-port-position` | `read` held look-ahead past the datum, so `read-char` after `read` was desynchronised (R7RS §6.13.2). Side effect: an error just after a datum now surfaces on the next read |
| `fix/let-syntax-body-literals` | `else`/`=>` failed in any `let-syntax` body: `literalScopesMatchWithDef` refused a literal carrying the body scope. `filterRebindingScopes` deleted; `Scope.IsRebinding` is now never read (public API, left) |
| `fix/define-for-syntax-within-unit` (on `fix/phase-shifted-expander-root`) | `define-for-syntax`/`begin-for-syntax`/`eval-when` ran at compile time, after the unit's `define-syntax` transformers were built. They now run during expansion. Side effects: inside `(meta …)` they no longer run; a transformer that `expand`s a subform containing one and returns it runs it twice; phase-1 code ignores `WithInlineThreshold` |
| `fix/phased-import-defects` | `(for-syntax A B)` / `for-template` / `for-meta` dropped every set after the first. Go `syntax-case` clause bodies read enclosing locals boxed (`#&7`) |
| `perf/optimize-eval-load-compile` | `compile`'s returned thunk skipped `Optimize()` (-27%). The report that `eval`/`load` procedures were unoptimized was false: `compileClosureBody` already optimizes them |
| `chore/stale-comments-and-doc-test` | `TestTrackedDocsDoNotReferenceIgnoredPaths` (exit 128 through the tracked symlinks); 23 files of stale comments; the 8 binding-tier ordinal labels (entry below); `compare-schemes.sh` for Chez/Racket |
| `docs/semantic-pass-remaining` | algebra, types, numeric, concurrency, coverage, learn docs checked against code |

**Open, needs a decision.**

- [x] **A `let` inside a procedure is not fresh when a continuation re-enters it** [High,
  correctness regression, Done 2026-09-14]: `6a017fa1` allocated a `let`'s slots in the enclosing
  procedure's frame, so re-entry wrote the slot a closure or continuation from the earlier pass
  still read (composable re-invocation `0 1 2 2`, Racket `0 1 2 1`). A `let` now merges only into
  a frame no continuation can be captured under (`29b9babc`); a capturing `let*`, whose frame
  exists before its inits run and was wrong before `6a017fa1`, compiles as nested lets
  (`9469c442`). Both MERGED in `6ce2f4c1`. Zebra +5.8%, deriv ~+2%, rest flat. Rejected: copying
  the frame on re-entry, unsound because assigned-but-uncaptured locals live unboxed. Internal
  `define`/`letrec` reuse the location too, which R7RS letrec* arguably permits and Chez/Racket do
  not do. Pinned by `TestLetIsFreshWhenAContinuationReentersIt`, `TestLetMergeFollowsCaptureSafety`.
- [x] **Composable continuation re-invocation** [High, Done 2026-09-14,
  `fix/composable-reinvocation-live-segment`]: re-invoking `kc` inside its own running segment
  (two or more frames; `call/cc` under a prompt inside its own run too) made `AcquireSegment` set
  `p.bottom.parent = nil` on a live frame, cutting the program off below the segment. Re-invocation
  now copies through the bottom frame (`DeepCopyThrough`) and writes no original. Trade-off: the
  original bottom keeps its graft edge, so a multi-shot continuation retains its first invoker's
  chain until dropped. Pinned by `TestComposableContinuationReinvocationKeepsLiveSegment`. The
  second filed symptom, locals leaking between invocations, was the `let` item above: ten binder
  shapes match Racket on `6ce2f4c1`.
- [x] **What an `(environment …)` namespace sees at phase ≥ 1** [Medium, Done 2026-09-14,
  `fix/profile-env-macro-phase-rows`]: an import-spec `(environment …)` stays empty above phase 0
  (R7RS §6.12; Racket's `make-base-namespace` phase 1 is empty), pinned by
  `TestImportSetEnvironmentIsEmptyAboveRuntime`. A profile environment carries the engine root's
  language rows: the row installers and vocabulary moved to
  `pkg/internal/bootstrap/language_rows.go` and `initializeEnvironmentWithRegistry` installs them,
  so both bootstrap sequences share one definition (a hook set from `pkg/wile` would have left
  testhelpers environments without them). Before, phase 1 worked in `(environment '(wile small))`
  only through the registry's exact phase-1 slots, and a phase-2 transformer body could not see
  `list`. Pinned by `TestProfileEnvironmentHasTheMacroVocabularyAtEveryPhase` and
  `TestBootstrapOwnersHaveTheMacroVocabularyAtPhaseTwo`.
- [x] **Schelog no longer runs unmodified** [Medium, docs claim]: decided 2026-09-15, keep phase
  separation. `schelog.scm` now carries `(import (for-syntax (scheme base) (scheme cxr)))`; all 13
  `run-all-tests.sh` cases pass and the zebra bench runs. README and BIBLIOGRAPHY no longer claim
  "unmodified" or `syntax-rules` (all 14 transformers are procedural). `test-schelog` runs only in
  `make cd`, which is why CI stayed green.
- [ ] **A macro defined in an `include`d file is unbound in the including file** [Medium, filed
  2026-09-15 as #820, not bisected]: `t6.scm` = `(define-syntax foo (syntax-rules () ((_ x) x)))`,
  `t7.scm` = `(include "t6.scm") (display (foo 42))`. `wile -f t7.scm` (with or without `-i`, and
  with an explicit `(begin …)` wrap) fails "no such local or global binding foo"; `wile -i < t7.scm`
  prints 42. A *sibling* include sees it (`(include "defs.scm") (include "use.scm")` works, which is
  why `stress-test.scm` passes); forms written in the file holding the `include` do not, whether
  that file is the `-f` file or itself included. A procedure defined the same way is visible.
  R7RS §4.1.7: `include` is `begin` over the file's contents, no new scope. Breaks `examples/logic/schelog/run-all-tests.scm` (`%which` unbound at line 65); its README
  instruction was removed 2026-09-15, restore it when this is fixed.
- [ ] **An included file cannot see a binding local to its includer** [Bug, Correctness, filed
  2026-10-16, fix with P5.7; revisit the schedule when P4 closes]: the reverse direction of the row
  above. f.scm = `(list x (m))`; `(let ((x 5)) (define-syntax m (syntax-rules () ((_) 42)))
  (include "f.scm"))` raises `f.scm:1:6: no such local or global binding "x"` at `a23c4543`; petite
  and racket 9.2 answer `(5 42)`. Same refusal for a `let-syntax` keyword and a body `define`; a
  top-level binding works only because it carries the empty scope set. Cause: `readIncludedForms`
  returns unscoped forms and the only stamp is the library scope in
  `processFormsWithLetrecSemantics`. The fix is P5.7's step 3 (stamp the `include` identifier's
  scopes onto the read forms); `TestIncludedFormsSeeIncluderLocals` (not yet written) was added 2026-10-16
  to P5.7's gate as group (4), because P5.7's other include gates pass under a splice that skips the stamp. A
  compiler-side point fix was declined: it needs the keyword routed to `compileIncludeImpl`, a
  dispatch seam P5.7 deletes. R7RS §4.1.7.
- [x] **Library export of a name the library did not define** [Done 2026-09-16, branch
  `feat/export-for-syntax`; see `findLibraryBinding` entry below]: the two-phase *defined* case
  exports phase 0 (measured 2026-09-14, matches Racket). A re-exported `syntax-rules` reaches a
  transformer only through its phase-1 row: `(export (for-syntax (rename syntax-rules my-sr)))`
  makes `(define-syntax two (my-sr () ((_) 2)))` work, and a plain `(export (rename syntax-rules
  my-sr))` refuses it, as Racket v9.2's `rename-out` does (both pinned by
  `TestLibraryExportRoundTripByPhase`). A `begin-for-syntax`-only define is refused at export, as
  Racket refuses it at `provide` (`TestLibraryExportRefusesNameNotBoundAtItsPhase`).

**Open, no decision needed.**

- [ ] `call-with-immediate-continuation-mark` sees an outer frame's mark after a non-tail call
  inside a procedure, to a primitive or a user procedure: `(define (f) (wcm 'a 1 (list (cicm 'a
  (lambda (v) v) 'default))))` gives `(1)`, Racket `(default)` (`GetImmediateMark` fallback). At top
  level it answers `default` (re-measured 2026-10-16).
- [ ] `(continuation-marks k)` for a `call/cc` captured in tail position of
  `with-continuation-mark` inside a procedure omits the capturing frame's marks (Wile `()`,
  Racket `(2)`).
- [ ] A literal's binding captured when a nested `syntax-rules` transformer compiles misses an
  enclosing `let-syntax`-bound `else`: `(let-syntax ((else …)) (let-syntax ((m (syntax-rules
  (else) …))) (m else)))` gives `other`, Chez `lit`.
- [x] A macro expanding to `(begin-for-syntax (define (name x) …))` leaves `name` invisible to
  a later transformer, one form per unit [Done 2026-10-02, P3.6/I170, with I140, branch
  `fix/for-syntax-binder-scopes-and-pruning`]. **The row as filed is only half true, and the
  distinction is the finding:** a name the macro's TEMPLATE writes stays invisible, and should —
  Racket refuses the same shape, so that half was never a defect. What was broken is a
  pattern-SUPPLIED name, which the user wrote: `((_ f) (begin-for-syntax (define (f x) …)))` then
  `(gen trp)`. `pruneUseSiteScopes` now routes `begin-for-syntax` to
  `pruneUseSiteScopesInBegin` alongside `begin`, so the definitions inside it get their binders
  pruned. **Residual, deliberately left open, and it now has its own row below** (r5b — refused
  before AND after, so the new route widened where that gap is reachable from rather than creating
  it). **The cause this clause gave is sufficient for the macro route only**: a hand-written
  `(begin-for-syntax (define-values (a b) (values 1 2)))` is refused before the pruner is ever
  consulted, because `values` is unbound at phase 1. Corrected 2026-10-02 by measurement; see both
  rows below.
- [ ] A `syntax-rules` TEMPLATE can read a `define-for-syntax` binding, so a phase-0 reference
  resolves against a phase-1 slot [filed 2026-10-01, measured against Racket]:
  `(define-for-syntax helper 42)` then `(define-syntax use (syntax-rules () ((_) helper)))` and
  `(use)` displays `42`; Racket with `(require (for-syntax racket/base))` answers
  `helper: unbound identifier`. A template emits phase-0 code and a `define-for-syntax` name is
  not bound there, so the reference should not resolve. The correct channel agrees: referencing
  the same name from phase-1 code, `(define-for-syntax probe helper)`, compiles in both.
  Discovered while measuring the `define-for-syntax` scopes/pruner pair — the leak is what made
  that pair's table read `42` where Racket refuses, so **re-measure any for-syntax visibility
  claim on the phase-1 channel, never through a template**.
  **Still open after that pair shipped (P3.6, 2026-10-02), and the measurement says something
  about how to fix it.** Of the three builds measured, the only one that REFUSES this channel is
  the scopes argument without the pruner widening — i.e. the half that also breaks a
  macro-supplied name. The shipped change restores the supplied name on both channels, so it
  restores this leak too, deliberately. **Do not close this row by narrowing
  `pruneDefinitionBinder` or `pruneUseSiteScopes`**: that reddens r2 and r4b of
  `TestForSyntaxBinderScopes`. The fix belongs where a phase-0 reference is RESOLVED against a
  phase-1 slot, not where the binder's scopes are decided. Chez's diagnostic names the rule best:
  `attempt to reference out-of-context identifier helper`.
- [ ] A macro-SUPPLIED binder under `(begin-for-syntax (define-values (a b) …))`, and its
  `define-record-type` twin, is refused where Racket answers 3 [r5b of P3.6, filed as its own row
  2026-10-02; **the residual clause it was filed in names one cause and there are TWO, stacked**].
  **Cause 1, the one on record, is real and is sufficient for the MACRO route:** `define-values`
  and `define-record-type` are not on `pruneDefinitionBinder`'s form gate
  (`expander_use_site.go`), so a supplied binder keeps the invocation's use-site scope and no
  later reference reaches it. Measured by patch, not argued: routing `define-values` to a
  formals-list walk that prunes every name in the list makes
  `TestForSyntaxSuppliedDefineValuesStillRefused`'s program answer 3, while the
  template-introduced twin stays refused — which is right, Racket refuses that shape too, exactly
  as it does r4. **Cause 2, NOT on record, and the shape a user writes first:** the hand-written
  `(begin-for-syntax (define-values (pp qq) (values 1 2)))` never reaches the pruner at all — it
  is refused on `values`, which is unbound at phase 1 (row below). Racket answers 3 on BOTH
  shapes. So closing cause 1 alone closes the macro route and leaves the plain one broken, and a
  row that reads "add the two forms to the list" would have been believed. Pinned as a
  flip-when-closed assertion in `TestForSyntaxSuppliedDefineValuesStillRefused` (`pkg/wile`).
- [ ] `values` and `call-with-values` are unbound at phase 1 [measured 2026-10-02 while
  re-deriving r5b's cause]: `(begin-for-syntax (define x (values 1)))` is refused with `no such
  binding "values" … at phase 1 of this unit's macro tower`. The "Multiple values" block in
  `pkg/registry/core/control.go` registers `PhaseSetRuntime` alone, as does every other block in
  that file (`call/cc`, `call-with-exit`, the barrier), while arithmetic, lists, strings,
  characters, pairs and equality all register `PhaseSetRuntime|PhaseSetExpand` (the R6RS hashes did
  too, until 2026-10-04). Same class
  as the four-hash phase-asymmetry row further down, and `pkg/extensions/io/register.go` is
  runtime-only too, so `(begin-for-syntax (display x))` is refused for the same reason. **A
  TEMPLATE-introduced occurrence resolves anyway, and that is a leak in the opposite direction
  from the row above:** `(define-syntax g (syntax-rules () ((_) (begin-for-syntax (display (values
  7))))))` then `(g)` prints 7, where Racket refuses with `displayln: unbound identifier; also, no
  #%app syntax transformer is bound in the transformer phase`. A free template identifier resolves
  at its definition site, which is phase 0; the identifier the user wrote resolves at phase 1 and
  is refused. The two halves are therefore not one fix — adding `PhaseSetExpand` legalises what
  the user wrote and leaves the template route bypassing the phase gate for everything that is
  genuinely runtime-only.
- [x] `(eval '(begin (define-syntax m …) (m)) (environment '(scheme base)))` gives `no such
  binding "m"`; two separate `eval`s work. [Done 2026-10-16, P5.9/I171, `f3c0910b` + `323f2580` + `70211cca`,
  merged `775ba045`]: the namespace's store had the phase-0 keyword rows but not
  the phase-1 handler rows, so the expander read every special form there as a call while the
  compiler dispatched it. `lookupHeadPrimitiveExpander` now probes the engine root's sealed tier.
  Same cause closed `(eval '(quote (m)) ns)` answering 42 instead of `(m)`. Pinned by
  `TestImportSetNamespaceDispatchesSpecialForms`. One recorded answer flipped, to Racket's: a
  phase-2 transformer body under `(environment '(scheme base) '(for-syntax (scheme base)))`
  answered 5 only because `let-syntax` was read as a call and its transformer built a phase low;
  it now refuses `list` at phase 2 and needs `(for-meta 2 (scheme base))`
  (`TestImportSetEnvironmentIsEmptyAboveRuntime`). The dispatch half covers the Go syntax layer
  only: under `WILE_SYNTAX_FORMS=scheme` the `begin`, body and `quote` rows and their prefixed twins
  still fail as on `a23c4543` (see the "imported `syntax-rules` is called as a closure" row); the
  phase-2 rows pass under both layers.
- [ ] **An import-set namespace compiles special forms it did not import** [Correctness, filed
  2026-10-16 from P5.9's re-review; cause NOT traced]: under `(environment '(only (scheme base)
  car))`, `(eval 'define-syntax env)` is unbound, yet `quote`, `if` and `begin` compile. The
  expander reads such a head as a call (its `DenotedForm` is `""`) while the compiler dispatches it
  by spelling: the I171 split from the other side. Same compiler-side spelling dispatch as the
  `null-environment` row below; likely one site. `define-syntax` now refuses (P4.6) when its
  keyword is unbound: `CompileDefineSyntax` binds nothing since the RHS moved into the expander,
  so it raises "m was not defined" rather than accept a no-op. Before P4.6 a lone one bound `m`.
  The refusal checks only that the keyword is bound as syntax, so a keyword that already is passes
  as a silent no-op: under `(only (scheme base) car when)`, `(define-syntax when (syntax-rules ()
  ((_) 42)))` is accepted and the old `when` stays (master answered 42), and after
  `(begin-for-syntax (define-syntax mm …))` a `(meta (define-syntax mm …))` passes without running
  its RHS. Fixing the spelling dispatch here closes both; a per-form "reached by the expander"
  record in `CompileDefineSyntax` is the narrower alternative.
- [ ] **`null-environment` binds no syntactic keywords** [Conformance, filed 2026-10-16 as P5.9's
  follow-on]: R5RS §6.5 requires `(null-environment 5)` to bind them. `(eval 'quote
  (null-environment 5))` raises `no such local or global binding "quote"`, and `(eval '(begin
  (define-syntax m (syntax-rules () ((_) 42))) (m)) (null-environment 5))` raises `no such binding
  "m"`. Forms still compile, because the compiler dispatches an unbound head by spelling, but the
  expander does not, and P5.9's root probe fires only where the head resolves to a keyword
  (`DenotedForm` is `""` here). Site: `PrimNullEnvironment` (`extensions/eval/prim_eval.go`).
- [ ] Go ER transformers receive plain symbols, so `(datum->syntax (car f) …)` fails where the
  Scheme syntax forms work. (The whole-clause `_` half closed with P4.4, 2026-10-15.)
- [ ] Numeric: `(sqrt #m2)` raises (`PrimSqrt` has no `BigFloat` case); the Inf/NaN guard in
  `pkg/values/promotion.go` (`lubIsComplex` branch) rounds a `BigComplex` operand to float64.
- [ ] Algebra: `run-analysis` returns all-`#f` outputs for a 3-block forward chain where block 1
  should be `#t`; `rewrite` treats the term comparison as boolean, `unification` as three-way
  (`ac-match` raises `=: expected number, got #f`).
- [ ] `(environment '(wile algebra))` fails with "unknown wile profile": every `(wile X)` is read
  as a profile name.
- [ ] Coverage: `stdlibPrefixes` (`coverage/gocover.go`) omits the embedded `chibi/` and `rnrs/`.
- [ ] Stale: `waitOnCondCtx` (`pkg/values/cond_wait.go`) and `LockContext`
  (`pkg/values/mutex.go`) say a terminated holder's lock stays held (`Thread.Terminate`
  abandons it); tutorial chapter 06 calls C_6 and 2·K_3 cospectral, chapter 05 lists a
  nonexistent `(wile algebra field)`; `Makefile` `bench-gabriel-compare` names `ack`.
- [ ] `read-syntax` positions do not count characters `read-char` consumed between reads (needs
  port-owned position tracking).
- [x] `set-current-directory!` still has a check-then-`os.Chdir` window. [Done 2026-10-16, P5.1
  `fix/engine-scoped-current-directory`, merged `5cb05fc6` — there is no `os.Chdir` left to race: the directory is
  engine state and every relative path is resolved against it before the gate]. Two proposed gates
  DROPPED, recorded rather than omitted. (1) An `os.Getwd` ruleguard ban (I186-R1b): production
  sites went 4 → 2, not to 0. `Namespace.CurrentDirectory`'s live-read fallback is the one channel and
  must keep it, and `pkg/security/path_containment.go` has no namespace to consult and sits under
  the `.golangci.yml` exclusion of `pkg/security/` from gocritic, so ruleguard cannot see it; a ban
  would be an allowlist of both survivors, not a ratchet. (2) `t.Parallel()` on the integration
  `set-current-directory!` tests (I186-R7): those tests no longer chdir, but
  33 sites across 9 test files (this task's own pins among them) still
  `t.Chdir` or `os.Chdir` to arrange a host directory, so parallelizing is a separate cleanup with
  nothing to prove here

### `Imported` is unsound evidence for `IsStable()` at phase 1 — DISCHARGED 2026-08-10, one refusal left (2026-08-09)

Opened by the wave-1 §5 / Phase 5 work, and it is the **named owner** for the
residual that work deliberately left standing. `Binding.IsStable()` returns
`m.Imported || m.Stable` (`pkg/environment/binding.go`), on the premise that a
define can no longer reach an import's slot. The T2 relocation made that premise
true at phase 0 only, and these three rows were the phase-1 gap.

**Discharged, re-verified 2026-08-14 by mutation:** `Imported` is sound evidence at phase 1 from
two directions: every route that rebinds an import's slot clears the flag (item 1), and the one
`Stable`-stamped phase-1 population is no longer user-writable (item 3). Item 2 is not work: it is
a **recorded refusal**, kept so nobody "finishes the job" by flipping a constant.

- [x] **`define-syntax` over an imported macro superseded in place and left `imported=true`**
      [Done 2026-08-09, `297bb18b`; the SUPERSEDE it fixed was itself replaced 2026-10-02]:
      `m.Imported = false` on the supersede (`compile_define_syntax.go`), symmetric with the
      variable path (R7RS §5.3.1). The `UpdateMeta` call sat behind `docstring != ""`, so it now
      runs for every `define-syntax`. Gate (renamed) `TestDefineSyntaxShadowsImportedMacro`
      (`pkg/wile/immutable_import_test.go`).

- [ ] **RE-OPENED 2026-10-02: the `m.Imported = false` clear in `compile_define_syntax.go` is now
      DEAD CODE** [Small, filed by P3.11/I128]: since a top-level `define-syntax` over an imported
      macro SHADOWS rather than superseding in place, the binding that `UpdateMeta` runs on is the
      user's own freshly created slot, which was never imported — so the clear always writes
      `false` over `false`. **Measured:** replacing the statement with a no-op reddens nothing in
      `pkg/wile` or `pkg/machine/compilation`. Not deleted yet, deliberately: the same `UpdateMeta`
      call still carries the docstring write, and a `define-syntax` that REUSES a mutable slot (a
      second `define-syntax` of one name) is a path the measurement above does not separate. Decide
      whether the clear is removable or whether that second path still needs it, and say which in
      the comment either way. The comment already records the dead state and the open question
      (`52ca2e72`); update it with the decision.

- [x] **The phase-1 import installs CAN take the shadowable tier, and the argument against it
      was wrong** [Done 2026-10-01, P3.10/I127, branch `fix/import-install-phase-general`]:
      `installImportedBinding`'s `&& env.PhaseLevel() == environment.PhaseRuntime` conjunct is
      deleted, so `placementShadowable` routes through `CreateImportedGlobalBindingAt` at every
      phase. **The refuted premise:** this row said an imported macro would land on a bootstrap
      macro's exact coordinates and `CreateGlobalBindingAt` would reuse the slot. It cannot —
      `createGlobalBindingAt`'s `sealed && IsImported() != imported` refusal carries **no phase
      restriction**, and a bootstrap macro is not `IsImported()`, so the refusal fires, no slot is
      reused, and the import gets one of its own at `tierExactImported`. The separation is the
      STAMP, not the phase. **The mutation this row and the test header both advertised is a
      MEASURED NO-OP**, before and after the deletion: flipping either `placementInPlace` to
      `placementShadowable` leaves `TestImportDoesNotOverwriteSealedBootstrapMacro` and
      `TestImportedBindingTakesTheSealedPhaseZeroTier` green, because the placement constant cannot
      bridge the stamp. The recipe that does redden it bypasses the stamp instead, and the test
      header now carries it. `TestOrderSymmetryMatrix` (`pkg/wile`) is the new ratchet; its two
      phase-0 rows pass on master and are the non-vacuity guard. What relocating the PROPAGATED
      install changes is the shadow-versus-supersede answer for a phase-1 `define-syntax` over an
      imported macro, which is a semantic fork with two priced options, not a safety question.

- [x] **The phase-1 registry copy was stamped `Stable` over an open writer set** [Done
      2026-08-10, `24e08ceb`]: `phaseTargets` bound `PhaseExpand` through `env.Expand()`, putting
      the 146 copies at `(1, mutable)`, the coordinate a top-level `define-for-syntax` writes. The
      fix routes `bindingEnv` through `env.SealedWriteViewAt(PhaseExpand)`; `closureEnv` stays
      `expandEnv`, since user code resolves through it and the sealed-write view hides every user
      define. **So `Stable` is stamped only where the writer set is closed at phase 1 too, and
      `define-for-syntax` SHADOWS the registry copy (a new slot) rather than superseding it.**
      Pinned by `TestBindingModelMatrix`'s three M7 rows, of which only the third discriminates:
      the preservation and by-name rows read the same under supersede and shadow.

### `findLibraryBinding` exports the wrong phase for a two-phase name (2026-09-08)

- [x] **Make the export walk pick the phase the importer needs, not the lowest one**
  [High, M, WITHDRAWN 2026-09-16, filed 2026-09-08 while checking whether an importable
  `syntax-rules` is possible for the Flatt Stage A design
  (`memory/2026-09-08-flatt-binding-model-a-design`, §9 Q3)]:
  `findLibraryBinding` (`pkg/machine/compilation/library_bindings.go`) walks
  `lib.Env.PresentPhases()` and returns the **first** hit. `PresentPhases`
  (`pkg/environment/environment_frame.go`) sorts ascending and trims below `PhaseRuntime`,
  measured as `[runtime expand phase(2)]`, so **phase 0 always wins**. For a name bound at
  two phases the export takes the phase-0 binding, which is not the one an importer of a
  syntactic name needs.

  **Measured, round-tripped through a real library** (`(export (rename X Y))`, then import
  and use). Pinned by `pkg/wile/library_export_phase_order_test.go`:

  | Exported | Import lands as | Usable |
  |---|---|---|
  | `let-syntax` (phase 1 only) | `*compilation.PrimitiveExpander` at exact@1 and exact@0 | yes — the renamed name expands, `(my-let-syntax ((m (syntax-rules () ((_) 42)))) (m))` is 42 |
  | `syntax-rules` (phase-0 `SyntaxCompiler` **and** phase-1 `PrimitiveExpander`) | `*compilation.SyntaxCompiler` at exact@0 only | no — `(define-syntax two (my-sr () ((_) 2)))` raises `no such local or global binding "my-sr"` |
  | `...` (ambient keyword row only) | `values.voidType` at exact@0 | no, and ellipsis is name-keyed in the matcher anyway (`match.DefaultEllipsis`) |

  **The failure is silent.** All three pass `validateLibraryExports`
  (`pkg/machine/compilation/compile_library_forms.go`), which asks only whether *some*
  binding is reachable via `findLibraryBinding` — the same first-hit walk. So export
  validation passing is not evidence the export is usable, and a library can ship an
  export that resolves to nothing at the use site.

  **Why it matters beyond the bug.** Stage A's §9 Q3 (does a `BulkSource` expose the whole
  store or a curated export list?) inherits this: a whole-store source must answer "which
  phase's slot does this name supply?", and "the lowest present one" is the wrong answer.
  A per-phase curated export list sidesteps it. It also blocks the idea of moving
  `syntax-rules` into an importable library so the phase-1 vocabulary is declared rather
  than hardcoded — the one name that motivates it is the failing row.

  **WITHDRAWN 2026-09-16:** exports now declare their phase (`(for-syntax ...)`, branch
  `feat/export-for-syntax`, Racket's `provide`), and `findLibraryBinding` takes the export's
  phase, not the importing one. Shifting an import shifts what an export denotes; it never
  re-selects it. Fork (a) below would contradict that. The record that follows is kept as
  history.

  ~~**Open fork, not decided.**~~ **DECIDED (a), 2026-09-10** (Stage B impl fork F). Either
  (a) `findLibraryBinding` takes the requesting phase and
  prefers a match there, falling back to lower phases, or (b) it returns every phase's
  binding and the install site picks. (b) is closer to what a bulk row wants but touches
  `importConflicts`, which currently compares one binding to one binding. Related:
  §4.6 of the Stage A design asserts the walk "probes phases for a *definition*" — true, but
  it does not say which definition wins, which is the whole defect.

  **Why (a), and what changed the answer.** Stage B settled fork A as **A2** — the phase-1
  reader gates STAY and imports are NOT routed through bulk rows — which removed (b)'s stated
  justification twice over. "(b) is closer to what a bulk row wants" is moot when there is no
  bulk row, and (b) would additionally have widened `importConflicts` from
  one-binding-vs-one-binding to set-vs-set for no remaining benefit. This stays a TODO.md item;
  **it is not Stage B work and nothing in Stage B depended on it.**

  **The obligation that rides with (a), and it is the half that will be forgotten.**
  `validateLibraryExports` uses the **same first-hit walk**, so **export validation passing is
  not evidence the export is usable**. Fixing `findLibraryBinding` alone leaves the false green
  standing: validation would keep certifying an export that resolves to nothing at the use
  site. The validator must ask the SAME widened question, in the same change.

  **Link to the phase-1 population question, in both directions** (Stage B fork E, declined
  below and in the design's §9 Q1): an importable `syntax-rules` is the motivating name here,
  and it is also the thing that would let the phase-1 vocabulary be **declared** rather than
  hardcoded. The recorded sequence is: identity fix across the import edge → **this item** →
  vocabulary becomes declarable → only then is Racket's empty phase 1 reachable. Doing them out
  of order does not work.

  **2026-09-15:** the motivating row (renamed `syntax-rules` in a transformer RHS,
  `TestLibraryExportRoundTripByPhase/syntax-rules_plain_export_does_not`) turns out to be a phase-isolation
  question, not an export-walk defect. Under default phase isolation a plain `export` binds at
  phase 0 only, matching Racket's `rename-out`: `(only-meta-in 0 (prefix-in b: racket/base))`
  refuses `b:syntax-rules` in a transformer RHS, and `racket/base` declares exactly `...`, `_`,
  `syntax-id-rules`, `syntax-rules` at phase 1. The fix is the "Option 2" item below: declared
  per-phase export tables (done 2026-09-16). That needs the renamed-keyword-by-binding dispatch
  shipped on `feat/keyword-denotation-dispatch` first, because `(import (scheme base) (for-syntax (scheme
  base)))` must see its two `syntax-rules` rows at phase 1 as one denotation before a per-phase
  export table can declare them as one name.

- [x] **Renamed `unquote`/`unquote-splicing` are matched by spelling, not by binding**
  [Done 2026-09-15, branch `feat/renamed-inner-positions`]: `quasi_expand.go`'s marker
  recognition (`dottedTailCell`, the depth walk `quasiNeedsRuntime`) compared a head symbol's
  spelling against `"unquote"` / `"unquote-splicing"`, never its resolved binding.
  `(import (rename (scheme base) (unquote uq))) `(1 (uq (+ 1 1)))` gave `(1 (uq (+ 1 1)))`,
  where Racket gives `(1 2)` — this branch now answers `(1 2)` too (same for
  `unquote-splicing`, and for a `prefix` import). **What now decides:** the form the marker's
  head denotes (`environment.DenotedForm`), with spelling as the fallback for a head that
  resolves to no form — an unbound head under a plain `prefix` import still keys on spelling
  unchanged (arm 2 of the keyword-denotation-dispatch rule). **Both marker walks moved, not
  one.** The first round moved only the expander's; `pkg/internal/validate/opaque_subtree.go`'s
  walk kept comparing spellings, and that walk is what records a quasiquoted `set!` as a
  mutation and a capture. The permissive disagreement its own doc warns about then happened:
  a renamed unquote left the `set!` unmarked, so the inliner kept a stale lambda body, a
  captured binder was never boxed, and top-level immutability REFUSED a legal `set!`. Fixed by
  giving the opaque node the environment it was validated in (`ValidatedQuasiquote.Env`,
  `ValidatedLiteral.Env`) so every consumer of the walk — the boxing pass's reference index and
  the or-shaped-let lowering — reads the same answer from the node rather than from wherever it
  happens to stand. Pinned by
  `TestRenamedQuasiquoteMarkers` and `TestRenamedQuasiquoteMarkerIsVisibleToTheOpaqueScan`
  (`pkg/wile/renamed_inner_positions_test.go`). **The first cost claim filed here was measured
  on a corpus without quasiquote in it** — `BenchmarkValidatePhase` and `BenchmarkFrontEndPhase`
  run `compileBenchCorpus`, which has no `quasiquote`, so "no statistically significant delta"
  said nothing about this change. Re-measured 2026-09-15 with a benchmark built for the path
  (`BenchmarkQuasiquoteExpand`, `pkg/machine/compilation`: five quasiquote defines through
  parse → expand → validate → compile, covering the element, nested-depth, splice, dotted-spine
  and deep shapes), 12-run interleaved A/B against `feat/keyword-denotation-dispatch`
  (079c62ab): **sec/op 66.71µ → 68.71µ, +2.99% (p=0.000, n=12)**; allocs/op 2.164k → 2.159k
  (−5, p=0.000); B/op −0.05%. The +3% is the resolution the fix adds per template head, in both
  the expander's walk and validate's; the −5 allocs are the `resolvedBy` closure this round
  replaced with a frame pointer on `quasiKeywords`. `BenchmarkValidatePhase` and
  `BenchmarkFrontEndPhase` remain flat (p=0.052 / p=0.198, 0 alloc change, same 12-run
  interleave), which is now a statement about quasiquote-free code rather than a claim about
  this fix.

- [x] **Renamed AUXILIARY keywords (`else`, `case`'s `else`, `=>`) are still matched by
  spelling, not by binding** [Done 2026-09-15, branch `feat/renamed-inner-positions`]:
  `sameLiteralBinding` (`pkg/internal/match/syntax_adapter.go`) accepted a pattern-literal
  match whenever both the definition-site and use-site bindings were `BindingTypePrimitive`,
  never asking whether the two denote the SAME form. `(import (scheme base) (rename
  (scheme base) (else otherwise))) (cond (#f 1) (otherwise 2))` failed on this branch's base
  and on master (`syntactic keyword "otherwise" used as a variable`); Racket answers `2` —
  this branch now answers `2` too (same for `case`'s `else`, and for a renamed `=>`). **What
  now decides:** the denoted form (`environment.DenotedForm`) instead of "both Primitive",
  narrowed further so an imported literal that denotes NO form (an ordinary variable or user
  macro) still falls back to matching spelling, or any imported name would satisfy any such
  literal (see `TestRenamedLiteralDoesNotOverAcceptUnrelatedImports`, which measured that
  exact over-acceptance before this narrowing). Pinned by `TestRenamedAuxiliaryKeywords` and
  `TestRenamedLiteralDoesNotOverAcceptUnrelatedImports`
  (`pkg/wile/renamed_inner_positions_test.go`). A reviewer flagged an unmeasured cost: an
  N-clause `cond`/`case` now pays a binding resolution per clause tried against `else`/`=>`,
  since match.go's literal arm used to refuse a differently-spelled symbol with a cheap
  string compare and now always resolves it (`pkg/internal/match/match.go`'s
  `ByteCodeCompareCar`). Measured 2026-09-15 with a benchmark built specifically to hit this
  path (`BenchmarkCondClauseLiteralExpand`, `pkg/machine/compilation`; bare-identifier clause
  tests, the shape whose else/=>-position input actually is a symbol — a compound-test clause
  like `(< n 2)` never reaches the literal comparison at all): no statistically significant
  delta and 0 extra allocs/op against `feat/keyword-denotation-dispatch`, 12-run interleaved.
  `case`'s clause heads are always `(atoms ...)` lists, so its cost stays O(1) regardless of
  clause count; `cond`'s bare-identifier/`=>`-clause shape is the one that pays O(N) in
  principle, and it still measures flat.

- [x] **A quasiquote head can now raise `ErrAmbiguousBinding` on a DATUM** [Low, S, filed
  2026-09-15 during the renamed-inner-positions whole-branch review, Done 2026-09-30, P3.1/I024,
  `f859fff0`, merged `e48b597f`: both marker walks resolve through `TryGetBinding`, which reports the
  tie instead of raising; verified from code, no Scheme-level repro exists]: marker recognition resolves a
  template head through `env.GetBinding` (`headFormName`, `pkg/machine/compilation`;
  `markerName`, `pkg/internal/validate/opaque_subtree.go`), and `GetBinding` raises
  `ErrAmbiguousBinding` on an equal-cardinality incomparable maximum (Fork C, auto-memory
  `ambiguous-binding-raise-fork-c.md`). Every previous consumer of that raise was asking about
  code; the quasi walk asks about a head sitting in pure DATA, where an identifier's identity was
  never consulted before — so `` `(a (foo b)) `` can in principle refuse to compile for a reason
  that has nothing to do with what the form means. Latent: no Scheme-level repro constructed, and
  the shape needs two same-spelled imported bindings at incomparable scope sets reachable at a
  template head. The conservative answer for this consumer is "not a marker" (keep the spelling),
  which is what the existing `denoted == ""` fallback already does for every non-raising failure.

- [x] **`(cond (otherwise 2))` works with a renamed `else` but `(cond-expand (otherwise …))` does
  not** [Low, S, filed 2026-09-15 during the renamed-inner-positions whole-branch review; an
  inconsistency this branch CREATES by fixing one of the two; Done 2026-09-30, P3.2/I025,
  `ec8a7f75`, merged `b0f3a327`: `isElseClause` and `parseFeatureRequirement` decide `else` by
  `featureName` (denotation, spelling fallback); both forms answer by binding]: `cond`'s and `case`'s `else` are
  pattern literals and now resolve by binding, so `(rename (scheme base) (else otherwise))` makes
  `(cond (#f 1) (otherwise 2))` answer `2`. `cond-expand`'s `else` is not a pattern literal — it is
  a hand-written spelling test, `sym.Key() == "else"` at
  `pkg/machine/compilation/compile_cond_expand.go:187` — so the same rename leaves
  `(cond-expand (otherwise …))` unrecognized. Two forms named `else`, two mechanisms, one of them
  now binding-aware. Fix candidate: the same `environment.DenotedForm` reading the other two use,
  spelling as the fallback. `isElseClause` is a package function, but its only caller
  (`resolveCondExpandClause`, same file) is a `*CompileTimeContinuation` method and already reads
  `p.env`, so the environment is in reach; what is NOT settled is whether `else` in a
  `cond-expand` position resolves to a form keyword at all in the frames this runs in, including
  a library body's. Measure that before writing the comparison.

- [x] **`environment.SameBinding` is the identity `literalNotShadowed` approximates** [Low, M,
  filed 2026-09-15 during the renamed-inner-positions whole-branch review; step one Done
  2026-09-30, P3.4/I026, `f5815abc`, merged `bbdfeaf3`: when both sides carry an origin,
  `SameBinding` decides, and the BOUNDARY row answers MATCHED-LITERAL. Step two (stamping
  library-private defines) stays tabled: `OriginRef` has no scope discriminator]: the pattern-literal
  rider in `pkg/internal/match/syntax_adapter.go` accepts an imported use-site binding and then
  discriminates with `DenotedForm`, falling back to spelling when neither side denotes a form —
  which is what the BOUNDARY row of `TestCrossLibraryPatternLiteralNeedsTheDefinitionSiteBinding`
  records as refusing a legitimate prefixed re-export. `environment.SameBinding`
  (`pkg/environment/binding.go:489`) is the origin-based identity that answers it properly, and
  the reviewer measured that adding it flips exactly that one documented BOUNDARY row to the R7RS
  answer with everything else green. Filed rather than applied only because nobody applied it; the
  objection originally recorded here is dead. Provenance-root origin is what SHIPPED for
  import-conflict detection (`5d654b3f`, auto-memory `import-conflict-detection-shipped.md`):
  `importConflicts` (`pkg/machine/compilation/library_bindings.go`) compares `*OriginRef` and
  nothing else. What PR #793 turned down was a SOURCE-LOCATION origin, which over-REJECTED by
  false-flagging a legal define-over-import shadow — a failure a root comparison cannot have, since
  `importConflicts` returns early on `!existing.IsImported()` and a define never enters one.

- [ ] **`quasisyntax`'s `#,` reaches the same permissive disagreement this branch fixed for
  `quasiquote`, live on MASTER, by a different route** [Medium, M, filed 2026-09-15 during the
  renamed-inner-positions whole-branch review]: `quasisyntax` is a bare passthrough
  (`registerPassthrough("quasisyntax")`, `pkg/internal/validate/register.go:75`), so
  `opaqueEntryDepth` enters its whole form at `quasiDepthCode` rather than at template depth, and
  `quasiHeadDepth("quote", 0)` is a BARRIER there — the validate walk stops at the `(quote …)`
  subform quasisyntax's own expansion wraps literal data in and marks nothing inside it. The
  compiler disagrees: `quasisyntaxKW.quoting` is `"syntax"`, not `"quote"`
  (`pkg/machine/compilation/quasi_expand.go:83-87`), and `expandQuasi` has no case for that
  quoting kind, so it descends into the same `(quote …)` at depth 1 and fires the `#,` inside —
  validate says barrier, the compiler says live, the exact shape the branch's C1 fix closed for
  `quasiquote`, reached here by a route that fix never touched. Measured identically on master
  `963cab4b`, on `079c62ab`, and at HEAD (`093679e7`) — this branch neither creates nor fixes it:
  `(let ((f (lambda () 7)) (n 0)) (quasisyntax (a (quote (#,(begin (set! n 1) (set! f (lambda ()
  99))))))) (list n (f)))` answers `(1 7)`; the canonical control (the same two `set!`s run
  directly, no quasisyntax) answers `(1 99)`.

- [x] **The branch's headline fix does not reach a marker introduced by a macro template** [Low,
  M, filed 2026-09-15 during the renamed-inner-positions whole-branch review; Done 2026-10-14,
  P4.2/I028, `3721b540`, merged `b6aacda9`: `headFormName` and `markerName` read
  `DenotedForm(PinnedBinding(sym))` before the scope walk; answers `(1 99)`, matching Racket and
  Chez. Gate `TestFreeTemplateIdHygiene_RenamedUnquoteViaLibraryTemplate`]: a library that
  imports `(rename (scheme base) (unquote uq))` and exports a `syntax-rules` macro whose template
  is `` `((uq e)) `` gives `(0 7)` at HEAD and at base (`079c62ab`) for
  `(let ((f (lambda () 7)) (n 0)) (mk (begin (set! n 1) (set! f (lambda () 99)))) (list n (f)))`
  — the `set!` never runs and `f` keeps its original body, where a recognized marker would answer
  `(1 99)`. The macro-introduced `uq` carries the macro's DEFINITION-site scopes and resolves to
  nothing at the compiled use site, so neither `markerName` nor the compiler's `headFormName` sees
  a marker there. Both walks agree, so this is a conformance gap, not the soundness hole the
  branch fixed — Racket fires the renamed unquote through the same shape.

- [ ] **The opaque scan cannot see a `set!` a macro template introduces** [High, M, filed
  2026-10-15 with P4.5 I144]: the opaque scan over-approximates every symbol in an evaluated
  position of the raw syntax (`opaque_subtree.go`), which covers a macro whose `set!` target is
  written in the operand, but not one whose template names the target itself. Such a use expands
  at compile time, after validation, so the inliner keeps the stale body. Each position, with
  `(let ((f (lambda () 7))) (define-syntax m (syntax-rules () ((_) (set! f (lambda () 99))))) <P>
  (f))`; Chez and Racket answer `99`, and so does `(m)` written directly:
  - `(cond-expand (else (m)))`: `7`, live on master. **Open.**
  - `include` of a file holding `(m)`: raises "no such binding m" (measured 2026-10-16): a
    local macro is not visible to the included forms, which masks the scan question. That is the
    reverse direction of the "A macro defined in an `include`d file …" row, filed 2026-10-16 as
    "An included file cannot see a binding local to its includer". **Open.**
  - `` `(,(m)) ``, `(syntax-case (m) () (_ 1))`, and `with-syntax`'s RHS: **fixed by
    `76e81aac`** (P4.5 crosscheck remediation, merged 2026-10-15). The first P4.5 commit
    expanded these operands at compile time, which reached the hole (`7`; master had raised "no
    such binding m"). They now expand in the expander pass (`expander_quasi.go`), before
    validation, so the scan sees the `set!`. The crosscheck found two more symptoms of the
    compile-time timing, and the same move fixed both: a `let-syntax` keyword was out of scope in
    an operand (the compiler's env holds no frame for it, so `` `(,(two)) `` resolved an outer
    `two`), and a macro recursing through an operand overflowed the Go stack, because each
    compile-time expander started a fresh depth guard. The same hole at top level: `(begin (define
    g 1) (define-syntax setg … (set! g 2)) `(,(setg)) g)` was REJECTED under the immutable top
    level ("cannot mutate immutable top-level binding"); it now answers `2`. A `with-syntax`
    with no bindings expands its body in the same pass; on master any macro use in such a body
    (`(with-syntax () (m 3))`) raises "no such binding". Pinned by
    `pkg/wile/quasi_operand_expansion_test.go`.
  - The branch adds a Variable-pin stop to `lookupMacroBinding` (needed so a user
    `(define-syntax list …)` cannot capture the Scheme layer's generated `(list …)`), scoped so a
    pin into the use site's own frame, in its mutable tier, does not stop the search: a later
    top-level `(define-syntax helper …)` still redefines a template's `(helper)` (`42`, as on
    master and in Chez and Racket). A sealed pin, or one into a library's frame, stops it.

  The remaining positions need the same move: expand cond-expand's chosen clause and an included
  file's forms before validation, or treat a macro use in one as an unknown set! of every captured
  binding. Syntax-case clause bodies still expand at compile time (`CompileSyntaxCase`), so a
  macro recursing through one still hangs rather than raising, live on master. The same timing
  leaves a `let-syntax` keyword out of scope in a clause body: with a top-level `two` → 2,
  `(let-syntax ((two (syntax-rules () ((_) 3)))) (syntax-case 'x () (_ (two))))` answers `2` on
  master and the branch; Chez and Racket answer `3`.

- [ ] **Go layer: a `syntax` template with an ellipsis before its last element builds a
  malformed syntax list** [Correctness, S, filed 2026-10-15 with the P4.5 Scheme-layer follow-up]:
  `(syntax-case #'(1 2 3) () ((x y ...) #'(y ... x)))` returns a list holding a nil element, so
  printing it SIGSEGVs in `SyntaxObject.SchemeString` (`wile -e`), and `syntax->datum` of it renders
  `(#<void> ... 1)`. Chez and the Scheme layer answer `(2 3 1)`; `syntax-rules` with the same
  template is correct, so the fault is in the Go `syntax` template builder only. Live on master
  (`c65c625b`), default layer.

- [ ] **A lexically shadowed `unquote` still fires** [Conformance, M, filed 2026-10-15 at the
  P4.5 crosscheck]: the compiler's quasi walk (`expandQuasi`) classifies a head through
  `headFormName` in the compiler's environment, which holds no `let` or `let-syntax` frame, so a
  local binding of the marker's name does not shadow it. `(let ((unquote 5)) `(a ,1))` answers
  `(a 1)`; Chez answers `(a (unquote 1))`. With `(import (rename (only (scheme base) unquote)
  (unquote uq)))` and a top-level `two`, `(let-syntax ((uq (syntax-rules () ((_ x) 'shadowed))))
  `(a (uq (two))))` raises "no such binding two" (the expander pass sees the let-syntax `uq` and
  leaves `(two)` unexpanded; the compiler then fires it); Chez and Racket answer
  `(a (uq (two)))`. Live on master. The fix is one authority for marker recognition: the expander
  pass, which holds every frame, stamps what it classified.

- [ ] **`with-syntax`'s synthesized `list` head is unpinned** [Correctness, S, filed 2026-10-15 at
  the P4.5 crosscheck]: `CompileWithSyntax` builds `(syntax-case (list expr …) …)` with
  `syntax.NewSyntaxSymbol("list", …)`, which carries no pin, so a user variable named `list`
  captures it: `(define (list . xs) 'captured) (with-syntax ((a 1)) (syntax->datum (syntax a)))`
  raises "syntax-case: no matching clause for input #'captured"; it should answer `1`. Inside a
  procedural transformer, a user `(define-syntax list …)` makes it fail as "syntactic keyword
  list used as a variable" (Chez `3`). Live on master. Likely fix: build the head with
  `quasiHead("list", …)`, which pins the sealed slot.

- [ ] **`with-syntax` with no bindings drops a dotted body tail** [Conformance, XS, filed
  2026-10-15 at the P4.5 crosscheck]: `compileWithSyntaxBody` stops at the first non-pair cdr, so
  `(with-syntax () 1 . junk)` answers `1` instead of raising (Chez: invalid syntax; Racket: bad
  syntax); same class as the `expectArgs` row (Tier 1). Live on master. With a macro in the body,
  `(with-syntax () (when #t 1) . 2)` fails as "no such binding when", because the expander pass
  leaves an improper body unexpanded.

- [ ] **An internal `define` in a `with-syntax` body panics** [Correctness, S, filed 2026-10-15
  at the P4.5 crosscheck]: `((lambda () (with-syntax ((q 1)) (define x 1) x)))` raises
  "recovered panic: … index out of range [2] with length 2"; it should answer `1`. A no-binding
  body with a `define` inside a transformer panics the same way. Live on master.

- [ ] **`validateInclude` never reads the included file** [Medium, M, filed 2026-09-15 during the
  renamed-inner-positions whole-branch review]: `validateInclude`
  (`pkg/internal/validate/validate_macro.go:235`) calls `markOpaqueCode(env, pair, result)` on
  `pair`, the `(include "f.scm")` form ITSELF — its own head symbol and string filename arguments
  — never on the forms the named file actually contains, which this validation pass does not even
  parse (its own comment says so: "The file's contents are not even readable here"). A `set!` of
  a captured or let-bound binding written inside the included file is therefore invisible to the
  opaque scan the same way a renamed marker was before this branch's fix, except here there is no
  marker-recognition question at all — the file's contents are structurally unreachable from this
  walk. Same hazard class as the fixed C1 bug (a hidden `set!` leaves a stale inline body or an
  unboxed capture). Repro: the top-level `include` row in "Redefining `if`" above (`set!: cannot
  mutate immutable top-level binding "y"`), constructed 2026-09-20.

- [ ] **`include` inside a procedure body skips letrec\* predeclaration** [Medium, S, filed
  2026-09-15 during the keyword-denotation-dispatch whole-branch review; pre-existing on master, not
  caused by this branch]: `(define (k) (include "x.scm") (inc-g))`, where the included file
  forward-references a later included `define`, raises `application: expected a procedure, got
  #<void>` at run time. R7RS §5.3.2 body semantics predeclare every name a body defines before any
  initializer runs (letrec\*); the body scan that does this for defines written directly in the body
  does not descend into an `include`d file's forms to find the names it will introduce.

- [x] **Option 2: declared per-phase export tables** [**Done 2026-09-16**, branches
  `feat/export-for-syntax` and `fix/renamed-syntax-rules-import`: decision (1) is the export
  syntax, `(export (for-syntax <spec> ...))`, nesting like Racket's; decision (2) is phase 0 only
  for a plain export, matching Racket's `provide`. `CompiledLibrary.Exports` is keyed by
  `(phase, name)`, and import modifiers act on a name at every phase. `(scheme base)` and
  `(scheme r5rs)` export `(for-syntax syntax-rules ... _)`, and `exportRoot` roots a base binding
  at `#%base` with no phase, so the two `syntax-rules` rows are one binding: a renamed or prefixed
  `syntax-rules` works in a transformer RHS, and `(import (scheme base) (for-syntax (scheme
  base)))` is no conflict (`TestSchemeBaseExportsSyntaxRulesForSyntax`). Original entry follows.] [Medium, L, filed 2026-09-15, follow-on to
  the `findLibraryBinding` item above]: that item's fix (preferring the requesting phase) only
  routes an import to the right EXISTING binding; it does not let a library DECLARE which phases
  a name exports for. `syntax-rules` needs both its phase-0 `SyntaxCompiler` row and its phase-1
  `PrimitiveExpander` row visible to an importer as one denotation, and Wile's stdlib forms are
  registered Go-side, not declared through `export` syntax at all. Two open decisions block a
  design: (1) declaration mechanism, a field on the Go registration call vs. an `export` syntax
  extension a library body writes; (2) whether a plain `(export name)` re-export (no `rename`,
  no explicit phase) carries the source's declared phase rows, or defaults to phase 0 only,
  matching Racket's `rename-out` under `only-meta-in`. Depends on the renamed-keyword-by-binding
  dispatch shipped on `feat/keyword-denotation-dispatch`: `(import (scheme base) (for-syntax
  (scheme base)))` must see the two `syntax-rules` rows at phase 1 as one denotation before a
  per-phase export table can declare them as one name.

### The D7 residual: a primitive expander above phase 1 (2026-09-09)

- [ ] **Precondition on any dialect that installs one; not an open defect today**
  [Low, S, filed 2026-09-09 closing out Flatt Stage A
  (`memory/2026-09-08-flatt-binding-model-a-impl`, Task 8 / Fork 1)]:
  `LookupPrimitiveExpander` reads `env.Expand()`, an ABSOLUTE phase 1. That is
  CORRECT rather than a residual, because the expander table is single-instance:
  `registerPrimitiveExpandersWithout`'s only write is
  `env.SealedWriteViewAt(PhaseExpand)`, the sole `NewPrimitiveExpander` write in
  the tree, so nothing installs an expander at any other phase and a phase-2 body
  finds its expanders BECAUSE the read is absolute. The design's D7 proposed
  `NextPhase()` there; measured, that one symbol breaks bootstrap loading
  (`LoadBootstrapCore: load bootstrap macros: … not a closure: values.voidType`),
  with the ambient tier still present — which also refuted D7's premise that
  ambient was masking the problem. D7 is WITHDRAWN.

  What is filed is the precondition: **if a dialect ever installs a primitive
  expander above phase 1, the absolute read becomes wrong at that moment.**
  `TestPhase2_UniformRule`'s third subtest passes for the absolute read's sake and
  says so in its comment, so a later reader does not re-file D7 from it.

  **Pinned** (2026-10-03, P3.16/I130): `TestPrimitiveExpandersLandAtExactPhaseOne`
  (`pkg/machine/compilation/primitive_expanders_registry_test.go`) asserts five form names resolve
  at `(phase 1, sealed)` and not at exact phase 0; flipping the taproot to `PhaseRuntime` reds all
  five subtests. It predates P3.16. `.goast-beliefs/primitive-expander-table-is-expand-sealed.scm`
  observes the site count (exactly one non-test builder) and write shape on demand; **at one site
  its status cannot go red**, so read `pattern` and `total`, and `.goast-beliefs/` is run by
  neither `make lint` nor `make ci`. History: `memory/2026-10-03-p3-16-first-cuts-refuted.md`.

- [ ] **Unexport `NewPrimitiveExpander`, so working expanders have one constructor**
  [Low, S, filed 2026-10-03 from the P3.16 crosscheck; the change is not compiled]: the precondition
  above is about PLACEMENT (no expander above phase 1). The belief watches only a proxy for it, the
  builder count and write shape. Unexporting would confine builders to the package; it enforces
  neither the count nor the write shape, and not placement.
  `NewPrimitiveExpander` (`primitive_expander.go`) and `RegisterPhaseBindings` are exported, so any
  importer can build a second table at another phase; unexported, a builder outside
  `pkg/machine/compilation` is a compile error. That confines construction to one package, NOT
  placement: measured, an external package can take an existing expander from the exported
  `LookupPrimitiveExpander` and store it at (2, sealed) through `GlobalEnvironment()`'s exported
  writes, and `&compilation.PrimitiveExpander{}` still compiles outside the package. The one non-test
  caller is in-package; the cost is `pkg/machine/value_methods_test.go`, whose six uses are in an
  external test package.

### `bootstrap_syntax{,_procedures}.scm` and the phase-1 vocabulary (2026-09-09)

- [ ] **Re-derive the macro vocabulary when the Scheme syntax layer flips**
  [Medium, M, filed 2026-09-09 closing out Flatt Stage A, Task 10 / D9's residual]:
  `MacroVocabulary` (`pkg/internal/bootstrap/language_rows.go`) is pinned against the **Go**
  layer by `TestPhase1VocabularyMembership`. Under `WithSchemeSyntaxForms`,
  `syntax-rules` becomes a Scheme macro with a procedural body, and that body's own
  transformer needs phase-**2** visibility — so the flip revises the set rather
  than contradicting it.

  The hazard is specific: someone widens the vocabulary to make the Scheme leg
  pass and deletes D4's phase-distinctness break for the Go leg at the same time,
  because a wider vocabulary is a name a procedural transformer no longer has to
  import. The ratchet asserts membership in BOTH directions so that shows up as a
  failure naming the added names, and
  `TestPhase1VocabularyIsPinnedAgainstTheGoLayer` records the coupling.

  The `%`-prefix arm is what currently carries the Scheme layer: its 56+ private
  helpers live in the base store and no `.sld` exports one or could, so no import
  could reach them. If the flip makes those importable, the arm can narrow.

### A phase-1 `(import (for-syntax (scheme base)))` is not behaviour-neutral (2026-09-09)

- [x] **Fixed at three reader sites** [Done 2026-09-09, `e61a3e37`, branch
  `fix/phase1-import-neutrality`]: ONE writer-side cause, THREE readers that failed closed on it.
  **Cause:** `installImportedBinding`'s shadowable arm is guarded `env.PhaseLevel() ==
  PhaseRuntime` (`library_bindings.go`), so a phase-SHIFTED import lands at
  `(ExactPhase(N>0), MUTABLE)`, which outranks every sealed phase row (expanders, syntax
  compilers, bootstrap macros) at the same empty scope set. `(scheme base)` exports 20 of the 40
  phase-1 expander names as value-less keywords, so the import blanked the binding-form
  machinery. **The auxiliary-keyword reading was REFUTED:** `(only (scheme base) let)` reproduces
  it, `(only (scheme base) else)` does not; the trigger is the collision set.
  **The three readers:** (1) `LookupPhaseBinding` (`phase_registry.go`) falls back to
  `SealedBindingAt(sym, q, phaseEnv.PhaseLevel())` on a type mismatch; (2) `lookupMacroBinding`
  ARM 2b (`expander_time_continuation.go`) also opens on a wrong-typed ARM 2 answer (victim:
  `syntax-rules` under `WithSchemeSyntaxForms`, where the Go form was dispatched in silence);
  (3) `invokeERTransformer` builds the rename closure at the expansion's phase,
  `DefEnv().AtPhase(p.env.PhaseLevel())` (prior art `GetGlobalIndexFromLibraryScopes`).
  **The discriminator is `BindingTypePrimitive`, not import provenance:** an `IsImported()` gate
  was tried and leaves the `syntax-rules` column red (that mask carries no `Imported` meta), and
  the binding-type rule keeps a user `(define define-syntax …)` shadowing
  (`TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler` caught the wider version).
  **Reader 3 was not import-specific:** `(define list 5)` plus an ER macro renaming `list` split
  the same way; the fix also closes a rename smuggling a phase-1-ONLY import into phase-0 output.
  Racket answers "unbound identifier" there; Chez is not the oracle (R6RS implicit phasing).
  Gates: `TestPhase1BaseImportIsBehaviourNeutral` (imported arm EQUALS bare arm),
  `TestPhase1BaseImportMasksNoPhaseRow` (derives the name set from `SealedSlots()`),
  `TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules` (a COUNTER, because the value assertion
  passes either way), `TestERRenameDenotesTheOutputPhase`. **Trap: a black-box "do the core forms
  still work?" table was REJECTED as the ratchet because it passes on the unfixed tree** (a masked
  head still reaches the compiler's own dispatch). Seven workaround sites in five files reverted;
  the old instruction to find them by grepping "not behaviour-neutral" was wrong (five wordings).
  `pkg/stdlib/lib/chibi/optional.sld`'s `cadr` uses sit under `(cond-expand (chibi …))`, and
  `chibi` is in neither `supportedFeatures` nor `platformFeatures`
  (`pkg/machine/compilation/features.go`), so that arm is dead and was not a site.
  Residual: `integration/quasisyntax_test.go` and `extensions/eval/prim_eval_test.go` are blocked
  by their own harness (a bare `NewEngine` with no source FS resolves no import), a one-line
  cleanup independent of this defect.

- [x] **RESIDUAL: the writer-side defect is FIXED; the premise that blocked it was false**
  [Done 2026-10-01, P3.10/I127; see "The phase-1 import installs CAN take the shadowable tier"
  above, which is the same one-line change and carries the measurement]: the bad
  `(ExactPhase(N>0), mutable)` slot is no longer created — the shadowable arm now routes through
  `CreateImportedGlobalBindingAt` at every phase, so a phase-N import takes
  `tierExactImported` and ranks BELOW a user define rather than above it. "There is no safe
  writer coordinate today" was the blocker, and it rested on a hazard the Imported stamp already
  makes unreachable. D12 (route every import through a bulk row of its own) stays declined and is
  no longer needed for this.
  The Stage-B-shaped question underneath: should `else` at phase 1 come from an IMPORT at
  all, or from the dialect's declared initial-import rows? `pkg/machine/compilation/er_macro_compare_test.go`
  currently depends on the import supplying it, which is what killed the writer-side variant.
  A cheap partial gate in the meantime: no import may land at T1 above a sealed phase row.

- [x] **The two reader gates STAY, and the reason is recorded in their own doc blocks**
  [settled 2026-09-10, Stage B Task 7, fork A answered A2]: they are not a workaround for a
  RANKING error Stage B would close. (1) **R7RS-small has no answer:** §5.2's `<import set>`
  admits `only`/`except`/`prefix`/`rename`; `for-syntax` is R6RS/Racket. (2) **The references
  disagree** (four-way: master, branch, `petite`, `racket`): Chez is SILENT, first-listed-wins
  (measured non-vacuously); Racket REFUSES require-vs-require but SHADOWS require-vs-language,
  hierarchy definition > require > language, discriminating on WHAT KIND the collidee is, not its
  phase. (3) **Wile's collidee is the LANGUAGE at phase 1** (`RegisterPhaseBindings`' rows, the
  `#lang` analogue), so under Racket's own rule the import out-ranking it is RIGHT; the design's
  `raise-already-bound` citation answers require-vs-require, which Wile already ships as
  `importConflicts`. (4) The winner passes `BindingType() == BindingTypePrimitive` while its VALUE
  is not a `*PrimitiveExpander`, which is why a reader must look past it to the sealed row; each
  library env mints its own `*Binding` per re-exported name, and `sameLiteralBinding`
  (`pkg/internal/match/syntax_adapter.go`) widens on `BindingTypePrimitive` for the same fact.
  Point 4's "identity defect" diagnosis as the gates' cause is struck by the follow-on below.

- [ ] **FOLLOW-ON: share binding identity across the import edge** [filed
  2026-09-10; **its stated MECHANISM STRUCK 2026-09-13, its PURPOSE reassigned**]:
  give a re-exported name ONE `*Binding` across the import, or give the base its
  own store so an import and the language never share a name table. (2026-10-16:
  the stale-imported-variable half SHIPPED as location sharing, see "Identity
  sharing" below; every `file:line` anchor in this row is as filed and has
  drifted, so resolve by symbol.)

  ~~**That** is what makes the two reader gates removable, and it is the only
  thing that does.~~ **MEASURABLY FALSE, and it is the sentence that mis-scoped
  every attempt to act on this item.** Neither gate's predicate can see pointer
  identity. Gate 1 tests `bnd.BindingType() == BindingTypePrimitive` then
  `bnd.Value().(T)` (`pkg/machine/compilation/phase_registry.go:166-170`); gate 2
  tests `bnd != nil && bnd.BindingType() == BindingTypePrimitive`
  (`pkg/machine/compilation/expander_time_continuation.go:476`). The import
  already supplies both inputs verbatim: `installImportedBinding` takes
  `libBinding.BindingType()` as its create argument and writes `source.Value()`
  at the sole value-write site on any import path
  (`pkg/machine/compilation/library_bindings.go:736`). Make the pointer identical
  and both predicates evaluate exactly as they do today. The two production doc
  blocks that asserted the same thing are corrected in the same pass.

  **What the gates are actually caused by, and what to do instead.**
  `findLibraryBinding` walks `lib.Env.PresentPhases()` ASCENDING and returns the
  first hit, so the LOWEST phase wins regardless of the phase the import will
  install at (`library_bindings.go:481-503`, pinned by
  `TestFindLibraryBindingPrefersRuntimeOverExpand`). `(scheme base)` holds
  `syntax-rules` at phase 0 as a `*SyntaxCompiler` and at phase 1 as a
  `*PrimitiveExpander`; the export walk hands a for-syntax import the phase-0
  one, which then fails gate 1's type assertion at the importer's phase 1. ~~That
  is fork (a) above, already decided and unshipped, and it is the item that
  should carry this work. Its rider stands: `validateLibraryExports`
  (`compile_library_forms.go:300-312`) makes the SAME first-hit call and
  discards both returns, so fixing the walk alone leaves the false green.~~
  **Fork (a) WITHDRAWN 2026-09-16** (`372d703c`, entry above): an export declares its
  phase, `findLibraryBinding` probes that phase (then phase+1 for a keyword) instead of
  walking `PresentPhases`, `validateLibraryExports` asks the same question, and
  `TestFindLibraryBindingPrefersRuntimeOverExpand` was replaced by
  `TestFindLibraryBindingAtExportPhase`. The three mechanisms and the fourth option below
  were designed for fork (a) and are kept as history. Re-measured 2026-09-17 at `9bf60318`
  (per-site partition below): gate 2 is unchanged, and gate 1 now reddens one ratchet only,
  since `68cf1ca2`, not since this withdrawal.

  **Three mechanisms for fork (a) were designed and all three took a fatal on
  adversarial review (2026-09-13, 20 agents).** Recorded so they are not
  re-proposed as novel: (i) a `targetPhase`-gated selector re-looks-up the NAME
  in the library env at another phase, where `ScopesCompatible(∅, {libScope})`
  is unconditionally true, so it substitutes a library's private
  `begin-for-syntax` define for the runtime export a name also has, and it never
  fires on fork (a)'s own pinned row (`TestLibraryExportRoundTripByPhase/syntax-rules_plain_export_does_not`,
  a `targetPhase == 0` import); measured, it also swaps the installed object for
  105 of 233 ordinary procedures, whose replacement closure captures
  `env.Expand()` rather than `env` (the G12 closure-env blocker). (ii) A
  writer-side guard suppressing the redundant install bypasses the R7RS §5.2
  conflict check outright: `importConflicts` runs behind `!created`
  (`library_bindings.go:730`), so with nothing installed there is no `target` and
  no check. (iii) Giving the language its own store blinds every sealed-tier
  reader at once, because `SealedBindingAt` is `probeRankedLocked` over slots
  alone (`global_environment_frame.go:1409-1414`) and row consultation exists
  only in `resolveRankedLocked:660-663`; `headDenotesSpecialForm`,
  `quasiHead`'s anti-hijack pin, and `namespace-undefine!`'s startup-set refusal
  all fail OPEN and all sit outside that proposal's file list.

  **The unmeasured fourth option, which is where a re-opener should start:**
  discriminate the export hit by SCOPE PROVENANCE, not by phase preference.
  `findLibraryBinding` already merges two populations and `Binding.Scopes()`
  already separates them: a `{libScope}` hit is the library's own definition
  (lowest present phase is correct there, and is what makes `(define foo 1)` plus
  `(begin-for-syntax (define foo 2))` export the runtime one), while an
  ∅-ambient hit is a registry or import row the library merely re-exports, where
  "lowest phase" means nothing. It cannot commit (i)'s fault because it never
  reaches a library's own `begin-for-syntax` define. **Two costs nobody has
  priced, and they gated the other three silently:**
  `TestFindLibraryBindingPrefersRuntimeOverExpand`'s fixture was entirely ambient
  (`NewCompiledLibrary` with a nil `Scope`, `DefineOwnGlobal(..., nil)`); that test
  is gone, and its successor `TestFindLibraryBindingAtExportPhase`
  (`library_bindings_test.go`) must be checked for ambience the same way, its wants
  deliberately re-specified rather than assumed to survive; and the propagation arm is
  `placementInPlace`, so an un-renamed two-phase re-export lands on a sealed
  phase-1 coordinate, the engine-wide overwrite `library_bindings.go:826-841`
  refuses to relocate.

  **The gates are not the justification for any of it.** Delete them afterwards
  only if they measurably fall out, keeping `phase_registry.go:174-176` (the
  `0d1204c6` keeper). Their whole measured symptom is a `GoSyntaxFormCompiles()`
  delta of +1 plus value tests that pass today; every removal designed so far
  traded a loud, ranked, documented workaround for a silent one.

  **Identity sharing survives only under its own motivation, which is real and
  is NOT the gates** [Medium, L]: the stale imported variable (a library `set!`
  after import left the importer reading the old value) is FIXED by sharing the
  LOCATION, not the object (`b91b8933`, merged `455ee5ed` 2026-10-03, P3.12; pin
  `TestImportSharesTheValueLocation`); Wile now reads through, as Racket does.
  What remains is the R7RS §4.3.2 over/under-acceptance. Note that
  `eq?`-across-import is NOT among them: the value location is shared
  (`ShareValueLocationWith` in `installImportedBinding`), and `eq?` answers `#f` because each library env
  re-mints its closures (`pkg/registry/apply.go:267`). The sound residue is
  sharing the storage LOCATION, not the object; sharing the OBJECT is refused on
  three independent grounds — `m.Imported` is half the slot coordinate
  (`global_environment_frame.go:1627`, `:1065`), `Binding.Scopes()` is the store
  key and the two sides need different sets (`:1630`, `:1112`;
  `library_bindings.go:708` vs `:485-489`), and the inline-HOF stamp is keyed on
  the per-import `exportName` so a re-exported sealed-base HOF would be
  permanently de-stamped at its home (`inline_hof.go:213-215`, `:230-233`). That
  shape was already run once by accident at `183171a1` and is written up in
  `installImportedBinding`'s own doc (`library_bindings.go:645-661`). Any re-open
  must move `Imported` and the scope set off `*Binding` onto `slotRef` FIRST.

  **Scope note: row 3 of the partition below was never in scope** (see that
  entry).

- [x] **The per-site failure partition, measured 2026-09-10** [scratch worktree at `c28616c1`,
  `GOWORK=off`, `make build` first]: recorded so the follow-on inherits its sensors. **Three
  SITES, two of them the reader gates**; each reverted ALONE:

  | # | Site | Reverting it alone reddens (2026-09-10; row 1 superseded below) |
  |---|---|---|
  | 1 | `LookupPhaseBinding` (`pkg/machine/compilation/phase_registry.go`) | `TestPhase1BaseImportIsBehaviourNeutral` (6/6), `TestPhase1BaseImportMasksNoPhaseRow` (20 of 40 rows masked), `TestP02_ExpandOnceMirrorsTheLoop`, `TestP05_FreeIdentifierEqualShadowProbe`, `TestP2_ERRenameIsFreshPerInvocation`, `integration/TestERMacro_Mixed` |
  | 2 | `lookupMacroBinding` ARM 2b's `masked` gate (`pkg/machine/compilation/expander_time_continuation.go`) | `TestPhase1BaseImportDoesNotReviveTheGoSyntaxRules`, `TestP2_SyntaxRulesAndERAreScheme`: both `compilation.GoSyntaxFormCompiles()` COUNTER deltas of +1; every value assertion stays green |
  | 3 | NOT A GATE: the ER rename closure's `.AtPhase(p.env.PhaseLevel())` (same file) | `TestERRenameDenotesTheOutputPhase` (2/3), `integration/TestERMacro_Cond` |

  The partitions are DISJOINT, which is why rows 1 and 2 are two gates, not one. **Row 3 is not
  a gate** (design B13: no `BindingType` test) and sharing binding identity would not retire it:
  a phase-1-ONLY import has no phase-0 slot to share identity with, so the smuggling hazard
  survives. **Do not fold row 3 into the follow-on; "three reader gates" is the recurring
  miscount.** The earlier filing's partition was wrong in both directions (missed three
  `pkg/wile` tests that carry `(import (for-syntax (scheme base)))` themselves, and
  `TestP2_SyntaxRulesAndERAreScheme`). **Do not delete `phase_registry.go`'s
  `bnd.BindingType() != BindingTypePrimitive` guard when the gate goes:** it predates the fix
  (`0d1204c6`, 2026-01-09) and keeps
  `TestLookupSyntaxCompiler_SamePhaseShadowOutranksTheSealedCompiler` green.

  **Re-measured 2026-09-17 at `9bf60318`** (full `go test ./...` with each gate reverted alone,
  plus a run logging every gate fallback that returns a value). **Row 2 is unchanged.** It fires
  for `syntax-rules` in its two counters and in `TestP2_ERPassThroughKeepsScopes`, which does not
  detect the revert. **Row 1 now reddens only `TestPhase1BaseImportMasksNoPhaseRow`** (the same
  20 names) and fires in no other test. Bisected with `TestPhase1BaseImportIsBehaviourNeutral` as
  the sensor to `68cf1ca2` (`feat/keyword-denotation-dispatch`): `lookupHeadPrimitiveExpander`
  probes the sealed phase-1 row for a head's denoted form before calling
  `LookupPrimitiveExpander`, so head dispatch steps around the mask without reaching the gate.
  What still reaches it: a direct `LookupPrimitiveExpander` call (the ratchet), that method's
  final fallback for coordinates with no sealed row, and `ExpandPrimitiveForm`, which has no
  production caller. No tested production path fires it; that no untested one needs it is not
  shown. Row 3 was not re-measured.

- [x] **`TestPhase1BaseImportMasksNoPhaseRow`'s non-vacuity floor has 20 names of headroom**
  [measured 2026-09-10 at `c28616c1`]: `len(before)` is **40** against a floor of `> 20`. The
  floor asks "is this test looking at anything", the `masked` assertion asks "did the import move
  any of them". A change dropping the expander table from 40 to 21 keeps reporting green while
  sensing half as much. A population that narrows must surface as an assertion failure, not as a
  quietly smaller universe the `masked` loop walks.

### Stage B S4 is STRUCK — the price, and what would have to be true first (2026-09-10)

S4 ("fold phase into the scope set as a representative scope, so `probeTiersLocked` loses its
`phase` parameter") is not happening. This records its price so it is not re-derived, and refiles
the design's two remaining §8.2 blind spots as preconditions. Figures re-measured at `c28616c1`.
**The rot has a direction**: of sixteen Stage B figures re-measured, all sixteen were wrong and
every one LOW, because code accretes, so a stale count systematically under-states the work. A
correction must record its command too: one "correction" was itself wrong and reverted in
`5d69a2fd`. **Re-run any figure in the Stage B documents before relying on it.**

- [x] **Q4, "can the phase registry be removed?", answers NO** [Done 2026-09-10]: four blockers.
  `PhaseRegistry.sealedViews` is the sole backing for `SealedWriteViewAt` (5 production callers) and
  `AtPhase`'s sealed climb; `GetOrCreate` is the only `EnsureMacroPhaseRows` hook, and the lazy,
  unbounded tower has no phase set to enumerate; `AtPhase` panics `ErrMissingPhaseRegistry` without
  it; the stable-pointer-per-(owner, phase) contract lives in `GetOrCreate`'s doc. **Do NOT scope a
  step as "rewrite the two `Phases()` consumers"** (`ApplyDocs`, `firstPhaseBinding`): `PresentPhases`
  itself calls `p.phases.appendPhases`, so freeing them frees nothing. The design's §7
  "frame-per-phase dissolves" is struck.
- [x] **The S4 inventory, as a price** [Done 2026-09-10]: non-test, tree-wide, at `c28616c1`.

  | Item | Plan said | Measured |
  |---|---|---|
  | phase-VALUE comparison bodies | 8 | **16** tree-wide, **10** in `pkg/environment`; a looser sweep reaches 18-19 |
  | struct fields typed `Phase` (incl. `map[Phase]…`) | 3 | **12**; **8** in `pkg/environment` |
  | signatures mentioning `Phase` | ~24 | **61** |
  | distinct widths | 3 | **3**, but **4 declarations** carry them |

  The bitset declarations (`exactPhases [2]uint64`, `macroPhaseSeenBits [2]atomic.Uint64`,
  `registry.PhaseSet uint8`) are dense-small-integer assumptions in TYPE form that no `>>6` grep
  finds. `appendExactPhases` is the reverse mapping `PresentPhases` needs, and `PresentPhases` is
  sort + compact + trim, so a fold that loses order loses dedup. Only 3 of the 10 `pkg/environment`
  comparisons are on the ranked-read path, so a read-side inventory finds under a third. The count
  is definition-sensitive (binary comparisons with a `Phase`-typed operand, excluding nil tests of
  `*PhaseRegistry` and `macroPhasesSeen`); quote the direction, not a bare number.
- [x] **The design's `ScopesCompatible` warning named the wrong artifact** [Done 2026-09-10]: its
  empty-set short-circuit is a no-op, since `ScopesMatch(u, ∅)` is already true; deleting it passes
  `go test ./...`, so an implementer told to preserve it would see green and stop trusting the
  document. The rule a fold would break is `∅ ⊆ X` inside `ScopesMatch` (12 direct calls in 5
  packages, plus 9 `ScopesCompatible` sites delegating). Three `ScopesMatch` sites are unreachable by
  editing `ScopesCompatible`: two in `internal/match` compare a pattern's scopes to a template's (a
  different relation), and `resolveNodeByScopes` is the frame-reclaim authority, whose false positive
  is corruption, so any change to the relation must reach it on purpose. `pkg/values/scope.go`'s doc
  now names all nine delegating consumers and that exception.

- [ ] **PRECONDITION 1 for any future S4: `ScopedBindingKeyOf`'s split is only half
  pinned** [Low, S, filed 2026-09-10]: the collapse direction has four ratchets
  (setting `ScopeKey := ""` reddens `validate_test.go`,
  `lambda_formals_hygiene_test.go` ×2, `framereclaim_scope_collision_test.go`), so
  `findDuplicateSymbols` is NOT unpinned. The frame-reclaim direction was fail-open and
  is now guarded by `TestBuildReclaimGraph_CollidedIsScopeKeyed`
  (`pkg/internal/validate/frame_reclaim_build_test.go`, landed `57353e22`), which is
  deliberately **scope-CONTENT-specific** — a uniform constant suffix appended to every
  `ScopeKey` preserves the partition and is invisible to a "duplicates are still caught"
  assertion. What remains open is that any S4 changing what a scope set CONTAINS must
  re-derive the expected key set from `syntax.ScopeFingerprint` rather than from the
  producer under test, or the ratchet stops discriminating.

- [ ] **PRECONDITION 2: `GlobalIndex.EqualTo` and `MaybeAppendLiteral` disagree about
  what a pin IS** [Low, S, filed 2026-09-10]: `EqualTo`
  (`pkg/environment/global_environment_frame.go`) compares `Index`, `Env` and `Slot`
  and deliberately ignores `query`, `phase` and `sealed`; the literal pool dedups
  through it (`pkg/machine/native_template.go` `MaybeAppendLiteral` → `literalIdentical`,
  the linear arm, since `*GlobalIndex` is not `Hashable`). So two pins that differ only
  in their re-resolution coordinates merge into one literal. The fields exist precisely
  to survive slot DEATH, which is after the merge has happened.

  **Widening `EqualTo` is not obviously free, and the usual objection is measured
  away.** The fear is that including the fields makes two pins to one LIVE slot
  distinct literals. Measured: widening `EqualTo` to compare `phase`, `sealed` and
  `query` moves the literal pool by **0 entries** — 9599 appends / 1809 `*GlobalIndex`
  literals, identical before and after, over a KitchenSink engine plus a program
  importing 14 libraries and defining three macros. The structural reason is in
  `GlobalIndex`'s own field doc: while a slot lives it is named by exactly one
  `slotRef`, so `(Env, Slot)` already determines `(phase, sealed)` and those two can
  never discriminate. Only `query` can differ for one live slot, and it did not occur.
  One test does go red, `TestCompileContext_CompileSetBang`, and it is a
  test-construction artifact: it builds an expected `*GlobalIndex` by hand, so its
  coordinates are zero values. Cost of the fix is therefore that one test, not pool
  growth — but re-measure on the workload of the day rather than trusting this line.

- [x] **`FreeIdKey`'s blast radius, and the encoding invariant any S4 must prove** [Done
  2026-09-10]: 3 production call sites (`compile_syntax_rules.go` ×2, `syntax_expand.go`); the inverse
  `FreeIdName` has zero production callers. The contract pin is `TestFreeIdKey_DiscriminatesScopeAndName`,
  which the design's ratchet list omits. The "nine failures across four packages" figure from
  `plans/2026-07-18-scope-keyed-global-bindings-design.md` is provenance, not a measurement of today's
  tree ([[freeids-collapse-is-wrong-pin-not-keying]]). **Residual (precondition):** `FreeIdKey` is
  `ScopeFingerprint(scopes) + "|" + name` and `FreeIdName` cuts at the FIRST `|`, which is unambiguous
  only because the fingerprint's charset is `[0-9,]`. Any slice putting a phase into the fingerprint
  must prove no `|` enters it, or every key silently truncates and surfaces as a wrong self-reference,
  not a parse error; a negative phase adds `-`, so move the charset doc with the encoding.

### Name-keyed identity survives in consumers of scope-keyed bindings (2026-07-19)

Consequences of `8afeb66a`/`a60e32e1` making one name own several slots, each previously visible only
inside a `plans/` section. The count grew from three to eight across two later reviews, which is the
argument against trusting a hardcoded one. **All eight are closed** (2026-07-29); the one open item
below is a separate pre-existing defect found while measuring #805.

- [x] **Frame-reclaim's verdict domain was name-keyed** [Medium, M, Done 2026-07-23, branch
  `fix/framereclaim-scope-keyed-verdict`]: `ClassifyFrameReclaim` returns
  `map[validate.ScopedBindingKey]bool`, the `{Sym.Key, ScopeFingerprint(scopes)}` identity shared with
  `findDuplicateSymbols` and `match.FreeIdKey`, not the physical-slot `BindingID`; hygiene-distinct
  same-name defines get separate verdicts. Edges resolve by subset over nodes without consulting the
  binding, completing the T1.5 decoupling. **Subset, not fingerprint equality, is load-bearing** for a
  nested cross-call whose ref scopes exceed the callee's (`TestFrameReclaimSeam_LetNestedMutualCallResolves`).
  `resolveNodeByScopes` **refuses** an ambiguous maximum rather than pick by map order, since
  `GetBinding`'s creation-order tie-break cannot be cheaply replicated and a false positive corrupts
  (`TestResolveNodeByScopes_AmbiguousMaxRefusesToGuess`). Chose scope-set identity over `*Binding`
  pointer keying.
  - [x] **Sub-hole: the verdict leaked below top level** [Correctness, S, Done 2026-07-20,
    `82046952`]: `CompileValidatedLet` compiles a let body on the same compiler with `p.env` swapped
    (the package's only such swap), so an internal define sharing a top-level `Key` collected the
    top-level verdict. Fixed by reading through `unitFrameReclaimVerdict`, which can withhold a verdict,
    never grant one (`pkg/wile/framereclaim_letbody_leak_test.go`). Identity keying now also covers
    it; the gate is kept as an explicit tightening.
- [x] **`MaybeCreateLocalBinding` used `ScopesCompatible` where exact equality is correct** [Medium,
  S, Done 2026-07-20]: a `{m}`-scoped binder could reuse and clobber a `{}`-scoped slot. **The bug was
  using a visibility predicate to decide identity**; creation now calls `scopeSetsEqual`, the global
  creation predicate itself, so the two paths cannot drift, and lookup keeps `ScopesCompatible`.
  Latent: no known Scheme program reaches it (a body binder carries the body scope; nil-passing
  callers use dedicated frames), so the guard is a unit test,
  `TestMaybeCreateLocalBinding_EmptyScopedSlotNotReused`. Only one direction was broken, so a
  one-directional test would pass a wrong fix. The reuse branch's `Scopes` backfill, half the clobber
  mechanism, was deleted rather than pinned.
- [x] **`DeleteBinding` was name-keyed while the namespace read surface is scope-exact** [Medium, S,
  issue #805, Done 2026-07-20]: `(namespace-undefine! ns 'x)` destroyed a macro-introduced `x` that
  `namespace-ref` reports unbound. `DeleteBinding(sym, scopes)` now resolves through the read's own
  resolution (today `resolveAtCoordsLocked`), so delete cannot drift from ref. **Delete has no
  wildcard mode**: `nil` means the empty set, deliberately diverging from the file's then-current
  nil-means-match-any convention, so the ambient footgun cannot fire destructively. The old
  delete-all tests were inverted, not updated (`TestGlobalFrame_DeleteClearsMultiSlotNameOneScopeSetAtATime`,
  `TestNamespaceUndefine_RemovesAmbientAndSparesMacroBinder`). The sealed-base probe became
  scope-exact too: a wildcard probe raised `ErrImmutableBinding` for a name the read calls unbound, a
  spurious denial rather than a permission.

- [ ] **`namespace-undefine!` does not stop compiled code from reading the binding**
  [Correctness, S, 2026-07-20 — root cause MEASURED 2026-07-21, fix DEFERRED as disproportionate]:
  found while measuring #805, **pre-existing on master**. After `(define v 7)` `(define (get-v) v)`
  `(namespace-undefine! ns 'v)`, `namespace-bound?` correctly answers `#f` but `(get-v)` still
  returns `7`. **Measure-first done — it is the global binding cache, not a re-resolving pinned
  index.** Disassembly shows `get-v` compiles to `OpLoadCachedBinding` (`bindings: [7]`): the
  closure captures the `*Binding` **pointer** at compile time and reads `cachedBindings[i].Value()`
  directly, never consulting the slot that `DeleteBinding` nils. This holds in BOTH mutable and
  immutable top level (the cache captures a live location so `set!` still works through it). A
  bare `OpLoadGlobal` re-resolver *does* observe the nil'd slot (with a redefine after undefine,
  `get-v` errors "no such global binding"), which is why two references to the same undefined name
  can disagree. **Why deferred**: making cached reads observe deletion requires a per-read check in
  the VM's hottest opcodes (`OpLoadCachedBinding`/`OpPushCachedBinding`/`OpCallCachedBinding`) —
  there is no existing "undefined value" sentinel to reuse cheaply — a poor risk/perf trade for a
  non-standard reflective primitive, in the exact global-read path the memory repeatedly warns
  against ([[global-binding-cache-already-exists]]). The current behavior (undefine removes the
  NAME — `bound?`→#f, new references fail — while closures keep the captured LOCATION) is also
  defensible lexical-capture semantics. The `DeleteBinding` comment ("stale GlobalIndex references
  … see nil, caught by resolveGlobal") was corrected to note the cached-pointer path; since
  `DeleteBinding` folded into `DeleteBindingAt` (`c6eb4302`) that NOTE lives in `removeSlotLocked`'s
  body, not in either method's doc. Still reproduces: `(#f 7)` (2026-10-16). Needs a maintainer call on whether the hot-path cost is worth it before proceeding.

- [x] **Scope-set resolution let the zero value answer an unanswerable question** [Correctness +
  API, M, Phases 0–2 done 2026-07-23, `d2e2e625` + `eaee2ed0`]: the read surface treated a nil scope
  set as MATCH ANY, against the nil-means-NONE convention, so a caller that forgot to thread scopes
  silently got a wider resolution, and that resolution was `slots[0]`, an expansion-order artifact,
  not a union. An earlier "fail-open / security posture" framing was withdrawn 2026-07-20: the effect
  can land either way (see the spurious denial above) and hygiene is a correctness boundary, not an
  authorization one. **Fix**: `values.ScopeSet` carries All / empty / specific, collapsing
  `AmbientScopes()`, the riding `matchAny` bool, and `GlobalIndex.scopeKeyed`; nil is un-passable at
  the query surface and `scopesToQueryMatchAny` is deleted. All 58 call sites were **triaged per
  intent, not mass-rewritten**, since a mechanical `nil` → `AmbientScopes()` sweep would freeze
  accidents into explicit form. **Two wildcards kept deliberately**, with rationale comments at each:
  `compile_syntax_form.go`'s pattern-var lookup (pattern vars are bound scopeless innermost, and a
  scoped query switches to maximal resolution where an enclosing lexical could outrank them), and
  `expander_time_continuation.go` ARM 3 (the helper's definition scopes are not a subset of the use
  site's, so a scoped query breaks cross-library helper resolution). Narrowing both kept the suite
  green, so the verdict rests on resolution semantics, not the suite.
- [x] **`freeIds` collapsed scope-verified answers into a name-keyed map** [Correctness, S, Done
  2026-07-20]: two same-named template identifiers with different scope sets resolved correctly, then
  the second overwrote the first in storage. Reachable from surface Scheme: a macro-generating macro
  gave `(99 99)` where hygiene demands `(1 99)`. Fix: `match.FreeIdKey(name, scopes)` at the one writer
  and one reader. Exact-scope-set keying, not Flatt-maximal, is right because it replays a
  per-occurrence answer keyed by the same immutable template symbol at both ends. Pins:
  `TestMacroGeneratingMacro_SameNameFreeIdsDoNotCollapse`, `TestFreeIdKey_DiscriminatesScopeAndName`.
- [x] **`BindingID` needed a scope discriminator before the load-order plan shipped** [Correctness +
  sequencing, M, MOOT 2026-07-24]: the load-order plan is archived as superseded, so its
  `BindingID{Origin, Phase, Sym, Local}` will not be built; its spec was corrected anyway to carry
  `ScopeKey string`, because a name-keyed version would have been a fourth wrong notion of "same
  binding" instead of the one that replaces three. The shipped `BindingID{*LocalEnvironmentFrame, slot}`
  is a physical local slot and is not this.
- [x] **The self-tail / frame-reclaim family decided self-identity by name** [Correctness, M, Done
  2026-07-29, branch `fix/selftail-scope-identity`]: both repros failed open (hangs), each against a
  distinct-name control.

  | repro | mechanism | before | after |
  |---|---|---|---|
  | (a) named-let arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (a) `define` arm | emit gate | 2 sites / hangs | 1 site / `300` |
  | (b) macro-hidden `set!` | shadow set | 2 sites / hangs | 0 sites / raises |

  The `define` arm was a second LIVE repro, not "exposed but unproven": immutable top level is the
  DEFAULT, so it arms (`TestSelfTailEmit_DefineArmEscape`). In the arming ratchet, `tak` explicit-
  **mutable** 0 is the discriminating row, not the immutable one. **Roles, from the design pass**
  (`memory/2026-07-29-name-keyed-identity-residuals-{design,impl}.md`, Branch B): the emit gate is the
  *authority* (a false positive corrupts), `calleeCaptureSafe` is *safety*, the mutation/tail walks
  are *applicability* (errors are deopts once the gate is exact). So 7 signatures move to identity
  and 5 keep `string`; `exprMutatesName` **must** stay name-only, since narrowing it risks missing a
  `set!` of the real self. Self-identity is resolved-`*Binding` comparison (subset-based:
  `ScopedBindingKey` equality would silently total-deopt every loop). `nameSet` is
  `map[string][]localBinding` with per-entry scopes, not `ScopedBindingKey`-keyed, because a
  fingerprint cannot answer a subset query. The shadow query is **tri-state** because
  `exprMutatesName` reads "shadowed" with the opposite safety polarity from the other consumers; its
  `shadowUnknown` branch killed nothing and is pinned as a tightening
  (`TestExprMutatesNameReportsThroughAmbiguousTie`). Two plan corrections: internals take the
  resolved `*Binding`, not `selfSym`; and the unresolvable-self guard's "fails open" justification
  was wrong (the walkers stay name-fed), so it is kept on another ground: a wrong env becomes an
  arming-count drop the ratchet catches. `LetBindingFrameReleasable` / `InternalDefineFrameReleasable`
  pass `self = nil` deliberately.
- [x] **`CompileSymbol`'s definition-time pin outranked the scoped *global* match** [Correctness, S,
  Done 2026-07-29, branch `fix/compilesymbol-cointroduced-global`, `bff525a8`]: Option A with the
  cardinality guard, a global-only arm between `GetLocalIndex` and `tryResolvedBinding`
  (`GetGlobalIndexWithScopes` + `GetOwnGlobalBinding`, not `GetBinding`, which searches locals first).
  `(entry 0)` 0 → 21; an unfiled co-introduced-global case went `(100 100)` → `(7 100)`. Dropping the
  cardinality guard (the naive reorder) breaks the cross-library free-identifier class the pin
  protects (`TestGlobalIndexCollision_*`, `TestLibraryInternalSyntaxCaseEllipsisHygiene`). Pin:
  `TestScopeResolution_CoIntroducedGlobalShadowsPin`. **Report correction**: the "user `rec` defined
  after the expansion" variant is the same defect; `predeclareBodyDefines` makes the
  **compilation-unit boundary** the discriminator, not source order. **Residual:**
  `collectFreeIdentifiersWithEllipsis` calls every non-pattern-variable template identifier free,
  including ones the template binds; filtering is undecidable in general (a template may use a
  binding macro), so it would be a partial precision gain, not a substitute (design's "Option D
  rejected").
- [x] **`lookupMacroBinding` arm 1 had no cardinality guard (for-syntax symmetry)** [Correctness, S,
  Done 2026-07-29, branch `fix/compilesymbol-cointroduced-global`]: proven reachable; the filing was
  right about the phase and wrong about the site. `begin-for-syntax` / `define-for-syntax` /
  `eval-when` root the expander at `p.env` and climb via arm 2's `NextPhase()`, which is why the filed
  probes could not discriminate. The reaching site is a **procedural transformer body**:
  `compileAndEvalLambdaTransformer` (shared by `lambda` and `er-macro-transformer`) roots at
  `env.NextPhase()`, where phase-0 `define-syntax` deposits `∅`-scoped keywords. `user-helper` →
  `lib-helper` (`TestForSyntaxPin_TransformerBodyKeepsPinnedTemplateID`). Fixed by **demoting** the
  `∅`-scoped arm-1 match below the pin, not dropping it: arm 1 is the only arm that finds a
  current-phase macro, so a drop regresses every direct macro reference in a transformer body. Both
  arms share `coIntroducedByExpansion`. Phase-0 expansion was confirmed safe: no `∅`-scoped syntax
  global lands in phase 0.

### plans/ sweep — correctness deltas not previously in TODO (2026-07-21)

Correctness work that lived only inside plan files, invisible to a TODO scan.

- [x] **stderr data can be lost on exit** [Correctness, S, Done 2026-07-21, no longer reproduces]:
  `writeAndFlush` (`pkg/extensions/io/prim_write.go`) flushes after every write primitive, so no
  output is lost on any exit path (verified on the built binary). That supersedes the 2026-05-14
  flush-on-exit design, which said "no per-message flushing is added" and would now be redundant
  machinery; it also missed `(exit)`/`(emergency-exit)` in `extensions/system/prim_system.go`, the
  bug's own canonical repro. `memory/2026-05-14-stderr-flush-on-exit.md`.
- [x] **SRFI-18 `thread-join!` wraps an uncaught exception** [Correctness/conformance, S, Done
  2026-07-21]: a joined thread that died on an uncaught exception surfaces as an
  `uncaught-exception` whose reason is the original condition (`eq?`-preserved), not the bare
  re-raised condition. Q1 = A: wrap unconditionally, strict SRFI-18. Guard:
  `TestThreadJoinWrapsUncaughtException`.
- [x] **SRFI-18 exception predicates + `thread-state`** [Correctness/conformance, S, Done
  2026-08-02]: `join-timeout-exception?`, `terminated-thread-exception?`, and
  `abandoned-mutex-exception?` added. The three conditions are now opaque `values.Value` handles
  raised via `machine.RaiseInPlace`, not string-only error-objects. Behaviour changes: joining a
  `thread-terminate!`d thread raises terminated-thread-exception, not uncaught-exception; none of
  the three is `error-object?`. `thread-state` is Gambit's, **not** SRFI-18 (which has only
  `mutex-state`); the stale `pkg/values/thread.go` comment claiming otherwise was corrected. Guards:
  `TestSRFI18ExceptionPredicatesDiscriminate`, `TestThreadState`. **Residual:** `(current-thread)`
  returns the symbol `'primordial` on the main goroutine, so `(thread-state (current-thread))` errors
  there; a pre-existing SRFI-18 gap.

> **The load-order plan is dead (archived 2026-07-24).** Part II's motivation is discharged: the C6
> capture is fixed by the bootstrap reorder (`1af62cd2`) plus the free-template-id-hygiene arc
> (PR #814), and `TestBootstrapMacrosPinLateBoundReferents` is a CI ratchet reporting 0 capturable
> nil pins — the regression guard Part II's cure would have provided. Part III is obsoleted by
> having been DONE under a different mechanism: all three sites it named compare provenance-root
> origin today — `sameFreeIdentifier` (`pkg/registry/core/prim_syntax.go`) and `erBindingsEqual`
> (`pkg/machine/compilation/er_macro_compare.go`) through `environment.SameBinding`, and import
> conflict through `importConflicts` (`pkg/machine/compilation/library_bindings.go`, `5d654b3f`).
> Nothing regressed. The value-identity type Part III proposed under that name was never built;
> the `environment.BindingID` that does exist is an unrelated local-frame map key
> (`{*LocalEnvironmentFrame, int}`) and cannot name an imported binding at all. One narrow
> residual survives, below.

- [x] **`free-identifier=?` and ER-compare were non-conformant on COMPLEMENTARY cases**
  [Correctness/conformance, M, verified 2026-07-24 vs Racket + Chez, low impact; **conformance FIXED**
  (`70a34421` + `36e1d268`); the checkbox closes the conformance defect, and the one Phase-2
  consumer still open below (`,doc`) is tracked in its own right, not as unfinished work on this
  defect]: `free-identifier=?` compared bindings by pointer, wrong on rename-aliases — two
  rename-imports of ONE binding answered "different" where Racket and Chez say same.
  `erBindingsEqual` added a `BindingType()`+`Value()` fallback, wrong the
  other way — two DISTINCT defines of the same value answered "same". **So pointing
  `free-identifier=?` at ER-compare was not the fix**; it swaps one wrong answer for the other.
  Neither pointer-eq nor value-eq is the correct notion. The conformant one is same binding
  **origin**, which Wile stored nowhere: imports copy the value into a fresh cell and a fresh
  `*Binding`, and `markBindingImported` recorded only a boolean.
  **Fixed** by folding each imported binding's root `OriginRef{RootLib,RootName}` at import (keyed on
  `internalName`, no traversal) and comparing via `environment.SameBinding`. ER-compare was pulled
  from Phase 1 and finished as option B: switching it to `SameBinding` *alone* regressed
  internal-vs-import compare `#t`→`#f`, because a definition-site rename had nil Origin while the
  import had one. `stampLibraryExportOrigins` closes that by giving every library export its own
  self-root at library **finalization** — chosen over an import-side-effect stamp so Origin stays a
  pure function of the definition and imports never mutate library-internal state — and the value
  fallback is deleted. Import-*conflict* detection reached the same answer independently: PR #793
  turned down a SOURCE-LOCATION origin there for false-flagging a legal define-over-import shadow,
  and `5d654b3f` then shipped provenance-root origin, which cannot have that failure. Design:
  `plans/2026-07-24-free-identifier-origin-provenance-design.md` (model: "same binding" = same
  root in the provenance graph Wile already walks at import, then discards).
  **Phase 2 — 2 of 3 shipped:**
  - [x] `stampImportedInlineHOF` gates on `Origin.RootLib` [Done 2026-07-24, `9b2afa8c`]: fixed the
    latent re-export miss and a coupled miscompile, where inline dispatch keyed on the call-site
    name, so a curated HOF renamed onto another curated HOF's name inlined the wrong body. Dispatch
    keys on the stamped `BindingMeta.InlineHOFName`, reset on every re-import.
  - [~] `,doc` follow `Origin` instead of eager-copying `Doc` at every import hop. **The defect this
    would have incidentally fixed was found and fixed on its own 2026-07-25, and needed no origin.**
    Repro: two libraries exporting one macro name, the first documented and the second not — under
    the §5.2 by-name conflation the second won the value while `,doc` kept reporting the first's
    docstring. The cause was not the copying but the `if doc != ""` guard on it, which made the field
    un-clearable: the same staleness class as the inline-HOF stamp reset three lines above it, which
    the doc field was never added to. Unconditional assignment (`library_bindings.go:127`) is smaller
    than the guarded copy it replaced, and the guard protected nothing (full suite green without it).
    Guard: `TestImportedMacroDocTracksTheWinningValue`, verified RED first. Procedures were always
    immune — a closure carries its docstring on its template, so it tracks its value for free.
    **What remains is structural-only and BLOCKED as filed**: `Binding.Doc()` lives in
    `pkg/environment`, but resolving `OriginRef` needs `LibraryRegistry.Lookup` + `findLibraryBinding`
    in `pkg/machine/compilation`, three layers up; `OriginRef.RootLib` is a `string` key, not a
    `LibraryName`, so even from above it needs a reverse lookup or a scan; and `repl/` is
    deliberately decoupled from `machine/compilation`. So the options are re-couple, inject a
    resolver into `environment`, or store a `*Binding` (rejected by D2). With no defect left
    motivating it, **re-justify before starting, or drop it.**
  - [x] Site 3, import-conflict detection [Done 2026-09-16, merge `5d654b3f`]: the by-name/`EqualTo`
    comparison and the "irreducible gap" it documented are gone. `importConflicts`
    (`pkg/machine/compilation/library_bindings.go`) compares provenance roots (`*OriginRef`) and
    nothing else, matching Racket's "identifier already required" rule. The gate this row set — go
    re-read PR #793's actual objection before adopting origin here — was discharged, and the
    hypothesis it recorded held: #793 rejected a SOURCE LOCATION, a define-over-import shadow is a
    non-imported local, and `importConflicts` returns early on `!existing.IsImported()`, so a
    define never reaches a root comparison. `TestImportConflictDetectionByOrigin`
    (`pkg/wile/engine_import_composition_test.go`) pins the three cases the gap named — same-named
    procedures, same-named macros, equal constants — as conflicts, and a diamond as legal.
### Layered-environment carve regressions (review `d8911c15..b04c6d74`, 2026-06-15)

Sealed-base carve + immutable-top-level-default arc. Two root patterns: own-frame `Keys()`
iterators that did not span the sealed base, and the immutable default reaching contexts the
design meant to stay mutable. **Scope decision (2026-06-15):** immutability is scoped to
**compilation units only** (Chez model) — immutable for files and `-e` batches, which preserves
the frame-reclaim GC win; mutable for every interactive/eval context (REPL, `--mcp`,
`(environment …)`, `scheme-report-environment`). Implemented as a root-namespace property, with
child namespaces always mutable and the `set!`-gate keying on `IsStable()` directly so anchors
stay protected inside mutable children. Design:
`memory/2026-06-13-immutable-toplevel-by-default-scoping.md`. 14/15 closed; D1 survives.

- [x] **A1–A4, read paths dropped every sealed-base name** [Done 2026-06-15]: own-frame `Keys()`
  walks lost sealed-base names (`caar`, `map`, `zero?`, `call/cc`, the cxr accessors).
  `collectBindingNames` (REPL completion) and `searchEnvironmentBindings` (`,apropos`) now walk
  `SealedBase()`; the completer test had asserted only `car`, which survives via the expand phase and
  masked the bug. `namespace-undefine!` on a sealed name raises `ErrImmutableBinding` instead of
  reporting false success (`TestNamespaceUndefineSealedRejected`).
- [x] **B1–B4, the immutable default reached entry points meant to stay mutable**
  [Done 2026-06-15]: each REPL line is its own unit, so a redefinition was rejected. The CLI REPL
  adds `WithMutableTopLevel()`, `mcp.go` is always mutable, `runEval` begin-wraps `-e` like
  `runFile`, and child namespaces are mutable (`TestSealedBase_B3_*`). B4 closed as documentation:
  `internal/bootstrap` mutable while public `NewEngine` is immutable is mechanism vs policy, not a
  split brain.
- [x] **C1–C3, two divergent `Stable`-stamping mechanisms** [Done 2026-06-15]: a profile child
  allowed `(set! car …)` but rejected `(set! caar …)`. Profile children are mutable, and the
  `set!`-gate keys on `IsStable()` directly, decoupled from `ImmutableTopLevel()`, so a Stable anchor
  is never `set!`-able even in a mutable child (`TestSealedBase_C1_*`). C2 closed as a doc fix:
  freezing bootstrap procedures is intentional (they are anchors, and narrowing the stamp loses the
  user-recursion GC win), so `docs/reference/r7rs-differences.md` was corrected, not the code.
- [x] **D2, lock asymmetry on a thread-shared global** [Low, M, pre-existing, Done 2026-07-01,
  `fbcd7654`]: global bindings publish through an `atomicCell` (atomic publish, lock-free load),
  with the `noCopy` pointer in the heap cell so `Binding` stays copylocks-clean; locals keep the
  plain field. "In a global frame ⇒ has a cell" is structural. `binding_race_test.go` is red under
  `-race` pre-fix. **Cost: +4.6% geomean on bench-gabriel, 15/16 slower** (`Binding` 32→40B),
  accepted; the shrink-`Binding` recovery lever is a Tier 4 follow-up.
- [x] **E1–E3, altitude, duplication, and a per-import re-walk** [Done 2026-06-15]:
  `EnvironmentFrame.MutableRuntime()` replaces 8 `.Namespace().Runtime()` sites (deliberately not
  `Runtime()`, which diverges for library frames; `TopLevel()` returns the frozen base). The two
  `loadBootstrapSources` had behaviourally diverged (one skipped optimization and pooling), so
  `compilation.LoadBootstrapSources` and `wireRuntimeFrames` are now the single sources.
  `registerSchemeDocstrings` re-parsed ~500 docstrings on every `(import …)` and is now guarded by
  `!env.IsNamespaceRuntime()`.

- [ ] **D1 — sub-context/thread capture can leak library-eval defines** [Low, S — DEFERRED 2026-06-15, arguably-not-a-bug]: `machine/machine_context_subcontext.go` now captures `MutableRuntime()` (named) which returns the engine-root mutable global even from a library frame. For SRFI-18 THREADS this is correct by design (threads share the engine global). The only edge is a sub-context (`load`/`call-with-exit`) spawned *during a library's own load* landing defines in the engine global rather than the library frame — an extreme, untested case. Revisit only if a concrete isolation bug surfaces; not worth a speculative fix.

---

- [x] **Data race: error/backtrace capture vs concurrent VM mutation under `thread-terminate!`**
  [High, M, Done 2026-06-13]: `NewThreadSubContext` set a thread's `parentMC` to the live spawning
  context, so every `parentMC` walk (`CaptureStackTrace`, `findParameterInMarks`, the pool release
  counter) crossed goroutines. The root cause is narrower than "terminate doesn't quiesce":
  `parentMC` is for synchronous sub-contexts only, and an earlier fix snapshotted the parent's fields
  but kept the pointer. Fixed by severing `parentMC` for threads. Trade-off: cross-thread
  dynamic-parameter inheritance dropped (a racy live read, not SRFI-18-correct). Not surfaced by
  `make ci`, which did not run `-race` on threads. Same family as PR #561 (concurrent apply).
- [x] **Five recursion-depth bounds, counted identically** [Medium, Done]: `DefaultMaxCallDepth`,
  `DefaultMaxParseDepth`, `DefaultMaxExpandDepth` (50000, `WithMaxExpandDepth`),
  `DefaultMaxWriteDepth` (10000, reused by `Value.SchemeString`). All count root = 1, +1 per
  container descent, so anything the writer emits reads back. **Length ≠ depth was the recurring
  bug**: spine recursion overflowed the host stack on long flat lists; spines now iterate and recurse
  only into elements. Cycle detection does not bound depth. The expander guard is shared **by
  pointer** across child expanders (the parser's is per-parse). `SchemeString` cannot raise, so it
  degrades to `#<deep>` (vs the cycle marker `...`). No `WithMaxWriteDepth`: the writer has no
  engine-owned entry point.
- [x] **Parser fuzz targets + reader crash-safety hardening** [Medium, Done]: `FuzzReadSyntax` (no
  panic; every non-EOF error a located `*ParserError`) and `FuzzReadWriteRoundTrip` found 8
  pre-existing reader bugs in ~2 minutes, 5 of them host panics; corpus in
  `pkg/parser/testdata/fuzz/`. Leaked foreign error types were closed as a class by the boundary
  catch-all `locateReaderErr`; nil-at-delimiter derefs fixed individually. The round-trip target
  also caught `String.SchemeString` using Go `%q` instead of R7RS `\xHH;`. Float64-overflowing
  scientific notation promotes to `BigFloat` in the reader and `string->number`. **Deferred, since
  fixed:** big floats write with the `l` exponent marker and round-trip (`7de98219`, 2026-07-31).
- [x] **Unify complex/imaginary number parsing** [Medium, Done, staff-sweep #5]: the reader and
  `string->number` duplicated the rectangular-complex grammar and had drifted into two different
  wrong answers on `+3/4i` (R7RS §6.2.5: exact `0+3/4i`). The shared grammar is fixed
  (`isExactPartString`) and the reader delegates to the pure functions, adding only location. Guard:
  `TestParseNumber_ReaderAgreesWithStringParsers`.
- [x] **Stable-matching selectors failed; matching tests did not gate CI** [High, M, Done]:
  `walk-for-cycle` stored rotation cycles newest-first (an extra `reverse`) while `apply-rotation`
  needs oldest-first; a 2-cycle is self-inverse, so the 2×2 fixtures masked it. **The CI-gate gap is
  the more important half**: 12 chibi-test files lacked `(test-exit)` and `sat-test.scm` printed
  "FAIL:" and exited 0. All now gated.
- [x] **Audit `PrimitiveSpec` `ReturnType`/`ParamTypes` annotations** [High, L, complete]: the
  four-axis framework (docs ↔ annotation ↔ implementation ↔ R7RS) is closed. Declared-too-narrow is
  **empty** (3 false positives); declared-too-wide is ~85 TypeConstraint-vocabulary gaps; ~25 are
  R7RS sub-domain refinements below `ValueType` granularity. Wile-specific primitives need a **local
  spec before they can be audited**. Load-bearing once extension contracts check at compile time,
  when unsound annotations reject valid programs. Next: vocabulary extension, separately scoped.
- [x] **Silent failures in `compilation/operation_syntax_case.go`** [Medium-High, Done, PR #732]:
  `matcher.Match` errors are no longer swallowed as "no matching clause" (gated on
  `errors.Is(err, match.ErrNotAMatch)`; the `nolint:nilerr` was the smoking gun). The bind loop's
  three branches are separated instead of falling through to `SetLocalValue(li, nil)`, and
  `SyntaxCaseState()` distinguishes nil from a type mismatch (a wrong type *can* be stored since the
  marker-interface revert). Messages gained input and source context.
- [x] **Exceptions and error stack traces** [Medium, Done, PR #657]: `SourcedError` in
  `compilation/`; `CompilationError.Source` is populated from the cause chain; datum-level functions
  have no syntax context, so callers wrap. **Residual:** foreign stack-trace entries for Native →
  Foreign → Native crossings (P3), `memory/2026-04-14-error-stack-traces-design.md` §P3.
- [x] **`read` mid-parse EOF raises a read-error instead of returning EOF** [Done]:
  `wrapMidParseEOF` converts `io.EOF` to a `ParserError` wrapping `io.ErrUnexpectedEOF` at all four
  mid-parse sites; the primitives' `errors.Is(err, io.EOF)` check rejects it and
  `WrapForeignReadErrorf` makes `(read-error? e)` `#t`.

- [x] **Error type identity** [Medium, Determined]: `CompilationError` and `RuntimeError` are
  **public boundary types**, translating internal errors to the embedder API; they should NOT
  implement `SchemeError` or `ForeignError`. Embedders match them with `errors.As`.
- [x] **vmState field coverage test** [High, S]: reflection-based, enumerating `vmState` fields and
  asserting each appears in a coverage table keyed by operation, so adding a field without handling
  it fails the build rather than silently corrupting state.
- [x] **MCP eval fails on schelog `include`** [Not a bug]: the report was missing `puzzle.scm` and
  `(set! *schelog-use-occurs-check?* #t)`; without the occurs check the puzzle infinite-loops into
  the MCP timeout.

### Macro-system defects found 2026-09-04

All five reproduced on v1.20.0 (`8ef4c75b`); three no longer reproduce and were closed 2026-09-26,
so neither the blanket reproduction claim nor the blanket owner survives. A fourth, the
lone-identifier row, closed with P4.4 on 2026-10-15. Two rows are open here and they do not share
an owner.

The ER-scope-stripping row (§5.1) is owned by
`plans/2026-09-04-scheme-specified-syntax-forms-design.md` §5 and closes with its phases, not with a
point fix. Re-measured at `956604f0` (2026-10-16): with the for-syntax import the repro needs for
`cadr`, the default Go layer returns 2 where its `syntax-rules` twin returns 11; the Scheme layer
already answers 11.

The third is the phase-1 scope-refusal wording, owned by the **provenance wave** rather than by that
design. It was lifted out of the `begin-for-syntax` row below, whose ordering half was fixed
2026-09-14 and whose diagnostic half was not.

- [ ] **ER macros strip scopes from pass-through identifiers** [Correctness / hygiene, S, design §5.1]:
  `invokeERTransformer` (`expander_time_continuation.go`) calls `UnwrapAll` on the input and re-wraps
  the output with bare use-site context, so any identifier that merely passes through an ER macro
  loses its scopes. `(import (for-syntax (scheme base)))` (`cadr` is not in the phase-1 vocabulary),
  `(define-syntax er-id (er-macro-transformer (lambda (f r c) (cadr f))))`,
  `(define-syntax via-er (syntax-rules () ((_ e) (let ((tmp 1)) (er-id (+ tmp e))))))`, then
  `(let ((tmp 10)) (via-er tmp))` returns **2**; the `syntax-rules` twin returns 11. The comment at
  the site calls the asymmetry deliberate; the capture it causes was not on record. Dies with the shim
  when ER is derived from `syntax-case` (design §3.5). Fixed on the Scheme layer since `ce2dad3d`
  (answers 11 under `WithSchemeSyntaxForms`, pinned by `TestP2_ERPassThroughKeepsScopes`); open only
  on the default Go layer.
- [x] **`define-syntax` dispatches on the transformer's head symbol** [Conformance, Done 2026-09-26,
  does not reproduce]: filed as design §5.2 and as the P0 prerequisite. The symbol the row named
  exists in no `.go` file; the live one is `compileTransformerValue`
  (`pkg/machine/compilation/compile_transformer.go:60`), which expands and evaluates the right-hand
  side as an expression one phase above `env`, as R6RS requires, instead of switching on its head
  symbol; its own doc comment records the replacement and cites §5.2. Measured at HEAD: the
  macro-produced transformer `(define-syntax my-er (syntax-rules () ((_ p) (lambda (stx) (p stx)))))` then
  `(define-syntax m (my-er (lambda (stx) #'1)))` prints **1**, the `let-syntax` half **42** and the
  `letrec-syntax` half **43**, all exit 0. Not tabled work and not a P0 prerequisite still open: the
  behaviour is pinned in default CI by `TestP01_TransformerIsAnExpression`
  (`pkg/wile/syntax_forms_p0_test.go:104`, six rows) and
  `TestP01_SelfReferencePinsThroughMacroProducedTransformer` (:178), whose harness `evalSyntaxForms`
  (:34) builds a plain `WithProfile(KitchenSink)` engine, so they run on the default (Go) layer
  wherever `WILE_SYNTAX_FORMS` is unset, which is the case in CI.
- [x] **`syntax-case` rejects a lone-identifier pattern** [Conformance, Done 2026-10-15, P4.4 I173]:
  `(syntax-case stx () (x #'1))` failed "pattern must be a list" (`match.CompileSyntaxPattern`).
  Fixed as a point fix rather than waiting on the Go `syntax-case`'s deletion: every syntax-case
  pattern and its input are wrapped one level, so a bare variable, `_`, `()`, a vector and a
  constant all compile, and a non-pair input is a non-match (I033). Pinned by
  `TestI173SyntaxCaseWholeClausePatterns` and `TestI033SyntaxCaseNonPairInputIsNoMatch`
  (`integration/syntax_case_whole_clause_test.go`), Chez-checked.
- [x] **Every free local referenced from a `syntax-case` or `with-syntax` body arrives boxed**
  [Correctness, Done 2026-09-26, does not reproduce]: filed as design §5.4. All three shapes the row
  predicted are unboxed at HEAD. `(let ((n 0)) (set! n 1) (syntax-case #'(a) () ((x) n)))` gives
  **1**, not `#&1`; the `with-syntax` twin gives **1**, not `#&1`; and
  `(let ((f (lambda (y) (* y 2)))) (syntax-case #'(a) () ((x) (f 21))))` gives **42**, not "expected
  a procedure, got `#&#<machine-closure>`". The `lambda` control still gives 1. The row's "pin it
  before deletion" instruction is honoured rather than dropped: the `syntax-case` clause-body shape
  was already pinned by `TestBoxedSlotNeverEscapesToScheme` ("an enclosing local read in a
  syntax-case clause body arrives unboxed"), and this close adds the missing `with-syntax` row
  beside it. That row earns its place on a specific argument, not on coverage — `with-syntax` is
  correct only TRANSITIVELY, because `compileWithSyntax` desugars to `syntax-case`
  (`pkg/machine/compilation/compile_with_syntax.go:71`, "Build: `(syntax-case (list expr ...) ()
  ((pattern ...) (begin body ...)))`"), and nothing pinned that the desugared body's free-local read
  is unboxed. Existing `with-syntax` coverage elsewhere (`syntax_case_frame_state_test.go` and
  friends) pins slot liveness, not boxing.
- [x] **A `begin-for-syntax` define is invisible to a later transformer** [Conformance, Done
  2026-09-26, does not reproduce; **not** owned by the design]: `(begin-for-syntax (define (helper x)
  x))` then `(define-syntax m (lambda (stx) (helper #'7)))` prints **7** at HEAD, and the
  `define-for-syntax` variant prints **7**, both exit 0. Measured in FILE delivery, which is the
  delivery this row's own 2026-09-14 correction named as the failing one.

  That correction reclassified the defect as a WITHIN-UNIT ORDERING one rather than a phase or scope
  one, and named the mechanism: `begin-for-syntax` was `expandUnchanged`, so its body ran only when
  the COMPILER reached it, while `define-syntax` compiles and evaluates its transformer during the
  EXPANDER's body scan; within one unit the transformer was therefore built before the preceding
  `begin-for-syntax` had run. That half is fixed, so the correction's delivery table is stale in its
  second row and is not carried forward — file delivery now answers `7`, the same as one form per
  REPL unit. `primitive_expanders_registry.go:52` reads
  `{"begin-for-syntax", (*ExpanderTimeContinuation).expandBeginForSyntax}`: the expander runs the
  compile-time half when it expands the form, and the compiler no longer runs it a second time.
  Guarded by `pkg/wile/phase1_definition_within_unit_test.go`, written RED on `fcf7b99c` — three
  tests, including a library-body delivery and a runs-once-per-unit guard.

  The row's diagnostic residual is NOT closed with it. It is lifted to its own row below.
- [ ] **A phase-1 lookup for an absent name reports a scope refusal** [Diagnostics, S, owned by the
  **provenance wave**; lifted 2026-09-26 out of the `begin-for-syntax` row above, whose ordering half
  is fixed and whose diagnostic half is not]: "with compatible scopes" is a misdiagnosis the
  diagnostic invites. Instrumented at the raise, a repeat lookup under `syntax.AllScopes()` also
  missed (`anyScopeHit=false`), so no scope set would have found it; the scoped arm is simply the
  last one tried. The wording is unchanged and still fires on a name that is absent outright —
  measured at HEAD, `(define-syntax m (lambda (stx) (totallyabsenthelper #'7)))` gives
  `no such binding "totallyabsenthelper" with compatible scopes at phase 1 of this unit's macro
  tower`. That arm should stop reporting a scope refusal when no binding of that name exists at the
  phase at all.
- [x] **A `define-syntax` at phase > 0 is unusable from every rung** [Conformance, Done
  2026-09-13, `2e1e55c2`]: the keyword twin of the `begin-for-syntax`-define item.
  `executeFormsAtCompileTime` (`compile_helpers.go`, shared by `begin-for-syntax` and
  compile-time `eval-when`) and
  `CompileDefineForSyntax` compiled the body against `p.env.NextPhase()` but rooted the expander
  at `p.env`, so a `define-syntax` in the body deposited at N+2 while lookup read N+1: off by
  one rung at every rung. Both sites now root the expander at `expandEnv`, as
  `compileTransformerValue` always did (`expand_and_compile.go:49`); both one-line halves are
  load-bearing. Enclosing-phase macros still resolve through `lookupMacroBinding`'s ambient arm.
  Pinned by `pkg/wile/phase1_define_syntax_reach_test.go` (five rows, plus a guard that a phase-1
  keyword stays invisible at phase 0: the fix lifts the deposit, it does not flatten the tower).
  The `begin-for-syntax`-define item above is a **different mechanism**; it was open when this one
  closed and was itself closed 2026-09-26 as non-reproducing.

### Reader residuals of the 2026-07-31 `#` dispatch rework

- [ ] **`BigFloat` rendering hangs on a huge exponent** [Low, S; found by
  `FuzzReadWriteRoundTrip` 2026-07-31, **pre-existing**, not caused by the reader
  fixes]: re-measured 2026-10-16 at `956604f0`: `(write #I100000000E0000010000000)` takes ~23 s and
  `(write 1e10000000)` exceeds 20 s; the original literal `1e10000010000000` is now a parse error.
  As filed: `(write 1e10000010000000)` took ~11 s, effectively all of it inside
  `big.Float.Text('g', -1)`, which renders the value to roughly 10^7 decimal
  digits before formatting discards nearly all of them. Measured at 11.34 s on
  `47ae48dc` and 11.16 s on `feat/reader-hash-dispatch`; master's fuzzer reaches
  the same class of input within ~14 s, so both branches are equally affected.
  Reproducer: `"#I100000000E0000010000000"`. Deliberately **not** committed to
  `pkg/parser/testdata/fuzz/`, where it would add 11 s to every
  `go test ./pkg/parser/`. Note the artifact the fuzzer writes for this is
  *not* a reproducer: the minimizer times out too, so the saved file holds a
  partially-minimized candidate that passes when re-run. Fix is presumably a
  magnitude bound in
  `BigFloat.SchemeString` (render an exponent form directly rather than asking
  `Text` for shortest-round-trip digits), but the writer's contract needs
  deciding first: what should `write` emit for a value with 10^7 digits?

- [ ] **Delimiter termination for decimal numerals** [Low, S; residual of
  "Reader fixes" decision D1]: `1abc` scans as `1` followed by the symbol `abc`,
  because a numeral is only implicitly terminated when it carries an explicit
  radix prefix (`requireDelimiterAfterRadixNumeral`, `pkg/internal/tokenizer/`).
  R7RS §7.1.1 reads as though *every* numeral requires a delimiter ("tokens which
  require implicit termination … may be terminated by any ⟨delimiter⟩"), so the
  present split is a real semantic difference between decimal and radix numerals,
  introduced for scoping reasons. Extending the guard to `r == 0` is a one-line
  change; the work is measuring the blast radius, which reaches the fuzz corpus
  and every `.scm` under test. Deliberately not bundled with the reader-fixes
  change, which is a three-item feature and would have conflated the two.

  **Scope narrowed 2026-08-09** (W5 cluster A): the ⟨infnan⟩ keywords and the
  unit imaginaries `+i`/`-i` now terminate implicitly too, so `+i2` and
  `+inf.0x` are single identifiers rather than a number plus a second datum.
  That rule lives in the sign-initial arms of `tokenizer_numbers.go`, **not** in
  `requireDelimiterAfterRadixNumeral` — extending the guard to `r == 0` as this
  item proposes would make `+i2` a read *error*, which is equally nonconformant.
  What remains open is only the unprefixed numeral: `1abc` still splits.

  The `#z` / `#m` inline scan is the same family and is **worse**, because it
  truncates silently instead of splitting into two visible datums. `readBigNum`
  scans base 10 and stops at the first character it does not recognize, so:

  | Input | Reads as | Leaves |
  |---|---|---|
  | `#z1/2` | `1` | `/2` |
  | `#m1/2` | `1.0` | `/2` |
  | `#m2+3i` | `2.0` | `+3i` |

  Verified identical on `47ae48dc` and on `feat/reader-hash-dispatch`, so this
  is not introduced by the introducer rework — the degenerate `#z<digits>` path
  is unchanged. Arguably these should be errors (a `#z` operand that is not an
  exact integer is rejected everywhere else: `#z1.5` errors), which makes this a
  narrower and safer fix than D1-b proper.

### Core-form arity check drops a dotted tail (2026-10-15)

- [x] **`expectArgs` truncates an improper spine instead of refusing it** [Conformance, Done
  2026-10-15, `4b69b233` + `951a090f`, merged `419e86c9`]: the loop in `expectArgs`
  (`expander_primitive_forms.go`) stopped at the first non-pair tail without recording it, so
  `(if #t 1 . 2)` collected two arguments, reported ok, and the caller rebuilt a proper `(if #t 1)`;
  the dotted tail never reached the validator and the form ran. Same path for `set!` and
  `with-continuation-mark`, the other two callers. Chez: invalid syntax. The fix returns ok=false on a
  non-empty tail, leaving the tail for the validator to reject; pinned by three dotted-tail rows in
  `TestExpanderPrimitiveFormsErrors`.
- [ ] **`define`'s variable form drops everything after the value** [Conformance, XS, found by the
  `expectArgs` crosscheck]: the simple-variable branch of `expandDefineForm`
  (`expander_primitive_forms.go`) expands the first operand and rebuilds `(define var value)`, so
  `(define x 1 2 3)` and `(define x 1 . 2)` both bind `x` to 1 (measured 2026-10-15; Chez: invalid
  syntax). The validator rejects both (`validateDefineVariable`: "requires exactly 1 value";
  `formPrologue`: "define form must be a proper list") but never sees them. The function-shorthand
  branches already refuse a dotted body. Fix: bail to `formUnchanged`
  unless the value's cdr is `()`, or route the branch through `expectArgs`.

### Filed by the P4.7 crosscheck (2026-10-16)

Pre-existing ER-transformer defects the five-lens review of `fix/er-transformer-keeps-scopes` measured
on master and on the branch alike. P4.7 made the form's leaves identifiers on both layers, which is
what makes the first two newly reachable on the default layer. Each is a Go-layer/Scheme-layer
disagreement on one program, the internal-invariant form of a defect.

- [x] **Go `rename` of an identifier drops its scopes** [Hygiene, S]: `extractSymbolKey`
  (`er_macro_rename.go`) read `v.Key()` off a `*syntax.SyntaxSymbol` and re-resolved the bare name at
  the definition site. With `(define x 'global)` and `rid` returning `(r (cadr f))`,
  `(let ((x 'local)) (rid x))` was `global` on Go, `local` on Scheme, whose `(datum->syntax #'k sym)`
  returns an identifier unchanged. **Contract: keep it**, as the Scheme shim does (refusing would
  make the Go layer reject a program the Scheme layer runs). `NewERRenameClosure` returns an
  identifier argument unchanged and caches only bare symbols; pinned by
  `TestP2_ERRenameOfAnIdentifierKeepsIt` (both layers). Merged `76df7d19`.
- [ ] **Go and Scheme `rename` still disagree off the identifier path** [Hygiene, S, needs a
  decision]: filed by the I172 crosscheck, measured on both layers. A non-symbol datum: `(r 5)` raises
  `ErrNotASymbol` on Go (`NewERRenameClosure`'s default arm) and is `5` on Scheme, whose
  `(datum->syntax #'k sym)` converts any datum. Two bare-symbol renames: `(eq? (r 'x) (r 'x))` is
  `#t` on Go (the per-name cache) and `#f` on Scheme (a fresh syntax object per call). Decide the
  contract (Go accepts any datum, or the shim refuses one; the shim memoizes, or Go stops promising
  `eq?`), then add a `syntaxLayers` row per arm.
- [ ] **Go `compare` accepts a bare symbol and resolves it without scopes** [Hygiene, S, needs a
  decision]: `resolveERIdentifier`'s `*values.Symbol` arm (`er_macro_compare.go`) uses
  `syntax.AllScopes()`. `(c (cadr f) 'else)` answers on Go, raises on Scheme (`free-identifier=?`:
  not an identifier); under `(let ((else 1)) (m else))` the Go answer is a hygiene miss. Raising
  matches the Scheme layer and breaks Go-layer code that compares against a quoted symbol today.
  `docs/reference/r7rs-differences.md` documents the split.
- [ ] **A rename pinned to a definition-site binding captures when used as a binder** [Hygiene, M]:
  `resolveRenamedSymbol` returns the binding's scopes with no intro scope when the name is bound at
  the definition site, so `(let ((,(r 'tmp) ,a)) …)` captures a pass-through `tmp`. `swap!` of a
  global `tmp` (`(define tmp 1) (define y 2) (swap! tmp y)`): Go `(1 2)`, Scheme `(2 1)`, on master
  and after P4.7. The reviewing lens also reported a `syntax-rules`-wrapped variant whose wrong answer
  changed shape across P4.7 (unverified here).
- [ ] **Go `rename` cannot see a body-local macro** [Hygiene, S]: an ER macro that renames a
  `define-syntax` bound in the same body (`(define (q) (define-syntax def5 …) (define-syntax via (er
  … (r 'def5) …)) (via zz) zz)`) fails "no such binding def5" on Go and answers 5 on Scheme. The
  rename reads `DefEnv().AtPhase(…)`, which holds no body-local rows.
- [ ] **Neither syntax converter builds a `*SyntaxBox`, so a box an ER proc passes through comes back
  holding syntax** [Correctness, S]: `(define-syntax pass (er-macro-transformer (lambda (f r c) (list
  (r 'quote) (cadr f)))))`, `(pass #&(a b))` is `#&#'(#'a #'b)` on Go (master too) and, since P4.7
  hands the proc a real box on the Scheme layer, `#&(#'a #'b)` there (master: `(a b)`, the box
  dropped). Go: `DatumToSyntaxValue`'s box arm (`pkg/schemeutil/syntax.go`) builds
  `SyntaxObject(Box(syntax))`; Scheme: `datumToSyntaxVisited` (`prim_syntax.go`) has no box arm and
  treats the box as an opaque atom. Fix both to `syntax.NewSyntaxBox` (the prim_syntax arm needs the
  vector arm's `visited` check); `TestDatumToSyntaxValue/TestBox` pins the current Go shape. A
  reviewing lens's scratch patch of both turned the repro to `#&(a b)` on both layers.
- [ ] **The Scheme syntax layer overflows the Go stack on a cycle reached past its label**
  [Correctness, S]: `(m #0=(#1=(x . #0#)) #1#)`, master and branch alike. The parser's label wrapper
  fences `#0=` (and `AddScope` leaves a label unchanged), but `#1#` reaches the raw cyclic pair, and
  the use-site / intro stamp in `expandMacroInvocation` walks it with `MapSyntaxTree`
  (`scope_utils.go`), which has no cycle guard. A `fatal error`, so no `recover` contains it. The Go
  ER layer now answers it (P4.7); `TestP2_ERSpineUnlabeledCycle` is Go-only because of this.
- [ ] **`ExpandOnce` drops a non-pair cdr** [Correctness, XS]: it replaces a non-`*SyntaxPair` cdr
  with `SyntaxEmptyList` before invoking the transformer, so `(m . 5)` loses the 5 under
  `expand-once`; `expandMacroInvocation` passes `expr` through.

### Filed by the P5.1 crosscheck (2026-10-16)

- [ ] **`Namespace.ResolvePath` mishandles Windows rooted and drive-relative paths** [Portability, S,
  unverified on Windows]: it prefixes the engine directory to any path `filepath.IsAbs` rejects, and on
  Windows that includes `\foo` (rooted on the current volume) and `C:foo` (relative to drive C's own
  cwd). The joins come out as `C:\cwd\\foo` and `C:\cwd\C:foo`. The tree cross-compiles
  (`GOOS=windows go build ./...` exits 0) but no CI job runs it, so nothing here can be tested. Fix
  shape: `\foo` takes `filepath.VolumeName(dir)` + path; `C:foo` joins the remainder onto the engine
  directory when the volumes match, and needs a decision when they do not (a process per-drive cwd is
  the only other answer, and it is host state).

### Closed in Tier 1

Moved to [`closed.md`](closed.md) (2026-10-17 split).
