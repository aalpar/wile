# Coverage: track all Scheme code, not just top-level templates

**Status:** Design draft (2026-04-23). **Queued — blocked by pending algebra Tier B per `plans/WORKSPACE-ROADMAP.md`.** Follow-up to the shipped scheme-coverage system (`plans/2026-04-18-scheme-line-coverage.md`).

**Problem:** `wile.WithCoverage(col)` currently only tracks templates reachable from the top-level expression's literals pool. Code executed from imported library bodies does not appear in `col.Entries()`, even though those templates compile, execute, and carry full `SourceContext` metadata. Users asking "what Scheme code ran during this program?" get an answer that excludes every `(import ...)`-loaded library.

**Scope:** Extend template tracking to cover every template that compiles and executes during a session: library bodies, procedures defined inside libraries, extension-supplied Scheme code, and `eval`/`load` dynamic compilation. Goal stated as "track ALL Scheme code executed."

---

## Evidence

Verified with a wile-goast test run in-session. After `(import (wile goast utils))` + `(unique '(1 1 2))`:

```
boundary.scm        count0=0  count1=2
(no other entries)
```

The `(unique '(1 1 2))` call's 2 count=1 entries all attribute to `boundary.scm` (the outer source), not to `lib/wile/goast/utils.scm` where `unique` is defined. The library's template is never registered with the collector.

---

## Current architecture

`engine.go:841-864` — `trackTemplateTree(col, root)`:

```go
func trackTemplateTree(col *coverage.Collector, root *machine.NativeTemplate) {
    queue := []*machine.NativeTemplate{root}
    for len(queue) > 0 {
        tpl := queue[0]; queue = queue[1:]
        if visited[tpl] { continue }
        col.Track(tpl)
        for _, lit := range tpl.Literals() {
            if child, ok := lit.(*machine.NativeTemplate); ok {
                queue = append(queue, child)
            }
        }
    }
}
```

Called at two sites:
- `engine.go:391` — after `Engine.Eval` compiles a single expression.
- `engine.go:719` — after `Engine.evalMultiple` compiles a chunk.

Both pass `tpl` = the top-level expression's template. Library templates are not in `tpl.Literals()` because the top-level references library exports by name (runtime binding lookup) rather than as literal values.

### How libraries actually compile

`machine/compilation/library_registry.go:90-98`:

```go
type CompiledLibrary struct {
    Name        LibraryName
    Description string
    Env         *environment.EnvironmentFrame
    Exports     map[string]string
    SourceFile  string
    Template    *machine.NativeTemplate   // ← body template
}
```

Each loaded library carries its own body `Template`. This template's literals pool contains the procedure templates for every `define`'d closure inside the library (how `define` attaches compiled procedures to the body's runtime environment). Running `trackTemplateTree(col, lib.Template)` should yield all the library's reachable templates transitively — the same walk logic, a different root.

### How library loads are observed

`engine.go:315-334` already installs an import observer chain. The existing hook `makeDocRegistrationObserver` fires on every library load to register docstrings. A coverage observer can plug into the same chain.

`LibraryImportEvent` fields carry `Library` (name) — enough to `libReg.Lookup(evt.Library)` and get the `*CompiledLibrary` with its template.

---

## Gap analysis: what "all Scheme code executed" covers

Beyond the library-body gap, other execution paths compile templates that may or may not be reachable today:

| Path | Current tracking | Notes |
|------|---|---|
| Top-level `Engine.Eval` / `EvalMultiple` | ✅ Tracked | The happy path. |
| Library body on import | ❌ **Not tracked** | This plan's primary target. |
| Procedures defined inside libraries | ❌ Not tracked | Reachable from library body's literals; lights up transitively once body is tracked. |
| Macro-expansion side effects (`define-syntax` transformer bodies) | ❓ Verify | Transformer closures are MachineClosures; their templates live in the expand-time env. Possibly not tracked. |
| `eval`/`load` dynamic compilation | ❓ Verify | Compiles a new top-level template; probably tracked if the sub-context path calls `trackTemplateTree`. |
| Extension-registered Scheme (via `stdlib/lib/...`) | ❌ Not tracked | Same mechanism as user libraries. Should light up once library-body tracking ships. |
| Synthetic libraries (no `SourceFile`) | ❌ Not tracked, not wanted | No `SourceContext` attribution means nothing useful to report. |

Full "all Scheme code" coverage requires addressing at minimum rows 1-3, 5, and 7. Rows 4 and 6 are verification items — may already be tracked or may need parallel hooks.

---

## Proposed fix

### Primary: coverage import observer

Mirror `makeDocRegistrationObserver`. Add `makeCoverageObserver(libReg, col)` that, on each import event, looks up the library and walks its body template:

```go
func makeCoverageObserver(libReg *compilation.LibraryRegistry, col *coverage.Collector) func(compilation.LibraryImportEvent) {
    return func(evt compilation.LibraryImportEvent) {
        if col == nil {
            return
        }
        lib := libReg.Lookup(evt.Library)
        if lib == nil || lib.Template == nil {
            return
        }
        trackTemplateTree(col, lib.Template)
    }
}
```

Wire it into the existing observer chain in `setupLibrarySystem`:

```go
// engine.go:328 (approximate — inside setupLibrarySystem)
docObserver := makeDocRegistrationObserver(libReg, reg)
covObserver := makeCoverageObserver(libReg, cfg.coverageCollector)  // NEW
composite := func(evt compilation.LibraryImportEvent) {
    docObserver(evt)
    covObserver(evt)
    if importObserver != nil { importObserver(evt) }
}
libReg.SetImportObserver(composite)
```

`trackTemplateTree` is already the right shape — it deduplicates via its `visited` map, so the same template being reached via both "top-level literals" and "library body" is safely a no-op.

**Idempotency**: `Collector.Track` already deduplicates by pointer identity (`slices.Contains`). Observing the same library twice via import chains won't double-register.

**Ordering**: Library import fires in `PhaseExpand` and `PhaseCompile`. Both are before runtime execution, so by the time the body runs, the template is tracked and its `executed[]` array is allocated. No race.

### Secondary: extension-library registration

`registerExtensionLibraries` (`engine.go:626`) loads extension-provided `.sld` files into the library registry. These take the same `CompiledLibrary` path; the coverage observer handles them once the observer is wired. No separate work needed.

### Verification items

For rows 4 and 6 of the gap table (macro transformers, dynamic compilation), add targeted tests:

- **Macro transformers**: define a macro whose transformer body has multiple expressions; invoke it; assert entries appear attributed to the define-syntax source location. If no entries appear, transformer compilation doesn't go through `trackTemplateTree` and needs its own hook.
- **eval / load**: evaluate `(eval '(+ 1 2) (interaction-environment))`; assert entries appear. Then `(load "some-file.scm")`; assert file entries appear. If either misses, the `evalMultiple` call path inside `eval`/`load` primitives isn't reaching `trackTemplateTree`.

Expect both to already work — the sub-context compilation path goes through `expandAndCompileOptimized` (same as top-level), which is called from `Eval`/`evalMultiple`, both of which call `trackTemplateTree`. But assert, don't assume.

---

## File-prefix exclusion review

Currently `coverage/gocover.go:25-28`:

```go
var stdlibPrefixes = []string{"scheme/", "wile/", "srfi/"}
```

These prefixes exclude the embedded R7RS stdlib by default from `WriteGoCover` and `WriteSummary`. After this change, wile-goast's libraries (path `wile/goast/...`) will start producing entries — and will be silently filtered by the `wile/` prefix.

**Decision Q-a:** What's the right filter behavior?

1. **Narrow stdlib prefix** to match only what ships in `stdlib/lib/` exactly (`scheme/`, `srfi/`, and e.g. `wile/algebra/`, `wile/chibi/`, `wile/extension-example/` — enumerated, not prefix-matched). wile-goast's `wile/goast/` escapes the filter.
2. **Switch to an opt-in allowlist**: pass a set of filename prefixes to include at `WriteGoCover` call time. No default exclusion. Users of `wile` CLI default to excluding only the embedded stdlib via explicit CLI argument; library users choose their own policy.
3. **Leave filter as-is**, add `--cover-include` flag/option to force-include specific prefixes. Least disruptive.

**Recommendation: Option 2** — explicit is better than implicit. The current prefix-based default is a hidden DSL for "don't show me internal stuff"; as the library ecosystem grows, the heuristic breaks down.

---

## Implementation phases

| Phase | Change | LOC | Tests |
|---:|---|---:|---:|
| 1 | Add `makeCoverageObserver` + wire into `setupLibrarySystem` | ~15 | — |
| 2 | Engine-level integration test: `(import (scheme base))` or similar yields library entries | — | +1 |
| 3 | Verify macro transformer coverage; add hook if needed | 0-30 | +1 |
| 4 | Verify eval/load coverage; add hook if needed | 0-30 | +2 |
| 5 | Resolve Q-a (file-prefix exclusion) per user decision | ~20 | +1-2 |
| 6 | Docs update: `docs/coverage/scheme-coverage.md` — rewrite "What is covered" section to explicitly include libraries | — | — |
| **Total** | | **~35-95** | **+5-6** |

Phase 1 is the load-bearing change. Phases 3-5 depend on what the verification turns up.

---

## Test plan

### Happy path (Phase 2)

```go
func TestWithCoverage_LibraryBodyTracked(t *testing.T) {
    col := coverage.NewCollector()
    eng, _ := wile.NewEngine(ctx,
        wile.WithCoverage(col),
        // ... standard library-enabled config
    )
    // Exercise something that requires library-body code to run.
    _, err := eng.EvalMultipleWithSource(ctx,
        `(import (srfi 1)) (length (filter odd? '(1 2 3 4 5)))`,
        "test.scm")
    // ...
    // Assert that at least one entry has File starting with "srfi/" or "scheme/"
    // AND Count=1.
}
```

Requires flipping `WriteGoCoverIncludingStdlib` or re-resolving the exclusion filter per Q-a.

### Negative control (Phase 2)

```go
func TestWithCoverage_UnusedLibraryProcedureNotCovered(t *testing.T) {
    // Import srfi-1; call only `length` — don't call `filter` or `map`.
    // Assert that the library template IS tracked (entries with Count=0
    // exist for it), but specifically `filter`'s and `map`'s body entries
    // are Count=0 while `length`'s is Count=1.
}
```

### Sub-context paths (Phase 4)

```go
func TestWithCoverage_EvalPrimitiveTracked(t *testing.T) {
    // (eval '(+ 1 2) (environment '(scheme base)))
    // Assert entries appear.
}
func TestWithCoverage_LoadPrimitiveTracked(t *testing.T) {
    // Write a temp .scm file; (load "temp.scm"); assert entries.
}
```

### Macro transformer paths (Phase 3)

```go
func TestWithCoverage_DefineSyntaxTransformerTracked(t *testing.T) {
    // (define-syntax foo (syntax-rules () ((_ x) (* x 2))))
    // (foo 3)
    // Assert that an entry tied to the syntax-rules body is registered
    // (even if Count=0 when not invoked, or Count=1 after expansion).
}
```

If the macro transformer templates don't appear in the collector, Phase 3 adds a second hook. The expected location is `compile_define_syntax.go` — where the transformer closure is created.

---

## Risks

1. **Performance on large library graphs.** Walking every library's template on every import call multiplies tracked-template count by (number of libraries × procedures per library). For programs importing large library graphs (e.g., `(import (scheme base))` transitively pulls in much of the stdlib), this is a bounded one-time cost at startup. `trackTemplateTree` is O(templates × average-literals-pool-size); deduplication via `visited` keeps it linear in total template count. No expected measurable impact on the hot path.
2. **EnableCoverage allocation.** `col.Track(tpl)` calls `tpl.EnableCoverage()`, which allocates an `executed []bool` sized to `tpl.code`. For a library with 500 procedures and typical 50-instruction templates, that's ~25,000 booleans — ~25 KB of extra allocation per coverage-enabled run. Negligible.
3. **Synthetic libraries with no SourceFile.** The observer skips when `lib == nil`, which is correct. If a library has a `Template` but `SourceFile == ""`, the entries will carry empty file names; `WriteGoCover` skips them naturally. No crash path.
4. **Coverage-observer error path.** The observer has no error return. If template walking panics, it crashes the import. Mitigation: wrap the body in a recover — but test coverage tooling is developer-facing, so a crash at import time is arguably fine (points directly at the bug). Leave as-is.

---

## Decision summary

One open question:

| # | Question | Default |
|---|---|---|
| Q-a | File-prefix exclusion filter behavior | Option 2 (opt-in allowlist at write time; no default exclusion) |

Everything else is mechanical once Phase 1 lands. Phases 3-4 are conditional on verification findings.

---

## Relation to existing work

- `plans/2026-04-18-scheme-line-coverage.md`: shipped the primary coverage mechanism. This plan extends it.
- `plans/2026-04-22-wile-goast-algebra-extraction-*.md`: PR #705 (merged) made wile-goast's Scheme libraries candidates for cross-project coverage — this plan makes that coverage visible.
- `docs/coverage/scheme-coverage.md`: current user doc. Phase 6 rewrites the "What is covered" section.
