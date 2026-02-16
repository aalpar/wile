# External Extensions Plan

**Status:** Phase 4 complete (2026-02-14). Remaining: Phase 5 (future), Tier 2 (deferred).

## Problem

Extensions live in `internal/extensions/`, blocking external repos from linking. External extensions can't be `go get`-installed.

## Completed (Phases 1-4)

- **Phase 1:** Moved `BoolToBoolean` et al. from `internal/schemeutil` to `values/` — unblocked 7/9 extensions
- **Phase 2:** Refactored `io` port param registration via `Registry.AddGlobalValue` — eliminated `ApplyContext.Environment()`
- **Phase 3:** Removed `ApplyContext` entirely — clean API boundary
- **Phase 4:** Moved 6 Tier 1 extensions from `internal/extensions/` to `extensions/` (system, math, gointerop, exceptions, files, threads)

6 of 9 extensions are now importable by external Go code.

## Extractability Tiers

```
Tier 1 — Extractable (DONE):
  system, math, gointerop, exceptions, files, threads
  → Now in extensions/ (public, importable)

Tier 2 — Requires further design:
  io          parser, tokenizer, syntax imports
  all         environment frame for closures

Tier 3 — Stays in-tree permanently:
  eval        full compiler pipeline coupling
```

## Phase 5 (Future): Extract to Separate Repos

Once the API boundary is proven, individual extensions can migrate to separate repos. This is a distribution concern — Phase 4 already achieves the importability goal.

| In-tree (`extensions/`) | Future external repo |
|-------------------------|---------------------|
| `extensions/system` | `github.com/aalpar/wile-system` |
| `extensions/math` | `github.com/aalpar/wile-math` |
| `extensions/gointerop` | `github.com/aalpar/wile-gointerop` |
| `extensions/exceptions` | `github.com/aalpar/wile-exceptions` |
| `extensions/files` | `github.com/aalpar/wile-files` |
| `extensions/threads` | `github.com/aalpar/wile-threads` |

## Tier 2 Extensions (Deferred)

- **`io`**: Needs `internal/parser`, `internal/tokenizer`, `internal/syntax` for `read`/`read-syntax`/`read-token`. Extracting `io` requires either making the parser public or splitting `io` into extractable port primitives vs. in-tree read primitives.
- **`all`**: Needs environment frame for record closures via `machine.NewForeignClosure`. The environment package is already public, but the usage pattern ties closures to `EnvironmentFrame` internals.

Both deferred until Tier 1 extraction proves the pattern.

### `eval` (Stays In-Tree Permanently)

`eval` imports the full compiler pipeline: parser, syntax, machine expander, machine compiler, environment. It IS the compiler exposed to Scheme. Extracting it would require making half of `machine/`'s internals public. Cost-benefit is clearly negative.
