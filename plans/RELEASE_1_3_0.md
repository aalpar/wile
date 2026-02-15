# Release TODO — v1.3.0

**Branch**: (create `release/1.3.0` when ready)
**Predecessor**: v1.2.0 (2026-02-11), 200+ commits since

---

## Release Scope

v1.3.0 is a feature + hardening release. Headline items:

| Category | Summary |
|----------|---------|
| **Features** | Load-path stack, load introspection primitives, R6RS compat shim |
| **Content** | 73 examples (12 categories), 21 Gabriel benchmarks, Schelog |
| **Bug fixes** | M1–M11 (machine), T1–T5 (threads), H1–H9 (hash), plus quasiquote/string-utf8 fixes |
| **Refactoring** | Error convention (~80 sites), Tuple adoption, OperationBase embed, tokenizer consolidation, expander simplification |
| **Infra** | Benchmark targets (`bench-gabriel`, `bench-gabriel-all`, `bench-gabriel-compare`), convenience `dist/scheme` symlink |

---

## Pre-Release Checklist

### 1. Validate

- [ ] `make lint` — clean
- [ ] `make test` — all Go tests pass
- [ ] `make test-scheme` — Scheme test suite passes
- [ ] `make build-all` — all 4 platforms build (darwin/linux × arm64/amd64)
- [ ] `make bench-gabriel` — benchmarks run without error
- [ ] Spot-check examples: pick 5–10 from `examples/` and run them

### 2. CHANGELOG

- [ ] Review `CHANGELOG.md` [Unreleased] section for completeness
  - Cross-check against `git log v1.2.0..HEAD --oneline` for missed items
- [ ] Stamp date: `## [1.3.0] - 2026-MM-DD`
- [ ] Add new `## [Unreleased]` section above
- [ ] Update footer links:
  ```
  [Unreleased]: https://github.com/aalpar/wile/compare/v1.3.0...HEAD
  [1.3.0]: https://github.com/aalpar/wile/compare/v1.2.0...v1.3.0
  ```

### 3. Version Bump

- [ ] `make bump-minor` (updates `VERSION` to `v1.3.0`)
- [ ] Verify `cat VERSION` → `v1.3.0`

### 4. Documentation

- [ ] Update README.md if any install/usage instructions reference specific versions
- [ ] Verify `PRIMITIVES.md` covers any new primitives (`current-load-path`, `current-load-directory`, `current-load-depth`)
- [ ] Verify `docs/dev/R7RS_SEMANTIC_DIFFERENCES.md` reflects any new deviations or resolved items
- [ ] Update `plans/PROJECT_STATUS.md` with release snapshot
- [ ] Update `TODO.md` — mark completed items, adjust statuses

### 5. Final Commit & Tag

- [ ] Create branch `release/1.3.0`
- [ ] Commit all release prep changes
- [ ] PR → master, review, merge
- [ ] `make tag` — creates annotated tag `v1.3.0`
- [ ] `git push origin v1.3.0`

### 6. Build & Publish

- [ ] `make release` (GoReleaser → GitHub Releases) or wait for CI
- [ ] Verify GitHub release page has all 4 platform binaries
- [ ] Verify `go install github.com/aalpar/wile/cmd/scheme@v1.3.0` works

### 7. Post-Release

- [ ] Verify `pkg.go.dev` picks up the new version
- [ ] Sync `plans/CLAUDE.md` if plan statuses changed
- [ ] Clean up release branch

---

## Known Gaps (not blocking 1.3.0)

These are acknowledged gaps that exist pre-release and are acceptable to ship with:

| Gap | Why OK |
|-----|--------|
| Scheme test content minimal (1 smoke test) | Infra exists; content is P3 |
| Tokenizer `readUreal` extraction deferred | Documented tech debt, not a correctness issue |
| Numeric tower type-switch (L19) | Deferred indefinitely per architectural review |
| No network libraries | P4, not in scope |
