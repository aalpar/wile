# .github/ — GitHub Workflow, Releases, Changelog

## GitHub Workflow

### Creating Branches and Pull Requests

**All changes must go through branches and PRs.** Never commit directly to master, even for small fixes. This ensures all changes are reviewed and tracked.

When making changes, follow this workflow:

1. **Create a feature branch** with a descriptive name:
   ```bash
   git checkout -b feature/<descriptive-name>
   # Examples:
   #   feature/schelog-example-and-runtime-api
   #   fix/continuation-escape-handling
   #   refactor/macro-expander
   ```

2. **Run lint before staging** to catch issues early:
   ```bash
   make lint  # Must pass with 0 issues before committing
   ```

3. **Stage only related files** (don't mix unrelated changes):
   ```bash
   git add <specific-files>
   git diff --cached --stat  # Verify what's staged
   ```

4. **Commit with a conventional commit message**:
   ```bash
   git commit -m "$(cat <<'EOF'
   feat: short description of the change

   Longer explanation of what changed and why.
   Use bullet points for multiple items:
   - First change
   - Second change
   EOF
   )"
   ```

   Commit prefixes:
   - `feat:` - New feature
   - `fix:` - Bug fix
   - `refactor:` - Code restructuring without behavior change
   - `docs:` - Documentation only
   - `test:` - Adding or fixing tests
   - `chore:` - Maintenance tasks

5. **Push the branch**:
   ```bash
   git push -u origin feature/<descriptive-name>
   ```

6. **Create a PR with description**:
   ```bash
   gh pr create --title "feat: short description" --body "$(cat <<'EOF'
   ## Summary
   What this PR does.

   ## Changes
   - Change 1
   - Change 2

   ## Test Plan
   - [ ] Tests pass
   - [ ] Manual verification
   EOF
   )"
   ```

7. **Assign and label the PR** (optional):
   ```bash
   gh pr edit <PR-number> --add-assignee <username>
   gh pr edit <PR-number> --add-label "enhancement,documentation"
   ```

8. **Wait for CI to pass** before merging:
   ```bash
   gh pr checks <PR-number> --watch  # Watch CI status
   ```
   **IMPORTANT**: Do not merge until all CI checks are green. This prevents broken builds on master.

### Pull Request Merging

The repository owner is an administrator. When merging PRs to master, use the `--admin` flag to bypass branch protection rules if necessary:

```bash
gh pr merge <PR> --merge --admin --delete-branch
```

**CI Gate**: Even with admin privileges, always wait for CI automation to pass before merging. The `--admin` flag bypasses branch protection (e.g., required approvals), but should not be used to bypass failing CI. A PR with failing CI should be fixed, not force-merged.

Note: GitHub does not allow self-approval of PRs even with admin privileges, but admin merge ensures PRs can be merged without external approval.

### Automated Code Review

**GitHub Copilot Pull Request Reviewer is excellent.** It catches technical errors that matter:
- Factual inaccuracies in documentation (wrong output examples, incorrect claims)
- Mismatches between code behavior and documentation
- Command-line errors (wrong binary names, incorrect flags)
- Pattern mismatches (test discovery rules, file naming conventions)

**Trust Copilot's technical feedback.** When it points out specific issues with suggestions, those are typically accurate and actionable. Fix them before merging.

**Greptile** provides useful PR summaries and numerical verification but less deep technical review. Use it for sanity checks on claims about file counts, scope, and risk assessment.

## Release Process

Releases are automated via GoReleaser v2 (`.goreleaser.yml`) and a GitHub Actions workflow (`.github/workflows/release.yml`). Pushing a `v*` tag to the remote triggers the full release pipeline.

### Artifacts

Each release produces:

| Artifact | Contents |
|----------|----------|
| `wile-v{X.Y.Z}-{os}-{arch}.tar.gz` | `wile` binary + LICENSE + README.md |
| `checksums.txt` | SHA256 checksums for all archives |

Platforms: darwin/linux x amd64/arm64 (4 archives total).

### How It Works

1. The `release.yml` workflow triggers on `v*` tag pushes
2. GoReleaser builds the `wile` binary from `./cmd/wile` for all 4 platform combinations with `CGO_ENABLED=0`
3. Binaries are stamped via ldflags: `-X main.BuildSHA={{ .ShortCommit }} -X main.BuildVersion={{ .Tag }}`
4. Archives and checksums are uploaded as a GitHub release
5. GitHub generates the release changelog from commit messages (`changelog.use: github-native`)
6. No manual secrets needed — `GITHUB_TOKEN` is provided automatically by GitHub Actions

### Cutting a Release

1. Ensure all changes are on master and `[Unreleased]` in `CHANGELOG.md` is up to date
2. Rename `[Unreleased]` to `[X.Y.Z] - YYYY-MM-DD` in `CHANGELOG.md`
3. Add a fresh empty `[Unreleased]` section above it
4. Update `VERSION` to `vX.Y.Z`
5. Commit: `chore: set X.Y.Z release date in changelog`
6. Tag and push:
   ```bash
   git tag vX.Y.Z
   git push origin master vX.Y.Z
   ```
7. The release workflow runs automatically — verify at `https://github.com/aalpar/wile/releases`

### Local Verification

Before tagging, validate the GoReleaser config and do a dry run:

```bash
make release-check      # Validates .goreleaser.yml syntax
make release-snapshot   # Builds all archives locally without publishing
ls dist/                # Inspect output: 4 tar.gz files + checksums.txt
```

### Key Files

| File | Purpose |
|------|---------|
| `.goreleaser.yml` | GoReleaser v2 config: builds, archives, checksums, release notes |
| `.github/workflows/release.yml` | GitHub Actions workflow triggered on `v*` tag pushes |
| `VERSION` | Current version string (e.g., `v1.2.0`) |
| `CHANGELOG.md` | Hand-written release notes following Keep a Changelog format |

## Changelog

Wile maintains a hand-written `CHANGELOG.md` following [Keep a Changelog](https://keepachangelog.com/) format.

### Categories

Use exactly these section headers, in this order:

| Section | Use for |
|---------|---------|
| **Added** | New features, new primitives, new API surface |
| **Changed** | Changes to existing behavior or API |
| **Deprecated** | Features marked for future removal |
| **Removed** | Features or API that were deleted |
| **Fixed** | Bug fixes |
| **Security** | Vulnerability fixes |

Omit empty sections — only include categories that have entries for a given release.

### Writing Entries

- **Write for users, not developers.** "Fixed crash when reading empty port" not "Changed io.EOF handling in prim_read_write.go."
- **One bullet per user-visible change.** Internal refactors don't get entries unless they change observable behavior.
- **Start with a verb.** "Add", "Fix", "Remove", "Change" — not "Added support for..." (the section header already provides tense).
- **No commit hashes or PR numbers in entries.** The version link at the bottom points to the full diff.
- **Group related changes into a single entry** when they form a coherent feature (e.g., one entry for "Full numeric tower with exact/inexact distinction" rather than separate entries for Integer, Float, Rational, Complex).

### When to Update

**Every commit that changes user-visible behavior must include a corresponding `CHANGELOG.md` entry under `[Unreleased]`.** This includes new features, bug fixes, changed behavior, removed functionality, and dependency changes (like minimum Go version). Internal refactors that don't affect users don't need entries.

Update the changelog in a separate follow-up commit to the described change.
