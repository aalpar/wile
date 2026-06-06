# Security Audit & Remediation — 2026-06-04

Record of a security review of Wile and the fixes it produced. Scope, on request:
the sandbox/authorizer model, code-level implementation review, dependency
vulnerabilities, and live Scheme-level escape testing.

All CRITICAL and HIGH findings, both reachable dependency CVEs, and the main
resource-exhaustion vector are fixed and on `master`. Two LOW items remain open
by deliberate choice (noted at the end).

## Findings at a glance

| # | Severity | Finding | Status | Landed in |
|---|----------|---------|--------|-----------|
| 0 | — | `master` did not compile (CodeQL autofix sweep) | Fixed | `63d2eaa4` |
| 1 | CRITICAL | Parser stack-overflow crashes the host | Fixed | parser branch (`29b4cba4` et al.) |
| 2 | HIGH | Symlink escape past the `/tmp` sandbox | Fixed | `4b405d0a` |
| 3 | HIGH | `eval`/`compile` run code with no authorizer gate | Fixed | `4b405d0a` |
| 4 | MED (deps) | Two reachable Go stdlib CVEs | Fixed | `5ec779c0` |
| 5 | LOW | Unbounded `make-*` allocation (OOM) | Fixed | `ec41478d` |
| 6 | LOW | TOCTOU between auth check and file open | Fixed | `8eb57e99` |
| 7 | LOW | Nil authorizer is fail-open (by design) | Open (accepted) | — |

---

## 0 — Build was broken by a CodeQL autofix sweep

`master` did not compile. PR #763 (`alert-autofix-6`) and siblings `9f34efe7` /
`9af18e99` were GitHub Copilot Autofix commits for CodeQL "Incorrect conversion
between integer types" alerts. Generated per-alert in isolation, merged together
they broke the build: a removed `math/bits` import still referenced by a sibling
patch, a `:=` against a struct field, and a `uint64` passed where `int` was
expected (`repl/debug.go`). No CI build gate caught it.

**Fix (`63d2eaa4`):** restore the import, split the `:=`, convert at the call
site. **Lesson:** treat `alert-autofix-*` branches as untrusted patches — run
`go build`/`make ci` before merging. See `memory/codeql-autofix-broke-build.md`.

## 1 — Parser stack-overflow crashes the host (CRITICAL)

The recursive-descent parser had no nesting-depth limit. Deeply nested input
(e.g. millions of `(`) overflowed the Go goroutine stack, producing a
`fatal error: stack overflow` that `recover()` cannot catch — killing the
embedding host process. Reachable from untrusted Scheme **text even under the
Console sandbox**, because parsing precedes any authorizer gate. The VM's
`DefaultMaxCallDepth` guards runtime recursion only, after parsing.

**Fix:** a depth counter at the single recursion chokepoint `readSyntax`
(`internal/parser/parser.go`), returning a catchable `werr.ErrParseDepthExceeded`
past `DefaultMaxParseDepth` (10000); `0` disables it. Configurable per engine via
`WithMaxParseDepth`. Plan: `memory/2026-06-04-parser-depth-limit-impl.md`.

**Verify:** `(` × 2,000,000 now yields a parse error and non-zero exit, no fatal
crash. Engine-level regression test `TestEngine_DeepNesting_DoesNotCrash`.

**Follow-up (open):** the expander (`machine/expander_*.go`) is also recursive.
The parser bound closes the textual-input surface; programmatically constructed
deep syntax (macro output, `datum->syntax`, quasiquote) can still overflow it.
Tracked in `TODO.md`.

## 2 — Symlink escape past the `/tmp` sandbox (HIGH)

The Console/ConsoleWithLoad/FilesystemRoot authorizers did a purely lexical
prefix check, so a symlink staged inside `/tmp` pointing outside it
(`/tmp/x -> /etc`) let evaluated Scheme read/write/delete arbitrary files.
Verified live: under Console, `(call-with-input-file "/tmp/link/passwd" ...)`
read `/etc/passwd`.

**Fix (`4b405d0a`):** shared `containedInRoot` (`security/path_containment.go`)
resolves symlinks on **both** root and target before comparison, following
symlinks on the existing ancestry while still admitting not-yet-existing paths
for file creation. Resolving the root too is required because macOS `/tmp` is a
symlink to `/private/tmp`. All three authorizers use it. See
`memory/sandbox-path-containment.md`.

## 3 — `eval`/`compile` run code with no authorizer gate (HIGH)

`PrimEval`/`PrimCompile` (`extensions/eval/prim_eval.go`) compiled and ran
arbitrary code with no `security.Check`, so no policy could control dynamic code
execution (the `code:load` restriction was bypassed for the most direct path).

**Fix (`4b405d0a`):** new `security.ActionEval`; both primitives gate on
`{ResourceCode, ActionEval}` before compilation. Posture (user-chosen):
ConsoleWithLoad **allows** `code:eval` — preserving its documented sandboxed-eval
use for wile-goast — while Console and any deny-code authorizer **refuse** it. The
side effects of evaluated code remain independently gated at their file/process/
env sinks. The fix adds the gate *mechanism*; default behavior is non-breaking.

**Verify:** `TestEngine_Eval_AllowedUnderConsoleWithLoad`,
`TestEngine_Eval_DeniedByAuthorizer`.

## 4 — Reachable Go stdlib CVEs (dependency)

`govulncheck` (run on go1.26.3) flagged two reachable stdlib vulnerabilities:
`GO-2026-5039` (`net/textproto`, via `process-spawn`) and `GO-2026-5037`
(`crypto/x509` hostname parsing). Both fixed in go1.26.4.

**Fix (`5ec779c0`):** pin the build toolchain via the `go.mod` `toolchain go1.26.4`
directive; the `go` language directive stays at 1.24.0 so consumers are not forced
to a newer language version. **Verify:** standalone (`GOWORK=off`) `govulncheck`
reports 0 vulnerabilities.

## 5 — Unbounded `make-*` allocation (LOW)

`make-vector`/`make-string`/`make-bytevector`/`make-list` guarded negative sizes
but accepted arbitrary positive counts, so untrusted Scheme could OOM the host via
`(make-vector 9999999999)`.

**Fix (`ec41478d`):** `helpers.ValidateMakeLength` (shared by all four sites,
replacing the duplicated negative-only checks) enforces a 2^32 entry ceiling. It
is a count ceiling, not a byte budget, but blocks the absurd allocations.
`make-list` now validates the `int64` count before the `int()` conversion, closing
a latent truncation.

## 6 — TOCTOU between auth check and file open (LOW)

The file primitives authorized a path string, then independently re-opened it by
name — a path component swapped in between (symlink race) could redirect the open
outside the root. **Not reachable from inside the sandbox** (no symlink-creation
primitive exists); it is defense-in-depth against an out-of-band process racing
`/tmp`.

**Fix (`8eb57e99`):** the optional `security.RootConfined` interface lets
confining authorizers expose their root (Console/ConsoleWithLoad → `/tmp`,
FilesystemRoot → its root); `security.ConfinementRootOf` discovers it (unwrapping
`All()` composites). File primitives open through **`os.Root`**
(`extensions/files/confined.go`) when confined — `os.Root` resolves components
relative to a directory descriptor and refuses symlink/`..` escape atomically,
eliminating the check-to-open window. Unconfined authorizers fall back to plain
`os.*`. Handles the macOS `/tmp` → `/private/tmp` alias. Process-global
`set-current-directory!` is unchanged.

## 7 — Nil authorizer is fail-open (LOW, accepted)

`CheckWithAuthorizer(nil, …)` returns allow, and the Tiny/Small/KitchenSink
profiles install no restrictive authorizer. This is intentional (open sandbox =
no authorizer), but a sink that loses its authorizer reference fails open rather
than closed. No current escalation path exists (`make-namespace` propagates the
authorizer; `(environment '(wile <profile>))` constructs a fresh one). Left as a
documented invariant question rather than a code change. A build/test invariant
asserting privileged sinks resolve a non-nil authorizer under restrictive
profiles would harden it if desired.

---

## Verified safe (audit coverage)

- `unsafe` package: not used in production (only in comments).
- `process-spawn`: direct `exec`, no shell, args as a slice (no injection);
  `(system)` uses `sh -c` by R7RS design, gated by `ResourceProcess`.
- Bytevector/string index & subrange conversions: bounds-checked.
- `..` traversal and `/tmpfoo`-vs-`/tmp` prefix-sibling bypass: blocked.
- Circular datum labels and circular printing: both terminate.
- Integer-conversion sites (the CodeQL trigger): no exploitable truncation/sign
  bug — the size/index conversions are guarded.

## Remaining work

- **#7** nil fail-open — accepted; optional invariant test to harden.
- **Expander depth bound** — follow-up to #1, tracked in `TODO.md`.
