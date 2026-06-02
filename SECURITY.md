# Security Policy

Wile is a Scheme interpreter and compiler designed to be embedded in Go
programs, including to run **untrusted Scheme code**. Security is part of
the product, not an afterthought. This document explains how to report
vulnerabilities and describes the security model so embedders can make
informed decisions.

## Reporting a Vulnerability

**Please report security issues privately. Do not open a public issue for
a suspected vulnerability.**

Use GitHub's private vulnerability reporting:

1. Go to the repository's **Security** tab → **Report a vulnerability**
   (Security Advisories). This opens a private channel with the maintainers.
2. If private reporting is unavailable, contact the maintainer directly
   rather than filing a public issue.

<!-- Maintainer: confirm the preferred private contact (email or GitHub
     advisory only) and enable "Private vulnerability reporting" in
     repository Settings → Security if not already on. -->

When reporting, please include:

- The affected version (`wile --version`) or commit SHA.
- A minimal Scheme program or Go embedding snippet that reproduces the issue.
- The engine configuration in use (profile, authorizer, enabled extensions).
- What you expected to be prevented and what actually happened.
- Any relevant stack trace or error output.

### What to expect

- **Acknowledgement** of your report as soon as practical.
- An assessment of severity and affected versions.
- A fix or mitigation, followed by coordinated disclosure. We prefer to
  publish a fix before public details, and will credit reporters who wish
  to be named.

Wile is an early-stage, actively developed project. Response times are
best-effort, not contractual.

## Supported Versions

Security fixes land on `master` and ship in the next release. The latest
released version is the supported version; there are no long-term support
branches at this stage.

| Version            | Supported          |
| ------------------ | ------------------ |
| Latest release     | :white_check_mark: |
| Older releases     | :x:                |

Per the project's versioning posture, APIs may still change between minor
versions. Pin a specific version if you depend on security-relevant
behavior, and review the CHANGELOG when upgrading.

## Security Model

Wile is a **language-level sandbox**, not an OS-level one. It runs Scheme
on a bytecode VM inside your Go process. Its protections are about *which
capabilities Scheme code can reach*, enforced in pure Go.

### Runtime posture

- **Pure Go.** No CGo and no `unsafe` in production code, so Wile inherits
  Go's memory safety. Scheme values are ordinary Go heap objects managed
  by the Go garbage collector.
- **No ambient host access.** A freshly constructed engine exposes only the
  extensions you enable. Dangerous capabilities — filesystem, process
  execution, environment, Go interop — live in **opt-in extensions**. If an
  extension is not loaded, its primitives do not exist for Scheme code to
  call.
- **One engine per goroutine.** An `Engine` is not safe for concurrent use
  by multiple goroutines. SRFI-18 threads *within* an engine are coordinated
  by the VM.

### Two-layer authorization

Sandboxing for embedded use has two orthogonal layers. The **effective
capability set is the intersection of the two**: an operation is possible
only if its extension is loaded *and* the authorizer allows it.

**1. Profiles** (`WithProfile`) select a bundle of extensions plus, for some
profiles, a built-in authorizer:

| Profile             | Capabilities                                                       | Built-in authorizer            |
| ------------------- | ------------------------------------------------------------------ | ------------------------------ |
| `Tiny`              | Core computation only — no I/O, filesystem, or threads             | none (open, but no risky prims) |
| `Console`           | Port I/O, file access restricted to `/tmp`, virtual env map        | `ConsoleAuthorizer`            |
| `ConsoleWithLoad`   | `Console` plus `eval`/`load` confined to `/tmp`                     | `ConsoleWithLoadAuthorizer`    |
| `Small`             | R7RS-small complete — file I/O, system interface; no threads/interop | none                         |
| `KitchenSink`       | Everything — threads, Go interop, process execution, namespaces    | none                           |

`WithSandbox` is an orthogonal modifier that wraps the authorizer with an
env-map restriction.

**2. Fine-grained authorization** (`security.Authorizer`) gates individual
privileged operations at runtime. Each gated operation issues an
`AccessRequest{Resource, Action, Target}`:

- **Resources:** `file`, `code`, `env`, `process`
- **Actions:** `read`, `write`, `delete`, `stat`, `load`, `exit`, `exec`,
  `exec-shell`
- **Target:** operation-specific — a file path, env-var name, or library name.

Gate sites include file primitives, the system/process and eval extensions,
`include`, and library import. Built-in authorizers include `DenyAll()`,
`ReadOnly()`, `ConsoleAuthorizer()`, `ConsoleWithLoadAuthorizer()`,
`SandboxAuthorizer(envPrefix)`, and `FilesystemRoot(root)`; `All(...)`
composes several into one (all must allow). Set one with `WithAuthorizer`,
or get one implicitly from a profile. Custom policies implement the
one-method `Authorizer` interface.

> **Open by default.** When no authorizer is configured, authorization
> checks **allow** the operation. A bare `NewEngine(ctx)` is safe mainly
> because it loads no risky extensions — not because anything is denied. If
> you enable I/O, process, or interop extensions, you **must** also set a
> restrictive profile or authorizer.

### Resource limits

To bound runaway or adversarial programs:

- `WithMaxCallDepth` — caps continuation/recursion depth.
- `WithMaxStackSize` — caps the evaluation stack.
- **Context cancellation and timeouts** — the VM checks `ctx` during
  execution, so `context.WithTimeout`/`WithCancel` (and the built-in timer
  interrupts) preempt long-running code.

### What the sandbox does *not* protect against

Be explicit about the boundary. The language-level sandbox does **not** by
itself defend against:

- **Resource exhaustion** beyond the limits above — e.g. large allocations
  causing memory pressure, or CPU spent inside primitives. Always run
  untrusted code with a `context` deadline and the call-depth/stack limits set.
- **Go interop escape.** If you enable the Go interop extension, Scheme code
  can reach the Go values and functions you expose. Do not expose
  capabilities you are unwilling to grant to the script.
- **OS-level isolation.** Wile does not use chroot, namespaces, or seccomp.
  The `/tmp` confinement in the Console profiles is enforced by the
  authorizer's path checks, not by the operating system.

For genuinely hostile input, combine Wile's sandbox with OS-level isolation
(containers, seccomp-bpf, a restricted user, resource cgroups) and a strict
authorizer that denies by default.

### The `wile` CLI

The standalone `wile` command-line tool is a developer utility. It runs with
full host access (all extensions, OS filesystem) and **is not sandboxed**.
The sandboxing described here is for the embedding API, not the CLI.

## Further Reading

- [`docs/security/sandboxing.md`](docs/security/sandboxing.md) — detailed
  sandboxing and authorization guide.
- [`docs/extensions/architecture.md`](docs/extensions/architecture.md) —
  extension system and capability surface.
- Package `security/` — the `Authorizer` interface and built-in authorizers.
