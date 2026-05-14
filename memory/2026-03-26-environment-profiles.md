# Environment Profiles Design

**Date:** 2026-03-26
**Status:** Draft — 0% implemented

> **Incomplete items:** All. No `Profile` type, `WithProfile`, `WithSandbox`, `WithEnv`, `WithEnvMap`, or `ConsoleAuthorizer` exist. `SafeExtensions`/`AllExtensions` still in use (21 files). See companion impl plan.

## Problem

`environment_tiny` is no longer tiny. The current extension system has two tiers
(`SafeExtensions` and `AllExtensions`) that conflate language completeness with
security posture. There is no way to create a minimal computational Scheme, a
console-capable sandbox, or a full R7RS-small environment as distinct, named
configurations — from Go or from Scheme.

## Design

Five named **profiles** define which extensions are loaded and what authorization
constraints apply. An orthogonal **sandbox** modifier restricts authority on any
profile. A **virtual environment map** provides capability-oriented configuration
without leaking host state.

### Profiles

```
Tiny  <  Console  <  ConsoleWithLoad  <  Small  <  KitchenSink
```

| Profile         | Extensions                                                        | Authorizer                                              |
|-----------------|-------------------------------------------------------------------|---------------------------------------------------------|
| Tiny            | *(core only)*                                                     | nil                                                     |
| Console         | io, files, math, envvars, all-safe                                | /tmp-only file, deny code/process                       |
| ConsoleWithLoad | io, files, math, envvars, all-safe, eval                          | /tmp-only file, /tmp-only code load, deny process       |
| Small           | io, files, math, introspection, eval, envvars, all, system        | nil                                                     |
| KitchenSink     | io, files, math, introspection, eval, namespace, threads, gointerop, envvars, all, system, process | nil                            |

Tiny is the LCD of all profiles — every profile is a superset of Tiny.

`ConsoleWithLoad` codifies the embedder composition pattern (Console plus
`eval.Extension`) for callers that want sandboxed `(eval ...)` and `(load ...)`.
It is not a strict capability ladder above Console — it widens authority to
include `code:load` under `/tmp` — but its security envelope is still bounded
to `/tmp`. wile-goast and similar embedders are the primary consumers.

### Profile Type

```go
// profile.go (new file)

type Profile int

const (
    Tiny            Profile = iota // core only, pure computation
    Console                        // core + I/O + /tmp sandbox
    ConsoleWithLoad                // Console + eval/load, sandboxed to /tmp
    Small                          // R7RS-small complete
    KitchenSink                    // every extension
)

func WithProfile(p Profile) EngineOption { ... }
```

### Sandbox

Sandbox is an orthogonal security modifier, not an extension filter. Language
features exist but authority is gated at runtime. Most-restrictive-wins when
composed with a profile's built-in authorizer.

```go
// sandbox.go (new file)

func WithSandbox(opts ...SandboxOption) EngineOption { ... }

func SandboxEnvPrefix(prefix string) SandboxOption { ... }
```

Default sandbox behavior:

| Resource  | Action   | Decision                                          |
|-----------|----------|---------------------------------------------------|
| `file`    | `read`   | Allow                                             |
| `file`    | `write`  | Deny                                              |
| `file`    | `delete` | Deny                                              |
| `env`     | `read`   | Allow if prefix match (default `WILE_`), else deny |
| `code`    | `load`   | Deny                                              |
| `process` | `*`      | Deny                                              |

### Console Authorizer

Console bakes in its own authorizer as part of the profile definition:

| Resource  | Action   | Decision                                      |
|-----------|----------|-----------------------------------------------|
| `file`    | `read`   | Allow if path under `/tmp`, deny otherwise    |
| `file`    | `write`  | Allow if path under `/tmp`, deny otherwise    |
| `file`    | `delete` | Allow if path under `/tmp`, deny otherwise    |
| `env`     | `read`   | Allow (virtual map only, no OS fallthrough)   |
| `code`    | `load`   | Deny                                          |
| `process` | `*`      | Deny                                          |

Standard ports (stdin/stdout/stderr) bypass the authorizer — they are pre-created
`Parameter` objects in the `io` extension, not opened via file primitives.

### ConsoleWithLoad Authorizer

ConsoleWithLoad mirrors Console for files and env, but allows `code:load`
within the same `/tmp` envelope. Loading code outside `/tmp` is denied — the
`/tmp` boundary is the entire security envelope.

| Resource  | Action   | Decision                                      |
|-----------|----------|-----------------------------------------------|
| `file`    | `read`   | Allow if path under `/tmp`, deny otherwise    |
| `file`    | `write`  | Allow if path under `/tmp`, deny otherwise    |
| `file`    | `delete` | Allow if path under `/tmp`, deny otherwise    |
| `env`     | `read`   | Allow (virtual map only, no OS fallthrough)   |
| `code`    | `load`   | Allow if path under `/tmp`, deny otherwise    |
| `process` | `*`      | Deny                                          |

This is the security model wile-goast and similar embedders need: an engine
that can `(eval ...)` arbitrary expressions and `(load ...)` Scheme files
the embedder has staged into `/tmp`, without granting access to the broader
filesystem or process execution.

### Virtual Environment Map

Environment variables are ambient authority. For sandboxed profiles, the Go
embedder grants specific configuration via a virtual map instead of exposing
`os.Getenv`:

```go
func WithEnv(key, value string) EngineOption { ... }
func WithEnvMap(m map[string]string) EngineOption { ... }
```

Properties:
- **Engine-level:** the map belongs to the engine, not individual environments
- **Immutable after creation:** set once at `NewEngine`, never modified
- **Inherited:** all child environments (including those created from Scheme via
  `(environment ...)`) see the same map
- **Console reads from virtual map only.** No OS fallthrough.
- **Small/KitchenSink fall through to `os.Getenv`** when no virtual map entry
  matches, subject to authorizer.

### Environment Variables Extension

Split from `system` into a new `internal/extensions/envvars/` package:

**Moved primitives:**
- `get-environment-variable` — reads virtual map or `os.Getenv`
- `get-environment-variables` — returns alist of all visible env vars

**Remains in `system`:**
- `command-line`, `exit`, `emergency-exit`, `features`
- `current-second`, `current-jiffy`, `jiffies-per-second`

The envvars primitive checks engine config: if a virtual map is set, read from
it; otherwise fall through to `os.Getenv` (subject to authorizer).

### Scheme-Level API

Extend `(environment ...)` to recognize profile specifiers:

```scheme
(environment '(wile tiny))                ; pure computation
(environment '(wile console))             ; I/O + /tmp sandbox
(environment '(wile console-with-load))   ; Console + sandboxed eval/load
(environment '(wile small))               ; R7RS-small
(environment '(wile kitchen-sink))        ; everything
```

These construct a new environment with the profile's extension set. The new
environment inherits the engine's virtual env map. Usable with `eval`:

```scheme
(eval '(+ 1 2) (environment '(wile tiny)))          ; => 3
(eval '(display "hi") (environment '(wile tiny)))   ; => error: unbound
```

`PrimEnvironment` recognizes `(wile ...)` as profile constructors — instead of
importing a library, they configure the new environment with the profile's
extension set.

## API Changes

### New Public API

| Symbol | Location | Purpose |
|--------|----------|---------|
| `Profile` | `profile.go` | Named environment configuration type |
| `Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink` | `profile.go` | Profile constants |
| `WithProfile(Profile)` | `profile.go` | Engine option |
| `WithSandbox(...SandboxOption)` | `sandbox.go` | Orthogonal security modifier |
| `SandboxOption` | `sandbox.go` | Sandbox configuration type |
| `SandboxEnvPrefix(string)` | `sandbox.go` | Configurable env var prefix |
| `WithEnv(key, value string)` | `options.go` | Add single virtual env var |
| `WithEnvMap(map[string]string)` | `options.go` | Set virtual env map |

### Deleted Public API

| Symbol | Replacement |
|--------|-------------|
| `SafeExtensions()` | `WithProfile(Console)` or `WithProfile(Small)` + `WithSandbox()` |
| `WithSafeExtensions()` | `WithProfile(Console)` or `WithProfile(Small)` + `WithSandbox()` |
| `AllExtensions()` | `WithProfile(KitchenSink)` |
| `WithAllExtensions()` | `WithProfile(KitchenSink)` |

### Retained Public API

| Symbol | Notes |
|--------|-------|
| `WithExtension(registry.Extension)` | Custom composition, adds to profile |
| `WithExtensions(...registry.Extension)` | Custom composition, adds to profile |
| `WithAuthorizer(security.Authorizer)` | Overrides profile authorizer |

## File Changes

### New Files

| File | Purpose |
|------|---------|
| `profile.go` | `Profile` type, constants, `WithProfile()`, profile-to-extensions mapping |
| `sandbox.go` | `WithSandbox()`, `SandboxOption`, `SandboxEnvPrefix()` |
| `security/console_authorizer.go` | Console /tmp + virtual-env-only authorizer |
| `security/console_with_load_authorizer.go` | ConsoleWithLoad /tmp file + /tmp code-load authorizer |
| `security/sandbox_authorizer.go` | Configurable sandbox authorizer |
| `internal/extensions/envvars/` | `get-environment-variable`, `get-environment-variables` |

### Modified Files

| File | Change |
|------|--------|
| `options.go` | Delete `SafeExtensions`, `WithSafeExtensions`, `AllExtensions`, `WithAllExtensions`. Add `WithEnv`, `WithEnvMap`. |
| `engine.go` | `engineConfig` gains `envMap`, `profile` fields. Wire virtual env map. |
| `extensions/system/` | Remove `get-environment-variable`, `get-environment-variables` |
| `internal/extensions/eval/prim_eval.go` | Recognize `(wile tiny/console/small/kitchen-sink)` in `PrimEnvironment` |
| `internal/bootstrap/environment_tiny.go` | Rename; update to work with profile-based extension selection |
| `cmd/wile/main.go` | `WithAllExtensions()` -> `WithProfile(wile.KitchenSink)` |
| All tests referencing `SafeExtensions`/`AllExtensions` | Update to `WithProfile` |

## Composition Rules

- `WithProfile` sets baseline extensions + authorizer
- `WithExtension` adds to whatever the profile provides
- `WithAuthorizer` overrides the profile's authorizer
- `WithSandbox` is a specific `WithAuthorizer` that restricts ambient authority
- Multiple authorizers (profile + sandbox): most-restrictive-wins
- No profile specified = no extensions, no authorizer (bare engine)

## Usage Examples

```go
// Tiny: pure computation
eng, _ := wile.NewEngine(ctx, wile.WithProfile(wile.Tiny))

// Console: I/O + /tmp sandbox + configuration
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Console),
    wile.WithEnv("APP_MODE", "production"),
)

// ConsoleWithLoad: Console + sandboxed eval/load (wile-goast pattern)
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.ConsoleWithLoad),
    wile.WithEnv("APP_MODE", "production"),
)

// Small: full R7RS-small
eng, _ := wile.NewEngine(ctx, wile.WithProfile(wile.Small))

// KitchenSink: everything (CLI uses this)
eng, _ := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))

// Small + sandbox with custom env prefix
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Small),
    wile.WithSandbox(wile.SandboxEnvPrefix("MYAPP_")),
)

// Custom: profile + extra extension
eng, _ := wile.NewEngine(ctx,
    wile.WithProfile(wile.Console),
    wile.WithExtension(threads.Extension),
)
```

## Design Decisions

1. **Profiles are extension sets + optional authorizer** — simple mapping, no new abstractions beyond enum + lookup
2. **Sandbox is orthogonal** — security posture is independent of language completeness
3. **Most-restrictive-wins** — composing authorizers takes the intersection, easy to reason about
4. **Virtual env map** — capability-oriented configuration, not ambient `os.Getenv`
5. **Env map is engine-level, immutable, inherited** — all child environments share it
6. **`environment_tiny` renamed** — historical name no longer accurate
7. **envvars split from system** — environment variables are configuration, not system interface
8. **v1.x, zero consumers** — delete old API outright, no deprecation ceremony
9. **`ConsoleWithLoad` is a baked profile, not just a composition pattern** — wile-goast
   needs `Console + eval.Extension` with an authorizer that allows `code:load` under
   `/tmp`. The authorizer change (not the extension list) is what makes this distinct;
   `WithProfile(Console) + WithExtension(eval.Extension)` would still have `code:load`
   denied by `ConsoleAuthorizer`. Encoding the (extensions, authorizer) pair as one
   profile keeps the security envelope coherent and discoverable.
