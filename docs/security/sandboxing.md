# Sandboxing

Wile's extension system provides capability-based sandboxing for embedded Scheme engines (Rees, "A Security Kernel Based on the Lambda Calculus", 1996). Primitives not in the engine's registry don't exist — attempts to use them produce compile-time errors, not runtime checks that could be bypassed.

Sandboxing has **two layers**. The registry layer decides which primitives are *nameable*; it is the mechanism described above. The authorization layer decides what a nameable primitive may *do*: every privileged operation calls `security.CheckWithAuthorizer` with a resource/action/target triple, which a `security.Authorizer` (installed by `WithAuthorizer` or by a profile) allows or denies. The first layer removes a capability; the second bounds one you chose to keep.

## How it works

By default, `NewEngine(ctx)` includes only core primitives (arithmetic, pairs, lists, vectors, strings, characters, bytevectors, control flow, syntax, parameters). Extensions are opt-in via `WithExtension()`. If the filesystem extension isn't loaded, `open-input-file` is an unbound variable — the binding doesn't exist in the environment at all.

This restriction is **transitive**: when the library system is enabled (`WithLibraryPaths`), library environments are created by a factory that closes over the engine's registry (`Engine.applyBaseEnvironment`, wired via `Namespace.SetLibraryEnvFactory` in `pkg/wile/engine.go`). A library loaded from a `.sld` file gets the same set of primitives as the engine that loaded it, and inherits the engine's authorizer (Hardy, "The Confused Deputy", 1988). One construct escapes the registry half of that statement: see [Profile namespaces widen the surface](#profile-namespaces-widen-the-surface).

## Extension security classification

| Category | Extensions | Package | Risk |
|----------|-----------|---------|------|
| **Safe** | core | `registry/core` | None. Pure computation. |
| **Safe** | io | `pkg/extensions/io` | Host stdio, gated. `current-{input,output,error}-port` are opened over the process's `stdin`/`stdout`/`stderr` at engine construction, each gated by `stream:{read,write}`; a refusal binds a closed in-memory port instead. Everything else is in-memory or caller-provided ports. No filesystem access. |
| **Safe** | math | `extensions/math` | None. `sqrt`, `sin`, `cos`, transcendental functions. |
| **Safe** | introspection | `extensions/introspection` | None on its own. `environment?`, `interaction-environment`, `environment-bound-names`, `environment-ref`, `environment-bound?`, `features`, `available-libraries`. Read-only: it observes an environment, it cannot add bindings to one. Note `environment-ref` returns the *value* of a binding, so any environment object handed to it yields the capabilities that environment holds. |
| **Safe** | charsets | `extensions/charsets` | None. SRFI-14 character sets. |
| **Safe** | sat | `extensions/sat` | None beyond CPU/memory. CDCL SAT solver: pure computation on caller-supplied clauses. |
| **Safe** | algebragraph | `extensions/algebragraph` | None. Graph analytics backing `(wile algebra …)`. |
| **Safe** | all (safe subset) | `pkg/internal/extensions/all` | None. Records, promises, additional string/character ops. |
| **Privileged** | files | `extensions/files` | Filesystem: `open-input-file`, `open-output-file`, `delete-file`, `file-exists?`, `create-directory`, `delete-directory`, `directory-files`, `current-directory`, `set-current-directory!`. |
| **Privileged** | eval | `extensions/eval` | Evaluation / compilation: `eval`, `load`, `environment`, `expand`, `compile`, `syntax-local-value`, `syntax-local-introduce`, `syntax-local-identifier-as-binding`. |
| **Privileged** | envvars | `pkg/internal/extensions/envvars` | Environment variables: `get-environment-variable`, `get-environment-variables`. `Console`/`ConsoleWithLoad` allocate an empty virtual map (no OS fallthrough); `Small`/`KitchenSink` fall through to `os.Getenv` when the envMap is unset. |
| **Privileged** | system | `extensions/system` | Process lifecycle: `exit`, `emergency-exit`, `command-line`, `current-second`, `current-jiffy`, `jiffies-per-second`. Gated: `exit`/`emergency-exit` as `process:exit`, `command-line` as `process:read`; the clock primitives are ungated. |
| **Privileged** | process | `extensions/process` | Process execution: `system`, `process-spawn`, `process-wait`, `process-kill`. |
| **Privileged** | namespace | `pkg/internal/extensions/namespace` | Namespace introspection: `namespace?`, `make-namespace`, `namespace-derive`, `namespace-define!`, `namespace-ref`, `namespace-bound?`, `namespace-bound-names`, `namespace-require`. Not gated by any authorizer; exclude it from the registry rather than relying on a policy. |
| **Context-dependent** | gointerop | `extensions/gointerop` | Go concurrency primitives: atomic boxes. Resource exhaustion via unbounded object creation. No ambient authority. Not gated by any authorizer. Safe for trusted code. |
| **Context-dependent** | threads | `extensions/threads` | SRFI-18 threads, mutexes, condition variables. Resource exhaustion via unbounded thread creation. Not gated by any authorizer. Safe for trusted code. |

**Safe** means no ambient authority (Dennis & Van Horn 1966; Miller, "Robust Composition", 2006) — no way to affect the host system. **Privileged** means the extension grants capabilities that untrusted code should not have. **Context-dependent** means the risk depends on the trust level of the code being executed.

## API

### Named profiles (recommended)

The primary API is `WithProfile`, which bundles an extension set with a matching authorizer:

| Profile | Extensions | Authorizer |
|---------|-----------|------------|
| `Tiny` | core only | none |
| `Console` | core + io + files + math + all-safe + charsets + envvars | `ConsoleAuthorizer` (file ops restricted to `/tmp`, env reads allowed, all `code` and `process` denied) |
| `ConsoleWithLoad` | Console set + eval | `ConsoleWithLoadAuthorizer` (Console + `code:load` under `/tmp` + unrestricted `code:eval`) |
| `Small` | R7RS-small baseline (io, files, math, introspection, eval, all, charsets, system, envvars) | none |
| `KitchenSink` | every extension | none |

The mapping is defined once, in `bootstrap.ProfileExtensions`; `Profile.extensions` and the Scheme-level constructor both dispatch through it.

```go
engine, err := wile.NewEngine(ctx, wile.WithProfile(wile.Console))
```

Use `Console` for untrusted code that needs basic I/O without filesystem escape. Use `ConsoleWithLoad` when you also need `(load ...)` from a `/tmp`-staged source. Use `KitchenSink` to match the CLI's full surface. `Small` and `KitchenSink` install **no** authorizer, so on those profiles every gate site is open by default.

`WithSandbox()` is an orthogonal modifier, not a profile: it intersects `security.SandboxAuthorizer` (file reads and stats only, env reads filtered by prefix, default `WILE_`, all code and process denied) on top of whatever authorizer the profile or `WithAuthorizer` resolved to. Intersection is most-restrictive-wins, so a sandbox layer can only tighten.

### Authorization vocabulary

An `AccessRequest` is a resource, an action, and an operation-specific target (`pkg/security/access.go`).

| Resource | Actions used at gate sites | Target |
|----------|---------------------------|--------|
| `file` | `read`, `write`, `exec`, `stat`, `delete` | the path |
| `code` | `load` (run a resolved file), `eval` (compile+run an in-memory datum) | the resolved path, or `<eval>`/`<compile>` |
| `env` | `read` | the variable name, or `*` for a whole-map read |
| `process` | `read` (argv), `exit`, `exec`, `exec-shell` | the command, or empty |
| `namespace` | `create` | the profile name |
| `stream` | `read` (stdin), `write` (stdout, stderr) | `stdin`, `stdout`, or `stderr` |

Both sets are open: an extension may define additional resources and actions without changing `pkg/security`.

#### `file` is the chmod triple, and the three bits must be enforced together

`read` / `write` / `exec` are one resource with one target, so a single containment predicate decides all three. Enforce them together: an authorizer that confines reads and writes but waves `exec` through confines **nothing**, because an executable outside the root is a general-purpose unconfined file accessor. That is not hypothetical — it is how `(system …)` escaped a `FilesystemRoot("/tmp")` engine before 2026-08-21.

**A primitive whose argument denotes a host path files it under `file`, whatever else it also asks.** `process:exec` is a *capability* question ("may this program spawn a subprocess at all"); `file:exec` is an *object* question ("may it run **this** binary, and — POSIX x on a directory being traverse — may a child **start** in this directory"). A spawn asks both, so a path-confining authorizer sees the binary and the working directory it would otherwise never be shown. `set-current-directory!` follows the same rule for the same reason: it files `file:write` on the destination rather than an opaque `{process, write, "cwd"}` request that no containment authorizer would inspect.

Two consequences worth stating outright:

- **The gated binary is the *resolved* path, not the string the program wrote.** `exec.Command` LookPath-resolves a bare name, and a relative path is resolved against the child's working directory — so gating the caller's string would authorize one file and run another.
- **A spawned child starts at the confinement root** when the authorizer reports one (`security.ConfinementRootOf`). Otherwise it would inherit the *host's* working directory, which the policy never saw and could not have allowed, and every relative path the child opened would resolve there.

### Gate sites

Every enforcement point calls `security.CheckWithAuthorizer(auth, req)`. `security.Check(ctx, req)` is deprecated: the authorizer lives on the `Namespace`, not on the context, so `Check` finds nil (open) unless a caller injected one explicitly.

| Site | Request |
|------|---------|
| `extensions/files`: `openFilePort`, `callWithFile`, `PrimFileExistsQ`, `PrimDeleteFile`, `PrimCreateDirectory`/`PrimDeleteDirectory`/`PrimDirectoryFiles`/`PrimCurrentDirectory`/`PrimSetCurrentDirectory`, plus `unconfinedTarget` in `confined.go` re-gating a resolved real path | `file:{read,write,delete,stat}` on the path |
| `extensions/eval`: `PrimEval`, `PrimCompile`, `PrimExpand`, `PrimExpandOnce` | `code:eval` |
| `extensions/system`: `PrimCommandLine`, `PrimExit`/`PrimEmergencyExit` | `process:read`, `process:exit` |
| `extensions/process`: `PrimSystem`, `PrimProcessSpawn` (`PrimProcessWait`/`PrimProcessKill` are ungated: they act on a process handle already obtained through a gated spawn) | `process:exec-shell`, `process:exec`, then `file:exec` twice — on the resolved binary (`/bin/sh` for `system`) and on the child's start directory |
| `pkg/internal/extensions/envvars`: `PrimGetEnvironmentVariable`, `PrimGetEnvironmentVariables` | `env:read` |
| Source loading (`include`, `include-ci`, `load`, library `import`): `resolver.openAuthorized`, `isAuthorized`, `openUnconfined`, `FSFileResolver.ResolveAndOpen`, `OSFileResolver.ResolveAndOpen` | `code:load` on the resolved path |
| `pkg/extensions/io`: `NewState`, once per engine when the port parameters are built | `stream:read` on `stdin`, `stream:write` on `stdout` and `stderr` |

`EmbedFileResolver` performs no check: it serves the compiled-in bootstrap sources, which are not attacker-controlled.

In a chain (`WithSourceFS` + `WithSourceOS`), a resolver's refusal is not the chain's answer: the search continues when some *later* resolver authorizes under a *different* source, because refusing a virtual path says nothing about the host file the OS resolver holds. It scans forward for that resolver, skipping members that authorize under the same source — they would re-ask the question the refusal already answered, so they decide nothing, but neither do they hide the members behind them (two `WithSourceFS` layers ahead of `WithSourceOS()` is the ordinary embedder shape). The scan stops at any resolver that authorizes nothing (`EmbedFileResolver`), which would otherwise hand out its copy of the refused file, and the chain still reports a denial when every source refuses. The declaration is `resolver.SourceGate`.

Note that the resolver gate keys on the resolved *target string*. Under `WithSourceFS`, that string is a virtual path meaningful only to the supplied `fs.FS`, so the request carries `TargetSource: security.SourceVirtualFS` to say so. `FilesystemRoot` and `ConsoleWithLoadAuthorizer` refuse such a target outright, on the source rather than by containment — a virtual path handed to `containedInRoot` is resolved against the *process working directory*, which made the verdict depend on where the host happened to be running. To serve an `fs.FS` under one of those policies, use the opt-in variants `FilesystemRootWithVirtualSources(root)` or `ConsoleWithLoadAllowingVirtualSources()`, which apply **no** path confinement to virtual targets: the `fs.FS` itself is then the boundary. `ConsoleAuthorizer` needs no variant — it denies the whole `code` resource already.

### Profile with library support

```go
engine, err := wile.NewEngine(ctx,
    wile.WithProfile(wile.ConsoleWithLoad),
    wile.WithLibraryPaths("/tmp/stagedlib"),
)
```

Libraries loaded from `/tmp/stagedlib` inherit the profile's restrictions transitively. A library that tries to call `open-input-file` outside `/tmp` is denied at runtime by the authorizer; primitives absent from the profile are unbound and fail at compile time.

The profile matters here. `Console` denies the whole `code` resource, so under `Console` a file-backed `import` or `include` is refused before the file is opened, whatever path it names. Pair `WithLibraryPaths` with `ConsoleWithLoad` (which permits `code:load` under `/tmp`) or with a custom authorizer that admits your library root.

### Profile + extra extensions

`WithProfile` composes with subsequent `WithExtension` calls. Authorizer resolution is order-independent (`engineConfig.resolveAuthorizer`): an explicit `WithAuthorizer` always overrides the profile's built-in one, even `WithAuthorizer(nil)`, which opens the engine fully; any `WithSandbox` layer is then intersected on top. Across several `WithProfile` calls, the last profile that *defines* an authorizer wins, and a later authorizer-less profile does not clear an earlier one.

```go
engine, err := wile.NewEngine(ctx,
    wile.WithProfile(wile.ConsoleWithLoad),
    wile.WithExtension(threads.Extension),
    wile.WithLibraryPaths("/tmp/stagedlib"),
)
```

### Profile namespaces widen the surface

When the eval extension is present, Scheme code can construct a namespace for any *named* profile:

```scheme
(eval '(+ 1 2) (environment '(wile tiny)))
```

`PrimEnvironment` routes a sole `(wile <name>)` spec through `tryWileProfile` to `bootstrap.NewProfileEnvironment`, which builds a child namespace registered with that profile's full extension set.

**Widening is refused when an authorizer is installed.** `bootstrap.checkProfileWidening` compares the requested profile's primitive names against the engine's own registry, and decides in three cases:

| Engine | Request | Outcome |
|--------|---------|---------|
| No authorizer | anything | Allowed. This arm is an escalation path and is kept as one — a `Small` engine reaches `make-thread`, and `system` (the `/bin/sh -c` primitive, registered by the `process` extension `Small` excludes), through `(environment '(wile kitchen-sink))`. The ground is not that no such path exists, but that an embedder who installed no policy has accepted whatever Scheme can reach. |
| Authorizer installed | profile ⊆ the engine's own surface | Allowed, without consulting the authorizer. Acquiring nothing new is not a capability question. |
| Authorizer installed | profile ⊄ the engine's own surface | Refused, unless the authorizer permits `namespace:create` with the profile name as target. The built-in authorizers deny unknown resources, so `Console`/`ConsoleWithLoad` refuse; a custom authorizer can opt in. |

Containment is over *primitive names*, not extension identities: `Console` carries `all.SafeExtension` while `KitchenSink` carries `all.Extension` and not the Safe one, so set-inclusion over extension values would report `Console ⊄ KitchenSink`. Names are what a program can actually call.

This closes the specific hole an authorizer could not: `threads`, `gointerop`, and `namespace` declare no gate sites at all, so their primitives became reachable from an engine that never registered them and no policy was ever consulted — an authorizer cannot refuse what it is not asked about.

An optional third element narrows the constructed namespace's *visible* top level — `(environment '(wile small core))` pre-binds only the core surface, `(environment '(wile small no-bindings))` pre-binds nothing. It is not a security control and does not narrow this widening: the profile still decides what is *registered*, and the constructed environment can import its way back to all of it. See [Strict namespace](../embedding/api-design.md#strict-namespace).

What the child namespace does inherit is the authorizer: `Namespace.NewChildNamespace` copies it, so gated operations (`file`, `code`, `env`, `process`, `namespace`, `stream`) stay under the same policy no matter which profile named them. A child given a *stricter* authorizer keeps it: the policy at a gate is `Namespace.EffectiveAuthorizer` — root ∧ child, most-restrictive-wins — resolved from the namespace the code is **executing** in, which for `Engine.EvalIn` is the target rather than the one that registered the primitive. The registry layer is therefore not an authority boundary against Scheme code that holds `environment`; the authorization layer is. If you rely on an extension's *absence* for safety, do not also grant the eval extension, or install an authorizer that covers the operations you care about.

### Virtual environment variables

`WithEnv(k, v)` and `WithEnvMap(m)` install a virtual env-var map. When set, `get-environment-variable` reads from this map instead of `os.Getenv`. `Console`/`ConsoleWithLoad` allocate an empty map by default, so OS env vars are sandboxed unless explicitly populated:

```go
engine, err := wile.NewEngine(ctx,
    wile.WithProfile(wile.Console),
    wile.WithEnv("APP_MODE", "production"),
)
```

### Custom: pick exactly what you need

```go
engine, err := wile.NewEngine(ctx,
    wile.WithExtension(io.Extension),
    wile.WithExtension(math.Extension),
)
```

Core is always included unless you explicitly opt out.

### Bare engine: no core

```go
engine, err := wile.NewEngine(ctx,
    wile.WithoutCore(),
    wile.WithExtension(math.Extension),
)
```

This produces an engine where only `sqrt`, `sin`, `cos`, etc. exist. Even `+`, `car`, and `if` are absent. This is useful for building highly specialized engines.

Note: Both `WithoutCore()` and `WithRegistry(reg)` set the registry. `WithRegistry` provides a pre-populated registry (skipping default core setup). `WithoutCore` provides an empty registry. If both are used, last-wins (standard Go options semantics).

## Enforcement mechanism

The first layer is enforced at the **registry level**, which operates at engine construction time:

1. `NewEngine` builds a `Registry` and populates it with core + requested extensions.
2. The registry is applied to the environment, creating global bindings for each primitive.
3. The compiler resolves variable references against the environment.
4. If a name has no binding, the compiler produces a `CompilationError` — the code never reaches the VM.

This means:
- **No runtime overhead**: There are no permission checks in the hot path. Absent primitives simply don't exist.
- **Fail-fast**: Errors are caught at compile time, not at execution time.
- **No bypass**: There is no `eval`-like escape hatch unless the eval extension is explicitly loaded.

The second layer is enforced at the **gate sites** listed above, at the moment the operation runs. `MachineContext.Authorizer()` reads the authorizer recorded on the namespace, and a denial returns an error wrapping `security.ErrAccessDenied` with the action, resource, and target attached. The policy is normally set once at construction, but `Namespace.SetAuthorizer` can tighten it later and a library load composes the caller namespace's authorizer with the root's, so a tightening reaches even an import the cache had already served. Two properties follow: the check costs one call per privileged operation and nothing at all on ordinary computation, and the policy sees a *value* (a path, a variable name, a command) that the registry layer could never inspect.

**A denial is not a Scheme condition.** `guard` and `with-exception-handler` cannot absorb it: it terminates the evaluation and escapes to the host as an error satisfying `errors.Is(err, security.ErrAccessDenied)`, carrying the raise-site source location and VM stack trace. Otherwise a sandboxed program could neutralise its own sandbox's refusals by wrapping them in a handler. An embedder who *wants* Scheme to handle a refusal has two options: compose a permissive authorizer for that operation and let the primitive's own `file-error` surface, or catch at the host boundary around `Eval`/`EvalMultiple`.

Where the target is a path, the check and the subsequent syscall must agree on which file they mean. Containment is not a lexical prefix test: `security.containedInRoot` canonicalizes both the root and the target through `filepath.EvalSymlinks` (resolving as far as the path exists, so a not-yet-created file is still admissible under an existing root), which is what lets a symlinked root such as macOS `/tmp` work and what rejects a symlink staged inside the root that points out of it. On top of that, an authorizer implementing `security.RootConfined` causes both the file primitives (`extensions/files/confined.go`) and the source loader (`resolver/confined.go`) to open through `os.Root`, closing the TOCTOU window between the by-name check and the by-descriptor open. When no root is reported, `resolver.openUnconfined` re-gates the symlink-resolved real path before opening it.

## Why Scheme makes sandboxing tractable

Wile's sandboxing works because Scheme's language design cooperates with capability-based security in ways that imperative languages resist. The registry mechanism described above is simple — but it is only simple *because* the language doesn't fight it. The properties below are structural consequences of Scheme's lambda-calculus foundation, not bolted-on restrictions.

### Authority is lexical

In Scheme, a procedure can only invoke operations present in its lexical environment. There is no ambient authority — no global `os` module reachable via `import`, no `globalThis.fetch`, no `System.exit()` accessible from any scope. If `open-input-file` is not bound in the environment, no Scheme expression can conjure it into existence.

This is the property Rees (1996) formalized: closures *are* capabilities. A closure over a file port can read that file; a closure without one cannot. Authority flows through bindings, and bindings are controlled by whoever constructs the environment — in Wile's case, the registry.

Contrast with imperative languages, where ambient authority creates escape hatches that sandboxing must individually seal:

| Language | Ambient authority escape | Mitigation |
|----------|--------------------------|------------|
| Python | `__import__('os')`, `__builtins__`, `getattr` | Restricted execution (abandoned in CPython as infeasible) |
| JavaScript | `globalThis`, prototype pollution, `eval`, `import()` | Frozen realms (SES/Hardened JS), membrane proxies |
| Java | `Class.forName`, `setAccessible(true)` | SecurityManager (deprecated JDK 17, permanently disabled JDK 24) |
| Ruby | `ObjectSpace`, `send`, `const_get` | No standard solution |
| Scheme | None — authority is lexically scoped | Registry controls which bindings exist |

Each of those mitigations is a patch over a language feature that assumes ambient access. Scheme doesn't need patches because it never assumed ambient access in the first place.

### No mutable dispatch

Scheme is not fully immutable — pairs have `set-car!`/`set-cdr!`, vectors have `vector-set!`, and R7RS strings have `string-set!`. But Scheme lacks **mutable dispatch**: there are no prototypes, method tables, or class hierarchies that an attacker can modify to change what operations mean.

In JavaScript, modifying `Array.prototype.push` affects every array in the program. In Python, monkey-patching a class method changes behavior for every instance. In Java, reflection can replace `private` field values on shared objects. Each of these is a single mutation that poisons behavior globally.

Scheme has no equivalent. Operations like `car`, `+`, and `open-input-file` are lexical bindings, not methods on mutable objects. A `set!` in one scope doesn't affect closures that already captured the original value. The authority graph — which bindings exist and what they resolve to — is determined by lexical structure, not by mutable object state.

### No reflection escape hatches

Scheme provides no built-in mechanism to:
- Access bindings outside the current lexical scope
- Enumerate or modify an environment's internal structure
- Bypass access controls via a metaobject protocol
- Load arbitrary code without an explicit `eval` binding

Wile's `introspection` extension (`environment-bound-names`, `environment-ref`) is opt-in and read-only: it observes an environment object, it cannot add a binding to one. It is not, however, a reachability bound. `environment-ref` yields the *value* of a binding, so it hands out whatever capability the environment it is given already holds. The eval extension's `(environment '(wile <profile>))` can still supply such an environment, but only one contained in the engine's own surface once an authorizer is installed (see [Profile namespaces widen the surface](#profile-namespaces-widen-the-surface)). The authorization layer, not the extension list, is what bounds authority once `environment` is in scope.

This is a sharp contrast with languages like Python (where `__builtins__` and `getattr` provide universal introspection), Java (where reflection can bypass `private` access), or JavaScript (where property enumeration and `Proxy` provide deep metaprogramming). In those languages, sandboxing must anticipate every reflective path to authority. In Scheme, there are no reflective paths unless you create them.

### Hygienic macros preserve boundaries

Unhygienic macro systems (C preprocessor, Common Lisp `defmacro`) can accidentally — or deliberately — capture bindings from the expansion site. A macro could smuggle a reference to a privileged operation into unprivileged code.

Wile's hygienic macro system (Flatt 2016) prevents this: macro-introduced identifiers resolve in the macro's *definition* environment, not the use site. A macro defined in a privileged library cannot leak its internal bindings into user code, and user code cannot capture a macro's internal references. The scope-set mechanism that enforces hygiene is the same mechanism that enforces sandboxing — both are consequences of lexical scoping.

### Closures are the composition mechanism

In capability-secure systems, the hard problem is *attenuation*: granting partial authority (read but not write, this directory but not that one). In Scheme, attenuation is just a closure:

```scheme
;; Full authority: can write anywhere
(define write-file open-output-file)

;; Attenuated: only accepts paths spelled under /tmp
(define (safe-write-file path)
  (if (string-prefix? "/tmp/" path)
      (open-output-file path)
      (error "access denied" path)))
```

The attenuated capability is a first-class value that can be passed, stored, and composed — using the same mechanisms as any other Scheme value. There is no separate "policy language" or "permission descriptor" — the language's own composition mechanism *is* the security mechanism.

The shape is the point, not the predicate. A lexical prefix test is not path containment: `/tmp/../etc/passwd` passes it, and so does a symlink under `/tmp`. Wile's own `/tmp` confinement is `security.containedInRoot`, which canonicalizes both sides first. Write the attenuation as a closure, but put a containment check inside it.

This is the central thesis of Miller (2006): in a language where authority flows through closures, capability security and software engineering are the same discipline. Good modularity *is* good security.

### The practical consequence

These properties compound. Because authority is lexical, removing a binding from the registry makes it inexpressible — not merely blocked. Because there are no ambient escape hatches, no permission check is needed on the hot path; the checks that do exist sit at the handful of gate sites where a capability actually touches the host. Because closures compose, fine-grained attenuation uses the same tools as ordinary programming.

The result is that the registry layer costs nothing at runtime and fails at compile time, the authorization layer costs one check per privileged operation, and neither requires ongoing maintenance against new escape vectors — because the language doesn't generate escape vectors.

## What sandboxing does NOT cover

| Concern | Status | Mitigation |
|---------|--------|-----------|
| CPU time | Not covered | Use `context.WithTimeout` on the `ctx` passed to `Eval`. |
| Memory / allocation | Not covered | Use OS-level limits (cgroups, ulimits). |
| Stack depth | Partially covered | `WithMaxCallDepth(n)` limits continuation stack depth. |
| Goroutine exhaustion | Not covered (if threads or gointerop extension loaded) | Don't load threads or gointerop extensions for untrusted code. Omitting them is now sufficient against `(environment '(wile kitchen-sink))` *provided an authorizer is installed* — see [Extension-set escalation](#profile-namespaces-widen-the-surface) below. |
| Information flow | Not covered | A privileged library can pass capabilities (e.g., an open file handle) to unprivileged code via exported values. Preventing this requires an object-capability model. |
| Extension-set escalation | Covered when an authorizer is installed | `(environment '(wile <profile>))` may only construct a namespace contained in the engine's own primitive surface; widening is refused unless the authorizer permits `namespace:create`. An engine with **no** authorizer keeps the unrestricted widening path by design: an embedder who installed no policy has accepted whatever Scheme can reach, which for this row means a `Small` engine can reach `make-thread`. Not that no such path exists — it does, and is measured. See [Profile namespaces widen the surface](#profile-namespaces-widen-the-surface). |
| `include` | Covered by authorizer | `include` is a compile-time form that reads files. It is NOT gated by the files extension — it's part of the compiler. However, it is gated by `security.CheckWithAuthorizer` (resource `code`, action `load`), so an authorizer can restrict it. Without an authorizer, it is unrestricted. `include-ci` is the same code path (`compileIncludeImpl` with case folding on) and is gated identically. |

### The `include` note

`(include "file.scm")` is a compile-time special form, not a runtime primitive. It reads a file during compilation, regardless of whether the files extension is loaded. However, `include`, `include-ci`, runtime `(load ...)`, and library `import` all resolve through the same `FileResolver` chain, and every OS- or FS-backed resolver in it gates on `security.CheckWithAuthorizer` (resource `code`, action `load`), so a `WithAuthorizer` policy can restrict which files are loaded. `EmbedFileResolver` is the exception, and it serves only the compiled-in bootstrap sources.

`WithSourceFS(fsys)` adds a virtual filesystem layer to the source resolver chain. Multiple calls add layers searched in order. When only `WithSourceFS` is used (without `WithSourceOS()`), the OS filesystem is excluded — Scheme code can only access files in the configured virtual filesystems.

Without an authorizer or `WithSourceFS`, `include` is unrestricted on the OS filesystem. If you are compiling untrusted source code, either:
- Use `WithSourceFS(fsys)` to confine source loading to a virtual filesystem.
- Set a `WithAuthorizer` policy (e.g., `FilesystemRoot("/app/src")`) to restrict load paths. As of 2026-08-21 `FilesystemRoot` also **denies** every resource it does not model — `process`, `env`, `namespace`, and `code:eval` — so it is a usable standalone policy for untrusted source rather than a file gate with the process door left open. Its one exemption is `stream`: the program can still write to the host's stdout. Compose with `DenyAll` via `All(...)` to take that away, and note that `FilesystemRoot` still bounds only *paths* — it says nothing about CPU or memory.
- Pre-compile trusted code and run it with `Engine.Run`.
- Use OS-level filesystem restrictions (chroot, namespaces).

## Testing

Isolation invariants are verified in `pkg/wile/engine_sandbox_test.go`:

- Safe engine rejects privileged primitives — at compile time for unregistered names (e.g., `eval`, `exit`, `make-atomic`) and at runtime via the authorizer for registered-but-gated operations (e.g., `open-input-file` outside `/tmp`) (`TestConsole_RejectsPrivileged`)
- Safe engine allows safe primitives (`TestConsole_AllowsSafe`)
- `WithoutCore()` produces a bare engine, and `WithoutCore()` + extension gives only that extension (`TestWithoutCore_BareEngine`, `TestWithoutCore_PlusExtension`)
- Library propagation respects restrictions (`TestConsole_LibraryPropagation`)
- Every gate action denies under `DenyAll()` (`TestAuthorizer_DenyAllSweep`), which is one row per action and is meant to grow when a gate action is added
- No denial is catchable from Scheme (`TestDenialIsUnswallowable`), the handler-wrapped mirror of that sweep plus `load`, `with-exception-handler`, `dynamic-wind`, and an SRFI-18 `thread-join!`; its last two rows are ratchets proving the rule keys on the denial sentinel rather than on file errors generally
- A denial still reaches the host with its source location and stack trace as *fields*, not merely flattened into the message (`TestDenialCarriesItsProvenance`), and it unwinds like any other escaping error rather than running an enclosing `dynamic-wind`'s after thunk (`TestUncaughtDenialUnwindsLikeAnyOtherUncaughtError`)
- A cached library import is re-authorized, and a library load composes the caller namespace's authorizer with the root's (`TestLibraryCacheReauthorizedOnHit`, `TestLibraryLoadUsesCallerAuthorizerOnMiss`, with `TestSyntheticLibraryImportSkipsPathGate` as the anti-over-denial ratchet)

Two companion files carry the rest: `pkg/wile/engine_sandbox_escape_test.go` (symlink escape denied; eval allowed under `ConsoleWithLoad` and denied by a denying authorizer) and `pkg/wile/authorizer_precedence_test.go` (explicit authorizer beats profile, order-independently; sandbox layers accumulate). Path containment itself is tested in `pkg/security/path_containment_test.go` and `pkg/machine/compilation/resolver/confined_test.go`.

## References

- Jack B. Dennis, Earl C. Van Horn, "Programming Semantics for Multiprogrammed Computations", CACM 1966. https://doi.org/10.1145/365230.365252
- Norm Hardy, "The Confused Deputy", ACM SIGOPS 1988. https://doi.org/10.1145/54289.871709
- Jonathan Rees, "A Security Kernel Based on the Lambda Calculus", MIT AI Memo 1564, 1996. https://dspace.mit.edu/handle/1721.1/5944
- Mark S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control", PhD Dissertation, Johns Hopkins, 2006. http://www.erights.org/talks/thesis/
- Mark S. Miller et al., "Caja: Safe active content in sanitized JavaScript", Google, 2008. https://google-code-archive-downloads.storage.googleapis.com/v2/code.google.com/google-caja/caja-spec-2008-06-06.pdf
- Matthew Flatt, "Binding as Sets of Scopes", POPL 2016. https://doi.org/10.1145/2837614.2837620
- Mark S. Miller, Mike Samuel, et al., "Safe ECMAScript (SES)", TC39 Proposal. https://github.com/tc39/proposal-ses

## Related

- [`extensions/architecture.md`](../extensions/architecture.md) — Extension system architecture, engine options reference
- [`embedding/api-design.md`](../embedding/api-design.md) — Public embedding API, sandboxing subsection
