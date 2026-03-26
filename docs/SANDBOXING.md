# Sandboxing

Wile's extension system provides capability-based sandboxing for embedded Scheme engines (Rees, "A Security Kernel Based on the Lambda Calculus", 1996). Primitives not in the engine's registry don't exist — attempts to use them produce compile-time errors, not runtime checks that could be bypassed.

## How it works

By default, `NewEngine(ctx)` includes only core primitives (arithmetic, pairs, lists, vectors, strings, characters, bytevectors, control flow, syntax, parameters). Extensions are opt-in via `WithExtension()`. If the filesystem extension isn't loaded, `open-input-file` is an unbound variable — the binding doesn't exist in the environment at all.

This restriction is **transitive**: when the library system is enabled (`WithLibraryPaths`), library environments are created by a factory that closes over the engine's registry. A library loaded from a `.sld` file gets the same set of primitives as the engine that loaded it. There is no way for Scheme code to escalate privileges within a single engine (Hardy, "The Confused Deputy", 1988).

## Extension security classification

| Category | Extensions | Package | Risk |
|----------|-----------|---------|------|
| **Safe** | core | `registry/core` | None. Pure computation. |
| **Safe** | io | `internal/extensions/io` | None. In-memory and caller-provided ports only. No filesystem access. |
| **Safe** | math | `extensions/math` | None. `sqrt`, `sin`, `cos`, transcendental functions. |
| **Safe** | introspection | `extensions/introspection` | None. `environment?`, `interaction-environment`, `environment-bound-names`, `environment-ref`, `environment-bound?`. Read-only. |
| **Safe** | all (safe subset) | `internal/extensions/all` | None. Records, promises, additional string/character ops. |
| **Privileged** | files | `extensions/files` | Filesystem: `open-input-file`, `open-output-file`, `delete-file`, `file-exists?`. |
| **Privileged** | eval | `internal/extensions/eval` | Code loading: `eval`, `load`, `environment`, `expand`, `compile`. |
| **Privileged** | system | `extensions/system` | Process: `exit`, `emergency-exit`, `command-line`, `get-environment-variable`. |
| **Privileged** | process | `extensions/process` | Process execution: `system`, `process-spawn`, `process-wait`, `process-kill`. |
| **Context-dependent** | gointerop | `extensions/gointerop` | Go concurrency primitives: channels, wait groups, rw-mutexes, atomics, once. Resource exhaustion via unbounded object creation. No ambient authority. Safe for trusted code. |
| **Context-dependent** | threads | `extensions/threads` | SRFI-18 threads, mutexes, condition variables. Resource exhaustion via unbounded thread creation. Safe for trusted code. |

**Safe** means no ambient authority (Dennis & Van Horn 1966; Miller, "Robust Composition", 2006) — no way to affect the host system. **Privileged** means the extension grants capabilities that untrusted code should not have. **Context-dependent** means the risk depends on the trust level of the code being executed.

## API

### Safe sandbox (recommended for untrusted code)

```go
engine, err := wile.NewEngine(ctx, wile.WithSafeExtensions())
```

This includes core + io + math + introspection + records/promises/strings/characters. No filesystem, no eval, no system calls, no Go concurrency (gointerop, threads).

### Safe sandbox with library support

```go
engine, err := wile.NewEngine(ctx,
    wile.WithSafeExtensions(),
    wile.WithLibraryPaths("./stdlib/lib"),
)
```

Libraries loaded from `./stdlib/lib` inherit the safe restriction. A library that tries to call `open-input-file` gets a compile-time error.

### Composable: safe + specific extensions

`SafeExtensions()` returns `[]EngineOption`, so you can compose:

```go
engine, err := wile.NewEngine(ctx,
    append(wile.SafeExtensions(),
        wile.WithExtension(threads.Extension),
        wile.WithLibraryPaths("./stdlib/lib"),
    )...,
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

Sandboxing is enforced at the **registry level**, which operates at engine construction time:

1. `NewEngine` builds a `Registry` and populates it with core + requested extensions.
2. The registry is applied to the environment, creating global bindings for each primitive.
3. The compiler resolves variable references against the environment.
4. If a name has no binding, the compiler produces a `CompilationError` — the code never reaches the VM.

This means:
- **No runtime overhead**: There are no permission checks in the hot path. Absent primitives simply don't exist.
- **Fail-fast**: Errors are caught at compile time, not at execution time.
- **No bypass**: There is no `eval`-like escape hatch unless the eval extension is explicitly loaded.

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

Wile's `introspection` extension (`environment-bound-names`, `environment-ref`) is opt-in and read-only. Even when loaded, it operates within the engine's existing environment — it cannot introduce new bindings or access extensions that weren't registered.

This is a sharp contrast with languages like Python (where `__builtins__` and `getattr` provide universal introspection), Java (where reflection can bypass `private` access), or JavaScript (where property enumeration and `Proxy` provide deep metaprogramming). In those languages, sandboxing must anticipate every reflective path to authority. In Scheme, there are no reflective paths unless you create them.

### Hygienic macros preserve boundaries

Unhygienic macro systems (C preprocessor, Common Lisp `defmacro`) can accidentally — or deliberately — capture bindings from the expansion site. A macro could smuggle a reference to a privileged operation into unprivileged code.

Wile's hygienic macro system (Flatt 2016) prevents this: macro-introduced identifiers resolve in the macro's *definition* environment, not the use site. A macro defined in a privileged library cannot leak its internal bindings into user code, and user code cannot capture a macro's internal references. The scope-set mechanism that enforces hygiene is the same mechanism that enforces sandboxing — both are consequences of lexical scoping.

### Closures are the composition mechanism

In capability-secure systems, the hard problem is *attenuation*: granting partial authority (read but not write, this directory but not that one). In Scheme, attenuation is just a closure:

```scheme
;; Full authority: can write anywhere
(define write-file open-output-file)

;; Attenuated: can only write to /tmp
(define (safe-write-file path)
  (if (string-prefix? "/tmp/" path)
      (open-output-file path)
      (error "access denied" path)))
```

The attenuated capability is a first-class value that can be passed, stored, and composed — using the same mechanisms as any other Scheme value. There is no separate "policy language" or "permission descriptor" — the language's own composition mechanism *is* the security mechanism.

This is the central thesis of Miller (2006): in a language where authority flows through closures, capability security and software engineering are the same discipline. Good modularity *is* good security.

### The practical consequence

These properties compound. Because authority is lexical, removing a binding from the registry makes it inexpressible — not merely blocked. Because there are no ambient escape hatches, there are no runtime permission checks on the hot path. Because closures compose, fine-grained attenuation uses the same tools as ordinary programming.

The result is that Wile's sandboxing has zero runtime cost, fails at compile time, and requires no ongoing maintenance against new escape vectors — because the language doesn't generate escape vectors.

## What sandboxing does NOT cover

| Concern | Status | Mitigation |
|---------|--------|-----------|
| CPU time | Not covered | Use `context.WithTimeout` on the `ctx` passed to `Eval`. |
| Memory / allocation | Not covered | Use OS-level limits (cgroups, ulimits). |
| Stack depth | Partially covered | `WithMaxCallDepth(n)` limits continuation stack depth. |
| Goroutine exhaustion | Not covered (if threads extension loaded) | Don't load threads extension for untrusted code. |
| Information flow | Not covered | A privileged library can pass capabilities (e.g., an open file handle) to unprivileged code via exported values. Preventing this requires an object-capability model. |
| `include` / `include-ci` | Covered by authorizer | These are compile-time forms that read files. They are NOT gated by the files extension — they're part of the compiler. However, they are gated by `security.Check` (resource `code`, action `load`), so an authorizer can restrict them. Without an authorizer, they are unrestricted. |

### The `include` note

`(include "file.scm")` is a compile-time special form, not a runtime primitive. It reads a file during compilation, regardless of whether the files extension is loaded. However, `include` and library loading are gated by `security.Check` (resource `code`, action `load`), so a `WithAuthorizer` policy can restrict which files are loaded.

`WithSourceFS(fsys)` adds a virtual filesystem layer to the source resolver chain. Multiple calls add layers searched in order. When only `WithSourceFS` is used (without `WithSourceOS()`), the OS filesystem is excluded — Scheme code can only access files in the configured virtual filesystems.

Without an authorizer or `WithSourceFS`, `include` is unrestricted on the OS filesystem. If you are compiling untrusted source code, either:
- Use `WithSourceFS(fsys)` to confine source loading to a virtual filesystem.
- Set a `WithAuthorizer` policy (e.g., `FilesystemRoot("/app/src")`) to restrict load paths.
- Pre-compile trusted code and run it with `Engine.Run`.
- Use OS-level filesystem restrictions (chroot, namespaces).

## Testing

Isolation invariants are verified in `engine_sandbox_test.go`:

- Safe engine rejects privileged primitives at compile time
- Safe engine allows safe primitives
- `WithoutCore()` produces a bare engine
- `WithoutCore()` + extension gives only that extension
- Library propagation respects restrictions

## References

- Jack B. Dennis, Earl C. Van Horn, "Programming Semantics for Multiprogrammed Computations", CACM 1966. https://doi.org/10.1145/365230.365252
- Norm Hardy, "The Confused Deputy", ACM SIGOPS 1988. https://doi.org/10.1145/54289.871709
- Jonathan Rees, "A Security Kernel Based on the Lambda Calculus", MIT AI Memo 1564, 1996. https://dspace.mit.edu/handle/1721.1/5944
- Mark S. Miller, "Robust Composition: Towards a Unified Approach to Access Control and Concurrency Control", PhD Dissertation, Johns Hopkins, 2006. http://www.erights.org/talks/thesis/
- Mark S. Miller et al., "Caja: Safe active content in sanitized JavaScript", Google, 2008. https://google-code-archive-downloads.storage.googleapis.com/v2/code.google.com/google-caja/caja-spec-2008-06-06.pdf
- Matthew Flatt, "Binding as Sets of Scopes", POPL 2016. https://doi.org/10.1145/2837614.2837620
- Mark S. Miller, Mike Samuel, et al., "Safe ECMAScript (SES)", TC39 Proposal. https://github.com/tc39/proposal-ses

## Related

- `docs/EXTENSIONS.md` — Extension system architecture, engine options reference
- `docs/design/EMBEDDING.md` — Public embedding API, sandboxing subsection
- `plans/SECURITY.md` — Opcode resource limits (match steps, expand steps, continuation copy depth)
