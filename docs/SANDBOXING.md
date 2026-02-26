# Sandboxing

Wile's extension system provides capability-based sandboxing for embedded Scheme engines. Primitives not in the engine's registry don't exist — attempts to use them produce compile-time errors, not runtime checks that could be bypassed.

## How it works

By default, `NewEngine(ctx)` includes only core primitives (arithmetic, pairs, lists, vectors, strings, characters, bytevectors, control flow, syntax, parameters). Extensions are opt-in via `WithExtension()`. If the filesystem extension isn't loaded, `open-input-file` is an unbound variable — the binding doesn't exist in the environment at all.

This restriction is **transitive**: when the library system is enabled (`WithLibraryPaths`), library environments are created by a factory that closes over the engine's registry. A library loaded from a `.sld` file gets the same set of primitives as the engine that loaded it. There is no way for Scheme code to escalate privileges within a single engine.

## Extension security classification

| Category | Extensions | Package | Risk |
|----------|-----------|---------|------|
| **Safe** | core | `registry/core` | None. Pure computation. |
| **Safe** | io | `internal/extensions/io` | None. In-memory and caller-provided ports only. No filesystem access. |
| **Safe** | exceptions | `extensions/exceptions` | None. `raise`, `guard`, `with-exception-handler`, `error`. |
| **Safe** | math | `extensions/math` | None. `sqrt`, `sin`, `cos`, transcendental functions. |
| **Safe** | all (safe subset) | `internal/extensions/all` | None. Records, promises, additional string/character ops. |
| **Privileged** | files | `extensions/files` | Filesystem: `open-input-file`, `open-output-file`, `delete-file`, `file-exists?`. |
| **Privileged** | eval | `internal/extensions/eval` | Code loading: `eval`, `load`, `interaction-environment`, `environment`. |
| **Privileged** | system | `extensions/system` | Process: `exit`, `emergency-exit`, `command-line`, `get-environment-variable`. |
| **Context-dependent** | gointerop | `extensions/gointerop` | Go concurrency primitives: channels, wait groups, rw-mutexes, atomics, once. Resource exhaustion via unbounded object creation. No ambient authority. Safe for trusted code. |
| **Context-dependent** | threads | `extensions/threads` | SRFI-18 threads, mutexes, condition variables. Resource exhaustion via unbounded thread creation. Safe for trusted code. |

**Safe** means no ambient authority — no way to affect the host system. **Privileged** means the extension grants capabilities that untrusted code should not have. **Context-dependent** means the risk depends on the trust level of the code being executed.

## API

### Safe sandbox (recommended for untrusted code)

```go
engine, err := wile.NewEngine(ctx, wile.WithSafeExtensions())
```

This includes core + io + exceptions + math + records/promises/strings/characters. No filesystem, no eval, no system calls, no Go concurrency (gointerop, threads).

### Safe sandbox with library support

```go
engine, err := wile.NewEngine(ctx,
    wile.WithSafeExtensions(),
    wile.WithLibraryPaths("./lib"),
)
```

Libraries loaded from `./lib` inherit the safe restriction. A library that tries to call `open-input-file` gets a compile-time error.

### Composable: safe + specific extensions

`SafeExtensions()` returns `[]EngineOption`, so you can compose:

```go
engine, err := wile.NewEngine(ctx,
    append(wile.SafeExtensions(),
        wile.WithExtension(threads.Extension),
        wile.WithLibraryPaths("./lib"),
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

Note: `WithoutCore()` and `WithRegistry(reg)` are independent. `WithRegistry` provides a pre-populated registry (skipping default core setup). `WithoutCore` creates an empty registry. If both are set, `WithRegistry` takes precedence (the custom registry is used as-is, `skipCore` has no effect).

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

## What sandboxing does NOT cover

| Concern | Status | Mitigation |
|---------|--------|-----------|
| CPU time | Not covered | Use `context.WithTimeout` on the `ctx` passed to `Eval`. |
| Memory / allocation | Not covered | Use OS-level limits (cgroups, ulimits). |
| Stack depth | Partially covered | `WithMaxCallDepth(n)` limits continuation stack depth. |
| Goroutine exhaustion | Not covered (if threads extension loaded) | Don't load threads extension for untrusted code. |
| Information flow | Not covered | A privileged library can pass capabilities (e.g., an open file handle) to unprivileged code via exported values. Preventing this requires an object-capability model. |
| `include` / `include-ci` | Not fully covered | These are compile-time forms that read files. They are NOT gated by the files extension — they're part of the compiler. A future authorization framework (see `plans/SECURITY.md`) will gate these. |

### The `include` gap

`(include "file.scm")` is a compile-time special form, not a runtime primitive. It reads a file from disk during compilation, regardless of whether the files extension is loaded. This is a known gap: sandboxed engines that compile untrusted code with `include` forms can read arbitrary files.

**Mitigations**:
- Don't compile untrusted source that may contain `include`. Pre-compile trusted code and run it with `Engine.Run`.
- Use OS-level filesystem restrictions (chroot, namespaces).
- The planned authorization framework will add `Check()` calls to `include` processing.

## Testing

Isolation invariants are verified in `engine_sandbox_test.go`:

- Safe engine rejects privileged primitives at compile time
- Safe engine allows safe primitives
- `WithoutCore()` produces a bare engine
- `WithoutCore()` + extension gives only that extension
- Library propagation respects restrictions

## Related

- `docs/EXTENSIONS.md` — Extension system architecture, engine options reference
- `docs/design/EMBEDDING.md` — Public embedding API, sandboxing subsection
- `plans/SECURITY.md` — Full security model: extension-level sandboxing, authorization framework, opcode resource limits
