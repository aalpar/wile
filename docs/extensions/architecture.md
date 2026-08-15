# Extension System

Wile's extension system lets Go code add Scheme primitives, macros, and global
values to the interpreter. Extensions are modular, composable, and — when the
R7RS library system is enabled — automatically importable from Scheme via
`(import ...)`.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                        Go embedder                              │
│                                                                 │
│  engine, _ := wile.NewEngine(ctx,                               │
│      wile.WithExtension(math.Extension),                        │
│      wile.WithExtension(myExt),                                 │
│      wile.WithLibraryPaths(),          ← enables (import ...)   │
│  )                                                              │
└──────────────────────────────┬──────────────────────────────────┘
                               │
               ┌───────────────▼───────────────┐
               │         Registry              │
               │                               │
               │  core primitives              │
               │  + math extension             │
               │  + myExt                      │
               │  + bindings, macros, globals  │
               └───────────────┬───────────────┘
                               │ Apply()
               ┌───────────────▼───────────────┐
               │     EnvironmentFrame          │
               │                               │
               │  Phase 0 (runtime)  ← procs   │
               │  Phase 1 (expand)   ← macros  │
               │  Phase 2 (compile)  ← syntax  │
               └───────────────────────────────┘
```

### Lifecycle

1. **Registration** — Extensions call `Registry.AddPrimitives`, `AddBindings`,
   `AddMacroSource`, `AddGlobalValue`, `AddNamespaceInit` during `NewEngine`.
2. **Application** — `Registry.Apply` materializes everything into a live
   environment, following a strict phase order.
3. **Library creation** — If `WithLibraryPaths` was called, each extension's
   runtime primitives are wrapped in a synthetic `CompiledLibrary` and
   registered in the `LibraryRegistry`.
4. **Evaluation** — Scheme code can use extension primitives directly (they're
   in the top-level environment) or import them selectively via `(import ...)`.

---

## Creating an Extension

### Minimal Extension

```go
package myext

import (
    "github.com/aalpar/wile/pkg/machine"
    "github.com/aalpar/wile/pkg/registry"
    "github.com/aalpar/wile/pkg/values"
    "github.com/aalpar/wile/pkg/werr"
)

// Extension is the package's entry point.
var Extension = registry.NewDescribedExtension("myext",
    "Brief description of what this extension provides.",
    AddToRegistry)

// Builder composes registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry is the combined registration function.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.PrimitiveRegistry) error {
    r.AddPrimitives([]registry.PrimitiveSpec{
        {
            Name:       "double",
            ParamCount: 1,
            Impl:       primDouble,
            Doc:        "Returns twice the argument.",
            ParamNames: []string{"n"},
            Category:   "myext",
        },
    }, registry.PhaseSetRuntime)
    return nil
}

func primDouble(mc machine.CallContext) error {
    n, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "double: expected number")
    }
    mc.SetValue(n.Add(n))
    return nil
}
```

### Loading the Extension

```go
ctx := context.Background()
engine, err := wile.NewEngine(ctx,
    wile.WithExtension(myext.Extension),
)
// double is now available in the top-level environment
result, _ := engine.Eval(ctx, engine.MustParse(ctx, "(double 21)")) // 42
```

### Multiple Extensions

```go
engine, err := wile.NewEngine(ctx,
    wile.WithExtensions(
        math.Extension,
        process.Extension,
        system.Extension,
        myext.Extension,
    ),
)
```

---

## Extension Interface

Every extension implements the `registry.Extension` interface:

```go
type Extension interface {
    Name() string                    // "math", "system", etc.
    AddToRegistry(r *Registry) error // Registers primitives with the registry
}
```

### `ExtensionFunc` Adapter

For simple extensions, `registry.NewDescribedExtension` wraps a function:

```go
var Extension = registry.NewDescribedExtension("myext",
    "Brief description of this extension.",
    func(r *registry.PrimitiveRegistry) error {
        r.AddPrimitive(spec, registry.PhaseSetRuntime)
        return nil
    })
```

### Optional Interfaces

Extensions can implement additional interfaces for extra behavior:

| Interface | Method | Purpose |
|-----------|--------|---------|
| `Describer` | `Description() string` | Human-readable description shown in `,doc` and `,libraries` |
| `LibraryNamer` | `LibraryName() []string` | Custom R7RS library name (default: `(wile <name>)`) |
| `Closeable` | `Close() error` | Resource cleanup when `Engine.Close()` is called |

`registry.ExtensionFunc` satisfies all three unconditionally and carries them as
slots filled by `ExtensionOption` values, so a hand-rolled struct is not needed:

```go
// Custom library name: (myorg utils) instead of (wile custom)
var Extension = registry.NewExtension("custom", AddToRegistry,
    registry.WithDescription("Brief description of this extension."),
    registry.WithLibraryName("myorg", "utils"),
    registry.WithClose(func() error {
        // cleanup
        return nil
    }))
```

An unset slot reports the zero value, which the engine reads as "not set" and
replaces with the default.

---

## Registry API

The `registry.PrimitiveRegistry` is the central store for all registration data.
Extensions interact with it during the registration phase.

### Registering Primitives

```go
// Single primitive
r.AddPrimitive(registry.PrimitiveSpec{
    Name:       "sqrt",
    ParamCount: 1,
    Impl:       primSqrt,
    Doc:        "Returns the square root of z.",
    ParamNames: []string{"z"},
    Category:   "math",
}, registry.PhaseSetRuntime)

// Batch registration
r.AddPrimitives([]registry.PrimitiveSpec{
    {Name: "sin", ParamCount: 1, Impl: primSin},
    {Name: "cos", ParamCount: 1, Impl: primCos},
}, registry.PhaseSetRuntime)
```

### PrimitiveSpec Fields

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `Name` | `string` | yes | Scheme-visible name (`"sqrt"`, `"string->number"`) |
| `ParamCount` | `int` | yes | Fixed parameter count (for variadic, `Arg(ParamCount-1)` holds the rest list) |
| `IsVariadic` | `bool` | no | Accepts variable arguments; last `Arg` slot is the rest list |
| `Impl` | `machine.ForeignFunction` | yes | Go implementation function |
| `Doc` | `string` | no | Brief description |
| `ParamNames` | `[]string` | no | Parameter names for documentation |
| `Category` | `string` | no | Grouping category |
| `ParamTypes` | `[]values.TypeConstraint` | no | Per-parameter type contract. For variadic, last slot annotates rest-list element type |
| `ReturnType` | `values.TypeConstraint` | no | Return-type declaration (nil = unspecified) |
| `Keywords` | `[]string` | no | Searchable tags for `apropos` discovery |
| `InvokesProcedure` | `bool` | see below | Marks a primitive that may call back into a Scheme procedure |

`PrimitiveSpec.Validate` rejects an empty `Name`, a nil `Impl`, a variadic spec
with `ParamCount < 1`, and a `ParamTypes` slice of the wrong length.
`AddPrimitive`/`AddPrimitives` **panic** on a spec that fails it, on the contract
that specs are source literals. An embedder assembling a spec dynamically must
call `Validate` first.

`InvokesProcedure` defaults to `false`, meaning "capture-safe": the frame-reclaim
classifier may trust a tail call to this primitive as non-capturing. Any `Impl`
that reaches `ApplyCallable` or runs a sub-context MUST set it to `true`. The
flipped default is a soundness commitment, and `TestInvokesProcedureStaticGuard`
in `pkg/wile` fails CI when the annotation is missing.

### Registering Other Items

```go
// Compile-time bindings (no runtime value — for auxiliary syntax like else, =>)
r.AddBinding("my-special-form")
r.AddBindings([]string{"else", "=>", "_"})

// Bootstrap macro source (Scheme code evaluated at engine startup)
r.AddMacroSource(`
    (define-syntax my-when
      (syntax-rules ()
        ((my-when test body ...)
         (if test (begin body ...)))))
`)

// Bootstrap procedure source (define forms, loaded into the sealed base frame;
// the file boundary between define-syntax and define is the phase boundary)
r.AddProcedureSource(`(define (my-helper x) (* x x))`)

// Global values (parameters, promises, or any Value), shared by every engine
// that applies this registry
r.AddGlobalValue("euler", eulerConstant)

// Per-engine state (run once per engine, with that engine's runtime frame)
r.AddNamespaceInit(func(env *environment.EnvironmentFrame) error {
    // build per-Namespace state here, not in a package global
    return nil
})

// Initialization callbacks (run after Apply, environment is fully populated)
r.AddInitFunc(func() error {
    // post-registration setup
    return nil
})
```

A `NamespaceInit` **must be idempotent**. The engine re-runs `Apply` (and so the
hook) for every library environment it builds, and those library environments
share the engine's `Namespace`. Minting fresh state on each re-run would reset
the engine mid-eval; this is why `extensions/io` reuses the `State` already on
the `Namespace` instead of allocating a new one (`addPortState` in
`pkg/extensions/io/register.go`), so `(import ...)` cannot discard a redirected
`current-output-port`.

### Application Order

`Registry.Apply()` processes registrations in this fixed order:

```
1. Compile-time bindings     (AddBindings)
2. Compile-only primitives   (PhaseCompile without PhaseRuntime)
3. Runtime primitives        (PhaseRuntime → ForeignClosure at phase 0)
4. Expand-time primitives    (PhaseExpand → ForeignClosure at phase 1)
5. Global values             (AddGlobalValue)
6. Namespace initializers    (AddNamespaceInit → per-engine state)
7. Init functions            (AddInitFunc)
```

Macro and procedure sources are loaded separately by the engine after `Apply()`.

---

## Phases

Phases control when a primitive is available. `registry.PhaseSet` is a bitset
over `environment.Phase` values, and its bits compose with `|`.

| `PhaseSet` bit | Bit | `environment.Phase` | Environment | Purpose |
|----------------|-----|---------------------|-------------|---------|
| `PhaseSetRuntime` | `1` | `PhaseRuntime` (0) | Top-level (phase 0) | Normal runtime evaluation |
| `PhaseSetExpand` | `2` | `PhaseExpand` (1) | Expand (phase 1) | Available during macro expansion |
| `PhaseSetCompile` | `4` | `PhaseCompile` (2) | Compile (phase 2) | Binding-only, no runtime value |

Most extension primitives use `PhaseSetRuntime` only. Primitives needed during
`syntax-rules` expansion use `PhaseSetRuntime | PhaseSetExpand`. Compile-time
bindings (auxiliary syntax keywords such as `else` and `=>`) use `PhaseSetCompile`
or `AddBinding`.

Three limits on this axis, all deliberate:

- **`PhaseSet` is narrower than `Phase`.** It is a `uint8` covering phase indices
  0..7 (`phaseSetBits`, `registry/phase.go`). `PhaseTemplate` (-1) and every macro
  tower phase ≥ 8 are unrepresentable: `With` panics on them, `Has` returns false.
  Registration is a compile-time-constant API, so a programmer error there fails
  loudly rather than silently shifting past the bitset width.
- **Only phases 0 and 1 receive values.** `Apply` iterates a two-row
  `phaseTargets` table (`registry/apply.go`); there is no registration path that
  installs a `ForeignClosure` at phase 2 or above. A primitive an expander needs
  at a tower phase has to reach it some other way.
- **`PhaseSetCompile` is inert alongside `PhaseSetRuntime`.** The compile-only
  pass is guarded by `Has(PhaseCompile) && !Has(PhaseRuntime)`, so
  `PhaseSetRuntime|PhaseSetCompile` registers exactly what `PhaseSetRuntime`
  alone does. Use `PhaseSetCompile` on its own, or not at all.

Under the default immutable top level, `PhaseSetRuntime` primitives are bound in
the sealed base frame rather than the mutable runtime frame, while the closure
still captures the mutable frame so it resolves user definitions. The
registration API does not change.

```go
// Available at runtime only
r.AddPrimitive(spec, registry.PhaseSetRuntime)

// Available at both runtime and during macro expansion
r.AddPrimitive(spec, registry.PhaseSetRuntime|registry.PhaseSetExpand)

// Compile-time binding only (e.g., auxiliary syntax)
r.AddBinding("else")
```

---

## RegistryBuilder: Composing Registration Functions

Large extensions can split registration into logical groups using
`RegistryBuilder`:

```go
var Builder = registry.NewRegistryBuilder(
    addTranscendental,  // exp, log, sin, cos, ...
    addRounding,        // floor, ceiling, truncate, round
    addDivision,        // floor/, truncate/, ...
    addComplex,         // make-rectangular, real-part, ...
)

var AddToRegistry = Builder.AddToRegistry
var Extension = registry.NewDescribedExtension("math",
    "Extended math: trigonometry, logarithms, bitwise operations.",
    AddToRegistry)
```

Each function receives the same `*Registry` and can independently register its
primitives. The builder runs them in order, stopping on the first error.

---

## Writing Primitive Implementations

A primitive implementation has the type `machine.ForeignFunction`:

```go
// machine.ForeignFunction
func(mc machine.CallContext) error
```

### Accessing Arguments

```go
func primAdd(mc machine.CallContext) error {
    a, ok := mc.Arg(0).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "add: first argument")
    }
    b, ok := mc.Arg(1).(values.Number)
    if !ok {
        return werr.WrapForeignErrorf(werr.ErrNotANumber, "add: second argument")
    }
    // The two checks above are load-bearing: Number.Add PANICS on an unexpected
    // type, and nothing on the call path recovers it into a Scheme condition.
    mc.SetValue(a.Add(b))
    return nil
}
```

### Variadic Arguments

For variadic primitives (`IsVariadic: true`), the last argument index holds
a proper list of the excess arguments:

```go
// (my-sum x ...) — ParamCount: 1, IsVariadic: true
func primMySum(mc machine.CallContext) error {
    rest := mc.Arg(0) // proper list of all arguments
    // Walk the list...
}
```

`ParamCount` must be at least 1 when `IsVariadic` is set: the rest parameter
occupies slot `ParamCount-1`, so `ParamCount: 0` with `IsVariadic: true` is
rejected by `PrimitiveSpec.Validate` (and would index past the argument slice in
the dispatch layer). `helpers.VariadicArgs` gathers the fixed arguments and the
rest list into one type-checked slice.

### Return Values

- **Single value**: `mc.SetValue(result)`
- **Void**: `mc.SetValue(values.Void)`
- **Multiple values**: Use `mc.SetValues()` or return values through
  continuation-based protocols

### Error Handling

Use project error types, not bare `errors.New` or `fmt.Errorf`:

```go
// Type check errors
return werr.WrapForeignErrorf(werr.ErrTypeConversion,
    "sqrt: expected number, got %s", v.SchemeString())

// Wrong argument count
return werr.WrapForeignErrorf(werr.ErrWrongNumberOfArguments,
    "my-fn: expected 2 arguments, got %d", n)
```

**Return the error; do not panic.** What the Scheme program and the embedding
host each get to see is specified in [The Error Contract](#the-error-contract)
below, and the two answers are different.

### Recursive Value Types MUST Implement `DeepEqualer`

An extension-defined `values.Value` that can **contain other values** — anything
a cycle could run through — must implement `values.DeepEqualer`:

```go
type DeepEqualer interface {
    EqualComponents(other values.Value, push func(a, b values.Value)) bool
}
```

`values.Equal` (which backs Scheme `equal?`) owns an explicit heap worklist and a
visited set. It decomposes containers by calling `EqualComponents`, so no input
can overflow the Go stack. An implementor decides only whether the two containers
are *shaped* alike (same type, same length, same record type) and `push`es the
component pairs; it must **not** compare components itself, and must not call
`EqualTo` on them — that puts the recursion back on the Go stack, which is the
whole thing the interface removes.

A recursive value type that does **not** implement `DeepEqualer` is compared
through its own `EqualTo`. If that `EqualTo` recurses, `equal?` on a cyclic
instance is a Go `fatal error: stack overflow` — which `recover()` **cannot**
catch, so it kills the embedding host process. Core containers (`*Pair`,
`*Vector`, `*Record`, `*Hashtable`, `*Box`, `*NativeError`, `*CompileTimeValue`)
all implement it; extension types are the remaining exposure, and only the
extension author can close it.

Values that cannot contain other values (a handle, a wrapped `int`, an opaque Go
struct) are leaves and need nothing.

The same reasoning applies to `SchemeString()`: a container that renders its
contents must route children through the writer's cycle-aware path rather than
calling `SchemeString()` on them directly.

---

## RegisterFunc: Go Functions as Scheme Primitives

For simpler Go↔Scheme bridging, `Engine.RegisterFunc` uses reflection to
automatically marshal arguments and return values:

```go
engine.RegisterFunc("double", func(x int64) int64 {
    return x * 2
})

engine.RegisterFunc("greet", func(name string) string {
    return "Hello, " + name
})
```

### Supported Types

| Go type | Scheme type |
|---------|-------------|
| `int64`, `int` | integer |
| `float64` | inexact real |
| `complex128` | complex number (parameter only) |
| `string` | string |
| `bool` | `#t` / `#f` |
| `[]byte` | bytevector |
| `[]T` | proper list (elements converted recursively) |
| `map[K]V` | hashtable (K must be string, int64, int, or bool) |
| `struct` | alist `((FieldName . value) ...)` |
| `func(...)` | callback, parameter only (invokes Scheme lambda via VM sub-context) |
| `wile.Value` | any Scheme value (pass-through) |
| `context.Context` | auto-forwarded (first param only, invisible to Scheme) |
| `error` | last return only — returned as Go error |

Parameter and return conversion are deliberately asymmetric: `complex128` and
`func(...)` are accepted as parameters but rejected as return types, and the
rejection happens at `RegisterFunc` time rather than at call time. Narrowing a
Scheme number to `float64`/`complex128` errors on precision loss unless the
engine was built with `WithLossyConversionsAllowed()`, which is frozen into the
converters at registration.

### Batch Registration

```go
engine.RegisterFuncs(map[string]any{
    "double":  func(x int64) int64 { return x * 2 },
    "greet":   func(name string) string { return "Hello, " + name },
    "is-even": func(x int64) bool { return x%2 == 0 },
})
```

Note: Go map iteration order is unspecified. If multiple functions are invalid,
which one fails first is non-deterministic. Functions registered before the
failure remain registered.

### Callbacks

Go functions can accept Scheme procedures as callback parameters:

```go
engine.RegisterFunc("apply-twice", func(f func(int64) int64, x int64) int64 {
    return f(f(x))
})
```

```scheme
(apply-twice (lambda (x) (* x 2)) 5)  ; → 20
```

Callbacks must be invoked synchronously during the registered function's
execution. Storing a callback or calling it from another goroutine is unsafe.

### Context Forwarding

If the first Go parameter is `context.Context`, the VM's context is forwarded
automatically. It does not count toward the Scheme parameter count:

```go
engine.RegisterFunc("fetch", func(ctx context.Context, url string) (string, error) {
    // ctx carries the VM's context (deadline, cancellation)
    return httpGet(ctx, url)
})
```

```scheme
(fetch "https://example.com")  ; 1 Scheme argument, not 2
```

---

## The Error Contract

**Every failure a primitive *returns* becomes a Scheme condition the program can
`guard`, carrying its message, its kind and its source location. Nothing reaches
the embedding host as a bare Go error except a genuinely host-fatal condition.**

That is one sentence with a narrow subject, and the narrowness is the whole
point of this section. Three words in it are doing work:

- **returns.** A primitive that *panics* is not covered. See
  [Panics are contained, not converted](#panics-are-contained-not-converted).
- **primitive.** The subject is a failure raised inside a primitive's Go body,
  or by the VM on its own behalf while executing one. Failures raised while
  *analysing* a program are a different contract that does not exist yet; see
  [What this contract does not reach](#what-this-contract-does-not-reach).
- **condition the program can guard.** Catchable is not the same as delivered.
  See [Catchable is not delivered](#catchable-is-not-delivered).

### What an embedder actually observes

```go
// The bridge, for orientation:
//   primitive returns err
//     → machine.applyCallableError   (pkg/machine/foreign_closure.go)
//         control signal?  → pass through untouched
//         otherwise        → goErrorToCondition, then RaiseInPlace
//     → the Scheme handler chain (guard / with-exception-handler)
//   nothing caught it
//     → *wile.RuntimeError at the Engine boundary, IsSchemeException() == true
```

`applyCallableError` passes four error types through as control flow rather than
converting them: a prompt abort, the uncaught-exception carrier, a timer
interrupt, and a continuation resume. An extension will not normally construct
any of them; a primitive that calls back into Scheme can *observe* one, and must
return it unchanged rather than swallowing it.

`goErrorToCondition` reads the error's kind off the chain with `errors.As`, so
wrapping is safe: a `*werr.ForeignFileError` anywhere in the chain makes
`(file-error? e)` true, and a `*werr.ForeignReadError` makes `(read-error? e)`
true. Any other error is a generic condition.

A `*values.NativeError` is not converted at all — it is already a condition, so
it is forwarded unchanged and keeps its irritants and its kind. Returning
`values.NewFileError("boom", n)` gives Scheme `(error-object-irritants e)` = `(n)`
and `(file-error? e)` = `#t`, matching what Scheme-side `(error "boom" n)`
produces. The recognition is a direct type assertion, so this applies to the
value you return and **not** to a condition buried under a `werr` wrap: wrapping
adds a message, and forwarding the inner value would discard it, so a wrapped
condition takes the rebuild path above and reports the wrap's message with no
irritants.

### The registration routes used to disagree; they no longer do

Registering the same panicking Go function twice — once with
`RegisterPrimitive`, once with `RegisterFunc` — used to give Scheme two
different answers: the `RegisterPrimitive` one escaped `guard`, the
`RegisterFunc` one was caught by it. The cause was the `RegisterFunc` wrapper's
own `defer recover()`, which lives *inside* the generated `ForeignFunction` and
therefore fired wherever that function was called; `RegisterPrimitive` installs
no such thing.

Closed by **narrowing the wrapper**, not by adding a recover to the other route.
The wrapper now converts only a fault raised by the callback protocol and
re-raises everything else, so a Go-level bug escapes `guard` from either route.
Pinned by `TestHostBugEscapesGuardFromEitherRoute`
(`pkg/wile/panic_channel_uniform_test.go`), which registers one body both ways.

### Panics are contained, not converted

A panic out of a primitive is a Go-level bug, and Wile deliberately refuses to
make it a Scheme condition — a Scheme programmer must not be able to `guard` an
impossible state (`CODING_STYLE.md`). It is contained rather than converted: the
host gets a `*wile.RuntimeError` whose `IsSchemeException()` is `false`, and the
Scheme program gets nothing.

Containment is **one** recover, at one place, plus two that recover something
narrower than "a panic":

| Site | Role |
|---|---|
| `pkg/machine/machine_context.go`, `MachineContext.RunResumable` | the boundary. Wraps any panic as an uncatchable `*machine.SchemeError`, attaching the VM source location and continuation-chain stack trace, naming the primitive when the faulting instruction identifies one, and chaining the original so `errors.Is` still resolves. A panic unwinds the whole goroutine stack, so this one site also catches panics tripped inside nested sub-context `Run()` calls. |
| `pkg/values/thread.go` | the root of an SRFI-18 thread's goroutine. No ancestor frame can recover it, so this is a necessity rather than a policy. |
| `pkg/wile/ffi_wrapper.go`, `ffiSpec.makeWrapper` | **not** a containment site. It converts a fault raised by the callback protocol — which panics because the host's Go signature has no error slot to return through — and re-panics everything else. Matching is on the two protocol sentinels (`ErrFFICallbackError`, `ErrCallbackResultConversion`), not on the error's type: the VM's own invariant guards panic with a `*werr.ForeignError` too, and a type test would convert one of those into a catchable condition whenever it crossed a host function that had called back into Scheme. |

That table is grep-checkable rather than asserted:
`grep -rn 'recover()' --include='*.go' pkg/machine/ pkg/values/ pkg/wile/ | grep -v _test.go`
returns those three plus `pkg/values/big_float.go` (a local guard around
`big.Float` parsing, not a VM boundary). An earlier revision of this
documentation described a further per-call recovery inside a bytecode
`OperationForeignFunctionCall`; that operation was measured to have no
production references and was deleted in 2026-08.

The practical consequence for an extension author is the one already stated
under [Error Handling](#error-handling): the type-coercion helpers in
`pkg/values/promotion.go` and `pkg/values/numeric_tower.go`, and
`emptyList.Car`/`Cdr`, panic on unexpected input. Type-check before calling
them. Nothing downstream will turn that panic into something the Scheme program
can handle.

### An authorizer denial is not an error

**Authorizer errors stay in the authorizer domain.** A denial is a message to
the party that installed the authorizer — the host — not to the program the
authorizer constrains, and it must not impersonate an R7RS condition. Where
R7RS gives a procedure a non-error way to report, that is the right answer:
`file-exists?` returning `#f` on a denied path is the procedure reporting what
it is permitted to report, not an error in disguise.

`file-exists?` is the only procedure with such a non-error way to report, and it
now uses it: a denied path answers `#f`, indistinguishably from an absent one.
Both halves of that are load-bearing. Answering at all is what keeps the denial
out of the program's error domain; answering the *same* as absence is what stops
the predicate becoming an oracle for the filesystem outside the sandbox. A
caller that needs to know which it was is not served by this procedure.

The remaining file procedures are commands, with no non-error answer to give.
Their denials do not become conditions either — they escape the program's
handlers entirely and reach the host, which is the party the message is for. Do
not write an authorizer whose denial is meant to be caught by the sandboxed
program.

Refusal is not the whole error space, and the split matters: a system error that
says nothing about whether a file exists — `EIO`, `ELOOP`, `ENAMETOOLONG`, a
timeout — still raises out of `file-exists?`. Collapsing those to `#f` would
report a file as absent on evidence that never spoke to absence.

### What this contract does not reach

Three carve-outs. Each is somebody's open work, not a decision to leave things
as they are.

1. **Anything raised before execution.** A denial or failure during macro
   expansion, `include`, or library import reaches the embedder as a
   `*wile.CompilationError`, and Scheme still cannot catch it: `(guard (e (#t …))
   (import (x)))` and `(guard (e (#t …)) (include "x"))` behave differently from
   `(guard (e (#t …)) (load "x"))`, because the guard is compiled after the
   failure that would have to reach it. That is the routing half, and it is
   unbuilt.

   The **representation** half has landed. `CompilationError.Condition` holds the
   same condition object a `guard` would have caught, built by the same
   `machine.ConditionFromError` the runtime bridge uses, so an analysis-time
   failure is no longer a different *kind* of thing to a host — only a differently
   *delivered* one. Read `Condition` for the message, irritants, kind and
   location as values; treat the absence of a Scheme-side handler as the open
   part.
2. **The VM's own cancellation poll.** When the engine's `context.Context` is
   cancelled or its deadline expires, `MachineContext.Run` returns
   `mc.ctx.Err()` directly. The host receives a `*wile.RuntimeError` for which
   `errors.Is(err, context.Canceled)` is true, and the Scheme program's `guard`
   never sees it. Deliberate for now, and separately owned; see
   [Blocking synchronization primitives and VM cancellation](../concurrency/cancellation.md)
   for what the surrounding machinery does and does not promise.
3. **Panics**, per the section above — contained, not converted, by design.

### Catchable is not delivered

Making a condition catchable does not guarantee a handler observes it. A handler
that invokes a continuation anywhere in its dynamic extent can currently swallow
the mandatory secondary exception, so a condition this contract makes catchable
may still be discarded without diagnostic:

```scheme
(with-exception-handler (lambda (e) (call/cc (lambda (k) (k 1))) 42)
                        (lambda () (car 5)))
;; returns 42 — the handler returned from a non-continuable raise and
;; the secondary exception was not raised
```

Until that counter is path-precise, "the program can catch it" is the strongest
claim this contract makes. It is not "the program will be told."

---

## Built-in Extensions

### Public Extensions (`extensions/`, `pkg/extensions/`)

These are importable by external Go code:

| Package | Library Name | Primitives |
|---------|-------------|------------|
| `pkg/extensions/io` | `(wile io)` | R7RS I/O: `read`, `write`, `display`, `newline`, string/bytevector ports, port state (41 primitives). Per-engine `State` via `AddNamespaceInit` |
| `extensions/math` | `(wile math)` | `exp`, `log`, `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `sqrt`, `expt`, `floor`, `ceiling`, `truncate`, `round`, `floor/`, `floor-quotient`, `floor-remainder`, `truncate/`, `truncate-quotient`, `truncate-remainder`, `finite?`, `infinite?`, `nan?`, `numerator`, `denominator`, `rationalize`, `exact-integer-sqrt`, `make-rectangular`, `make-polar`, `real-part`, `imag-part`, `magnitude`, `angle`, `number->string`, `string->number`, `inexact-with-accuracy`, `inexact-accuracy`, `inexact-lossless?`, `complex-inexact-with-accuracy` (39 primitives, plus the `pi` and `euler` global values) |
| `extensions/system` | `(wile system)` | `command-line`, `exit`, `emergency-exit`, `current-second`, `current-jiffy`, `jiffies-per-second` (6 primitives) |
| `extensions/files` | `(wile files)` | `open-input-file`, `open-output-file`, `open-binary-input-file`, `open-binary-output-file`, `file-exists?`, `delete-file`, `call-with-input-file`, `call-with-output-file`, `create-directory`, `delete-directory`, `directory-files`, `current-directory`, `set-current-directory!` (13 primitives) |
| `extensions/process` | `(wile process)` | `system`, `process-spawn`, `process-stdout`, `process-stderr`, `process-stdin`, `process-wait`, `process-kill`, `process?` (8 primitives) |
| `extensions/threads` | `(wile threads)` | SRFI-18 threading: `make-thread`, `thread-start!`, `thread-join!`, `thread-yield!`, `make-mutex`, `mutex-lock!`, `make-condition-variable`, `condition-variable-signal!`, etc. |
| `extensions/gointerop` | `(wile gointerop)` | Go concurrency: `make-atomic`, `atomic-load`, `atomic-store!`, `atomic-swap!`, `atomic-compare-and-swap!` |
| `extensions/introspection` | `(wile introspection)` | `environment?`, `interaction-environment`, `environment-bound-names`, `environment-ref`, `environment-bound?`, `features`, `available-libraries`, `disassemble` (8 primitives) |
| `extensions/eval` | `(wile eval)` | `eval`, `load`, `current-load-path`, `current-load-directory`, `current-load-depth`, `scheme-report-environment`, `null-environment`, `environment`, `expand`, `expand-once`, `compile`, `syntax-local-value`, `syntax-local-value/immediate`, `make-compile-time-value`, `syntax-local-introduce`, `syntax-local-identifier-as-binding` (16 primitives) |
| `extensions/charsets` | `(wile charsets)` | SRFI-14 character sets: `char-set?`, `char-set-contains?`, `char-set-size`, set algebra, named char-sets (20 primitives) |
| `extensions/sat` | `(wile sat)` | CDCL SAT kernel backing `(wile algebra sat)`: `sat-cnf-flat?`, `sat-cnf-flat-model` (2 primitives) |
| `extensions/algebragraph` | `(wile algebragraph)` | Graph kernels backing `(wile algebra graph)`: `count-paths-in-dag`, `count-paths-cyclic` (2 primitives) |

`extensions/algebra/graph` is a plain Go support package, not an extension: it
holds the algorithms `extensions/algebragraph` exposes.

### Internal Extensions (`pkg/internal/extensions/`)

Not importable by external code:

| Package | Purpose |
|---------|---------|
| `pkg/internal/extensions/envvars` | Environment-variable primitives: `get-environment-variable`, `get-environment-variables` (sandbox-aware) |
| `pkg/internal/extensions/namespace` | Namespace introspection and management: `make-namespace`, `namespace-derive`, `namespace-define!`, `namespace-ref`, etc. |
| `pkg/internal/extensions/all` | Records, promises, exceptions, strings, characters, and other R7RS primitives. Exports both `Extension` and a reduced `SafeExtension` (the `Console` profile's subset) |
| `pkg/internal/extensions/iotest` | Fault-injecting I/O ports, composed with `io` in tests only |

Extension registration is wired in `pkg/internal/bootstrap/bootstrap.go`
(`allExtensions` and `ProfileExtensions`), the first package that sees both
`pkg/registry` and `pkg/machine/compilation`. Its `ADDING A NEW EXTENSION`
comment is the authoritative checklist.

---

## Engine Options Reference

| Option | Description |
|--------|-------------|
| `WithExtension(ext)` | Add a single extension |
| `WithExtensions(ext...)` | Add multiple extensions |
| `WithProfile(p)` | Apply a named profile bundle: `Tiny`, `Console`, `ConsoleWithLoad`, `Small`, `KitchenSink` |
| `WithoutCore()` | Skip core primitives — creates a bare engine with only explicitly added extensions |
| `WithContractEnforcement()` | Validate arguments against declared `ParamTypes` before each call. Off by default; a correctness aid for extension authors |
| `WithLossyConversionsAllowed()` | Let `RegisterFunc` conversions truncate instead of raising `ErrLossyConversion` |
| `WithLibraryPaths(paths...)` | Enable R7RS library system with optional search paths |
| `WithStrictNamespace()` | Register the profile's extension primitives but withhold them from the top level, so `(import ...)` is the only path to them |
| `WithRegistry(reg)` | Use a custom registry (skips core primitives) |
| `WithMaxCallDepth(n)` | Set maximum VM recursion depth |
| `WithAuthorizer(auth)` | Set fine-grained runtime authorization policy (see [`sandboxing.md`](../security/sandboxing.md)) |
| `WithSandbox()` | Compose the sandbox env-prefix wrapper with the current authorizer |
| `WithEnv(k, v)`, `WithEnvMap(m)` | Install a virtual environment-variable map |

`WithProfile(KitchenSink)` matches the CLI's full extension set; `WithProfile(Console)` is the safe-by-default bundle (io with stdin/stdout/stderr, files restricted to `/tmp`, math, `all.SafeExtension`, charsets, and envvars) plus a matching `ConsoleAuthorizer`. The profile-to-extensions mapping has a single source of truth in `bootstrap.ProfileExtensions`. See [`sandboxing.md`](../security/sandboxing.md) for the full profile table.

---

## Thread Safety

- `Registry` is thread-safe for concurrent registration (uses `sync.RWMutex`).
- `Engine` is **not** safe for concurrent use. Each goroutine needs its own
  engine, or external synchronization.
- SRFI-18 threads within a single engine are safe — the VM handles coordination
  internally.

---

## Naming Conventions

| Convention | Example | Meaning |
|------------|---------|---------|
| Trailing `?` | `pair?`, `finite?` | Predicate (returns boolean) |
| Trailing `!` | `set-car!`, `string-fill!` | Mutator (side effect) |
| `->` | `number->string` | Type conversion |
| `%` prefix | `%make-lazy-promise` | Internal/private |

---

## Package Dependencies

Extensions should depend only on public packages:

```
extensions/myext
  ├── github.com/aalpar/wile/pkg/registry       ← Extension, Registry, PrimitiveSpec
  ├── github.com/aalpar/wile/pkg/machine        ← CallContext, ForeignFunction
  ├── github.com/aalpar/wile/pkg/values         ← Value types, type constraints
  ├── github.com/aalpar/wile/pkg/werr           ← Sentinels, WrapForeignErrorf
  ├── github.com/aalpar/wile/pkg/environment    ← EnvironmentFrame (AddNamespaceInit only)
  └── github.com/aalpar/wile/pkg/registry/helpers  ← Argument extraction, type conversion
```

Depend on `machine.CallContext`, not `*machine.MachineContext`: the interface is
the extension-facing surface, and `machine.RequireMachineContext` is the
documented escape hatch for the few primitives that need full VM internals.

No circular dependencies between extensions. Each is independently importable.
