# Extension Contracts Phase 2+ Design

**Date**: 2026-03-26
**Status**: Draft
**Depends on**: PR #577 (Phase 1 infrastructure + Phase 2 core annotations)

## Problem

Phase 1 shipped the `ValueType` enum, `PrimitiveSpec` fields (`ParamTypes`, `ReturnType`),
and `,doc` formatting. Phase 2 annotated all ~172 core primitives in `registry/core/`. But:

1. **228 extension primitives have no annotations.** Seven public and four internal extension
   packages register primitives without type contracts. `,doc` shows no type information for
   any extension primitive.
2. **No enforcement exists.** Annotations are metadata only. There is no mechanism to validate
   arguments against declared contracts at any layer — no way to verify annotations are correct
   except by reading code.

## Goals

1. Add `ParamTypes`/`ReturnType` to all 228 extension primitives (documentation).
2. Build opt-in runtime enforcement as a dry-run tool (verify annotations match reality).
3. Document the annotation threading so future consumers (wile-goast, linter) know where
   contract data lives.

## Consumer Hierarchy

Annotations serve three consumers in priority order:

| Priority | Consumer | Purpose | When |
|----------|----------|---------|------|
| 1 | `,doc` / MCP tools | Documentation at boundaries | Always (annotations are metadata) |
| 2 | `WithContractEnforcement()` | Dry-run validation | Opt-in, verify annotations match reality |
| 3 | wile-goast | Static analysis of call sites | Future, query contracts at analysis time |

Runtime enforcement is **not a production feature**. It is a testing tool: enable it, run
the test suite, confirm no annotation is wrong. The primary value of annotations is
documentation — making types at extension boundaries explicit for callers (including LLMs).

## Architecture

### Annotation Data Flow

Annotations flow through two independent paths depending on the consumer.

#### Documentation Path (always active)

```
                           Registration
                           ┌─────────────────────────────┐
  Extension                │         Registry             │
  register.go              │                              │
 ┌──────────────┐          │  ┌────────────────────────┐  │
 │ PrimitiveSpec │─────────►│  │ PrimitiveRegistration  │  │
 │  .ParamTypes  │  AddPrim │  │  .Spec.ParamTypes      │  │
 │  .ReturnType  │          │  │  .Spec.ReturnType      │  │
 └──────────────┘          │  └───────────┬────────────┘  │
                           │              │               │
                           └──────────────┼───────────────┘
                                          │
                                          │ LookupDoc()
                                          ▼
                           ┌─────────────────────────────┐
                           │   RegistryDocProvider        │
                           │                              │
                           │  ┌────────────────────────┐  │
                           │  │ DocInfo                 │  │
                           │  │  .ParamTypes            │  │
                           │  │  .ReturnType            │  │
                           │  └───────────┬────────────┘  │
                           └──────────────┼───────────────┘
                                          │
                                          │ formatPrimitiveDoc()
                                          ▼
                           ┌─────────────────────────────┐
                           │  ,doc string-ref             │
                           │  (string-ref s k) -> char    │
                           │    s : string                │
                           │    k : exact integer         │
                           └─────────────────────────────┘
```

Annotations stay in the `Registry`. The `,doc` command reads them at query time via
`RegistryDocProvider.LookupDoc()`. No annotations reach the execution context.

#### Enforcement Path (opt-in via WithContractEnforcement)

```
                  Registration (engine startup)
 ┌──────────────┐
 │ PrimitiveSpec │
 │  .ParamTypes  │
 └──────┬───────┘
        │
        │ buildValidator(spec)
        │ (captures cloned []ValueType)
        ▼
 ┌──────────────────┐
 │ validator closure │──── func(*MachineContext) error
 └──────┬───────────┘
        │
        │ closure.SetValidator(...)
        ▼
 ┌──────────────────┐      stored in environment
 │ ForeignClosure    │      as global binding
 │  .fn   = PrimFoo  │
 │  .validate = [closure]│
 └──────┬───────────┘
        │
        │              Execution (VM dispatch)
        │
        │  callForeignCached / applyForeign
        ▼
 ┌──────────────────────────────────┐
 │ 1. drain stack                   │
 │ 2. check arity                   │
 │ 3. bind args to env              │
 │ 4. fcls.validate(mc)  ◄── HERE   │
 │ 5. fcls.fn(mc)                   │
 └──────────────────────────────────┘
        │
        │ inside validator:
        ▼
 ┌──────────────────────────────────┐
 │ for each arg:                    │
 │   ValueType.Check(mc.Arg(i))    │
 │     -> Go type assertion         │
 │     -> (narrowed, ok, error)     │
 └──────────────────────────────────┘
```

The `PrimitiveSpec` is consumed at registration time. The validator closure is the only
artifact that carries type information into the execution context. When enforcement is off
(the default), `ForeignClosure.validate` is nil and the nil check in the dispatch path
is skipped.

### Key Boundary: Registration Consumes, Runtime Queries

```
 registry/           machine/             values/
 ┌───────────┐       ┌───────────────┐    ┌──────────────┐
 │ PrimitiveSpec      │ ForeignClosure │    │ ValueType    │
 │  .ParamTypes──┐    │  .validate ────┼───►│  .Check()    │
 │  .ReturnType  │    │  .fn           │    │  .String()   │
 └───────────────┘    └───────────────┘    │  .Description│
       │   ▲               ▲               └──────────────┘
       │   │               │
       │   │ LookupDoc()   │ SetValidator()
       │   │               │
       │  ┌┴──────────┐   ┌┴──────────────┐
       │  │DocProvider │   │buildValidator()│
       │  └───────────┘   └───────────────┘
       │
       │ AddPrimitives()
       ▼
 ┌───────────┐
 │ Registry   │ ── stores specs for doc queries
 └───────────┘
```

- `registry/` owns `PrimitiveSpec` and builds validators from it
- `machine/` owns `ForeignClosure` and calls validators in the dispatch path
- `values/` owns `ValueType` and the `Check()` predicate
- No new import edges — `registry/` already imports both `machine/` and `values/`

## Design Decisions

### ForeignClosure.validate Field

Add `validate ForeignFunction` to `ForeignClosure` (nil = no validation).

```go
type ForeignClosure struct {
    fn         ForeignFunction
    validate   ForeignFunction   // nil = no validation
    env        *environment.EnvironmentFrame
    paramCount int
    isVariadic bool
    name       string
}
```

`SetValidator(v ForeignFunction)` sets the field. Same type as `fn` — no new types needed.
The validator has access to `mc.Arg(i)` because it runs after `bindArgs`.

### Dispatch Path Changes

In `callForeignCached` and `applyForeign`, insert before `fcls.fn(mc)`:

```go
if fcls.validate != nil {
    if err := fcls.validate(mc); err != nil {
        return nil, applyCallableError(mc, err)
    }
}
```

Cost when enforcement is off: one nil check per call (branch prediction: always not-taken).

### Validator Builder

```go
// registry/contract.go
func buildValidator(spec PrimitiveSpec) ForeignFunction {
    if len(spec.ParamTypes) == 0 {
        return nil
    }
    types := slices.Clone(spec.ParamTypes)
    name := spec.Name
    isVariadic := spec.IsVariadic
    return func(mc *machine.MachineContext) error {
        argc := mc.ArgCount()
        for i := 0; i < argc; i++ {
            vt := paramTypeForContract(types, i, isVariadic)
            if vt == values.TypeAny {
                continue
            }
            _, ok, err := vt.Check(mc.Arg(i))
            if !ok {
                return werr.WrapForeignErrorf(err, "%s: argument %d", name, i)
            }
        }
        return nil
    }
}
```

Captures cloned `types`, `name`, `isVariadic`. Returns nil for uncontracted specs.

### Engine Option

```go
func WithContractEnforcement() EngineOption
```

Propagated as a bool through `Registry.Apply()` to `registerRuntimePrimitive` and
`registerExpandTimePrimitive`. When true, `buildValidator(spec)` is called and installed
via `SetValidator`. Same treatment for `engine.go:RegisterPrimitive`.

### RequireArg Unchanged

`RequireArg[T]` inside `Prim*` functions stays as-is. When enforcement is off (default),
it is the only type check. When enforcement is on (dry-run), it is redundant but harmless.
No changes to existing `Prim*` implementations.

## Rollout

### PR 1: Enforcement Infrastructure + Files Extension

Proves the full stack end-to-end with the smallest extension package.

1. `ForeignClosure.validate` field + `SetValidator`
2. Dispatch path changes in `callForeignCached` and `applyForeign`
3. `buildValidator` in `registry/contract.go`
4. `WithContractEnforcement()` engine option, bool through `Apply`
5. Annotate `extensions/files/register.go` (13 primitives)
6. Tests: validator unit tests, enforcement integration tests, `,doc` verification

### Subsequent PRs: Remaining Extension Packages

One PR per logical group. Each is mechanical: read `Prim*` implementations, add
`ParamTypes`/`ReturnType`, run tests.

| Package | Primitives | Notes |
|---------|-----------|-------|
| `extensions/math/` | 35 | Numeric tower types, some variadic |
| `extensions/threads/` | 30 | Thread/mutex/condition types → TypeAny |
| `extensions/gointerop/` | 33 | Go types → TypeAny for most |
| `extensions/system/` | 8 | Mixed |
| `extensions/process/` | 8 | Mixed |
| `extensions/introspection/` | 6 | Reflection → TypeProcedure params |
| `internal/extensions/io/` | 41 | Port types, current-*-port parameters |
| `internal/extensions/eval/` | 16 | Mixed |
| `internal/extensions/namespace/` | 10 | Mixed |
| `internal/extensions/all/` | 28 | Records, promises, core derived forms |

### Process Per File

1. Read each `Prim*` implementation — verify actual `RequireArg` / type assertion calls
2. Add `ParamTypes` and `ReturnType` to each spec
3. Run `go test -v ./path/to/package/...`
4. Run `make lint && make covercheck`

## Future Directions (Out of Scope)

- **wile-goast integration**: Query `Registry` for contract data, check Scheme call sites
  against declared types at analysis time.
- **RequireArg deprecation**: Only after all primitives are contracted AND enforcement is
  proven reliable. Gated on removing the opt-in flag.
- **Compile-time checking in validate package**: Requires type inference or at minimum
  literal-type tracking. Separate feature, separate design.
