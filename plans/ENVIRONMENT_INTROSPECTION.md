# Plan: Read-Only Environment Introspection Primitives

## Context

Wile exposes environments as first-class values (`TopLevelEnvironment` implements `values.Value`),
but provides no Scheme-level API to inspect their contents. The Go side has `Keys()`, `Bindings()`,
`GetBinding()` — none of it reachable from Scheme. This adds 4 read-only introspection primitives
following MIT Scheme naming conventions, informed by prior art from MIT Scheme, Chez Scheme, and Guile.

Part of the broader **reflection features** initiative.

## Prior Art

### MIT Scheme (most complete)
- `environment?`, `environment-has-parent?`, `environment-parent`
- `environment-bound-names`, `environment-macro-names`, `environment-bindings`
- `environment-lookup`, `environment-assigned?`, `environment-lookup-macro`
- `environment-define`, `environment-assign!`, `environment-definable?`

### Chez Scheme (pragmatic)
- `environment?`, `environment-symbols`, `copy-environment`
- `top-level-value`, `top-level-bound?`
- `define-top-level-value`, `set-top-level-value!`

### Guile (module-centric)
- Environments ≈ modules; `resolve-module`, `resolve-interface`
- `module-obarray`, `variable-ref`, `variable-set!`

### Design choices
- MIT-style naming: `environment-*` (most established convention)
- Read-only first cut (no mutation)
- All binding types in a single list (no separate macro-names)

## Primitives (Phase 1 — Read-Only)

| Primitive | Params | Returns |
|-----------|--------|---------|
| `environment?` | 1 (obj) | `#t` if obj is an environment |
| `environment-bound-names` | 1 (env) | List of all bound symbols (variables, syntax, primitives) |
| `environment-ref` | 2 (env, symbol) | Value bound to symbol; error if unbound |
| `environment-bound?` | 2 (env, symbol) | `#t` if symbol is bound |

All registered at `PhaseRuntime` (same as existing eval primitives).

## Files to Modify

### 1. `internal/extensions/eval/prim_eval.go` — Add 4 implementations

Append after `PrimSyntaxLocalIdentifierAsBinding` (end of file):

- **`PrimEnvironmentQ`**: Use `helpers.MakeTypePredicate` checking `*environment.TopLevelEnvironment`.
  Follows identical pattern to `PrimBooleanQ`, `PrimStringQ` in `registry/core/prim_predicates.go`.

- **`PrimEnvironmentBoundNames`**: Type-assert arg → `*TopLevelEnvironment`, call
  `topLevelEnv.Runtime().GlobalEnvironment().Keys()`, iterate the `map[values.Symbol]int`,
  cons each `*values.Symbol` onto an accumulator starting from `values.EmptyList`.
  Order is non-deterministic (map iteration) — MIT Scheme doesn't guarantee order either.

- **`PrimEnvironmentRef`**: Type-assert env arg, require `*values.Symbol` for second arg,
  intern via `env.InternSymbol(sym)`, call `env.GetBinding(sym)`. Return `binding.Value()`.
  Error with `ErrNoSuchBinding` if nil.

- **`PrimEnvironmentBoundQ`**: Same as ref but return `BoolToBoolean(binding != nil)` instead
  of the value. No error on unbound — just `#f`.

No new imports needed — `environment`, `values`, `helpers` already imported.

### 2. `internal/extensions/eval/register.go` — Add 4 specs

Insert after the `"environment"` spec (line ~49) in `addPrimitives()`:

```go
{Name: "environment?", ParamCount: 1, Impl: PrimEnvironmentQ,
    Doc: "Returns #t if the argument is an environment.", ParamNames: []string{"obj"}, Category: "eval"},
{Name: "environment-bound-names", ParamCount: 1, Impl: PrimEnvironmentBoundNames,
    Doc: "Returns a list of all symbols bound in the environment.", ParamNames: []string{"env"}, Category: "eval"},
{Name: "environment-ref", ParamCount: 2, Impl: PrimEnvironmentRef,
    Doc: "Returns the value bound to a symbol in the environment.", ParamNames: []string{"env", "symbol"}, Category: "eval"},
{Name: "environment-bound?", ParamCount: 2, Impl: PrimEnvironmentBoundQ,
    Doc: "Returns #t if the symbol is bound in the environment.", ParamNames: []string{"env", "symbol"}, Category: "eval"},
```

### 3. `registry/core/prim_env_extra_test.go` — Add tests

Tests go here (alongside existing `TestNullEnvironment`, `TestSchemeReportEnvironment`,
`TestEvalWithEnvironments`). Uses `runSchemeCode(t, code)` and `runSchemeCodeExpectError(t, code)`
from `test_helpers_test.go`. Package is `core_test`.

**Table-driven test structure** (per project convention):

- `TestEnvironmentQ` — success table: `(environment? (interaction-environment))` → `#t`,
  `(environment? 42)` → `#f`, `(environment? "hello")` → `#f`; error table: wrong arity

- `TestEnvironmentBoundNames` — success table: `(pair? (environment-bound-names (interaction-environment)))` → `#t`,
  `(environment-bound-names (environment))` → `()`,
  elements are symbols; error table: wrong type, wrong arity

- `TestEnvironmentRef` — success: look up primitive `(procedure? (environment-ref (interaction-environment) '+))` → `#t`;
  error table: unbound symbol, wrong env type, wrong symbol type, wrong arity

- `TestEnvironmentBoundQ` — success table: `(environment-bound? (interaction-environment) '+)` → `#t`,
  `(environment-bound? (environment) 'x)` → `#f`;
  error table: wrong types, wrong arity

## Design Decisions

- **`environment-bound-names` includes all binding types** (variables, syntax, primitives) in one list.
  Callers can filter with `environment-ref` if needed.
- **`environment-ref` traverses the parent chain** via `GetBinding()`. For `TopLevelEnvironment`s
  the runtime frame has no parent (by construction), so this is equivalent to checking globals only,
  but is more correct if the architecture ever changes.
- **Symbol interning before lookup** is critical — `env.InternSymbol(sym)` ensures the Scheme-side
  symbol matches the definition-site interning.
- **No mutation primitives** in this cut. `environment-define!`, `environment-set!` deferred to future work.

## Go Infrastructure Already Available

```
Scheme primitive          →  Go path
──────────────────────────────────────────────────────────────
environment?              →  type assertion on *TopLevelEnvironment
environment-bound-names   →  topLevel.Runtime().GlobalEnvironment().Keys()
environment-ref           →  topLevel.Runtime().GetBinding(sym).Value()
environment-bound?        →  topLevel.Runtime().GetBinding(sym) != nil
```

## Future Extensions

Phase 2 (mutation):
- `environment-define!` — create new binding
- `environment-set!` — modify existing binding

Phase 3 (navigation/metadata):
- `environment-has-parent?` — check parent existence
- `environment-parent` — navigate parent chain
- `environment-macro-names` — syntax bindings only
- `environment-assigned?` — bound vs assigned distinction
- `environment-copy` — deep copy

## Verification

```bash
make lint
go test -v -run 'TestEnvironment' ./registry/core/...
make test
```

## Sources

- [MIT Scheme Environment Operations](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Environment-Operations.html)
- [MIT Scheme Environment Variables](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Environment-Variables.html)
- [Chez Scheme System Operations (CSUG 9.5)](https://cisco.github.io/ChezScheme/csug9.5/system.html)
- [Guile Module System Reflection](https://www.gnu.org/software/guile/manual/html_node/Module-System-Reflection.html)
