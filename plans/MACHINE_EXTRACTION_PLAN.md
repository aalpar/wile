# Machine Package Extraction — Research Plan

**Status:** Research / Deferred
**Origin:** Staff Review P3 item (February 2026)
**Decision:** Defer until coupling hotspots are resolved

---

## Current Structure

The `machine/` package contains 131 files (78 source, 53 test) totaling ~14.5K source lines and ~21.5K test lines.

### File Groups

| Group | Files | Lines | % | Largest File |
|-------|-------|-------|---|-------------|
| Compilation (`compile_*`) | 14 | 5,669 | 39% | `compile_time_continuation.go` (1,036) |
| Operations (`operation_*`) | 52 | 2,518 | 17% | `operation_foreign_function_call.go` (89) |
| Expansion (`expander_*`) | 5 | 1,569 | 11% | `expander_time_continuation.go` (1,047) |
| VM Runtime | 4 | 1,284 | 9% | `machine_context.go` (1,084) |
| Infrastructure | ~10 | ~700 | 8% | `native_template.go` (189) |
| Libraries | 4 | 619 | 4% | `library.go` (232) |
| Continuation/Control | ~8 | ~600 | 5% | `exception_escape.go` (184) |

### Key Types

```
MachineContext      — VM execution state, Run() loop, continuation stack
NativeTemplate      — Immutable compiled bytecode (operations + constants)
MachineContinuation — Captured VM state for continuations
ForeignFunction     — func(context.Context, *MachineContext) error
ForeignClosure      — ForeignFunction + metadata (name, arity, variadic)
SyntaxCompiler      — Dispatches compilation of special forms
```

---

## Candidate Subpackage Boundaries

### Tier 1: Low Risk

**`machine/library`** — R7RS library support (4 files, 619 lines)

```
library.go              232 lines
library_loader.go       189 lines
library_load_path.go     93 lines
library_types.go        105 lines
```

- Self-contained: defines `Library`, `LibraryLoader`, `LibraryRegistry`
- Depends on: `MachineContext` (for evaluation), `NativeTemplate`
- Depended on by: `compile_*` (library imports), `expander_*` (library resolution)
- **Risk:** Would need interface for `MachineContext` evaluation capability
- **Circular dependency risk:** Low — library loader calls into compiler/expander but could receive them as injected dependencies

**`machine/operations`** — Bytecode instruction implementations (52 files, 2,518 lines)

- Each operation implements `Apply(ctx, mc) (*MachineContext, error)`
- All depend solely on `MachineContext` — no cross-references between operations
- **Risk:** Operations are created by the compiler and consumed by the VM. If both stay in `machine/`, extraction is trivial. If compiler moves out, circular dependency.
- **Circular dependency risk:** Low if compiler stays in `machine/`

### Tier 2: Medium Risk

**`machine/compiler`** — Compilation pipeline (14 files, 5,669 lines)

- Transforms validated syntax to `NativeTemplate` bytecode
- 39% of the package — largest group
- Depends on: `MachineContext` (sub-contexts), operations (creates them), `NativeTemplate`
- **Risk:** Compiler creates operation objects. If operations are also extracted, this creates `compiler → operations` AND `operations → compiler` (for `OperationSaveContinuationOffsetImmediate` which references compilation)
- **Circular dependency risk:** Medium — bidirectional with operations

**`machine/expander`** — Macro expansion (5 files, 1,569 lines)

- Fairly self-contained expansion driver
- Depends on: compiler (for `syntax-case` fender compilation), `MachineContext`
- **Risk:** Expander invokes compiler for fender expressions; compiler invokes expander for macro expansion
- **Circular dependency risk:** Medium — bidirectional with compiler

### Tier 3: High Risk

**Continuation/Control group** — Exception handling, dynamic-wind, prompts

```
exception_escape.go           184 lines
machine_continuation.go       157 lines
dynamic_wind.go               ~80 lines
composable_continuation.go    ~100 lines
prompt.go                     ~80 lines
```

- **Mutual coupling:** Exception ↔ dynamic-wind ↔ continuation have cross-references
- Cannot be separated from each other or from `MachineContext`
- **Circular dependency risk:** High — these are tightly interwoven with VM state

---

## Coupling Analysis

### Internal Coupling Matrix

```
                    VM    Compiler  Expander  Operations  Library  Control
VM (MachineContext)  —      ←         ←          ←          ←        ←
Compiler             →      —         ↔          →          →        →
Expander             →      ↔         —          →          →
Operations           →                           —
Library              →      ←         ←                     —
Control              →                                               —

→ = depends on    ← = depended on by    ↔ = bidirectional
```

**Central hub:** `MachineContext` is imported by every group. Any extraction must either:
1. Keep `MachineContext` in the parent package, or
2. Define interfaces that abstract it

### External Consumers

48+ files across the codebase import `machine/`:

| Package | Key imports |
|---------|-------------|
| `registry/core/*` | `ForeignFunction`, `MachineContext` |
| `internal/extensions/*` | `ForeignFunction`, `MachineContext`, `ForeignClosure` |
| `wile/` (root) | `MachineContext`, `NativeTemplate`, compilation pipeline |
| `registry/` | `ForeignFunction` type only |

The primary external API surface is `ForeignFunction` and `MachineContext` — these cannot move without breaking all consumers.

---

## Circular Dependency Risks

### Risk 1: Compiler ↔ Operations

The compiler creates operation objects (`OperationLoad`, `OperationBranch`, etc.). If both are in separate packages, the compiler imports operations. But `OperationSaveContinuationOffsetImmediate` calls back into `MachineContext.SaveContinuation()`, which is part of the VM core. This is manageable as long as `MachineContext` stays in a shared location.

### Risk 2: Compiler ↔ Expander

The compiler invokes the expander for macro expansion during compilation. The expander invokes the compiler to compile fender expressions in `syntax-case`. Breaking this cycle requires an interface boundary or callback injection.

### Risk 3: Control ↔ VM

`MachineContext` directly implements continuation save/restore, dynamic-wind tracking, exception handler chains, and prompt tags. These cannot be separated from `MachineContext` without major refactoring.

---

## Prerequisites for Safe Extraction

1. **Define `MachineContext` interface** — Extract the subset of `MachineContext` methods that each group needs into interfaces. This is the single largest prerequisite and would touch every operation and every primitive.

2. **Break compiler ↔ expander cycle** — Introduce callback injection: compiler receives an `Expander` interface, expander receives a `Compiler` interface. Both live in a shared types package.

3. **Stabilize operation API** — Currently operations are concrete structs created by the compiler. An operation registry (factory pattern) would break the direct import.

4. **Move `ForeignFunction`/`ForeignClosure` to a types package** — These are the most-imported types from `machine/`. Moving them to a leaf package (no dependencies) would reduce coupling.

---

## Recommendation

**Defer extraction.** The current single-package structure works. The file count (131) is high but manageable with clear naming conventions. The coupling analysis shows that extraction would require significant interface scaffolding for modest organizational benefit.

**Revisit when:**
- `machine/` exceeds ~200 files
- A second consumer of the compilation pipeline appears (e.g., AOT compiler, LSP server)
- The compiler or expander needs independent versioning

**Incremental steps that would make future extraction easier (no urgency):**
1. Move `ForeignFunction`/`ForeignClosure` to a leaf types package
2. Extract library support behind an interface
3. Define narrower `MachineContext` interfaces per consumer group
