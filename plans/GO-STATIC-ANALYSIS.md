# Go Static Analysis Extensions

**Status**: Phases 1-4 complete
**Foundation**: `extensions/goast/` (see `plans/GO-AST.md`)
**Dependencies**: `golang.org/x/tools v0.42.0` (already vendored) — `go/ssa`, `go/callgraph`, `go/cfg`, `go/analysis`

## Vision

Expose Go's compiler intermediate representations as Scheme s-expressions, building on the existing `(wile goast)` AST extension. Each IR layer adds a different kind of queryability:

| Layer | IR | What it answers |
|-------|----|----|
| `(wile goast)` | AST + types | "What is the shape of this code?" (syntax, structure) |
| `(wile goast ssa)` | SSA | "Where does this value come from? Where does it flow?" (data flow) |
| `(wile goast callgraph)` | Call graph | "Who calls whom? What's reachable?" (inter-procedural) |
| `(wile goast cfg)` | CFG + dominance | "Must this check happen before that return?" (control flow) |
| `(wile goast lint)` | Analysis passes | "What do existing analyzers report?" (pluggable diagnostics) |

Combined, these let you write custom static analyses in Scheme that span syntax, data flow, call structure, and control flow — queries that individually require writing dedicated Go analyzers with hundreds of lines of boilerplate.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Extension split | Separate Scheme libraries, separate Go packages | Pay for what you use; no forced SSA cost when you only need AST |
| Shared helpers | Exported from base `goast` package | `Node()`, `Field()`, `Str()`, `Sym()`, `ValueList()` exported from `extensions/goast/`; downstream extensions import `goast` for alist construction. Reflects real dependency: SSA needs typed ASTs, callgraph needs SSA. Survives extraction to separate modules. |
| Node format | Same tagged-alist `(tag (field . val) ...)` as goast | One query vocabulary for all layers; `walk`, `nf`, `tag?` work everywhere |
| Loading | Each primitive loads independently via `go/packages` | No opaque handles, no shared state between extensions, simple mental model |
| Cross-referencing | Position strings (`file:line:col`) | AST and SSA share the same `token.FileSet` coordinate system; correlation is string equality |
| No new `values.*` types | Standard Scheme lists, symbols, strings | Same as goast — no custom types needed |

## Extension Architecture

### Package Layout

```
extensions/
  goast/              ← existing: (wile goast) — also exports alist helpers
  goastssa/           ← new: (wile goast ssa) — imports goast for helpers
  goastcg/            ← new: (wile goast callgraph) — imports goastssa for SSA build
  goastcfg/           ← new: (wile goast cfg) — imports goastssa for SSA functions
  goastlint/          ← new: (wile goast lint) — standalone, uses go/analysis directly
```

Dependency chain (reflects computational reality):

```
goast ← goastssa ← goastcg
                 ← goastcfg
goast ← goastlint (independent of SSA)
```

**Extraction scenarios**: If all extensions move to one module (`wile-goast`), the import paths change but the structure is identical. If split into separate modules, each module depends on its upstream — `wile-goast-ssa` depends on `wile-goast`, `wile-goast-callgraph` depends on `wile-goast-ssa`. The dependency direction is honest: SSA requires typed ASTs, callgraph requires SSA.

### Library Names

Each extension implements `LibraryNamer` to produce hierarchical Scheme library names:

| Go package | Library name | Import |
|---|---|---|
| `extensions/goast` | `(wile goast)` | `(import (wile goast))` |
| `extensions/goastssa` | `(wile goast ssa)` | `(import (wile goast ssa))` |
| `extensions/goastcg` | `(wile goast callgraph)` | `(import (wile goast callgraph))` |
| `extensions/goastcfg` | `(wile goast cfg)` | `(import (wile goast cfg))` |
| `extensions/goastlint` | `(wile goast lint)` | `(import (wile goast lint))` |

### Shared Alist Helpers (exported from `goast`)

The existing unexported helpers in `extensions/goast/helpers.go` are promoted to exported:

```go
package goast

// Alist construction — used by goast mappers and downstream extensions (SSA, callgraph, etc.)
func Tag(name string) values.Value                              // symbol for node tag
func Field(key string, val values.Value) values.Value           // (key . val) pair
func Node(tag string, fields ...values.Value) values.Value      // (tag (k . v) ...)
func Str(s string) values.Value                                 // Scheme string
func Sym(s string) values.Value                                 // Scheme symbol
func ValueList(vs []values.Value) values.Value                  // proper list
func GetField(fields values.Value, key string) (values.Value, bool)
func RequireField(fields values.Value, nodeType, key string) (values.Value, error)
```

The goast mapper itself calls these directly (no wrappers — the unexported names are replaced). Downstream extensions import them:

```go
// extensions/goastssa/mapper.go
import "github.com/aalpar/wile/extensions/goast"

goast.Node("ssa-binop",
    goast.Field("name", goast.Str(instr.Name())),
    goast.Field("op", goast.Sym(instr.Op.String())),
)
```

### Loading Model

Each extension's primary primitive loads packages independently:

```
go-typecheck-package "pkg"  →  packages.Load + mapAST    → s-expression AST
go-ssa-build "pkg"          →  packages.Load + ssa.Build → s-expression SSA
go-callgraph "pkg" 'vta     →  packages.Load + vta.Graph → s-expression graph
go-cfg "pkg"                →  packages.Load + cfg.New   → s-expression CFG
```

Redundant loading across calls is a non-issue: `packages.Load` takes <1s for typical packages, and scripting tools optimize for simplicity over microseconds.

Cross-referencing between layers uses position strings. When both AST and SSA include `'positions`, the same source location produces the same `"file.go:42:5"` string, enabling joins:

```scheme
(define ast-nodes (go-typecheck-package "pkg" 'positions))
(define ssa-funcs (go-ssa-build "pkg" 'positions))
;; correlate by matching position strings
```

---

## High-Level Roadmap

### Phase 1: `(wile goast ssa)` — SSA / Data Flow  ✓ Complete (instruction mapping)

**See detailed design below.**

Exposes `go/ssa` as s-expressions. Enables data-flow queries: def-use chains, phi-node analysis, field store tracking, mutation independence checks.

**Key primitives**: `go-ssa-build` ✓, `go-ssa-operands` (not implemented — operands embedded in each instruction node), `go-ssa-referrers` (not implemented — achievable via Scheme-side tree walking)

**Deliverable**: The state-trace analysis from our prototype can answer "are these fields mutated independently?" — the gap identified in the current goast-only approach.

### Phase 2: `(wile goast callgraph)` — Call Graph  ✓ Complete

Exposes `go/callgraph` with algorithm selection. Builds on Phase 1 (call graph algorithms take `*ssa.Program` as input, so the SSA infrastructure is reused internally).

**Key primitives**:

| Primitive | Signature | Description |
|---|---|---|
| `go-callgraph` | `(go-callgraph pattern algorithm)` | Build call graph; `algorithm` is `'static`, `'cha`, `'rta`, or `'vta` |
| `go-callgraph-callers` | `(go-callgraph-callers graph func-name)` | All direct callers of a function |
| `go-callgraph-callees` | `(go-callgraph-callees graph func-name)` | All direct callees of a function |
| `go-callgraph-reachable` | `(go-callgraph-reachable graph root-name)` | Transitive closure from a root |

**S-expression encoding**:

```scheme
(callgraph
  (nodes . ((cg-node (name . "main") (pkg . "main")
              (edges-out . ((cg-edge (callee . "fmt.Println")
                              (pos . "main.go:10:2")
                              (site . "static"))))
              (edges-in . ()))))
```

**Estimated scope**: ~3 node types (graph, node, edge), 4-5 primitives. The mapper is straightforward — call graphs are flat node/edge structures.

### Phase 3: `(wile goast cfg)` — Control Flow Graph + Dominance  ✓ Complete

Exposes `go/cfg` for intra-procedural control flow and `go/ssa`'s dominator tree for path analysis.

**Key primitives**:

| Primitive | Signature | Description |
|---|---|---|
| `go-cfg` | `(go-cfg ssa-func)` | CFG for a single SSA function |
| `go-cfg-dominators` | `(go-cfg-dominators ssa-func)` | Dominator tree |
| `go-cfg-dominates?` | `(go-cfg-dominates? dom-tree block-a block-b)` | Does A dominate B? |
| `go-cfg-paths` | `(go-cfg-paths cfg from-block to-block)` | Enumerate paths between blocks |

**S-expression encoding**:

```scheme
(cfg-block (index . 0) (name . "entry")
  (succs . (1 2))    ;; successor block indices
  (preds . ())       ;; predecessor block indices
  (instrs . (...)))  ;; SSA instructions (same encoding as Phase 1)

(dom-node (block . 0)
  (children . (1 2 3))  ;; dominated blocks
  (idom . #f))          ;; immediate dominator (#f for entry)
```

**Enables**: "Does every path from entry to return pass through a security check?" — dominance queries.

**Estimated scope**: ~3 node types, 4 primitives. Small mapper; main complexity is in the path enumeration algorithm.

**Dependency**: Uses SSA function representation from Phase 1. Could share the SSA loading infrastructure rather than re-loading.

### Phase 4: `(wile goast lint)` — Analysis Passes  ✓ Complete

Exposes `go/analysis` framework, letting Scheme scripts invoke any registered analyzer and query its diagnostics and facts.

**Key primitives**:

| Primitive | Signature | Description |
|---|---|---|
| `go-analyze` | `(go-analyze pattern analyzer-names ...)` | Run named analyzers on a package |
| `go-analyze-list` | `(go-analyze-list)` | List available analyzer names |

**S-expression encoding**:

```scheme
((diagnostic
  (analyzer . "nilness")
  (pos . "server.go:42:5")
  (message . "nil dereference of x")
  (severity . "warning")
  (suggested-fix . #f))
 (diagnostic
  (analyzer . "shadow")
  (pos . "handler.go:18:2")
  (message . "declaration of err shadows declaration at handler.go:10:2")
  (severity . "warning")
  (suggested-fix . #f)))
```

**Available analyzers** (from `go/analysis/passes/`): ~40 including `nilness`, `shadow`, `copylock`, `loopclosure`, `errorsas`, `fieldalignment`, `bools`, `assign`, etc.

**Design question (deferred)**: Whether to expose analyzer *facts* (structured inter-package data) in addition to diagnostics. Facts are powerful but complex — each analyzer defines its own fact types. Diagnostics alone cover the common use case.

**Estimated scope**: ~2 node types (diagnostic, suggested-fix), 2 primitives. The complexity is in wiring up the analysis driver, not in the mapper.

---

## Phase 1 Detail: `(wile goast ssa)` — SSA Extension

### Overview

Expose Go's SSA intermediate representation as s-expressions. SSA (Static Single Assignment) is the compiler IR where every value has exactly one definition. This makes data-flow questions — "where does this value come from? what uses it? are these mutations coordinated?" — direct lookups rather than whole-program searches.

### Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Granularity | Per-function SSA, not whole-program | Matches `go/ssa` structure; users load specific packages |
| Value references | String names (`"t0"`, `"a"`) | SSA values have unique names within a function; strings are simple and `equal?`-comparable |
| Block references | Integer indices | Blocks are numbered 0..N within a function; integers are simple |
| Type annotations | Always present (`type` field on every value-producing instruction) | SSA values always have types; no reason to make it opt-in |
| Position annotations | Opt-in via `'positions` flag | Same convention as goast AST |
| Operand/referrer queries | Dedicated primitives, not embedded | Embedding all referrers in every node would bloat the tree; query on demand |

### Primitives

| Primitive | Signature | Security | Description |
|---|---|---|---|
| `go-ssa-build` | `(go-ssa-build pattern . options)` | `ResourceProcess`/`ActionLoad` | Load package, build SSA, return list of `ssa-func` nodes |
| `go-ssa-operands` | `(go-ssa-operands instr)` | None | Return operand value names for an instruction node |
| `go-ssa-referrers` | `(go-ssa-referrers func name)` | None | Find all instructions that use a named value in a function |

Options for `go-ssa-build`:
- `'positions` — include `(pos . "file:line:col")` on instructions
- `'synthetic` — include compiler-generated functions (init, wrappers)

### S-expression Encoding

#### Structural types

```scheme
;; Package-level: list of functions
((ssa-func ...)
 (ssa-func ...)
 ...)

;; Function
(ssa-func
  (name . "Add")
  (pkg . "github.com/example/pkg")
  (signature . "func(a int, b int) int")
  (params . ((ssa-param (name . "a") (type . "int"))
             (ssa-param (name . "b") (type . "int"))))
  (free-vars . ())   ;; captured variables for closures
  (locals . ())      ;; Alloc instructions promoted to names
  (blocks . ((ssa-block ...) ...)))

;; Basic block
(ssa-block
  (index . 0)
  (comment . "entry")          ;; human-readable label
  (preds . ())                 ;; predecessor block indices
  (succs . (1 2))              ;; successor block indices
  (instrs . ((ssa-binop ...) (ssa-if ...) ...)))
```

#### Value-producing instructions

Every instruction that produces a value has a `name` field (the SSA register name) and a `type` field.

```scheme
;; Arithmetic / comparison
(ssa-binop (name . "t0") (op . +) (x . "a") (y . "b") (type . "int"))
(ssa-unop (name . "t1") (op . -) (x . "t0") (type . "int"))
(ssa-unop (name . "t2") (op . !) (x . "cond") (type . "bool"))

;; Function call
(ssa-call (name . "t3")
  (func . "fmt.Println")    ;; resolved callee name
  (args . ("t0" "t1"))
  (type . "(n int, err error)"))

;; Method call
(ssa-call (name . "t4")
  (func . "(*bytes.Buffer).Write")
  (recv . "buf")
  (args . ("data"))
  (type . "(n int, err error)"))

;; Phi node — value depends on which predecessor block we came from
(ssa-phi (name . "t5")
  (edges . ((0 . "a") (1 . "b")))  ;; (block-index . value-name)
  (type . "int"))

;; Struct field address (key for state-trace)
(ssa-field-addr (name . "t6") (x . "p") (field . "Handled")
  (field-index . 3) (type . "*bool"))

;; Memory operations
(ssa-alloc (name . "t7") (type . "*int") (heap . #t))  ;; heap vs stack
(ssa-store (addr . "t6") (val . "t8"))                  ;; no name (void)
(ssa-load (name . "t9") (addr . "t6") (type . "bool"))  ;; ssa.UnOp with *

;; Constants
(ssa-const (name . "0:int") (value . "0") (type . "int"))

;; Type operations
(ssa-type-assert (name . "t10") (x . "iface") (assert-type . "*Foo")
  (comma-ok . #t) (type . "(*Foo, bool)"))
(ssa-make-interface (name . "t11") (x . "concrete") (type . "io.Reader"))
(ssa-change-type (name . "t12") (x . "t0") (type . "MyInt"))

;; Closures
(ssa-make-closure (name . "t13")
  (func . "pkg.funcName$1")
  (bindings . ("captured1" "captured2"))
  (type . "func()"))

;; Extract (from multi-valued call)
(ssa-extract (name . "t14") (tuple . "t3") (index . 0) (type . "int"))
```

#### Non-value instructions (control flow, side effects)

These have no `name` field — they don't produce values.

```scheme
;; Control flow
(ssa-jump (target . 3))                      ;; unconditional branch
(ssa-if (cond . "t2") (then . 1) (else . 2))  ;; conditional branch
(ssa-return (results . ("t0")))
(ssa-panic (x . "t5"))

;; Side effects
(ssa-store (addr . "t6") (val . "t8"))
(ssa-map-update (map . "m") (key . "k") (value . "v"))
(ssa-send (chan . "ch") (x . "t0"))
(ssa-go (call . (ssa-call ...)))             ;; goroutine
(ssa-defer (call . (ssa-call ...)))          ;; deferred call
(ssa-run-defers)                             ;; execute deferred calls
```

#### Globals

```scheme
(ssa-global (name . "pkg.ErrNotFound") (type . "*error"))
```

### Mapper Architecture

#### Go-side structure

```go
// extensions/goastssa/mapper.go

type ssaMapper struct {
    fset      *token.FileSet
    positions bool
}

func (p *ssaMapper) mapFunction(fn *ssa.Function) values.Value
func (p *ssaMapper) mapBlock(b *ssa.BasicBlock) values.Value
func (p *ssaMapper) mapInstruction(instr ssa.Instruction) values.Value
func (p *ssaMapper) mapValue(v ssa.Value) values.Value  // for operand references
```

`mapInstruction` dispatches on `ssa.Instruction` type (type switch over ~35 concrete types). Each case extracts fields and builds a tagged alist using exported helpers from the base `goast` package.

**Value references**: SSA values are referenced by their `.Name()` string. The mapper does NOT embed the full definition at every use site — instead, instructions reference operands by name. The `go-ssa-referrers` primitive provides the reverse lookup.

**DebugRef mapping**: `ssa.DebugRef` instructions link SSA values back to AST expressions. When `'positions` is set, the mapper includes these as `(ssa-debug-ref (value . "t0") (pos . "file:10:5") (expr . "x + y"))` nodes, enabling AST ↔ SSA correlation.

#### Package structure

```
extensions/goastssa/
  doc.go              # Package documentation
  register.go         # Extension registration, LibraryNamer → (wile goast ssa)
  prim_ssa.go         # Primitive implementations
  mapper.go           # SSA → s-expression mapper
  mapper_test.go      # Mapper tests
  prim_ssa_test.go    # Primitive tests
```

### Security

`go-ssa-build` requires the same authorization as `go-typecheck-package`:

```go
security.Check(mc.Context(), security.AccessRequest{
    Resource: security.ResourceProcess,
    Action:   security.ActionLoad,
    Target:   "go",
})
```

`go-ssa-operands` and `go-ssa-referrers` operate on s-expression data only — no security gate.

### Error Handling

Extension-local sentinels:

```go
var (
    errSSABuildError = werr.NewStaticError("ssa build error")
)
```

### Sub-phases

#### Sub-phase 1A: Core instructions ✓ Complete (~18 types)

Enough for data-flow analysis on typical Go code: arithmetic, calls, field access, memory, control flow.

| Category | SSA types |
|---|---|
| Values | `Const`, `Parameter`, `FreeVar`, `Global` |
| Arithmetic | `BinOp`, `UnOp` |
| Calls | `Call` (static + method + dynamic) |
| Memory | `Alloc`, `Store`, `FieldAddr`, `IndexAddr`, `Field`, `Index` |
| Control | `Phi`, `If`, `Jump`, `Return` |
| Structure | `Function`, `BasicBlock` |

**Deliverable**: `go-ssa-build`, `go-ssa-operands`, `go-ssa-referrers` working on core instruction set. State-trace "mutation independence" query is possible.

#### Sub-phase 1B: Collections + concurrency ✓ Complete

| Category | SSA types |
|---|---|
| Maps | `MakeMap`, `MapUpdate`, `Lookup` |
| Slices | `MakeSlice`, `Slice` |
| Channels | `MakeChan`, `Send`, `Select`, `SelectState` |
| Goroutines | `Go`, `Defer`, `RunDefers` |
| Iteration | `Range`, `Next` |
| Panic | `Panic` |

#### Sub-phase 1C: Type operations + closures ✓ Complete

| Category | SSA types |
|---|---|
| Type conversion | `ChangeType`, `Convert`, `MultiConvert`, `ChangeInterface`, `SliceToArrayPointer` |
| Interfaces | `MakeInterface`, `TypeAssert` |
| Closures | `MakeClosure` |
| Tuple extract | `Extract` |
| Debug | `DebugRef` |

### Testing Strategy

#### Mapper tests (`mapper_test.go`)

Table-driven, one entry per instruction type:

1. Construct SSA programmatically (using `ssautil.CreateProgram` or `ssatest` helpers)
2. Map to s-expression
3. Verify structure (tag, expected fields, value names, types)

For instructions that require full programs (phi nodes, control flow), use `go-ssa-build` on inline Go source.

#### Primitive tests (`prim_ssa_test.go`)

External test package. Engine loads `goastssa.Extension`:

1. Build SSA from known Go source string, verify function list
2. Query operands of a known instruction, verify value names
3. Query referrers of a known value, verify instruction list
4. Error cases: invalid pattern, non-existent package

#### Integration tests

Scheme-side scripts exercising the SSA extension on real code:

1. State-trace mutation independence check (the motivating use case)
2. "Find all functions that call fmt.Errorf" via SSA Call instructions
3. "Find all field stores to .phase across a package"

#### Completion criteria

- All instruction types in sub-phase have mapper coverage
- All primitives have happy-path + error-path tests
- At least one integration test per sub-phase
- `make lint && make covercheck` pass

---

## Cross-referencing Between Layers

All layers share the same position coordinate system (`token.FileSet` → `"file:line:col"` strings). When `'positions` is requested, every node that has a source location includes a `(pos . "file:line:col")` field.

Cross-referencing flow:

```
AST node at pos "server.go:42:5"
    ↕ position string equality
SSA instruction at pos "server.go:42:5"
    ↕ SSA function identity (name + package)
Call graph node for that function
    ↕ block index
CFG block containing that instruction
    ↕ dominance relationship
Dominator tree queries
```

Scheme-side correlation:

```scheme
;; Find the AST node and SSA instruction for the same source location
(define (correlate-ast-ssa ast-file ssa-func pos)
  (let ((ast-node (walk ast-file
                    (lambda (n) (and (equal? (nf n 'pos) pos) n))))
        (ssa-instr (walk ssa-func
                     (lambda (n) (and (equal? (nf n 'pos) pos) n)))))
    (list ast-node ssa-instr)))
```

---

## Dependencies

### New Go dependencies

None. `golang.org/x/tools v0.42.0` already vendored — all required packages (`go/ssa`, `go/callgraph/*`, `go/cfg`, `go/analysis`) are included.

### Impact on existing code

- `extensions/goast/helpers.go`: unexported helpers (`tag`, `field`, `node`, `str`, `sym`, `valueList`) renamed to exported forms (`Tag`, `Field`, `Node`, `Str`, `Sym`, `ValueList`). Internal callers updated.
- `extensions/goast/mapper.go`: calls updated to exported names. No behavioral change.
- `extensions/goast/unmapper*.go`: calls to `getField`/`requireField` updated to `GetField`/`RequireField`.
- Public API surface grows by ~8 exported functions in the `goast` package. These are stable — the alist encoding is a foundational design decision.
