# Go Static Analysis with Scheme

Wile includes five extension libraries that expose Go's compiler intermediate
representations as Scheme s-expressions. Together they enable ad-hoc, cross-layer
static analysis of Go source code — queries that would otherwise require writing
custom `go/analysis` passes in Go.

## Extension Libraries

| Library | Go Package | What it exposes |
|---------|------------|-----------------|
| `(wile goast)` | `extensions/goast` | AST + type information |
| `(wile goast ssa)` | `extensions/goastssa` | SSA intermediate representation (data flow) |
| `(wile goast callgraph)` | `extensions/goastcg` | Call graph (static, CHA, RTA, VTA algorithms) |
| `(wile goast cfg)` | `extensions/goastcfg` | Control flow graph + dominator tree |
| `(wile goast lint)` | `extensions/goastlint` | `go/analysis` framework (~40 built-in analyzers) |

All five layers share one node format — tagged alists `(tag (key . val) ...)` —
queryable with standard Scheme list operations. Cross-referencing between layers
uses position strings (`file:line:col`).

## Primitives

### AST Layer — `(wile goast)`

| Primitive | Description |
|-----------|-------------|
| `(go-parse-file filename . options)` | Parse a Go source file to s-expression AST |
| `(go-parse-string source . options)` | Parse a Go source string |
| `(go-parse-expr source)` | Parse a single Go expression |
| `(go-format ast)` | Convert s-expression AST back to Go source |
| `(go-node-type ast)` | Return the tag symbol of an AST node |
| `(go-typecheck-package pattern . options)` | Load and type-check a Go package |

Options: `'positions` (include source positions), `'comments` (include comments).

### SSA Layer — `(wile goast ssa)`

| Primitive | Description |
|-----------|-------------|
| `(go-ssa-build pattern . options)` | Build SSA for a Go package; returns list of `ssa-func` nodes |

Maps 35 SSA instruction types including `ssa-binop`, `ssa-call`, `ssa-field-addr`,
`ssa-store`, `ssa-phi`, `ssa-if`, `ssa-return`, closures, channels, and type operations.

### Call Graph Layer — `(wile goast callgraph)`

| Primitive | Description |
|-----------|-------------|
| `(go-callgraph pattern algorithm)` | Build call graph; algorithm is `'static`, `'cha`, `'rta`, or `'vta` |
| `(go-callgraph-callers graph func-name)` | Direct callers of a function |
| `(go-callgraph-callees graph func-name)` | Direct callees of a function |
| `(go-callgraph-reachable graph root-name)` | Transitive closure from a root |

### CFG Layer — `(wile goast cfg)`

| Primitive | Description |
|-----------|-------------|
| `(go-cfg pattern func-name . options)` | Build CFG for a named function |
| `(go-cfg-dominators cfg)` | Build dominator tree from CFG |
| `(go-cfg-dominates? dom-tree a b)` | Does block `a` dominate block `b`? |
| `(go-cfg-paths cfg from to)` | Enumerate simple paths between blocks (capped at 1024) |

### Lint Layer — `(wile goast lint)`

| Primitive | Description |
|-----------|-------------|
| `(go-analyze pattern analyzer-names ...)` | Run named analyzers on a package |
| `(go-analyze-list)` | List available analyzer names |

~40 analyzers from `go/analysis/passes/`: `nilness`, `shadow`, `copylock`,
`errorsas`, `fieldalignment`, `bools`, `assign`, and more.

## Why Scheme for Go Analysis?

Existing Go analysis tools each handle one layer well:

| Tool | Strength | Limitation |
|------|----------|------------|
| `golangci-lint` | 40+ fixed analyzers, CI integration | Can't compose ad-hoc queries |
| `gopls` | IDE-level incremental analysis | Single-query lookups, not scriptable |
| Semgrep | Syntactic pattern matching | No SSA, no CFG, no call graph |
| CodeQL | Rich query language, data flow | Proprietary, database build step, separate QL language |
| `go/analysis` | Full access to Go's compiler IRs | Requires writing Go, hundreds of lines of boilerplate per analyzer |

The goast extensions let you compose queries across all five IR layers from a
single Scheme script. The s-expression representation is uniform (same `walk`,
`assoc`, `map` utilities work everywhere), self-describing, and a natural target
for LLM-generated analysis scripts.

## Example: Cross-Layer Split-State Detection

The script [`examples/embedding/goast-query/state-trace-full.scm`](../examples/embedding/goast-query/state-trace-full.scm)
demonstrates a four-layer analysis that no single existing Go tool can perform.
It detects **split state** — conceptually atomic values scattered across multiple
struct fields, checked piecewise in distributed conditionals.

### What it does

| Pass | Layer | Question |
|------|-------|----------|
| 1 | AST | Which structs have 2+ boolean fields? (enum candidates) |
| 2 | AST | Which if-chains check multiple fields of the same receiver? (cascading checks) |
| 3 | SSA | Are those boolean fields mutated independently across functions? |
| 4 | CFG | Do reads of one field always dominate reads of the other? (fixed priority order) |

### Running it

```bash
# Build wile (all goast extensions are compiled in)
make build

# Run the analysis against a package
./dist/wile -f examples/embedding/goast-query/state-trace-full.scm
```

Edit the `target` variable at the top of the script to analyze a different package.

### Sample output

```
══════════════════════════════════════════════════
  State-Trace: Cross-Layer Split State Detection
══════════════════════════════════════════════════

── Pass 1: Boolean Clusters (AST) ──
  struct ErrExceptionEscape: bool fields (Continuable Handled)
  struct NativeTemplate: bool fields (isVariadic noCopyApply)
  struct opcodeInfo: bool fields (writesValue isBranch)

── Pass 2: If-Chain Field Sweeps (AST) ──
  receiver mc: fields (multiValues singleValue) across 2-branch chain

── Pass 3: Mutation Independence (SSA) ──
  struct NativeTemplate:
    NewForeignClosure stores only: (isVariadic)
    bindRestParameter stores only: (isVariadic)
    NewNativeTemplate stores only: (isVariadic)
    computeNoCopyApply stores only: (noCopyApply)

── Pass 4: Check Ordering (SSA + CFG) ──
  struct NativeTemplate:
    func Copy:
      isVariadic [block 4] -> noCopyApply [block 4]: same-block
  struct ErrExceptionEscape:
    func goErrorToSchemeException:
      Continuable [block 2] -> Handled [block 2]: same-block

── Summary ──
  Boolean clusters:          3
  Field sweep chains:        2
  Independent mutation sites: 4
  Dominance orderings:       2
```

### What each layer contributes

**AST alone** finds struct declarations and if-chain patterns but cannot
determine whether fields are mutated together or separately.

**SSA adds data flow**: it traces `ssa-field-addr` + `ssa-store` instructions to
show that `NativeTemplate.isVariadic` and `NativeTemplate.noCopyApply` are always
stored by different functions — evidence of independent mutation.

**CFG adds control flow ordering**: it proves that when both fields are accessed
in the same function, their reads share the same basic block (`same-block`) or
one dominates the other (`dominates`). A `dominates` result means the check order
is fixed across every execution path.

Further manual investigation of the `singleValue`/`multiValues` sweep (found by
Pass 2) using ad-hoc CFG queries confirms that `multiValues` dominates
`singleValue` in every function that checks both — proving a fixed priority
ordering. The source code independently documents this: these fields are mutually
exclusive (a discriminated union encoded as two nullable fields).

### The script is ~400 lines of Scheme

The equivalent Go implementation would require:

- A custom `go/analysis` pass for the AST patterns
- Manual SSA construction and traversal for mutation tracking
- CFG and dominator tree setup for ordering verification
- Registration boilerplate, test infrastructure, and build integration

The Scheme version composes all four layers using the same `walk`, `assoc`, and
`filter-map` utilities — no boilerplate, no type switches, no driver setup.

## More Examples

### Parse and query Go source

```scheme
(define file (go-parse-string
  "package demo
   func Add(a, b int) int { return a + b }
   func helper() {}"))

;; Extract all function names
(define names
  (filter-map
    (lambda (decl)
      (and (eq? (car decl) 'func-decl)
           (cdr (assoc 'name (cdr decl)))))
    (cdr (assoc 'decls (cdr file)))))

names ; => ("Add" "helper")
```

### Find functions returning error

```scheme
(define pkg (car (go-typecheck-package "github.com/example/pkg")))

(define error-funcs
  (filter-map
    (lambda (decl)
      (and (eq? (car decl) 'func-decl)
           (returns-error? decl)
           (cdr (assoc 'name (cdr decl)))))
    (all-decls pkg)))
```

### Build a call graph and query reachability

```scheme
(define cg (go-callgraph "." 'vta))

;; Who calls this function?
(define callers (go-callgraph-callers cg "ProcessRequest"))

;; What's reachable from main?
(define reachable (go-callgraph-reachable cg "main"))
```

### Check dominance in control flow

```scheme
;; Does every path through Run() pass through a security check?
(define cfg (go-cfg "." "Run"))
(define dom (go-cfg-dominators cfg))
(go-cfg-dominates? dom security-check-block return-block)
```

### Run static analysis passes

```scheme
;; Run nilness and shadow analyzers
(define diags (go-analyze "./..." "nilness" "shadow"))

(for-each
  (lambda (d)
    (display (cdr (assoc 'pos (cdr d))))
    (display ": ")
    (display (cdr (assoc 'message (cdr d))))
    (newline))
  diags)
```

## Security

All primitives that invoke `go list` or load packages require security
authorization:

```go
security.Check(mc.Context(), security.AccessRequest{
    Resource: security.ResourceProcess,
    Action:   security.ActionLoad,
    Target:   "go",
})
```

File-parsing primitives (`go-parse-file`) require `ResourceFile`/`ActionRead`.
Pure s-expression operations (query, format) require no authorization.

## Design

See [`plans/GO-STATIC-ANALYSIS.md`](../plans/GO-STATIC-ANALYSIS.md) for the full
design document covering architecture decisions, s-expression encoding for each
layer, mapper structure, and the phased implementation plan.
