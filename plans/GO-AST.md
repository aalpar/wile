# Go AST Extension

**Status**: Phases 1, 2, 3 & 4 complete
**Package**: `extensions/goast/`, importable as `(wile goast)`
**Dependencies**: `go/ast`, `go/parser`, `go/token`, `go/printer`, `go/format` (all stdlib)

## Overview

Scheme extension exposing Go's AST packages as s-expressions. Parse Go source into nested Scheme lists/symbols, transform with standard Scheme operations, format back to Go source. Enables writing code generation and analysis tools in Scheme.

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Representation | S-expression (nested alists) | Tree transformation is Scheme's strength; avoids 100+ opaque accessor primitives |
| Node format | Associative `(tag (field . val) ...)` | Order-independent, self-documenting, generators can omit optional fields |
| Positions/comments | Opt-in via parse flags | Clean trees for generation, rich trees for analysis |
| Round-trip fidelity | Best-effort structural | Comments survive round-trip; `go/format` controls whitespace |
| New `values.*` types | None | Output is standard Scheme lists, symbols, strings |
| Phasing | 3 phases: core, advanced, comments/generics | Fast feedback; core covers real Go programs |

## Package Structure

```
extensions/goast/
├── doc.go                # Package documentation
├── register.go           # Extension registration, (wile goast) library
├── prim_goast.go         # Primitive implementations
├── mapper.go             # Go AST -> s-expression direction
├── unmapper.go           # S-expression -> Go AST direction
├── helpers.go            # Alist construction utilities
├── prim_goast_test.go    # Primitive tests (external test package)
├── mapper_test.go        # Mapper round-trip tests
```

## Primitives

| Primitive | Signature | Security | Description |
|-----------|-----------|----------|-------------|
| `go-parse-file` | `(go-parse-file filename . options)` | `ResourceFile`/`ActionRead` | Parse file from disk to s-expression |
| `go-parse-string` | `(go-parse-string source . options)` | None | Parse Go source string as file |
| `go-parse-expr` | `(go-parse-expr source)` | None | Parse single Go expression |
| `go-format` | `(go-format ast)` | None | S-expression to formatted Go source string |
| `go-node-type` | `(go-node-type ast)` | None | Returns tag symbol of AST node |

### Parse Options

Variadic symbols after the required argument:

```scheme
(go-parse-file "main.go")                      ; no positions, no comments
(go-parse-file "main.go" 'positions)            ; include pos fields
(go-parse-file "main.go" 'positions 'comments)  ; include both
```

## S-expression Format

Every AST node is a tagged alist: `(node-type (field . value) ...)`. Optional/nil fields are represented as `#f` (e.g., `(recv . #f)` when a function has no receiver). Fields are always present in the alist for a given node type; `#f` indicates "absent" rather than omitting the field entirely.

### Atoms

```scheme
(ident (name . "fmt"))
(lit (kind . INT) (value . "42"))
(lit (kind . STRING) (value . "\"hello\""))
```

### Declarations

```scheme
;; Function
(func-decl
  (name . "Add")
  (recv . #f)
  (type . (func-type
    (params . ((field (names "a" "b") (type (ident (name . "int"))))))
    (results . ((field (type (ident (name . "int"))))))))
  (body . (block (list . (...)))))

;; Generic declaration (import/const/type/var)
(gen-decl
  (tok . import)
  (specs . ((import-spec (name . #f) (path . (lit (kind . string) (value . "\"fmt\"")))))))

(gen-decl
  (tok . var)
  (specs . ((value-spec
    (names . ("x" "y"))
    (type . (ident (name . "int")))
    (values . ((lit (kind . int) (value . "0"))))))))
```

### Statements

```scheme
(assign-stmt
  (lhs . ((ident (name . "x"))))
  (tok . :=)
  (rhs . ((lit (kind . int) (value . "1")))))

(if-stmt
  (init . #f)
  (cond . (binary-expr ...))
  (body . (block (list . (...))))
  (else . #f))

(for-stmt (init . ...) (cond . ...) (post . ...) (body . (block ...)))

(range-stmt
  (key . (ident (name . "i")))
  (value . (ident (name . "v")))
  (tok . :=)
  (x . (ident (name . "items")))
  (body . (block (list . (...)))))

(return-stmt (results . ((ident (name . "x")))))
```

### Expressions

```scheme
(binary-expr (op . +) (x . ...) (y . ...))
(unary-expr (op . -) (x . ...))
(call-expr (fun . ...) (args . (...)))
(selector-expr (x . (ident (name . "pkg"))) (sel . "Name"))
(index-expr (x . ...) (index . ...))
(composite-lit (type . ...) (elts . (...)))
(func-lit (type . (func-type ...)) (body . (block ...)))
(star-expr (x . ...))
(paren-expr (x . ...))
(type-assert-expr (x . ...) (type . ...))
(kv-expr (key . ...) (value . ...))
```

### Type Expressions

```scheme
(array-type (len . ...) (elt . ...))        ; len=#f for slices
(map-type (key . ...) (value . ...))
(struct-type (fields . ((field ...))))
(interface-type (methods . ((field ...))))
(func-type (params . (...)) (results . (...)))
```

### File (top-level)

```scheme
(file
  (name . "main")
  (decls . ((gen-decl ...) (func-decl ...) ...)))
```

### Opt-in Fields

When `'positions` flag is set, nodes include:
```scheme
(func-decl (pos . "main.go:10:1") (name . "Add") ...)
```

When `'comments` flag is set, nodes with doc comments include:
```scheme
(func-decl (doc . ("// Add returns the sum of a and b.")) (name . "Add") ...)
```

## Mapper Architecture

### Go -> S-expression (`mapper.go`)

Recursive walker over `ast.Node`. Carries options struct for conditional position/comment emission.

```go
type mapperOpts struct {
    fset      *token.FileSet
    positions bool
    comments  bool
}
```

Each node type has a `mapXxx` method that builds an alist using helpers from `helpers.go`:

```go
func tag(name string) values.Value
func field(key string, val values.Value) values.Value
func node(tag string, fields ...values.Value) values.Value
func stringList(ss []string) values.Value
```

### S-expression -> Go AST (`unmapper.go`)

Reverse direction. Parses tagged alist, extracts fields by key:

```go
func unmapNode(v values.Value) (ast.Node, error)
func getField(fields values.Value, key string) (values.Value, bool)
func requireField(fields values.Value, key string) (values.Value, error)
```

Tolerant of field ordering (alist lookup by key). Missing optional fields return `#f`.

### Round-trip Comment Fidelity

Comments survive the round-trip when parsed with `'comments` flag. The unmapper reconstructs synthetic `token.Pos` values to maintain relative ordering between nodes and their comments — exact positions don't matter, only the ordering relationship that `go/printer` uses for attachment.

Standalone comments (between declarations, end-of-file, before first declaration)
are preserved by interleaving `(comment-group ...)` entries in the `decls` list.
The mapper classifies groups using pointer identity against the Doc/Comment fields
it collects (declarations, specs, struct/interface/func-type fields, receivers);
groups not attached via those fields are treated as standalone and emitted at
their source positions.

## Error Handling

### New Sentinels

Extension-local sentinels in `extensions/goast/helpers.go` (unexported, scoped to the extension):

```go
var (
	errGoParseError   = werr.NewStaticError("go parse error")
	errMalformedGoAST = werr.NewStaticError("malformed go ast")
)
```

### Usage

```go
// Parse failure
werr.WrapForeignErrorf(werr.ErrGoParseError, "go-parse-file: %s: %s", filename, parseErr)

// Unmapper validation failure
werr.WrapForeignErrorf(werr.ErrMalformedGoAST, "go-format: func-decl missing required field 'name'")
```

## Security

Only `go-parse-file` requires authorization:

```go
security.Check(mc.Context(), security.AccessRequest{
    Resource: security.ResourceFile,
    Action:   security.ActionRead,
    Target:   filename,
})
```

All other primitives operate on strings/lists — no security gate needed.

## Phases

### Phase 4 — Type-checked Package AST  ✓ Complete

Adds `go-typecheck-package`: loads a whole package via `golang.org/x/tools/go/packages`
(which invokes `go list` for module-aware import resolution), type-checks it with
`go/types`, and returns annotated ASTs.

#### New primitive

| Primitive | Signature | Security | Description |
|-----------|-----------|----------|-------------|
| `go-typecheck-package` | `(go-typecheck-package pattern . options)` | `ResourceProcess`/`ActionLoad` | Load and type-check a package; return annotated AST list |

`pattern` is a `go list`-compatible pattern: `"."`, `"./..."`, or a full import path.

#### Return value

A Scheme list of `package` nodes:

```scheme
((package
    (name . "values")
    (path . "github.com/aalpar/wile/values")
    (files . ((file ...) (file ...) ...))))
```

#### Type annotations

Added to expression nodes when type information is available:

- `(inferred-type . "TYPE_STRING")` — on all expression nodes, via `types.Info.Types`; key is distinct from the structural `type` field used by `composite-lit` and `func-lit`
- `(obj-pkg . "PKG_PATH")` — on `ident` nodes only, via `types.Info.Uses`; identifies which package the name resolves to

Example annotated nodes:

```scheme
(ident (name . "Errorf") (inferred-type . "func(format string, a ...any) error") (obj-pkg . "fmt"))
(call-expr (fun . ...) (args . (...)) (inferred-type . "error"))
(binary-expr (op . +) (x . ...) (y . ...) (inferred-type . "int"))
; composite-lit: structural (type . AST-NODE) and annotation coexist without collision
(composite-lit (type . (ident (name . "Foo"))) (elts . (...)) (inferred-type . "pkg.Foo"))
```

#### Architecture

`mapperOpts` gains a `typeInfo *types.Info` field (nil for plain parses).
`addTypeAnnotation(e ast.Expr, opts, fields)` and
`addObjPkgAnnotation(id *ast.Ident, opts, fields)` are new helpers in `mapper.go`.
All expression-level `mapXxx` functions call these before returning.

`mapPackage` in `prim_goast.go` constructs `mapperOpts` with `pkg.Fset` and
`pkg.TypesInfo`, then maps each file in `pkg.Syntax`.

#### Dependency

`golang.org/x/tools/go/packages` added to `go.mod`. Required for module-aware
import resolution; `go/importer.Default()` does not handle modules correctly.

---

### Phase 1 — Core Subset (~28 node types)  ✓ Complete

Enough to parse and generate real Go programs without concurrency or generics.

| Category | Types |
|----------|-------|
| Top-level | `File` |
| Declarations | `FuncDecl`, `GenDecl` |
| Specs | `ImportSpec`, `ValueSpec`, `TypeSpec` |
| Statements | `BlockStmt`, `ReturnStmt`, `ExprStmt`, `AssignStmt`, `IfStmt`, `ForStmt`, `RangeStmt`, `BranchStmt`, `DeclStmt`, `IncDecStmt` |
| Expressions | `Ident`, `BasicLit`, `BinaryExpr`, `UnaryExpr`, `CallExpr`, `SelectorExpr`, `IndexExpr`, `StarExpr`, `ParenExpr`, `CompositeLit`, `KeyValueExpr`, `FuncLit` |
| Types | `ArrayType`, `MapType`, `StructType`, `InterfaceType`, `FuncType`, `Field`, `FieldList` |

**Deliverables**: All 5 primitives, bidirectional mapping, mapper round-trip tests, integration test parsing real Go source.

### Phase 2 — Concurrency, Switch, Advanced (13 node types)

Enough to parse and generate Go programs with concurrency, switch statements, and advanced expressions.

| Category | Types |
|----------|-------|
| Statements | `GoStmt`, `DeferStmt`, `SendStmt`, `LabeledStmt`, `SwitchStmt`, `TypeSwitchStmt`, `CaseClause`, `SelectStmt`, `CommClause` |
| Expressions | `SliceExpr`, `TypeAssertExpr` |
| Types | `ChanType`, `Ellipsis` |

**See detailed design: `plans/GO-AST-PHASE-2.md`**

### Phase 3 — Comments, Error Recovery, Generics (6 node types + doc field attachment)

Three sub-features with different complexity levels:

| Category | Types | Complexity |
|----------|-------|------------|
| Error recovery | `BadExpr`, `BadStmt`, `BadDecl` | Low — empty tag nodes; positions opt-in |
| Generics | `IndexListExpr` | Low — same shape as `IndexExpr` but with list of indices |
| Comments | `Comment`, `CommentGroup` + doc/comment/comments attachments on existing node types | High — position-based attachment for `go/printer` round-trip |

**See detailed design: `plans/GO-AST-PHASE-3.md`**

## Testing Strategy

### Mapper round-trip tests (`mapper_test.go`)

Table-driven, one entry per node type:
1. Construct Go AST node programmatically
2. Map to s-expression
3. Verify structure (tag, expected fields)
4. Unmap back to Go AST
5. Map again, compare with step 2

### Primitive tests (`prim_goast_test.go`)

External test package (`goast_test`). Engine loads only `goast.Extension`:
- Parse expression, verify node type
- Format round-trip (parse string -> format -> parse again -> compare)
- Error cases (invalid Go source, malformed s-expressions)

### Integration tests

Scheme-side tests parsing real Go source, verifying structure, formatting back.

### Completion criteria per phase

- All node types in phase have bidirectional mapping with round-trip tests
- All primitives have happy-path + error-path tests
- At least one integration test exercising the phase's node types
