# Go AST Extension — Phase 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add error recovery nodes, generic type instantiation, and comment round-trip support to the `(wile goast)` extension.

**Architecture:** All changes are in `extensions/goast/`. Error recovery and generics follow the Phase 1 pattern. Comments require a two-pass unmapper approach to avoid changing existing unmapper function signatures.

**Design doc:** `plans/GO-AST.md` (Phase 3 overview)

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Bad node encoding | Empty tag node; positions only when `'positions` flag set | Bad nodes have no structural content — just error spans |
| Bad node round-trip | One-way (map only); unmapper returns error | Bad nodes represent parse errors — they cannot produce valid Go source |
| `IndexListExpr` encoding | `(index-list-expr (x . EXPR) (indices . (EXPR ...)))` | Parallel to `index-expr` but with a list instead of single index |
| Comment text encoding | List of strings, each the full text including `//` or `/* */` | Preserves round-trip fidelity; `ast.Comment.Text` includes delimiters |
| Doc field encoding | `(doc . ("// line1" "// line2"))` | List of comment strings; only present when `'comments` flag set |
| Comment field encoding | `(comment . ("// trailing"))` on `Field`, `ValueSpec`, `ImportSpec` | Same format as `doc`; trailing line comment |
| File comments | `(comments . (("// group1" "// group2") ("// group3")))` | List of comment groups; each group is a list of comment strings |
| Comment unmapping strategy | Two-pass: unmap AST normally, then walk result attaching comments | Avoids changing existing unmapper function signatures |
| Synthetic positions | Monotonic counter with line-based allocation | `go/printer` needs only relative ordering, not exact positions |

## S-expression Encoding

### Error recovery

```scheme
;; Without positions flag:
(bad-expr)
(bad-stmt)
(bad-decl)

;; With positions flag:
(bad-expr (pos . "file.go:10:1") (end . "file.go:10:5"))
(bad-stmt (pos . "file.go:10:1") (end . "file.go:10:5"))
(bad-decl (pos . "file.go:10:1") (end . "file.go:10:5"))
```

### Generics

```scheme
;; Map[string, int]  (type instantiation with multiple type args)
(index-list-expr
  (x . (ident (name . "Map")))
  (indices . ((ident (name . "string")) (ident (name . "int")))))
```

### Comments (when `'comments` flag set)

```scheme
;; File with comments
(file
  (name . "main")
  (comments . (("// Copyright 2024")
               ("// Package main implements ...")))
  (decls . (...)))

;; Function with doc comment
(func-decl
  (doc . ("// Add returns the sum of a and b."
          "// It panics on overflow."))
  (name . "Add")
  (recv . #f)
  (type . (func-type ...))
  (body . (block ...)))

;; Struct field with doc + trailing comment
(field
  (doc . ("// X is the horizontal coordinate."))
  (names "X")
  (type (ident (name . "int")))
  (comment . ("// pixels")))
```

### Nodes with Doc/Comment fields

These 7 existing types gain optional fields when `'comments` is set:

| Node type | `doc` field | `comment` field |
|-----------|------------|-----------------|
| `func-decl` | yes | no |
| `gen-decl` | yes | no |
| `type-spec` | yes | `comment` (trailing) |
| `value-spec` | yes | `comment` (trailing) |
| `import-spec` | yes | `comment` (trailing) |
| `field` | yes | `comment` (trailing) |
| `file` | no (uses `comments` for file-level list) | no |

## Mapper Functions (new)

| Go AST type | Tag | Mapper function | Fields |
|---|---|---|---|
| `*ast.BadExpr` | `bad-expr` | `mapBadExpr` | `pos`, `end` (when positions set) |
| `*ast.BadStmt` | `bad-stmt` | `mapBadStmt` | `pos`, `end` (when positions set) |
| `*ast.BadDecl` | `bad-decl` | `mapBadDecl` | `pos`, `end` (when positions set) |
| `*ast.IndexListExpr` | `index-list-expr` | `mapIndexListExpr` | `x`, `indices` |

### New mapper helpers (comments)

```go
// commentGroupToStrings converts a CommentGroup to a list of text strings.
func commentGroupToStrings(cg *ast.CommentGroup) values.Value {
    if cg == nil {
        return values.FalseValue
    }
    strs := make([]values.Value, len(cg.List))
    for i, c := range cg.List {
        strs[i] = Str(c.Text)
    }
    return ValueList(strs)
}

// mapCommentGroups converts []*ast.CommentGroup to a list of string lists.
func mapCommentGroups(groups []*ast.CommentGroup) values.Value {
    if groups == nil {
        return values.FalseValue
    }
    gs := make([]values.Value, len(groups))
    for i, g := range groups {
        gs[i] = commentGroupToStrings(g)
    }
    return ValueList(gs)
}
```

### Existing mapper function modifications (comments)

For each of the 6 node types with `Doc`:

```go
// In mapFuncDecl, prepend to fields list:
if opts.comments {
    fs = append(fs, Field("doc", commentGroupToStrings(f.Doc)))
}
```

Note: `commentGroupToStrings` already returns `#f` for nil input, so always emitting the field preserves the invariant from `GO-AST.md` that fields are always present with `#f` for absent values.

For the 4 node types with trailing `Comment`:

```go
// In mapField, append to fields list:
if opts.comments {
    fs = append(fs, Field("comment", commentGroupToStrings(f.Comment)))
}
```

For `mapFile`:

```go
// Add comments field:
if opts.comments {
    fs = append(fs, Field("comments", mapCommentGroups(f.Comments)))
}
```

## Unmapper Functions (new)

| Tag | Unmapper function | File | Behavior |
|---|---|---|---|
| `"bad-expr"` | (inline error) | `unmapper.go` | Returns error — cannot unmap parse errors |
| `"bad-stmt"` | (inline error) | `unmapper.go` | Returns error |
| `"bad-decl"` | (inline error) | `unmapper.go` | Returns error |
| `"index-list-expr"` | `unmapIndexListExpr` | `unmapper_expr.go` | Standard pattern |

### Bad node unmapper entries

```go
case "bad-expr":
    return nil, werr.WrapForeignErrorf(errMalformedGoAST,
        "goast: bad-expr cannot be unmapped (represents a parse error)")
case "bad-stmt":
    return nil, werr.WrapForeignErrorf(errMalformedGoAST,
        "goast: bad-stmt cannot be unmapped (represents a parse error)")
case "bad-decl":
    return nil, werr.WrapForeignErrorf(errMalformedGoAST,
        "goast: bad-decl cannot be unmapped (represents a parse error)")
```

### IndexListExpr unmapper

```go
func unmapIndexListExpr(fields values.Value) (*ast.IndexListExpr, error) {
    xVal, err := RequireField(fields, "index-list-expr", "x")
    if err != nil {
        return nil, err
    }
    x, err := unmapExpr(xVal)
    if err != nil {
        return nil, err
    }
    indicesVal, err := RequireField(fields, "index-list-expr", "indices")
    if err != nil {
        return nil, err
    }
    indices, err := unmapExprList(indicesVal, "index-list-expr", "indices")
    if err != nil {
        return nil, err
    }
    return &ast.IndexListExpr{X: x, Indices: indices}, nil
}
```

## Comment Round-Trip Architecture

### Two-pass unmapping approach

**Pass 1**: Unmap the s-expression using existing code. No changes to any existing unmapper function signatures. The result is an `*ast.File` with `nil` positions and no comment groups.

**Pass 2**: Walk the resulting `*ast.File` and the original s-expression in parallel. For each node that has `doc` or `comment` fields in the s-expression:
1. Reconstruct `*ast.CommentGroup` from the string lists
2. Assign synthetic `token.Pos` values with correct line ordering
3. Attach the comment group to the AST node's `Doc`/`Comment` field
4. Collect all comment groups into `file.Comments`

### Position allocator

`go/printer` uses `token.Position.Line` to determine comment attachment. The allocator must ensure:
- Doc comment lines come BEFORE the declaration line
- Trailing comment lines are on the SAME line as the declaration
- All positions are monotonically increasing

```go
// posAllocator assigns synthetic positions with line tracking.
type posAllocator struct {
    fset *token.FileSet
    file *token.File
    line int // current line (1-based)
}
```

**Algorithm** for each node with doc comment:
1. For each line in the doc comment: `alloc.nextLine()` → assign to `Comment.Slash`
2. For the node itself: `alloc.nextLine()` → assign to node position
3. For trailing comment (if any): `alloc.sameLine()` → assign to `Comment.Slash`
4. Add all `CommentGroup`s to the `file.Comments` slice

**Synthetic file setup**: Create a `token.File` with enough capacity. Each "line" is a fixed-width slot (e.g., 1000 bytes). Call `file.AddLine(offset)` for each slot boundary. This gives deterministic line numbers from position offsets.

### Detection of comment mode

The unmapper detects comment mode by checking for the `comments` field on the `file` node, or `doc`/`comment` fields on any child node. If no comment fields are present, Pass 2 is skipped entirely — existing behavior is preserved.

### New file: `unmapper_comments.go`

Contains:
- `posAllocator` type and methods
- `attachComments(*ast.File, sexprFields, *token.FileSet) error` — top-level function
- `stringsToCommentGroup(values.Value, *posAllocator) (*ast.CommentGroup, error)` — converts string list
- AST walker that matches s-expression nodes to AST nodes by position in tree

---

## Tasks

### Task 1: BadExpr, BadStmt, BadDecl

Three error recovery placeholder nodes. Map to tag-only s-expressions. Unmapping returns error.

**Files:**
- Modify: `extensions/goast/mapper.go` — 3 cases + 3 mapper functions
- Modify: `extensions/goast/unmapper.go` — 3 error cases in dispatch
- Modify: `extensions/goast/mapper_test.go` — one-way mapping tests (NOT round-trip)
- Modify: `extensions/goast/prim_goast_test.go` — error test for `go-format` on bad nodes

**Test**: Parse invalid Go source with `parser.AllErrors`, verify bad node tags appear. Verify `unmapNode` returns error for bad tags.

```go
// Verify bad nodes map without panic
func TestMapBadNodes(t *testing.T) {
    fset := token.NewFileSet()
    // ParseFile with AllErrors mode
    f, _ := parser.ParseFile(fset, "test.go",
        "package p\nfunc {\n}\n", parser.AllErrors)
    if f == nil {
        t.Skip("parser did not produce a partial AST")
    }
    opts := &mapperOpts{fset: fset}
    _ = mapNode(f, opts) // must not panic
}

// Verify unmapping bad nodes returns error
func TestUnmapBadNodesError(t *testing.T) {
    for _, tag := range []string{"bad-expr", "bad-stmt", "bad-decl"} {
        t.Run(tag, func(t *testing.T) {
            _, err := unmapNode(Node(tag))
            qt.New(t).Assert(err, qt.IsNotNil)
        })
    }
}
```

**Commit:**
```
feat(goast): map BadExpr, BadStmt, BadDecl

Error recovery nodes map to tag-only s-expressions (with optional
pos/end when positions flag is set). Unmapping returns a clear
error — bad nodes represent parse errors and cannot produce valid
Go source.
```

---

### Task 2: IndexListExpr

Go 1.18+ generic type instantiation: `Map[string, int]`. Same shape as `IndexExpr` but with a list of indices.

**Files:**
- Modify: `extensions/goast/mapper.go` — 1 case + 1 mapper function
- Modify: `extensions/goast/unmapper.go` — 1 case in dispatch
- Modify: `extensions/goast/unmapper_expr.go` — 1 unmapper function
- Modify: `extensions/goast/mapper_test.go` — round-trip test

**Round-trip test source:**
```go
{name: "generic instantiation", source: "package p\n\ntype Pair[K comparable, V any] struct {\n\tKey   K\n\tValue V\n}\n\nvar _ Pair[string, int]\n"},
```

**Commit:**
```
feat(goast): map IndexListExpr for generic type instantiation

Same shape as index-expr but with a list of indices. Covers
multi-type-argument instantiation like Map[string, int].
```

---

### Task 3: Comment mapping (mapper-side only)

Enable the `'comments` flag by adding comment fields to existing mapper functions. This task handles the mapper direction only — unmapping with comments is Task 4.

**Files:**
- Modify: `extensions/goast/mapper.go` — add `commentGroupToStrings`, `mapCommentGroups` helpers; modify 7 existing mapper functions
- Modify: `extensions/goast/mapper_test.go` — one-way comment mapping tests
- Modify: `extensions/goast/prim_goast_test.go` — integration test with `'comments` flag

**Mapper modifications (7 functions):**

| Function | Add `doc` field | Add `comment` field |
|----------|----------------|---------------------|
| `mapFuncDecl` | yes | no |
| `mapGenDecl` | yes | no |
| `mapTypeSpec` | yes | yes |
| `mapValueSpec` | yes | yes |
| `mapImportSpec` | yes | yes |
| `mapField` | yes | yes |
| `mapFile` | `comments` field | no |

**Pattern for each function** (prepend doc, append comment):

```go
// In mapFuncDecl:
var fs []values.Value
if opts.comments && f.Doc != nil {
    fs = append(fs, Field("doc", commentGroupToStrings(f.Doc)))
}
fs = append(fs,
    Field("name", Str(f.Name.Name)),
    // ... existing fields ...
)
return Node("func-decl", fs...)
```

**Note**: Functions that currently use direct `Node(...)` calls without a `fs` slice will need refactoring to the `fs = append(fs, ...)` pattern. Check each function: `mapFuncDecl` already uses `fs` for conditional `recv`. Others (`mapGenDecl`, `mapTypeSpec`, `mapValueSpec`, `mapImportSpec`) may use direct `Node()` calls and need the same treatment.

**Test (one-way mapping):**

```go
func TestMapComments(t *testing.T) {
    c := qt.New(t)
    fset := token.NewFileSet()
    source := "package p\n\n// Add returns the sum.\nfunc Add(a, b int) int {\n\treturn a + b\n}\n"
    f, err := parser.ParseFile(fset, "test.go", source, parser.ParseComments)
    c.Assert(err, qt.IsNil)

    opts := &mapperOpts{fset: fset, comments: true}
    sexpr := mapNode(f, opts)

    // Navigate to the func-decl and check for doc field
    // ... verify (doc . ("// Add returns the sum.")) is present
}
```

**Integration test (Scheme-side):**

Verify that `(go-parse-string source 'comments)` produces nodes with `doc` fields, and that `(go-parse-string source)` (without flag) does NOT produce `doc` fields.

**Commit:**
```
feat(goast): add comment mapping with 'comments flag

When 'comments is set, doc fields appear on FuncDecl, GenDecl,
TypeSpec, ValueSpec, ImportSpec, and Field. Trailing comment
fields appear on Field, ValueSpec, ImportSpec, and TypeSpec.
File-level comments list contains all comment groups.
```

---

### Task 4: Comment unmapping with synthetic position reconstruction

Enable round-trip fidelity for comments using the two-pass approach.

**Files:**
- Create: `extensions/goast/unmapper_comments.go` — `posAllocator`, `attachComments`, helpers
- Modify: `extensions/goast/unmapper.go` — call `attachComments` after `unmapFile` when comments present
- Modify: `extensions/goast/mapper_test.go` — comment round-trip tests

**Two-pass integration point** (in `unmapFile` or a new wrapper):

```go
// After constructing the *ast.File:
commentsVal, hasComments := GetField(fields, "comments")
if hasComments && !IsFalse(commentsVal) {
    fset := token.NewFileSet()
    err = attachComments(file, fields, fset)
    if err != nil {
        return nil, err
    }
}
```

**`unmapper_comments.go` contents:**

1. **`posAllocator`** — wraps a `token.FileSet` and `token.File`, tracks current line
2. **`attachComments`** — top-level: walks AST + s-expression together, attaches `Doc`/`Comment` groups
3. **`stringsToCommentGroup`** — converts `("// line1" "// line2")` to `*ast.CommentGroup`
4. **`assignPositions`** — walks AST assigning synthetic positions to all nodes

**Key challenge**: Matching s-expression nodes to their corresponding AST nodes during the walk. Since the unmap was just performed, the tree structure is identical — a parallel DFS walk on both trees keeps them aligned.

**Round-trip test sources:**

```go
{name: "func with doc comment", source: "package p\n\n// Add returns the sum.\nfunc Add(a, b int) int {\n\treturn a + b\n}\n"},
{name: "struct with field comments", source: "package p\n\ntype Point struct {\n\t// X is horizontal.\n\tX int\n\t// Y is vertical.\n\tY int\n}\n"},
```

These tests use `parser.ParseComments` mode and `comments: true` in mapper opts. The `roundTripFile` helper needs an option to enable comment mode.

**Commit:**
```
feat(goast): add comment round-trip with synthetic positions

Two-pass unmapping: first unmap AST normally, then walk the result
attaching comment groups with synthetic positions. go/printer uses
position ordering for comment attachment — exact positions don't
matter, only the relative order.
```

---

### Task 5: Full verification and plan update

**Step 1**: Run `make lint && make test`
**Step 2**: Run `make covercheck`
**Step 3**: Verify comment round-trip on real Go source with `go-parse-file` + `'comments` + `go-format`
**Step 4**: Update `plans/GO-AST.md` Phase 3 status → "Complete"
**Step 5**: Commit: `docs: mark GO-AST Phase 3 complete`

---

## Post-implementation checklist

- [ ] `BadExpr`, `BadStmt`, `BadDecl` map without panic; unmap returns error
- [ ] `IndexListExpr` round-trips with multi-type-arg generic instantiation
- [ ] `'comments` flag produces `doc` fields on all 6 node types with `Doc`
- [ ] `'comments` flag produces `comment` fields on all 4 node types with `Comment`
- [ ] `'comments` flag produces `comments` field on `file` nodes
- [ ] Without `'comments` flag, no `doc`/`comment`/`comments` fields appear (no regression)
- [ ] Doc comments round-trip: parse with `'comments` → format → compare
- [ ] Trailing comments round-trip on struct fields
- [ ] `go/printer` attaches comments to correct declarations after unmapping
- [ ] Existing Phase 1 + Phase 2 tests continue to pass
- [ ] `make lint && make covercheck` pass
