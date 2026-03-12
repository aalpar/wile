# Go AST Extension — Phase 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add bidirectional mapping for 13 AST node types covering concurrency, switch/select statements, slice expressions, type assertions, channel types, and ellipsis.

**Architecture:** All changes are in `extensions/goast/`. The pattern is identical to Phase 1: add a `case *ast.Foo:` to `mapNode`, implement `mapFoo`, add `"foo-tag"` to `unmapNode` dispatch, implement `unmapFoo`, write round-trip tests.

**Design doc:** `plans/GO-AST.md` (Phase 2 overview); `extensions/goast/mapper.go` — current type switch

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| `CaseClause.List` nil encoding | `#f` for default, proper list for cases | Consistent with existing `#f`-for-absent pattern; empty `()` would be ambiguous |
| `CommClause.Comm` nil encoding | `#f` for default | Same pattern as `CaseClause` |
| `ChanType.Dir` encoding | Symbol: `send`, `recv`, or `both` | Human-readable; `ast.ChanDir` bitmask (SEND=1, RECV=2, SEND\|RECV=3) maps cleanly |
| `SliceExpr.Slice3` encoding | Boolean `#t`/`#f` | Direct mapping via `BoolToBoolean`; distinguishes `s[a:b]` from `s[a:b:c]` |
| `TypeAssertExpr.Type` nil encoding | `#f` for type switch `x.(type)` | Nil Type means unresolved type switch; `#f` preserves that signal |
| `LabeledStmt.Label` encoding | String (not `ident` node) | Matches `BranchStmt.Label` convention — labels are simple names, not full identifiers |
| `SwitchStmt.Body` / `SelectStmt.Body` | Regular `block` node | Body is `*ast.BlockStmt` in Go AST; unmapper handles it normally, each list item dispatches to `case-clause` or `comm-clause` |
| `CaseClause.Body` / `CommClause.Body` encoding | Flat statement list via `ValueList` | These are `[]Stmt` (not `*ast.BlockStmt`); encoded same as `ReturnStmt.Results` |

## S-expression Encoding

### Simple statements

```scheme
;; go f()
(go-stmt
  (call . (call-expr (fun . (ident (name . "f"))) (args . ()))))

;; defer f()
(defer-stmt
  (call . (call-expr (fun . (ident (name . "f"))) (args . ()))))

;; ch <- v
(send-stmt
  (chan . (ident (name . "ch")))
  (value . (ident (name . "v"))))

;; myLabel: for { ... }
(labeled-stmt
  (label . "myLabel")
  (stmt . (for-stmt (init . #f) (cond . #f) (post . #f) (body . (block ...)))))
```

### Switch statements

```scheme
;; switch x { case 1: f(); default: g() }
(switch-stmt
  (init . #f)
  (tag . (ident (name . "x")))
  (body . (block (list . (
    (case-clause
      (list . ((lit (kind . INT) (value . "1"))))
      (body . ((expr-stmt (x . (call-expr ...))))))
    (case-clause
      (list . #f)        ;; default case
      (body . ((expr-stmt (x . (call-expr ...)))))))))))

;; switch v := x.(type) { case int: ... }
(type-switch-stmt
  (init . #f)
  (assign . (assign-stmt
    (lhs . ((ident (name . "v"))))
    (tok . :=)
    (rhs . ((type-assert-expr (x . (ident (name . "x"))) (type . #f))))))
  (body . (block (list . (
    (case-clause
      (list . ((ident (name . "int"))))
      (body . (...))))))))
```

### Select statement

```scheme
;; select { case v := <-ch: f(v); default: g() }
(select-stmt
  (body . (block (list . (
    (comm-clause
      (comm . (assign-stmt
        (lhs . ((ident (name . "v"))))
        (tok . :=)
        (rhs . ((unary-expr (op . <-) (x . (ident (name . "ch"))))))))
      (body . ((expr-stmt (x . (call-expr ...))))))
    (comm-clause
      (comm . #f)        ;; default case
      (body . ((expr-stmt (x . (call-expr ...)))))))))))
```

### Expressions and types

```scheme
;; s[1:3:5]
(slice-expr
  (x . (ident (name . "s")))
  (low . (lit (kind . INT) (value . "1")))
  (high . (lit (kind . INT) (value . "3")))
  (max . (lit (kind . INT) (value . "5")))
  (slice3 . #t))

;; s[1:3]  (2-index slice)
(slice-expr
  (x . ...)
  (low . ...)
  (high . ...)
  (max . #f)
  (slice3 . #f))

;; x.(int)
(type-assert-expr
  (x . (ident (name . "x")))
  (type . (ident (name . "int"))))

;; x.(type)  — in type switch
(type-assert-expr
  (x . (ident (name . "x")))
  (type . #f))

;; chan<- int / <-chan int / chan int
(chan-type
  (dir . send)           ;; or recv, or both
  (value . (ident (name . "int"))))

;; func(args ...int) — the Ellipsis wraps the element type
(ellipsis
  (elt . (ident (name . "int"))))

;; [...]int — Ellipsis with no element type (array length context)
(ellipsis
  (elt . #f))
```

## Mapper Functions

Each mapper follows `map<NodeType>(n *ast.<NodeType>, opts *mapperOpts) values.Value`. All are added to the type switch in `mapNode`.

| Go AST type | Tag | Mapper function | Fields |
|---|---|---|---|
| `*ast.GoStmt` | `go-stmt` | `mapGoStmt` | `call` |
| `*ast.DeferStmt` | `defer-stmt` | `mapDeferStmt` | `call` |
| `*ast.SendStmt` | `send-stmt` | `mapSendStmt` | `chan`, `value` |
| `*ast.LabeledStmt` | `labeled-stmt` | `mapLabeledStmt` | `label` (string), `stmt` |
| `*ast.SwitchStmt` | `switch-stmt` | `mapSwitchStmt` | `init` (#f), `tag` (#f), `body` |
| `*ast.TypeSwitchStmt` | `type-switch-stmt` | `mapTypeSwitchStmt` | `init` (#f), `assign`, `body` |
| `*ast.CaseClause` | `case-clause` | `mapCaseClause` | `list` (#f for default), `body` (stmt list) |
| `*ast.SelectStmt` | `select-stmt` | `mapSelectStmt` | `body` |
| `*ast.CommClause` | `comm-clause` | `mapCommClause` | `comm` (#f for default), `body` (stmt list) |
| `*ast.SliceExpr` | `slice-expr` | `mapSliceExpr` | `x`, `low` (#f), `high` (#f), `max` (#f), `slice3` (bool) |
| `*ast.TypeAssertExpr` | `type-assert-expr` | `mapTypeAssertExpr` | `x`, `type` (#f for `x.(type)`) |
| `*ast.ChanType` | `chan-type` | `mapChanType` | `dir` (symbol), `value` |
| `*ast.Ellipsis` | `ellipsis` | `mapEllipsis` | `elt` (#f) |

### New mapper helper

```go
// chanDirSymbol converts ast.ChanDir to a Scheme symbol.
func chanDirSymbol(dir ast.ChanDir) values.Value {
    switch dir {
    case ast.SEND:
        return Sym("send")
    case ast.RECV:
        return Sym("recv")
    default:
        return Sym("both")
    }
}
```

### Type annotation rules

`addTypeAnnotation` must be called on expression nodes: `SliceExpr`, `TypeAssertExpr`, `Ellipsis`. It must NOT be called on type nodes: `ChanType` (consistent with `ArrayType`, `MapType`, etc.).

### CaseClause mapper reference

`CaseClause.Body` and `CommClause.Body` are `[]Stmt` (not wrapped in `BlockStmt`). Encode as a statement list using `ValueList`:

```go
func mapCaseClause(c *ast.CaseClause, opts *mapperOpts) values.Value {
    var listVal values.Value
    if c.List == nil {
        listVal = values.FalseValue // default case
    } else {
        exprs := make([]values.Value, len(c.List))
        for i, e := range c.List {
            exprs[i] = mapExpr(e, opts)
        }
        listVal = ValueList(exprs)
    }
    stmts := make([]values.Value, len(c.Body))
    for i, s := range c.Body {
        stmts[i] = mapStmt(s, opts)
    }
    return Node("case-clause",
        Field("list", listVal),
        Field("body", ValueList(stmts)),
    )
}
```

## Unmapper Functions

Each unmapper follows `unmap<NodeType>(fields values.Value) (*ast.<NodeType>, error)`. All are added to the tag dispatch in `unmapNode`.

| Tag | Unmapper function | File |
|---|---|---|
| `"go-stmt"` | `unmapGoStmt` | `unmapper_stmt.go` |
| `"defer-stmt"` | `unmapDeferStmt` | `unmapper_stmt.go` |
| `"send-stmt"` | `unmapSendStmt` | `unmapper_stmt.go` |
| `"labeled-stmt"` | `unmapLabeledStmt` | `unmapper_stmt.go` |
| `"switch-stmt"` | `unmapSwitchStmt` | `unmapper_stmt.go` |
| `"type-switch-stmt"` | `unmapTypeSwitchStmt` | `unmapper_stmt.go` |
| `"case-clause"` | `unmapCaseClause` | `unmapper_stmt.go` |
| `"select-stmt"` | `unmapSelectStmt` | `unmapper_stmt.go` |
| `"comm-clause"` | `unmapCommClause` | `unmapper_stmt.go` |
| `"slice-expr"` | `unmapSliceExpr` | `unmapper_expr.go` |
| `"type-assert-expr"` | `unmapTypeAssertExpr` | `unmapper_expr.go` |
| `"chan-type"` | `unmapChanType` | `unmapper_types.go` |
| `"ellipsis"` | `unmapEllipsis` | `unmapper_expr.go` |

### New unmapper helper

```go
// chanDirFromSymbol converts a Scheme symbol to ast.ChanDir.
func chanDirFromSymbol(v values.Value) (ast.ChanDir, error) {
    name, err := RequireSymbol(v, "chan-type", "dir")
    if err != nil {
        return 0, err
    }
    switch name {
    case "send":
        return ast.SEND, nil
    case "recv":
        return ast.RECV, nil
    case "both":
        return ast.SEND | ast.RECV, nil
    default:
        return 0, werr.WrapForeignErrorf(errMalformedGoAST,
            "goast: chan-type field 'dir' unknown direction '%s'", name)
    }
}
```

### Key unmapping patterns

**SliceExpr.Slice3** — extract boolean (strict validation, consistent with unmapper conventions):
```go
slice3Val, err := RequireField(fields, "slice-expr", "slice3")
// ...
b, ok := slice3Val.(*values.Boolean)
if !ok {
    return nil, werr.WrapForeignErrorf(errMalformedGoAST,
        "goast: slice-expr field 'slice3' must be boolean, got %T", slice3Val)
}
slice3 := b.Value
```

**CaseClause.List** — `#f` for default, `[]Expr` otherwise:
```go
listVal, err := RequireField(fields, "case-clause", "list")
// ...
var list []ast.Expr
if !IsFalse(listVal) {
    list, err = unmapExprList(listVal, "case-clause", "list")
    // ...
}
```

**CaseClause.Body** — `[]Stmt` (not a block), use `unmapStmtList`:
```go
bodyVal, err := RequireField(fields, "case-clause", "body")
// ...
body, err := unmapStmtList(bodyVal, "case-clause", "body")
```

---

## Tasks

### Task 1: GoStmt, DeferStmt, SendStmt, LabeledStmt

Four simple statement types. GoStmt and DeferStmt each wrap a `CallExpr`. SendStmt has `Chan` and `Value` expressions. LabeledStmt has a string label and a wrapped statement.

**Files:**
- Modify: `extensions/goast/mapper.go` — 4 cases + 4 mapper functions
- Modify: `extensions/goast/unmapper.go` — 4 cases in `unmapNode` dispatch
- Modify: `extensions/goast/unmapper_stmt.go` — 4 unmapper functions
- Modify: `extensions/goast/mapper_test.go` — round-trip tests
- Modify: `extensions/goast/prim_goast_test.go` — integration tests

**Round-trip test sources** (add to `roundTripFile` table):

```go
{name: "go statement", source: "package p\n\nfunc f() {\n\tgo g()\n}\n\nfunc g() {\n}\n"},
{name: "defer statement", source: "package p\n\nfunc f() {\n\tdefer g()\n}\n\nfunc g() {\n}\n"},
{name: "send statement", source: "package p\n\nfunc f(ch chan int) {\n\tch <- 42\n}\n"},
{name: "labeled statement", source: "package p\n\nfunc f() {\nouter:\n\tfor {\n\t\tbreak outer\n\t}\n}\n"},
```

**Commit:**
```
feat(goast): map GoStmt, DeferStmt, SendStmt, LabeledStmt

GoStmt and DeferStmt wrap a call-expr. SendStmt has chan and value
expression fields. LabeledStmt uses a string label (matching the
branch-stmt convention) with a wrapped statement.
```

---

### Task 2: SwitchStmt, TypeSwitchStmt, CaseClause, TypeAssertExpr

Switch statement family. `CaseClause` is shared by both `SwitchStmt` and `TypeSwitchStmt`. `TypeAssertExpr` is pulled into this task because `TypeSwitchStmt` round-trip tests require the `x.(type)` form.

**Key detail**: `CaseClause.List` is nil for default case (encoded as `#f`). `CaseClause.Body` is `[]Stmt` (not a `BlockStmt`), encoded as a flat statement list.

**Key detail**: `TypeSwitchStmt.Assign` is either an `*ast.ExprStmt` wrapping `x.(type)` or an `*ast.AssignStmt` with `x := y.(type)`. The unmapper handles this as a regular `Stmt`.

**Files:**
- Modify: `extensions/goast/mapper.go` — 4 cases + 4 mapper functions
- Modify: `extensions/goast/unmapper.go` — 4 cases in dispatch
- Modify: `extensions/goast/unmapper_stmt.go` — 3 unmapper functions
- Modify: `extensions/goast/unmapper_expr.go` — 1 unmapper function (TypeAssertExpr)
- Modify: `extensions/goast/mapper_test.go` — round-trip tests

**Round-trip test sources:**

```go
{name: "switch statement", source: "package p\n\nfunc f(x int) int {\n\tswitch x {\n\tcase 1:\n\t\treturn 10\n\tcase 2, 3:\n\t\treturn 20\n\tdefault:\n\t\treturn 0\n\t}\n}\n"},
{name: "bare switch", source: "package p\n\nfunc f(x int) {\n\tswitch {\n\tcase x > 0:\n\t\treturn\n\t}\n}\n"},
{name: "type switch", source: "package p\n\nfunc f(x interface{}) {\n\tswitch v := x.(type) {\n\tcase int:\n\t\t_ = v\n\tcase string:\n\t\t_ = v\n\tdefault:\n\t\t_ = v\n\t}\n}\n"},
{name: "type assertion", source: "package p\n\nfunc f(x interface{}) int {\n\treturn x.(int)\n}\n"},
```

**Expression round-trip** (add to `roundTripExpr` table):
```go
{name: "type assert", source: "x.(int)"},
```

**Commit:**
```
feat(goast): map SwitchStmt, TypeSwitchStmt, CaseClause, TypeAssertExpr

CaseClause.List is #f for default case; Body is a flat statement
list. TypeSwitchStmt.Assign handles both ExprStmt and AssignStmt
forms. TypeAssertExpr.Type is #f for x.(type) in type switches.
```

---

### Task 3: SelectStmt, CommClause

Select statement family. `CommClause.Comm` is nil for default case, or a `SendStmt` / assignment-wrapping-receive. `CommClause.Body` is `[]Stmt`, same encoding as `CaseClause.Body`.

**Key detail**: `CommClause.Comm` can be: (a) nil → default, (b) `*ast.SendStmt` → `case ch <- v:`, (c) `*ast.ExprStmt` → `case <-ch:`, or (d) `*ast.AssignStmt` → `case v := <-ch:`. All are regular statements handled by existing + Task 1 unmappers.

**Dependency**: `SendStmt` must be implemented before this task (done in Task 1).

**Files:**
- Modify: `extensions/goast/mapper.go` — 2 cases + 2 mapper functions
- Modify: `extensions/goast/unmapper.go` — 2 cases in dispatch
- Modify: `extensions/goast/unmapper_stmt.go` — 2 unmapper functions
- Modify: `extensions/goast/mapper_test.go` — round-trip tests

**Round-trip test sources:**
```go
{name: "select statement", source: "package p\n\nfunc f(c1, c2 chan int) int {\n\tselect {\n\tcase v := <-c1:\n\t\treturn v\n\tcase c2 <- 42:\n\t\treturn 0\n\tdefault:\n\t\treturn -1\n\t}\n}\n"},
```

**Commit:**
```
feat(goast): map SelectStmt, CommClause

CommClause.Comm is #f for default case. Body is a flat statement
list. SendStmt (Task 1) is reused for send-case clauses.
```

---

### Task 4: SliceExpr, ChanType, Ellipsis

Remaining expression and type nodes. `TypeAssertExpr` was pulled into Task 2.

**Key detail**: `SliceExpr` has three optional bounds (`Low`, `High`, `Max` — each `#f` when absent) and a `Slice3` boolean. `ChanType.Dir` encodes as symbol `send`/`recv`/`both` via `chanDirSymbol`/`chanDirFromSymbol` helpers. `Ellipsis.Elt` is `#f` when used as array length (`[...]int`).

**Key detail**: `addTypeAnnotation` must be called on `SliceExpr` and `Ellipsis` (expression nodes) but NOT on `ChanType` (type node — consistent with `ArrayType`, `MapType`, etc.).

**Files:**
- Modify: `extensions/goast/mapper.go` — 3 cases + 3 mapper functions + `chanDirSymbol`
- Modify: `extensions/goast/unmapper.go` — 3 cases in dispatch
- Modify: `extensions/goast/unmapper_expr.go` — 2 unmapper functions (SliceExpr, Ellipsis)
- Modify: `extensions/goast/unmapper_types.go` — 1 unmapper function (ChanType) + `chanDirFromSymbol`
- Modify: `extensions/goast/mapper_test.go` — round-trip tests

**Round-trip test sources (file):**
```go
{name: "slice expr", source: "package p\n\nfunc f(s []int) []int {\n\treturn s[1:3]\n}\n"},
{name: "3-index slice", source: "package p\n\nfunc f(s []int) []int {\n\treturn s[1:3:5]\n}\n"},
{name: "channel types", source: "package p\n\nvar (\n\ta chan int\n\tb chan<- int\n\tc <-chan int\n)\n"},
{name: "variadic function", source: "package p\n\nfunc f(args ...int) {\n}\n"},
{name: "array ellipsis", source: "package p\n\nvar a = [...]int{1, 2, 3}\n"},
```

**Expression round-trip:**
```go
{name: "slice 2-index", source: "s[1:3]"},
{name: "slice 3-index", source: "s[1:3:5]"},
{name: "slice no low",  source: "s[:3]"},
```

**Commit:**
```
feat(goast): map SliceExpr, ChanType, Ellipsis

SliceExpr includes low/high/max (#f when absent) and slice3 boolean.
ChanType.Dir is encoded as send/recv/both symbol. Ellipsis.Elt is
#f for array-length context ([...]int).
```

---

### Task 5: Full verification and plan update

**Step 1**: Run `make lint && make test`
**Step 2**: Run `make covercheck`
**Step 3**: Verify no `unknown` tags when parsing Go with concurrency constructs
**Step 4**: Update `plans/GO-AST.md` Phase 2 status → "Complete"
**Step 5**: Commit: `docs: mark GO-AST Phase 2 complete`

---

## Implementation Deviations

### 1. `send statement` and `select statement` tests moved to Task 4

The plan placed send-stmt in Task 1 and select-stmt in Task 3. Both test sources
include a channel parameter in the function signature, which contains an
`*ast.ChanType` node. Unmapping fails with "unsupported Go node type *ast.ChanType"
until ChanType is implemented in Task 4. Both tests were moved there and added
alongside the other channel-type round-trip tests.

### 2. `type switch` and `type assertion` tests use a named type instead of `interface{}`

The plan used `interface{}` as the parameter type in these test sources. An empty
interface type round-trips as `interface {\n}` rather than `interface{}` because
`go/printer` inserts a newline inside empty interface braces when no position
information is present in the AST. This is a pre-existing limitation of the
position-stripped round-trip, not introduced by Phase 2. The tests use the
identifier `Any` as a named placeholder type, which maps to a plain `*ast.Ident`
and avoids the formatting discrepancy.

---

## Post-implementation checklist

- [ ] All 13 node types have bidirectional mapping (mapper + unmapper)
- [ ] All 13 node types have round-trip tests in `mapper_test.go`
- [ ] `CaseClause` default case (`List == nil`) round-trips as `#f`
- [ ] `CommClause` default case (`Comm == nil`) round-trips as `#f`
- [ ] `ChanType` all three directions round-trip correctly
- [ ] `SliceExpr` 2-index and 3-index forms both round-trip
- [ ] `TypeAssertExpr` with `Type == nil` (type switch form) round-trips
- [ ] `Ellipsis` with and without `Elt` both round-trip
- [ ] `addTypeAnnotation` called on `SliceExpr`, `TypeAssertExpr`, `Ellipsis`
- [ ] `addTypeAnnotation` NOT called on `ChanType`
- [ ] `make lint && make covercheck` pass
