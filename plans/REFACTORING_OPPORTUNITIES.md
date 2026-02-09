# Refactoring Opportunities

This document catalogs refactoring opportunities identified in the Wile codebase. Each follows the pattern: **same logical operation, different surface implementation**.

For larger structural reductions (numeric tower, port base types, operation boilerplate, etc.), see `ALGEBRAIC_REDUCTIONS.md`.

---

## 1. Form-Checking Predicates

**Status**: Ready to implement
**Impact**: ~20 lines reduced, enables further unification
**Risk**: Low

### Problem

Two functions in `go/machine/expander_time_continuation.go` are structurally identical:

**`isExpandedDefineForm` (lines 486-497)**:
```go
func isExpandedDefineForm(expr syntax.SyntaxValue) bool {
    pair, ok := expr.(*syntax.SyntaxPair)
    if !ok || syntax.IsSyntaxEmptyList(pair) {
        return false
    }
    car := pair.SyntaxCar()
    sym, ok := car.(*syntax.SyntaxSymbol)
    if !ok {
        return false
    }
    return sym.Sym.Key == "define"
}
```

**`isDefineSyntaxSyntax` (lines 1104-1115)**:
```go
func isDefineSyntaxSyntax(expr syntax.SyntaxValue) bool {
    pair, ok := expr.(*syntax.SyntaxPair)
    if !ok {
        return false
    }
    carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
    if !ok {
        return false
    }
    sym, ok := carSym.Unwrap().(*values.Symbol)
    return ok && sym.Key == "define-syntax"
}
```

The only differences are:
1. Variable names (`car` vs `carSym`)
2. One checks `IsSyntaxEmptyList`, one doesn't (accidental)
3. One uses `sym.Sym.Key`, one uses `carSym.Unwrap().(*values.Symbol)` (both get the symbol key)
4. The target keyword ("define" vs "define-syntax")

### Solution

Create a single generic helper:

```go
// isSyntaxFormWithKeyword checks if expr is a syntax pair whose car is
// a syntax symbol with the given keyword.
func isSyntaxFormWithKeyword(expr syntax.SyntaxValue, keyword string) bool {
    pair, ok := expr.(*syntax.SyntaxPair)
    if !ok || syntax.IsSyntaxEmptyList(pair) {
        return false
    }
    carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
    if !ok {
        return false
    }
    sym, ok := carSym.Unwrap().(*values.Symbol)
    return ok && sym.Key == keyword
}
```

Then replace:
```go
// Before
if isExpandedDefineForm(expr) { ... }
if isDefineSyntaxSyntax(expanded) { ... }

// After
if isSyntaxFormWithKeyword(expr, "define") { ... }
if isSyntaxFormWithKeyword(expanded, "define-syntax") { ... }
```

### Files to Modify

| File | Change |
|------|--------|
| `go/machine/expander_time_continuation.go` | Add `isSyntaxFormWithKeyword`, remove `isExpandedDefineForm` and `isDefineSyntaxSyntax` |

### Verification

```bash
cd go && make build && go test ./... -count=1
```

---

## 2. CxR Accessor Primitives

**Status**: Ready to implement
**Impact**: ~300 lines reduced
**Risk**: Low (mechanical transformation)

### Problem

`go/registry/core/prim_pairs.go` contains 28 nearly identical functions (lines 127-428):

```go
func PrimCaar(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    v, err := cxrHelper("caar", "aa", o)
    if err != nil {
        return err
    }
    mc.SetValue(v)
    return nil
}

func PrimCadr(_ context.Context, mc *machine.MachineContext) error {
    o := mc.Arg(0)
    v, err := cxrHelper("cadr", "ad", o)
    if err != nil {
        return err
    }
    mc.SetValue(v)
    return nil
}
// ... 26 more identical functions
```

Each function differs only in:
1. Function name (PrimCaar, PrimCadr, ...)
2. First argument to cxrHelper (primitive name string)
3. Second argument to cxrHelper (operation string: "aa", "ad", "da", ...)

### Solution

**Factory Function**:

```go
// makeCxrPrimitive returns a primitive function for the given CxR operation.
func makeCxrPrimitive(name, ops string) func(context.Context, *machine.MachineContext) error {
    return func(_ context.Context, mc *machine.MachineContext) error {
        v, err := cxrHelper(name, ops, mc.Arg(0))
        if err != nil {
            return err
        }
        mc.SetValue(v)
        return nil
    }
}

// Registration becomes:
var cxrPrimitives = []struct{ name, ops string }{
    {"caar", "aa"}, {"cadr", "ad"}, {"cdar", "da"}, {"cddr", "dd"},
    {"caaar", "aaa"}, {"caadr", "aad"}, {"cadar", "ada"}, {"caddr", "add"},
    {"cdaar", "daa"}, {"cdadr", "dad"}, {"cddar", "dda"}, {"cdddr", "ddd"},
    {"caaaar", "aaaa"}, {"caaadr", "aaad"}, {"caadar", "aada"}, {"caaddr", "aadd"},
    {"cadaar", "adaa"}, {"cadadr", "adad"}, {"caddar", "adda"}, {"cadddr", "addd"},
    {"cdaaar", "daaa"}, {"cdaadr", "daad"}, {"cdadar", "dada"}, {"cdaddr", "dadd"},
    {"cddaar", "ddaa"}, {"cddadr", "ddad"}, {"cdddar", "ddda"}, {"cddddr", "dddd"},
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/registry/core/prim_pairs.go` | Replace 28 functions with factory + data |
| `go/registry/core/pairs.go` | Update registration to use factory |

### Verification

```bash
cd go && make build
echo '(cadr (list 1 2 3))' | ../dist/scheme  # Expected: 2
echo '(caddr (list 1 2 3 4))' | ../dist/scheme  # Expected: 3
cd go && go test ./registry/core/... -count=1
```

---

## 3. Compile-Time Form Argument Extraction

**Status**: Ready to implement
**Impact**: ~50 lines reduced across 6 files
**Risk**: Low

### Problem

Six compile files extract form arguments with identical code:

**`compile_syntax_form.go:35-47`**:
```go
argsPair, ok := expr.(*syntax.SyntaxPair)
if !ok || argsPair.IsEmptyList() {
    return values.NewForeignError("syntax: expected exactly one argument")
}
template := argsPair.SyntaxCar()
rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
if !ok || !rest.IsEmptyList() {
    return values.NewForeignError("syntax: expected exactly one argument")
}
```

Similar patterns appear in:
- `compile_syntax_case.go:48-60` (extracts input expression, literals, and clauses)
- `compile_with_syntax.go:34-50` (extracts bindings and body)
- `compile_define_for_syntax.go:44-60` (extracts name and expression)
- `compile_eval_when.go:54-72` (extracts phases and body)
- `compile_begin_for_syntax.go:43-52` (extracts expression list)

### Solution

Add helpers to `compile_time_continuation.go`:

```go
// extractSingleArg extracts exactly one argument from a form's argument list.
// Returns the argument and an error if the form doesn't have exactly one argument.
func extractSingleArg(expr syntax.SyntaxValue, formName string) (syntax.SyntaxValue, error) {
    argsPair, ok := expr.(*syntax.SyntaxPair)
    if !ok || argsPair.IsEmptyList() {
        return nil, values.NewForeignError(formName + ": expected exactly one argument")
    }
    arg := argsPair.SyntaxCar()
    rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
    if !ok || !rest.IsEmptyList() {
        return nil, values.NewForeignError(formName + ": expected exactly one argument")
    }
    return arg, nil
}

// extractTwoArgs extracts exactly two arguments from a form's argument list.
func extractTwoArgs(expr syntax.SyntaxValue, formName string) (syntax.SyntaxValue, syntax.SyntaxValue, error) {
    argsPair, ok := expr.(*syntax.SyntaxPair)
    if !ok || argsPair.IsEmptyList() {
        return nil, nil, values.NewForeignError(formName + ": expected two arguments")
    }
    first := argsPair.SyntaxCar()
    rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
    if !ok || rest.IsEmptyList() {
        return nil, nil, values.NewForeignError(formName + ": expected two arguments")
    }
    second := rest.SyntaxCar()
    rest2, ok := rest.SyntaxCdr().(*syntax.SyntaxPair)
    if !ok || !rest2.IsEmptyList() {
        return nil, nil, values.NewForeignError(formName + ": expected exactly two arguments")
    }
    return first, second, nil
}

// extractArgList extracts the argument list from a form, returning it as a SyntaxPair.
// Use when you need to iterate over arguments rather than extract a fixed number.
func extractArgList(expr syntax.SyntaxValue, formName string) (*syntax.SyntaxPair, error) {
    argsPair, ok := expr.(*syntax.SyntaxPair)
    if !ok {
        return nil, values.NewForeignError(formName + ": expected arguments")
    }
    return argsPair, nil
}
```

Then in each compile file:
```go
// Before (compile_syntax_form.go)
argsPair, ok := expr.(*syntax.SyntaxPair)
if !ok || argsPair.IsEmptyList() {
    return values.NewForeignError("syntax: expected exactly one argument")
}
template := argsPair.SyntaxCar()
rest, ok := argsPair.SyntaxCdr().(*syntax.SyntaxPair)
if !ok || !rest.IsEmptyList() {
    return values.NewForeignError("syntax: expected exactly one argument")
}

// After
template, err := extractSingleArg(expr, "syntax")
if err != nil {
    return err
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/machine/compile_time_continuation.go` | Add `extractSingleArg`, `extractTwoArgs`, `extractArgList` |
| `go/machine/compile_syntax_form.go` | Use `extractSingleArg` |
| `go/machine/compile_syntax_case.go` | Use `extractArgList` (has 3+ args) |
| `go/machine/compile_with_syntax.go` | Use `extractArgList` (bindings + body) |
| `go/machine/compile_define_for_syntax.go` | Use `extractArgList` (name + expr or function form) |
| `go/machine/compile_eval_when.go` | Use `extractArgList` |
| `go/machine/compile_begin_for_syntax.go` | Use `extractArgList` |

### Verification

```bash
cd go && make build && go test ./machine/... -count=1
```

---

> **Note**: Items previously numbered 4 (Compile-Time Code Execution), 5 (Type Assertion Helpers), and 6 (Optional Range Argument Parsing) have been moved to `CODE_CONSOLIDATION_PLAN.md` Phases 5, 2, and 4 respectively, where they are described in greater detail.

---

## Implementation Order

**Recommended sequence** (smallest to largest scope):

1. **#1 Form-Checking Predicates** — Single file, 20 lines, trivial
2. **#3 Form Argument Extraction** — 6 files, 50 lines, straightforward
3. **#2 CxR Primitives** — 2 files, 300 lines, mechanical but large

Each refactoring is independent and can be done separately.
