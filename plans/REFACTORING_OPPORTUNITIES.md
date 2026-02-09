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

## 4. Compile-Time Code Execution

**Status**: Ready to implement
**Impact**: ~40 lines reduced, centralizes critical semantics
**Risk**: Medium (affects compile-time evaluation)

### Problem

Three files have nearly identical compile-time execution patterns:

**`compile_begin_for_syntax.go:70-88`** (inside ForEach callback):
```go
tmpTpl := NewNativeTemplate(0, 0, false)
tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)
err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
if err != nil {
    return values.WrapForeignErrorf(err, "begin-for-syntax: compilation failed")
}
cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
mc := NewMachineContext(context.Background(), cont)
err = mc.Run()
if err != nil {
    if !errors.Is(err, ErrMachineHalt) {
        return values.WrapForeignErrorf(err, "begin-for-syntax: evaluation failed")
    }
}
```

**`compile_define_for_syntax.go:99-124`**: Same pattern, also retrieves `mc.GetValue()`.

**`compile_eval_when.go:176-192`**: Same pattern inside ForEach callback.

### Solution

Add helper to `compile_time_continuation.go`:

```go
// evalAtCompileTime compiles and executes an expression at compile time.
// Returns the resulting value and any error that occurred.
//
// This is used by begin-for-syntax, define-for-syntax, and eval-when
// to execute code during compilation.
func evalAtCompileTime(
    ctctx CompileTimeCallContext,
    expandedExpr syntax.SyntaxValue,
    expandEnv *environment.EnvironmentFrame,
) (values.Value, error) {
    // Compile to temporary template
    tmpTpl := NewNativeTemplate(0, 0, false)
    tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)
    err := tmpCcnt.CompileExpression(ctctx, expandedExpr)
    if err != nil {
        return nil, err
    }

    // Execute at compile time
    cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
    mc := NewMachineContext(context.Background(), cont)
    err = mc.Run()
    if err != nil && !errors.Is(err, ErrMachineHalt) {
        return nil, err
    }

    return mc.GetValue(), nil
}
```

Then in each file:
```go
// Before (compile_define_for_syntax.go:99-127)
tmpTpl := NewNativeTemplate(0, 0, false)
tmpCcnt := NewCompiletimeContinuation(tmpTpl, expandEnv)
err = tmpCcnt.CompileExpression(ctctx, expandedExpr)
if err != nil {
    return values.WrapForeignErrorf(err, "define-for-syntax: compilation failed")
}
cont := NewMachineContinuation(nil, tmpTpl, expandEnv)
mc := NewMachineContext(context.Background(), cont)
err = mc.Run()
if err != nil {
    if !errors.Is(err, ErrMachineHalt) {
        return values.WrapForeignErrorf(err, "define-for-syntax: evaluation failed")
    }
}
result := mc.GetValue()

// After
result, err := evalAtCompileTime(ctctx, expandedExpr, expandEnv)
if err != nil {
    return values.WrapForeignErrorf(err, "define-for-syntax: evaluation failed")
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/machine/compile_time_continuation.go` | Add `evalAtCompileTime` |
| `go/machine/compile_begin_for_syntax.go` | Use `evalAtCompileTime` (lines 70-88) |
| `go/machine/compile_define_for_syntax.go` | Use `evalAtCompileTime` (lines 99-124) |
| `go/machine/compile_eval_when.go` | Use `evalAtCompileTime` (lines 176-192) |

### Verification

```bash
cd go && make build
echo '(begin-for-syntax (display "compile time\n"))' | ../dist/scheme
echo '(define-for-syntax x 42) x' | ../dist/scheme  # Should error - x not in runtime
cd go && go test ./machine/... -count=1
```

---

## 5. Type Assertion Helpers

**Status**: Ready to implement
**Impact**: ~200+ call sites across primitive implementations
**Risk**: Medium (touches many files)

### Problem

Throughout `go/registry/core/prim_*.go`, there's a repeated pattern for extracting typed values:

```go
// Repeated ~100+ times with variations across non-test prim files:
//   prim_vectors.go (28 occurrences)
//   prim_byte_vectors.go (24 occurrences)
//   prim_strings.go (22 occurrences)
//   prim_lists.go (18 occurrences)
//   prim_arithmetic.go (8 occurrences)
//   prim_predicates.go (8 occurrences)
//   prim_pairs.go (5 occurrences)
//   prim_equality.go (2 occurrences)
//   prim_characters.go (2 occurrences)

n, ok := v.(*values.Integer)
if !ok {
    return values.WrapForeignErrorf(values.ErrNotANumber, "quotient: expected an integer but got %T", v)
}
```

Note: the `helpers/` package already has good abstractions for numeric folds (`NumericFoldVariadic`, `NumericFoldWithFirst`), comparisons (`NumericChainCompare`, `CharCompareVariadic`, `StringCompareVariadic`), and type predicates (`MakeTypePredicate`). These cover arithmetic, comparisons, and predicates well. The remaining manual assertions are concentrated in **element access and mutation operations** on vectors, bytevectors, strings, and lists — operations that extract an index, a fill value, or a specific typed element.

### Solution

Add type extraction helpers to `go/registry/helpers/`:

```go
// extract.go

// ExtractInteger extracts an integer from a value, returning an error if the type doesn't match.
func ExtractInteger(v values.Value, procName string) (*values.Integer, error) {
    n, ok := v.(*values.Integer)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer but got %T", procName, v)
    }
    return n, nil
}

// ExtractString extracts a string from a value.
func ExtractString(v values.Value, procName string) (*values.String, error) {
    s, ok := v.(*values.String)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAString, "%s: expected a string but got %T", procName, v)
    }
    return s, nil
}

// ExtractPair extracts a pair from a value.
func ExtractPair(v values.Value, procName string) (*values.Pair, error) {
    p, ok := v.(*values.Pair)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAPair, "%s: expected a pair but got %T", procName, v)
    }
    return p, nil
}

// ExtractVector extracts a vector from a value.
func ExtractVector(v values.Value, procName string) (*values.Vector, error) {
    vec, ok := v.(*values.Vector)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotAVector, "%s: expected a vector but got %T", procName, v)
    }
    return vec, nil
}

// ExtractCharacter extracts a character from a value.
func ExtractCharacter(v values.Value, procName string) (*values.Character, error) {
    ch, ok := v.(*values.Character)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotACharacter, "%s: expected a character but got %T", procName, v)
    }
    return ch, nil
}

// ExtractBytevector extracts a bytevector from a value.
func ExtractBytevector(v values.Value, procName string) (*values.Bytevector, error) {
    bv, ok := v.(*values.Bytevector)
    if !ok {
        return nil, values.WrapForeignErrorf(values.ErrNotABytevector, "%s: expected a bytevector but got %T", procName, v)
    }
    return bv, nil
}
```

Then throughout prim_*.go files:
```go
// Before
n, ok := v.(*values.Integer)
if !ok {
    return values.WrapForeignErrorf(values.ErrNotANumber, "quotient: expected an integer but got %T", v)
}

// After
n, err := helpers.ExtractInteger(v, "quotient")
if err != nil {
    return err
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/registry/helpers/extract.go` | New file with extraction helpers |
| `go/registry/core/prim_vectors.go` | Use helpers (28 sites) |
| `go/registry/core/prim_byte_vectors.go` | Use helpers (24 sites) |
| `go/registry/core/prim_strings.go` | Use helpers (22 sites) |
| `go/registry/core/prim_lists.go` | Use helpers (18 sites) |
| `go/registry/core/prim_arithmetic.go` | Use helpers (8 sites) |
| `go/registry/core/prim_predicates.go` | Use helpers (8 sites) |
| `go/registry/core/prim_pairs.go` | Use helpers (5 sites) |
| `go/registry/core/prim_equality.go` | Use helpers (2 sites) |
| `go/registry/core/prim_characters.go` | Use helpers (2 sites) |

### Verification

```bash
cd go && make build && go test ./registry/... -count=1
```

---

## 6. Optional Range Argument Parsing

**Status**: Ready to implement
**Impact**: ~120 lines reduced across 7 functions
**Risk**: Low

### Problem

Seven primitives parse optional `[start [end]]` arguments with ~20 lines of identical boilerplate each:

| Primitive | File | Pattern |
|-----------|------|---------|
| `vector->list` | `prim_vectors.go` | `[start [end]]` with vector length default |
| `vector-copy` | `prim_vectors.go` | `[start [end]]` with vector length default |
| `vector-fill!` | `prim_vectors.go` | `[start [end]]` with vector length default |
| `bytevector-copy` | `prim_byte_vectors.go` | `[start [end]]` with bytevector length default |
| `bytevector-copy!` | `prim_byte_vectors.go` | `[start [end]]` with bytevector length default |
| `string->list` | `prim_strings.go` | `[start [end]]` with string length default |
| `string-copy` | `prim_strings.go` | `[start [end]]` with string length default |

Each repeats the same logic: check if rest is non-empty, extract first integer as start, check if remainder is non-empty, extract second integer as end.

### Solution

Add helper to `go/registry/helpers/`:

```go
// range.go

// ParseOptionalRange extracts optional [start [end]] arguments from a rest parameter.
// Returns start (default 0) and end (default length) bounds.
func ParseOptionalRange(rest values.Value, length int64, name string) (start, end int64, err error) {
    start, end = 0, length
    if values.IsEmptyList(rest) {
        return
    }
    tuple, ok := rest.(values.Tuple)
    if !ok {
        return 0, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
    }
    startVal, ok := tuple.Car().(*values.Integer)
    if !ok {
        return 0, 0, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer for start but got %T", name, tuple.Car())
    }
    start = startVal.Value

    cdr := tuple.Cdr()
    if values.IsEmptyList(cdr) {
        return
    }
    tuple2, ok := cdr.(values.Tuple)
    if !ok {
        return 0, 0, values.WrapForeignErrorf(values.ErrNotAList, "%s: improper argument list", name)
    }
    endVal, ok := tuple2.Car().(*values.Integer)
    if !ok {
        return 0, 0, values.WrapForeignErrorf(values.ErrNotANumber, "%s: expected an integer for end but got %T", name, tuple2.Car())
    }
    end = endVal.Value
    return
}
```

### Files to Modify

| File | Change |
|------|--------|
| `go/registry/helpers/range.go` | New file with `ParseOptionalRange` |
| `go/registry/core/prim_vectors.go` | 3 call sites |
| `go/registry/core/prim_byte_vectors.go` | 2 call sites |
| `go/registry/core/prim_strings.go` | 2 call sites |

### Verification

```bash
cd go && make build && go test ./registry/core/... -count=1
```

---

## Implementation Order

**Recommended sequence** (smallest to largest scope):

1. **#1 Form-Checking Predicates** — Single file, 20 lines, trivial
2. **#3 Form Argument Extraction** — 6 files, 50 lines, straightforward
3. **#4 Compile-Time Execution** — 4 files, 40 lines, centralizes important semantics
4. **#6 Optional Range Parsing** — 4 files, 120 lines, mechanical
5. **#2 CxR Primitives** — 2 files, 300 lines, mechanical but large
6. **#5 Type Assertion Helpers** — Many files, biggest impact but most invasive

Each refactoring is independent and can be done separately.
