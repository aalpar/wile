# ER Macro Transformer Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** Complete — ER macros fully implemented

**Goal:** Add `er-macro-transformer` to Wile's macro system, providing procedural macros with opt-in hygiene via `rename` and `compare` closures.

**Architecture:** New `ERMacroTransformer` marker type wraps a 3-arg `MachineClosure` + definition-site environment. The expander detects this type in `expandMacroInvocation`, unwraps the input form, creates `rename`/`compare` as `ForeignClosure`s, calls the transformer with 3 args, and re-wraps the result via `DatumToSyntaxValue`. The `rename` closure creates `SyntaxSymbol`s with definition-site scopes and `ResolvedBinding` for cross-library hygiene. The `compare` closure resolves both identifiers and checks binding equality.

**Tech Stack:** Go, Wile's `machine/`, `environment/`, `internal/syntax/`, `internal/schemeutil/` packages.

**Design reference:** `plans/MACRO_SYSTEM.md` (ER macro transformer section)

---

## Task 1: ERMacroTransformer Type + Return Type Change

**Files:**
- Create: `machine/er_macro_transformer.go`
- Modify: `machine/compile_transformer.go` (return type)
- Modify: `machine/compile_define_syntax.go:82` (variable type)
- Test: `machine/er_macro_transformer_test.go`

### Step 1: Write the ERMacroTransformer type test

```go
// machine/er_macro_transformer_test.go
package machine_test

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestERMacroTransformer_IsValue(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert := machine.NewERMacroTransformer(cls, env)

	// Satisfies values.Value
	var v values.Value = ert
	c.Assert(v, qt.Not(qt.IsNil))
	c.Assert(ert.SchemeString(), qt.Equals, "#<er-macro-transformer>")
	c.Assert(ert.IsVoid(), qt.IsFalse)

	// Accessors
	c.Assert(ert.Closure(), qt.Equals, cls)
	c.Assert(ert.DefEnv(), qt.Equals, env)
}

func TestERMacroTransformer_EqualTo(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert1 := machine.NewERMacroTransformer(cls, env)
	ert2 := machine.NewERMacroTransformer(cls, env)

	// Identity semantics
	c.Assert(ert1.EqualTo(ert1), qt.IsTrue)
	c.Assert(ert1.EqualTo(ert2), qt.IsFalse)
	c.Assert(ert1.EqualTo(values.TrueValue), qt.IsFalse)
}
```

### Step 2: Run test to verify it fails

```bash
go test -v -run TestERMacroTransformer ./machine/...
```
Expected: FAIL — `machine.NewERMacroTransformer` undefined.

### Step 3: Write ERMacroTransformer type

```go
// machine/er_macro_transformer.go
package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
)

var _ values.Value = (*ERMacroTransformer)(nil)

// ERMacroTransformer wraps a 3-arg MachineClosure to identify it as an
// explicit-renaming transformer in expandMacroInvocation. The defEnv
// captures the expand-time environment at the macro definition site,
// used by the rename closure to resolve definition-site bindings.
type ERMacroTransformer struct {
	closure *MachineClosure
	defEnv  *environment.EnvironmentFrame
}

func NewERMacroTransformer(closure *MachineClosure, defEnv *environment.EnvironmentFrame) *ERMacroTransformer {
	return &ERMacroTransformer{
		closure: closure,
		defEnv:  defEnv,
	}
}

func (p *ERMacroTransformer) Closure() *MachineClosure {
	return p.closure
}

func (p *ERMacroTransformer) DefEnv() *environment.EnvironmentFrame {
	return p.defEnv
}

func (p *ERMacroTransformer) IsVoid() bool {
	return p == nil
}

func (p *ERMacroTransformer) SchemeString() string {
	return "#<er-macro-transformer>"
}

func (p *ERMacroTransformer) EqualTo(o values.Value) bool {
	_, ok := o.(*ERMacroTransformer)
	if !ok {
		return false
	}
	return p == o
}
```

### Step 4: Run test to verify it passes

```bash
go test -v -run TestERMacroTransformer ./machine/...
```
Expected: PASS

### Step 5: Change `compileTransformerToMachineClosure` return type to `values.Value`

In `machine/compile_transformer.go`:
- Change function signature from `(*MachineClosure, error)` to `(values.Value, error)`
- Both existing return paths (`CompileSyntaxRules` and `compileAndEvalLambdaTransformer`) return `*MachineClosure` which satisfies `values.Value`, so no other changes needed in this file.

In `machine/compile_define_syntax.go:82`:
- Change `closure, err := compileTransformerToMachineClosure(...)` — the variable `closure` is used at line 104 as `expandEnv.SetOwnGlobalValue(globalIndex, closure)` which accepts `values.Value`. No further changes needed.

### Step 6: Verify build

```bash
go build ./machine/...
```
Expected: BUILD SUCCESS

### Step 7: Run all existing macro tests to verify no regression

```bash
go test -v -run "TestBasicHygiene|TestLetMacro|TestSyntaxRules" ./machine/...
```
Expected: All PASS (return type change is transparent).

### Step 8: Commit

```
feat(machine): add ERMacroTransformer type and widen compileTransformerToMachineClosure return
```

---

## Task 2: ER Compilation Path

**Files:**
- Modify: `machine/compile_transformer.go:66-75` (add case)
- Create: `machine/compile_er_macro.go`
- Test: `machine/er_macro_transformer_test.go` (append)

### Step 1: Write test for ER compilation

Append to `machine/er_macro_transformer_test.go`:

```go
func TestCompileERMacroTransformer(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Parse (define-syntax my-id (er-macro-transformer (lambda (form rename compare) (cadr form))))
	form := parseString(t, env, `
		(define-syntax my-id
		  (er-macro-transformer
		    (lambda (form rename compare) (cadr form))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false)
	args := extractDefineSyntaxArgs(t, form)
	err := ctc.CompileDefineSyntax(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify the binding is an ERMacroTransformer
	expandEnv := env.Expand()
	bnd := expandEnv.GetBinding(values.NewSymbol("my-id"))
	c.Assert(bnd, qt.Not(qt.IsNil))

	_, ok := bnd.Value().(*machine.ERMacroTransformer)
	c.Assert(ok, qt.IsTrue)
}
```

Note: this test reuses `createHygieneTestEnv`, `parseString`, and `extractDefineSyntaxArgs` from existing test helpers in `hygiene_test.go` and `syntax_rules_test.go`. The test file needs `context` import.

### Step 2: Run test to verify it fails

```bash
go test -v -run TestCompileERMacroTransformer ./machine/...
```
Expected: FAIL — `er-macro-transformer` not recognized.

### Step 3: Add `er-macro-transformer` case to compile_transformer.go

In `machine/compile_transformer.go:66`, add case before `default`:

```go
case "er-macro-transformer":
	return compileERMacroTransformer(ctx, env, transformerPair)
```

### Step 4: Write compileERMacroTransformer

```go
// machine/compile_er_macro.go
package machine

import (
	"context"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// compileERMacroTransformer compiles (er-macro-transformer <lambda-expr>) into
// an *ERMacroTransformer wrapping the 3-arg closure and definition-site environment.
func compileERMacroTransformer(
	ctx context.Context,
	env *environment.EnvironmentFrame,
	erForm *syntax.SyntaxPair,
) (*ERMacroTransformer, error) {
	// Extract the lambda expression from (er-macro-transformer <lambda>)
	cdr := erForm.SyntaxCdr()
	argsPair, ok := cdr.(*syntax.SyntaxPair)
	if !ok || syntax.IsSyntaxEmptyList(cdr) {
		return nil, werr.WrapForeignErrorf(
			werr.ErrInvalidSyntax,
			"er-macro-transformer: expected a lambda expression",
		)
	}

	lambdaExpr := argsPair.SyntaxCar()
	if lambdaExpr == nil {
		return nil, werr.WrapForeignErrorf(
			werr.ErrUnexpectedNil,
			"er-macro-transformer: lambda expression is nil",
		)
	}

	// Compile and evaluate the lambda to get a MachineClosure
	closure, err := compileAndEvalLambdaTransformer(ctx, env, lambdaExpr)
	if err != nil {
		return nil, werr.WrapForeignErrorf(err, "er-macro-transformer: failed to compile lambda")
	}

	// Validate arity: the lambda must accept exactly 3 parameters (form, rename, compare)
	if !closure.AcceptsArity(3) {
		return nil, werr.WrapForeignErrorf(
			werr.ErrArityMismatch,
			"er-macro-transformer: lambda must accept exactly 3 arguments (form rename compare)",
		)
	}

	// Wrap in ERMacroTransformer with the definition-site expand environment
	defEnv := env.Expand()
	return NewERMacroTransformer(closure, defEnv), nil
}
```

### Step 5: Run test to verify it passes

```bash
go test -v -run TestCompileERMacroTransformer ./machine/...
```
Expected: PASS

### Step 6: Run all macro tests for regression

```bash
go test -v -run "TestBasicHygiene|TestLetMacro|TestSyntaxRules|TestERMacro" ./machine/...
```
Expected: All PASS

### Step 7: Commit

```
feat(machine): compile er-macro-transformer to ERMacroTransformer
```

---

## Task 3: Rename and Compare Operations

**Files:**
- Create: `machine/er_macro_rename.go`
- Create: `machine/er_macro_compare.go`
- Test: `machine/er_macro_transformer_test.go` (append)

### Step 1: Write rename test

Append to `machine/er_macro_transformer_test.go`:

```go
func TestERRename_Basic(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Define a binding so rename can find it
	sym := values.NewSymbol("if")

	rename := machine.NewERRenameClosure(env.Expand(), nil)

	// Create a sub-context to call the foreign closure
	mc := machine.NewMachineContext(context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env))

	// Call rename with symbol 'if'
	_, err := mc.ApplyCallable(rename, sym)
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	stxSym, ok := result.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue, qt.Commentf("rename should return SyntaxSymbol, got %T", result))
	c.Assert(stxSym.Sym.Key, qt.Equals, "if")
}

func TestERRename_CachesResults(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	rename := machine.NewERRenameClosure(env.Expand(), nil)

	mc := machine.NewMachineContext(context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env))

	// First call
	_, err := mc.ApplyCallable(rename, values.NewSymbol("tmp"))
	c.Assert(err, qt.IsNil)
	result1 := mc.GetValue()

	// Second call with same symbol
	mc2 := machine.NewMachineContext(context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env))
	_, err = mc2.ApplyCallable(rename, values.NewSymbol("tmp"))
	c.Assert(err, qt.IsNil)
	result2 := mc2.GetValue()

	// Must be pointer-equal (eq? contract)
	c.Assert(result1, qt.Equals, result2)
}
```

### Step 2: Run test to verify it fails

```bash
go test -v -run TestERRename ./machine/...
```
Expected: FAIL — `machine.NewERRenameClosure` undefined.

### Step 3: Implement rename

```go
// machine/er_macro_rename.go
package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// NewERRenameClosure creates the `rename` closure for an ER macro invocation.
// defExpandEnv is the definition-site expand environment.
// useSiteCtx is the source context for the macro use site (may be nil).
//
// The rename closure accepts a symbol and returns a SyntaxSymbol that resolves
// to the binding at the macro definition site. Results are cached per-invocation
// so that (eq? (rename 'x) (rename 'x)) is #t.
func NewERRenameClosure(
	defExpandEnv *environment.EnvironmentFrame,
	useSiteCtx *syntax.SourceContext,
) *ForeignClosure {
	cache := make(map[string]*syntax.SyntaxSymbol)

	fn := func(mc *MachineContext) error {
		arg := mc.GetLocalValue(0)
		key, err := extractSymbolKey(arg)
		if err != nil {
			return err
		}

		// Check cache first — eq? contract
		if cached, ok := cache[key]; ok {
			mc.SetValue(cached)
			return nil
		}

		// Look up the symbol in the definition-site expand environment
		sym := values.NewSymbol(key)
		bnd := defExpandEnv.GetBinding(sym)

		var result *syntax.SyntaxSymbol
		if bnd != nil {
			// Found in expand env — use binding's scopes
			bindingScopes := bnd.Scopes()
			sctx := syntax.NewSourceContext("", "", syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
			for _, scope := range bindingScopes {
				sctx = sctx.WithScope(scope)
			}
			result = syntax.NewSyntaxSymbol(key, sctx)

			// Check for global binding to set ResolvedBinding for cross-library hygiene
			gi := defExpandEnv.GetGlobalIndex(sym)
			if gi != nil {
				result = result.WithResolvedBinding(gi).(*syntax.SyntaxSymbol)
			}
		} else {
			// Not found in expand env — check runtime env
			runtimeEnv := defExpandEnv
			// Walk to find the runtime phase
			for runtimeEnv != nil && runtimeEnv.PhaseLevel() != 0 {
				runtimeEnv = runtimeEnv.Parent()
			}
			if runtimeEnv != nil {
				bnd = runtimeEnv.GetBinding(sym)
			}

			if bnd != nil {
				bindingScopes := bnd.Scopes()
				sctx := syntax.NewSourceContext("", "", syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
				for _, scope := range bindingScopes {
					sctx = sctx.WithScope(scope)
				}
				result = syntax.NewSyntaxSymbol(key, sctx)

				gi := runtimeEnv.GetGlobalIndex(sym)
				if gi != nil {
					result = result.WithResolvedBinding(gi).(*syntax.SyntaxSymbol)
				}
			} else {
				// Not found anywhere — top-level, empty scopes
				sctx := syntax.NewSourceContext("", "", syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
				result = syntax.NewSyntaxSymbol(key, sctx)
			}
		}

		cache[key] = result
		mc.SetValue(result)
		return nil
	}

	cls := NewForeignClosure(defExpandEnv, 1, false, fn)
	cls.SetName("er-rename")
	return cls
}

// extractSymbolKey extracts the string key from a symbol or syntax symbol argument.
func extractSymbolKey(arg values.Value) (string, error) {
	switch v := arg.(type) {
	case *values.Symbol:
		return v.Key, nil
	case *syntax.SyntaxSymbol:
		return v.Sym.Key, nil
	default:
		return "", werr.WrapForeignErrorf(
			werr.ErrNotASymbol,
			"er-rename: expected a symbol, got %T", arg,
		)
	}
}
```

### Step 4: Run rename tests

```bash
go test -v -run TestERRename ./machine/...
```
Expected: PASS

### Step 5: Write compare test

Append to `machine/er_macro_transformer_test.go`:

```go
func TestERCompare_SameBinding(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := machine.NewERCompareClosure(env)

	mc := machine.NewMachineContext(context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env))

	sym := values.NewSymbol("if")
	_, err := mc.ApplyCallable(compare, sym, sym)
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	c.Assert(result, qt.Equals, values.TrueValue)
}

func TestERCompare_DifferentBindings(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()
	compare := machine.NewERCompareClosure(env)

	mc := machine.NewMachineContext(context.Background(),
		machine.NewMachineContinuation(nil, machine.NewNativeTemplate(0, 0, false), env))

	_, err := mc.ApplyCallable(compare, values.NewSymbol("if"), values.NewSymbol("let"))
	c.Assert(err, qt.IsNil)

	result := mc.GetValue()
	c.Assert(result, qt.Equals, values.FalseValue)
}
```

### Step 6: Implement compare

```go
// machine/er_macro_compare.go
package machine

import (
	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/schemeutil"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// NewERCompareClosure creates the `compare` closure for an ER macro invocation.
// useEnv is the use-site environment for resolving identifiers.
//
// The compare closure accepts two identifiers and returns #t if they refer
// to the same binding, or #f otherwise. Used for literal matching (e.g.,
// checking if an identifier is `else` or `=>`).
func NewERCompareClosure(useEnv *environment.EnvironmentFrame) *ForeignClosure {
	fn := func(mc *MachineContext) error {
		id1 := mc.GetLocalValue(0)
		id2 := mc.GetLocalValue(1)

		bnd1, name1, err := resolveERIdentifier(useEnv, id1)
		if err != nil {
			return err
		}
		bnd2, name2, err := resolveERIdentifier(useEnv, id2)
		if err != nil {
			return err
		}

		// Same binding pointer, or both unbound with same name
		same := false
		if bnd1 != nil && bnd2 != nil {
			same = bnd1 == bnd2
		} else if bnd1 == nil && bnd2 == nil {
			same = name1 == name2
		}

		mc.SetValue(schemeutil.BoolToBoolean(same))
		return nil
	}

	cls := NewForeignClosure(useEnv, 2, false, fn)
	cls.SetName("er-compare")
	return cls
}

// resolveERIdentifier resolves an identifier (symbol or syntax symbol) to its
// binding in the given environment. Returns (binding, name, error).
func resolveERIdentifier(env *environment.EnvironmentFrame, id values.Value) (*environment.Binding, string, error) {
	switch v := id.(type) {
	case *values.Symbol:
		bnd := env.GetBinding(v)
		return bnd, v.Key, nil
	case *syntax.SyntaxSymbol:
		sym := v.Sym
		scopes := v.Scopes()
		if len(scopes) > 0 {
			bnd := env.GetBindingWithScopes(sym, scopes)
			return bnd, sym.Key, nil
		}
		bnd := env.GetBinding(sym)
		return bnd, sym.Key, nil
	default:
		return nil, "", werr.WrapForeignErrorf(
			werr.ErrNotASymbol,
			"er-compare: expected a symbol, got %T", id,
		)
	}
}
```

### Step 7: Run compare tests

```bash
go test -v -run TestERCompare ./machine/...
```
Expected: PASS

### Step 8: Commit

```
feat(machine): add rename and compare closures for ER macros
```

---

## Task 4: Expander Integration

**Files:**
- Modify: `machine/expander_time_continuation.go:304-346` (expandMacroInvocation)
- Test: `machine/er_macro_transformer_test.go` (append)

This is the critical task: when the expander encounters a binding whose value is `*ERMacroTransformer`, it must unwrap the input, create rename/compare closures, call the 3-arg transformer, and re-wrap the result.

### Step 1: Write end-to-end expansion test

Append to `machine/er_macro_transformer_test.go`:

```go
func TestERMacro_EndToEnd_Identity(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Define: (define-syntax my-id (er-macro-transformer (lambda (form rename compare) (cadr form))))
	form := parseString(t, env, `
		(define-syntax my-id
		  (er-macro-transformer
		    (lambda (form rename compare) (cadr form))))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false)
	args := extractDefineSyntaxArgs(t, form)
	err := ctc.CompileDefineSyntax(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Expand: (my-id 42) should expand to 42
	testForm := parseString(t, env, `(my-id 42)`)
	etc := machine.NewExpanderTimeContinuation(context.Background(), env)
	expanded, err := etc.ExpandExpression(testForm)
	c.Assert(err, qt.IsNil)

	// The result should be a syntax object wrapping 42
	t.Logf("Expanded: %s", expanded.SchemeString())
	unwrapped := expanded.UnwrapAll()
	c.Assert(unwrapped.SchemeString(), qt.Equals, "42")
}

func TestERMacro_Hygiene_Rename(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Define a let1 macro first (needed by the ER macro output)
	letForm := parseString(t, env, `
		(define-syntax let1
		  (syntax-rules ()
		    ((let1 ((name val) ...) body ...)
		     ((lambda (name ...) (begin body ...)) val ...))))
	`)
	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false)
	args := extractDefineSyntaxArgs(t, letForm)
	err := ctc.CompileDefineSyntax(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Define my-or using ER macro with rename for hygiene
	orForm := parseString(t, env, `
		(define-syntax my-or
		  (er-macro-transformer
		    (lambda (form rename compare)
		      (let1 ((a (cadr form))
		             (b (caddr form)))
		        (list (rename 'let1)
		              (list (list (rename 'tmp) a))
		              (list (rename 'if) (rename 'tmp) (rename 'tmp) b))))))
	`)
	ctc2 := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx2 := machine.NewCompileTimeCallContext(context.Background(), false)
	args2 := extractDefineSyntaxArgs(t, orForm)
	err = ctc2.CompileDefineSyntax(ctctx2, args2)
	c.Assert(err, qt.IsNil)

	// Expand (my-or x y)
	testForm := parseString(t, env, `(my-or x y)`)
	etc := machine.NewExpanderTimeContinuation(context.Background(), env)
	expanded, err := etc.ExpandExpression(testForm)
	c.Assert(err, qt.IsNil)

	t.Logf("Expanded: %s", expanded.SchemeString())
	// Should compile without error (renamed symbols resolve correctly)
	ctc3 := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx3 := machine.NewCompileTimeCallContext(context.Background(), false)
	err = ctc3.CompileExpression(ctctx3, expanded)
	c.Assert(err, qt.IsNil)
}
```

### Step 2: Run test to verify it fails

```bash
go test -v -run "TestERMacro_EndToEnd|TestERMacro_Hygiene" ./machine/...
```
Expected: FAIL — expander doesn't know about `*ERMacroTransformer`.

### Step 3: Modify expandMacroInvocation

In `machine/expander_time_continuation.go`, modify `expandMacroInvocation` (around line 306). Before the existing `cls, ok := bnd.Value().(Closure)` line, add ER macro detection:

```go
func (p *ExpanderTimeContinuation) expandMacroInvocation(sym *syntax.SyntaxSymbol, expr syntax.SyntaxValue, bnd *environment.Binding) (syntax.SyntaxValue, error) {
	// Check for ER macro transformer first
	if erTransformer, ok := bnd.Value().(*ERMacroTransformer); ok {
		return p.expandERMacroInvocation(sym, expr, erTransformer)
	}

	// Existing code continues unchanged...
	cls, ok := bnd.Value().(Closure)
	// ...
```

### Step 4: Write expandERMacroInvocation

Add this method to `machine/expander_time_continuation.go` (after `expandMacroInvocation`):

```go
// expandERMacroInvocation handles expansion of explicit-renaming macro invocations.
// It unwraps the input form, creates rename/compare closures, calls the 3-arg
// transformer, and re-wraps the result back to syntax for recursive expansion.
func (p *ExpanderTimeContinuation) expandERMacroInvocation(
	sym *syntax.SyntaxSymbol,
	expr syntax.SyntaxValue,
	erTransformer *ERMacroTransformer,
) (syntax.SyntaxValue, error) {
	// Build complete input form: (macro-name . args)
	inputForm := syntax.NewSyntaxCons(sym, expr, sym.SourceContext())

	// Unwrap to raw s-expression for the transformer
	rawForm := inputForm.UnwrapAll()

	// Create rename closure (captures definition-site expand env)
	renameCls := NewERRenameClosure(erTransformer.DefEnv(), sym.SourceContext())

	// Create compare closure (captures use-site env)
	compareCls := NewERCompareClosure(p.env)

	// Invoke the 3-arg transformer: (transformer form rename compare)
	expanderCtx := NewExpanderContext(p.env, p)

	mc := acquireMacroContext(p.ctx, erTransformer.Closure())
	if expanderCtx != nil {
		mc.SetExpanderContext(expanderCtx)
	}
	_, err := mc.Apply(erTransformer.Closure(), rawForm, renameCls, compareCls)
	if err != nil {
		ReleaseSubContext(mc)
		return nil, werr.WrapForeignErrorf(err, "er-macro-transformer: failed to apply transformer")
	}
	err = mc.Run()
	if err != nil {
		ReleaseSubContext(mc)
		return nil, werr.WrapForeignErrorf(err, "er-macro-transformer: transformer raised an error")
	}
	defer ReleaseSubContext(mc)

	result := mc.GetValue()
	if values.IsVoid(result) {
		return nil, werr.WrapForeignErrorf(werr.ErrUnexpectedNil, "er-macro-transformer: transformer produced no result")
	}

	// Re-wrap the result to syntax.
	// Already-SyntaxValue nodes (e.g., from rename) pass through unchanged.
	// Raw symbols get use-site source context (no special scopes = use-site resolution).
	wrapped := schemeutil.DatumToSyntaxValue(p.ctx, sym.SourceContext(), result)

	// Recursively expand the result
	return p.ExpandExpression(wrapped)
}
```

Add necessary import to `expander_time_continuation.go`:
```go
"github.com/aalpar/wile/internal/schemeutil"
```

### Step 5: Run tests

```bash
go test -v -run "TestERMacro_EndToEnd|TestERMacro_Hygiene" ./machine/...
```
Expected: PASS

### Step 6: Run full macro test suite for regression

```bash
go test -v -run "Test.*Hygiene|Test.*Macro|Test.*Syntax" ./machine/...
```
Expected: All PASS

### Step 7: Commit

```
feat(machine): integrate ER macros into expander dispatch
```

---

## Task 5: Primitive Expander Registration

**Files:**
- Modify: `machine/primitive_expanders_registry.go:36-73`

### Step 1: Register `er-macro-transformer` as pass-through

In `machine/primitive_expanders_registry.go`, add to the `primitives` slice (in the "Forms that return unchanged" group, after `"syntax-case"`):

```go
{"er-macro-transformer", (*ExpanderTimeContinuation).expandUnchanged},
```

This prevents the expander from trying to expand `er-macro-transformer` as a procedure call when it appears inside `define-syntax`.

### Step 2: Verify build and no regression

```bash
go build ./machine/... && go test -v -run "Test.*Hygiene|Test.*Macro|Test.*Syntax|TestERMacro" ./machine/...
```
Expected: BUILD SUCCESS, all PASS

### Step 3: Commit

```
feat(machine): register er-macro-transformer as primitive expander
```

---

## Task 6: Generalize let-syntax Transformer Compilation

**Files:**
- Modify: `machine/expander_let_syntax.go:177-197`

### Step 1: Write test for ER macro in let-syntax

Append to `machine/er_macro_transformer_test.go`:

```go
func TestERMacro_InLetSyntax(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Parse a let-syntax with ER macro
	form := parseString(t, env, `
		(let-syntax
		  ((my-id (er-macro-transformer
		            (lambda (form rename compare) (cadr form)))))
		  (my-id 99))
	`)

	etc := machine.NewExpanderTimeContinuation(context.Background(), env)
	expanded, err := etc.ExpandExpression(form)
	c.Assert(err, qt.IsNil)

	t.Logf("Expanded: %s", expanded.SchemeString())
	unwrapped := expanded.UnwrapAll()
	c.Assert(unwrapped.SchemeString(), qt.Equals, "99")
}
```

### Step 2: Run test to verify it fails

```bash
go test -v -run TestERMacro_InLetSyntax ./machine/...
```
Expected: FAIL — "only syntax-rules transformers are currently supported"

### Step 3: Replace hardcoded syntax-rules with compileTransformerToMachineClosure

In `machine/expander_let_syntax.go`, replace lines 177-197 (the hardcoded `syntax-rules` check and `CompileSyntaxRules` call) with a call to the unified transformer compiler:

Replace this block:
```go
		// Check if transformer is a syntax-rules form
		transformerPairExpr, ok := transformerExpr.(*syntax.SyntaxPair)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}
		car := transformerPairExpr.SyntaxCar()
		if car == nil {
			return nil, werr.WrapForeignErrorf(werr.ErrUnsupportedTransformer, "%s: invalid transformer", formName)
		}
		srSym, ok := car.(*syntax.SyntaxSymbol)
		if !ok {
			return nil, werr.WrapForeignErrorf(werr.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}
		srSymVal := srSym.Unwrap()
		srSymbol, ok := srSymVal.(*values.Symbol)
		if !ok || srSymbol.Key != "syntax-rules" {
			return nil, werr.WrapForeignErrorf(werr.ErrUnsupportedTransformer, "%s: only syntax-rules transformers are currently supported", formName)
		}

		// Compile the syntax-rules transformer
		closure, err := CompileSyntaxRules(p.ctx, p.env, transformerPairExpr, p.libraryScope)
```

With:
```go
		// Compile the transformer (supports syntax-rules, lambda, er-macro-transformer)
		closure, err := compileTransformerToMachineClosure(p.ctx, p.env, transformerExpr, p.libraryScope)
```

The rest of the code stores `closure` via `SetLocalValue` which accepts `values.Value`, so no further changes are needed.

### Step 4: Run test

```bash
go test -v -run TestERMacro_InLetSyntax ./machine/...
```
Expected: PASS

### Step 5: Run let-syntax regression tests

```bash
go test -v -run "TestLetSyntax|TestLetrecSyntax|TestERMacro" ./machine/...
```
Expected: All PASS

### Step 6: Commit

```
refactor(machine): generalize let-syntax to accept any transformer type
```

---

## Task 7: Integration Tests via Engine.Eval

**Files:**
- Create: `integration/er_macro_test.go`

These tests exercise the full pipeline: parse -> expand -> compile -> run.

### Step 1: Write integration tests

```go
// integration/er_macro_test.go
package integration_test

import (
	"testing"
	"time"
)

func TestERMacro_BasicIdentity(t *testing.T) {
	runSchemeTest(t, "er_macro_basic.scm", 15*time.Second, "ER macro basic")
}

func TestERMacro_Hygiene(t *testing.T) {
	runSchemeTest(t, "er_macro_hygiene.scm", 15*time.Second, "ER macro hygiene")
}

func TestERMacro_Compare(t *testing.T) {
	runSchemeTest(t, "er_macro_compare.scm", 15*time.Second, "ER macro compare")
}
```

### Step 2: Create test data files

Create `integration/testdata/er_macro_basic.scm`:

```scheme
;; Basic ER macro tests

;; Identity macro — returns the second element of the form
(define-syntax my-id
  (er-macro-transformer
    (lambda (form rename compare)
      (cadr form))))

;; Test basic expansion
(define result (my-id 42))
(if (= result 42)
    (display "PASS: my-id basic\n")
    (begin (display "FAIL: my-id basic, got ")
           (display result)
           (newline)
           (exit 1)))

;; Swap macro using rename for hygiene
(define-syntax my-swap!
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        `(,(rename 'let) ((,(rename 'tmp) ,a))
           (,(rename 'set!) ,a ,b)
           (,(rename 'set!) ,b ,(rename 'tmp)))))))

(define x 1)
(define y 2)
(my-swap! x y)
(if (and (= x 2) (= y 1))
    (display "PASS: my-swap!\n")
    (begin (display "FAIL: my-swap!, got x=")
           (display x)
           (display " y=")
           (display y)
           (newline)
           (exit 1)))

(display "All basic ER macro tests passed\n")
```

Create `integration/testdata/er_macro_hygiene.scm`:

```scheme
;; ER macro hygiene tests — renamed symbols don't capture user variables

(define-syntax my-or
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((a (cadr form))
            (b (caddr form)))
        `(,(rename 'let) ((,(rename 'tmp) ,a))
           (,(rename 'if) ,(rename 'tmp) ,(rename 'tmp) ,b))))))

;; Test 1: basic operation
(define r1 (my-or #f 42))
(if (= r1 42)
    (display "PASS: my-or basic\n")
    (begin (display "FAIL: my-or basic, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Test 2: hygiene — user's 'tmp' is NOT captured by macro's renamed 'tmp'
(define tmp 99)
(define r2 (my-or #f tmp))
(if (= r2 99)
    (display "PASS: my-or hygiene\n")
    (begin (display "FAIL: my-or hygiene, got ")
           (display r2)
           (newline)
           (exit 1)))

;; Test 3: un-renamed symbols resolve at use site (intentional hygiene breaking)
(define-syntax aif
  (er-macro-transformer
    (lambda (form rename compare)
      (let ((test (cadr form))
            (then (caddr form))
            (els  (if (null? (cdddr form)) #f (cadddr form))))
        ;; 'it' is NOT renamed — intentionally anaphoric
        `(,(rename 'let) ((it ,test))
           (,(rename 'if) it ,then ,els))))))

(define r3 (aif (+ 1 2) (* it 10) 0))
(if (= r3 30)
    (display "PASS: aif anaphoric\n")
    (begin (display "FAIL: aif anaphoric, got ")
           (display r3)
           (newline)
           (exit 1)))

(display "All hygiene ER macro tests passed\n")
```

Create `integration/testdata/er_macro_compare.scm`:

```scheme
;; ER macro compare tests — literal matching via compare closure

(define-syntax my-cond
  (er-macro-transformer
    (lambda (form rename compare)
      (let loop ((clauses (cdr form)))
        (if (null? clauses)
            (rename 'void)
            (let ((clause (car clauses))
                  (rest (cdr clauses)))
              (if (compare (car clause) (rename 'else))
                  ;; else clause — expand body
                  `(,(rename 'begin) ,@(cdr clause))
                  ;; normal clause
                  `(,(rename 'if) ,(car clause)
                     (,(rename 'begin) ,@(cdr clause))
                     ,(loop rest)))))))))

;; Test 1: normal clauses
(define r1
  (my-cond
    (#f 1)
    (#t 2)
    (else 3)))
(if (= r1 2)
    (display "PASS: my-cond normal\n")
    (begin (display "FAIL: my-cond normal, got ")
           (display r1)
           (newline)
           (exit 1)))

;; Test 2: else clause
(define r2
  (my-cond
    (#f 1)
    (#f 2)
    (else 99)))
(if (= r2 99)
    (display "PASS: my-cond else\n")
    (begin (display "FAIL: my-cond else, got ")
           (display r2)
           (newline)
           (exit 1)))

(display "All compare ER macro tests passed\n")
```

### Step 3: Build and run integration tests

```bash
make build && go test -v -run "TestERMacro" ./integration/...
```
Expected: All PASS

### Step 4: Commit

```
test(integration): add ER macro integration tests
```

---

## Task 8: Lint, Coverage, Final Verification

### Step 1: Run lint

```bash
make lint
```
Fix any issues.

### Step 2: Run coverage check

```bash
make covercheck
```
Fix any issues.

### Step 3: Run full test suite

```bash
make test
```
Expected: All PASS

### Step 4: Commit any lint/coverage fixes

```
chore: fix lint and coverage for ER macro implementation
```

---

## Task 9: Update TODO.md and Plan Status

### Step 1: Mark TODO item done

In `TODO.md`, change line 115:
```
- [ ] **ER macro transformer** [Macro system]: ...
```
to:
```
- [x] **ER macro transformer** [Macro system]: ...
```

### Step 2: Update plan status

In `plans/MACRO_SYSTEM.md`, change line 8:
```
**Status:** Proposed
```
to:
```
**Status:** Complete
```

### Step 3: Commit

```
docs: mark ER macro transformer complete
```

---

## Summary of Files Changed

| File | Change |
|------|--------|
| `machine/er_macro_transformer.go` | **New** — ERMacroTransformer type |
| `machine/compile_er_macro.go` | **New** — ER compilation logic |
| `machine/er_macro_rename.go` | **New** — rename ForeignClosure |
| `machine/er_macro_compare.go` | **New** — compare ForeignClosure |
| `machine/compile_transformer.go` | Return type `values.Value`, add `er-macro-transformer` case |
| `machine/compile_define_syntax.go` | Variable type follows return type change |
| `machine/expander_time_continuation.go` | ER detection + `expandERMacroInvocation` method |
| `machine/expander_let_syntax.go` | Generalize to use `compileTransformerToMachineClosure` |
| `machine/primitive_expanders_registry.go` | Register `er-macro-transformer` |
| `machine/er_macro_transformer_test.go` | **New** — unit tests |
| `integration/er_macro_test.go` | **New** — integration test driver |
| `integration/testdata/er_macro_basic.scm` | **New** — basic ER macro tests |
| `integration/testdata/er_macro_hygiene.scm` | **New** — hygiene tests |
| `integration/testdata/er_macro_compare.scm` | **New** — compare/literal tests |
| `TODO.md` | Mark ER macro item complete |
| `plans/MACRO_SYSTEM.md` | Update status |
