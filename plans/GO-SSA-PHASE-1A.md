# Go SSA Extension — Phase 1A Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the `(wile goast ssa)` extension with core SSA instruction mapping, enabling data-flow queries like mutation independence checking.

**Architecture:** New Go package `extensions/goastssa/` imports alist helpers exported from `extensions/goast/`. Loads packages via `go/packages`, builds SSA via `ssautil.Packages`, maps ~18 SSA instruction types to tagged-alist s-expressions using the same `(tag (field . val) ...)` encoding as goast. Each instruction includes an `(operands . ("name" ...))` field for referrer queries in Scheme.

**Tech Stack:** `golang.org/x/tools/go/ssa`, `golang.org/x/tools/go/ssa/ssautil`, `golang.org/x/tools/go/packages` (all already vendored via `golang.org/x/tools v0.42.0`)

**Design doc:** `plans/GO-STATIC-ANALYSIS.md` (full design); `plans/GO-AST.md` (existing AST extension)

---

### Task 1: Export goast alist helpers

Rename unexported helpers in `extensions/goast/helpers.go` to exported forms so downstream extensions (`goastssa`, etc.) can import them. ~229 call sites across mapper, unmapper, and primitive files.

**Files:**
- Modify: `extensions/goast/helpers.go`
- Modify: `extensions/goast/mapper.go`
- Modify: `extensions/goast/unmapper.go`
- Modify: `extensions/goast/unmapper_decl.go`
- Modify: `extensions/goast/unmapper_expr.go`
- Modify: `extensions/goast/unmapper_stmt.go`
- Modify: `extensions/goast/unmapper_types.go`
- Modify: `extensions/goast/prim_goast.go`

**Step 1: Rename helpers using gopls**

Use `go_rename_symbol` for each helper. This is safe — gopls resolves references symbolically, so even `str` won't match substrings. Run these in order:

```
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "tag", newName: "Tag")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "field", newName: "Field")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "node", newName: "Node")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "str", newName: "Str")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "sym", newName: "Sym")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "valueList", newName: "ValueList")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "getField", newName: "GetField")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "requireField", newName: "RequireField")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "requireString", newName: "RequireString")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "requireSymbol", newName: "RequireSymbol")
go_rename_symbol(file: "extensions/goast/helpers.go", symbol: "isFalse", newName: "IsFalse")
```

The sentinels (`errGoParseError`, etc.) stay unexported — they're extension-local.

**Step 2: Verify all call sites updated**

gopls rename handles all call sites automatically. Spot-check a few files to confirm:

**Step 3: Run existing tests to verify no regression**

Run: `go test -v ./extensions/goast/...`
Expected: All tests pass. No behavioral change — only names changed.

**Step 4: Run lint**

Run: `make lint`
Expected: Clean.

**Step 5: Commit**

```
feat(goast): export alist construction helpers for downstream extensions

Renames unexported helpers (tag, field, node, str, sym, etc.) to
exported forms (Tag, Field, Node, Str, Sym, etc.) so that the
SSA and callgraph extensions can import them.
```

---

### Task 2: Create goastssa extension skeleton

Create the package structure, extension registration with `LibraryNamer`, and a stub `go-ssa-build` primitive that loads a package and returns an empty list.

**Files:**
- Create: `extensions/goastssa/doc.go`
- Create: `extensions/goastssa/register.go`
- Create: `extensions/goastssa/prim_ssa.go`
- Create: `extensions/goastssa/mapper.go`
- Create: `extensions/goastssa/prim_ssa_test.go`

**Step 1: Write the failing test**

`extensions/goastssa/prim_ssa_test.go` (external test package):

```go
package goastssa_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastssa "github.com/aalpar/wile/extensions/goastssa"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastssa.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

func runScheme(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

func runSchemeExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestGoSSABuild_ReturnsListOfFunctions(t *testing.T) {
	engine := newEngine(t)

	// Load a known package, verify we get a list back.
	result := runScheme(t, engine,
		`(pair? (go-ssa-build "github.com/aalpar/wile/extensions/goast"))`)
	qt.New(t).Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoSSABuild_Errors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong arg type", code: `(go-ssa-build 42)`},
		{name: "nonexistent package", code: `(go-ssa-build "github.com/aalpar/wile/does-not-exist-xyz")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... 2>&1 | head -20`
Expected: FAIL — package doesn't exist yet.

**Step 3: Write skeleton files**

`extensions/goastssa/doc.go`:
```go
// Package goastssa exposes Go's SSA (Static Single Assignment)
// intermediate representation as Scheme s-expressions.
// Build SSA for Go packages and query data flow with standard
// Scheme list operations.
package goastssa
```

`extensions/goastssa/register.go`:
```go
package goastssa

import "github.com/aalpar/wile/registry"

// ssaExtension wraps ExtensionFunc to implement LibraryNamer.
type ssaExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast ssa) for R7RS import.
func (p *ssaExtension) LibraryName() []string {
	return []string{"wile", "goast", "ssa"}
}

// Extension is the SSA extension entry point.
var Extension registry.Extension = &ssaExtension{
	Extension: registry.NewExtension("goast-ssa", AddToRegistry),
}

// Builder aggregates all SSA registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all SSA primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-ssa-build", ParamCount: 2, IsVariadic: true,
			Impl:       PrimGoSSABuild,
			Doc:        "Builds SSA for a Go package and returns a list of ssa-func nodes.",
			ParamNames: []string{"pattern", "options"},
			Category:   "goast-ssa",
		},
	}, registry.PhaseRuntime)
	return nil
}
```

`extensions/goastssa/mapper.go`:
```go
package goastssa

import (
	"go/token"
	"go/types"

	"golang.org/x/tools/go/ssa"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type ssaMapper struct {
	fset      *token.FileSet
	positions bool
}

// mapFunction maps an ssa.Function to an (ssa-func ...) s-expression.
func (p *ssaMapper) mapFunction(fn *ssa.Function) values.Value {
	// Stub: returns minimal structure, filled in by Task 3.
	return goast.Node("ssa-func",
		goast.Field("name", goast.Str(fn.Name())),
	)
}
```

`extensions/goastssa/prim_ssa.go`:
```go
package goastssa

import (
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var errSSABuildError = werr.NewStaticError("ssa build error")

// parseSSAOpts extracts mapper options from a variadic rest-arg list.
func parseSSAOpts(rest values.Value, fset *token.FileSet) *ssaMapper {
	opts := &ssaMapper{fset: fset}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return opts
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		s, ok := pair.Car().(*values.Symbol)
		if ok {
			switch s.Key {
			case "positions":
				opts.positions = true
			}
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return opts
}

// PrimGoSSABuild implements (go-ssa-build pattern . options).
func PrimGoSSABuild(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-ssa-build")
	if err != nil {
		return err
	}

	err = security.Check(mc.Context(), security.AccessRequest{
		Resource: security.ResourceProcess,
		Action:   security.ActionLoad,
		Target:   "go",
	})
	if err != nil {
		return err
	}

	fset := token.NewFileSet()
	mapper := parseSSAOpts(mc.Arg(1), fset)

	cfg := &packages.Config{
		Mode: packages.NeedName |
			packages.NeedFiles |
			packages.NeedSyntax |
			packages.NeedTypes |
			packages.NeedTypesInfo |
			packages.NeedImports |
			packages.NeedDeps,
		Context: mc.Context(),
		Fset:    fset,
	}

	pkgs, loadErr := packages.Load(cfg, pattern.Value)
	if loadErr != nil {
		return werr.WrapForeignErrorf(errSSABuildError,
			"go-ssa-build: %s: %s", pattern.Value, loadErr)
	}

	// Check for package load errors.
	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errSSABuildError,
			"go-ssa-build: %s: %s", pattern.Value,
			strings.Join(errs, "; "))
	}

	// Build SSA.
	prog, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg != nil {
			ssaPkg.Build()
		}
	}

	// Collect source-level functions from the requested packages.
	var funcs []values.Value
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg == nil {
			continue
		}
		for _, mem := range ssaPkg.Members {
			fn, ok := mem.(*ssa.Function)
			if !ok {
				continue
			}
			if fn.Synthetic != "" {
				continue // skip compiler-generated functions
			}
			funcs = append(funcs, mapper.mapFunction(fn))
		}
		// Collect methods on named types.
		for _, mem := range ssaPkg.Members {
			typ, ok := mem.(*ssa.Type)
			if !ok {
				continue
			}
			mset := prog.MethodSets.MethodSet(types.NewPointer(typ.Type()))
			for i := 0; i < mset.Len(); i++ {
				fn := prog.MethodValue(mset.At(i))
				if fn == nil || fn.Synthetic != "" {
					continue
				}
				if fn.Pkg == ssaPkg {
					funcs = append(funcs, mapper.mapFunction(fn))
				}
			}
			// Value receiver methods.
			mset = prog.MethodSets.MethodSet(typ.Type())
			for i := 0; i < mset.Len(); i++ {
				fn := prog.MethodValue(mset.At(i))
				if fn == nil || fn.Synthetic != "" {
					continue
				}
				if fn.Pkg == ssaPkg {
					funcs = append(funcs, mapper.mapFunction(fn))
				}
			}
		}
	}

	mc.SetValue(goast.ValueList(funcs))
	return nil
}
```

**Step 3a: Run `go mod tidy`**

After importing `golang.org/x/tools/go/ssa` for the first time, run:

Run: `go mod tidy`
Expected: Updates `go.sum` with SSA package hashes. No changes to `go.mod` (x/tools already present).

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastssa/... -run TestGoSSABuild`
Expected: PASS — the primitive loads the package and returns a non-empty list.

**Step 5: Commit**

```
feat(goastssa): add extension skeleton with go-ssa-build primitive

Creates the (wile goast ssa) extension with LibraryNamer support
and a go-ssa-build primitive that loads packages, builds SSA, and
returns a list of ssa-func nodes (minimal structure for now).
```

---

### Task 3: Map Function + BasicBlock structure

Fill out `mapFunction` to include params, free vars, signature, package, and blocks. Fill out `mapBlock` with index, comment, predecessors, successors, and an empty instruction list (filled in Tasks 4-8).

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/prim_ssa_test.go`

**Step 1: Write the failing test**

Add to `prim_ssa_test.go`:

```go
func TestGoSSABuild_FunctionStructure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Cache the SSA build result.
	runScheme(t, engine, `(define funcs (go-ssa-build "github.com/aalpar/wile/extensions/goast"))`)

	t.Run("function has name", func(t *testing.T) {
		// Find a known function: PrimGoParseExpr
		result := runScheme(t, engine, `
			(let loop ((fs funcs))
				(cond
					((null? fs) #f)
					((equal? (cdr (assoc 'name (cdr (car fs)))) "PrimGoParseExpr")
					 (car fs))
					(else (loop (cdr fs)))))`)
		// Should find the function (not #f).
		c.Assert(result.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("function has params", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let ((fn (car funcs)))
				(pair? (cdr (assoc 'params (cdr fn)))))`)
		// First function should have params (it's a real function).
		// Result might be #t or #f depending on whether params is non-empty.
		// Just check that the params field exists.
		result2 := runScheme(t, engine, `
			(let ((fn (car funcs)))
				(assoc 'params (cdr fn)))`)
		c.Assert(result2.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("function has blocks", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let ((fn (car funcs)))
				(assoc 'blocks (cdr fn)))`)
		c.Assert(result.Internal(), qt.Not(qt.Equals), values.FalseValue)
	})

	t.Run("block has index", func(t *testing.T) {
		result := runScheme(t, engine, `
			(let* ((fn (car funcs))
				   (blocks (cdr (assoc 'blocks (cdr fn))))
				   (block (car blocks)))
				(cdr (assoc 'index (cdr block))))`)
		c.Assert(result.Internal(), qt.Equals, values.NewInteger(0))
	})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestGoSSABuild_FunctionStructure`
Expected: FAIL — `mapFunction` only returns name, no params/blocks.

**Step 3: Implement mapFunction and mapBlock**

Update `mapper.go`. First, add the missing imports to the import block:

```go
import (
	"fmt"
	"go/token"
	"go/types"

	"golang.org/x/tools/go/ssa"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)
```

Then replace the stub `mapFunction` and add `mapBlock`, `mapInstruction`, `valName`, `valNames`:

```go
func (p *ssaMapper) mapFunction(fn *ssa.Function) values.Value {
	params := make([]values.Value, len(fn.Params))
	for i, param := range fn.Params {
		params[i] = goast.Node("ssa-param",
			goast.Field("name", goast.Str(param.Name())),
			goast.Field("type", goast.Str(types.TypeString(param.Type(), nil))),
		)
	}

	freeVars := make([]values.Value, len(fn.FreeVars))
	for i, fv := range fn.FreeVars {
		freeVars[i] = goast.Node("ssa-free-var",
			goast.Field("name", goast.Str(fv.Name())),
			goast.Field("type", goast.Str(types.TypeString(fv.Type(), nil))),
		)
	}

	blocks := make([]values.Value, len(fn.Blocks))
	for i, b := range fn.Blocks {
		blocks[i] = p.mapBlock(b)
	}

	fields := []values.Value{
		goast.Field("name", goast.Str(fn.Name())),
		goast.Field("signature", goast.Str(fn.Signature.String())),
		goast.Field("params", goast.ValueList(params)),
		goast.Field("free-vars", goast.ValueList(freeVars)),
		goast.Field("blocks", goast.ValueList(blocks)),
	}
	if fn.Pkg != nil {
		fields = append(fields, goast.Field("pkg", goast.Str(fn.Pkg.Pkg.Path())))
	}
	return goast.Node("ssa-func", fields...)
}

func (p *ssaMapper) mapBlock(b *ssa.BasicBlock) values.Value {
	preds := make([]values.Value, len(b.Preds))
	for i, pred := range b.Preds {
		preds[i] = values.NewInteger(int64(pred.Index))
	}
	succs := make([]values.Value, len(b.Succs))
	for i, succ := range b.Succs {
		succs[i] = values.NewInteger(int64(succ.Index))
	}
	instrs := make([]values.Value, 0, len(b.Instrs))
	for _, instr := range b.Instrs {
		if instr == nil {
			continue
		}
		instrs = append(instrs, p.mapInstruction(instr))
	}
	fields := []values.Value{
		goast.Field("index", values.NewInteger(int64(b.Index))),
		goast.Field("preds", goast.ValueList(preds)),
		goast.Field("succs", goast.ValueList(succs)),
		goast.Field("instrs", goast.ValueList(instrs)),
	}
	if b.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(b.Comment)))
	}
	return goast.Node("ssa-block", fields...)
}

// mapInstruction dispatches on SSA instruction type.
// Unmapped types produce (ssa-unknown ...) nodes.
func (p *ssaMapper) mapInstruction(instr ssa.Instruction) values.Value {
	// Stub: returns unknown for all types. Filled in by Tasks 4-8.
	fields := []values.Value{
		goast.Field("go-type", goast.Str(fmt.Sprintf("%T", instr))),
	}
	if v, ok := instr.(ssa.Value); ok {
		fields = append(fields, goast.Field("name", goast.Str(v.Name())))
		fields = append(fields, goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))))
	}
	return goast.Node("ssa-unknown", fields...)
}

// valName returns the SSA value name for use as an operand reference.
func valName(v ssa.Value) values.Value {
	if v == nil {
		return values.FalseValue
	}
	return goast.Str(v.Name())
}

// valNames returns a list of SSA value name strings.
func valNames(vs []ssa.Value) values.Value {
	names := make([]values.Value, len(vs))
	for i, v := range vs {
		names[i] = goast.Str(v.Name())
	}
	return goast.ValueList(names)
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastssa/... -run TestGoSSABuild_FunctionStructure`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`

**Step 6: Commit**

```
feat(goastssa): map Function and BasicBlock structure

ssa-func nodes now include name, signature, params, free-vars,
pkg, and blocks. ssa-block nodes include index, preds, succs,
instrs (as ssa-unknown stubs), and optional comment.
```

---

### Task 4: Map arithmetic and value types (BinOp, UnOp, Const, Alloc)

Add the instruction dispatch for value-producing arithmetic instructions and structural value types. Each instruction includes an `operands` field listing its input value names.

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Create: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

`extensions/goastssa/mapper_test.go` (internal test package):

```go
package goastssa

import (
	"go/token"
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// buildSSAFromSource loads Go source, builds SSA, and returns the
// first package's named function.
func buildSSAFromSource(t *testing.T, dir, source, funcName string) *ssa.Function {
	t.Helper()
	c := qt.New(t)

	// Write source to temp dir.
	writeTestPackage(t, dir, source)

	fset := token.NewFileSet()
	cfg := &packages.Config{
		Mode: packages.NeedName | packages.NeedFiles | packages.NeedSyntax |
			packages.NeedTypes | packages.NeedTypesInfo |
			packages.NeedImports | packages.NeedDeps,
		Fset: fset,
		Dir:  dir,
	}
	pkgs, err := packages.Load(cfg, ".")
	c.Assert(err, qt.IsNil)
	c.Assert(len(pkgs), qt.Not(qt.Equals), 0)

	prog, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	_ = prog
	for _, p := range ssaPkgs {
		if p != nil {
			p.Build()
		}
	}

	fn := ssaPkgs[0].Func(funcName)
	c.Assert(fn, qt.IsNotNil, qt.Commentf("function %s not found", funcName))
	return fn
}

// writeTestPackage writes a Go source file and go.mod to a temp dir.
func writeTestPackage(t *testing.T, dir, source string) {
	t.Helper()
	c := qt.New(t)

	err := os.WriteFile(filepath.Join(dir, "go.mod"),
		[]byte("module testpkg\n\ngo 1.23\n"), 0o644)
	c.Assert(err, qt.IsNil)

	err = os.WriteFile(filepath.Join(dir, "main.go"),
		[]byte(source), 0o644)
	c.Assert(err, qt.IsNil)
}

func TestMapBinOp(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func Add(a, b int) int {
	return a + b
}
`, "Add")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	// The function should have blocks with instructions.
	// Find a ssa-binop instruction.
	found := findNodeByTag(result, "ssa-binop")
	c.Assert(found, qt.IsNotNil, qt.Commentf("expected ssa-binop in SSA of Add"))

	// Verify op field is +.
	op, ok := goast.GetField(found.(*values.Pair).Cdr(), "op")
	c.Assert(ok, qt.IsTrue)
	c.Assert(op.(*values.Symbol).Key, qt.Equals, "+")

	// Verify operands field exists and has 2 entries.
	operands, ok := goast.GetField(found.(*values.Pair).Cdr(), "operands")
	c.Assert(ok, qt.IsTrue)
	c.Assert(listLength(operands), qt.Equals, 2)
}

// findNodeByTag does a depth-first search for a node with the given tag.
func findNodeByTag(v values.Value, tag string) values.Value {
	pair, ok := v.(*values.Pair)
	if !ok {
		return nil
	}
	sym, ok := pair.Car().(*values.Symbol)
	if ok && sym.Key == tag {
		return v
	}
	// Search fields.
	fields, ok := pair.Cdr().(values.Tuple)
	if !ok {
		return nil
	}
	for !values.IsEmptyList(fields) {
		fp, ok := fields.(*values.Pair)
		if !ok {
			break
		}
		entry, ok := fp.Car().(*values.Pair)
		if ok {
			result := findNodeByTag(entry.Cdr(), tag)
			if result != nil {
				return result
			}
			// Also search lists of nodes.
			if listVal, ok := entry.Cdr().(values.Tuple); ok {
				for !values.IsEmptyList(listVal) {
					lp, ok := listVal.(*values.Pair)
					if !ok {
						break
					}
					result := findNodeByTag(lp.Car(), tag)
					if result != nil {
						return result
					}
					listVal, ok = lp.Cdr().(values.Tuple)
					if !ok {
						break
					}
				}
			}
		}
		fields, ok = fp.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return nil
}

func listLength(v values.Value) int {
	n := 0
	tuple, ok := v.(values.Tuple)
	if !ok {
		return 0
	}
	for !values.IsEmptyList(tuple) {
		n++
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return n
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapBinOp`
Expected: FAIL — `mapInstruction` returns `ssa-unknown` for all types.

**Step 3: Implement mapBinOp, mapUnOp, mapConst, mapAlloc**

Add to `mapper.go` — replace the stub `mapInstruction` with a type switch:

```go
func (p *ssaMapper) mapInstruction(instr ssa.Instruction) values.Value {
	switch v := instr.(type) {
	case *ssa.BinOp:
		return p.mapBinOp(v)
	case *ssa.UnOp:
		return p.mapUnOp(v)
	case *ssa.Alloc:
		return p.mapAlloc(v)
	// ... more cases added in Tasks 5-8
	default:
		return p.mapUnknown(instr)
	}
}

func (p *ssaMapper) mapBinOp(v *ssa.BinOp) values.Value {
	return goast.Node("ssa-binop",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("op", goast.Sym(v.Op.String())),
		goast.Field("x", valName(v.X)),
		goast.Field("y", valName(v.Y)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Y)})),
	)
}

func (p *ssaMapper) mapUnOp(v *ssa.UnOp) values.Value {
	return goast.Node("ssa-unop",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("op", goast.Sym(v.Op.String())),
		goast.Field("x", valName(v.X)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapAlloc(v *ssa.Alloc) values.Value {
	return goast.Node("ssa-alloc",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("heap", values.BoolToBoolean(v.Heap)),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapUnknown(instr ssa.Instruction) values.Value {
	fields := []values.Value{
		goast.Field("go-type", goast.Str(fmt.Sprintf("%T", instr))),
	}
	if v, ok := instr.(ssa.Value); ok {
		fields = append(fields, goast.Field("name", goast.Str(v.Name())))
		fields = append(fields, goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))))
	}
	fields = append(fields, goast.Field("operands", values.EmptyList))
	return goast.Node("ssa-unknown", fields...)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/...`
Expected: PASS.

**Step 5: Commit**

```
feat(goastssa): map BinOp, UnOp, Alloc instructions

Adds instruction dispatch type switch and mappers for arithmetic
(ssa-binop, ssa-unop) and allocation (ssa-alloc). Each instruction
includes an operands field for referrer queries in Scheme. Unmapped
types produce ssa-unknown with go-type diagnostic.
```

---

### Task 5: Map Call instruction

The Call instruction is the most complex — it wraps `CallCommon` which handles both static calls and interface invocations.

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

Add to `mapper_test.go`:

```go
func TestMapCall(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

import "fmt"

func Hello() {
	fmt.Println("hello")
}
`, "Hello")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	found := findNodeByTag(result, "ssa-call")
	c.Assert(found, qt.IsNotNil, qt.Commentf("expected ssa-call in SSA of Hello"))

	// Verify it has a func field.
	funcField, ok := goast.GetField(found.(*values.Pair).Cdr(), "func")
	c.Assert(ok, qt.IsTrue)
	c.Assert(funcField, qt.Not(qt.Equals), values.FalseValue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapCall`
Expected: FAIL — Call not in the type switch.

**Step 3: Implement mapCall**

Add to `mapper.go`:

```go
case *ssa.Call:
	return p.mapCall(v)
```

```go
func (p *ssaMapper) mapCall(v *ssa.Call) values.Value {
	fields := p.mapCallCommon(&v.Call)
	fields = append(fields, goast.Field("name", goast.Str(v.Name())))
	fields = append(fields, goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))))
	return goast.Node("ssa-call", fields...)
}

func (p *ssaMapper) mapCallCommon(c *ssa.CallCommon) []values.Value {
	args := make([]values.Value, len(c.Args))
	operands := make([]values.Value, 0, len(c.Args)+1)

	for i, a := range c.Args {
		args[i] = valName(a)
		operands = append(operands, valName(a))
	}

	fields := []values.Value{
		goast.Field("args", goast.ValueList(args)),
	}

	if c.IsInvoke() {
		// Interface method call.
		fields = append(fields,
			goast.Field("mode", goast.Sym("invoke")),
			goast.Field("method", goast.Str(c.Method.Name())),
			goast.Field("recv", valName(c.Value)),
		)
		operands = append(operands, valName(c.Value))
	} else {
		// Static or dynamic function call.
		fields = append(fields,
			goast.Field("mode", goast.Sym("call")),
			goast.Field("func", valName(c.Value)),
		)
		operands = append(operands, valName(c.Value))
	}
	fields = append(fields, goast.Field("operands", goast.ValueList(operands)))
	return fields
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/...`
Expected: PASS.

**Step 5: Commit**

```
feat(goastssa): map Call instruction with CallCommon

Handles both static calls (mode=call) and interface invocations
(mode=invoke). Includes func/method, args, and operands fields.
```

---

### Task 6: Map memory instructions (Store, FieldAddr, Field, IndexAddr, Index)

These are the critical instructions for state-trace analysis — they show where struct fields are read and written.

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

Add to `mapper_test.go`:

```go
func TestMapFieldAddr(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

type Point struct {
	X int
	Y int
}

func SetX(p *Point, v int) {
	p.X = v
}
`, "SetX")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	// Should have ssa-field-addr for p.X.
	found := findNodeByTag(result, "ssa-field-addr")
	c.Assert(found, qt.IsNotNil, qt.Commentf("expected ssa-field-addr in SSA of SetX"))

	fieldName, ok := goast.GetField(found.(*values.Pair).Cdr(), "field")
	c.Assert(ok, qt.IsTrue)
	c.Assert(fieldName.(*values.String).Value, qt.Equals, "X")

	// Should also have ssa-store.
	store := findNodeByTag(result, "ssa-store")
	c.Assert(store, qt.IsNotNil, qt.Commentf("expected ssa-store in SSA of SetX"))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapFieldAddr`

**Step 3: Implement memory instruction mappers**

Add cases to the type switch and mapper functions:

```go
case *ssa.Store:
	return p.mapStore(v)
case *ssa.FieldAddr:
	return p.mapFieldAddr(v)
case *ssa.Field:
	return p.mapField(v)
case *ssa.IndexAddr:
	return p.mapIndexAddr(v)
case *ssa.Index:
	return p.mapIndex(v)
```

```go
func (p *ssaMapper) mapStore(v *ssa.Store) values.Value {
	return goast.Node("ssa-store",
		goast.Field("addr", valName(v.Addr)),
		goast.Field("val", valName(v.Val)),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Addr), valName(v.Val)})),
	)
}

func (p *ssaMapper) mapFieldAddr(v *ssa.FieldAddr) values.Value {
	// Resolve field name from the struct type.
	structType := typesDeref(v.X.Type())
	fieldName := fieldNameAt(structType, v.Field)
	return goast.Node("ssa-field-addr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("field", goast.Str(fieldName)),
		goast.Field("field-index", values.NewInteger(int64(v.Field))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapField(v *ssa.Field) values.Value {
	structType := v.X.Type()
	fieldName := fieldNameAt(structType, v.Field)
	return goast.Node("ssa-field",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("field", goast.Str(fieldName)),
		goast.Field("field-index", values.NewInteger(int64(v.Field))),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X)})),
	)
}

func (p *ssaMapper) mapIndexAddr(v *ssa.IndexAddr) values.Value {
	return goast.Node("ssa-index-addr",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

func (p *ssaMapper) mapIndex(v *ssa.Index) values.Value {
	return goast.Node("ssa-index",
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("x", valName(v.X)),
		goast.Field("index", valName(v.Index)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.X), valName(v.Index)})),
	)
}

// typesDeref dereferences a pointer type to get the element type.
func typesDeref(t types.Type) types.Type {
	if pt, ok := t.Underlying().(*types.Pointer); ok {
		return pt.Elem()
	}
	return t
}

// fieldNameAt returns the field name at index i in a struct type.
func fieldNameAt(t types.Type, i int) string {
	st, ok := t.Underlying().(*types.Struct)
	if !ok {
		return fmt.Sprintf("field_%d", i)
	}
	if i < st.NumFields() {
		return st.Field(i).Name()
	}
	return fmt.Sprintf("field_%d", i)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/...`
Expected: PASS.

**Step 5: Commit**

```
feat(goastssa): map memory instructions (Store, FieldAddr, Field, IndexAddr, Index)

FieldAddr and Field resolve the struct field name from the type
system, providing human-readable field names alongside numeric
indices. These are the key instructions for state-trace analysis.
```

---

### Task 7: Map control flow instructions (Phi, If, Jump, Return)

These complete the core instruction set — Phi nodes are the SSA mechanism for values that depend on control flow.

**Files:**
- Modify: `extensions/goastssa/mapper.go`
- Modify: `extensions/goastssa/mapper_test.go`

**Step 1: Write the failing test**

Add to `mapper_test.go`:

```go
func TestMapControlFlow(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
`, "Max")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	// Should have ssa-if (conditional branch).
	ifNode := findNodeByTag(result, "ssa-if")
	c.Assert(ifNode, qt.IsNotNil, qt.Commentf("expected ssa-if in SSA of Max"))

	// Should have ssa-return.
	retNode := findNodeByTag(result, "ssa-return")
	c.Assert(retNode, qt.IsNotNil, qt.Commentf("expected ssa-return in SSA of Max"))

	// Multiple blocks expected.
	blocks, ok := goast.GetField(result.(*values.Pair).Cdr(), "blocks")
	c.Assert(ok, qt.IsTrue)
	c.Assert(listLength(blocks) >= 2, qt.IsTrue,
		qt.Commentf("expected multiple blocks, got %d", listLength(blocks)))
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastssa/... -run TestMapControlFlow`

**Step 3: Implement control flow mappers**

Add cases and functions:

```go
case *ssa.Phi:
	return p.mapPhi(v)
case *ssa.If:
	return p.mapIf(v)
case *ssa.Jump:
	return p.mapJump(v)
case *ssa.Return:
	return p.mapReturn(v)
```

```go
func (p *ssaMapper) mapPhi(v *ssa.Phi) values.Value {
	edges := make([]values.Value, len(v.Edges))
	operands := make([]values.Value, len(v.Edges))
	for i, e := range v.Edges {
		blockIdx := values.NewInteger(int64(v.Block().Preds[i].Index))
		edges[i] = values.NewCons(blockIdx, valName(e))
		operands[i] = valName(e)
	}
	fields := []values.Value{
		goast.Field("name", goast.Str(v.Name())),
		goast.Field("edges", goast.ValueList(edges)),
		goast.Field("type", goast.Str(types.TypeString(v.Type(), nil))),
		goast.Field("operands", goast.ValueList(operands)),
	}
	if v.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(v.Comment)))
	}
	return goast.Node("ssa-phi", fields...)
}

func (p *ssaMapper) mapIf(v *ssa.If) values.Value {
	return goast.Node("ssa-if",
		goast.Field("cond", valName(v.Cond)),
		goast.Field("then", values.NewInteger(int64(v.Block().Succs[0].Index))),
		goast.Field("else", values.NewInteger(int64(v.Block().Succs[1].Index))),
		goast.Field("operands", goast.ValueList([]values.Value{valName(v.Cond)})),
	)
}

func (p *ssaMapper) mapJump(v *ssa.Jump) values.Value {
	return goast.Node("ssa-jump",
		goast.Field("target", values.NewInteger(int64(v.Block().Succs[0].Index))),
		goast.Field("operands", values.EmptyList),
	)
}

func (p *ssaMapper) mapReturn(v *ssa.Return) values.Value {
	results := make([]values.Value, len(v.Results))
	operands := make([]values.Value, len(v.Results))
	for i, r := range v.Results {
		results[i] = valName(r)
		operands[i] = valName(r)
	}
	return goast.Node("ssa-return",
		goast.Field("results", goast.ValueList(results)),
		goast.Field("operands", goast.ValueList(operands)),
	)
}
```

**Step 4: Run tests**

Run: `go test -v ./extensions/goastssa/...`
Expected: PASS.

**Step 5: Run full lint + test**

Run: `make lint && go test -v ./extensions/goastssa/...`

**Step 6: Commit**

```
feat(goastssa): map control flow instructions (Phi, If, Jump, Return)

Phi nodes encode edges as (block-index . value-name) pairs.
If encodes then/else as successor block indices. This completes
the core instruction set for Phase 1A.
```

---

### Task 8: Integration test — state-trace mutation independence

Write an end-to-end test that uses `go-ssa-build` from Scheme to check whether struct fields are mutated independently — the exact query that motivated the SSA extension.

**Files:**
- Modify: `extensions/goastssa/prim_ssa_test.go`

**Step 1: Write the integration test**

Add to `prim_ssa_test.go`:

```go
func TestIntegration_FieldStoreQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build SSA for a package with a known struct.
	// Query: find all ssa-store instructions whose addr is an ssa-field-addr.
	// This is the Scheme-side equivalent of "find all field writes."
	result := runScheme(t, engine, `
		(define funcs (go-ssa-build "github.com/aalpar/wile/extensions/goast"))

		;; AST utilities for walking SSA s-expressions.
		(define (nf node key)
			(let ((e (assoc key (cdr node))))
				(if e (cdr e) #f)))

		(define (tag? node t)
			(and (pair? node) (eq? (car node) t)))

		;; Walk all instructions in a function, collect those matching pred.
		(define (walk-instrs fn pred)
			(let loop ((blocks (cdr (assoc 'blocks (cdr fn)))) (acc '()))
				(if (null? blocks) (reverse acc)
					(let ((instrs (cdr (assoc 'instrs (cdr (car blocks))))))
						(loop (cdr blocks)
							(let iloop ((is instrs) (a acc))
								(if (null? is) a
									(iloop (cdr is)
										(if (pred (car is))
											(cons (car is) a)
											a)))))))))

		;; Check: do any functions contain ssa-field-addr instructions?
		(define has-field-addrs
			(let loop ((fs funcs))
				(if (null? fs) #f
					(let ((addrs (walk-instrs (car fs) (lambda (i) (tag? i 'ssa-field-addr)))))
						(if (pair? addrs) #t (loop (cdr fs)))))))
		has-field-addrs
	`)

	// The goast package has struct field accesses (mapperOpts, etc.)
	// so we expect field-addr instructions.
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run test**

Run: `go test -v ./extensions/goastssa/... -run TestIntegration_FieldStoreQuery -timeout 60s`
Expected: PASS.

**Step 3: Run full suite**

Run: `make lint && make test`
Expected: All tests pass.

**Step 4: Commit**

```
feat(goastssa): add integration test for field-store query

Demonstrates the motivating use case: loading a real package,
building SSA, and querying field-addr instructions from Scheme.
Validates the complete pipeline from go-ssa-build through the
mapper to Scheme-side tree walking.
```

---

## Post-implementation checklist

- [x] All 8 tasks committed
- [x] `make lint` clean
- [x] `make test` passes (not just `./extensions/goastssa/...`)
- [x] `make covercheck` passes
- [x] `plans/GO-STATIC-ANALYSIS.md` updated: Phase 1A sub-phase status → "Complete"
- [x] Unmapped instruction types produce `ssa-unknown` with `go-type` diagnostic (no panics)

## What this enables

After Phase 1A, the state-trace script can answer "are these fields mutated independently?" by:

```scheme
(define funcs (go-ssa-build "target/package"))

;; Find all ssa-field-addr for field "Handled"
;; Check which basic blocks they appear in
;; Compare with field-addr for "Continuable"
;; If they appear in different blocks → independent mutation → not split state
```

Phase 1B (collections + concurrency) and 1C (type ops + closures) add more instruction types using the same pattern — add a case to the type switch, write a mapper function, write a test.
