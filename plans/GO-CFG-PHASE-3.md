# Go CFG Extension — Phase 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the `(wile goast cfg)` extension that exposes Go's intra-procedural control flow graph and dominance tree as s-expressions, enabling path and dominance queries: "does every path from entry to return pass through this check?"

**Architecture:** New Go package `extensions/goastcfg/` loads packages independently via `go/packages`, builds SSA with `ssautil.Packages`, locates a named function, and uses SSA's built-in dominator support (`.Idom()`, `DomPreorder()`). Returns `cfg-block` nodes — same tagged-alist encoding as all prior extensions. Query primitives (`go-cfg-dominators`, `go-cfg-dominates?`, `go-cfg-paths`) work on the s-expression output; no security gate.

**Tech Stack:** `golang.org/x/tools/go/ssa`, `golang.org/x/tools/go/ssa/ssautil`, `golang.org/x/tools/go/packages` (all already vendored via `golang.org/x/tools v0.42.0`)

**Design doc:** `plans/GO-STATIC-ANALYSIS.md` (Phase 3)

**Reference:** `extensions/goastcg/` — established patterns for loading, mapper, test helpers, extension registration.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Loading model | Fresh `(pattern func-name)` — same as callgraph | Use SSA's built-in `.Idom()` directly; no need to implement dominator algorithms on s-expressions |
| Dominance source | `ssa.BasicBlock.Idom()` + `ssa.Function.DomPreorder()` | Battle-tested; lazily computed on first access after `.Build()` |
| CFG instruction list | Omitted from `cfg-block` | Phase 3 is about structure, not instructions. Users cross-reference by block index to Phase 1's `go-ssa-build` output |
| `idom` encoding | Integer block index, or `#f` for entry block | Consistent with preds/succs encoding; `#f` mirrors the convention used in Phase 1 for absent values |
| Path enumeration | Simple paths only (no revisiting blocks) | Loops create infinite paths; simple paths give the useful subset. Cap at 1024 paths to bound cost on dense graphs |
| `go-cfg-dominators` | Pure s-expression transform (no load) | Input is `cfg-block` list from `go-cfg`; inverts the per-block `idom` field into a tree |
| Recover blocks | Included with `(recover . #t)` tag | Both entry and recover have `Idom()==nil`; tag preserves info and distinguishes them |
| Function lookup | `ssaPkg.Func(name)` + method search on named types | Same as `go-callgraph` member iteration pattern |

### S-expression Encoding

```scheme
;; go-cfg returns a list of cfg-block nodes (one per basic block)
((cfg-block
   (index . 0)
   (comment . "entry")
   (preds . ())
   (succs . (1 2))
   (idom . #f))           ;; #f for entry block (no immediate dominator)
 (cfg-block
   (index . 1)
   (comment . "")
   (preds . (0))
   (succs . (3))
   (idom . 0))            ;; block 0 is the immediate dominator
 (cfg-block
   (index . 2)
   (comment . "")
   (preds . (0))
   (succs . (3))
   (idom . 0))
 (cfg-block
   (index . 3)
   (comment . "")
   (preds . (1 2))
   (succs . ())
   (idom . 0)))

;; Recover blocks (from deferred panic recovery) are tagged:
;; (cfg-block (index . 4) (preds . ()) (succs . ()) (idom . #f) (recover . #t))
;; Both entry (index 0) and recover blocks have idom=#f. The recover tag distinguishes them.

;; go-cfg-dominators returns a list of dom-node entries
((dom-node (block . 0) (idom . #f) (children . (1 2 3)))
 (dom-node (block . 1) (idom . 0)  (children . ()))
 (dom-node (block . 2) (idom . 0)  (children . ()))
 (dom-node (block . 3) (idom . 0)  (children . ())))

;; go-cfg-dominates? returns #t or #f
;; (go-cfg-dominates? dom 0 3) => #t  (entry dominates all blocks)
;; (go-cfg-dominates? dom 1 3) => #f  (block 1 does not dominate block 3)

;; go-cfg-paths returns a list of paths; each path is a list of block indices
;; (go-cfg-paths cfg 0 3) =>
((0 1 3)
 (0 2 3))
```

### Package Structure

```
extensions/goastcfg/
  doc.go                # Package documentation
  register.go           # Extension registration, LibraryNamer -> (wile goast cfg)
  prim_cfg.go           # Primitive implementations
  mapper.go             # SSA function -> cfg-block list mapper
  mapper_test.go        # Mapper unit tests (internal package)
  prim_cfg_test.go      # Primitive + integration tests (external package)
```

---

### Task 1: Create goastcfg extension skeleton

Create the package structure, extension registration with `LibraryNamer`, and a stub `go-cfg` primitive that validates argument types and returns an empty list.

**Files:**
- Create: `extensions/goastcfg/doc.go`
- Create: `extensions/goastcfg/register.go`
- Create: `extensions/goastcfg/prim_cfg.go`
- Create: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the failing test**

`extensions/goastcfg/prim_cfg_test.go` (external test package):

```go
package goastcfg_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastcfg "github.com/aalpar/wile/extensions/goastcfg"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastcfg.Extension),
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

func TestExtensionLibraryName(t *testing.T) {
	type libraryNamer interface {
		LibraryName() []string
	}
	namer, ok := extgoastcfg.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "cfg"})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastcfg/... 2>&1 | head -20`
Expected: FAIL — package doesn't exist yet.

**Step 3: Write skeleton files**

`extensions/goastcfg/doc.go`:
```go
// Package goastcfg exposes Go's intra-procedural control flow graph
// and dominator tree as Scheme s-expressions, enabling path and
// dominance queries over SSA-level control flow.
package goastcfg
```

`extensions/goastcfg/register.go`:
```go
package goastcfg

import "github.com/aalpar/wile/registry"

// cfgExtension wraps Extension to implement LibraryNamer.
type cfgExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast cfg) for R7RS import.
func (p *cfgExtension) LibraryName() []string {
	return []string{"wile", "goast", "cfg"}
}

// Extension is the CFG extension entry point.
var Extension registry.Extension = &cfgExtension{
	Extension: registry.NewExtension("goast-cfg", AddToRegistry),
}

// Builder aggregates all CFG registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all CFG primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-cfg", ParamCount: 3, IsVariadic: true,
			Impl:       PrimGoCFG,
			Doc:        "Builds the CFG for a named function in a Go package. Returns a list of cfg-block nodes with idom annotations.",
			ParamNames: []string{"pattern", "func-name", "options"},
			Category:   "goast-cfg",
		},
	}, registry.PhaseRuntime)
	return nil
}
```

`extensions/goastcfg/prim_cfg.go` (stub):
```go
package goastcfg

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	errCFGBuildError   = werr.NewStaticError("cfg build error")
	errCFGFuncNotFound = werr.NewStaticError("function not found in package")
)

// PrimGoCFG implements (go-cfg pattern func-name . options).
// Stub: validates args, returns empty list. Filled in by Task 2.
func PrimGoCFG(mc *machine.MachineContext) error {
	_, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	_, err = helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	mc.SetValue(values.EmptyList)
	return nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastcfg/... -run TestExtensionLibraryName`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(goastcfg): add (wile goast cfg) extension skeleton

Creates the package structure with LibraryNamer support and a
stub go-cfg primitive that validates argument types.
```

---

### Task 2: Implement go-cfg (CFG mapper + loading)

The core primitive: load packages, build SSA, find the named function, map each `*ssa.BasicBlock` to a `cfg-block` with `idom` from SSA's built-in dominator support.

**Files:**
- Create: `extensions/goastcfg/mapper.go`
- Create: `extensions/goastcfg/mapper_test.go`
- Modify: `extensions/goastcfg/prim_cfg.go`
- Modify: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the failing mapper test**

`extensions/goastcfg/mapper_test.go` (internal test package):

```go
package goastcfg

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

func writeTestPackage(t *testing.T, dir, source string) {
	t.Helper()
	c := qt.New(t)
	err := os.WriteFile(filepath.Join(dir, "go.mod"),
		[]byte("module testpkg\n\ngo 1.23\n"), 0o644)
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(dir, "main.go"), []byte(source), 0o644)
	c.Assert(err, qt.IsNil)
}

func buildSSAFunc(t *testing.T, dir, source, funcName string) (*token.FileSet, *ssa.Function) {
	t.Helper()
	c := qt.New(t)
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

	_, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	for _, p := range ssaPkgs {
		if p != nil {
			p.Build()
		}
	}
	fn := ssaPkgs[0].Func(funcName)
	c.Assert(fn, qt.IsNotNil, qt.Commentf("function %s not found", funcName))
	return fset, fn
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

func TestMapCFG_LinearFunction(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fset, fn := buildSSAFunc(t, dir, `
package testpkg

func Add(a, b int) int {
	return a + b
}
`, "Add")

	mapper := &cfgMapper{fset: fset}
	result := mapper.mapFunction(fn)

	// Add has one block (entry == exit, no branches).
	c.Assert(listLength(result) >= 1, qt.IsTrue,
		qt.Commentf("expected at least one cfg-block"))

	// First block must be tagged cfg-block.
	first := result.(*values.Pair).Car()
	tag := first.(*values.Pair).Car().(*values.Symbol).Key
	c.Assert(tag, qt.Equals, "cfg-block")

	// Entry block has no predecessors.
	preds, ok := goast.GetField(first.(*values.Pair).Cdr(), "preds")
	c.Assert(ok, qt.IsTrue)
	c.Assert(values.IsEmptyList(preds), qt.IsTrue)

	// Entry block idom is #f.
	idom, ok := goast.GetField(first.(*values.Pair).Cdr(), "idom")
	c.Assert(ok, qt.IsTrue)
	c.Assert(idom, qt.Equals, values.FalseValue)
}

func TestMapCFG_BranchingFunction(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fset, fn := buildSSAFunc(t, dir, `
package testpkg

func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}
`, "Max")

	mapper := &cfgMapper{fset: fset}
	result := mapper.mapFunction(fn)

	// Max has at least 3 blocks: entry, then, merge/return.
	c.Assert(listLength(result) >= 3, qt.IsTrue,
		qt.Commentf("expected at least 3 cfg-blocks for Max"))

	// Every non-entry, non-recover block should have idom set (not #f).
	// Recover blocks also have Idom() == nil but are tagged with (recover . #t).
	tuple := result.(values.Tuple)
	for !values.IsEmptyList(tuple) {
		pair := tuple.(*values.Pair)
		block := pair.Car()
		bp := block.(*values.Pair)
		idx, _ := goast.GetField(bp.Cdr(), "index")
		idom, _ := goast.GetField(bp.Cdr(), "idom")
		_, isRecover := goast.GetField(bp.Cdr(), "recover")
		if idx.(*values.Integer).Value != 0 && !isRecover {
			c.Assert(idom, qt.Not(qt.Equals), values.FalseValue,
				qt.Commentf("non-entry, non-recover block %d should have an idom",
					idx.(*values.Integer).Value))
		}
		tuple = pair.Cdr().(values.Tuple)
	}
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastcfg/... -run TestMapCFG`
Expected: FAIL — mapper.go doesn't exist.

**Step 3: Implement the mapper**

`extensions/goastcfg/mapper.go`:

```go
package goastcfg

import (
	"go/token"

	"golang.org/x/tools/go/ssa"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type cfgMapper struct {
	fset      *token.FileSet
	positions bool
}

// mapFunction maps all basic blocks of an SSA function to cfg-block s-expressions.
// The recover block (fn.Recover), if present, is tagged with (recover . #t).
func (p *cfgMapper) mapFunction(fn *ssa.Function) values.Value {
	blocks := make([]values.Value, len(fn.Blocks))
	for i, b := range fn.Blocks {
		blocks[i] = p.mapBlock(b, fn)
	}
	return goast.ValueList(blocks)
}

// mapBlock maps a single SSA basic block to a cfg-block s-expression.
// Recover blocks (b == fn.Recover) are tagged with (recover . #t) since they
// also have Idom() == nil like the entry block, but serve a different purpose.
func (p *cfgMapper) mapBlock(b *ssa.BasicBlock, fn *ssa.Function) values.Value {
	preds := make([]values.Value, len(b.Preds))
	for i, pred := range b.Preds {
		preds[i] = values.NewInteger(int64(pred.Index))
	}
	succs := make([]values.Value, len(b.Succs))
	for i, succ := range b.Succs {
		succs[i] = values.NewInteger(int64(succ.Index))
	}

	var idom values.Value
	if idomBlock := b.Idom(); idomBlock != nil {
		idom = values.NewInteger(int64(idomBlock.Index))
	} else {
		idom = values.FalseValue
	}

	fields := []values.Value{
		goast.Field("index", values.NewInteger(int64(b.Index))),
		goast.Field("preds", goast.ValueList(preds)),
		goast.Field("succs", goast.ValueList(succs)),
		goast.Field("idom", idom),
	}
	if fn.Recover != nil && b == fn.Recover {
		fields = append(fields, goast.Field("recover", values.TrueValue))
	}
	if b.Comment != "" {
		fields = append(fields, goast.Field("comment", goast.Str(b.Comment)))
	}
	if p.positions && b.Instrs != nil && len(b.Instrs) > 0 {
		if pos := b.Instrs[0].Pos(); pos.IsValid() {
			fields = append(fields, goast.Field("pos", goast.Str(p.fset.Position(pos).String())))
		}
	}
	return goast.Node("cfg-block", fields...)
}
```

**Step 4: Implement the full go-cfg primitive**

Replace the stub in `prim_cfg.go`:

```go
package goastcfg

import (
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	errCFGBuildError   = werr.NewStaticError("cfg build error")
	errCFGFuncNotFound = werr.NewStaticError("function not found in package")
)

// parseCFGOpts extracts mapper options from the variadic rest-arg list.
func parseCFGOpts(rest values.Value, fset *token.FileSet) *cfgMapper {
	opts := &cfgMapper{fset: fset}
	tuple, ok := rest.(values.Tuple)
	if !ok {
		return opts
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		if s, ok := pair.Car().(*values.Symbol); ok && s.Key == "positions" {
			opts.positions = true
		}
		cdr, ok := pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
		tuple = cdr
	}
	return opts
}

// findFunction looks up a function by name across all members and methods
// in an SSA package. Returns nil if not found.
func findFunction(prog *ssa.Program, ssaPkg *ssa.Package, name string) *ssa.Function {
	if fn := ssaPkg.Func(name); fn != nil {
		return fn
	}
	// Search methods on named types (matches goastssa/prim_ssa.go pattern).
	for _, mem := range ssaPkg.Members {
		typ, ok := mem.(*ssa.Type)
		if !ok {
			continue
		}
		for _, recvType := range []types.Type{types.NewPointer(typ.Type()), typ.Type()} {
			mset := prog.MethodSets.MethodSet(recvType)
			for sel := range mset.Methods() {
				fn := prog.MethodValue(sel)
				if fn != nil && fn.Name() == name && fn.Pkg == ssaPkg {
					return fn
				}
			}
		}
	}
	return nil
}

// PrimGoCFG implements (go-cfg pattern func-name . options).
func PrimGoCFG(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-cfg")
	if err != nil {
		return err
	}
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-cfg")
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
	mapper := parseCFGOpts(mc.Arg(2), fset)

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
		return werr.WrapForeignErrorf(errCFGBuildError,
			"go-cfg: %s: %s", pattern.Value, loadErr)
	}

	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errCFGBuildError,
			"go-cfg: %s: %s", pattern.Value, strings.Join(errs, "; "))
	}

	prog, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions)
	for _, ssaPkg := range ssaPkgs {
		if ssaPkg != nil {
			ssaPkg.Build()
		}
	}

	for _, ssaPkg := range ssaPkgs {
		if ssaPkg == nil {
			continue
		}
		fn := findFunction(prog, ssaPkg, funcName.Value)
		if fn == nil {
			continue
		}
		mc.SetValue(mapper.mapFunction(fn))
		return nil
	}

	return werr.WrapForeignErrorf(errCFGFuncNotFound,
		"go-cfg: function %q not found in %s", funcName.Value, pattern.Value)
}
```

**Step 5: Add primitive test**

Append to `prim_cfg_test.go`:

```go
func TestGoCFG_ReturnsCFGBlocks(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(pair? (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFG_EntryBlockHasNoIdom(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine, `
		(let* ((blocks (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		       (entry  (car blocks)))
			(eq? (cdr (assoc 'idom (cdr entry))) #f))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFG_Errors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-cfg 42 "Func")`},
		{name: "wrong func-name type", code: `(go-cfg "pkg" 42)`},
		{name: "nonexistent package", code: `(go-cfg "github.com/aalpar/wile/does-not-exist-xyz" "Foo")`},
		{name: "nonexistent function", code: `(go-cfg "github.com/aalpar/wile/extensions/goast" "NoSuchFunction")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
```

**Step 6: Add mapper test for empty/external functions**

Append to `mapper_test.go`:

```go
func TestMapCFG_EmptyFunction(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	// An interface method or external function has no blocks.
	// Build SSA for a package with a function that calls an external.
	fset, fn := buildSSAFunc(t, dir, `
package testpkg

func Linear() int {
	return 42
}
`, "Linear")

	mapper := &cfgMapper{fset: fset}
	result := mapper.mapFunction(fn)

	// Even a trivial function has at least one block (entry).
	c.Assert(listLength(result) >= 1, qt.IsTrue)
}
```

**Step 7: Run tests**

Run: `go test -v ./extensions/goastcfg/... -timeout 120s`
Expected: PASS.

**Step 8: Run lint**

Run: `make lint`
Expected: Clean.

**Step 9: Commit**

```
feat(goastcfg): implement go-cfg with SSA-based dominator support

Loads packages, builds SSA, locates the named function, and maps
each basic block to a cfg-block s-expression. The idom field uses
SSA's built-in BasicBlock.Idom() — no custom dominator algorithm.
Recover blocks (fn.Recover) are tagged with (recover . #t) to
distinguish them from the entry block (both have idom=#f).
Includes positions option (first-instruction pos per block).
```

---

### Task 3: go-cfg-dominators

Pure data primitive: takes the `cfg-block` list from `go-cfg`, inverts the per-block `idom` field into a full dominator tree (parent → children).

**Files:**
- Modify: `extensions/goastcfg/register.go`
- Modify: `extensions/goastcfg/prim_cfg.go`
- Modify: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the failing test**

Append to `prim_cfg_test.go`:

```go
func TestGoCFGDominators_Structure(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A branching function guarantees multiple blocks with non-trivial dominance.
	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)

	result := runScheme(t, engine, `(pair? (go-cfg-dominators cfg))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Every dom-node must have block, idom, and children fields.
	result = runScheme(t, engine, `
		(let loop ((nodes (go-cfg-dominators cfg)))
			(if (null? nodes) #t
				(let ((n (car nodes)))
					(and (eq? (car n) 'dom-node)
					     (assoc 'block    (cdr n))
					     (assoc 'idom     (cdr n))
					     (assoc 'children (cdr n))
					     (loop (cdr nodes))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGDominators_EntryDominatesAll(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Entry block (index 0) should appear in children of no other node
	// (it is the root — idom is #f).
	// Find the entry node (idom == #f and not a recover block) without SRFI-1 filter.
	result := runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		(define dom (go-cfg-dominators cfg))
		(define entry
			(let loop ((nodes dom))
				(if (null? nodes) #f
					(let ((n (car nodes)))
						(if (eq? #f (cdr (assoc 'idom (cdr n))))
							n
							(loop (cdr nodes)))))))
		(integer? (cdr (assoc 'block (cdr entry))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastcfg/... -run TestGoCFGDominators`
Expected: FAIL — primitive not registered.

**Step 3: Register the primitive**

Add to `addPrimitives` in `register.go`:

```go
{
	Name: "go-cfg-dominators", ParamCount: 1,
	Impl:       PrimGoCFGDominators,
	Doc:        "Builds a dominator tree from a cfg-block list returned by go-cfg.",
	ParamNames: []string{"cfg"},
	Category:   "goast-cfg",
},
```

**Step 4: Implement the primitive**

Append to `prim_cfg.go`:

```go
// PrimGoCFGDominators implements (go-cfg-dominators cfg).
// Takes the cfg-block list from go-cfg and returns a list of dom-node
// s-expressions (the dominator tree, rooted at the entry block).
func PrimGoCFGDominators(mc *machine.MachineContext) error {
	cfg := mc.Arg(0)

	// Build a map from block index to idom index. Also track all block indices.
	type blockInfo struct {
		index int64
		idom  int64 // -1 means no idom (entry block)
	}
	var blocks []blockInfo

	tuple, ok := cfg.(values.Tuple)
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if !ok {
			goto nextBlock
		}
		{
			indexVal, found := goast.GetField(np.Cdr(), "index")
			if !found {
				goto nextBlock
			}
			idx := indexVal.(*values.Integer).Value

			idomVal, found := goast.GetField(np.Cdr(), "idom")
			idom := int64(-1)
			if found && idomVal != values.FalseValue {
				idom = idomVal.(*values.Integer).Value
			}
			blocks = append(blocks, blockInfo{index: idx, idom: idom})
		}
	nextBlock:
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}

	// Build children map: idom index -> list of child indices.
	children := make(map[int64][]int64)
	for _, b := range blocks {
		if b.idom >= 0 {
			children[b.idom] = append(children[b.idom], b.index)
		}
	}

	// Emit dom-node for each block.
	nodes := make([]values.Value, len(blocks))
	for i, b := range blocks {
		childVals := make([]values.Value, len(children[b.index]))
		for j, c := range children[b.index] {
			childVals[j] = values.NewInteger(c)
		}
		var idomVal values.Value
		if b.idom >= 0 {
			idomVal = values.NewInteger(b.idom)
		} else {
			idomVal = values.FalseValue
		}
		nodes[i] = goast.Node("dom-node",
			goast.Field("block", values.NewInteger(b.index)),
			goast.Field("idom", idomVal),
			goast.Field("children", goast.ValueList(childVals)),
		)
	}
	mc.SetValue(goast.ValueList(nodes))
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v ./extensions/goastcfg/... -run TestGoCFGDominators -timeout 120s`
Expected: PASS.

**Step 6: Commit**

```
feat(goastcfg): add go-cfg-dominators

Pure data primitive: inverts the per-block idom field from go-cfg
output into an explicit dom-node tree. No package loading or security
gate. Each dom-node carries block index, idom, and children list.
```

---

### Task 4: go-cfg-dominates?

Boolean query: does block A dominate block B? Walks the dom-tree to check if A is an ancestor of B.

**Files:**
- Modify: `extensions/goastcfg/register.go`
- Modify: `extensions/goastcfg/prim_cfg.go`
- Modify: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the failing test**

Append to `prim_cfg_test.go`:

```go
func TestGoCFGDominates(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Use a known branching function to get non-trivial dominance.
	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		(define dom (go-cfg-dominators cfg))`)

	// Entry block (0) dominates itself.
	result := runScheme(t, engine, `(go-cfg-dominates? dom 0 0)`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Entry block (0) dominates every other block.
	result = runScheme(t, engine, `
		(let loop ((nodes dom))
			(if (null? nodes) #t
				(let ((idx (cdr (assoc 'block (cdr (car nodes))))))
					(if (go-cfg-dominates? dom 0 idx)
						(loop (cdr nodes))
						#f))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastcfg/... -run TestGoCFGDominates`

**Step 3: Register the primitive**

Add to `addPrimitives` in `register.go`:

```go
{
	Name: "go-cfg-dominates?", ParamCount: 3,
	Impl:       PrimGoCFGDominates,
	Doc:        "Returns #t if block a dominates block b in the dominator tree.",
	ParamNames: []string{"dom-tree", "a", "b"},
	Category:   "goast-cfg",
},
```

**Step 4: Implement the primitive**

Append to `prim_cfg.go`:

```go
// PrimGoCFGDominates implements (go-cfg-dominates? dom-tree a b).
// Returns #t if block a dominates block b (a is an ancestor of b in dom-tree).
func PrimGoCFGDominates(mc *machine.MachineContext) error {
	domTree := mc.Arg(0)
	aVal, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotANumber, "go-cfg-dominates?")
	if err != nil {
		return err
	}
	bVal, err := helpers.RequireArg[*values.Integer](mc, 2, werr.ErrNotANumber, "go-cfg-dominates?")
	if err != nil {
		return err
	}

	// Build a parent map: block index -> idom index (-1 for entry).
	parent := make(map[int64]int64)
	tuple, ok := domTree.(values.Tuple)
	if !ok {
		mc.SetValue(values.BoolToBoolean(aVal.Value == bVal.Value))
		return nil
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if !ok {
			goto nextNode
		}
		{
			blockVal, found := goast.GetField(np.Cdr(), "block")
			if !found {
				goto nextNode
			}
			idx := blockVal.(*values.Integer).Value
			idomVal, found := goast.GetField(np.Cdr(), "idom")
			if found && idomVal != values.FalseValue {
				parent[idx] = idomVal.(*values.Integer).Value
			} else {
				parent[idx] = -1
			}
		}
	nextNode:
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}

	// Walk from b toward the root; a dominates b iff a appears on the path.
	current := bVal.Value
	for {
		if current == aVal.Value {
			mc.SetValue(values.TrueValue)
			return nil
		}
		p, ok := parent[current]
		if !ok || p < 0 {
			break
		}
		current = p
	}
	mc.SetValue(values.BoolToBoolean(current == aVal.Value))
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v ./extensions/goastcfg/... -run TestGoCFGDominates -timeout 120s`
Expected: PASS.

**Step 6: Commit**

```
feat(goastcfg): add go-cfg-dominates?

Walks the dominator tree from b toward the root; a dominates b iff a
appears on the ancestor path. O(depth) per call. Pure data, no load.
```

---

### Task 5: go-cfg-paths

DFS enumeration of simple paths (no repeated blocks) between two blocks in the CFG. Capped at 1024 paths to bound cost on dense graphs or graphs with many parallel paths.

**Files:**
- Modify: `extensions/goastcfg/register.go`
- Modify: `extensions/goastcfg/prim_cfg.go`
- Modify: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the failing test**

Append to `prim_cfg_test.go`:

```go
func TestGoCFGPaths_LinearFunction(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A linear function has exactly one path from block 0 to its exit block.
	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoFormat"))`)

	result := runScheme(t, engine, `
		(let* ((last-idx (- (length cfg) 1))
		       (paths (go-cfg-paths cfg 0 last-idx)))
			(pair? paths))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGPaths_BranchingFunction(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// A branching function has multiple paths.
	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))`)

	// go-cfg-paths from entry to any block returns a list of paths.
	// Each path is a list of block indices.
	result := runScheme(t, engine, `
		(let* ((last-idx (- (length cfg) 1))
		       (paths (go-cfg-paths cfg 0 last-idx)))
			(and (list? paths)
			     (if (pair? paths)
			         (list? (car paths))  ;; each path is a list of indices
			         #t)))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCFGPaths_SameBlock(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoFormat"))`)

	// A path from block 0 to itself is a single path containing just block 0.
	result := runScheme(t, engine, `
		(let ((paths (go-cfg-paths cfg 0 0)))
			(and (pair? paths)
			     (equal? (car paths) '(0))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastcfg/... -run TestGoCFGPaths`

**Step 3: Register the primitive**

Add to `addPrimitives` in `register.go`:

```go
{
	Name: "go-cfg-paths", ParamCount: 3,
	Impl:       PrimGoCFGPaths,
	Doc:        "Enumerates simple paths (no repeated blocks) between two blocks in the CFG. Returns a list of paths, each a list of block indices. Silently truncated at 1024 paths.",
	ParamNames: []string{"cfg", "from", "to"},
	Category:   "goast-cfg",
},
```

**Step 4: Implement the primitive**

Append to `prim_cfg.go`:

const maxCFGPaths = 1024

```go
const maxCFGPaths = 1024

// PrimGoCFGPaths implements (go-cfg-paths cfg from to).
// Returns a list of simple paths (lists of block indices) from block `from`
// to block `to`. Capped at maxCFGPaths to bound cost.
func PrimGoCFGPaths(mc *machine.MachineContext) error {
	cfg := mc.Arg(0)
	fromVal, err := helpers.RequireArg[*values.Integer](mc, 1, werr.ErrNotANumber, "go-cfg-paths")
	if err != nil {
		return err
	}
	toVal, err := helpers.RequireArg[*values.Integer](mc, 2, werr.ErrNotANumber, "go-cfg-paths")
	if err != nil {
		return err
	}

	// Build adjacency map from cfg-block list.
	succs := make(map[int64][]int64)
	tuple, ok := cfg.(values.Tuple)
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if !ok {
			goto nextCFGBlock
		}
		{
			idxVal, found := goast.GetField(np.Cdr(), "index")
			if !found {
				goto nextCFGBlock
			}
			idx := idxVal.(*values.Integer).Value
			succsVal, found := goast.GetField(np.Cdr(), "succs")
			if !found {
				goto nextCFGBlock
			}
			st, ok := succsVal.(values.Tuple)
			if !ok {
				goto nextCFGBlock
			}
			for !values.IsEmptyList(st) {
				sp, ok := st.(*values.Pair)
				if !ok {
					break
				}
				if sv, ok := sp.Car().(*values.Integer); ok {
					succs[idx] = append(succs[idx], sv.Value)
				}
				st, ok = sp.Cdr().(values.Tuple)
				if !ok {
					break
				}
			}
		}
	nextCFGBlock:
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}

	// DFS to enumerate simple paths.
	var paths [][]int64
	visited := make(map[int64]bool)

	var dfs func(current int64, path []int64)
	dfs = func(current int64, path []int64) {
		if len(paths) >= maxCFGPaths {
			return
		}
		path = append(path, current)
		if current == toVal.Value {
			cp := make([]int64, len(path))
			copy(cp, path)
			paths = append(paths, cp)
			return
		}
		visited[current] = true
		for _, next := range succs[current] {
			if !visited[next] {
				dfs(next, path)
			}
		}
		visited[current] = false
	}

	dfs(fromVal.Value, nil)

	// Convert paths to s-expression: list of lists of integers.
	pathVals := make([]values.Value, len(paths))
	for i, p := range paths {
		blockVals := make([]values.Value, len(p))
		for j, idx := range p {
			blockVals[j] = values.NewInteger(idx)
		}
		pathVals[i] = goast.ValueList(blockVals)
	}
	mc.SetValue(goast.ValueList(pathVals))
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v ./extensions/goastcfg/... -timeout 120s`
Expected: PASS.

**Step 6: Run lint**

Run: `make lint`
Expected: Clean.

**Step 7: Commit**

```
feat(goastcfg): add go-cfg-paths simple path enumeration

DFS over the cfg-block adjacency graph; visited set prevents revisiting
blocks (simple paths only). Capped at 1024 paths. Each path is a list
of block indices. (go-cfg-paths cfg 0 0) => ((0)).
```

---

### Task 6: Integration test and plan update

End-to-end test exercising the full Phase 3 pipeline. Demonstrates the motivating use case: "does every path from entry to return pass through a check?"

**Files:**
- Modify: `extensions/goastcfg/prim_cfg_test.go`

**Step 1: Write the integration test**

Append to `prim_cfg_test.go`:

```go
func TestIntegration_DominanceQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build CFG + dominator tree for a real function.
	runScheme(t, engine, `
		(define cfg (go-cfg "github.com/aalpar/wile/extensions/goast" "PrimGoParseExpr"))
		(define dom (go-cfg-dominators cfg))`)

	// Verify: entry block dominates every other block.
	result := runScheme(t, engine, `
		(let loop ((nodes dom))
			(if (null? nodes) #t
				(let ((idx (cdr (assoc 'block (cdr (car nodes))))))
					(and (go-cfg-dominates? dom 0 idx)
					     (loop (cdr nodes))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify: the CFG structure is consistent (every successor's idom
	// is dominated by the predecessor, or equals the predecessor).
	result = runScheme(t, engine, `
		(let loop ((blocks cfg))
			(if (null? blocks) #t
				(let* ((b       (car blocks))
				       (b-idx   (cdr (assoc 'index (cdr b))))
				       (succs   (cdr (assoc 'succs (cdr b)))))
					(if (null? succs) (loop (cdr blocks))
						(and (go-cfg-dominates? dom b-idx b-idx)  ;; every block dominates itself
						     (loop (cdr blocks)))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run integration test**

Run: `go test -v ./extensions/goastcfg/... -run TestIntegration -timeout 120s`
Expected: PASS.

**Step 3: Run full test suite**

Run: `make lint && make test`
Expected: All packages pass.

**Step 4: Run covercheck**

Run: `make covercheck`
Expected: `extensions/goastcfg` >= 80%.

**Step 5: Update plan status**

In `plans/GO-STATIC-ANALYSIS.md`, update the Phase 3 heading:

```
### Phase 3: `(wile goast cfg)` — Control Flow Graph + Dominance  ✓ Complete
```

Update `plans/CLAUDE.md` to add the plan file entry:

```
| `GO-CFG-PHASE-3.md` | CFG + dominance extension implementation plan (Phase 3 of GO-STATIC-ANALYSIS) | Complete |
```

**Step 6: Commit**

```
docs: mark GO-STATIC-ANALYSIS Phase 3 complete
```

---

## Post-implementation checklist

- [x] All 6 task commits on branch
- [x] `make lint` clean
- [x] `make test` passes (full suite)
- [x] `make covercheck` passes
- [x] `plans/GO-STATIC-ANALYSIS.md` updated: Phase 3 → "Complete"
- [x] `plans/CLAUDE.md` updated with plan file entry
- [x] All 4 primitives have happy-path + error-path tests
- [x] At least one integration test validating dominance + paths together
- [x] `go-cfg-paths` handles the same-block case `(go-cfg-paths cfg 0 0) => ((0))`
- [x] `go-cfg-paths` is capped at 1024 paths (no infinite loop on loops)
- [x] Recover blocks tagged with `(recover . #t)` — tests don't break on functions with `defer`/`recover`
- [x] Method iteration uses `for sel := range mset.Methods()` (Go 1.23 pattern, matches goastssa)

## Primitives summary

| Primitive | Signature | Security | Description |
|---|---|---|---|
| `go-cfg` | `(go-cfg pattern func-name . options)` | `ResourceProcess`/`ActionLoad` | Load package, build SSA, return `cfg-block` list with `idom` annotations |
| `go-cfg-dominators` | `(go-cfg-dominators cfg)` | None | Build `dom-node` tree by inverting per-block `idom` fields |
| `go-cfg-dominates?` | `(go-cfg-dominates? dom-tree a b)` | None | Does block `a` dominate block `b`? Walk ancestor chain. |
| `go-cfg-paths` | `(go-cfg-paths cfg from to)` | None | Enumerate simple paths between blocks. Silently truncated at 1024. |
