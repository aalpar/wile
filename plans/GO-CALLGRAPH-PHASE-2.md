# Go Callgraph Extension — Phase 2 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build the `(wile goast callgraph)` extension that exposes Go call graph analysis as s-expressions, enabling inter-procedural queries: "who calls whom?", "what's reachable from this function?"

**Architecture:** New Go package `extensions/goastcg/` imports alist helpers from `extensions/goast/`. Loads packages via `go/packages`, builds SSA for the **entire program** (including transitive dependencies), runs one of four call graph algorithms (`static`, `cha`, `rta`, `vta`), and maps the resulting `callgraph.Graph` to tagged-alist s-expressions using the same `(tag (field . val) ...)` encoding as goast and goastssa. Query primitives navigate the s-expression graph without opaque handles.

**Tech Stack:** `golang.org/x/tools/go/callgraph` + `callgraph/{static,cha,rta,vta}`, `golang.org/x/tools/go/ssa`, `golang.org/x/tools/go/packages` (all already available via `golang.org/x/tools v0.42.0`)

**Design doc:** `plans/GO-STATIC-ANALYSIS.md` (Phase 2)

**Reference:** `extensions/goastssa/` — established patterns for SSA loading, mapper structure, test helpers, extension registration.

---

## Design Decisions

| Decision | Choice | Rationale |
|----------|--------|-----------|
| Graph representation | Eager serialization to s-expression list | Pure data, composable with Scheme, no hidden state; consistent with SSA extension returning list of `ssa-func` |
| Query primitives | Go-side primitives that search the s-expression | Efficient lookup; users don't need to write Scheme traversal for common queries |
| Algorithm argument | Required symbol: `'static`, `'cha`, `'rta`, `'vta` | Explicit choice; no default avoids surprising cost differences |
| SSA building | Full program build (`prog.AllPackages().Build()`) | Call graph algorithms need cross-package edges; differs from SSA extension which builds only requested packages |
| Node naming | `ssa.Function.String()` (fully qualified) | Unique across packages; e.g., `"fmt.Println"`, `"(*bytes.Buffer).Write"` |
| Edge encoding | Both `caller` and `callee` on every edge | Self-describing; works whether extracted from `edges-in` or `edges-out` |
| RTA roots | Main functions; error if none found | RTA is whole-program-from-main; use CHA/VTA for libraries |
| VTA bootstrap | CHA as initial graph | Standard VTA usage pattern; CHA provides the over-approximation that VTA refines |
| Import from goastssa | None — independent SSA loading | Loading strategies differ (per-package vs whole-program); avoids coupling |

### S-expression Encoding

```scheme
;; go-callgraph returns a list of cg-node entries:
((cg-node
   (name . "main.main")
   (id . 0)
   (pkg . "main")
   (edges-out . ((cg-edge (caller . "main.main") (callee . "fmt.Println")
                   (pos . "main.go:10:2") (description . "static call"))
                 (cg-edge ...)))
   (edges-in . ()))
 (cg-node
   (name . "fmt.Println")
   (id . 1)
   (pkg . "fmt")
   (edges-out . ())
   (edges-in . ((cg-edge (caller . "main.main") (callee . "fmt.Println")
                  (pos . "main.go:10:2") (description . "static call"))))))
```

### Package Structure

```
extensions/goastcg/
  doc.go                # Package documentation
  register.go           # Extension registration, LibraryNamer -> (wile goast callgraph)
  prim_callgraph.go     # Primitive implementations
  mapper.go             # callgraph.Graph -> s-expression mapper
  mapper_test.go        # Mapper unit tests (internal package)
  prim_callgraph_test.go # Primitive + integration tests (external package)
```

---

### Task 1: Create goastcg extension skeleton

Create the package structure, extension registration with `LibraryNamer`, and a stub `go-callgraph` primitive that returns an empty list.

**Files:**
- Create: `extensions/goastcg/doc.go`
- Create: `extensions/goastcg/register.go`
- Create: `extensions/goastcg/prim_callgraph.go`
- Create: `extensions/goastcg/prim_callgraph_test.go`

**Step 1: Write the failing test**

`extensions/goastcg/prim_callgraph_test.go` (external test package):

```go
package goastcg_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgoastcg "github.com/aalpar/wile/extensions/goastcg"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extgoastcg.Extension),
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
	namer, ok := extgoastcg.Extension.(libraryNamer)
	qt.New(t).Assert(ok, qt.IsTrue)
	qt.New(t).Assert(namer.LibraryName(), qt.DeepEquals, []string{"wile", "goast", "callgraph"})
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastcg/... 2>&1 | head -20`
Expected: FAIL — package doesn't exist yet.

**Step 3: Write skeleton files**

`extensions/goastcg/doc.go`:
```go
// Package goastcg exposes Go call graph analysis as Scheme
// s-expressions. Supports static, CHA, RTA, and VTA algorithms.
package goastcg
```

`extensions/goastcg/register.go`:
```go
package goastcg

import "github.com/aalpar/wile/registry"

// cgExtension wraps Extension to implement LibraryNamer.
type cgExtension struct {
	registry.Extension
}

// LibraryName returns (wile goast callgraph) for R7RS import.
func (p *cgExtension) LibraryName() []string {
	return []string{"wile", "goast", "callgraph"}
}

// Extension is the callgraph extension entry point.
var Extension registry.Extension = &cgExtension{
	Extension: registry.NewExtension("goast-callgraph", AddToRegistry),
}

// Builder aggregates all callgraph registration functions.
var Builder = registry.NewRegistryBuilder(addPrimitives)

// AddToRegistry registers all callgraph primitives.
var AddToRegistry = Builder.AddToRegistry

func addPrimitives(r *registry.Registry) error {
	r.AddPrimitives([]registry.PrimitiveSpec{
		{
			Name: "go-callgraph", ParamCount: 2, IsVariadic: false,
			Impl:       PrimGoCallgraph,
			Doc:        "Builds a call graph for a Go package using the specified algorithm.",
			ParamNames: []string{"pattern", "algorithm"},
			Category:   "goast-callgraph",
		},
	}, registry.PhaseRuntime)
	return nil
}
```

`extensions/goastcg/prim_callgraph.go`:
```go
package goastcg

import (
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/helpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

var (
	errCGBuildError       = werr.NewStaticError("callgraph build error")
	errCGInvalidAlgorithm = werr.NewStaticError("invalid callgraph algorithm")
)

// PrimGoCallgraph implements (go-callgraph pattern algorithm).
// Stub: returns empty list. Filled in by Task 2.
func PrimGoCallgraph(mc *machine.MachineContext) error {
	_, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-callgraph")
	if err != nil {
		return err
	}
	_, err = helpers.RequireArg[*values.Symbol](mc, 1, werr.ErrNotASymbol, "go-callgraph")
	if err != nil {
		return err
	}

	mc.SetValue(values.EmptyList)
	return nil
}
```

**Step 4: Run test to verify it passes**

Run: `go test -v ./extensions/goastcg/... -run TestExtensionLibraryName`
Expected: PASS.

**Step 5: Run lint**

Run: `make lint`
Expected: Clean.

**Step 6: Commit**

```
feat(goastcg): add (wile goast callgraph) extension skeleton

Creates the package structure with LibraryNamer support and a
stub go-callgraph primitive that validates argument types.
```

---

### Task 2: Implement go-callgraph with all four algorithms

The core primitive: load packages, build SSA for the whole program, dispatch to the selected algorithm, map the resulting `callgraph.Graph` to s-expressions.

**Files:**
- Modify: `extensions/goastcg/prim_callgraph.go`
- Create: `extensions/goastcg/mapper.go`
- Create: `extensions/goastcg/mapper_test.go`
- Modify: `extensions/goastcg/prim_callgraph_test.go`

**Step 1: Write the failing mapper test**

`extensions/goastcg/mapper_test.go` (internal test package):

```go
package goastcg

import (
	"go/token"
	"os"
	"path/filepath"
	"sort"
	"testing"

	"golang.org/x/tools/go/callgraph/static"
	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

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

// buildCallgraph loads Go source, builds SSA for the whole program,
// and returns a static call graph plus the fset.
func buildCallgraph(t *testing.T, dir, source string) (*token.FileSet, values.Value) {
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
	c.Assert(len(pkgs), qt.Not(qt.Equals), 0)

	prog, _ := ssautil.Packages(pkgs, ssa.SanityCheckFunctions|ssa.InstantiateGenerics)
	for _, pkg := range prog.AllPackages() {
		pkg.Build()
	}

	cg := static.CallGraph(prog)

	mapper := &cgMapper{fset: fset}
	result := mapper.mapGraph(cg)
	return fset, result
}

// findCGNodeByName searches a list of cg-nodes for one with the given name suffix.
func findCGNodeByName(graph values.Value, nameSuffix string) values.Value {
	tuple, ok := graph.(values.Tuple)
	if !ok {
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
			goto next
		}
		{
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				if s, ok := nameVal.(*values.String); ok {
					if len(s.Value) >= len(nameSuffix) &&
						s.Value[len(s.Value)-len(nameSuffix):] == nameSuffix {
						return node
					}
				}
			}
		}
	next:
		tuple, ok = pair.Cdr().(values.Tuple)
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

// collectNodeNames extracts all "name" fields from the cg-node list.
func collectNodeNames(graph values.Value) []string {
	var names []string
	tuple, ok := graph.(values.Tuple)
	if !ok {
		return names
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if ok {
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				if s, ok := nameVal.(*values.String); ok {
					names = append(names, s.Value)
				}
			}
		}
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	sort.Strings(names)
	return names
}

func TestMapCallgraph_Static(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildCallgraph(t, dir, `
package testpkg

func main() {
	helper()
}

func helper() {
	leaf()
}

func leaf() {}
`)

	// Graph should contain cg-nodes.
	c.Assert(listLength(graph) > 0, qt.IsTrue,
		qt.Commentf("expected non-empty callgraph"))

	// Find main function node.
	mainNode := findCGNodeByName(graph, ".main")
	c.Assert(mainNode, qt.IsNotNil, qt.Commentf("expected main node"))

	// main should have edges-out to helper.
	edgesOut, ok := goast.GetField(mainNode.(*values.Pair).Cdr(), "edges-out")
	c.Assert(ok, qt.IsTrue)
	c.Assert(listLength(edgesOut) > 0, qt.IsTrue,
		qt.Commentf("expected main to have outgoing edges"))

	// helper should have edges-out to leaf.
	helperNode := findCGNodeByName(graph, ".helper")
	c.Assert(helperNode, qt.IsNotNil, qt.Commentf("expected helper node"))
	helperOut, ok := goast.GetField(helperNode.(*values.Pair).Cdr(), "edges-out")
	c.Assert(ok, qt.IsTrue)
	c.Assert(listLength(helperOut) > 0, qt.IsTrue,
		qt.Commentf("expected helper to call leaf"))

	// leaf should have edges-in from helper.
	leafNode := findCGNodeByName(graph, ".leaf")
	c.Assert(leafNode, qt.IsNotNil, qt.Commentf("expected leaf node"))
	leafIn, ok := goast.GetField(leafNode.(*values.Pair).Cdr(), "edges-in")
	c.Assert(ok, qt.IsTrue)
	c.Assert(listLength(leafIn) > 0, qt.IsTrue,
		qt.Commentf("expected leaf to be called by helper"))
}

func TestMapCallgraph_EdgeFields(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildCallgraph(t, dir, `
package testpkg

func caller() {
	callee()
}

func callee() {}
`)

	callerNode := findCGNodeByName(graph, ".caller")
	c.Assert(callerNode, qt.IsNotNil)

	edgesOut, ok := goast.GetField(callerNode.(*values.Pair).Cdr(), "edges-out")
	c.Assert(ok, qt.IsTrue)

	// Get first edge.
	firstEdge := edgesOut.(*values.Pair).Car()
	c.Assert(firstEdge, qt.IsNotNil)

	// Edge should have caller, callee, and description fields.
	ep := firstEdge.(*values.Pair)
	callerField, ok := goast.GetField(ep.Cdr(), "caller")
	c.Assert(ok, qt.IsTrue)
	c.Assert(callerField.(*values.String).Value, qt.Contains, "caller")

	calleeField, ok := goast.GetField(ep.Cdr(), "callee")
	c.Assert(ok, qt.IsTrue)
	c.Assert(calleeField.(*values.String).Value, qt.Contains, "callee")

	descField, ok := goast.GetField(ep.Cdr(), "description")
	c.Assert(ok, qt.IsTrue)
	c.Assert(descField.(*values.String).Value, qt.Not(qt.Equals), "")
}
```

**Step 2: Run test to verify it fails**

Run: `go test -v ./extensions/goastcg/... -run TestMapCallgraph`
Expected: FAIL — mapper.go doesn't exist.

**Step 3: Implement the mapper**

`extensions/goastcg/mapper.go`:

```go
package goastcg

import (
	"go/token"
	"sort"

	"golang.org/x/tools/go/callgraph"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"
)

type cgMapper struct {
	fset *token.FileSet
}

// mapGraph converts a callgraph.Graph to a list of cg-node s-expressions.
func (p *cgMapper) mapGraph(cg *callgraph.Graph) values.Value {
	// Collect non-nil nodes sorted by ID for deterministic output.
	sorted := make([]*callgraph.Node, 0, len(cg.Nodes))
	for _, node := range cg.Nodes {
		if node.Func == nil {
			continue
		}
		sorted = append(sorted, node)
	}
	sort.Slice(sorted, func(i, j int) bool {
		return sorted[i].ID < sorted[j].ID
	})

	nodes := make([]values.Value, len(sorted))
	for i, node := range sorted {
		nodes[i] = p.mapNode(node)
	}
	return goast.ValueList(nodes)
}

// mapNode converts a callgraph.Node to a cg-node s-expression.
func (p *cgMapper) mapNode(n *callgraph.Node) values.Value {
	edgesIn := make([]values.Value, len(n.In))
	for i, e := range n.In {
		edgesIn[i] = p.mapEdge(e)
	}

	edgesOut := make([]values.Value, len(n.Out))
	for i, e := range n.Out {
		edgesOut[i] = p.mapEdge(e)
	}

	fields := []values.Value{
		goast.Field("name", goast.Str(n.Func.String())),
		goast.Field("id", values.NewInteger(int64(n.ID))),
		goast.Field("edges-in", goast.ValueList(edgesIn)),
		goast.Field("edges-out", goast.ValueList(edgesOut)),
	}
	if n.Func.Pkg != nil {
		fields = append(fields, goast.Field("pkg", goast.Str(n.Func.Pkg.Pkg.Path())))
	}
	return goast.Node("cg-node", fields...)
}

// mapEdge converts a callgraph.Edge to a cg-edge s-expression.
func (p *cgMapper) mapEdge(e *callgraph.Edge) values.Value {
	fields := make([]values.Value, 0, 4)

	if e.Caller != nil && e.Caller.Func != nil {
		fields = append(fields, goast.Field("caller", goast.Str(e.Caller.Func.String())))
	}
	if e.Callee != nil && e.Callee.Func != nil {
		fields = append(fields, goast.Field("callee", goast.Str(e.Callee.Func.String())))
	}

	pos := e.Pos()
	if pos.IsValid() && p.fset != nil {
		fields = append(fields, goast.Field("pos", goast.Str(p.fset.Position(pos).String())))
	}

	fields = append(fields, goast.Field("description", goast.Str(e.Description())))

	return goast.Node("cg-edge", fields...)
}
```

**Step 4: Implement the full go-callgraph primitive**

Replace the stub in `prim_callgraph.go`:

```go
package goastcg

import (
	"go/token"
	"strings"

	"golang.org/x/tools/go/callgraph"
	"golang.org/x/tools/go/callgraph/cha"
	"golang.org/x/tools/go/callgraph/rta"
	"golang.org/x/tools/go/callgraph/static"
	"golang.org/x/tools/go/callgraph/vta"
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

var (
	errCGBuildError       = werr.NewStaticError("callgraph build error")
	errCGInvalidAlgorithm = werr.NewStaticError("invalid callgraph algorithm")
	errCGNoMainFunction   = werr.NewStaticError("no main function for rta")
)

// validAlgorithms lists the accepted algorithm symbols.
var validAlgorithms = map[string]bool{
	"static": true,
	"cha":    true,
	"rta":    true,
	"vta":    true,
}

// PrimGoCallgraph implements (go-callgraph pattern algorithm).
func PrimGoCallgraph(mc *machine.MachineContext) error {
	pattern, err := helpers.RequireArg[*values.String](mc, 0, werr.ErrNotAString, "go-callgraph")
	if err != nil {
		return err
	}

	algo, err := helpers.RequireArg[*values.Symbol](mc, 1, werr.ErrNotASymbol, "go-callgraph")
	if err != nil {
		return err
	}

	if !validAlgorithms[algo.Key] {
		return werr.WrapForeignErrorf(errCGInvalidAlgorithm,
			"go-callgraph: algorithm must be static, cha, rta, or vta; got %s", algo.Key)
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
		return werr.WrapForeignErrorf(errCGBuildError,
			"go-callgraph: %s: %s", pattern.Value, loadErr)
	}

	// Check for package load errors.
	var errs []string
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e.Error())
		}
	}
	if len(errs) > 0 {
		return werr.WrapForeignErrorf(errCGBuildError,
			"go-callgraph: %s: %s", pattern.Value,
			strings.Join(errs, "; "))
	}

	// Build SSA for the entire program (callgraph needs cross-package edges).
	prog, _ := ssautil.Packages(pkgs, ssa.SanityCheckFunctions|ssa.InstantiateGenerics)
	for _, pkg := range prog.AllPackages() {
		pkg.Build()
	}

	// Dispatch to selected algorithm.
	cg, cgErr := buildCallgraph(prog, algo.Key)
	if cgErr != nil {
		return cgErr
	}

	mapper := &cgMapper{fset: fset}
	mc.SetValue(mapper.mapGraph(cg))
	return nil
}

// buildCallgraph dispatches to the selected call graph algorithm.
func buildCallgraph(prog *ssa.Program, algorithm string) (*callgraph.Graph, error) {
	switch algorithm {
	case "static":
		return static.CallGraph(prog), nil

	case "cha":
		return cha.CallGraph(prog), nil

	case "rta":
		mains := ssautil.MainPackages(prog.AllPackages())
		var roots []*ssa.Function
		for _, m := range mains {
			if f := m.Func("main"); f != nil {
				roots = append(roots, f)
			}
		}
		if len(roots) == 0 {
			return nil, werr.WrapForeignErrorf(errCGNoMainFunction,
				"go-callgraph: rta requires a main function; use cha or vta for libraries")
		}
		result := rta.Analyze(roots, true)
		return result.CallGraph, nil

	case "vta":
		initial := cha.CallGraph(prog)
		allFuncs := ssautil.AllFunctions(prog)
		return vta.CallGraph(allFuncs, initial), nil

	default:
		return nil, werr.WrapForeignErrorf(errCGInvalidAlgorithm,
			"go-callgraph: unknown algorithm %s", algorithm)
	}
}
```

**Step 5: Add primitive tests**

Append to `prim_callgraph_test.go`:

```go
func TestGoCallgraph_Static(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Load a known package with static analysis.
	result := runScheme(t, engine,
		`(pair? (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_CHA(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	result := runScheme(t, engine,
		`(pair? (go-callgraph "github.com/aalpar/wile/extensions/goast" 'cha))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_Errors(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{name: "wrong pattern type", code: `(go-callgraph 42 'static)`},
		{name: "wrong algorithm type", code: `(go-callgraph "pkg" "static")`},
		{name: "invalid algorithm", code: `(go-callgraph "github.com/aalpar/wile/extensions/goast" 'unknown)`},
		{name: "nonexistent package", code: `(go-callgraph "github.com/aalpar/wile/does-not-exist-xyz" 'static)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runSchemeExpectError(t, engine, tc.code)
		})
	}
}
```

**Step 6: Run all tests**

Run: `go test -v ./extensions/goastcg/... -timeout 120s`
Expected: PASS.

**Step 7: Run lint**

Run: `make lint`
Expected: Clean.

**Step 8: Commit**

```
feat(goastcg): implement go-callgraph with four algorithms

Loads packages, builds SSA for the full program, dispatches to
static/cha/rta/vta callgraph algorithms, and maps the result to
cg-node/cg-edge s-expressions. RTA requires a main function;
VTA uses CHA as its initial over-approximation.
```

---

### Task 3: go-callgraph-callers and go-callgraph-callees

Query primitives that search the graph s-expression for a function by name and return its incoming or outgoing edges.

**Files:**
- Modify: `extensions/goastcg/register.go`
- Modify: `extensions/goastcg/prim_callgraph.go`
- Modify: `extensions/goastcg/prim_callgraph_test.go`

**Step 1: Write the failing tests**

Append to `prim_callgraph_test.go`:

```go
func TestGoCallgraphCallers(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build a static callgraph for a package.
	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// PrimGoParseExpr should have callers (it's called from test code or other functions).
	// At minimum, verify we get a list back (possibly empty for a leaf function).
	result := runScheme(t, engine,
		`(list? (go-callgraph-callers cg "goast.PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphCallees(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// PrimGoParseExpr calls go/parser functions — it should have outgoing edges.
	result := runScheme(t, engine,
		`(list? (go-callgraph-callees cg "goast.PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphCallers_NotFound(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Nonexistent function returns empty list.
	result := runScheme(t, engine,
		`(null? (go-callgraph-callers cg "does.not.Exist"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastcg/... -run 'TestGoCallgraphCallers|TestGoCallgraphCallees'`
Expected: FAIL — primitives not registered.

**Step 3: Register the new primitives**

Add to the `addPrimitives` function in `register.go`:

```go
{
	Name: "go-callgraph-callers", ParamCount: 2,
	Impl:       PrimGoCallgraphCallers,
	Doc:        "Returns the incoming edges (callers) of a function in the call graph.",
	ParamNames: []string{"graph", "func-name"},
	Category:   "goast-callgraph",
},
{
	Name: "go-callgraph-callees", ParamCount: 2,
	Impl:       PrimGoCallgraphCallees,
	Doc:        "Returns the outgoing edges (callees) of a function in the call graph.",
	ParamNames: []string{"graph", "func-name"},
	Category:   "goast-callgraph",
},
```

**Step 4: Implement the primitives and helper**

Append to `prim_callgraph.go`:

```go
// findCGNode walks a list of cg-node s-expressions and returns the
// node whose "name" field matches the given function name.
// Returns nil if not found.
func findCGNode(graph values.Value, name string) values.Value {
	tuple, ok := graph.(values.Tuple)
	if !ok {
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
			goto next
		}
		{
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				if s, ok := nameVal.(*values.String); ok && s.Value == name {
					return node
				}
			}
		}
	next:
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return nil
}

// PrimGoCallgraphCallers implements (go-callgraph-callers graph func-name).
func PrimGoCallgraphCallers(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-callers")
	if err != nil {
		return err
	}

	node := findCGNode(graph, funcName.Value)
	if node == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	edgesIn, ok := goast.GetField(node.(*values.Pair).Cdr(), "edges-in")
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	mc.SetValue(edgesIn)
	return nil
}

// PrimGoCallgraphCallees implements (go-callgraph-callees graph func-name).
func PrimGoCallgraphCallees(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	funcName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-callees")
	if err != nil {
		return err
	}

	node := findCGNode(graph, funcName.Value)
	if node == nil {
		mc.SetValue(values.EmptyList)
		return nil
	}

	edgesOut, ok := goast.GetField(node.(*values.Pair).Cdr(), "edges-out")
	if !ok {
		mc.SetValue(values.EmptyList)
		return nil
	}
	mc.SetValue(edgesOut)
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v ./extensions/goastcg/... -run 'TestGoCallgraphCallers|TestGoCallgraphCallees' -timeout 120s`
Expected: PASS.

**Step 6: Run full suite + lint**

Run: `make lint && go test -v ./extensions/goastcg/... -timeout 120s`
Expected: Clean.

**Step 7: Commit**

```
feat(goastcg): add go-callgraph-callers and go-callgraph-callees

Query primitives that search the graph s-expression for a named
function and return its edges-in or edges-out. Returns empty list
if the function is not found in the graph.
```

---

### Task 4: go-callgraph-reachable

Transitive closure: given a root function, return all functions reachable by following outgoing call edges.

**Files:**
- Modify: `extensions/goastcg/register.go`
- Modify: `extensions/goastcg/prim_callgraph.go`
- Modify: `extensions/goastcg/prim_callgraph_test.go`
- Modify: `extensions/goastcg/mapper_test.go`

**Step 1: Write the failing test**

Append to `mapper_test.go` (internal, for direct mapper access):

```go
func TestMapCallgraph_Reachable(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildCallgraph(t, dir, `
package testpkg

func root() {
	mid()
}

func mid() {
	leaf()
}

func leaf() {}

func unreachable() {}
`)

	// Build node map and verify BFS from root reaches mid and leaf but not unreachable.
	nodeMap := buildNodeMap(graph)

	_, hasRoot := nodeMap["testpkg.root"]
	c.Assert(hasRoot, qt.IsTrue, qt.Commentf("expected root in graph"))

	_, hasMid := nodeMap["testpkg.mid"]
	c.Assert(hasMid, qt.IsTrue, qt.Commentf("expected mid in graph"))

	_, hasLeaf := nodeMap["testpkg.leaf"]
	c.Assert(hasLeaf, qt.IsTrue, qt.Commentf("expected leaf in graph"))

	// Verify reachable from root.
	reachable := computeReachable(nodeMap, "testpkg.root")
	c.Assert(reachable["testpkg.root"], qt.IsTrue)
	c.Assert(reachable["testpkg.mid"], qt.IsTrue)
	c.Assert(reachable["testpkg.leaf"], qt.IsTrue)
	c.Assert(reachable["testpkg.unreachable"], qt.IsFalse)
}
```

Append to `prim_callgraph_test.go`:

```go
func TestGoCallgraphReachable(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Reachable from a known function should return a non-empty list of strings.
	result := runScheme(t, engine,
		`(pair? (go-callgraph-reachable cg "goast.PrimGoParseExpr"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// The root itself should appear in the reachable set.
	result = runScheme(t, engine, `
		(let ((reachable (go-callgraph-reachable cg "goast.PrimGoParseExpr")))
			(let loop ((r reachable))
				(cond
					((null? r) #f)
					((equal? (car r) "goast.PrimGoParseExpr") #t)
					(else (loop (cdr r))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraphReachable_NotFound(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Nonexistent root returns empty list.
	result := runScheme(t, engine,
		`(null? (go-callgraph-reachable cg "does.not.Exist"))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}
```

**Step 2: Run tests to verify they fail**

Run: `go test -v ./extensions/goastcg/... -run 'TestMapCallgraph_Reachable|TestGoCallgraphReachable'`
Expected: FAIL — `buildNodeMap`, `computeReachable`, and the primitive don't exist.

**Step 3: Register the primitive**

Add to `addPrimitives` in `register.go`:

```go
{
	Name: "go-callgraph-reachable", ParamCount: 2,
	Impl:       PrimGoCallgraphReachable,
	Doc:        "Returns a list of function names transitively reachable from the root.",
	ParamNames: []string{"graph", "root-name"},
	Category:   "goast-callgraph",
},
```

**Step 4: Implement buildNodeMap, computeReachable, and the primitive**

Append to `prim_callgraph.go`:

```go
// buildNodeMap parses a list of cg-node s-expressions into a Go map
// keyed by function name for efficient lookup.
func buildNodeMap(graph values.Value) map[string]values.Value {
	nodeMap := make(map[string]values.Value)
	tuple, ok := graph.(values.Tuple)
	if !ok {
		return nodeMap
	}
	for !values.IsEmptyList(tuple) {
		pair, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		node := pair.Car()
		np, ok := node.(*values.Pair)
		if ok {
			nameVal, found := goast.GetField(np.Cdr(), "name")
			if found {
				if s, ok := nameVal.(*values.String); ok {
					nodeMap[s.Value] = node
				}
			}
		}
		tuple, ok = pair.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return nodeMap
}

// calleesOf extracts the callee function names from the edges-out of a cg-node.
func calleesOf(node values.Value) []string {
	np, ok := node.(*values.Pair)
	if !ok {
		return nil
	}
	edgesOut, ok := goast.GetField(np.Cdr(), "edges-out")
	if !ok {
		return nil
	}
	var names []string
	tuple, ok := edgesOut.(values.Tuple)
	if !ok {
		return nil
	}
	for !values.IsEmptyList(tuple) {
		ep, ok := tuple.(*values.Pair)
		if !ok {
			break
		}
		edge := ep.Car()
		edgePair, ok := edge.(*values.Pair)
		if ok {
			callee, found := goast.GetField(edgePair.Cdr(), "callee")
			if found {
				if s, ok := callee.(*values.String); ok {
					names = append(names, s.Value)
				}
			}
		}
		tuple, ok = ep.Cdr().(values.Tuple)
		if !ok {
			break
		}
	}
	return names
}

// computeReachable does BFS from rootName, returning the set of reachable names.
func computeReachable(nodeMap map[string]values.Value, rootName string) map[string]bool {
	visited := make(map[string]bool)
	queue := []string{rootName}

	for len(queue) > 0 {
		current := queue[0]
		queue = queue[1:]
		if visited[current] {
			continue
		}
		visited[current] = true

		node, ok := nodeMap[current]
		if !ok {
			continue
		}

		for _, callee := range calleesOf(node) {
			if !visited[callee] {
				queue = append(queue, callee)
			}
		}
	}
	return visited
}

// PrimGoCallgraphReachable implements (go-callgraph-reachable graph root-name).
func PrimGoCallgraphReachable(mc *machine.MachineContext) error {
	graph := mc.Arg(0)
	rootName, err := helpers.RequireArg[*values.String](mc, 1, werr.ErrNotAString, "go-callgraph-reachable")
	if err != nil {
		return err
	}

	nodeMap := buildNodeMap(graph)
	reachable := computeReachable(nodeMap, rootName.Value)

	result := make([]values.Value, 0, len(reachable))
	for name := range reachable {
		result = append(result, goast.Str(name))
	}
	mc.SetValue(goast.ValueList(result))
	return nil
}
```

**Step 5: Run tests**

Run: `go test -v ./extensions/goastcg/... -run 'TestMapCallgraph_Reachable|TestGoCallgraphReachable' -timeout 120s`
Expected: PASS.

**Step 6: Run full suite + lint**

Run: `make lint && go test -v ./extensions/goastcg/... -timeout 120s`
Expected: Clean.

**Step 7: Commit**

```
feat(goastcg): add go-callgraph-reachable transitive closure

BFS from a named root function, following outgoing call edges.
Returns a flat list of reachable function name strings.
Internally builds a name->node map for O(n) setup + O(n+e) BFS.
```

---

### Task 5: Integration test and error edge cases

End-to-end test using the Scheme API on a real package. Also test algorithm-specific behavior.

**Files:**
- Modify: `extensions/goastcg/prim_callgraph_test.go`

**Step 1: Write the integration test**

Append to `prim_callgraph_test.go`:

```go
func TestIntegration_CallgraphQuery(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Build static callgraph for the goast extension.
	runScheme(t, engine,
		`(define cg (go-callgraph "github.com/aalpar/wile/extensions/goast" 'static))`)

	// Define helpers.
	runScheme(t, engine, `
		(define (nf node key)
			(let ((e (assoc key (cdr node))))
				(if e (cdr e) #f)))`)

	// Verify the graph has cg-node entries with expected structure.
	result := runScheme(t, engine, `
		(let ((first-node (car cg)))
			(and (eq? (car first-node) 'cg-node)
			     (string? (nf first-node 'name))
			     (integer? (nf first-node 'id))
			     (list? (nf first-node 'edges-in))
			     (list? (nf first-node 'edges-out))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify edges have expected structure.
	result = runScheme(t, engine, `
		(let* ((node (car cg))
		       (edges (nf node 'edges-out)))
			(if (null? edges)
				;; Skip if this node has no outgoing edges.
				#t
				(let ((edge (car edges)))
					(and (eq? (car edge) 'cg-edge)
					     (string? (nf edge 'description))))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)

	// Verify reachable returns a list of strings.
	result = runScheme(t, engine, `
		(let ((reachable (go-callgraph-reachable cg "goast.PrimGoFormat")))
			(if (null? reachable)
				;; PrimGoFormat might be a leaf; empty is acceptable.
				#t
				(string? (car reachable))))`)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue)
}

func TestGoCallgraph_RTA_NoMain(t *testing.T) {
	// RTA on a library package (no main) should error.
	engine := newEngine(t)
	runSchemeExpectError(t, engine,
		`(go-callgraph "github.com/aalpar/wile/extensions/goast" 'rta)`)
}
```

**Step 2: Run the integration test**

Run: `go test -v ./extensions/goastcg/... -run 'TestIntegration_CallgraphQuery|TestGoCallgraph_RTA_NoMain' -timeout 120s`
Expected: PASS.

**Step 3: Run full test suite**

Run: `go test -v ./extensions/goastcg/... -timeout 120s`
Expected: All tests pass.

**Step 4: Run lint + covercheck**

Run: `make lint && make covercheck`
Expected: Clean. Coverage for `extensions/goastcg` >= 80%.

**Step 5: Commit**

```
feat(goastcg): add integration test and RTA error handling

Validates end-to-end: build callgraph, verify cg-node/cg-edge
structure, query reachable set. RTA on library packages (no main
function) produces a clear error.
```

---

### Task 6: Full verification and plan update

**Step 1: Run full test suite**

Run: `make lint && make test`
Expected: All packages pass, not just `./extensions/goastcg/...`.

**Step 2: Run covercheck**

Run: `make covercheck`
Expected: `extensions/goastcg` >= 80%.

**Step 3: Update plan status**

In `plans/GO-STATIC-ANALYSIS.md`, update the Phase 2 heading:

```
### Phase 2: `(wile goast callgraph)` — Call Graph  ✓ Complete
```

Update `plans/CLAUDE.md` to add the plan file entry:

```
| `GO-CALLGRAPH-PHASE-2.md` | Callgraph extension implementation plan (Phase 2 of GO-STATIC-ANALYSIS) | Complete |
```

**Step 4: Commit**

```
docs: mark GO-STATIC-ANALYSIS Phase 2 complete
```

---

## Post-implementation checklist

- [x] All 5 task commits on branch
- [x] `make lint` clean
- [x] `make test` passes (full suite)
- [x] `make covercheck` passes
- [x] `plans/GO-STATIC-ANALYSIS.md` updated: Phase 2 -> "Complete"
- [x] `plans/CLAUDE.md` updated with plan file entry
- [x] All 4 primitives have happy-path + error-path tests
- [x] At least one integration test validating the full pipeline
- [x] Mapper handles synthetic root node (nil Func) gracefully
- [x] RTA produces clear error when no main function exists

## Primitives summary

| Primitive | Signature | Description |
|---|---|---|
| `go-callgraph` | `(go-callgraph pattern algorithm)` | Build call graph; algorithm is `'static`, `'cha`, `'rta`, or `'vta` |
| `go-callgraph-callers` | `(go-callgraph-callers graph func-name)` | Incoming edges (who calls this function) |
| `go-callgraph-callees` | `(go-callgraph-callees graph func-name)` | Outgoing edges (what this function calls) |
| `go-callgraph-reachable` | `(go-callgraph-reachable graph root-name)` | Transitive closure: all reachable function names |
