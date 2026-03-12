package goastcg

import (
	"go/token"
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/tools/go/callgraph/static"
	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"

	"github.com/aalpar/wile/extensions/goast"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// writeTestPkg writes a Go source file and go.mod to a temp dir.
func writeTestPkg(t *testing.T, dir, source string) {
	t.Helper()
	c := qt.New(t)

	err := os.WriteFile(filepath.Join(dir, "go.mod"),
		[]byte("module testpkg\n\ngo 1.23\n"), 0o644)
	c.Assert(err, qt.IsNil)

	err = os.WriteFile(filepath.Join(dir, "main.go"),
		[]byte(source), 0o644)
	c.Assert(err, qt.IsNil)
}

// buildTestCallgraph loads Go source, builds SSA for the whole program,
// and returns a static call graph mapped to an s-expression.
func buildTestCallgraph(t *testing.T, dir, source string) (*token.FileSet, values.Value) {
	t.Helper()
	c := qt.New(t)

	writeTestPkg(t, dir, source)

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

// findCGNodeByNameSuffix searches a cg-node list for a node whose name ends with nameSuffix.
func findCGNodeByNameSuffix(graph values.Value, nameSuffix string) values.Value {
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
				s, ok := nameVal.(*values.String)
				if ok {
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

func cgListLength(v values.Value) int {
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

func TestMapCallgraph_Static(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildTestCallgraph(t, dir, `
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
	c.Assert(cgListLength(graph) > 0, qt.IsTrue,
		qt.Commentf("expected non-empty callgraph"))

	// Find main function node.
	mainNode := findCGNodeByNameSuffix(graph, ".main")
	c.Assert(mainNode, qt.IsNotNil, qt.Commentf("expected main node"))

	// main should have edges-out to helper.
	edgesOut, ok := goast.GetField(mainNode.(*values.Pair).Cdr(), "edges-out")
	c.Assert(ok, qt.IsTrue)
	c.Assert(cgListLength(edgesOut) > 0, qt.IsTrue,
		qt.Commentf("expected main to have outgoing edges"))

	// helper should have edges-out to leaf.
	helperNode := findCGNodeByNameSuffix(graph, ".helper")
	c.Assert(helperNode, qt.IsNotNil, qt.Commentf("expected helper node"))
	helperOut, ok := goast.GetField(helperNode.(*values.Pair).Cdr(), "edges-out")
	c.Assert(ok, qt.IsTrue)
	c.Assert(cgListLength(helperOut) > 0, qt.IsTrue,
		qt.Commentf("expected helper to call leaf"))

	// leaf should have edges-in from helper.
	leafNode := findCGNodeByNameSuffix(graph, ".leaf")
	c.Assert(leafNode, qt.IsNotNil, qt.Commentf("expected leaf node"))
	leafIn, ok := goast.GetField(leafNode.(*values.Pair).Cdr(), "edges-in")
	c.Assert(ok, qt.IsTrue)
	c.Assert(cgListLength(leafIn) > 0, qt.IsTrue,
		qt.Commentf("expected leaf to be called by helper"))
}

func TestMapCallgraph_Reachable(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildTestCallgraph(t, dir, `
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

func TestMapCallgraph_EdgeFields(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()

	_, graph := buildTestCallgraph(t, dir, `
package testpkg

func caller() {
	callee()
}

func callee() {}
`)

	callerNode := findCGNodeByNameSuffix(graph, ".caller")
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
