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

	// Every non-entry block should have idom set (not #f).
	tuple := result.(values.Tuple)
	first := true
	for !values.IsEmptyList(tuple) {
		pair := tuple.(*values.Pair)
		block := pair.Car()
		if !first {
			idom, ok := goast.GetField(block.(*values.Pair).Cdr(), "idom")
			c.Assert(ok, qt.IsTrue)
			c.Assert(idom, qt.Not(qt.Equals), values.FalseValue,
				qt.Commentf("non-entry block should have an idom"))
		}
		first = false
		tuple = pair.Cdr().(values.Tuple)
	}
}
