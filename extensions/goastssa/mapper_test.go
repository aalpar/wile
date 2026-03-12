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

func TestMapField(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	// ssa.Field is produced when accessing a field of a struct *value* (not pointer).
	fn := buildSSAFromSource(t, dir, `
package testpkg

type Point struct {
	X int
	Y int
}

func makePoint() Point {
	return Point{X: 1, Y: 2}
}

func GetX() int {
	return makePoint().X
}
`, "GetX")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	found := findNodeByTag(result, "ssa-field")
	c.Assert(found, qt.IsNotNil, qt.Commentf("expected ssa-field in SSA of GetX"))

	fieldName, ok := goast.GetField(found.(*values.Pair).Cdr(), "field")
	c.Assert(ok, qt.IsTrue)
	c.Assert(fieldName.(*values.String).Value, qt.Equals, "X")
}

func TestMapIndex(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	// ssa.Index is produced when indexing an array *value* (not slice pointer).
	fn := buildSSAFromSource(t, dir, `
package testpkg

type Arr [3]int

func makeArr() Arr {
	return Arr{1, 2, 3}
}

func GetFirst() int {
	return makeArr()[0]
}
`, "GetFirst")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	found := findNodeByTag(result, "ssa-index")
	c.Assert(found, qt.IsNotNil, qt.Commentf("expected ssa-index in SSA of GetFirst"))
}

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
			listVal, isListVal := entry.Cdr().(values.Tuple)
			if isListVal {
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
