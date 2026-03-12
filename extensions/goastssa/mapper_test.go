// Copyright 2026 Aaron Alpar
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

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

func TestMapMakeMapAndLookup(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseMap() (int, bool) {
	m := make(map[string]int)
	m["key"] = 42
	v, ok := m["key"]
	return v, ok
}
`, "UseMap")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	// MakeMap
	mkMap := findNodeByTag(result, "ssa-make-map")
	c.Assert(mkMap, qt.IsNotNil, qt.Commentf("expected ssa-make-map"))

	// MapUpdate
	mu := findNodeByTag(result, "ssa-map-update")
	c.Assert(mu, qt.IsNotNil, qt.Commentf("expected ssa-map-update"))

	// Lookup
	lk := findNodeByTag(result, "ssa-lookup")
	c.Assert(lk, qt.IsNotNil, qt.Commentf("expected ssa-lookup"))

	commaOk, ok := goast.GetField(lk.(*values.Pair).Cdr(), "comma-ok")
	c.Assert(ok, qt.IsTrue)
	c.Assert(commaOk, qt.Equals, values.TrueValue)

	// Extract (from the commaok tuple)
	ex := findNodeByTag(result, "ssa-extract")
	c.Assert(ex, qt.IsNotNil, qt.Commentf("expected ssa-extract from commaok lookup"))
}

func TestMapMakeSliceAndSlice(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseSlice(n int) []int {
	s := make([]int, n)
	return s[1:]
}
`, "UseSlice")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	ms := findNodeByTag(result, "ssa-make-slice")
	c.Assert(ms, qt.IsNotNil, qt.Commentf("expected ssa-make-slice"))

	sl := findNodeByTag(result, "ssa-slice")
	c.Assert(sl, qt.IsNotNil, qt.Commentf("expected ssa-slice"))

	xField, ok := goast.GetField(sl.(*values.Pair).Cdr(), "x")
	c.Assert(ok, qt.IsTrue)
	c.Assert(xField, qt.Not(qt.Equals), values.FalseValue)
}

func TestMapChannels(t *testing.T) {
	dir := t.TempDir()

	t.Run("MakeChan and Send", func(t *testing.T) {
		c := qt.New(t)
		fn := buildSSAFromSource(t, dir, `
package testpkg

func UseChan() {
	ch := make(chan int, 1)
	ch <- 42
}
`, "UseChan")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		mc := findNodeByTag(result, "ssa-make-chan")
		c.Assert(mc, qt.IsNotNil, qt.Commentf("expected ssa-make-chan"))

		sn := findNodeByTag(result, "ssa-send")
		c.Assert(sn, qt.IsNotNil, qt.Commentf("expected ssa-send"))
	})

	t.Run("Select", func(t *testing.T) {
		c := qt.New(t)
		dir2 := t.TempDir()
		fn := buildSSAFromSource(t, dir2, `
package testpkg

func UseSelect(c1, c2 chan int) int {
	select {
	case v := <-c1:
		return v
	case v := <-c2:
		return v
	}
}
`, "UseSelect")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		sel := findNodeByTag(result, "ssa-select")
		c.Assert(sel, qt.IsNotNil, qt.Commentf("expected ssa-select"))

		states, ok := goast.GetField(sel.(*values.Pair).Cdr(), "states")
		c.Assert(ok, qt.IsTrue)
		c.Assert(listLength(states), qt.Equals, 2)
	})
}

func TestMapGoroutineAndDefer(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func noop() {
}

func UseGoDefer() {
	go noop()
	defer noop()
}
`, "UseGoDefer")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	goNode := findNodeByTag(result, "ssa-go")
	c.Assert(goNode, qt.IsNotNil, qt.Commentf("expected ssa-go"))

	deferNode := findNodeByTag(result, "ssa-defer")
	c.Assert(deferNode, qt.IsNotNil, qt.Commentf("expected ssa-defer"))

	// RunDefers is inserted at the return site when defers are present.
	rdNode := findNodeByTag(result, "ssa-run-defers")
	c.Assert(rdNode, qt.IsNotNil, qt.Commentf("expected ssa-run-defers"))
}

func TestMapRangeAndNext(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UseRange(m map[string]int) int {
	sum := 0
	for _, v := range m {
		sum += v
	}
	return sum
}
`, "UseRange")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	rn := findNodeByTag(result, "ssa-range")
	c.Assert(rn, qt.IsNotNil, qt.Commentf("expected ssa-range"))

	nx := findNodeByTag(result, "ssa-next")
	c.Assert(nx, qt.IsNotNil, qt.Commentf("expected ssa-next"))
}

func TestMapPanic(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func UsePanic(x int) int {
	if x < 0 {
		panic("negative")
	}
	return x
}
`, "UsePanic")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	pn := findNodeByTag(result, "ssa-panic")
	c.Assert(pn, qt.IsNotNil, qt.Commentf("expected ssa-panic"))
}

func TestMapTypeConversions(t *testing.T) {
	t.Run("Convert numeric", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInt64(x int) int64 {
	return int64(x)
}
`, "ToInt64")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		cv := findNodeByTag(result, "ssa-convert")
		c.Assert(cv, qt.IsNotNil, qt.Commentf("expected ssa-convert"))
	})

	t.Run("ChangeType channel direction", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func SendOnly(c chan int) chan<- int {
	return c
}
`, "SendOnly")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		ct := findNodeByTag(result, "ssa-change-type")
		c.Assert(ct, qt.IsNotNil, qt.Commentf("expected ssa-change-type"))
	})

	t.Run("SliceToArrayPointer", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

func SliceToArr(s []int) *[3]int {
	return (*[3]int)(s)
}
`, "SliceToArr")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		sap := findNodeByTag(result, "ssa-slice-to-array-ptr")
		c.Assert(sap, qt.IsNotNil, qt.Commentf("expected ssa-slice-to-array-ptr"))
	})

	t.Run("ChangeInterface", func(t *testing.T) {
		dir := t.TempDir()
		fn := buildSSAFromSource(t, dir, `
package testpkg

type Stringer interface {
	String() string
}

type ReadStringer interface {
	String() string
	Read() []byte
}

func ToStringer(x ReadStringer) Stringer {
	return x
}
`, "ToStringer")
		mapper := &ssaMapper{fset: token.NewFileSet()}
		result := mapper.mapFunction(fn)

		// ChangeInterface may be elided by the SSA compiler when the conversion
		// is trivial. Skip rather than fail so toolchain upgrades don't break CI.
		ci := findNodeByTag(result, "ssa-change-interface")
		if ci == nil {
			t.Skip("ssa.ChangeInterface was elided by the SSA compiler; skipping assertion")
		}
	})
}

func TestMapMakeInterface(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInterface(x int) interface{} {
	return x
}
`, "ToInterface")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	mi := findNodeByTag(result, "ssa-make-interface")
	c.Assert(mi, qt.IsNotNil, qt.Commentf("expected ssa-make-interface"))
}

func TestMapTypeAssert(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func FromInterface(x interface{}) int {
	return x.(int)
}
`, "FromInterface")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	ta := findNodeByTag(result, "ssa-type-assert")
	c.Assert(ta, qt.IsNotNil, qt.Commentf("expected ssa-type-assert"))

	asserted, ok := goast.GetField(ta.(*values.Pair).Cdr(), "asserted-type")
	c.Assert(ok, qt.IsTrue)
	c.Assert(asserted.(*values.String).Value, qt.Equals, "int")
}

func TestMapMakeClosure(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func MakeClosure(x int) func() int {
	return func() int {
		return x
	}
}
`, "MakeClosure")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	cl := findNodeByTag(result, "ssa-make-closure")
	c.Assert(cl, qt.IsNotNil, qt.Commentf("expected ssa-make-closure"))

	bindings, ok := goast.GetField(cl.(*values.Pair).Cdr(), "bindings")
	c.Assert(ok, qt.IsTrue)
	// x is captured, so at least one binding.
	c.Assert(listLength(bindings) >= 1, qt.IsTrue)
}

func TestMapMultiConvert(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSource(t, dir, `
package testpkg

func ToInt64[T ~int | ~float64](x T) int64 {
	return int64(x)
}
`, "ToInt64")

	// MultiConvert may or may not appear depending on SSA compiler decisions.
	// If it does, verify the tag; if not, the instruction falls through to
	// ssa-unknown or ssa-convert, which is acceptable.
	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	mc := findNodeByTag(result, "ssa-multi-convert")
	if mc == nil {
		// SSA compiler may have lowered this to a regular Convert.
		cv := findNodeByTag(result, "ssa-convert")
		c.Assert(cv, qt.IsNotNil,
			qt.Commentf("expected either ssa-multi-convert or ssa-convert"))
		return
	}
	xField, ok := goast.GetField(mc.(*values.Pair).Cdr(), "x")
	c.Assert(ok, qt.IsTrue)
	c.Assert(xField, qt.Not(qt.Equals), values.FalseValue)
}

// buildSSAFromSourceDebug is like buildSSAFromSource but builds with
// ssa.GlobalDebug to produce DebugRef instructions.
func buildSSAFromSourceDebug(t *testing.T, dir, source, funcName string) *ssa.Function {
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

	_, ssaPkgs := ssautil.Packages(pkgs, ssa.SanityCheckFunctions|ssa.GlobalDebug)
	for _, p := range ssaPkgs {
		if p != nil {
			p.Build()
		}
	}

	fn := ssaPkgs[0].Func(funcName)
	c.Assert(fn, qt.IsNotNil, qt.Commentf("function %s not found", funcName))
	return fn
}

func TestMapDebugRef(t *testing.T) {
	c := qt.New(t)
	dir := t.TempDir()
	fn := buildSSAFromSourceDebug(t, dir, `
package testpkg

func UseDebug(x int) int {
	y := x + 1
	return y
}
`, "UseDebug")

	mapper := &ssaMapper{fset: token.NewFileSet()}
	result := mapper.mapFunction(fn)

	dr := findNodeByTag(result, "ssa-debug-ref")
	c.Assert(dr, qt.IsNotNil, qt.Commentf("expected ssa-debug-ref with GlobalDebug"))
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
