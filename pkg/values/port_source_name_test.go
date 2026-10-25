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

package values

import (
	"bytes"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"sort"
	"strconv"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// portFactoriesWithoutAName is the shrink-only census behind I055-b.
//
// It reads every non-test file of pkg/values and collects every exported port
// factory (a New… function returning *PortObject, input or output, wherever it
// is declared) that cannot name its port: it neither stores a name parameter
// nor has a "…WithName" sibling that does. A factory added without a way to
// name its port moves the count, with no hand-written table to forget it in.
//
// "Stores" is checked, not assumed: a factory names its port when its body
// assigns its name parameter to a srcName field, or passes it to a factory
// that does. A sibling that takes a name and drops it does not count.
//
// The ceiling is the honest count. Input: the in-memory ports (string,
// bytevector) have no name to carry, and NewCharacterInputPort /
// NewBinaryInputPort wrap a caller's *bufio.Reader and are exempt because they
// have no production callers. Output: the string and bytevector buffer ports,
// and NewByteVectorOutputPortFromWriter, which has no named sibling yet. The
// ceiling is an equality, so a shrink forces lowering it.
func portFactoriesWithoutAName(files []*ast.File) []string {
	declared := map[string]*ast.FuncDecl{}
	for _, f := range files {
		for _, d := range f.Decls {
			fn, ok := d.(*ast.FuncDecl)
			if ok && fn.Recv == nil && fn.Name.IsExported() {
				declared[fn.Name.Name] = fn
			}
		}
	}
	names := map[string]bool{}
	for {
		changed := false
		for name, fn := range declared {
			if !names[name] && storesName(fn, names) {
				names[name] = true
				changed = true
			}
		}
		if !changed {
			break
		}
	}

	var q []string
	for name, fn := range declared {
		if !strings.HasPrefix(name, "New") || !returnsPortObject(fn) {
			continue
		}
		if names[name] || names[name+"WithName"] {
			continue
		}
		q = append(q, name)
	}
	sort.Strings(q)
	return q
}

// returnsPortObject reports whether fn's only result is *PortObject.
func returnsPortObject(fn *ast.FuncDecl) bool {
	res := fn.Type.Results
	if res == nil || len(res.List) != 1 {
		return false
	}
	star, ok := res.List[0].Type.(*ast.StarExpr)
	if !ok {
		return false
	}
	id, ok := star.X.(*ast.Ident)
	return ok && id.Name == "PortObject"
}

// storesName reports whether fn takes a string parameter called name and
// either assigns it to a srcName field or passes it to a function already known
// to store it (named).
func storesName(fn *ast.FuncDecl, named map[string]bool) bool {
	if !takesName(fn) || fn.Body == nil {
		return false
	}
	found := false
	ast.Inspect(fn.Body, func(n ast.Node) bool {
		if found {
			return false
		}
		switch x := n.(type) {
		case *ast.AssignStmt:
			for i, lhs := range x.Lhs {
				sel, ok := lhs.(*ast.SelectorExpr)
				if ok && sel.Sel.Name == "srcName" && i < len(x.Rhs) && isIdent(x.Rhs[i], "name") {
					found = true
				}
			}
		case *ast.CallExpr:
			callee, ok := x.Fun.(*ast.Ident)
			if !ok || !named[callee.Name] {
				return true
			}
			for _, arg := range x.Args {
				if isIdent(arg, "name") {
					found = true
				}
			}
		}
		return !found
	})
	return found
}

func isIdent(e ast.Expr, name string) bool {
	id, ok := e.(*ast.Ident)
	return ok && id.Name == name
}

// takesName reports whether fn has a parameter called name of type string.
func takesName(fn *ast.FuncDecl) bool {
	for _, field := range fn.Type.Params.List {
		typ, ok := field.Type.(*ast.Ident)
		if !ok || typ.Name != "string" {
			continue
		}
		for _, id := range field.Names {
			if id.Name == "name" {
				return true
			}
		}
	}
	return false
}

// parsePackageSources parses every non-test .go file in the current directory.
func parsePackageSources(t *testing.T) []*ast.File {
	t.Helper()
	entries, err := os.ReadDir(".")
	qt.Assert(t, err, qt.IsNil)
	fset := token.NewFileSet()
	var q []*ast.File
	for _, e := range entries {
		name := e.Name()
		if e.IsDir() || !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		f, perr := parser.ParseFile(fset, name, nil, 0)
		qt.Assert(t, perr, qt.IsNil)
		q = append(q, f)
	}
	return q
}

func TestPortFactoryNamingCensus(t *testing.T) {
	c := qt.New(t)
	const ceiling = 12
	unnamed := portFactoriesWithoutAName(parsePackageSources(t))
	c.Assert(len(unnamed), qt.Equals, ceiling,
		qt.Commentf("port factories that cannot name their port: %v "+
			"(more than the ceiling is a new unnamed factory; fewer means lower the ceiling)",
			unnamed))
}

// TestPortFactoryNamingCensusSeesItsTargets pins the census on synthetic
// source: the shapes the file-scoped, "Input"-keyed census missed.
func TestPortFactoryNamingCensusSeesItsTargets(t *testing.T) {
	const a = `package values
func NewFooInputPort() *PortObject { return nil }
func NewFooInputPortWithName(name string) *PortObject {
	q := &PortObject{}
	q.srcName = name
	return q
}
func NewBarPortFromReader() *PortObject { return NewBarPortFromReaderWithName("") }
func NewBarPortFromReaderWithName(name string) *PortObject { return &PortObject{} }
func NewBazOutputPort() *PortObject { return nil }
func NewQuxPort(name string) *PortObject { return NewFooInputPortWithName(name) }
func NewNotAPort() int { return 0 }
`
	const b = `package values
func NewElsewherePort() *PortObject { return nil }
`
	fset := token.NewFileSet()
	var files []*ast.File
	for i, src := range []string{a, b} {
		f, err := parser.ParseFile(fset, strconv.Itoa(i)+".go", src, 0)
		qt.Assert(t, err, qt.IsNil)
		files = append(files, f)
	}
	// NewFooInputPort has a storing sibling and NewQuxPort forwards its name to
	// one; NewBarPortFromReader's sibling drops the name; NewBazOutputPort has
	// no "Input" in its name; NewElsewherePort is in another file.
	qt.Assert(t, portFactoriesWithoutAName(files), qt.DeepEquals, []string{
		"NewBarPortFromReader",
		"NewBarPortFromReaderWithName",
		"NewBazOutputPort",
		"NewElsewherePort",
	})
}

func TestNamedPortConstructorsCarryTheName(t *testing.T) {
	tests := []struct {
		name string
		port *PortObject
	}{
		{"character input", NewCharacterInputPortFromReaderWithName(strings.NewReader(""), "in.scm")},
		{"binary input", NewBinaryInputPortFromReaderWithName(strings.NewReader(""), "in.scm")},
		{"character output", NewCharacterOutputPortFromWriterWithName(&bytes.Buffer{}, "in.scm")},
		{"binary output", NewBinaryOutputPortFromWriterWithName(&bytes.Buffer{}, "in.scm")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.New(t).Assert(tc.port.PortName(), qt.Equals, "in.scm")
		})
	}
}

func TestUnnamedPortConstructorsHaveNoName(t *testing.T) {
	tests := []struct {
		name string
		port *PortObject
	}{
		{"character input", NewCharacterInputPortFromReader(strings.NewReader(""))},
		{"binary input", NewBinaryInputPortFromReader(strings.NewReader(""))},
		{"character output", NewCharacterOutputPortFromWriter(&bytes.Buffer{})},
		{"binary output", NewBinaryOutputPortFromWriter(&bytes.Buffer{})},
		{"string input", NewStringInputPortWithBuffer(bytes.NewBufferString("x"))},
		{"string output", NewStringOutputPort()},
		{"bytevector input", NewByteVectorInputPortFromReader(strings.NewReader(""))},
		{"bytevector output", NewByteVectorBufferedOutputPort()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.New(t).Assert(tc.port.PortName(), qt.Equals, "")
		})
	}
}

// A name is provenance, not identity: ports that differ only in name are
// equal, so EqualTo must ignore srcName. The two ports share their datum so
// the name is the only difference; the test fails if EqualTo ever compares it.
func TestPortEqualityIgnoresThePortName(t *testing.T) {
	c := qt.New(t)
	a := NewCharacterInputPortFromReaderWithName(strings.NewReader(""), "a.scm")
	b := NewCharacterInputPortFromReaderWithName(strings.NewReader(""), "b.scm")
	b.datum = a.datum
	c.Assert(a.PortName(), qt.Not(qt.Equals), b.PortName())
	c.Assert(a.EqualTo(b), qt.IsTrue)
}

func TestNilPortHasNoName(t *testing.T) {
	var p *PortObject
	qt.New(t).Assert(p.PortName(), qt.Equals, "")
}
