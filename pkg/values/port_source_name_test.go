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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// inputPortFactoriesWithoutAName is the shrink-only census behind I055-b.
//
// It parses port_constructors.go and collects every exported input-capable
// factory (New…Port… with "Input" in its name) that neither takes a name
// parameter nor has a "…WithName" sibling in the same file. A factory added
// without a way to name its port moves the count, with no hand-written table
// to forget it in. The ceiling is the honest count after the fix: the two
// FromReader forms gained siblings; the in-memory ports (string, bytevector)
// have no name to carry; and NewCharacterInputPort / NewBinaryInputPort wrap a
// caller's *bufio.Reader and are exempt because they have no production
// callers. The ceiling is an equality, so a shrink forces lowering it.
func inputPortFactoriesWithoutAName(t *testing.T) []string {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "port_constructors.go", nil, 0)
	qt.New(t).Assert(err, qt.IsNil)

	declared := map[string]*ast.FuncDecl{}
	for _, d := range f.Decls {
		fn, ok := d.(*ast.FuncDecl)
		if ok && fn.Recv == nil && fn.Name.IsExported() {
			declared[fn.Name.Name] = fn
		}
	}

	var q []string
	for name, fn := range declared {
		if !strings.HasPrefix(name, "New") || !strings.Contains(name, "Port") || !strings.Contains(name, "Input") {
			continue
		}
		if strings.HasSuffix(name, "WithName") {
			continue
		}
		_, hasSibling := declared[name+"WithName"]
		if hasSibling || takesName(fn) {
			continue
		}
		q = append(q, name)
	}
	return q
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

func TestInputPortFactoryNamingCensus(t *testing.T) {
	c := qt.New(t)
	const ceiling = 7
	unnamed := inputPortFactoriesWithoutAName(t)
	c.Assert(len(unnamed), qt.Equals, ceiling,
		qt.Commentf("input-capable port factories that cannot name their port: %v "+
			"(more than the ceiling is a new unnamed factory; fewer means lower the ceiling)",
			unnamed))
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
