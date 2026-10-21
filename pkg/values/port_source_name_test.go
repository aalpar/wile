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
// parameter nor has a "…Named" sibling in the same file. A factory added
// without a way to name its port moves the count, with no hand-written table
// to forget it in. The ceiling is the honest count after the fix: the two
// FromReader forms gained siblings; the *bufio.Reader forms and the in-memory
// ports (string, bytevector) have no name to carry.
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
		if strings.HasSuffix(name, "Named") {
			continue
		}
		_, hasSibling := declared[name+"Named"]
		if hasSibling || takesName(fn) {
			continue
		}
		q = append(q, name)
	}
	return q
}

func takesName(fn *ast.FuncDecl) bool {
	for _, field := range fn.Type.Params.List {
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
	c.Assert(len(unnamed) <= ceiling, qt.IsTrue,
		qt.Commentf("%d input-capable port factories cannot name their port (ceiling %d): %v",
			len(unnamed), ceiling, unnamed))
}

func TestNamedPortConstructorsCarryTheName(t *testing.T) {
	tests := []struct {
		name string
		port *PortObject
	}{
		{"character input", NewCharacterInputPortFromReaderNamed(strings.NewReader(""), "in.scm")},
		{"binary input", NewBinaryInputPortFromReaderNamed(strings.NewReader(""), "in.scm")},
		{"character output", NewCharacterOutputPortFromWriterNamed(&bytes.Buffer{}, "in.scm")},
		{"binary output", NewBinaryOutputPortFromWriterNamed(&bytes.Buffer{}, "in.scm")},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.New(t).Assert(tc.port.SourceName(), qt.Equals, "in.scm")
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
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			qt.New(t).Assert(tc.port.SourceName(), qt.Equals, "")
		})
	}
}

// A name is provenance, not identity: port equality is unchanged.
func TestPortEqualityIgnoresTheSourceName(t *testing.T) {
	c := qt.New(t)
	r := strings.NewReader("")
	a := NewCharacterInputPortFromReaderNamed(r, "a.scm")
	b := NewCharacterInputPortFromReaderNamed(r, "b.scm")
	c.Assert(a.EqualTo(a), qt.IsTrue)
	c.Assert(a.EqualTo(b), qt.IsFalse)
}
