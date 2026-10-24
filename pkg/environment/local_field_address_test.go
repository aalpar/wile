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

package environment

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"slices"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// The comment on EnvironmentFrame.local lists every function that takes the
// address of that field. This pins the list: a new accessor that returns
// &p.local without extending the comment fails here, which a compiler never
// would (the staleness is silent, and a textual merge of the two edits exits 0).
func TestLocalFieldAddressTakers(t *testing.T) {
	c := qt.New(t)
	entries, err := os.ReadDir(".")
	c.Assert(err, qt.IsNil)

	var found []string
	fset := token.NewFileSet()
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		file, err := parser.ParseFile(fset, name, nil, 0)
		c.Assert(err, qt.IsNil)
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				unary, ok := n.(*ast.UnaryExpr)
				if !ok || unary.Op != token.AND {
					return true
				}
				sel, ok := unary.X.(*ast.SelectorExpr)
				if ok && sel.Sel.Name == "local" {
					found = append(found, fn.Name.Name)
				}
				return true
			})
		}
	}
	slices.Sort(found)
	c.Assert(found, qt.DeepEquals, []string{
		"InitApplyFrameWithParent",
		"LocalEnvironment",
		"LocalLayout",
		"ResolveBindingID",
	})
}
