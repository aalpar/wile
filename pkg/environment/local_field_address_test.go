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

// The comment on EnvironmentFrame.local lists the functions that explicitly take
// the address of that field (&p.local). This checks the list: a new accessor
// that returns &p.local fails here until the list is updated, which a compiler
// never would. It does not see implicit address-taking through pointer-receiver
// calls on the field, and the comment itself is maintained by hand.
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
				// The match is on the selector name alone, ignoring the receiver's
				// type: a BindingRef.local written &r.local would be a false
				// positive.
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
		"bindingIDAt",
	})
}
