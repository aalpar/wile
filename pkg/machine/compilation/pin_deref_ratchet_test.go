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

package compilation

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// pinDerefCeiling bounds the production sites that read a SyntaxSymbol's
// definition-site pin by type-asserting ResolvedBinding. The one site is
// environment.PinnedIndex; everything else asks it or PinnedBinding.
//
// The read lives in pkg/environment so that pkg/internal/validate, which cannot
// import this package, can reach it too. Moving the helper here would leave
// validate to re-derive the cast, and this count would say so.
const pinDerefCeiling = 1

// pinDerefOwner is the one site the ceiling admits, as "<file>:<func>". The
// count alone cannot tell the owner from a stray cast that replaced it.
const pinDerefOwner = "pkg/environment/global_environment_frame.go:PinnedIndex"

// pinDerefRoot is the module root, relative to this file's directory. The whole
// module is walked, not a package list: a cast reintroduced anywhere counts.
const pinDerefRoot = "../../.."

// isPinDeref matches a type assertion or type switch on `<expr>.ResolvedBinding`,
// whatever the asserted type. Asserting *GlobalIndex is today's only shape, but
// the property is that one function knows the pin's representation.
//
// The match is syntactic: a cast on a copy (`rb := x.ResolvedBinding;
// rb.(T)`) or on a parenthesized selector is not seen. It catches the shape
// the four replaced sites had, not every way to spell a read.
func isPinDeref(n ast.Node) bool {
	ta, ok := n.(*ast.TypeAssertExpr)
	if !ok {
		return false
	}
	sel, ok := ta.X.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "ResolvedBinding"
}

func TestPinDerefSiteCount(t *testing.T) {
	var sites []string

	err := filepath.WalkDir(pinDerefRoot, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		name := d.Name()
		if d.IsDir() {
			if path != pinDerefRoot && (strings.HasPrefix(name, ".") || name == "testdata") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if !bytes.Contains(src, []byte("ResolvedBinding")) {
			return nil
		}
		fset := token.NewFileSet()
		file, err := parser.ParseFile(fset, path, src, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(pinDerefRoot, path)
		if err != nil {
			return err
		}
		for _, decl := range file.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok {
				continue
			}
			ast.Inspect(fn, func(n ast.Node) bool {
				if isPinDeref(n) {
					pos := fset.Position(n.Pos())
					sites = append(sites, fmt.Sprintf("%s:%s (line %d)", filepath.ToSlash(rel), fn.Name.Name, pos.Line))
				}
				return true
			})
		}
		return nil
	})
	qt.Assert(t, err, qt.IsNil)

	sort.Strings(sites)
	qt.Assert(t, len(sites) <= pinDerefCeiling, qt.IsTrue,
		qt.Commentf("%d production sites type-assert ResolvedBinding, ceiling %d; use environment.PinnedIndex or PinnedBinding:\n  %s",
			len(sites), pinDerefCeiling, strings.Join(sites, "\n  ")))
	// Presence half: zero sites would also satisfy the ceiling, and would mean
	// the walk stopped seeing the helper (moved root, renamed field), not that
	// the reads vanished.
	qt.Assert(t, len(sites), qt.Not(qt.Equals), 0,
		qt.Commentf("no site found; the walk no longer reaches environment.PinnedIndex"))
	qt.Assert(t, strings.HasPrefix(sites[0], pinDerefOwner+" "), qt.IsTrue,
		qt.Commentf("the one cast is %s, want it in %s", sites[0], pinDerefOwner))
}
