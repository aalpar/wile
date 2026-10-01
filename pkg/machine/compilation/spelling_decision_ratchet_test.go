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
	"fmt"
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// A decision about what an identifier MEANS belongs to its resolved binding,
// never to the spelling at the use site — CLAUDE.local.md's binding-identity
// rule, and the whole subject of I025. This file counts the functions that still
// decide by spelling, so the number can only go down.
//
// spellingDecisionCeiling is a CEILING, not an equality. Master before I025
// carried SIX such functions and this task removes one; the two each further
// task removes ratchet the number down with it.
//
// WHY A CEILING AND NOT A LIST: a list would have to be regenerated on every
// rename, and the regeneration is where a ratchet goes falsely green (see
// memory/axis-b-manifest-line-drift). A ceiling cannot be satisfied by moving a
// decision between files or functions.
const spellingDecisionCeiling = 5

// spellingDecisionPackages are the three packages that read syntax and dispatch
// on it: the compiler and expander, the validator, and the matcher. Paths are
// relative to this file's own directory.
var spellingDecisionPackages = []string{
	".",
	"../../internal/validate",
	"../../internal/match",
}

// bindingResolvers are the calls that make a function's spelling comparison
// legitimate: a function that HAS resolved the identifier may compare the
// spelling as a documented fallback (headFormName and markerName both do). It is
// the function that compares a spelling and never resolves anything that is the
// defect this counts.
var bindingResolvers = []string{
	"GetBinding", "TryGetBinding", "ExactBinding", "ExactBindingAt",
	"DenotedForm", "SameBinding", "GetLiteralBinding", "lookupLiteralBinding",
	"headFormName", "markerName", "featureName", "asFormDenoting",
	"sameLiteralBinding", "literalNotShadowed",
}

// spellingComparison reports whether n compares an identifier's SPELLING to a
// string literal.
//
// BOTH shapes count, and the distinction is an accident of the value type rather
// than of the decision: values.Symbol.Key is a struct FIELD while
// SyntaxSymbol.Key() is a method, so `sym.Key == "..."` and `sym.Key() == "..."`
// are the same decision written against two receivers. Counting only the call
// form would have let two of the six master sites through
// (compile_syntax_form.go's two), and the ceiling would have been satisfied
// before anything was fixed.
func spellingComparison(n ast.Node) bool {
	bin, ok := n.(*ast.BinaryExpr)
	if !ok {
		return false
	}
	if bin.Op != token.EQL && bin.Op != token.NEQ {
		return false
	}
	return isKeyExpr(bin.X) && isStringLit(bin.Y) || isKeyExpr(bin.Y) && isStringLit(bin.X)
}

// isKeyExpr matches `<expr>.Key` and `<expr>.Key()`.
func isKeyExpr(e ast.Expr) bool {
	call, ok := e.(*ast.CallExpr)
	if ok {
		e = call.Fun
	}
	sel, ok := e.(*ast.SelectorExpr)
	return ok && sel.Sel.Name == "Key"
}

func isStringLit(e ast.Expr) bool {
	lit, ok := e.(*ast.BasicLit)
	return ok && lit.Kind == token.STRING
}

// resolvesABinding reports whether fn calls anything in bindingResolvers.
func resolvesABinding(fn *ast.FuncDecl) bool {
	found := false
	ast.Inspect(fn, func(n ast.Node) bool {
		call, ok := n.(*ast.CallExpr)
		if !ok {
			return true
		}
		name := ""
		switch f := call.Fun.(type) {
		case *ast.Ident:
			name = f.Name
		case *ast.SelectorExpr:
			name = f.Sel.Name
		}
		if slices.Contains(bindingResolvers, name) {
			found = true
			return false
		}
		return true
	})
	return found
}

// TestSpellingDecisionsDoNotGrow is the committed source walk I025's
// wave-level belief was replaced by. The belief it replaces was red on master
// for six reasons this phase never touches, and nothing in `make lint` or
// `make ci` runs .goast-beliefs/ anyway, so it could not have gated anything.
func TestSpellingDecisionsDoNotGrow(t *testing.T) {
	var sites []string

	for _, dir := range spellingDecisionPackages {
		entries, err := os.ReadDir(dir)
		qt.Assert(t, err, qt.IsNil, qt.Commentf("package dir %s", dir))

		for _, e := range entries {
			name := e.Name()
			if e.IsDir() || !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
				continue
			}
			path := filepath.Join(dir, name)
			fset := token.NewFileSet()
			file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
			qt.Assert(t, err, qt.IsNil, qt.Commentf("parse %s", path))

			for _, decl := range file.Decls {
				fn, ok := decl.(*ast.FuncDecl)
				if !ok || fn.Body == nil || resolvesABinding(fn) {
					continue
				}
				hit := false
				ast.Inspect(fn.Body, func(n ast.Node) bool {
					if spellingComparison(n) {
						hit = true
						return false
					}
					return true
				})
				if hit {
					sites = append(sites, fmt.Sprintf("%s:%s",
						filepath.ToSlash(path), fn.Name.Name))
				}
			}
		}
	}

	sort.Strings(sites)
	qt.Assert(t, len(sites) <= spellingDecisionCeiling, qt.IsTrue,
		qt.Commentf("%d functions decide by spelling with no binding call, ceiling %d:\n  %s",
			len(sites), spellingDecisionCeiling, strings.Join(sites, "\n  ")))
}
