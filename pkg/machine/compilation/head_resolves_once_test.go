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
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestHeadPrimitiveExpanderDoesNotReResolve pins that the expander resolves a
// symbol head ONCE per dispatch rather than twice.
//
// ExpandSyntaxExpression asks lookupMacroBinding, whose arm 1 resolves the head,
// and then — on a miss — asks lookupHeadPrimitiveExpander, which resolved the
// same key again off the same receiver one call later: same env, same symbol,
// same scope set, no binding created in between. The fold passes arm 1's binding
// through instead.
//
// THE GATE IS A SHAPE ASSERTION, AND THAT IS HONEST RATHER THAN IDEAL, because
// the change is a deletion with no behavioural seam. The design's proposed
// ratchet — wrap the test env's frame with a counting GetBinding — is
// UNBUILDABLE: ExpanderTimeContinuation.env is a concrete
// *environment.EnvironmentFrame and GetBinding is a concrete method, so there is
// nothing to wrap. The one exported counter that does exist,
// GlobalEnvironmentFrame.BulkResolutionCount, reads zero delta at n=1/11/101/201
// because bulk rows are consulted only on a miss.
//
// It is deliberately narrow: it finds the FuncDecl and fails only on a selector
// call named GetBinding inside it. The SealedBindingAt and
// LookupPrimitiveExpander calls the fix keeps are untouched, so the assertion
// goes green only when the binding is passed in rather than re-derived.
func TestHeadPrimitiveExpanderDoesNotReResolve(t *testing.T) {
	c := qt.New(t)

	const (
		path   = "expander_time_continuation.go"
		target = "lookupHeadPrimitiveExpander"
	)

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	var (
		found  bool
		resolv []string
	)
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok || fn.Name.Name != target {
			continue
		}
		found = true
		ast.Inspect(fn.Body, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			if sel.Sel.Name == "GetBinding" {
				resolv = append(resolv, fset.Position(sel.Sel.Pos()).String())
			}
			return true
		})
	}

	// Without this the walk goes vacuously green if the function is ever renamed
	// or factored away — the failure mode a pure absence assertion cannot see.
	c.Assert(found, qt.IsTrue, qt.Commentf("%s: no FuncDecl named %s; the assertion below would be vacuous", path, target))

	c.Assert(resolv, qt.HasLen, 0,
		qt.Commentf("%s must take its head binding as a parameter, not re-resolve it; sites: %v", target, resolv))
}

// TestLocalVariableCheckStaysAheadOfHeadResolution pins the boundary the fold
// must NOT cross, and its artefact is this test plus the comment at the call
// site rather than a behavioural probe.
//
// Exactly TWO lookups are unified. hasLocalVariableBinding stays first, ahead of
// both. Folding it in as well would move an ErrAmbiguousBinding panic EARLIER:
// today an ambiguous head shadowed by a local variable binding never reaches
// GetBinding at all. That ordering is not constructible from Scheme source — an
// ambiguous head needs two equal-cardinality incomparable scope sets, and the
// only in-tree constructions are hand-built frames in pkg/environment — so it
// cannot be pinned behaviourally at the level it protects.
func TestLocalVariableCheckStaysAheadOfHeadResolution(t *testing.T) {
	c := qt.New(t)

	const (
		path   = "expander_time_continuation.go"
		target = "ExpandSyntaxExpression"
	)

	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, parser.ParseComments)
	c.Assert(err, qt.IsNil)

	order := map[string]int{}
	var found bool
	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok || fn.Name.Name != target {
			continue
		}
		found = true
		ast.Inspect(fn.Body, func(n ast.Node) bool {
			call, ok := n.(*ast.CallExpr)
			if !ok {
				return true
			}
			sel, ok := call.Fun.(*ast.SelectorExpr)
			if !ok {
				return true
			}
			_, seen := order[sel.Sel.Name]
			if !seen {
				order[sel.Sel.Name] = fset.Position(sel.Sel.Pos()).Line
			}
			return true
		})
	}
	c.Assert(found, qt.IsTrue, qt.Commentf("%s: no FuncDecl named %s", path, target))

	local, ok := order["hasLocalVariableBinding"]
	c.Assert(ok, qt.IsTrue, qt.Commentf("%s no longer checks for a local variable binding", target))
	macro, ok := order["lookupMacroBinding"]
	c.Assert(ok, qt.IsTrue, qt.Commentf("%s no longer resolves a macro head", target))

	c.Assert(local < macro, qt.IsTrue,
		qt.Commentf("hasLocalVariableBinding (line %d) must precede lookupMacroBinding (line %d): R7RS 4.2.2 shadowing, and folding it in would move an ErrAmbiguousBinding panic earlier", local, macro))
}
