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

package validate

import (
	"go/ast"
	"go/parser"
	"go/token"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// This file is the Phase 2 ratchet of the frame-release-under-flat-closures
// plan. It pins the property the four frame-release gates rest on:
//
//	an opaque subtree ⟹ bodyReferencesCaptureOperator refuses the body
//
// The gates release a frame only when the capture walk clears the body. A
// closure whose template retains the lexical env really does pin the creating
// frame, so if a shape ever became opaque to Pass 1's enumeration
// (compilation.bodyReadsThroughFrameChain) without also being unsafe to the
// capture walk, the release would be a use-after-release rather than a forgone
// optimization. Both walks now ask IsOpaqueSubtree, so the implication holds by
// construction — this file exists so that a change which breaks the
// construction is loud rather than silent.

// opaqueFixture is a shape that opaqueRawSyntax classifies, paired with a
// transparent instance of the SAME validated type. Both are required: the type
// alone does not decide opacity for *ValidatedLiteral, and a fixture set that
// only ever asserted "true" would pass a predicate that returned true always.
type opaqueFixture struct {
	// form is the validated form type name, matching the type-switch arm in
	// opaqueRawSyntax that TestOpaqueSubtreeCoversEveryArm enumerates.
	form        string
	opaque      ValidatedExpr
	transparent ValidatedExpr
}

// opaqueFixtures covers every arm of opaqueRawSyntax. Adding an arm without
// adding a row here fails TestOpaqueSubtreeCoversEveryArm.
func opaqueFixtures() []opaqueFixture {
	return []opaqueFixture{
		{
			form: "ValidatedQuasiquote",
			// `(,(set! x 1)) — an unquote can hold any expression.
			opaque: quasi(values.List(
				values.NewSymbol("unquote"),
				values.List(values.NewSymbol("set!"), values.NewSymbol("x"), values.NewInteger(1)),
			)),
			// A nil Template is opaque too (a nil payload is when we know
			// LEAST), so there is no transparent quasiquote to contrast with.
			transparent: nil,
		},
		{
			form: "ValidatedLiteral",
			// (cond-expand (else (lambda () x))) parked as a passthrough form:
			// a non-empty syntax pair, which is what makes it code and not data.
			opaque: &ValidatedLiteral{
				validatedBase: validatedBase{formName: "@literal"},
				Value: makeSyntax(values.List(
					values.NewSymbol("cond-expand"),
					values.List(values.NewSymbol("else"), values.NewSymbol("x")),
				)),
			},
			// Self-evaluating data: never a syntax pair.
			transparent: &ValidatedLiteral{
				validatedBase: validatedBase{formName: "@literal"},
				Value:         makeSyntax(values.NewInteger(42)),
			},
		},
	}
}

// neverCaptureOp resolves nothing as a capture primitive. The implication under
// test must hold on opacity ALONE — a stub that recognized call/cc would let a
// fixture pass for the wrong reason.
func neverCaptureOp(*syntax.SyntaxSymbol) bool {
	return false
}

// TestOpaqueSubtreeCoversEveryArm fails when opaqueRawSyntax grows a third
// shape that opaqueFixtures does not exercise.
//
// The check is structural, in the shape free_vars_test.go's
// TestCollectFreeVarsCoversEveryBinder already uses: parse this package's own
// source, read the type-switch arms out of opaqueRawSyntax, and compare against
// the fixture table. It fails in BOTH directions, so the table cannot drift out
// of date either.
func TestOpaqueSubtreeCoversEveryArm(t *testing.T) {
	c := qt.New(t)
	arms := opaqueRawSyntaxArms(t)
	// Vacuity guard: a parse that found nothing would pass everything below.
	c.Assert(len(arms) > 0, qt.IsTrue,
		qt.Commentf("no type-switch arm was discovered — the go/ast scan is "+
			"broken, not opaqueRawSyntax"))

	covered := map[string]bool{}
	for _, f := range opaqueFixtures() {
		covered[f.form] = true
	}
	c.Assert(covered, qt.DeepEquals, arms,
		qt.Commentf("opaqueRawSyntax's shape set changed. Every opaque shape "+
			"must ALSO be refused by bodyReferencesCaptureOperator, or the "+
			"frame-release gates release a frame a retained closure still "+
			"reads. Add the fixture, then this map."))
}

// TestOpaqueSubtreeImpliesCaptureRefusal is the implication itself, asserted at
// the top level of a body and again through a nested lambda.
//
// The nesting arm is not decoration: both walks are transitive through
// WalkSubExprs, and the frame at risk is the OUTER one — an opaque subtree
// buried in an inner lambda must disqualify the enclosing body, or the chain
// the inner closure reads through is severed above it.
func TestOpaqueSubtreeImpliesCaptureRefusal(t *testing.T) {
	c := qt.New(t)
	for _, f := range opaqueFixtures() {
		t.Run(f.form, func(t *testing.T) {
			c := qt.New(t)
			c.Assert(IsOpaqueSubtree(f.opaque), qt.IsTrue)
			c.Assert(bodyReferencesCaptureOperator([]ValidatedExpr{f.opaque}, neverCaptureOp),
				qt.IsTrue,
				qt.Commentf("an opaque %s must be refused on opacity alone", f.form))

			// (g (lambda () <opaque>)) — the outer body must still refuse.
			nested := call(symRef("g"), lam(f.opaque))
			c.Assert(bodyReferencesCaptureOperator([]ValidatedExpr{nested}, neverCaptureOp),
				qt.IsTrue,
				qt.Commentf("an opaque %s inside a nested lambda must refuse "+
					"the ENCLOSING body", f.form))

			if f.transparent == nil {
				return
			}
			c.Assert(IsOpaqueSubtree(f.transparent), qt.IsFalse)
			c.Assert(bodyReferencesCaptureOperator([]ValidatedExpr{f.transparent}, neverCaptureOp),
				qt.IsFalse,
				qt.Commentf("a transparent %s must not be refused — otherwise "+
					"the implication above holds vacuously", f.form))
		})
	}
	// Guard against a predicate that is true for everything: an ordinary call
	// carrying neither an opaque subtree nor a capture operator must clear.
	clean := []ValidatedExpr{call(symRef("+"), symRef("a"), symRef("b"))}
	c.Assert(bodyReferencesCaptureOperator(clean, neverCaptureOp), qt.IsFalse)
}

// TestIsOpaqueSubtreeMatchesPayloadHalf pins the two halves of the same
// classification together. IsOpaqueSubtree is the exported boolean; callers
// that scan still take the payload from opaqueRawSyntax, and a *ValidatedQuasiquote
// with a nil Template is opaque with a nil payload — the case where reading
// "no payload" as "transparent" would be exactly backwards.
func TestIsOpaqueSubtreeMatchesPayloadHalf(t *testing.T) {
	c := qt.New(t)
	exprs := []ValidatedExpr{
		symRef("x"),
		call(symRef("f"), symRef("x")),
		lam(symRef("x")),
		&ValidatedQuasiquote{validatedBase: validatedBase{formName: "quasiquote"}},
	}
	for _, f := range opaqueFixtures() {
		exprs = append(exprs, f.opaque)
		if f.transparent != nil {
			exprs = append(exprs, f.transparent)
		}
	}
	for _, e := range exprs {
		_, want := opaqueRawSyntax(e)
		c.Assert(IsOpaqueSubtree(e), qt.Equals, want,
			qt.Commentf("IsOpaqueSubtree disagrees with opaqueRawSyntax on %T", e))
	}

	// The nil-Template quasiquote, stated as its own assertion rather than left
	// to the loop: opaque, with nothing to scan.
	nilTemplate := &ValidatedQuasiquote{validatedBase: validatedBase{formName: "quasiquote"}}
	raw, ok := opaqueRawSyntax(nilTemplate)
	c.Assert(ok, qt.IsTrue)
	c.Assert(raw, qt.IsNil)
}

// opaqueRawSyntaxArms returns the validated form type names opaqueRawSyntax
// switches on, read out of this package's own source.
func opaqueRawSyntaxArms(t *testing.T) map[string]bool {
	t.Helper()
	fset := token.NewFileSet()
	f, err := parser.ParseFile(fset, "opaque_subtree.go", nil, 0)
	if err != nil {
		t.Fatalf("parse opaque_subtree.go: %v", err)
	}
	q := map[string]bool{}
	for _, d := range f.Decls {
		fd, ok := d.(*ast.FuncDecl)
		if !ok || fd.Name.Name != "opaqueRawSyntax" || fd.Body == nil {
			continue
		}
		ast.Inspect(fd.Body, func(n ast.Node) bool {
			cc, ok := n.(*ast.CaseClause)
			if !ok {
				return true
			}
			for _, expr := range cc.List {
				star, ok := expr.(*ast.StarExpr)
				if !ok {
					continue
				}
				ident, ok := star.X.(*ast.Ident)
				if ok {
					q[ident.Name] = true
				}
			}
			return true
		})
	}
	return q
}
