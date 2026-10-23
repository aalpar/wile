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

package wile_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// originChainKinds is one row per transformer kind. Each definition expands
// `(bad y)` to the malformed `(let ((y)) y)`. The malformed binding pair is
// built by the transformer itself, so the row measures a node the macro
// produced; a binding pair that arrived through the macro's input is the
// pass-through case, covered by TestMacroOriginChainNestsAndSparesPassThrough.
var originChainKinds = []struct {
	name string
	def  string
	opts []wile.EngineOption
}{
	{
		name: "Go syntax-rules",
		def:  `(syntax-rules () ((_ x) (let ((x)) x)))`,
	},
	{
		name: "syntax-case",
		def:  `(lambda (stx) (syntax-case stx () ((_ x) #'(let ((x)) x))))`,
	},
	{
		name: "er-macro-transformer",
		def:  `(er-macro-transformer (lambda (form rename compare) (list (rename 'let) (list (list 'y)) 'y)))`,
	},
	{
		name: "Scheme-layer syntax-rules",
		def:  `(syntax-rules () ((_ x) (let ((x)) x)))`,
		opts: []wile.EngineOption{wile.WithSchemeSyntaxForms()},
	},
	{
		name: "Scheme-layer syntax-case",
		def:  `(lambda (stx) (syntax-case stx () ((_ x) #'(let ((x)) x))))`,
		opts: []wile.EngineOption{wile.WithSchemeSyntaxForms()},
	},
	{
		name: "Scheme-layer er-macro-transformer",
		def:  `(er-macro-transformer (lambda (form rename compare) (list (rename 'let) (list (list 'y)) 'y)))`,
		opts: []wile.EngineOption{wile.WithSchemeSyntaxForms()},
	},
}

// TestMacroErrorCarriesOriginChainForEveryTransformerKind (I099): the origin
// chain was stamped inside the Go syntax-rules transform only, so `expanded
// from 'bad'` appeared for that one transformer kind. The table is two-axis:
// transformer kind x entry point. The entry-point axis is what the stamp's
// boundary placement needs, because expand-once is a documented hand-copy of
// the expansion loop's closure arm, and a kind-only table cannot see a stamp
// that lands on one entry and not the other.
func TestMacroErrorCarriesOriginChainForEveryTransformerKind(t *testing.T) {
	entries := []struct {
		name string
		use  string
	}{
		{"expand", `(bad y)`},
		{"expand-once", `(call-with-values (lambda () (expand-once #'(bad y))) (lambda (r changed) (eval r (interaction-environment))))`},
	}
	for _, kind := range originChainKinds {
		for _, entry := range entries {
			t.Run(kind.name+"/"+entry.name, func(t *testing.T) {
				c := qt.New(t)
				src := "(define-syntax bad " + kind.def + ")\n" + entry.use
				err := evalSyntaxFormsErr(t, src, kind.opts...)
				c.Assert(err, qt.IsNotNil)
				c.Assert(err.Error(), qt.Contains, "let binding must be (name init) in let form")
				c.Assert(err.Error(), qt.Contains, "expanded from 'bad'")
			})
		}
	}
}

// expandOnceEval is a program that expands form one step and evaluates the
// result, so a validation error on the expansion surfaces through expand-once.
func expandOnceEval(form string) string {
	return "(call-with-values (lambda () (expand-once #'" + form + ")) (lambda (r changed) (eval r (interaction-environment))))"
}

// TestMacroOriginChainText pins the rendered chain block, anchored on the
// message before it and on what follows, so a reordered, duplicated,
// truncated or position-less chain cannot match. The link positions record the
// behavior as shipped, not an ideal: :2:1 is the use the chain was stamped at,
// and :2:45 is where expand-once's quoted form sits in the program above.
func TestMacroOriginChainText(t *testing.T) {
	entries := []struct{ name, use, chain string }{
		{"expand", `(bad y)`, "in let form\n  expanded from 'bad' at :2:1: invalid syntax"},
		{"expand-once", expandOnceEval(`(bad y)`), "in let form\n  expanded from 'bad' at :2:45\nStack trace:"},
	}
	for _, kind := range originChainKinds {
		for _, entry := range entries {
			t.Run(kind.name+"/"+entry.name, func(t *testing.T) {
				err := evalSyntaxFormsErr(t, "(define-syntax bad "+kind.def+")\n"+entry.use, kind.opts...)
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, err.Error(), qt.Contains, entry.chain)
			})
		}
	}
}

// TestMacroOriginChainNests: an inner macro written by an outer one reports
// both links, innermost first. Where each link points records current
// behavior: the template-written `(inner x)` takes the outer use site as its
// position (UseSiteCtx), so for syntax-rules the inner link reads :3:1.
func TestMacroOriginChainNests(t *testing.T) {
	cases := []struct{ name, src, chain string }{
		{"syntax-case in syntax-case",
			"(define-syntax inner (lambda (stx) (syntax-case stx () ((_ x) #'(let ((x)) x)))))\n" +
				"(define-syntax outer (lambda (stx) (syntax-case stx () ((_ x) #'(inner x)))))\n(outer y)",
			"in let form\n  expanded from 'inner' at :2:65\n  expanded from 'outer' at :3:1: invalid syntax"},
		{"er in syntax-rules",
			"(define-syntax inner (er-macro-transformer (lambda (form rename compare) (list (rename 'let) (list (list 'y)) 'y))))\n" +
				"(define-syntax outer (syntax-rules () ((_ x) (inner x))))\n(outer y)",
			"in let form\n  expanded from 'inner' at :3:1\n  expanded from 'outer' at :3:1: invalid syntax"},
		// The keyword of the inner use is an ER rename, which carries no origin
		// of its own; the chain continues through the call pair around it.
		{"rename keyword in er",
			"(define-syntax inner (syntax-rules () ((_ x) (let ((x)) x))))\n" +
				"(define-syntax outer (er-macro-transformer (lambda (form rename compare) (list (rename 'inner) 'y))))\n(outer)",
			"expanded from 'outer'"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, tc.src)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, tc.chain)
		})
	}
}

// TestMacroOriginChainThroughRenameExpandOnce: the keyword of the second step's
// use is an ER rename, which carries no origin of its own, so the chain
// continues through the call pair around it, on expand-once as on expand.
func TestMacroOriginChainThroughRenameExpandOnce(t *testing.T) {
	src := "(define-syntax inner (syntax-rules () ((_ x) (let ((x)) x))))\n" +
		"(define-syntax outer (er-macro-transformer (lambda (form rename compare) (list (rename 'inner) 'y))))\n" +
		"(call-with-values (lambda () (expand-once #'(outer)))\n" +
		"  (lambda (r ch) (call-with-values (lambda () (expand-once r))\n" +
		"    (lambda (r2 ch2) (eval r2 (interaction-environment))))))"
	err := evalSyntaxFormsErr(t, src)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "expanded from 'inner'")
	qt.Assert(t, err.Error(), qt.Contains, "expanded from 'outer'")
}

// TestMacroOriginChainSparesPassThrough: syntax the user wrote and a macro
// handed back unchanged is not attributed to the macro, for every transformer
// kind, both entry points, both syntax layers.
func TestMacroOriginChainSparesPassThrough(t *testing.T) {
	scheme := []wile.EngineOption{wile.WithSchemeSyntaxForms()}
	defs := []struct{ name, def string }{
		{"syntax-rules", `(syntax-rules () ((_ e) e))`},
		{"syntax-case", `(lambda (stx) (syntax-case stx () ((_ e) #'e)))`},
		{"er-macro-transformer", `(er-macro-transformer (lambda (f r c) (car (cdr f))))`},
	}
	layers := []struct {
		name string
		opts []wile.EngineOption
	}{{"go", nil}, {"scheme", scheme}}
	entries := []struct{ name, use string }{
		{"expand", `(pass (let ((x)) x))`},
		{"expand-once", expandOnceEval(`(pass (let ((x)) x))`)},
	}
	for _, def := range defs {
		for _, layer := range layers {
			for _, entry := range entries {
				t.Run(def.name+"/"+layer.name+"/"+entry.name, func(t *testing.T) {
					err := evalSyntaxFormsErr(t, "(define-syntax pass "+def.def+")\n"+entry.use, layer.opts...)
					qt.Assert(t, err, qt.IsNotNil)
					qt.Assert(t, err.Error(), qt.Contains, "let binding must be (name init) in let form")
					qt.Assert(t, err.Error(), qt.Not(qt.Contains), "expanded from")
				})
			}
		}
	}

	other := []struct {
		name, src string
		opts      []wile.EngineOption
	}{
		{"embedded pattern var", "(define-syntax pass (syntax-rules () ((_ e) (begin e))))\n(pass (let ((x)) x))", nil},
		{"do body", "(do ((i 0 (+ i 1))) ((= i 1)) (let ((x)) x))", nil},
		{"do body/scheme-layer", "(do ((i 0 (+ i 1))) ((= i 1)) (let ((x)) x))", scheme},
		{"vector element/syntax-rules", "(define-syntax pass (syntax-rules () ((_ #(e)) e)))\n(pass #((let ((x)) x)))", nil},
		{"vector element/syntax-case", "(define-syntax pass (lambda (stx) (syntax-case stx () ((_ #(e)) #'e))))\n(pass #((let ((x)) x)))", nil},
	}
	for _, tc := range other {
		t.Run(tc.name, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, tc.src, tc.opts...)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, "let binding must be (name init) in let form")
			qt.Assert(t, err.Error(), qt.Not(qt.Contains), "expanded from")
		})
	}
}

// TestMacroOriginChainBlamesMacroBuiltSyntax: syntax a transformer builds in
// the keyword's context carries the introduction scope, as the user's input
// does, and is the macro's all the same. Go and Scheme layers agree.
func TestMacroOriginChainBlamesMacroBuiltSyntax(t *testing.T) {
	defs := []struct{ name, def string }{
		{"er literals", `(er-macro-transformer (lambda (f r c) (list 'lambda (list 'y 'y) 1)))`},
		{"syntax-case datum->syntax on the keyword", `(lambda (stx) (syntax-case stx () ((k) (datum->syntax #'k '(lambda (y y) 1)))))`},
	}
	layers := []struct {
		name string
		opts []wile.EngineOption
	}{{"go", nil}, {"scheme", []wile.EngineOption{wile.WithSchemeSyntaxForms()}}}
	for _, def := range defs {
		for _, layer := range layers {
			t.Run(def.name+"/"+layer.name, func(t *testing.T) {
				err := evalSyntaxFormsErr(t, "(define-syntax m "+def.def+")\n(m)", layer.opts...)
				qt.Assert(t, err, qt.IsNotNil)
				qt.Assert(t, err.Error(), qt.Contains, "duplicate parameter name")
				qt.Assert(t, err.Error(), qt.Contains, "expanded from 'm' at :2:1")
			})
		}
	}
}

// TestMacroOriginChainKeepsSplicedKeywordPosition: a keyword a macro splices
// into head position is the user's identifier, with a position of its own, so
// the inner link points at it and the macro that wrote the call around it is
// not added as a second link. Only an ER rename, which has no position, falls
// back to the call pair.
func TestMacroOriginChainKeepsSplicedKeywordPosition(t *testing.T) {
	src := "(define-syntax inner (syntax-rules () ((_ a) (let ((a)) a))))\n" +
		"(define-syntax outer (syntax-rules () ((_ k) (k z))))\n(outer inner)"
	layers := []struct {
		name string
		opts []wile.EngineOption
	}{{"go", nil}, {"scheme", []wile.EngineOption{wile.WithSchemeSyntaxForms()}}}
	for _, layer := range layers {
		t.Run(layer.name, func(t *testing.T) {
			err := evalSyntaxFormsErr(t, src, layer.opts...)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, err.Error(), qt.Contains, "in let form\n  expanded from 'inner' at :3:7: invalid syntax")
			qt.Assert(t, err.Error(), qt.Not(qt.Contains), "expanded from 'outer'")
		})
	}
}

// TestSchemeLayerERConversionMatchesDatumToSyntax: the Scheme layer's ER shim
// converts a transformer's result as datum->syntax does, so a box keeps its raw
// contents and a void stays a literal. The Go layer's converter has always
// differed on both; that difference is recorded, not asserted as a goal.
func TestSchemeLayerERConversionMatchesDatumToSyntax(t *testing.T) {
	scheme := wile.WithSchemeSyntaxForms()
	box := "(define-syntax m (er-macro-transformer (lambda (f r c) (list 'quote (box 'foo)))))\n(m)"
	void := "(define-syntax m (er-macro-transformer (lambda (f r c) (if #f #f))))\n(list (m))"
	t.Run("box/scheme", func(t *testing.T) {
		qt.Assert(t, evalSyntaxForms(t, box, scheme), qt.Equals, "#&foo")
	})
	t.Run("void/scheme", func(t *testing.T) {
		qt.Assert(t, evalSyntaxForms(t, void, scheme), qt.Equals, "(#<void>)")
	})
	t.Run("void/go refuses", func(t *testing.T) {
		err := evalSyntaxFormsErr(t, void)
		qt.Assert(t, err, qt.IsNotNil)
	})
}
