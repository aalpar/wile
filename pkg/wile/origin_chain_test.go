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
// `(bad y)` to the malformed `(let ((y)) y)`, and the malformed binding pair is
// built by the transformer itself: after P4.7 an ER transformer's pass-through
// nodes keep their own SourceContext, so a binding pair that arrived through the
// macro's input would stop carrying the stamp with no code change to blame.
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

// TestMacroOriginChainNestsAndSparesPassThrough: an inner macro written by an
// outer one reports both links, and syntax the user wrote and a macro merely
// passed through is not attributed to the macro.
func TestMacroOriginChainNestsAndSparesPassThrough(t *testing.T) {
	c := qt.New(t)

	nested := "(define-syntax inner (lambda (stx) (syntax-case stx () ((_ x) #'(let ((x)) x)))))\n" +
		"(define-syntax outer (lambda (stx) (syntax-case stx () ((_ x) #'(inner x)))))\n" +
		"(outer y)"
	err := evalSyntaxFormsErr(t, nested)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "expanded from 'inner'")
	c.Assert(err.Error(), qt.Contains, "expanded from 'outer'")

	for _, def := range []string{
		`(syntax-rules () ((_ e) e))`,
		`(lambda (stx) (syntax-case stx () ((_ e) #'e)))`,
	} {
		err = evalSyntaxFormsErr(t, "(define-syntax pass "+def+")\n(pass (let ((x)) x))")
		c.Assert(err, qt.IsNotNil)
		c.Assert(err.Error(), qt.Contains, "let binding must be (name init) in let form")
		c.Assert(err.Error(), qt.Not(qt.Contains), "expanded from")
	}
}
