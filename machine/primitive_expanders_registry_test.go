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

package machine

import (
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPrimitiveExpandersRegistry(t *testing.T) {
	// RegisterPrimitiveExpanders binds PrimitiveExpander values into
	// env.Expand(). After registration, LookupPrimitiveExpander should
	// find them by symbol with nil scopes.
	env := environment.NewTopLevelEnvironment().Runtime()
	err := RegisterPrimitiveExpanders(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		// Unchanged forms
		{name: "quote", formName: "quote"},
		{name: "define-syntax", formName: "define-syntax"},
		{name: "quasiquote", formName: "quasiquote"},
		{name: "unquote", formName: "unquote"},
		{name: "unquote-splicing", formName: "unquote-splicing"},
		{name: "include", formName: "include"},
		{name: "include-ci", formName: "include-ci"},
		{name: "define-library", formName: "define-library"},
		{name: "cond-expand", formName: "cond-expand"},
		{name: "syntax", formName: "syntax"},
		{name: "syntax-case", formName: "syntax-case"},
		{name: "er-macro-transformer", formName: "er-macro-transformer"},
		{name: "quasisyntax", formName: "quasisyntax"},
		{name: "unsyntax", formName: "unsyntax"},
		{name: "unsyntax-splicing", formName: "unsyntax-splicing"},
		{name: "with-syntax", formName: "with-syntax"},
		{name: "let-syntax", formName: "let-syntax"},
		{name: "letrec-syntax", formName: "letrec-syntax"},
		{name: "with-binding-scope", formName: "with-binding-scope"},
		{name: "syntax-error", formName: "syntax-error"},

		// Forms that expand subexpressions
		{name: "if", formName: "if"},
		{name: "begin", formName: "begin"},
		{name: "set!", formName: "set!"},
		{name: "define", formName: "define"},
		{name: "lambda", formName: "lambda"},
		{name: "case-lambda", formName: "case-lambda"},
		{name: "with-continuation-mark", formName: "with-continuation-mark"},
		{name: "import", formName: "import"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			pe := LookupPrimitiveExpander(env, sym, nil)
			qt.Assert(t, pe, qt.IsNotNil, qt.Commentf("LookupPrimitiveExpander(%q) returned nil", tc.formName))
			qt.Assert(t, pe.Name(), qt.Equals, tc.formName)
		})
	}
}

func TestPrimitiveExpandersRegistryLookupMiss(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()
	err := RegisterPrimitiveExpanders(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("not-a-primitive-expander")
	pe := LookupPrimitiveExpander(env, sym, nil)
	qt.Assert(t, pe, qt.IsNil)
}
