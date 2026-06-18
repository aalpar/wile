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
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestSyntaxCompilersRegistry(t *testing.T) {
	// RegisterSyntaxCompilers binds SyntaxCompiler values into
	// env.Compile(). After registration, LookupSyntaxCompiler should
	// find them by symbol with nil scopes.
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		{name: "syntax", formName: "syntax"},
		{name: "syntax-case", formName: "syntax-case"},
		{name: "meta", formName: "meta"},
		{name: "include", formName: "include"},
		{name: "include-ci", formName: "include-ci"},
		{name: "define-syntax", formName: "define-syntax"},
		{name: "define-library", formName: "define-library"},
		{name: "library (R6RS alias)", formName: "library"},
		{name: "import", formName: "import"},
		{name: "export", formName: "export"},
		{name: "unquote", formName: "unquote"},
		{name: "unquote-splicing", formName: "unquote-splicing"},
		{name: "quasisyntax", formName: "quasisyntax"},
		{name: "unsyntax", formName: "unsyntax"},
		{name: "unsyntax-splicing", formName: "unsyntax-splicing"},
		{name: "with-syntax", formName: "with-syntax"},
		{name: "cond-expand", formName: "cond-expand"},
		{name: "define-for-syntax", formName: "define-for-syntax"},
		{name: "begin-for-syntax", formName: "begin-for-syntax"},
		{name: "eval-when", formName: "eval-when"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			sc := LookupSyntaxCompiler(env, sym, nil)
			qt.Assert(t, sc, qt.IsNotNil, qt.Commentf("LookupSyntaxCompiler(%q) returned nil", tc.formName))
			qt.Assert(t, sc.Name(), qt.Equals, tc.formName)
		})
	}
}

func TestSyntaxCompilersRegistryLookupMiss(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sym := values.NewSymbol("not-a-syntax-compiler")
	sc := LookupSyntaxCompiler(env, sym, nil)
	qt.Assert(t, sc, qt.IsNil)
}

func TestSyntaxCompilersRegistryCoreFormsNotRegistered(t *testing.T) {
	// Core forms (if, define, lambda, etc.) are handled by Tier 1 validated
	// compilers registered in the forms package, NOT as SyntaxCompiler
	// bindings in the compile environment. Verify they do NOT appear here.
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	tcs := []struct {
		name     string
		formName string
	}{
		{name: "if not in syntax compilers", formName: "if"},
		{name: "define not in syntax compilers", formName: "define"},
		{name: "lambda not in syntax compilers", formName: "lambda"},
		{name: "set! not in syntax compilers", formName: "set!"},
		{name: "begin not in syntax compilers", formName: "begin"},
		{name: "quote not in syntax compilers", formName: "quote"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			sym := values.NewSymbol(tc.formName)
			sc := LookupSyntaxCompiler(env, sym, nil)
			qt.Assert(t, sc, qt.IsNil, qt.Commentf("%q should not be a SyntaxCompiler", tc.formName))
		})
	}
}
