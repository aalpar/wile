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

	"github.com/aalpar/wile/internal/forms"

	qt "github.com/frankban/quicktest"
)

func TestRegisterFormsCompilers(t *testing.T) {
	// register.go init() registers compilers for both typed (Tier 1) and
	// syntax (Tier 2) forms via forms.RegisterCompiler. Verify that
	// forms.Lookup returns non-nil specs with Compile set for key forms.
	tcs := []struct {
		name     string
		formName string
	}{
		// Tier 1: typed ValidatedExpr compilers
		{name: "if registered", formName: "if"},
		{name: "define registered", formName: "define"},
		{name: "lambda registered", formName: "lambda"},
		{name: "set! registered", formName: "set!"},
		{name: "begin registered", formName: "begin"},
		{name: "quote registered", formName: "quote"},
		{name: "quasiquote registered", formName: "quasiquote"},
		{name: "case-lambda registered", formName: "case-lambda"},
		{name: "dynamic-wind registered", formName: "dynamic-wind"},
		{name: "apply registered", formName: "apply"},
		{name: "with-continuation-mark registered", formName: "with-continuation-mark"},

		// Tier 2: syntax passthrough compilers
		{name: "syntax registered", formName: "syntax"},
		{name: "syntax-case registered", formName: "syntax-case"},
		{name: "meta registered", formName: "meta"},
		{name: "include registered", formName: "include"},
		{name: "include-ci registered", formName: "include-ci"},
		{name: "define-syntax registered", formName: "define-syntax"},
		{name: "define-library registered", formName: "define-library"},
		{name: "library registered", formName: "library"},
		{name: "import registered", formName: "import"},
		{name: "export registered", formName: "export"},
		{name: "unquote registered", formName: "unquote"},
		{name: "unquote-splicing registered", formName: "unquote-splicing"},
		{name: "quasisyntax registered", formName: "quasisyntax"},
		{name: "unsyntax registered", formName: "unsyntax"},
		{name: "unsyntax-splicing registered", formName: "unsyntax-splicing"},
		{name: "with-syntax registered", formName: "with-syntax"},
		{name: "cond-expand registered", formName: "cond-expand"},
		{name: "define-for-syntax registered", formName: "define-for-syntax"},
		{name: "begin-for-syntax registered", formName: "begin-for-syntax"},
		{name: "eval-when registered", formName: "eval-when"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			spec := forms.Lookup(tc.formName)
			qt.Assert(t, spec, qt.IsNotNil, qt.Commentf("forms.Lookup(%q) returned nil", tc.formName))
			qt.Assert(t, spec.Compile, qt.IsNotNil, qt.Commentf("Compile field nil for %q", tc.formName))
		})
	}
}

func TestRegisterFormsLookupMiss(t *testing.T) {
	// A form name that was never registered should return nil.
	spec := forms.Lookup("definitely-not-a-form")
	qt.Assert(t, spec, qt.IsNil)
}
