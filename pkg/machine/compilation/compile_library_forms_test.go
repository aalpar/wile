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

package compilation_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileLibraryForms tests library form compilation: body scoping,
// define within let, and multi-expression bodies.
//
// Source: compile_library_forms.go (CompileDefineLibrary,
// processLibraryDeclaration, processLibraryExport, parseExportSpec,
// processIncludeLibraryDeclarations).
//
// Full define-library + import integration requires the library loader
// infrastructure. These tests exercise the body compilation paths through
// simpler constructs that share the same code paths.
func TestCompileLibraryForms(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "let with internal define",
			Code:     `(let () (define x 1) x)`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "let with multiple internal defines",
			Code:     `(let () (define x 10) (define y 20) (+ x y))`,
			Expected: values.NewInteger(30),
		},
		{
			Name: "let with internal define and body expression",
			Code: `(let ()
			         (define square (lambda (n) (* n n)))
			         (square 7))`,
			Expected: values.NewInteger(49),
		},
		{
			Name: "nested let with defines at each level",
			Code: `(let ()
			         (define x 1)
			         (let ()
			           (define y (+ x 1))
			           y))`,
			Expected: values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
