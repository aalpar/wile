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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// TestCompileImport tests import compilation. The bootstrap environment
// (NewNamespaceFrameTiny) already imports (scheme base) implicitly,
// so these tests verify that the primitives are available.
//
// Source: compile_import.go (CompileImport, processLibraryImport).
//
// Full library import tests (define-library + import) are covered in
// integration/ tests. Here we verify basic import mechanics.
func TestCompileImport(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// These primitives come from (scheme base) which is implicitly imported
		{
			Name:     "arithmetic from scheme base",
			Code:     `(+ 1 2)`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "list from scheme base",
			Code:     `(list 1 2 3)`,
			Expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "string-append from scheme base",
			Code:     `(string-append "hello" " " "world")`,
			Expected: values.NewString("hello world"),
		},
		{
			Name:     "boolean predicate from scheme base",
			Code:     `(boolean? #t)`,
			Expected: values.TrueValue,
		},
		{
			Name:     "map from scheme base",
			Code:     `(map + '(1 2 3) '(10 20 30))`,
			Expected: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33)),
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
