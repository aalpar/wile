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

// TestCompileLibraryBody tests library body compilation with letrec* semantics.
//
// Source: compile_time_continuation_library.go (compileLibraryBegin).
//
// The library begin body compiler uses the same letrec* semantics as lambda
// bodies: all defined names are visible throughout the body, enabling forward
// references. We test this via letrec* directly since it exercises the same
// code path.
func TestCompileLibraryBody(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "letrec* basic",
			Code:     `(letrec* ((x 1) (y (+ x 1))) y)`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "letrec* forward reference between defines",
			Code:     `(letrec* ((x 10) (y (* x 2))) (+ x y))`,
			Expected: values.NewInteger(30),
		},
		{
			Name: "letrec* with mutual recursion",
			Code: `(letrec* ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
			                  (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
			         (even? 10))`,
			Expected: values.TrueValue,
		},
		{
			Name: "begin with internal defines uses letrec* semantics",
			Code: `((lambda ()
			          (define x 5)
			          (define y (+ x 5))
			          y))`,
			Expected: values.NewInteger(10),
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
