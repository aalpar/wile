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

package core_test

import (
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// values Tests (R7RS §6.4 - Return multiple values)

func TestValuesComprehensive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Single value (direct return)
		{Name: "single value", Code: `(values 42)`, Expected: values.NewInteger(42)},

		// Multiple values with call-with-values to capture
		{Name: "two values via cwv", Code: `(call-with-values (lambda () (values 1 2)) +)`, Expected: values.NewInteger(3)},
		{Name: "three values via cwv", Code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, Expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Zero values
		{Name: "zero values via cwv", Code: `(call-with-values (lambda () (values)) (lambda () 'empty))`, Expected: values.NewSymbol("empty")},

		// Values of different types
		{Name: "mixed types", Code: `(call-with-values (lambda () (values 1 "hello" 'sym)) list)`, Expected: values.List(values.NewInteger(1), values.NewString("hello"), values.NewSymbol("sym"))},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
