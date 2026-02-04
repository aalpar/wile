// Copyright 2025 Aaron Alpar
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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// values Tests (R7RS §6.4 - Return multiple values)

func TestValuesComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Single value (direct return)
		{name: "single value", code: `(values 42)`, expected: values.NewInteger(42)},

		// Multiple values with call-with-values to capture
		{name: "two values via cwv", code: `(call-with-values (lambda () (values 1 2)) +)`, expected: values.NewInteger(3)},
		{name: "three values via cwv", code: `(call-with-values (lambda () (values 'a 'b 'c)) list)`, expected: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))},

		// Zero values
		{name: "zero values via cwv", code: `(call-with-values (lambda () (values)) (lambda () 'empty))`, expected: values.NewSymbol("empty")},

		// Values of different types
		{name: "mixed types", code: `(call-with-values (lambda () (values 1 "hello" 'sym)) list)`, expected: values.List(values.NewInteger(1), values.NewString("hello"), values.NewSymbol("sym"))},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
