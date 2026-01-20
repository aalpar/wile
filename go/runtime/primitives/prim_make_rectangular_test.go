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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestMakeRectangularComprehensive(t *testing.T) {
	tcs := []schemeCodeTestCase{
		// Integer args
		{name: "make-rectangular integers", code: `(make-rectangular 3 4)`,
			expected: values.NewComplexFromParts(3.0, 4.0)},
		{name: "make-rectangular negative integers", code: `(make-rectangular -3 -4)`,
			expected: values.NewComplexFromParts(-3.0, -4.0)},
		{name: "make-rectangular zero imaginary", code: `(make-rectangular 5 0)`,
			expected: values.NewComplexFromParts(5.0, 0.0)},
		{name: "make-rectangular zero real", code: `(make-rectangular 0 5)`,
			expected: values.NewComplexFromParts(0.0, 5.0)},

		// Float args
		{name: "make-rectangular floats", code: `(make-rectangular 3.0 4.0)`,
			expected: values.NewComplexFromParts(3.0, 4.0)},
		{name: "make-rectangular mixed int float", code: `(make-rectangular 3 4.0)`,
			expected: values.NewComplexFromParts(3.0, 4.0)},

		// Rational args
		{name: "make-rectangular rationals", code: `(make-rectangular 1/2 3/4)`,
			expected: values.NewComplexFromParts(0.5, 0.75)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestMakeRectangularErrors(t *testing.T) {
	tcs := []schemeCodeErrorTestCase{
		{name: "make-rectangular string arg", code: `(make-rectangular "3" 4)`},
		{name: "make-rectangular symbol arg", code: `(make-rectangular 3 'four)`},
		{name: "make-rectangular complex arg", code: `(make-rectangular 1+2i 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
