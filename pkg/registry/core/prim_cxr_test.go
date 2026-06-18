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

func TestCxR2Level(t *testing.T) {
	tests := []testhelpers.SchemeCodeTestCase{
		// 2-level accessors
		{
			Name:     "caar",
			Code:     `(caar '((1 2) 3))`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "cadr",
			Code:     `(cadr '(1 2 3))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "cdar",
			Code:     `(cdar '((1 2 3) 4))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "cddr",
			Code:     `(cddr '(1 2 3 4))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCxR3Level(t *testing.T) {
	tests := []testhelpers.SchemeCodeTestCase{
		// 3-level accessors
		{
			Name:     "caaar",
			Code:     `(caaar '(((1 2) 3) 4))`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "caadr",
			Code:     `(caadr '(1 (2 3) 4))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "cadar",
			Code:     `(cadar '((1 2 3) 4))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "caddr",
			Code:     `(caddr '(1 2 3 4))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "cdaar",
			Code:     `(cdaar '(((1 2 3) 4) 5))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "cdadr",
			Code:     `(cdadr '(1 (2 3 4) 5))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			Name:     "cddar",
			Code:     `(cddar '((1 2 3 4) 5))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			Name:     "cdddr",
			Code:     `(cdddr '(1 2 3 4 5))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCxR4Level(t *testing.T) {
	tests := []testhelpers.SchemeCodeTestCase{
		// 4-level accessors
		{
			Name:     "caaaar",
			Code:     `(caaaar '((((1 2) 3) 4) 5))`,
			Expected: values.NewInteger(1),
		},
		{
			Name:     "caaadr",
			Code:     `(caaadr '(1 ((2 3) 4) 5))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "caadar",
			Code:     `(caadar '(((1 2) (3 4) 5) 6))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "caaddr",
			Code:     `(caaddr '(1 2 (3 4) 5))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "cadaar",
			Code:     `(cadaar '(((1 2 3) 4) 5))`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "cadadr",
			Code:     `(cadadr '(1 (2 3 4) 5))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "caddar",
			Code:     `(caddar '((1 2 3 4) 5))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "cadddr",
			Code:     `(cadddr '(1 2 3 4 5))`,
			Expected: values.NewInteger(4),
		},
		{
			Name:     "cdaaar",
			Code:     `(cdaaar '((((1 2 3) 4) 5) 6))`,
			Expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			Name:     "cdaadr",
			Code:     `(cdaadr '(1 ((2 3 4) 5) 6))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			Name:     "cdadar",
			Code:     `(cdadar '(((1 2) (3 4 5) 6) 7))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			Name:     "cdaddr",
			Code:     `(cdaddr '(1 2 (3 4 5) 6))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			Name:     "cddaar",
			Code:     `(cddaar '(((1 2 3 4) 5) 6))`,
			Expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			Name:     "cddadr",
			Code:     `(cddadr '(1 (2 3 4 5) 6))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			Name:     "cdddar",
			Code:     `(cdddar '((1 2 3 4 5) 6))`,
			Expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			Name:     "cddddr",
			Code:     `(cddddr '(1 2 3 4 5 6))`,
			Expected: values.List(values.NewInteger(5), values.NewInteger(6)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCxRErrors(t *testing.T) {
	tests := []testhelpers.SchemeCodeErrorTestCase{
		// 2-level errors
		{Name: "caar on non-pair", Code: `(caar 42)`},
		{Name: "caar on empty list", Code: `(caar '())`},
		{Name: "caar car is not pair", Code: `(caar '(1 2))`},
		{Name: "cadr on empty list", Code: `(cadr '())`},
		{Name: "cadr on single element list", Code: `(cadr '(1))`},
		{Name: "cdar on empty list", Code: `(cdar '())`},
		{Name: "cdar car is not pair", Code: `(cdar '(1))`},
		{Name: "cddr on empty list", Code: `(cddr '())`},
		{Name: "cddr on single element list", Code: `(cddr '(1))`},

		// 3-level errors
		{Name: "caaar on non-pair", Code: `(caaar 'not-a-pair)`},
		{Name: "caddr on too short list", Code: `(caddr '(1))`},
		{Name: "caaar inner too shallow", Code: `(caaar '((1)))`},

		// 4-level errors
		{Name: "caaaar on non-pair", Code: `(caaaar '(((1))))`},
		{Name: "cadddr on too short list", Code: `(cadddr '(1 2))`},
	}

	for _, tc := range tests {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
