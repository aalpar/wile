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

func TestCxR2Level(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// 2-level accessors
		{
			name:     "caar",
			code:     `(caar '((1 2) 3))`,
			expected: values.NewInteger(1),
		},
		{
			name:     "cadr",
			code:     `(cadr '(1 2 3))`,
			expected: values.NewInteger(2),
		},
		{
			name:     "cdar",
			code:     `(cdar '((1 2 3) 4))`,
			expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:     "cddr",
			code:     `(cddr '(1 2 3 4))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCxR3Level(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// 3-level accessors
		{
			name:     "caaar",
			code:     `(caaar '(((1 2) 3) 4))`,
			expected: values.NewInteger(1),
		},
		{
			name:     "caadr",
			code:     `(caadr '(1 (2 3) 4))`,
			expected: values.NewInteger(2),
		},
		{
			name:     "cadar",
			code:     `(cadar '((1 2 3) 4))`,
			expected: values.NewInteger(2),
		},
		{
			name:     "caddr",
			code:     `(caddr '(1 2 3 4))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "cdaar",
			code:     `(cdaar '(((1 2 3) 4) 5))`,
			expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:     "cdadr",
			code:     `(cdadr '(1 (2 3 4) 5))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name:     "cddar",
			code:     `(cddar '((1 2 3 4) 5))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name:     "cdddr",
			code:     `(cdddr '(1 2 3 4 5))`,
			expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCxR4Level(t *testing.T) {
	tests := []struct {
		name     string
		code     string
		expected values.Value
	}{
		// 4-level accessors
		{
			name:     "caaaar",
			code:     `(caaaar '((((1 2) 3) 4) 5))`,
			expected: values.NewInteger(1),
		},
		{
			name:     "caaadr",
			code:     `(caaadr '(1 ((2 3) 4) 5))`,
			expected: values.NewInteger(2),
		},
		{
			name:     "caadar",
			code:     `(caadar '(((1 2) (3 4) 5) 6))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "caaddr",
			code:     `(caaddr '(1 2 (3 4) 5))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "cadaar",
			code:     `(cadaar '(((1 2 3) 4) 5))`,
			expected: values.NewInteger(2),
		},
		{
			name:     "cadadr",
			code:     `(cadadr '(1 (2 3 4) 5))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "caddar",
			code:     `(caddar '((1 2 3 4) 5))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "cadddr",
			code:     `(cadddr '(1 2 3 4 5))`,
			expected: values.NewInteger(4),
		},
		{
			name:     "cdaaar",
			code:     `(cdaaar '((((1 2 3) 4) 5) 6))`,
			expected: values.List(values.NewInteger(2), values.NewInteger(3)),
		},
		{
			name:     "cdaadr",
			code:     `(cdaadr '(1 ((2 3 4) 5) 6))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name:     "cdadar",
			code:     `(cdadar '(((1 2) (3 4 5) 6) 7))`,
			expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			name:     "cdaddr",
			code:     `(cdaddr '(1 2 (3 4 5) 6))`,
			expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			name:     "cddaar",
			code:     `(cddaar '(((1 2 3 4) 5) 6))`,
			expected: values.List(values.NewInteger(3), values.NewInteger(4)),
		},
		{
			name:     "cddadr",
			code:     `(cddadr '(1 (2 3 4 5) 6))`,
			expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			name:     "cdddar",
			code:     `(cdddar '((1 2 3 4 5) 6))`,
			expected: values.List(values.NewInteger(4), values.NewInteger(5)),
		},
		{
			name:     "cddddr",
			code:     `(cddddr '(1 2 3 4 5 6))`,
			expected: values.List(values.NewInteger(5), values.NewInteger(6)),
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestCxRErrors(t *testing.T) {
	tests := []struct {
		name string
		code string
	}{
		{
			name: "caaar on non-pair",
			code: `(caaar 'not-a-pair)`,
		},
		{
			name: "caddr on too short list",
			code: `(caddr '(1))`,
		},
		{
			name: "caaaar on non-pair",
			code: `(caaaar '(((1))))`,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			_, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
