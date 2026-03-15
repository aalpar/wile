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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

func TestPrimContinuationMarkSetQ(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "mark set", Code: `(continuation-mark-set? (current-continuation-marks))`, Expected: values.TrueValue},
		{Name: "integer", Code: `(continuation-mark-set? 42)`, Expected: values.FalseValue},
		{Name: "string", Code: `(continuation-mark-set? "hello")`, Expected: values.FalseValue},
		{Name: "list", Code: `(continuation-mark-set? '(1 2))`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestPrimContinuationMarkSetToList(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no marks",
			code:     `(continuation-mark-set->list (current-continuation-marks) 'k)`,
			expected: "()",
		},
		{
			name: "single mark",
			code: `(with-continuation-mark 'k 1
				(continuation-mark-set->list (current-continuation-marks) 'k))`,
			expected: "(1)",
		},
		{
			name: "tail replacement same key",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(continuation-mark-set->list (current-continuation-marks) 'k)))`,
			expected: "(2)",
		},
		{
			name: "different keys",
			code: `(with-continuation-mark 'a 1
				(with-continuation-mark 'b 2
					(continuation-mark-set->list (current-continuation-marks) 'a)))`,
			expected: "(1)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimContinuationMarkSetFirst(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no marks returns false",
			code:     `(continuation-mark-set-first (current-continuation-marks) 'k)`,
			expected: "#f",
		},
		{
			name:     "custom default",
			code:     `(continuation-mark-set-first (current-continuation-marks) 'k 'custom)`,
			expected: "custom",
		},
		{
			name: "returns value",
			code: `(with-continuation-mark 'k 42
				(continuation-mark-set-first (current-continuation-marks) 'k))`,
			expected: "42",
		},
		{
			name: "returns nearest",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(continuation-mark-set-first (current-continuation-marks) 'k)))`,
			expected: "2",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}

func TestPrimContMarkErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "->list wrong type", Code: `(continuation-mark-set->list 42 'k)`},
		{Name: "first wrong type", Code: `(continuation-mark-set-first 42 'k)`},
		{Name: "current-continuation-marks wrong tag", Code: `(current-continuation-marks 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

func TestPrimCallWithImmediateContMark(t *testing.T) {
	tcs := []struct {
		name     string
		code     string
		expected string
	}{
		{
			name:     "no mark uses default #f",
			code:     `(call-with-immediate-continuation-mark 'k (lambda (v) v))`,
			expected: "#f",
		},
		{
			name:     "no mark uses explicit default",
			code:     `(call-with-immediate-continuation-mark 'k (lambda (v) v) 'missing)`,
			expected: "missing",
		},
		{
			name: "reads mark from current frame",
			code: `(with-continuation-mark 'k 99
				(call-with-immediate-continuation-mark 'k (lambda (v) v)))`,
			expected: "99",
		},
		{
			name: "tail replacement — sees only innermost",
			code: `(with-continuation-mark 'k 1
				(with-continuation-mark 'k 2
					(call-with-immediate-continuation-mark 'k (lambda (v) v))))`,
			expected: "2",
		},
		{
			name: "different key returns default",
			code: `(with-continuation-mark 'k 1
				(call-with-immediate-continuation-mark 'other (lambda (v) v)))`,
			expected: "#f",
		},
		{
			name: "proc result is returned",
			code: `(call-with-immediate-continuation-mark 'k
				(lambda (v) (+ 1 2)))`,
			expected: "3",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result.SchemeString(), qt.Equals, tc.expected)
		})
	}
}
