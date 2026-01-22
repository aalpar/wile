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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// Port Predicate Edge Case Tests (R7RS §6.13.1)

func TestPortPredicatesWithEmptyList(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "port? with empty list",
			code: `(port? '())`,
			out:  values.FalseValue,
		},
		{
			name: "input-port? with empty list",
			code: `(input-port? '())`,
			out:  values.FalseValue,
		},
		{
			name: "output-port? with empty list",
			code: `(output-port? '())`,
			out:  values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestPortPredicatesWithVector(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "port? with vector",
			code: `(port? #(1 2 3))`,
			out:  values.FalseValue,
		},
		{
			name: "input-port? with vector",
			code: `(input-port? #(1 2 3))`,
			out:  values.FalseValue,
		},
		{
			name: "output-port? with vector",
			code: `(output-port? #(1 2 3))`,
			out:  values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
