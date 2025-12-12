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

func TestEval(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "eval simple arithmetic",
			code: "(eval '(+ 1 2) (interaction-environment))",
			out:  values.NewInteger(3),
		},
		{
			name: "eval multiplication",
			code: "(eval '(* 3 4) (interaction-environment))",
			out:  values.NewInteger(12),
		},
		{
			name: "eval quoted symbol",
			code: "(eval ''hello (interaction-environment))",
			out:  values.NewSymbol("hello"),
		},
		{
			name: "eval list constructor",
			code: "(eval '(list 1 2 3) (interaction-environment))",
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
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

func TestInteractionEnvironment(t *testing.T) {
	t.Run("returns environment", func(t *testing.T) {
		result, err := runSchemeCode(t, "(interaction-environment)")
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, result, qt.IsNotNil)
	})
}
