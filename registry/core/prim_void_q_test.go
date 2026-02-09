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

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestVoidQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "void? on void value",
			code: "(void? (if #f #t))",
			out:  values.TrueValue,
		},
		{
			name: "void? on define result",
			code: "(void? (define x 1))",
			out:  values.TrueValue,
		},
		{
			name: "void? on set! result",
			code: "(begin (define y 1) (void? (set! y 2)))",
			out:  values.TrueValue,
		},
		{
			name: "void? on integer",
			code: "(void? 42)",
			out:  values.FalseValue,
		},
		{
			name: "void? on string",
			code: `(void? "hello")`,
			out:  values.FalseValue,
		},
		{
			name: "void? on #f",
			code: "(void? #f)",
			out:  values.FalseValue,
		},
		{
			name: "void? on #t",
			code: "(void? #t)",
			out:  values.FalseValue,
		},
		{
			name: "void? on empty list",
			code: "(void? '())",
			out:  values.FalseValue,
		},
		{
			name: "void? on symbol",
			code: "(void? 'void)",
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
