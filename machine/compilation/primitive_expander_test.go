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

package compilation

import (
	"testing"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestPrimitiveExpander(t *testing.T) {
	// stubFn is a no-op expander used to test construction and value interface.
	stubFn := func(
		etc *ExpanderTimeContinuation,
		sym *syntax.SyntaxSymbol,
		expr syntax.SyntaxValue,
	) (syntax.SyntaxValue, error) {
		return expr, nil
	}

	tcs := []struct {
		name    string
		peName  string
		checkFn func(t *testing.T, pe *PrimitiveExpander)
	}{
		{
			name:   "Name returns constructor name",
			peName: "if",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.Name(), qt.Equals, "if")
			},
		},
		{
			name:   "SchemeString format",
			peName: "lambda",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.SchemeString(), qt.Equals, "#<primitive-expander:lambda>")
			},
		},
		{
			name:   "IsVoid returns false",
			peName: "quote",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.IsVoid(), qt.IsFalse)
			},
		},
		{
			name:   "fn field is set",
			peName: "begin",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.fn, qt.IsNotNil)
			},
		},
		{
			name:   "EqualTo same name",
			peName: "define",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				other := NewPrimitiveExpander("define", stubFn)
				qt.Assert(t, pe.EqualTo(other), qt.IsTrue)
			},
		},
		{
			name:   "EqualTo different name",
			peName: "define",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				other := NewPrimitiveExpander("set!", stubFn)
				qt.Assert(t, pe.EqualTo(other), qt.IsFalse)
			},
		},
		{
			name:   "EqualTo nil returns false",
			peName: "if",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.EqualTo(nil), qt.IsFalse)
			},
		},
		{
			name:   "EqualTo wrong type returns false",
			peName: "if",
			checkFn: func(t *testing.T, pe *PrimitiveExpander) {
				qt.Assert(t, pe.EqualTo(values.NewInteger(1)), qt.IsFalse)
			},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			pe := NewPrimitiveExpander(tc.peName, stubFn)
			tc.checkFn(t, pe)
		})
	}
}
