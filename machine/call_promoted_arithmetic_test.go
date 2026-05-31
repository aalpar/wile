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

package machine_test

import (
	"testing"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestCallPromotedArithmetic(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		// Addition
		{Name: "add integers", Code: `(+ 1 2)`, Expected: values.NewInteger(3)},
		{Name: "add float and integer", Code: `(+ 1.5 2)`, Expected: values.NewFloat(3.5)},

		// Subtraction
		{Name: "subtract integers", Code: `(- 5 3)`, Expected: values.NewInteger(2)},
		{Name: "subtract negative result", Code: `(- 3 5)`, Expected: values.NewInteger(-2)},

		// Multiplication
		{Name: "multiply integers", Code: `(* 2 3)`, Expected: values.NewInteger(6)},
		{Name: "multiply by zero", Code: `(* 5 0)`, Expected: values.NewInteger(0)},

		// Division
		{Name: "divide exact", Code: `(/ 6 2)`, Expected: values.NewInteger(3)},
		{Name: "divide inexact", Code: `(/ 7.0 2.0)`, Expected: values.NewFloat(3.5)},

		// Comparisons: <
		{Name: "less than true", Code: `(< 1 2)`, Expected: values.TrueValue},
		{Name: "less than false", Code: `(< 2 1)`, Expected: values.FalseValue},
		{Name: "less than equal", Code: `(< 1 1)`, Expected: values.FalseValue},

		// Comparisons: >
		{Name: "greater than true", Code: `(> 2 1)`, Expected: values.TrueValue},
		{Name: "greater than false", Code: `(> 1 2)`, Expected: values.FalseValue},
		{Name: "greater than equal", Code: `(> 1 1)`, Expected: values.FalseValue},

		// Comparisons: =
		{Name: "numeric equal true", Code: `(= 1 1)`, Expected: values.TrueValue},
		{Name: "numeric equal false", Code: `(= 1 2)`, Expected: values.FalseValue},
		{Name: "numeric equal cross-type", Code: `(= 1 1.0)`, Expected: values.TrueValue},

		// Comparisons: <=
		{Name: "less or equal true equal", Code: `(<= 1 1)`, Expected: values.TrueValue},
		{Name: "less or equal true less", Code: `(<= 1 2)`, Expected: values.TrueValue},
		{Name: "less or equal false", Code: `(<= 2 1)`, Expected: values.FalseValue},

		// Comparisons: >=
		{Name: "greater or equal true equal", Code: `(>= 1 1)`, Expected: values.TrueValue},
		{Name: "greater or equal true greater", Code: `(>= 2 1)`, Expected: values.TrueValue},
		{Name: "greater or equal false", Code: `(>= 1 2)`, Expected: values.FalseValue},

		// Variadic fallback (not promoted — more than 2 args)
		{Name: "variadic add", Code: `(+ 1 2 3)`, Expected: values.NewInteger(6)},
		{Name: "variadic multiply", Code: `(* 2 3 4)`, Expected: values.NewInteger(24)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallPromotedArithmeticErrors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "divide by exact zero", Code: `(/ 1 0)`},
		{Name: "add non-number", Code: `(+ 1 "a")`},
		{Name: "subtract non-number", Code: `(- 1 "a")`},
		{Name: "multiply non-number", Code: `(* 1 "a")`},
		{Name: "divide non-number", Code: `(/ 1 "a")`},
		{Name: "less than non-number", Code: `(< 1 "a")`},
		{Name: "greater than non-number", Code: `(> 1 "a")`},

		// Non-real complex rejection — the guard popTwoReals adds on top of
		// popTwoNumbers. Each ordering comparison rejects a complex operand
		// with a non-zero imaginary part, in either operand position.
		{Name: "less than complex left", Code: `(< (make-rectangular 1 2) 3)`},
		{Name: "less than complex right", Code: `(< 3 (make-rectangular 1 2))`},
		{Name: "less or equal complex", Code: `(<= 1 (make-rectangular 0 1))`},
		{Name: "greater than complex right", Code: `(> 3 (make-rectangular 1 2))`},
		{Name: "greater or equal complex left", Code: `(>= (make-rectangular 1 2) 3)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestPopTwoRealsSemanticBoundary pins the property that justifies popTwoReals
// existing as a helper distinct from popTwoNumbers: ordering comparisons
// (<, <=, >, >=) require *real* arguments and reject a non-real complex with
// werr.ErrNotAReal, whereas numeric equality (=) permits complex arguments
// (R7RS §6.2.6). The "an error occurred" check in TestCallPromotedArithmeticErrors
// above is not enough — it would pass even if the wrong error were raised, or
// if = wrongly rejected complex too. This test asserts the *specific* error and
// the *exempt* case, which together are the whole reason the guard is selective.
func TestPopTwoRealsSemanticBoundary(t *testing.T) {
	// TODO(you): implement the two halves of the boundary.
	//
	//   Half 1 — ordering rejects non-real complex with the RIGHT error.
	//     For each of `(< (make-rectangular 1 2) 3)`, `(<= ...)`, `(> ...)`,
	//     `(>= ...)`: run it, then assert errors.Is(err, werr.ErrNotAReal) is
	//     true. (Plain IsNotNil is already covered above; the value here is
	//     proving it's ErrNotAReal specifically — verify against the sentinel,
	//     never compare error strings.)
	//
	//   Half 2 — equality is EXEMPT.
	//     `(= (make-rectangular 1 2) (make-rectangular 1 2))` must succeed:
	//     err == nil AND result is values.TrueValue. This is the case that
	//     proves the guard lives in popTwoReals and not in popTwoNumbers.
	//
	// Pick the table shape from registry/CLAUDE.md (a []struct with a `wantReal`
	// or `wantErr` discriminant reads cleanly here). The behaviors are already
	// confirmed at the language level; you are encoding them as a regression lock.
	t.Skip("TODO: implement popTwoReals semantic-boundary assertions")
}
