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

package wile_test

import (
	"context"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

// evalOne evaluates code on a fresh engine and returns its written form.
func evalOne(t *testing.T, code string) string {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	v, err := eng.EvalMultiple(ctx, code)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("evaluating %s", code))
	return v.SchemeString()
}

// TestPeculiarIdentifiersAreDistinctBindings is the evaluation-level half of the
// reader gate in pkg/parser: a symbol minted from the wrong buffer offset is not
// merely misspelled, it collides. `+nabc` and `+nxyz` both read as the empty
// symbol, so binding them in one `let` failed with `duplicate binding name ""`,
// and `(let ((+node 5)) +nap)` returned 5 for a name that was never bound.
func TestPeculiarIdentifiersAreDistinctBindings(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{{
		name: "sign_dot_symbol_keeps_its_prefix",
		code: `(symbol->string '+.abc)`,
		want: `"+.abc"`,
	}, {
		name: "sign_dot_symbol_at_end_of_input",
		code: `(symbol->string '-.f)`,
		want: `"-.f"`,
	}, {
		name: "sign_dot_symbols_are_not_eq",
		code: `(eq? '-.f '+.g)`,
		want: `#f`,
	}, {
		name: "sign_dot_definitions_are_distinct",
		code: `(begin (define +.abc 1) (define -.abc 2) +.abc)`,
		want: `1`,
	}, {
		name: "nan_prefix_mismatch_keeps_its_prefix",
		code: `(symbol->string '+nabc)`,
		want: `"+nabc"`,
	}, {
		name: "nan_prefix_mismatches_are_not_eq",
		code: `(eq? '+nabc '+nxyz)`,
		want: `#f`,
	}, {
		name: "nan_prefix_mismatches_bind_separately",
		code: `(let ((+nabc 1) (+nxyz 2)) (list +nabc +nxyz))`,
		want: `(1 2)`,
	}, {
		name: "distinct_identifiers_do_not_alias",
		code: `(list '+node '+nap '+n)`,
		want: `(+node +nap +n)`,
	}, {
		name: "truncated_infinity_is_an_identifier",
		code: `(list '+inf '-inf)`,
		want: `(+inf -inf)`,
	}, {
		name: "truncated_nan_is_an_identifier",
		code: `(list '+nan '+nane '+nan_x)`,
		want: `(+nan +nane +nan_x)`,
	}, {
		name: "unit_imaginary_prefix_is_one_datum",
		code: `(list 'a '+i2 'b)`,
		want: `(a +i2 b)`,
	}, {
		name: "unit_imaginary_prefix_quotes_as_one_argument",
		code: `(list (quote +ifoo) (quote -ibar))`,
		want: `(+ifoo -ibar)`,
	}, {
		// Controls: the spellings R7RS does exempt stay numbers. -nan.0 writes
		// as +nan.0 — the printer does not carry a NaN's sign, unrelated here.
		name: "infnan_keywords_are_still_numbers",
		code: `(list +inf.0 -inf.0 +nan.0 -nan.0)`,
		want: `(+inf.0 -inf.0 +nan.0 +nan.0)`,
	}, {
		name: "unit_imaginaries_are_still_numbers",
		code: `(list +i -i)`,
		want: `(0+1i 0-1i)`,
	}}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			c.Check(evalOne(t, tc.code), qt.Equals, tc.want)
		})
	}
}

// TestPeculiarIdentifierDoesNotShadowAnUnboundName is the sharpest symptom of
// the aliasing: `+node` and `+nap` were the same (empty) symbol, so binding one
// bound the other and the unbound reference silently resolved.
func TestPeculiarIdentifierDoesNotShadowAnUnboundName(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx)
	qt.Assert(t, err, qt.IsNil)
	_, err = eng.EvalMultiple(ctx, `(let ((+node 5)) +nap)`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, `+nap`)
}
