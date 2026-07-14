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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/registry/helpers"
	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestNumericEquality(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "unequal integers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "zero equals zero",
			prog: values.List(values.NewSymbol("="), values.NewInteger(0), values.NewInteger(0)),
			out:  values.TrueValue,
		},
		{
			name: "negative numbers",
			prog: values.List(values.NewSymbol("="), values.NewInteger(-5), values.NewInteger(-5)),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestEqQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eq? same symbol",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eq? different symbols",
			prog: values.List(values.NewSymbol("eq?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eq? same integers",
			prog: values.List(values.NewSymbol("eq?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "eq? booleans",
			prog: values.List(values.NewSymbol("eq?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestEqualQ(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "equal? same integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(5)),
			out:  values.TrueValue,
		},
		{
			name: "equal? different integers",
			prog: values.List(values.NewSymbol("equal?"), values.NewInteger(5), values.NewInteger(3)),
			out:  values.FalseValue,
		},
		{
			name: "equal? same booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "equal? different booleans",
			prog: values.List(values.NewSymbol("equal?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestEqQWithDifferentPairs(t *testing.T) {
	// Test eq? with two different pairs that have same contents
	// According to R7RS, eq? should return #f for different objects
	prog := values.List(values.NewSymbol("eq?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(3), values.NewInteger(4))))
	result, err := testhelpers.RunProgramAST(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// Two different pairs should not be eq?
	qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
}

func TestEqualQWithLists(t *testing.T) {
	// Test equal? with two equivalent lists
	prog := values.List(values.NewSymbol("equal?"),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
		values.List(values.NewSymbol("quote"),
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))))
	result, err := testhelpers.RunProgramAST(t, prog)
	qt.Assert(t, err, qt.IsNil)
	// equal? compares by value, so equivalent lists should be equal?
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestEqv(t *testing.T) {
	tcs := []struct {
		name   string
		a      values.Value
		b      values.Value
		expect bool
	}{
		// Same singleton objects
		{
			name:   "true vs true same object",
			a:      values.TrueValue,
			b:      values.TrueValue,
			expect: true,
		},
		{
			name:   "false vs false same object",
			a:      values.FalseValue,
			b:      values.FalseValue,
			expect: true,
		},
		{
			name:   "true vs false",
			a:      values.TrueValue,
			b:      values.FalseValue,
			expect: false,
		},
		{
			name:   "empty list vs empty list",
			a:      values.EmptyList,
			b:      values.EmptyList,
			expect: true,
		},
		// Integer comparisons (different objects with same value)
		{
			name:   "equal integers different objects",
			a:      values.NewInteger(42),
			b:      values.NewInteger(42),
			expect: true,
		},
		{
			name:   "unequal integers",
			a:      values.NewInteger(42),
			b:      values.NewInteger(43),
			expect: false,
		},
		{
			name:   "zero integers",
			a:      values.NewInteger(0),
			b:      values.NewInteger(0),
			expect: true,
		},
		{
			name:   "negative integers equal",
			a:      values.NewInteger(-5),
			b:      values.NewInteger(-5),
			expect: true,
		},
		// Float comparisons
		{
			name:   "equal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(3.14),
			expect: true,
		},
		{
			name:   "unequal floats",
			a:      values.NewFloat(3.14),
			b:      values.NewFloat(2.71),
			expect: false,
		},
		{
			name:   "zero floats",
			a:      values.NewFloat(0.0),
			b:      values.NewFloat(0.0),
			expect: true,
		},
		// Character comparisons
		{
			name:   "equal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('A'),
			expect: true,
		},
		{
			name:   "unequal characters",
			a:      values.NewCharacter('A'),
			b:      values.NewCharacter('B'),
			expect: false,
		},
		{
			name:   "unicode characters equal",
			a:      values.NewCharacter('λ'),
			b:      values.NewCharacter('λ'),
			expect: true,
		},
		// Cross-type comparisons (should always be false)
		{
			name:   "integer vs float",
			a:      values.NewInteger(42),
			b:      values.NewFloat(42.0),
			expect: false,
		},
		{
			name:   "integer vs string",
			a:      values.NewInteger(42),
			b:      values.NewString("42"),
			expect: false,
		},
		{
			name:   "symbol vs string",
			a:      values.NewSymbol("foo"),
			b:      values.NewString("foo"),
			expect: false,
		},
		// Pairs (different objects)
		{
			name:   "different pairs same contents",
			a:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			b:      values.NewCons(values.NewInteger(1), values.NewInteger(2)),
			expect: false,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := helpers.Eqv(tc.a, tc.b)
			qt.Assert(t, result, qt.Equals, tc.expect)
		})
	}
}

// TestEqvWithSamePointer tests eqv with the same pointer values
func TestEqvWithSamePointer(t *testing.T) {
	// When a and b are the same object, eqv should return true
	i := values.NewInteger(42)
	qt.Assert(t, helpers.Eqv(i, i), qt.IsTrue)

	f := values.NewFloat(3.14)
	qt.Assert(t, helpers.Eqv(f, f), qt.IsTrue)

	c := values.NewCharacter('X')
	qt.Assert(t, helpers.Eqv(c, c), qt.IsTrue)

	s := values.NewSymbol("foo")
	qt.Assert(t, helpers.Eqv(s, s), qt.IsTrue)

	str := values.NewString("hello")
	qt.Assert(t, helpers.Eqv(str, str), qt.IsTrue)

	pair := values.NewCons(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, helpers.Eqv(pair, pair), qt.IsTrue)
}

// TestEqvWithRational tests eqv with rational numbers
func TestEqvWithRational(t *testing.T) {
	// Same rational value
	r1 := values.NewRational(1, 2)
	r2 := values.NewRational(1, 2)
	qt.Assert(t, helpers.Eqv(r1, r2), qt.IsTrue)

	// Different rational values
	r3 := values.NewRational(1, 3)
	qt.Assert(t, helpers.Eqv(r1, r3), qt.IsFalse)

	// Equivalent rationals (reduced form)
	r4 := values.NewRational(2, 4) // Should reduce to 1/2
	qt.Assert(t, helpers.Eqv(r1, r4), qt.IsTrue)

	// Same object
	qt.Assert(t, helpers.Eqv(r1, r1), qt.IsTrue)
}

// TestEqvWithComplex tests eqv with complex numbers
func TestEqvWithComplex(t *testing.T) {
	// Same complex value
	c1 := values.NewComplex(complex(1, 2))
	c2 := values.NewComplex(complex(1, 2))
	qt.Assert(t, helpers.Eqv(c1, c2), qt.IsTrue)

	// Different complex values
	c3 := values.NewComplex(complex(1, 3))
	qt.Assert(t, helpers.Eqv(c1, c3), qt.IsFalse)

	// Same object
	qt.Assert(t, helpers.Eqv(c1, c1), qt.IsTrue)
}

// TestEqvQPrimitive tests the eqv? primitive through program execution
func TestEqvQPrimitive(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "eqv? same integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(42)),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different integers",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewInteger(43)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.TrueValue),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different booleans",
			prog: values.List(values.NewSymbol("eqv?"), values.TrueValue, values.FalseValue),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo"))),
			out: values.TrueValue,
		},
		{
			name: "eqv? different symbols",
			prog: values.List(values.NewSymbol("eqv?"),
				values.List(values.NewSymbol("quote"), values.NewSymbol("foo")),
				values.List(values.NewSymbol("quote"), values.NewSymbol("bar"))),
			out: values.FalseValue,
		},
		{
			name: "eqv? integer vs float (different types)",
			prog: values.List(values.NewSymbol("eqv?"), values.NewInteger(42), values.NewFloat(42.0)),
			out:  values.FalseValue,
		},
		{
			name: "eqv? same characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('a')),
			out:  values.TrueValue,
		},
		{
			name: "eqv? different characters",
			prog: values.List(values.NewSymbol("eqv?"), values.NewCharacter('a'), values.NewCharacter('b')),
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestEquivalenceLattice pins R7RS §6.1's ordering of the three equivalence
// predicates by coarseness: eq? ⊆ eqv? ⊆ equal?. Each must answer #t wherever
// the finer one does; equal? may be coarser (it compares string and list
// contents), but it may never be FINER.
//
// equal? fell below eqv? on NaN. eqv? opens with an identity check, so
// (eqv? x x) was #t, while equal? went straight to Float.EqualTo, which compares
// values — and IEEE-754 says NaN != NaN. The lattice inverted, and the damage was
// not academic: (member x lst) could not find the very object it was handed,
// because member is equal?-based while memv is eqv?-based.
//
// Written as a law over representative values rather than a NaN special case: any
// future leaf type whose EqualTo is non-reflexive breaks the same way, and this
// table catches it without anyone remembering to think about NaN.
func TestEquivalenceLattice(t *testing.T) {
	tcs := []struct {
		name  string
		value string // expression bound to x, then compared with itself
	}{
		{name: "nan", value: `+nan.0`},
		{name: "nan in list", value: `(list +nan.0)`},
		{name: "nan in vector", value: `(vector +nan.0)`},
		{name: "nan nested deeply", value: `(list (vector (list +nan.0)))`},
		{name: "integer", value: `42`},
		{name: "float", value: `1.5`},
		{name: "symbol", value: `(quote sym)`},
		{name: "string", value: `"s"`},
		{name: "char", value: `#\a`},
		{name: "empty list", value: `(quote ())`},
		{name: "pair", value: `(cons 1 2)`},
		{name: "vector", value: `(vector 1 2)`},
		{name: "infinity", value: `+inf.0`},
		{name: "negative zero", value: `-0.0`},
		{name: "boolean", value: `#t`},
	}

	// eq? => eqv? => equal?, asserted as implication rather than equality: a #f on
	// the finer predicate constrains nothing, so the law — not a fixed expected
	// value — is what each case checks.
	const law = `(let* ((x %s)
	                    (a (eq? x x))
	                    (b (eqv? x x))
	                    (c (equal? x x)))
	               (and (if a b #t) (if b c #t)))`

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, fmt.Sprintf(law, tc.value))
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue,
				qt.Commentf("eq? \u2286 eqv? \u2286 equal? violated for %s", tc.name))
		})
	}
}

// TestEqualIsReflexive pins the consequence that bit the equal?-based list
// searches: an object must be found in a list that contains it.
func TestEqualIsReflexive(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "equal? nan self", Code: `(let ((x +nan.0)) (equal? x x))`, Expected: values.TrueValue},
		{Name: "member finds nan", Code: `(let ((x +nan.0)) (if (member x (list 1 x 2)) #t #f))`, Expected: values.TrueValue},
		{Name: "assoc finds nan key", Code: `(let ((x +nan.0)) (if (assoc x (list (cons x 1))) #t #f))`, Expected: values.TrueValue},
		{Name: "nested nan agrees with leaf", Code: `(let ((x +nan.0)) (eq? (equal? x x) (equal? (list x) (list x))))`, Expected: values.TrueValue},

		// Distinct NaN objects stay #f: eqv? says #f for them, and equal? on
		// non-compound values is defined to agree with eqv?. Only reflexivity moved.
		{Name: "distinct nan literals stay false", Code: `(equal? +nan.0 +nan.0)`, Expected: values.FalseValue},

		// equal? stays COARSER than eqv? where R7RS requires it to be.
		{Name: "equal? still compares string contents", Code: `(equal? (string-copy "ab") (string-copy "ab"))`, Expected: values.TrueValue},
		{Name: "eqv? still distinguishes those strings", Code: `(eqv? (string-copy "ab") (string-copy "ab"))`, Expected: values.FalseValue},
		{Name: "equal? still compares list contents", Code: `(equal? (list 1 2) (list 1 2))`, Expected: values.TrueValue},

		// Numeric = is a DIFFERENT predicate and keeps IEEE-754 (R7RS §6.2.6): NaN is
		// = to nothing, ITSELF INCLUDED. Reflexivity is a law of equivalence
		// relations, not of =. Conflating the two is what produced the bug — the old
		// EqualTo applied "NaN != NaN" to equal?, where it does not belong.
		{Name: "= stays IEEE on same object", Code: `(let ((x +nan.0)) (= x x))`, Expected: values.FalseValue},
		{Name: "= stays IEEE on distinct nans", Code: `(= +nan.0 +nan.0)`, Expected: values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}
