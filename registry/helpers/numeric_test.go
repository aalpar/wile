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

package helpers

import (
	"errors"
	"math"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

// ── binOps used by tests ─────────────────────────────────────────────

func addOp(a, b values.Number) (values.Number, error) {
	return a.Add(b), nil
}

func mulOp(a, b values.Number) (values.Number, error) {
	return a.Multiply(b), nil
}

func subOp(a, b values.Number) (values.Number, error) {
	return a.Subtract(b), nil
}

func negOp(v values.Number) (values.Number, error) {
	return v.Negate(), nil
}

func divOp(a, b values.Number) (values.Number, error) {
	return a.Divide(b)
}

func recipOp(v values.Number) (values.Number, error) {
	return values.NewInteger(1).Divide(v)
}

// ── NumericFoldVariadic ──────────────────────────────────────────────

func TestNumericFoldVariadic(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		args     values.Value
		identity values.Number
		binOp    func(a, b values.Number) (values.Number, error)
		want     values.Value
	}{
		// Addition identity
		{
			"add empty list returns 0",
			values.EmptyList,
			values.NewInteger(0),
			addOp,
			values.NewInteger(0),
		},
		// Single element
		{
			"add single integer",
			values.List(values.NewInteger(5)),
			values.NewInteger(0),
			addOp,
			values.NewInteger(5),
		},
		// Two elements
		{
			"add two integers",
			values.List(values.NewInteger(3), values.NewInteger(4)),
			values.NewInteger(0),
			addOp,
			values.NewInteger(7),
		},
		// Three elements
		{
			"add three integers",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
			values.NewInteger(0),
			addOp,
			values.NewInteger(6),
		},
		// Multiply identity
		{
			"mul empty list returns 1",
			values.EmptyList,
			values.NewInteger(1),
			mulOp,
			values.NewInteger(1),
		},
		// Multiply single
		{
			"mul single integer",
			values.List(values.NewInteger(7)),
			values.NewInteger(1),
			mulOp,
			values.NewInteger(7),
		},
		// Multiply two
		{
			"mul two integers",
			values.List(values.NewInteger(3), values.NewInteger(4)),
			values.NewInteger(1),
			mulOp,
			values.NewInteger(12),
		},
		// Float
		{
			"add integer and float",
			values.List(values.NewInteger(1), values.NewFloat(2.5)),
			values.NewInteger(0),
			addOp,
			values.NewFloat(3.5),
		},
		// Negative
		{
			"add negative integers",
			values.List(values.NewInteger(-3), values.NewInteger(-4)),
			values.NewInteger(0),
			addOp,
			values.NewInteger(-7),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := NumericFoldVariadic(mc, "test", tc.identity, tc.binOp)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestNumericFoldVariadic_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		args     values.Value
		sentinel error
	}{
		{
			"non-list arg",
			values.NewInteger(42),
			werr.ErrNotAList,
		},
		{
			"non-number first element",
			values.List(values.NewString("bad")),
			werr.ErrNotANumber,
		},
		{
			"non-number in rest",
			values.List(values.NewInteger(1), values.NewString("bad")),
			werr.ErrNotANumber,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.args)
			err := NumericFoldVariadic(mc, "test", values.NewInteger(0), addOp)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}

// ── NumericFoldWithFirst ─────────────────────────────────────────────

func TestNumericFoldWithFirst(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name    string
		arg0    values.Value // first arg
		arg1    values.Value // rest list
		unaryOp func(values.Number) (values.Number, error)
		binOp   func(a, b values.Number) (values.Number, error)
		want    values.Value
	}{
		// Unary negate: (- 5) => -5
		{
			"unary negate",
			values.NewInteger(5),
			values.EmptyList,
			negOp,
			subOp,
			values.NewInteger(-5),
		},
		// Binary subtraction: (- 10 3) => 7
		{
			"binary subtract",
			values.NewInteger(10),
			values.List(values.NewInteger(3)),
			negOp,
			subOp,
			values.NewInteger(7),
		},
		// Ternary subtraction: (- 10 3 2) => 5
		{
			"ternary subtract",
			values.NewInteger(10),
			values.List(values.NewInteger(3), values.NewInteger(2)),
			negOp,
			subOp,
			values.NewInteger(5),
		},
		// Unary reciprocal: (/ 4) => 1/4
		{
			"unary reciprocal",
			values.NewInteger(4),
			values.EmptyList,
			recipOp,
			divOp,
			values.NewRational(1, 4),
		},
		// Binary divide: (/ 12 3) => 4
		{
			"binary divide integers",
			values.NewInteger(12),
			values.List(values.NewInteger(3)),
			recipOp,
			divOp,
			values.NewInteger(4),
		},
		// Mixed types
		{
			"subtract float from integer",
			values.NewInteger(10),
			values.List(values.NewFloat(2.5)),
			negOp,
			subOp,
			values.NewFloat(7.5),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericFoldWithFirst(mc, "test", tc.unaryOp, tc.binOp)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestNumericFoldWithFirst_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		sentinel error
	}{
		{
			"first arg not a number",
			values.NewString("bad"),
			values.EmptyList,
			werr.ErrNotANumber,
		},
		{
			"rest not a list",
			values.NewInteger(1),
			values.NewInteger(2),
			werr.ErrNotAList,
		},
		{
			"non-number in rest car",
			values.NewInteger(1),
			values.List(values.NewString("bad")),
			werr.ErrNotANumber,
		},
		{
			"non-number in rest tail",
			values.NewInteger(10),
			values.List(values.NewInteger(3), values.NewString("bad")),
			werr.ErrNotANumber,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericFoldWithFirst(mc, "test", negOp, subOp)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}

// ── NumericChainCompare ──────────────────────────────────────────────

func lessThanFails(prev, curr values.Number) bool {
	return !prev.LessThan(curr)
}

func equalFails(prev, curr values.Number) bool {
	return prev.Compare(curr) != 0
}

func TestNumericChainCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		arg0  values.Value
		arg1  values.Value
		fails func(prev, curr values.Number) bool
		want  values.Value
	}{
		// Single arg is always true
		{
			"single arg < is true",
			values.NewInteger(42),
			values.EmptyList,
			lessThanFails,
			values.TrueValue,
		},
		// Ascending: (< 1 2 3)
		{
			"ascending < is true",
			values.NewInteger(1),
			values.List(values.NewInteger(2), values.NewInteger(3)),
			lessThanFails,
			values.TrueValue,
		},
		// Not ascending: (< 1 3 2)
		{
			"non-ascending < is false",
			values.NewInteger(1),
			values.List(values.NewInteger(3), values.NewInteger(2)),
			lessThanFails,
			values.FalseValue,
		},
		// Equal: (= 5 5 5)
		{
			"all equal = is true",
			values.NewInteger(5),
			values.List(values.NewInteger(5), values.NewInteger(5)),
			equalFails,
			values.TrueValue,
		},
		// Not equal: (= 5 5 6)
		{
			"not all equal = is false",
			values.NewInteger(5),
			values.List(values.NewInteger(5), values.NewInteger(6)),
			equalFails,
			values.FalseValue,
		},
		// Mixed types: (< 1 2.5)
		{
			"mixed types < true",
			values.NewInteger(1),
			values.List(values.NewFloat(2.5)),
			lessThanFails,
			values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericChainCompare(mc, "test", tc.fails)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestNumericChainCompare_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		sentinel error
	}{
		{
			"first arg not a number",
			values.NewString("bad"),
			values.EmptyList,
			werr.ErrNotANumber,
		},
		{
			"rest not a list",
			values.NewInteger(1),
			values.NewInteger(2),
			werr.ErrNotAList,
		},
		{
			"non-number in rest",
			values.NewInteger(1),
			values.List(values.NewString("bad")),
			werr.ErrNotANumber,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericChainCompare(mc, "test", lessThanFails)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}

// ── NumericChainCompareReal ──────────────────────────────────────────

func TestNumericChainCompareReal(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name  string
		arg0  values.Value
		arg1  values.Value
		fails func(prev, curr values.Number) bool
		want  values.Value
	}{
		{
			"real numbers ascending",
			values.NewInteger(1),
			values.List(values.NewInteger(2)),
			lessThanFails,
			values.TrueValue,
		},
		{
			"real numbers not ascending",
			values.NewInteger(2),
			values.List(values.NewInteger(1)),
			lessThanFails,
			values.FalseValue,
		},
		{
			"single real arg",
			values.NewFloat(3.14),
			values.EmptyList,
			lessThanFails,
			values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericChainCompareReal(mc, "test", tc.fails)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestNumericChainCompareReal_ComplexRejection(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
	}{
		{
			"complex first arg",
			values.NewComplex(complex(1, 2)),
			values.List(values.NewInteger(3)),
		},
		{
			"complex in rest",
			values.NewInteger(1),
			values.List(values.NewComplex(complex(2, 3))),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericChainCompareReal(mc, "test", lessThanFails)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotAReal), qt.IsTrue)
		})
	}
}

// ── NumericExtremum ──────────────────────────────────────────────────

func isBetterMin(candidate, current values.Number) bool {
	return candidate.LessThan(current)
}

func isBetterMax(candidate, current values.Number) bool {
	return current.LessThan(candidate)
}

func TestNumericExtremum(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		isBetter func(candidate, current values.Number) bool
		want     values.Value
	}{
		// Single arg
		{
			"min single exact",
			values.NewInteger(42),
			values.EmptyList,
			isBetterMin,
			values.NewInteger(42),
		},
		// Two args min
		{
			"min of 3 and 5",
			values.NewInteger(3),
			values.List(values.NewInteger(5)),
			isBetterMin,
			values.NewInteger(3),
		},
		// Two args max
		{
			"max of 3 and 5",
			values.NewInteger(3),
			values.List(values.NewInteger(5)),
			isBetterMax,
			values.NewInteger(5),
		},
		// Three args
		{
			"min of 7 2 9",
			values.NewInteger(7),
			values.List(values.NewInteger(2), values.NewInteger(9)),
			isBetterMin,
			values.NewInteger(2),
		},
		// Exactness contagion: exact min but inexact arg -> inexact result
		{
			"min exact with inexact makes inexact",
			values.NewInteger(1),
			values.List(values.NewFloat(5.0)),
			isBetterMin,
			values.NewFloat(1.0),
		},
		// Single inexact arg
		{
			"min single inexact",
			values.NewFloat(3.14),
			values.EmptyList,
			isBetterMin,
			values.NewFloat(3.14),
		},
		// Negative values
		{
			"min of -3 and -7",
			values.NewInteger(-3),
			values.List(values.NewInteger(-7)),
			isBetterMin,
			values.NewInteger(-7),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericExtremum(mc, "test", tc.isBetter)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestNumericExtremum_NaN(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
	}{
		{
			"NaN first arg",
			values.NewFloat(math.NaN()),
			values.EmptyList,
		},
		{
			"NaN in rest",
			values.NewInteger(1),
			values.List(values.NewFloat(math.NaN())),
		},
		{
			"NaN first with rest",
			values.NewFloat(math.NaN()),
			values.List(values.NewInteger(5)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericExtremum(mc, "test", isBetterMin)
			c.Assert(err, qt.IsNil)
			f, ok := mc.GetValue().(*values.Float)
			c.Assert(ok, qt.IsTrue, qt.Commentf("expected Float, got %T", mc.GetValue()))
			c.Assert(math.IsNaN(f.Value), qt.IsTrue)
		})
	}
}

func TestNumericExtremum_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name     string
		arg0     values.Value
		arg1     values.Value
		sentinel error
	}{
		{
			"first arg not a number",
			values.NewString("bad"),
			values.EmptyList,
			werr.ErrNotANumber,
		},
		{
			"rest not a list",
			values.NewInteger(1),
			values.NewInteger(2),
			werr.ErrNotAList,
		},
		{
			"non-number in rest",
			values.NewInteger(1),
			values.List(values.NewString("bad")),
			werr.ErrNotANumber,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := NumericExtremum(mc, "test", isBetterMin)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, tc.sentinel), qt.IsTrue,
				qt.Commentf("expected %v, got %v", tc.sentinel, err))
		})
	}
}

// ── MaybeToInexact ───────────────────────────────────────────────────

func TestMaybeToInexact(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name       string
		n          values.Number
		hasInexact bool
		want       values.Value
	}{
		{
			"exact with no inexact flag unchanged",
			values.NewInteger(42),
			false,
			values.NewInteger(42),
		},
		{
			"exact with inexact flag converts",
			values.NewInteger(42),
			true,
			values.NewFloat(42.0),
		},
		{
			"inexact with inexact flag unchanged",
			values.NewFloat(3.14),
			true,
			values.NewFloat(3.14),
		},
		{
			"inexact with no inexact flag unchanged",
			values.NewFloat(3.14),
			false,
			values.NewFloat(3.14),
		},
		{
			"rational with inexact flag converts",
			values.NewRational(1, 2),
			true,
			values.NewFloat(0.5),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := MaybeToInexact(tc.n, tc.hasInexact)
			c.Assert(result, valuestest.SchemeEquals, tc.want)
		})
	}
}
