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
	"testing"
	"unicode"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

// ── MakeTypePredicate ────────────────────────────────────────────────

func TestMakeTypePredicate(t *testing.T) {
	c := qt.New(t)

	isInteger := MakeTypePredicate(func(v values.Value) bool {
		_, ok := v.(*values.Integer)
		return ok
	})

	tcs := []struct {
		name string
		arg  values.Value
		want values.Value
	}{
		{"integer returns true", values.NewInteger(42), values.TrueValue},
		{"float returns false", values.NewFloat(3.14), values.FalseValue},
		{"string returns false", values.NewString("hello"), values.FalseValue},
		{"zero integer returns true", values.NewInteger(0), values.TrueValue},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := isInteger(mc)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

// ── MakeNumericPredicate ─────────────────────────────────────────────

func TestMakeNumericPredicate(t *testing.T) {
	c := qt.New(t)

	isExact := MakeNumericPredicate[values.Number](
		"exact?",
		werr.ErrNotANumber,
		func(n values.Number) bool {
			return n.IsExact()
		},
	)

	tcs := []struct {
		name string
		arg  values.Value
		want values.Value
	}{
		{"exact integer", values.NewInteger(42), values.TrueValue},
		{"inexact float", values.NewFloat(3.14), values.FalseValue},
		{"exact rational", values.NewRational(1, 2), values.TrueValue},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := isExact(mc)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestMakeNumericPredicate_Errors(t *testing.T) {
	c := qt.New(t)

	isExact := MakeNumericPredicate[values.Number](
		"exact?",
		werr.ErrNotANumber,
		func(n values.Number) bool {
			return n.IsExact()
		},
	)

	tcs := []struct {
		name string
		arg  values.Value
	}{
		{"string", values.NewString("hello")},
		{"boolean", values.TrueValue},
		{"character", values.NewCharacter('a')},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := isExact(mc)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotANumber), qt.IsTrue)
		})
	}
}

// ── MakeCharPredicate ────────────────────────────────────────────────

func TestMakeCharPredicate(t *testing.T) {
	c := qt.New(t)

	isUpper := MakeCharPredicate("char-upper-case?", unicode.IsUpper)

	tcs := []struct {
		name string
		arg  values.Value
		want values.Value
	}{
		{"uppercase", values.NewCharacter('A'), values.TrueValue},
		{"lowercase", values.NewCharacter('a'), values.FalseValue},
		{"digit", values.NewCharacter('5'), values.FalseValue},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := isUpper(mc)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestMakeCharPredicate_Errors(t *testing.T) {
	c := qt.New(t)

	isUpper := MakeCharPredicate("char-upper-case?", unicode.IsUpper)

	mc := makeMC(values.NewInteger(42))
	err := isUpper(mc)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotACharacter), qt.IsTrue)
}

// ── MakeCharTransform ────────────────────────────────────────────────

func TestMakeCharTransform(t *testing.T) {
	c := qt.New(t)

	toUpper := MakeCharTransform("char-upcase", unicode.ToUpper)

	tcs := []struct {
		name string
		arg  values.Value
		want values.Value
	}{
		{"lowercase to upper", values.NewCharacter('a'), values.NewCharacter('A')},
		{"already upper", values.NewCharacter('A'), values.NewCharacter('A')},
		{"digit unchanged", values.NewCharacter('5'), values.NewCharacter('5')},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg)
			err := toUpper(mc)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestMakeCharTransform_Errors(t *testing.T) {
	c := qt.New(t)

	toUpper := MakeCharTransform("char-upcase", unicode.ToUpper)

	mc := makeMC(values.NewString("not a char"))
	err := toUpper(mc)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrNotACharacter), qt.IsTrue)
}

// ── ChainEquality ────────────────────────────────────────────────────

func boolTypeCheck(v values.Value) error {
	if v == values.TrueValue || v == values.FalseValue {
		return nil
	}
	return werr.WrapForeignErrorf(werr.ErrNotANumber, "boolean=?: expected a boolean but got %T", v)
}

func boolEquals(a, b values.Value) bool {
	return a == b
}

func TestChainEquality(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
		want values.Value
	}{
		{
			"single arg true",
			values.TrueValue,
			values.EmptyList,
			values.TrueValue,
		},
		{
			"two equal booleans",
			values.TrueValue,
			values.List(values.TrueValue),
			values.TrueValue,
		},
		{
			"two unequal booleans",
			values.TrueValue,
			values.List(values.FalseValue),
			values.FalseValue,
		},
		{
			"three equal booleans",
			values.FalseValue,
			values.List(values.FalseValue, values.FalseValue),
			values.TrueValue,
		},
		{
			"three booleans last differs",
			values.TrueValue,
			values.List(values.TrueValue, values.FalseValue),
			values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := ChainEquality(mc, "boolean=?", boolTypeCheck, boolEquals)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestChainEquality_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
	}{
		{
			"first arg wrong type",
			values.NewInteger(1),
			values.EmptyList,
		},
		{
			"rest arg wrong type",
			values.TrueValue,
			values.List(values.NewInteger(1)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := ChainEquality(mc, "boolean=?", boolTypeCheck, boolEquals)
			c.Assert(err, qt.IsNotNil)
		})
	}
}
