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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"
)

func charLT(a, b rune) bool {
	return a < b
}

func charEQ(a, b rune) bool {
	return a == b
}

// ── CharCompare ──────────────────────────────────────────────────────

func TestCharCompare(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    values.Value
		b    values.Value
		cmp  func(a, b rune) bool
		want values.Value
	}{
		{
			"a < b true",
			values.NewCharacter('a'),
			values.NewCharacter('b'),
			charLT,
			values.TrueValue,
		},
		{
			"b < a false",
			values.NewCharacter('b'),
			values.NewCharacter('a'),
			charLT,
			values.FalseValue,
		},
		{
			"same char equal",
			values.NewCharacter('x'),
			values.NewCharacter('x'),
			charEQ,
			values.TrueValue,
		},
		{
			"different chars not equal",
			values.NewCharacter('x'),
			values.NewCharacter('y'),
			charEQ,
			values.FalseValue,
		},
		{
			"unicode comparison",
			values.NewCharacter('\u03B1'), // alpha
			values.NewCharacter('\u03B2'), // beta
			charLT,
			values.TrueValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.a, tc.b)
			err := CharCompare(mc, "test", tc.cmp)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestCharCompare_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		a    values.Value
		b    values.Value
	}{
		{
			"first arg not a character",
			values.NewInteger(65),
			values.NewCharacter('A'),
		},
		{
			"second arg not a character",
			values.NewCharacter('A'),
			values.NewString("A"),
		},
		{
			"both args not characters",
			values.NewInteger(1),
			values.NewInteger(2),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.a, tc.b)
			err := CharCompare(mc, "test", charLT)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotACharacter), qt.IsTrue)
		})
	}
}

// ── CharCompareVariadic ──────────────────────────────────────────────

func TestCharCompareVariadic(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
		cmp  func(a, b rune) bool
		want values.Value
	}{
		{
			"single char always true",
			values.NewCharacter('a'),
			values.EmptyList,
			charLT,
			values.TrueValue,
		},
		{
			"two chars ascending",
			values.NewCharacter('a'),
			values.List(values.NewCharacter('b')),
			charLT,
			values.TrueValue,
		},
		{
			"two chars not ascending",
			values.NewCharacter('b'),
			values.List(values.NewCharacter('a')),
			charLT,
			values.FalseValue,
		},
		{
			"three chars ascending",
			values.NewCharacter('a'),
			values.List(values.NewCharacter('b'), values.NewCharacter('c')),
			charLT,
			values.TrueValue,
		},
		{
			"three chars equal",
			values.NewCharacter('x'),
			values.List(values.NewCharacter('x'), values.NewCharacter('x')),
			charEQ,
			values.TrueValue,
		},
		{
			"three chars not all equal",
			values.NewCharacter('x'),
			values.List(values.NewCharacter('x'), values.NewCharacter('y')),
			charEQ,
			values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := CharCompareVariadic(mc, "test", tc.cmp)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestCharCompareVariadic_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
	}{
		{
			"first arg not a character",
			values.NewInteger(65),
			values.List(values.NewCharacter('A')),
		},
		{
			"non-character in rest",
			values.NewCharacter('A'),
			values.List(values.NewInteger(66)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := CharCompareVariadic(mc, "test", charLT)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotACharacter), qt.IsTrue)
		})
	}
}
