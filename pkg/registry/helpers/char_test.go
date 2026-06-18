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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

func charLT(a, b rune) bool {
	return a < b
}

func charEQ(a, b rune) bool {
	return a == b
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

// TestCompareVariadic_ErrorMessageContainsTypeName pins the rest-loop
// error format in CompareVariadic. The helper has its own format string
// independent of RequireArg/RequireType; the type phrase must surface
// from the sentinel correctly.
func TestCompareVariadic_ErrorMessageContainsTypeName(t *testing.T) {
	c := qt.New(t)
	// Two chars then an integer — the integer is the type-mismatch.
	mc := makeMC(
		values.NewCharacter('a'),
		values.List(values.NewCharacter('b'), values.NewInteger(99)),
	)
	err := CharCompareVariadic(mc, "test-prim", charLT)
	c.Assert(err, qt.IsNotNil)
	msg := err.Error()
	c.Assert(
		msg,
		qt.Matches,
		`.*expected a character but got.*`,
		qt.Commentf("CompareVariadic should surface TypeName phrase: %q", msg),
	)
}

// TestCharCompareVariadic_TypeErrorAfterCmpFail pins CP1's behavioral
// invariant: when a comparison fails before reaching an ill-typed element,
// the helper short-circuits to #f without raising the type error. Going
// through VariadicArgs (which type-checks all elements first) would
// reverse this — we restored the streaming form to preserve the historical
// behavior.
//
// (char<? #\b #\a not-a-char) — old: cmp(#\b, #\a)→false → #f
//
//	new: same #f
func TestCharCompareVariadic_TypeErrorAfterCmpFail(t *testing.T) {
	c := qt.New(t)
	mc := makeMC(
		values.NewCharacter('b'),
		values.List(values.NewCharacter('a'), values.NewInteger(99)),
	)
	err := CharCompareVariadic(mc, "char<?", charLT)
	c.Assert(err, qt.IsNil, qt.Commentf("first cmp failed; later type error must NOT be raised"))
	c.Assert(mc.GetValue(), qt.Equals, values.Value(values.FalseValue))
}

// TestCharCompareVariadic_FirstPairFails pins the short-circuit-on-first-pair
// path explicitly (existing tests cover only last-pair-failure).
func TestCharCompareVariadic_FirstPairFails(t *testing.T) {
	c := qt.New(t)
	mc := makeMC(
		values.NewCharacter('z'),
		values.List(values.NewCharacter('a'), values.NewCharacter('b'), values.NewCharacter('c')),
	)
	err := CharCompareVariadic(mc, "char<?", charLT)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Value(values.FalseValue))
}

// TestCharCompareVariadic_SingleArg covers the 1-arg case (no rest elements).
// With CompareVariadic in streaming form, this is the path that walks zero
// rest elements and returns #t.
func TestCharCompareVariadic_SingleArg(t *testing.T) {
	c := qt.New(t)
	mc := makeMC(values.NewCharacter('a'), values.EmptyList)
	err := CharCompareVariadic(mc, "char<?", charLT)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), qt.Equals, values.Value(values.TrueValue))
}
