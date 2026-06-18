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

func strLT(a, b string) bool {
	return a < b
}

func strEQ(a, b string) bool {
	return a == b
}

// ── StringCompareVariadic ────────────────────────────────────────────

func TestStringCompareVariadic(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
		cmp  func(a, b string) bool
		want values.Value
	}{
		{
			"single string always true",
			values.NewString("hello"),
			values.EmptyList,
			strLT,
			values.TrueValue,
		},
		{
			"two strings ascending",
			values.NewString("abc"),
			values.List(values.NewString("def")),
			strLT,
			values.TrueValue,
		},
		{
			"two strings not ascending",
			values.NewString("def"),
			values.List(values.NewString("abc")),
			strLT,
			values.FalseValue,
		},
		{
			"three strings ascending",
			values.NewString("a"),
			values.List(values.NewString("b"), values.NewString("c")),
			strLT,
			values.TrueValue,
		},
		{
			"three strings equal",
			values.NewString("x"),
			values.List(values.NewString("x"), values.NewString("x")),
			strEQ,
			values.TrueValue,
		},
		{
			"three strings not all equal",
			values.NewString("x"),
			values.List(values.NewString("x"), values.NewString("y")),
			strEQ,
			values.FalseValue,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := StringCompareVariadic(mc, "test", tc.cmp)
			c.Assert(err, qt.IsNil)
			c.Assert(mc.GetValue(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestStringCompareVariadic_Errors(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		arg0 values.Value
		arg1 values.Value
	}{
		{
			"first arg not a string",
			values.NewInteger(1),
			values.List(values.NewString("hello")),
		},
		{
			"non-string in rest",
			values.NewString("hello"),
			values.List(values.NewInteger(42)),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc := makeMC(tc.arg0, tc.arg1)
			err := StringCompareVariadic(mc, "test", strLT)
			c.Assert(err, qt.IsNotNil)
			c.Assert(errors.Is(err, werr.ErrNotAString), qt.IsTrue)
		})
	}
}
