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

// ── MakeUnaryAccessor ────────────────────────────────────────────────

func TestMakeUnaryAccessor(t *testing.T) {
	c := qt.New(t)

	unbox := MakeUnaryAccessor(werr.ErrNotABox, "unbox", func(b *values.Box) values.Value {
		return b.Unbox()
	})

	box := values.NewBox(values.NewInteger(42))
	mc := makeMC(box)
	err := unbox(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestMakeUnaryAccessor_WrongType(t *testing.T) {
	c := qt.New(t)

	unbox := MakeUnaryAccessor(werr.ErrNotABox, "unbox", func(b *values.Box) values.Value {
		return b.Unbox()
	})

	mc := makeMC(values.NewInteger(1))
	err := unbox(mc)
	c.Assert(err, qt.IsNotNil)
	// Sentinel identity must survive the factory (callers match via errors.Is).
	c.Assert(errors.Is(err, werr.ErrNotABox), qt.IsTrue)
}

// ── MakeUnarySideEffect ──────────────────────────────────────────────

func TestMakeUnarySideEffect(t *testing.T) {
	c := qt.New(t)

	called := false
	touch := MakeUnarySideEffect(werr.ErrNotABox, "touch!", func(b *values.Box) {
		called = true
		b.Value = values.NewInteger(7)
	})

	box := values.NewBox(values.NewInteger(0))
	mc := makeMC(box)
	err := touch(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(called, qt.IsTrue)
	c.Assert(box.Unbox(), valuestest.SchemeEquals, values.NewInteger(7))
	// Side-effect primitives return the unspecified value.
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.Void)
}

func TestMakeUnarySideEffect_WrongType(t *testing.T) {
	c := qt.New(t)

	touch := MakeUnarySideEffect(werr.ErrNotABox, "touch!", func(b *values.Box) {})
	mc := makeMC(values.NewInteger(1))
	err := touch(mc)
	c.Assert(errors.Is(err, werr.ErrNotABox), qt.IsTrue)
}

// ── MakeBinarySetter ─────────────────────────────────────────────────

// TestMakeBinarySetter validates the core design property: the SECOND argument
// is threaded through untyped and the setter mutates the FIRST (typed) argument
// with it, returning Void.
func TestMakeBinarySetter(t *testing.T) {
	c := qt.New(t)

	setBox := MakeBinarySetter(werr.ErrNotABox, "set-box!", func(b *values.Box, val values.Value) {
		b.Value = val
	})

	box := values.NewBox(values.NewInteger(0))
	mc := makeMC(box, values.NewInteger(99))
	err := setBox(mc)
	c.Assert(err, qt.IsNil)
	c.Assert(box.Unbox(), valuestest.SchemeEquals, values.NewInteger(99))
	c.Assert(mc.GetValue(), valuestest.SchemeEquals, values.Void)
}

func TestMakeBinarySetter_WrongType(t *testing.T) {
	c := qt.New(t)

	setBox := MakeBinarySetter(werr.ErrNotABox, "set-box!", func(b *values.Box, val values.Value) {})
	mc := makeMC(values.NewInteger(1), values.NewInteger(2))
	err := setBox(mc)
	c.Assert(errors.Is(err, werr.ErrNotABox), qt.IsTrue)
}
