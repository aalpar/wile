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
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// TestRegisterPrimitiveValidatesSpec pins the contract that Engine.RegisterPrimitive
// and PrimitiveRegistry.AddPrimitives are two implementations of ONE contract:
// a spec that AddPrimitives panics on must not pass RegisterPrimitive silently.
//
// The ParamCount:0 + IsVariadic shape is the one that wedges the engine rather
// than merely panicking once: the rest parameter occupies slot ParamCount-1, so
// the panic fires during frame setup and leaves the engine unable to compile
// anything afterwards.
func TestRegisterPrimitiveValidatesSpec(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	defer engine.Close()

	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "wedge",
		ParamCount: 0,
		IsVariadic: true,
		Impl: func(mc wile.CallContext) error {
			mc.SetValue(values.NewInteger(1))
			return nil
		},
	})
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("RegisterPrimitive accepted a spec AddPrimitives panics on"))
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
		qt.Commentf("got %v", err))

	// The rejected registration must leave the engine usable: the whole point of
	// returning an error instead of binding the closure is that the host survives.
	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(+ 1 2)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")

	// And the name must not have been bound.
	_, ok := engine.Get("wedge")
	c.Assert(ok, qt.IsFalse)
}

// TestRegisterPrimitiveRejectsNilImpl covers the other Validate rule an embedder
// can trip from the public API.
func TestRegisterPrimitiveRejectsNilImpl(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	defer engine.Close()

	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "no-impl",
		ParamCount: 1,
	})
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
		qt.Commentf("got %v", err))
}

// TestRegisterPrimitiveAcceptsValidSpec is the guard: no in-tree registration
// shape newly fails.
func TestRegisterPrimitiveAcceptsValidSpec(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := wile.NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	defer engine.Close()

	err = engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "rest-sum",
		ParamCount: 1,
		IsVariadic: true,
		Impl: func(mc wile.CallContext) error {
			mc.SetValue(values.NewInteger(7))
			return nil
		},
	})
	c.Assert(err, qt.IsNil)

	result, err := engine.Eval(ctx, engine.MustParse(ctx, "(rest-sum 1 2 3)"))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "7")
}
