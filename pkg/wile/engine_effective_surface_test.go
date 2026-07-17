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
	"slices"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/wile"
)

func hasPrimitive(reg *registry.Registry, name string) bool {
	_, ok := reg.FindPrimitive(name, 0)
	return ok
}

// TestEffectiveRegistry_ReportsDialectNarrowing pins the split between the
// pre-dialect base and the surface the engine actually has.
//
// set-car! is the probe, not set!: set! is a special form, removed via the forms
// registry, and never appears in the primitive registry at all. NoMutation's
// RemovedPrimitives list is the 20 mutation *procedures*.
func TestEffectiveRegistry_ReportsDialectNarrowing(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithDialect(wile.NoMutation))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	// The runtime rejects it — that is the ground truth both accessors are judged against.
	_, err = eng.EvalMultiple(ctx, `(set-car! (list 1 2) 9)`)
	c.Assert(err, qt.IsNotNil)

	// Registry is the pre-dialect base and still advertises the removed procedure.
	c.Assert(hasPrimitive(eng.Registry(), "set-car!"), qt.IsTrue,
		qt.Commentf("Registry should report the pre-dialect base"))

	// EffectiveRegistry agrees with the runtime.
	c.Assert(hasPrimitive(eng.EffectiveRegistry(), "set-car!"), qt.IsFalse,
		qt.Commentf("EffectiveRegistry still lists a primitive the dialect removed"))

	// A primitive the dialect did not touch survives in both.
	c.Assert(hasPrimitive(eng.Registry(), "car"), qt.IsTrue)
	c.Assert(hasPrimitive(eng.EffectiveRegistry(), "car"), qt.IsTrue)
}

// TestForms_ReportsDialectFormRemoval covers the axis EffectiveRegistry cannot see.
// set! is removed from the forms registry, so no registry accessor reports it.
func TestForms_ReportsDialectFormRemoval(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	plain, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer plain.Close()

	narrowed, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithDialect(wile.NoMutation))
	c.Assert(err, qt.IsNil)
	defer narrowed.Close()

	c.Assert(slices.Contains(plain.Forms(), "set!"), qt.IsTrue)
	c.Assert(slices.Contains(narrowed.Forms(), "set!"), qt.IsFalse,
		qt.Commentf("Forms should track the dialect's form removal"))

	// A form neither dialect touches, and the sort contract.
	c.Assert(slices.Contains(narrowed.Forms(), "lambda"), qt.IsTrue)
	c.Assert(slices.IsSorted(narrowed.Forms()), qt.IsTrue)

	// The removed form is invisible to the registry axis in BOTH engines: this is
	// why Forms exists rather than folding set! into EffectiveRegistry.
	c.Assert(hasPrimitive(narrowed.EffectiveRegistry(), "set!"), qt.IsFalse)
	c.Assert(hasPrimitive(plain.EffectiveRegistry(), "set!"), qt.IsFalse)
}

// TestEffectiveRegistry_UnnarrowedEngineMatchesBase pins the fallback: with no
// dialect and no strict mode, the two accessors describe the same surface.
func TestEffectiveRegistry_UnnarrowedEngineMatchesBase(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	c.Assert(eng.EffectiveRegistry().PrimitiveCount(), qt.Equals, eng.Registry().PrimitiveCount())
	c.Assert(hasPrimitive(eng.EffectiveRegistry(), "set-car!"), qt.IsTrue)
}

// TestEffectiveRegistry_SurvivesWithNamespace is the reason the narrowed registry
// is recorded on the Namespace rather than returned out of bootstrap: this path
// hands NewEngine a pre-built namespace, so a value held only in an Engine field
// set during bootstrap would be gone. The dialect narrowing happens inside
// NewNamespace here, not in NewEngine.
func TestEffectiveRegistry_SurvivesWithNamespace(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ns, err := wile.NewNamespace(ctx, wile.WithProfile(wile.KitchenSink), wile.WithDialect(wile.NoMutation))
	c.Assert(err, qt.IsNil)

	eng, err := wile.NewEngine(ctx, wile.WithNamespace(ns))
	c.Assert(err, qt.IsNil)
	defer eng.Close()

	_, err = eng.EvalMultiple(ctx, `(set-car! (list 1 2) 9)`)
	c.Assert(err, qt.IsNotNil, qt.Commentf("dialect should have narrowed this namespace"))

	c.Assert(hasPrimitive(eng.Registry(), "set-car!"), qt.IsTrue)
	c.Assert(hasPrimitive(eng.EffectiveRegistry(), "set-car!"), qt.IsFalse,
		qt.Commentf("narrowing recorded in NewNamespace did not survive to the engine"))
	c.Assert(slices.Contains(eng.Forms(), "set!"), qt.IsFalse)
}
