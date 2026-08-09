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

// BindingMeta.Stable is a compiler PROOF that a binding will not be rebound, and
// it has two consumers that act on it irreversibly: the OpSelfTailCall emit gate,
// and compile_call_arity.go's compile-time arity refusal. The second one produces
// no program at all, so no runtime re-check can rescue a falsified proof — which
// is why the proof is repaired at the WRITE rather than the promise narrowed at
// the reader (review-wave-1 §8 Q3).
//
// Four doors reached a Stable slot and rewrote it. This file pins all four shut
// and pins what must still work, because every one of these fixes fails toward
// "refuse more", and a refusal that is too wide is invisible until an embedder
// hits it.
//
// The oracle throughout is the eval/load path, which ALREADY refused correctly.
// Each assertion below is "agrees with eval", not a new invented policy.

import (
	"context"
	"errors"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

func stableRebindEngine(t *testing.T) *wile.Engine {
	t.Helper()
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS), wile.WithLibraryPaths("."))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func evalStable(t *testing.T, eng *wile.Engine, src string) (string, error) {
	t.Helper()
	v, err := eng.EvalMultiple(context.Background(), src)
	if err != nil {
		return "", err
	}
	return v.Internal().SchemeString(), nil
}

// TestStableRebind_EvalBaselineRefuses is the ORACLE the three rows below are
// measured against, not an independent assertion. It already passed before any
// of this work; if it ever fails, the other rows are asserting agreement with
// something that no longer refuses and mean nothing.
func TestStableRebind_EvalBaselineRefuses(t *testing.T) {
	eng := stableRebindEngine(t)
	_, err := evalStable(t, eng, `(define (f n) 'OLD)`)
	qt.Assert(t, err, qt.IsNil)

	_, err = evalStable(t, eng, `(eval '(define (f n) 'NEW) (interaction-environment))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("got: %v", err))

	got, err := evalStable(t, eng, `(f 1)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "OLD")
}

// TestStableRebind_CompiledThunkRefuses is finding 34a. (compile '(define …))
// compiles against a LEXICAL CHILD of the engine root, which inherits the root's
// store and phase but is not the root object — so the immutable-top-level gate,
// keyed on the frame-IDENTITY test IsOwnerRoot, never armed while the write went
// to the root's slot regardless.
//
// Measured before the fix: this sequence completed and (f 1) answered NEW, on the
// very engine where the eval oracle above refuses the identical program.
func TestStableRebind_CompiledThunkRefuses(t *testing.T) {
	eng := stableRebindEngine(t)
	_, err := evalStable(t, eng, `(define (f n) 'OLD)`)
	qt.Assert(t, err, qt.IsNil)

	_, err = evalStable(t, eng, `(define t (compile '(define (f n) 'NEW)))`)
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("compile must refuse exactly as eval does; a nil error here means the "+
			"gate is back on frame identity and a thunk can rebind a stable binding"))
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("got: %v", err))

	got, err := evalStable(t, eng, `(f 1)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "OLD")
}

// TestStableRebind_NamespaceDefineRefuses is finding 34b, and it is the one that
// is reachable from ordinary Scheme at RUNTIME rather than through the Go API:
// (interaction-environment) IS the engine's own root namespace, so
// namespace-define! writes the same slot a top-level define does.
//
// It closes an asymmetry as well: namespace-undefine! already refused a binding
// it must not remove, while namespace-define! would overwrite one.
func TestStableRebind_NamespaceDefineRefuses(t *testing.T) {
	eng := stableRebindEngine(t)
	_, err := evalStable(t, eng, `(define (f n) 'OLD)`)
	qt.Assert(t, err, qt.IsNil)

	_, err = evalStable(t, eng, `(namespace-define! (interaction-environment) 'f (lambda (n) 'NEW))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("got: %v", err))

	got, err := evalStable(t, eng, `(f 1)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "OLD")
}

// TestStableRebind_EngineDefineRefuses covers the two Go-API doors.
func TestStableRebind_EngineDefineRefuses(t *testing.T) {
	eng := stableRebindEngine(t)
	_, err := evalStable(t, eng, `(define (f n) 'OLD)`)
	qt.Assert(t, err, qt.IsNil)

	err = eng.Define("f", wile.NewInteger(42))
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("got: %v", err))

	err = eng.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "f",
		ParamCount: 1,
		Impl: func(mc wile.CallContext) error {
			mc.SetValue(wile.NewInteger(42).Internal())
			return nil
		},
	})
	qt.Assert(t, err, qt.IsNotNil,
		qt.Commentf("RegisterPrimitive is the same door as Define and must refuse alike"))
	qt.Assert(t, errors.Is(err, werr.ErrImmutableBinding), qt.IsTrue,
		qt.Commentf("got: %v", err))

	got, err := evalStable(t, eng, `(f 1)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "OLD")
}

// TestStableRebind_HostOwnedNameStaysRebindable is the ANTI-OVERREACH control,
// and it is the more important half of this file.
//
// All four refusals above fail toward "refuse more", and a refusal that swallowed
// the ordinary host-variable pattern would break every embedder while every
// assertion above still passed. A name the host owns end to end carries no
// compiler proof — nothing stamps Stable on a binding Scheme never defined — so
// Define may rebind it as often as it likes. This is the shape
// ExampleEngine_Compile documents.
func TestStableRebind_HostOwnedNameStaysRebindable(t *testing.T) {
	eng := stableRebindEngine(t)
	for _, n := range []int64{1, 2, 3} {
		err := eng.Define("hostvar", wile.NewInteger(n))
		qt.Assert(t, err, qt.IsNil,
			qt.Commentf("a host-owned name must stay rebindable; refusing it would break "+
				"the documented Compile-once/Define-many embedding pattern"))
	}
	got, err := evalStable(t, eng, `hostvar`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "3")
}

// TestStableRebind_MutableTopLevelOptsOut pins the documented escape hatch, and
// doubles as the discriminator for the whole file: it holds the programs fixed
// and varies only the engine option, so a build that refused unconditionally
// (rather than because a binding is Stable) fails here.
func TestStableRebind_MutableTopLevelOptsOut(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink), wile.WithMutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})

	_, err = eng.EvalMultiple(ctx, `(define (f n) 'OLD)`)
	qt.Assert(t, err, qt.IsNil)

	err = eng.Define("f", wile.NewInteger(42))
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("nothing is Stable under a mutable top level, so nothing may be refused"))

	_, err = eng.EvalMultiple(ctx, `(namespace-define! (interaction-environment) 'f 7)`)
	qt.Assert(t, err, qt.IsNil)

	v, err := eng.EvalMultiple(ctx, `f`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "7")
}

// TestStableRebind_DefineOverImportStillWorks is the second anti-overreach
// control, and it is why the tier relocation had to land on the same branch.
// The refusal keys on the Stable FIELD and on matching coordinates. An import is
// IsStable() (Imported is standing evidence) but NOT Stable-stamped, and since
// the relocation it does not even share coordinates with a define — so both the
// Scheme define and the Go Define must still land. Shipping the refusal without
// the relocation would have made these error on every name an embedder imported.
func TestStableRebind_DefineOverImportStillWorks(t *testing.T) {
	eng := stableRebindEngine(t)
	_, err := evalStable(t, eng, `(import (scheme base))`)
	qt.Assert(t, err, qt.IsNil)

	got, err := evalStable(t, eng, `(define list-copy 7) list-copy`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "7")

	err = eng.Define("list-tail", wile.NewInteger(9))
	qt.Assert(t, err, qt.IsNil,
		qt.Commentf("Engine.Define over an imported name must land; refusing it is the "+
			"regression that makes this refusal unshippable without the T2 relocation"))
	got, err = evalStable(t, eng, `list-tail`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, got, qt.Equals, "9")
}
