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
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The gate for wave 4 item 5+7 half A, which is a DIAGNOSTIC item, not a
// catchability one: it converts two argument-domain faults that were reaching
// Scheme as recovered panics into ordinary conditions, and changes nothing about
// the panic channel itself.
//
// Half B — a blanket recover at the foreign-call boundary, making every
// primitive panic catchable — was proposed and refused. That refusal is what
// these tests exist to keep: a later change that quietly reinstated the recover
// would make every half-A assertion pass MORE easily, so the domain-error rows
// alone cannot detect it. The negative arm below is the one that can.

// panicEngine builds a KitchenSink engine carrying two deliberately faulting
// primitives, one per registration route.
//
// The two routes differ TODAY, and the difference is the subject:
//
//   - RegisterPrimitive takes the ForeignFunction as given. Nothing wraps it, so
//     a panic in the Impl unwinds to the VM boundary recover (RunResumable) and
//     reaches the embedder — guard never sees it.
//   - RegisterFunc reflects a plain Go function and installs ffiSpec.makeWrapper
//     around it (pkg/wile/ffi_wrapper.go:28), whose defer converts a panic into a
//     returned error. A returned error routes through bridgeForeignError like any
//     other, so guard DOES catch it.
func panicEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	if err != nil {
		t.Fatalf("NewEngine: %v", err)
	}
	t.Cleanup(func() {
		_ = eng.Close()
	})

	err = eng.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "raw-panic",
		ParamCount: 0,
		Impl: func(_ wile.CallContext) error {
			panic("deliberate host fault from a raw primitive")
		},
	})
	if err != nil {
		t.Fatalf("RegisterPrimitive: %v", err)
	}

	err = eng.RegisterFunc("ffi-panic", func() int {
		panic("deliberate host fault from an FFI function")
	})
	if err != nil {
		t.Fatalf("RegisterFunc: %v", err)
	}
	return eng
}

// TestMalformedArgumentIsACondition is half A's positive gate, driven through
// the public Engine API rather than the internal pipeline, because "can an
// embedder's program guard this?" is the question the item is about.
//
// Both expressions supply input outside the primitive's R7RS domain, and both
// used to answer with a recovered panic that aborted the whole evaluation.
func TestMalformedArgumentIsACondition(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := panicEngine(t)

	tcs := []struct {
		name string
		code string
	}{
		{
			// assq's alist entry is the empty list: outside R7RS §6.3's "list of
			// pairs". helpers.AssocLookup asserted values.Tuple, which '() has,
			// then called Car() on it.
			name: "assq on an alist holding the empty list",
			code: `(guard (e (#t 'caught)) (assq 'x '(())))`,
		},
		{
			// The other half of the item, and the half that was already closed
			// when the work started — it landed inside the collapsed PRIM-EVAL
			// branch rather than under its own item, so nothing pinned it. It is
			// pinned here because an incidental fix is exactly the kind that
			// regresses without anyone noticing it had been made.
			name: "environment with an empty import set",
			code: `(guard (e (#t 'caught)) (environment '()))`,
		},
		{
			// The asymmetry check the design ran before committing to the
			// fall-through: the same malformed spec given twice already took the
			// ordinary error path, so the fix landed on an existing clean
			// diagnostic rather than needing a new one.
			name: "environment with two empty import sets (control)",
			code: `(guard (e (#t 'caught)) (environment '() '()))`,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v, err := eng.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewSymbol("caught"))
		})
	}
}

// TestGenuinePrimitivePanicStaysUncatchable is half A's negative arm: the item
// must not drift into half B.
//
// A panic from a raw RegisterPrimitive Impl is a violated host invariant, not an
// argument-domain fault, and it must keep escaping guard and reaching the
// embedder. The FFI row is a labelled CONTROL, not an assertion that the two
// routes agree — they do not, and recording which way each falls is the point:
// half B would have made the first row read like the second, and the review
// declined it.
func TestGenuinePrimitivePanicStaysUncatchable(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := panicEngine(t)

	t.Run("raw primitive panic escapes guard", func(t *testing.T) {
		_, err := eng.EvalMultiple(ctx, `(guard (e (#t 'caught)) (raw-panic))`)
		c.Assert(err, qt.IsNotNil,
			qt.Commentf("a raw primitive panic became guard-catchable: half A drifted into half B"))
	})

	t.Run("FFI panic is caught by guard (control)", func(t *testing.T) {
		v, err := eng.EvalMultiple(ctx, `(guard (e (#t 'caught)) (ffi-panic))`)
		c.Assert(err, qt.IsNil)
		c.Assert(v.Internal(), valuestest.SchemeEquals, values.NewSymbol("caught"))
	})
}
