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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

// captureSafetyEngine builds a KitchenSink engine with every extension loaded, so
// the procedure-invoking primitives from the eval / threads / files / gointerop
// extensions are bound and their BindingMeta.CaptureSafe stamp is observable.
// CaptureSafe is stamped unconditionally at registration (independent of the
// immutable-top-level flag), but KitchenSink + WithImmutableTopLevel mirrors how the
// frame-reclaim classifier actually consumes it.
func captureSafetyEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths(),
		wile.WithImmutableTopLevel())
	qt.Assert(t, err, qt.IsNil)
	return eng
}

// TestInvokesProcedureCompleteness is the safety net for Lever E's flipped default
// (PrimitiveSpec.InvokesProcedure defaults false = capture-safe). The flipped default
// is a soundness commitment: a procedure-invoking primitive that is NOT annotated
// would be wrongly stamped CaptureSafe and trusted by the frame-reclaim classifier,
// releasing a frame across a capturing call — silent corruption. This test pins the
// curated set of known procedure-invoking primitives (the Q-E1 audit) to
// IsCaptureSafe()==false, so adding a new higher-order primitive without the
// annotation — or dropping an existing annotation — fails CI.
//
// It cannot catch a brand-new procedure-invoker absent from this list; that is the
// continuation suite's job. If a continuation test ever regresses, the first fix is
// to add the missing name here AND annotate the primitive — the incomplete list is
// the real bug (per the plan's kill criterion).
func TestInvokesProcedureCompleteness(t *testing.T) {
	eng := captureSafetyEngine(t)
	env := eng.Environment()

	// The Q-E1 audit set: every Go primitive that runs a Scheme procedure or
	// arbitrary code (ApplyCallable, or sub-context compile/run for eval/load).
	procedureInvokers := []string{
		// core control / continuations / exceptions / parameters
		"apply", "call/cc", "call-with-current-continuation", "call-with-values",
		"call-with-continuation-barrier", "call-with-immediate-continuation-mark",
		"with-exception-handler", "raise", "raise-continuable", "call-with-exit",
		"make-parameter", "call-with-continuation-prompt",
		"call-with-composable-continuation", "with-timeout",
		// eval extension: run / transform arbitrary code
		"eval", "load", "expand", "compile",
		// promises / threads / files / gointerop
		"force", "make-thread", "thread-start!",
		"call-with-input-file", "call-with-output-file", "once-do!",
		// %parameter-convert applies the parameter's user converter via ApplyCallable
		// (the converter may call/cc) — the crosscheck-found omission this list now pins.
		"%parameter-convert",
	}
	for _, name := range procedureInvokers {
		t.Run("invoker/"+name, func(t *testing.T) {
			c := qt.New(t)
			b := env.GetBinding(values.NewSymbol(name), nil)
			c.Assert(b, qt.IsNotNil,
				qt.Commentf("%q must be bound in KitchenSink for the completeness check to cover it", name))
			c.Assert(b.IsCaptureSafe(), qt.IsFalse,
				qt.Commentf("%q invokes a Scheme procedure — it MUST be InvokesProcedure:true (not CaptureSafe), or the frame-reclaim classifier will trust a capturing call", name))
		})
	}

	// The inverse direction — capture-safe callees that MUST now be trusted, by the
	// two routes Lever E establishes:
	//   - Go primitives via the InvokesProcedure capability (assq, eq?, max,
	//     vector-ref, … — were NOT in the old 16-name whitelist; their recovery is
	//     the point of E). car/+/= were whitelisted and anchor the no-regression edge.
	//   - stdlib Scheme procedures via compile-time proof (ProcedureBodyIsCaptureSafe):
	//     not = (if x #f #t), cadr = (car (cdr x)) — proven from their bodies, never
	//     reachable by the old name whitelist, and not Go primitives. This is the
	//     generalization that lets E recover zero?/not (regressed by retiring the
	//     whitelist) without a residual list or Go facades.
	captureSafe := []string{
		"car", "cdr", "cons", "+", "-", "=", "null?", // Go prims, formerly whitelisted
		"assq", "eq?", "eqv?", "max", "min", "vector-ref", "list", // Go prims, newly trusted via capability
		"not", "zero?", "positive?", "cadr", // stdlib procedures, trusted via compile-time proof
	}
	for _, name := range captureSafe {
		t.Run("safe/"+name, func(t *testing.T) {
			c := qt.New(t)
			b := env.GetBinding(values.NewSymbol(name), nil)
			c.Assert(b, qt.IsNotNil, qt.Commentf("%q must be bound", name))
			c.Assert(b.IsCaptureSafe(), qt.IsTrue,
				qt.Commentf("%q does not invoke a Scheme procedure and must be CaptureSafe (the recovery Lever E unlocks)", name))
		})
	}

	// Stdlib higher-order PROCEDURES (not Go primitives) that apply a user procedure
	// MUST stay un-trusted: ProcedureBodyIsCaptureSafe must NOT stamp them, because
	// their body calls the procedure parameter (an unknown callee that may capture).
	// This is the soundness boundary of the prove-them generalization — without it a
	// regression that wrongly stamped map/for-each would release a frame across a
	// capturing callback. All are ambient under KitchenSink (bootstrap_procedures.scm).
	stdlibHigherOrder := []string{"map", "for-each", "vector-map", "vector-for-each", "sort"}
	for _, name := range stdlibHigherOrder {
		t.Run("ho-proc/"+name, func(t *testing.T) {
			c := qt.New(t)
			b := env.GetBinding(values.NewSymbol(name), nil)
			c.Assert(b, qt.IsNotNil, qt.Commentf("%q must be bound in KitchenSink", name))
			c.Assert(b.IsCaptureSafe(), qt.IsFalse,
				qt.Commentf("%q applies a user procedure (may capture) and MUST NOT be proven CaptureSafe", name))
		})
	}
}

// TestImportPropagatesCaptureSafe pins the import-propagation fix: markBindingImported
// must carry CaptureSafe from the source onto the freshly-created imported binding. The
// name→capability switch would otherwise silently break every (import (scheme base))
// program — the import path creates a new binding the registration-time stamp does not
// reach. Asserting IsImported()==true on the binding distinguishes propagation from the
// ambient registration stamp. This is otherwise only covered indirectly (via the
// measure harness).
func TestImportPropagatesCaptureSafe(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	eng := captureSafetyEngine(t)
	_, err := eng.EvalMultiple(ctx, "(import (scheme base))")
	c.Assert(err, qt.IsNil)
	env := eng.Environment()

	// car: a capture-safe primitive; imported ⇒ CaptureSafe must propagate from source.
	car := env.GetBinding(values.NewSymbol("car"), nil)
	c.Assert(car, qt.IsNotNil)
	c.Assert(car.IsImported(), qt.IsTrue,
		qt.Commentf("car should be the imported binding after (import (scheme base))"))
	c.Assert(car.IsCaptureSafe(), qt.IsTrue,
		qt.Commentf("imported car must carry CaptureSafe from source — else import programs lose the frame optimization"))

	// apply: a procedure-invoking primitive; must stay not-CaptureSafe across import.
	app := env.GetBinding(values.NewSymbol("apply"), nil)
	c.Assert(app, qt.IsNotNil)
	c.Assert(app.IsCaptureSafe(), qt.IsFalse,
		qt.Commentf("imported apply invokes a procedure and must NOT be CaptureSafe"))
}
