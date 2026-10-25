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

// PIN, not a gate: buildRestArg hands every variadic ForeignClosure call the SAME
// per-MachineContext PairBlock, so a rest list a primitive retains past its call
// changes under it when the next variadic call re-links the block.
//
// This is green on master by construction. It records the current aliasing as a
// ratchet against a future change, not a bug fix. The exploit does not reproduce in
// anything checked: PrimList copies the rest spine rather than returning it, and the
// buffer is documented as safe only because the frame is used synchronously. The 129
// variadic primitives and the variadic finalizer closures were audited for retention on
// 2026-10-24 and none retains its rest list; the audited set is pinned by
// TestVariadicRestRetentionReviewed and TestVariadicForeignClosureSitesReviewed
// (pkg/wile/variadic_rest_retention_guard_test.go), which pin the set, not the property.
//
// Mutation that reddens it: make buildRestArg allocate fresh, for example
// `return make(values.PairBlock, n).LinkWith(vs[start:])`. Every aliasing subtest then
// fails (the retained list no longer changes; the two lists are distinct cells). That is
// the signal wanted: whoever stops sharing the buffer changes the safety argument in
// buildRestArg's comment and should update it and remove this pin.
//
// Entry points driven: buildRestArg directly; the generic path (applyForeign, which
// passes p.buildRestArg to bindArgs); and the peephole fast path (callForeignCached,
// reached through cachedBindings, which is the path most primitive calls take).
//
// What it does not cover:
//
//   - The grow path beyond one row. buildRestArg reallocates to 2n when cap < n, so a
//     call larger than the buffer's capacity (twice the first call's n) stops aliasing
//     the earlier list. One subtest pins that boundary; the doubling factor itself is
//     not pinned.
//   - Concurrency. The buffer is per MachineContext, so SRFI-18 threads do not share
//     it; nothing here exercises two contexts.
//   - The tail-call variant of callForeignCached, and callForeignCachedReassigned.

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

const restArgRemedy = "If sharing was dropped on purpose, update buildRestArg's SAFETY comment and remove this pin."

func restArgInts(ns ...int64) []values.Value {
	q := make([]values.Value, len(ns))
	for i, n := range ns {
		q[i] = values.NewInteger(n)
	}
	return q
}

func TestBuildRestArgAliasesTheSharedBlockPin(t *testing.T) {
	t.Run("a second call re-links the first call's list", func(t *testing.T) {
		c := qt.New(t)
		mc := &MachineContext{}

		first := mc.buildRestArg(restArgInts(1, 2, 3), 0)
		c.Assert(first.SchemeString(), qt.Equals, "(1 2 3)")

		// n=2 does not exceed the first call's n=3, so the block is reused, not grown.
		second := mc.buildRestArg(restArgInts(8, 9), 0)
		c.Assert(second.SchemeString(), qt.Equals, "(8 9)")

		c.Assert(first.SchemeString(), qt.Equals, "(8 9)",
			qt.Commentf("the first call's rest list should have been re-linked in place by the second. %s", restArgRemedy))
		c.Assert(first.(*values.Pair), qt.Equals, second.(*values.Pair),
			qt.Commentf("both calls should have returned the head of the same block. %s", restArgRemedy))
	})

	t.Run("a second call as large as the capacity still aliases", func(t *testing.T) {
		c := qt.New(t)
		mc := &MachineContext{}

		// n=3 allocates a buffer of 6, so n=6 fits exactly and reuses it.
		first := mc.buildRestArg(restArgInts(1, 2, 3), 0)
		second := mc.buildRestArg(restArgInts(10, 20, 30, 40, 50, 60), 0)

		c.Assert(first.(*values.Pair), qt.Equals, second.(*values.Pair),
			qt.Commentf("a call of n equal to the buffer capacity should reuse the block. %s", restArgRemedy))
		c.Assert(first.SchemeString(), qt.Equals, "(10 20 30 40 50 60)")
	})

	t.Run("a call larger than the capacity reallocates and stops aliasing", func(t *testing.T) {
		c := qt.New(t)
		mc := &MachineContext{}

		first := mc.buildRestArg(restArgInts(1, 2, 3), 0)
		second := mc.buildRestArg(restArgInts(1, 2, 3, 4, 5, 6, 7), 0)

		c.Assert(first.(*values.Pair), qt.Not(qt.Equals), second.(*values.Pair),
			qt.Commentf("n above the capacity should get a fresh block. %s", restArgRemedy))
		c.Assert(first.SchemeString(), qt.Equals, "(1 2 3)",
			qt.Commentf("the first list sits in the old block, which a grown buffer no longer touches"))
	})

	t.Run("through applyForeign, a retained rest list changes under the caller", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()

		var retained values.Value
		// (lambda (x . rest) ...) retaining rest past the call.
		cls := newTestForeignClosure(env, 2, true, func(cc CallContext) error {
			retained = cc.EnvironmentFrame().LocalEnvironment().Bindings()[1].Value()
			return nil
		})

		tpl := NewNativeTemplate(0, 0, false)
		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		_, err := mc.applyForeign(cls, restArgInts(0, 2, 3, 4)...)
		c.Assert(err, qt.IsNil)
		firstRest := retained
		c.Assert(firstRest.SchemeString(), qt.Equals, "(2 3 4)")

		_, err = mc.applyForeign(cls, restArgInts(0, 8, 9)...)
		c.Assert(err, qt.IsNil)

		c.Assert(firstRest.SchemeString(), qt.Equals, "(8 9)",
			qt.Commentf("the retained list from the first applyForeign should have been re-linked by the second. %s", restArgRemedy))
	})

	t.Run("through callForeignCached, the peephole fast path", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()

		var retained []values.Value
		varEnv := environment.NewNamespace().Runtime()
		fcls := NewForeignClosure(varEnv, 2, true, func(cc CallContext) error {
			mc := cc.(*MachineContext)
			retained = append(retained, mc.env.LocalEnvironment().Bindings()[1].Value())
			mc.SetValue(values.Void)
			return nil
		})
		bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

		tpl := NewNativeTemplate(0, 0, false)
		cbIdx := tpl.AppendCachedBinding(bd)
		lit := func(n int64) int32 {
			return int32(tpl.MaybeAppendLiteral(values.NewInteger(n)))
		}

		// Two non-tail calls in one template. SaveContinuation's Arg is the pc one past
		// its CallForeignCached: 6 for the first call (pcs 0-5), 11 for the second (6-10).
		tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 6})
		for _, n := range []int64{0, 2, 3, 4} {
			tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: lit(n)})
		}
		tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})
		tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 11})
		for _, n := range []int64{0, 8, 9} {
			tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: lit(n)})
		}
		tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

		mc := AcquireTopLevelContext(context.Background(), tpl, env)
		defer ReleaseTopLevelContext(mc)
		err := mc.Run()
		c.Assert(err, qt.IsNil)

		c.Assert(len(retained), qt.Equals, 2)
		c.Assert(retained[1].SchemeString(), qt.Equals, "(8 9)")
		c.Assert(retained[0].SchemeString(), qt.Equals, "(8 9)",
			qt.Commentf("the first cached call's rest list should have been re-linked by the second. %s", restArgRemedy))
	})
}
