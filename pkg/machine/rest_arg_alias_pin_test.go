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
// ratchet against a future change, not a bug fix: no variadic primitive in the tree
// retains its rest Tuple past the call that received it (the buffer is documented as
// safe only because the frame is used synchronously), so there is no exploit to
// close.
//
// Mutation that reddens it: make buildRestArg allocate fresh, for example
// `return make(values.PairBlock, n).LinkWith(vs[start:])`. Both subtests then fail
// (the retained list no longer changes; the two lists are distinct cells). That is the
// signal wanted: whoever stops sharing the buffer changes the safety argument in
// buildRestArg's comment and should say so here.
//
// What it does not cover:
//
//   - The grow path. A second call with a LARGER n reallocates the buffer, after
//     which the first call's list is no longer aliased, so the pin keeps the second
//     call's n at or below the first's.
//   - The cached-call and generic-apply entry points beyond that both pass
//     p.buildRestArg to bindArgs (call_foreign_cached.go, machine_context_apply.go).
//     The second subtest drives the generic one; the first drives buildRestArg alone.
//   - Concurrency. The buffer is per MachineContext, so SRFI-18 threads do not share
//     it; nothing here exercises two contexts.

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

func TestBuildRestArgAliasesTheSharedBlockPin(t *testing.T) {
	ints := func(ns ...int64) []values.Value {
		q := make([]values.Value, len(ns))
		for i, n := range ns {
			q[i] = values.NewInteger(n)
		}
		return q
	}

	t.Run("a second call re-links the first call's list", func(t *testing.T) {
		c := qt.New(t)
		mc := &MachineContext{}

		first := mc.buildRestArg(ints(1, 2, 3), 0)
		c.Assert(first.SchemeString(), qt.Equals, "(1 2 3)")

		// n=2 does not exceed the first call's n=3, so the block is reused, not grown.
		second := mc.buildRestArg(ints(8, 9), 0)
		c.Assert(second.SchemeString(), qt.Equals, "(8 9)")

		c.Assert(first.SchemeString(), qt.Equals, "(8 9)",
			qt.Commentf("the first call's rest list was re-linked in place by the second call"))
		c.Assert(first.(*values.Pair), qt.Equals, second.(*values.Pair),
			qt.Commentf("both calls returned the head of the same block"))
	})

	t.Run("through applyForeign, a retained rest list changes under the caller", func(t *testing.T) {
		c := qt.New(t)
		env := environment.NewNamespace().Runtime()

		var retained values.Value
		// (lambda (x . rest) ...) retaining rest past the call, which no primitive in
		// the tree does.
		cls := newTestForeignClosure(env, 2, true, func(cc CallContext) error {
			retained = cc.EnvironmentFrame().LocalEnvironment().Bindings()[1].Value()
			return nil
		})

		tpl := NewNativeTemplate(0, 0, false)
		cont := NewMachineContinuation(nil, tpl, env)
		mc := NewMachineContext(context.Background(), cont)

		_, err := mc.applyForeign(cls, ints(0, 2, 3, 4)...)
		c.Assert(err, qt.IsNil)
		firstRest := retained
		c.Assert(firstRest.SchemeString(), qt.Equals, "(2 3 4)")

		_, err = mc.applyForeign(cls, ints(0, 8, 9)...)
		c.Assert(err, qt.IsNil)

		c.Assert(firstRest.SchemeString(), qt.Equals, "(8 9)",
			qt.Commentf("the retained list from the first applyForeign was re-linked by the second"))
	})
}
