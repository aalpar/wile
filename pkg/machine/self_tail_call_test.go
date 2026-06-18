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

package machine

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// TestOpSelfTailCall_RebindsSlotsAndJumpsToZero pins the OpSelfTailCall runtime:
// it drains the (already-evaluated) argument values off the eval stack, writes
// them into the current frame's parameter slots in place (parallel assignment),
// and sets pc=0 — no frame acquire, no continuation growth.
//
// The template is [OpBranch(+2), OpSelfTailCall(2)] entered at pc=1: the op runs
// once, jumps to pc=0, and the branch carries pc past the end to halt. This
// isolates a single in-place rebind from the self-loop it normally drives.
func TestOpSelfTailCall_RebindsSlotsAndJumpsToZero(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpBranch, Arg: 2})       // pc=0 → past end, halt
	tpl.AppendInstruction(Instruction{Op: OpSelfTailCall, Arg: 2}) // pc=1 → rebind, pc=0

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	// Activation frame: two parameter slots holding the OLD values.
	lenv := environment.NewLocalEnvironment(2)
	frame := environment.NewEnvironmentFrameWithParent(lenv, env)
	pre := frame.LocalEnvironment().Bindings()
	pre[0].SetValue(values.NewInteger(5))
	pre[1].SetValue(values.NewInteger(10))
	mc.env = frame

	// New argument values, already evaluated onto the eval stack in slot order.
	mc.evals.Push(values.NewInteger(6))
	mc.evals.Push(values.NewInteger(11))
	mc.pc = 1

	err := mc.Run()
	if err != nil {
		t.Fatalf("Run: %v", err)
	}

	got := frame.LocalEnvironment().Bindings()
	g0 := got[0].Value().(*values.Integer).Value
	g1 := got[1].Value().(*values.Integer).Value
	if g0 != 6 || g1 != 11 {
		t.Fatalf("slots after self-tail rebind = %d,%d; want 6,11", g0, g1)
	}
	// Drain ONCE — it is destructive — and assert on the result. (A two-call form
	// would empty the stack on the first call and always see empty on the second,
	// making the check tautological.)
	remaining := mc.evals.Drain()
	if len(remaining) != 0 {
		t.Errorf("eval stack must be empty after the self-tail call drained its args, got %d", len(remaining))
	}
}
