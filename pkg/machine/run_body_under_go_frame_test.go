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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

// newGoFrameHarness builds a context sitting on ONE tail continuation whose
// template has a single instruction, and reports whether that instruction ran.
//
// The tail is what makes these tests non-vacuous. NewMachineContext consumes the
// continuation it is handed (mc.cont becomes that continuation's PARENT), so a
// harness rooted on an empty template would leave the context rootless — and on
// the rootless return path an extra pc++ runs off the end of a one-instruction
// template exactly as a correct return does. With a tail to come back to, a
// Go-frame that advances pc when it should not skips the tail's instruction, and
// tailRan goes false.
func newGoFrameHarness(t *testing.T) (*MachineContext, *environment.EnvironmentFrame, *bool) {
	t.Helper()
	env := environment.NewNamespace().Runtime()
	tailRan := false
	tailTpl := NewNativeTemplate(0, 0, false)
	tailTpl.AppendInstruction(tailTpl.AppendSideTableOp(newTestInlinedOp(func(mc *MachineContext) (*MachineContext, error) {
		tailRan = true
		mc.pc++
		return mc, nil
	})))
	tail := NewMachineContinuation(nil, tailTpl, env)
	root := NewMachineContinuation(tail, NewNativeTemplate(0, 0, false), env)
	return NewMachineContext(context.Background(), root), env, &tailRan
}

// TestRunBodyUnderGoFrame covers the two legal endings of a GoFrameFunc plus its
// error path. The property that makes the helper worth having — the body runs on
// the LIVE chain, so a continuation captured inside it spans the frame — is
// asserted structurally here (the body observes a frame whose parent is the
// caller's continuation) and end-to-end by the force rows of
// pkg/registry/core/continuation_subcontext_truncation_red_test.go.
func TestRunBodyUnderGoFrame(t *testing.T) {
	tcs := []struct {
		name string
		fn   func(env *environment.EnvironmentFrame) GoFrameFunc
		want values.Value
	}{
		{
			name: "callback delivers a value",
			fn: func(*environment.EnvironmentFrame) GoFrameFunc {
				return func(mc *MachineContext, v values.Value) error {
					n, ok := v.(*values.Integer)
					if !ok {
						return werr.WrapForeignErrorf(werr.ErrNotAnInteger, "go-frame test: body value")
					}
					mc.SetValue(n.Add(values.NewInteger(1)))
					return nil
				}
			},
			want: values.NewInteger(11),
		},
		{
			name: "callback reconfigures the VM",
			fn: func(env *environment.EnvironmentFrame) GoFrameFunc {
				next := NewForeignClosure(env, 0, false, func(cc CallContext) error {
					cc.SetValue(values.NewInteger(99))
					return nil
				})
				return func(mc *MachineContext, _ values.Value) error {
					_, err := mc.ApplyCallable(next)
					return err
				}
			},
			want: values.NewInteger(99),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			mc, env, tailRan := newGoFrameHarness(t)
			before := mc.cont
			// Read the frame's shape INSIDE the body: the frame is released to the
			// pool and zeroed the moment it is restored, so a pointer kept past the
			// body's return reports a blank frame.
			pushed := false
			var seenParent *MachineContinuation
			body := NewForeignClosure(env, 0, false, func(cc CallContext) error {
				bodyMC, err := RequireMachineContext(cc, "go-frame test body")
				if err != nil {
					return err
				}
				pushed = bodyMC.cont != before
				seenParent = bodyMC.cont.parent
				bodyMC.SetValue(values.NewInteger(10))
				return nil
			})

			_, err := mc.RunBodyUnderGoFrame(body, tc.fn(env))
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.Run(), qt.IsNil)
			qt.Assert(t, mc.GetValue(), qt.CmpEquals(), tc.want)
			// The body ran under a chain-resident frame, not in a sub-context:
			// a continuation it did not push, parented on the caller's.
			qt.Assert(t, pushed, qt.IsTrue)
			qt.Assert(t, seenParent, qt.Equals, before)
			// Control reached the tail rather than running off the end of it.
			qt.Assert(t, *tailRan, qt.IsTrue)
		})
	}
}

// TestRunBodyUnderGoFrameCallbackError pins that a callback error propagates out
// of Run rather than being swallowed by the frame's return.
func TestRunBodyUnderGoFrameCallbackError(t *testing.T) {
	mc, env, tailRan := newGoFrameHarness(t)
	body := NewForeignClosure(env, 0, false, func(cc CallContext) error {
		cc.SetValue(values.NewInteger(10))
		return nil
	})
	fn := func(*MachineContext, values.Value) error {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "go-frame test: deliberate failure")
	}

	_, err := mc.RunBodyUnderGoFrame(body, fn)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.Run(), qt.ErrorIs, werr.ErrNotAProcedure)
	qt.Assert(t, *tailRan, qt.IsFalse)
}

// TestOperationGoReturnIdentity pins that OperationGoReturn compares by identity.
// Two frames carrying different callbacks are not interchangeable, and Go funcs
// are not comparable, so the usual SameType idiom would be wrong here.
func TestOperationGoReturnIdentity(t *testing.T) {
	noop := func(*MachineContext, values.Value) error {
		return nil
	}
	a := NewOperationGoReturn(noop)
	b := NewOperationGoReturn(noop)

	qt.Assert(t, a.OpKind(), qt.Equals, OpComplex)
	qt.Assert(t, a.EqualTo(a), qt.IsTrue)
	qt.Assert(t, a.EqualTo(b), qt.IsFalse)
	qt.Assert(t, a.EqualTo(values.Void), qt.IsFalse)
}
