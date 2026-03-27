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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
	"github.com/aalpar/wile/werr"

	qt "github.com/frankban/quicktest"
)

// foreignAddClosure creates a ForeignClosure that adds two integer arguments.
func foreignAddClosure() *ForeignClosure {
	env := environment.NewNamespace().Runtime()
	return NewForeignClosure(env, 2, false, func(mc *MachineContext) error {
		bnds := mc.env.LocalEnvironment().Bindings()
		a := bnds[0].Value().(*values.Integer)
		b := bnds[1].Value().(*values.Integer)
		mc.SetValue(a.Add(b))
		return nil
	})
}

// foreignErrorClosure creates a ForeignClosure that always returns an error.
func foreignErrorClosure() *ForeignClosure {
	env := environment.NewNamespace().Runtime()
	return NewForeignClosure(env, 0, false, func(mc *MachineContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "intentional test error")
	})
}

func TestCallForeignCachedValidatorCalled(t *testing.T) {
	c := qt.New(t)

	validatorCalls := 0
	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	closureEnv := environment.NewNamespace().Runtime()
	fcls := NewForeignClosure(closureEnv, 0, false, func(mc *MachineContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	})
	fcls.SetValidator(func(mc *MachineContext) error {
		validatorCalls++
		return nil
	})
	bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

	tpl := NewNativeTemplate(0, 0, false)
	cbIdx := tpl.AppendCachedBinding(bd)

	tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()
	c.Assert(err, qt.IsNil)
	c.Assert(validatorCalls, qt.Equals, 1)
	c.Assert(fnCalls, qt.Equals, 1)
}

func TestCallForeignCachedValidatorRejectsCall(t *testing.T) {
	c := qt.New(t)

	fnCalls := 0

	env := environment.NewNamespace().Runtime()
	closureEnv := environment.NewNamespace().Runtime()
	fcls := NewForeignClosure(closureEnv, 0, false, func(mc *MachineContext) error {
		fnCalls++
		mc.SetValue(values.TrueValue)
		return nil
	})
	fcls.SetValidator(func(mc *MachineContext) error {
		return werr.WrapForeignErrorf(werr.ErrNotAProcedure, "validator rejected")
	})
	bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

	tpl := NewNativeTemplate(0, 0, false)
	cbIdx := tpl.AppendCachedBinding(bd)

	tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 2})
	tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	defer ReleaseTopLevelContext(mc)

	err := mc.Run()
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "validator rejected")
	c.Assert(fnCalls, qt.Equals, 0, qt.Commentf("fn must not be called when validator rejects"))
}

func TestOpCallForeignCached(t *testing.T) {
	tests := []struct {
		name      string
		setupTpl  func() (*NativeTemplate, *environment.EnvironmentFrame)
		wantValue values.Value
		wantErr   error
	}{
		{
			name: "non-tail: add 3+4=7",
			setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
				env := environment.NewNamespace().Runtime()
				fcls := foreignAddClosure()
				bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

				tpl := NewNativeTemplate(0, 0, false)
				cbIdx := tpl.AppendCachedBinding(bd)

				// SaveContinuation provides stack isolation for non-tail.
				// Offset 4 targets one past CallForeignCached (return point).
				tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 4})
				litIdx3 := tpl.MaybeAppendLiteral(values.NewInteger(3))
				litIdx4 := tpl.MaybeAppendLiteral(values.NewInteger(4))
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx3)})
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx4)})
				tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

				return tpl, env
			},
			wantValue: values.NewInteger(7),
		},
		{
			name: "tail: add 5+6=11",
			setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
				env := environment.NewNamespace().Runtime()
				fcls := foreignAddClosure()
				bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

				tpl := NewNativeTemplate(0, 0, false)
				cbIdx := tpl.AppendCachedBinding(bd)

				litIdx5 := tpl.MaybeAppendLiteral(values.NewInteger(5))
				litIdx6 := tpl.MaybeAppendLiteral(values.NewInteger(6))
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx5)})
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx6)})
				tpl.AppendInstruction(Instruction{Op: OpCallForeignCachedTail, Arg: cbIdx})

				return tpl, env
			},
			wantValue: values.NewInteger(11),
		},
		{
			name: "arity error: too few args",
			setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
				env := environment.NewNamespace().Runtime()
				fcls := foreignAddClosure() // expects 2 args
				bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

				tpl := NewNativeTemplate(0, 0, false)
				cbIdx := tpl.AppendCachedBinding(bd)

				// SaveContinuation for non-tail; offset 3 = one past CallForeignCached.
				tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 3})
				// Push only 1 arg.
				litIdx := tpl.MaybeAppendLiteral(values.NewInteger(1))
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx)})
				tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

				return tpl, env
			},
			wantErr: werr.ErrWrongNumberOfArguments,
		},
		{
			name: "variadic: sum with rest args",
			setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
				env := environment.NewNamespace().Runtime()

				// paramCount=2, isVariadic=true: bnds[0]=first required, bnds[1]=rest list
				varEnv := environment.NewNamespace().Runtime()
				fcls := NewForeignClosure(varEnv, 2, true, func(mc *MachineContext) error {
					bnds := mc.env.LocalEnvironment().Bindings()
					first := bnds[0].Value().(*values.Integer)
					restVal := bnds[1].Value()

					sum := first
					cur := restVal
					for !values.IsEmptyList(cur) {
						p := cur.(*values.Pair)
						n := p.Car().(*values.Integer)
						sum = sum.Add(n).(*values.Integer)
						cur = p.Cdr()
					}
					mc.SetValue(sum)
					return nil
				})
				bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

				tpl := NewNativeTemplate(0, 0, false)
				cbIdx := tpl.AppendCachedBinding(bd)

				// SaveContinuation for non-tail; offset 5 = one past CallForeignCached.
				tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 5})
				// Push 3 args: 10, 20, 30 => first=10, rest=(20 30), sum=60
				litIdx10 := tpl.MaybeAppendLiteral(values.NewInteger(10))
				litIdx20 := tpl.MaybeAppendLiteral(values.NewInteger(20))
				litIdx30 := tpl.MaybeAppendLiteral(values.NewInteger(30))
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx10)})
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx20)})
				tpl.AppendInstruction(Instruction{Op: OpPushLiteral, Arg: int32(litIdx30)})
				tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

				return tpl, env
			},
			wantValue: values.NewInteger(60),
		},
		{
			name: "error propagation from foreign fn",
			setupTpl: func() (*NativeTemplate, *environment.EnvironmentFrame) {
				env := environment.NewNamespace().Runtime()
				fcls := foreignErrorClosure() // 0 params, always errors
				bd := environment.NewBinding(fcls, environment.BindingTypeVariable)

				tpl := NewNativeTemplate(0, 0, false)
				cbIdx := tpl.AppendCachedBinding(bd)

				// SaveContinuation for non-tail; offset 2 = one past CallForeignCached.
				tpl.AppendInstruction(Instruction{Op: OpSaveContinuation, Arg: 2})
				// No args — 0-param closure.
				tpl.AppendInstruction(Instruction{Op: OpCallForeignCached, Arg: cbIdx})

				return tpl, env
			},
			// The error gets wrapped as ErrExceptionEscape by applyCallableError,
			// so we check for the exception escape wrapper, not the raw sentinel.
			wantErr: werr.ErrNotAProcedure,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			tpl, env := tt.setupTpl()
			mc := AcquireTopLevelContext(context.Background(), tpl, env)
			defer ReleaseTopLevelContext(mc)

			err := mc.Run()

			if tt.wantErr != nil {
				qt.Assert(t, err, qt.IsNotNil, qt.Commentf("expected error"))
				// The error may be wrapped in ErrExceptionEscape, so we just
				// check that the original sentinel is somewhere in the chain.
				// applyCallableError wraps non-exception errors via goErrorToSchemeException.
				// For ErrWrongNumberOfArguments, it's wrapped in ErrExceptionEscape
				// whose .Error() contains the original message.
				// For the generic error test, the foreign fn's error is also wrapped.
				qt.Assert(t, err.Error(), qt.Contains, tt.wantErr.Error())
				return
			}

			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, tt.wantValue)
		})
	}
}
