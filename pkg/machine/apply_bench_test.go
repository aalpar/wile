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
	"fmt"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// This file benchmarks the individual stages of the Apply path to identify
// where time is spent when calling a foreign function like (+ 1 2).
//
// The stages build incrementally:
//   1. BindingAccess     — read a value from a Binding
//   2. StackPushPopAll   — push 2 args + PopAll (the allocation cost)
//   3. TypeSwitch        — interface type assertion on values.Value
//   4. DeferRecover      — defer/recover overhead with no panic
//   5. ForeignDirect     — call a Go function directly (no VM machinery)
//   6. ApplyForeign      — full applyForeign path
//   7. ApplyCallableFull — full ApplyCallable (type switch + applyForeign)

var benchSink values.Value

// BenchmarkOpcodeDispatch measures the per-opcode cost of the VM dispatch
// loop by running a template full of OpLoadVoid instructions (the cheapest
// inlined op: just sets value register to Void and increments pc).
func BenchmarkOpcodeDispatch(b *testing.B) {
	counts := []int{1, 4, 8, 16}
	for _, n := range counts {
		b.Run(fmt.Sprintf("%d_ops", n), func(b *testing.B) {
			env := newBenchEnv()
			tpl := NewNativeTemplate(0, 0, false)
			for range n {
				tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
			}
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				mc := AcquireTopLevelContext(context.Background(), tpl, env)
				err := mc.Run()
				ReleaseTopLevelContext(mc)
				if err != nil {
					b.Fatal(err)
				}
			}
		})
	}
}

// stubAdd is a minimal foreign function: pops two integers, adds them.
func stubAdd(cc CallContext) error {
	mc := cc.(*MachineContext)
	bnds := mc.env.LocalEnvironment().Bindings()
	a := bnds[0].Value().(*values.Integer)
	b := bnds[1].Value().(*values.Integer)
	mc.SetValue(a.Add(b))
	return nil
}

// newBenchEnv creates a top-level environment for benchmarking.
func newBenchEnv() *environment.EnvironmentFrame {
	return environment.NewNamespace().Runtime()
}

// newBenchMC creates a minimal MachineContext suitable for benchmarking.
func newBenchMC(env *environment.EnvironmentFrame) *MachineContext {
	tpl := NewNativeTemplate(0, 0, false)
	return AcquireTopLevelContext(context.Background(), tpl, env)
}

// BenchmarkBindingAccess measures the cost of reading a value from a Binding.
func BenchmarkBindingAccess(b *testing.B) {
	bd := environment.NewBinding(values.NewInteger(42), environment.BindingTypeVariable)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		benchSink = bd.Value()
	}
}

// BenchmarkStackPushPopAll measures Push(2 values) + PopAll.
func BenchmarkStackPushPopAll(b *testing.B) {
	one := values.NewInteger(1)
	two := values.NewInteger(2)
	s := NewStack()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s.Push(one)
		s.Push(two)
		_ = s.PopAll()
	}
}

// BenchmarkStackPushDrain measures Push(2 values) + Drain (zero-allocation).
func BenchmarkStackPushDrain(b *testing.B) {
	one := values.NewInteger(1)
	two := values.NewInteger(2)
	s := NewStack()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s.Push(one)
		s.Push(two)
		_ = s.Drain()
	}
}

// BenchmarkTypeSwitch measures the cost of a type switch on values.Value
// dispatching to *ForeignClosure (the common case for primitive calls).
func BenchmarkTypeSwitch(b *testing.B) {
	env := newBenchEnv()
	var callable values.Value = NewForeignClosure(env, 2, false, stubAdd)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		switch callable.(type) {
		case *MachineClosure:
		case *ForeignClosure:
			benchSink = callable
		case *CaseLambdaClosure:
		case *Parameter:
		case *ComposableContinuation:
		}
	}
}

// BenchmarkDeferRecover measures the overhead of defer/recover when no
// panic occurs. This is the cost paid on every applyForeign call.
func BenchmarkDeferRecover(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		func() {
			defer func() {
				recover() //nolint:errcheck
			}()
		}()
	}
}

// BenchmarkForeignDirect measures calling a Go function directly with
// manual arg binding — no VM machinery, no defer, no error handling.
func BenchmarkForeignDirect(b *testing.B) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, stubAdd)
	mc := newBenchMC(env)
	mc.env = fcls.env // applyForeign does this before calling fn
	one := values.NewInteger(1)
	two := values.NewInteger(2)

	bnds := fcls.env.LocalEnvironment().Bindings()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		bnds[0].SetValue(one)
		bnds[1].SetValue(two)
		_ = stubAdd(mc)
	}
}

// BenchmarkApplyForeign measures the full applyForeign path: arity check,
// arg binding, defer/recover, function call, error checks, continuation
// restore. This is what runs when the VM calls a primitive.
func BenchmarkApplyForeign(b *testing.B) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, stubAdd)
	mc := newBenchMC(env)
	one := values.NewInteger(1)
	two := values.NewInteger(2)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := mc.applyForeign(fcls, one, two)
		if err != nil {
			b.Fatal(err)
		}
		mc = result
	}
}

// BenchmarkApplyCallableFull measures the full ApplyCallable path: type
// switch dispatch + applyForeign. This is what OpApply calls.
func BenchmarkApplyCallableFull(b *testing.B) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, stubAdd)
	mc := newBenchMC(env)
	one := values.NewInteger(1)
	two := values.NewInteger(2)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := mc.ApplyCallable(fcls, one, two)
		if err != nil {
			b.Fatal(err)
		}
		mc = result
	}
}
