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
)

// This file isolates the cost centers visible in a fib(10) CPU profile.
//
// CPU profile breakdown (BenchmarkRun/Fibonacci, M4 Max):
//
//   Run() dispatch loop   12% flat  -- opcode switch, loop overhead
//   applyForeign          21% cum   -- defer/recover, arity, arg bind, restore
//   Apply (MachineClosure) 9% cum   -- env frame copy for recursive calls
//   SaveContinuation       7% cum   -- continuation create + stack acquire
//   RestoreAndRelease     10% cum   -- pool releases (stack + cont + env)
//   PopAll / Pull          6% cum   -- stack copy operations
//   Arithmetic (-, +, <=)  7% cum   -- actual numeric work
//
// Each benchmark isolates one of these cost centers so improvements
// can be validated independently.

// ---------- helpers ----------

// fibEnv creates a top-level env with a 1-param binding (simulating fib's env).
func fibEnv() (*environment.EnvironmentFrame, *environment.EnvironmentFrame) {
	tl := environment.NewNamespace()
	runtime := tl.Runtime()
	local := environment.NewLocalEnvironment(1)
	closureEnv := environment.NewEnvironmentFrameWithParent(local, runtime)
	return runtime, closureEnv
}

// ---------- Continuation round-trip (Save + Restore) ----------

// BenchmarkContinuationRoundTrip measures one SaveContinuation + RestoreAndRelease
// cycle. fib(10) does ~885 of these (5 saves × 177 calls).
func BenchmarkContinuationRoundTrip(b *testing.B) {
	env := newBenchEnv()
	tpl := NewNativeTemplate(0, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	mc := AcquireTopLevelContext(context.Background(), tpl, env)
	mc.SetValue(values.Void)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		err := mc.SaveContinuation(0)
		if err != nil {
			b.Fatal(err)
		}
		mc.RestoreAndRelease(mc.cont)
	}
	b.StopTimer()
	ReleaseTopLevelContext(mc)
}

// ---------- Env copy (Apply path for MachineClosure) ----------

// BenchmarkEnvCopy measures the env-frame acquisition + InitApplyFrame
// that happens on every non-tail MachineClosure call. Every recursive call
// copies the env frame to prevent aliasing and thread races.
func BenchmarkEnvCopy(b *testing.B) {
	_, closureEnv := fibEnv()

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		frame := acquireEnvFrame()
		closureEnv.InitApplyFrame(frame)
		releaseEnvFrame(frame)
	}
}

// ---------- Full MachineClosure Apply ----------

// BenchmarkApplyMachineClosure measures the full Apply path for a
// MachineClosure with 1 parameter (like fib). Includes arity check,
// env copy, and binding assignment.
func BenchmarkApplyMachineClosure(b *testing.B) {
	_, closureEnv := fibEnv()
	tpl := NewNativeTemplate(1, 0, false)
	tpl.AppendInstruction(Instruction{Op: OpLoadVoid})
	cls := NewClosureWithTemplate(tpl, closureEnv)

	env := newBenchEnv()
	mc := newBenchMC(env)
	arg := values.NewInteger(5)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := mc.Apply(cls, arg)
		if err != nil {
			b.Fatal(err)
		}
		mc = result
	}
}

// ---------- Stack PopAll ----------

// BenchmarkPopAll_2 measures PopAll with 2 elements (the common case for
// binary operator calls like (+ a b), (<= n 1), (- n 1)).
func BenchmarkPopAll_2(b *testing.B) {
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

// BenchmarkDrain_2 measures Drain with 2 elements (zero-allocation).
func BenchmarkDrain_2(b *testing.B) {
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

// ---------- Pool acquire/release cycles ----------

// BenchmarkStackPool measures acquire + release of an eval stack.
func BenchmarkStackPool(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		s := acquireStack()
		releaseStack(s)
	}
}

// BenchmarkContinuationPool measures acquire + release of a continuation.
func BenchmarkContinuationPool(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		c := acquireContinuation()
		releaseContinuation(c)
	}
}

// BenchmarkEnvFramePool measures acquire + release of an env frame.
func BenchmarkEnvFramePool(b *testing.B) {
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		f := acquireEnvFrame()
		releaseEnvFrame(f)
	}
}

// ---------- defer/recover overhead in applyForeign ----------

// BenchmarkDeferRecoverFib re-measures the defer/recover cost with a
// realistic foreign function body (integer comparison, same as <=).
func BenchmarkDeferRecoverFib(b *testing.B) {
	env := newBenchEnv()
	fcls := NewForeignClosure(env, 2, false, stubLe)
	mc := newBenchMC(env)
	five := values.NewInteger(5)
	one := values.NewInteger(1)

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		result, err := mc.applyForeign(fcls, five, one)
		if err != nil {
			b.Fatal(err)
		}
		mc = result
	}
}

// stubLe is a minimal foreign function matching <=: compares two integers.
func stubLe(mc *MachineContext) error {
	bnds := mc.env.LocalEnvironment().Bindings()
	a := bnds[0].Value().(*values.Integer)
	b := bnds[1].Value().(*values.Integer)
	mc.SetValue(values.BoolToBoolean(a.Compare(b) <= 0))
	return nil
}
