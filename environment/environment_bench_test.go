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

package environment

import (
	"fmt"
	"testing"

	"github.com/aalpar/wile/values"
)

// setupLocalEnv creates an EnvironmentFrame with n local bindings for benchmarking.
// Returns the frame and the symbols used as keys.
func setupLocalEnv(n int) (*EnvironmentFrame, []*values.Symbol) {
	tl := NewTopLevelEnvironment()
	parent := tl.Runtime()
	env := NewEnvironmentFrameWithParent(NewLocalEnvironment(0), parent)

	syms := make([]*values.Symbol, n)
	for i := range n {
		sym := env.InternSymbol(values.NewSymbol(fmt.Sprintf("x%d", i)))
		syms[i] = sym
		env.EnsureLocalBinding(sym, BindingTypeVariable)
		li := env.GetLocalIndex(sym)
		env.SetLocalValue(li, values.NewInteger(int64(i)))
	}
	return env, syms
}

// setupGlobalEnv creates an EnvironmentFrame with n global bindings for benchmarking.
// Returns the frame and the GlobalIndex values used as keys.
func setupGlobalEnv(n int) (*EnvironmentFrame, []*GlobalIndex) {
	tl := NewTopLevelEnvironment()
	env := tl.Runtime()

	gis := make([]*GlobalIndex, n)
	for i := range n {
		sym := env.InternSymbol(values.NewSymbol(fmt.Sprintf("g%d", i)))
		env.MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
		gi := env.GetGlobalIndex(sym)
		env.SetOwnGlobalValue(gi, values.NewInteger(int64(i)))
		gis[i] = gi
	}
	return env, gis
}

// BenchmarkLocalLookup measures GetLocalIndex — map lookup for local binding resolution.
func BenchmarkLocalLookup(b *testing.B) {
	env, syms := setupLocalEnv(10)
	sym := syms[5]
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = env.GetLocalIndex(sym)
	}
}

// BenchmarkLocalBindingByIndex measures GetLocalBindingByIndex — direct array access.
// This is the VM hot path for reading local variables.
func BenchmarkLocalBindingByIndex(b *testing.B) {
	env, syms := setupLocalEnv(10)
	li := env.GetLocalIndex(syms[5])
	idx := li[0]
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = env.GetLocalBindingByIndex(idx)
	}
}

// BenchmarkLocalSet measures SetLocalValue — the VM hot path for writing local variables.
func BenchmarkLocalSet(b *testing.B) {
	env, syms := setupLocalEnv(10)
	li := env.GetLocalIndex(syms[5])
	val := values.NewInteger(99)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		env.SetLocalValue(li, val)
	}
}

// BenchmarkGlobalLookup measures GetGlobalBinding(GetGlobalIndex(sym)) — global binding resolution.
func BenchmarkGlobalLookup(b *testing.B) {
	env, gis := setupGlobalEnv(10)
	gi := gis[5]
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		_ = env.GetGlobalBinding(gi)
	}
}

// BenchmarkGlobalSet measures SetOwnGlobalValue — global variable write.
func BenchmarkGlobalSet(b *testing.B) {
	env, gis := setupGlobalEnv(10)
	gi := gis[5]
	val := values.NewInteger(99)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		env.SetOwnGlobalValue(gi, val)
	}
}

// BenchmarkLocalFrameCopy measures LocalEnvironmentFrame.Copy() at various frame sizes.
// This is the Phase 3 CoW target — called on every non-tail function application.
func BenchmarkLocalFrameCopy(b *testing.B) {
	for _, n := range []int{1, 5, 10, 25, 50} {
		b.Run(fmt.Sprintf("bindings=%d", n), func(b *testing.B) {
			env, _ := setupLocalEnv(n)
			local := env.LocalEnvironment()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_ = local.Copy()
			}
		})
	}
}

// BenchmarkFrameCreation measures NewEnvironmentFrameWithParent — frame allocation.
func BenchmarkFrameCreation(b *testing.B) {
	for _, n := range []int{1, 5, 10, 25, 50} {
		b.Run(fmt.Sprintf("bindings=%d", n), func(b *testing.B) {
			env, _ := setupLocalEnv(n)
			local := env.LocalEnvironment()
			parent := env.Parent()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				_ = NewEnvironmentFrameWithParent(local, parent)
			}
		})
	}
}

// BenchmarkFrameCopyAndCreate measures Copy + NewEnvironmentFrameWithParent combined.
// This simulates what happens in Apply for non-tail calls: the closure's environment
// is copied and a new frame is created with the copy.
func BenchmarkFrameCopyAndCreate(b *testing.B) {
	for _, n := range []int{1, 5, 10, 25, 50} {
		b.Run(fmt.Sprintf("bindings=%d", n), func(b *testing.B) {
			env, _ := setupLocalEnv(n)
			local := env.LocalEnvironment()
			parent := env.Parent()
			b.ResetTimer()
			for i := 0; i < b.N; i++ {
				copied := local.Copy().(*LocalEnvironmentFrame)
				_ = NewEnvironmentFrameWithParent(copied, parent)
			}
		})
	}
}
