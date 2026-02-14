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
	"sync"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
)

func TestNewTopLevelEnvironment(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()

	c.Assert(topLevel, qt.Not(qt.IsNil))
	c.Assert(topLevel.Runtime(), qt.Not(qt.IsNil))
	c.Assert(topLevel.Phases(), qt.Not(qt.IsNil))
	c.Assert(topLevel.Runtime().TopLevelEnv(), qt.Equals, topLevel)
}

func TestTopLevelEnvironment_InternSymbol(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()

	// Intern a symbol
	sym1 := values.NewSymbol("foo")
	interned1 := topLevel.InternSymbol(sym1)

	c.Assert(interned1, qt.Not(qt.IsNil))
	c.Assert(interned1.Datum(), qt.Equals, "foo")

	// Intern the same symbol again
	sym2 := values.NewSymbol("foo")
	interned2 := topLevel.InternSymbol(sym2)

	// Should return the same pointer (identity)
	c.Assert(interned2, qt.Equals, interned1)
}

func TestTopLevelEnvironment_SymbolIsolation(t *testing.T) {
	c := qt.New(t)

	// Create two independent TopLevelEnvironments
	topLevel1 := NewTopLevelEnvironment()
	topLevel2 := NewTopLevelEnvironment()

	// Intern the same symbol name in both
	sym1 := topLevel1.InternSymbol(values.NewSymbol("isolated"))
	sym2 := topLevel2.InternSymbol(values.NewSymbol("isolated"))

	// The symbols should have the same name
	c.Assert(sym1.Datum(), qt.Equals, sym2.Datum())

	// But they should NOT be the same pointer (different VMs)
	c.Assert(sym1 != sym2, qt.IsTrue, qt.Commentf("symbols from different TopLevelEnvironments should not be identical"))

	// Within each VM, interning should return the same pointer
	sym1Again := topLevel1.InternSymbol(values.NewSymbol("isolated"))
	c.Assert(sym1Again, qt.Equals, sym1)

	sym2Again := topLevel2.InternSymbol(values.NewSymbol("isolated"))
	c.Assert(sym2Again, qt.Equals, sym2)
}

func TestTopLevelEnvironment_SymbolInternCount(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()

	c.Assert(topLevel.SymbolInternCount(), qt.Equals, 0)

	topLevel.InternSymbol(values.NewSymbol("a"))
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, 1)

	topLevel.InternSymbol(values.NewSymbol("b"))
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, 2)

	// Interning the same symbol again shouldn't increase the count
	topLevel.InternSymbol(values.NewSymbol("a"))
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, 2)
}

func TestInternSymbol_Stress(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	const count = 10000

	// Intern 10,000 unique symbols
	symbols := make([]*values.Symbol, count)
	for i := 0; i < count; i++ {
		sym := values.NewSymbol("stress-sym-" + string(rune(i/256+1)) + string(rune(i%256+1)))
		symbols[i] = topLevel.InternSymbol(sym)
	}
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, count)

	// Re-interning all returns same pointers
	for i := 0; i < count; i++ {
		again := topLevel.InternSymbol(values.NewSymbol(symbols[i].Datum()))
		c.Assert(again, qt.Equals, symbols[i],
			qt.Commentf("re-intern of symbol %d should return same pointer", i))
	}
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, count,
		qt.Commentf("count should not increase after re-interning"))
}

func TestInternSymbol_Concurrent(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	const goroutines = 10
	const perGoroutine = 1000

	// 100 shared names + 900 unique per goroutine = 100 + 9000 = 9100 total
	var wg sync.WaitGroup
	results := make([][perGoroutine]*values.Symbol, goroutines)

	for g := 0; g < goroutines; g++ {
		wg.Add(1)
		go func(gIdx int) {
			defer wg.Done()
			for i := 0; i < perGoroutine; i++ {
				var name string
				if i < 100 {
					// Shared names across all goroutines
					name = "shared-" + string(rune(i+1))
				} else {
					// Unique per goroutine
					name = "g" + string(rune(gIdx+1)) + "-" + string(rune(i+1))
				}
				results[gIdx][i] = topLevel.InternSymbol(values.NewSymbol(name))
			}
		}(g)
	}
	wg.Wait()

	// Shared symbols: all goroutines should get the same pointer
	for i := 0; i < 100; i++ {
		for g := 1; g < goroutines; g++ {
			c.Assert(results[g][i], qt.Equals, results[0][i],
				qt.Commentf("shared symbol %d should be identical across goroutines", i))
		}
	}

	// Total count: 100 shared + 900*10 unique = 9100
	c.Assert(topLevel.SymbolInternCount(), qt.Equals, 9100)
}

func TestTopLevelEnvironment_Phases(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()

	// Runtime is phase 0
	runtime := topLevel.Runtime()
	c.Assert(runtime.PhaseLevel(), qt.Equals, PhaseRuntime)

	// AtPhase(0) should return the same as Runtime()
	c.Assert(topLevel.AtPhase(PhaseRuntime), qt.Equals, runtime)

	// Expand is phase 1
	expand := topLevel.Expand()
	c.Assert(expand.PhaseLevel(), qt.Equals, PhaseExpand)
	c.Assert(topLevel.AtPhase(PhaseExpand), qt.Equals, expand)

	// Compile is phase 2
	compile := topLevel.Compile()
	c.Assert(compile.PhaseLevel(), qt.Equals, PhaseCompile)
	c.Assert(topLevel.AtPhase(PhaseCompile), qt.Equals, compile)

	// All phases should share the same TopLevelEnvironment
	c.Assert(runtime.TopLevelEnv(), qt.Equals, topLevel)
	c.Assert(expand.TopLevelEnv(), qt.Equals, topLevel)
	c.Assert(compile.TopLevelEnv(), qt.Equals, topLevel)
}

func TestTopLevelEnvironment_LibraryRegistry(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()

	// Initially nil
	c.Assert(topLevel.LibraryRegistry(), qt.IsNil)

	// Set a registry (using a placeholder)
	placeholder := "test-registry"
	topLevel.SetLibraryRegistry(placeholder)
	c.Assert(topLevel.LibraryRegistry(), qt.Equals, placeholder)

	// Runtime frame should also see it via delegation
	c.Assert(topLevel.Runtime().LibraryRegistry(), qt.Equals, placeholder)
}

func TestEnvironmentFrame_InternSymbol_DelegatesToTopLevel(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	env := topLevel.Runtime()

	// Intern via environment frame
	sym1 := env.InternSymbol(values.NewSymbol("delegate"))

	// Intern directly via TopLevelEnvironment
	sym2 := topLevel.InternSymbol(values.NewSymbol("delegate"))

	// Should be the same pointer
	c.Assert(sym1, qt.Equals, sym2)
}

func TestNewEnvironmentFrameWithParent_InheritsTopLevel(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	parent := topLevel.Runtime()

	// Create a child environment
	local := NewLocalEnvironment(1)
	child := NewEnvironmentFrameWithParent(local, parent)

	// Child should inherit topLevel
	c.Assert(child.TopLevelEnv(), qt.Equals, topLevel)

	// Symbol interning should work through child
	sym1 := child.InternSymbol(values.NewSymbol("inherited"))
	sym2 := topLevel.InternSymbol(values.NewSymbol("inherited"))
	c.Assert(sym1, qt.Equals, sym2)
}

func TestTopLevelEnvironment_ChildSharesLoadPathStack(t *testing.T) {
	c := qt.New(t)

	parent := NewTopLevelEnvironment()
	child := parent.NewChildTopLevelEnvironment()

	// Verify both have non-nil stacks
	c.Assert(parent.LoadPathStack(), qt.Not(qt.IsNil))
	c.Assert(child.LoadPathStack(), qt.Not(qt.IsNil))

	// Verify they share the same stack (per-VM design)
	c.Assert(child.LoadPathStack(), qt.Equals, parent.LoadPathStack())

	// Push to parent, verify child sees it
	c.Assert(parent.LoadPathStack().Push("/parent/file.scm"), qt.IsNil)
	c.Assert(child.LoadPathStack().Current(), qt.Equals, "/parent/file.scm")
	c.Assert(child.LoadPathStack().Depth(), qt.Equals, 1)

	// Push to child, verify parent sees it
	c.Assert(child.LoadPathStack().Push("/child/file.scm"), qt.IsNil)
	c.Assert(parent.LoadPathStack().Current(), qt.Equals, "/child/file.scm")
	c.Assert(parent.LoadPathStack().Depth(), qt.Equals, 2)

	// Pop from child, verify parent sees it
	child.LoadPathStack().Pop()
	c.Assert(parent.LoadPathStack().Current(), qt.Equals, "/parent/file.scm")
	c.Assert(parent.LoadPathStack().Depth(), qt.Equals, 1)

	// Verify CurrentDir works through delegation
	c.Assert(child.LoadPathStack().CurrentDir(), qt.Equals, "/parent")
}

func TestTopLevelEnvironment_NestedChildSharesLoadPathStack(t *testing.T) {
	c := qt.New(t)

	root := NewTopLevelEnvironment()
	child1 := root.NewChildTopLevelEnvironment()
	child2 := child1.NewChildTopLevelEnvironment()

	// All three should share the same stack
	c.Assert(child1.LoadPathStack(), qt.Equals, root.LoadPathStack())
	c.Assert(child2.LoadPathStack(), qt.Equals, root.LoadPathStack())

	// Push to deepest child
	c.Assert(child2.LoadPathStack().Push("/deep/file.scm"), qt.IsNil)

	// All should see it
	c.Assert(root.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
	c.Assert(child1.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
	c.Assert(child2.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
}
