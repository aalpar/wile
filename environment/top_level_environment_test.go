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

func TestNewEnvironmentFrameWithParent_InheritsTopLevel(t *testing.T) {
	c := qt.New(t)

	topLevel := NewTopLevelEnvironment()
	parent := topLevel.Runtime()

	// Create a child environment
	local := NewLocalEnvironment(1)
	child := NewEnvironmentFrameWithParent(local, parent)

	// Child should inherit topLevel
	c.Assert(child.TopLevelEnv(), qt.Equals, topLevel)
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

func TestConstructorEquivalence(t *testing.T) {
	c := qt.New(t)

	parent := NewTopLevelEnvironment()
	childRuntime := parent.NewChildRuntime()

	c.Assert(childRuntime.TopLevelEnv(), qt.Equals, parent)
	c.Assert(childRuntime.PhaseLevel(), qt.Equals, PhaseRuntime)
	c.Assert(childRuntime.GlobalEnvironment(), qt.IsNotNil)
	c.Assert(childRuntime.IsTopLevel(), qt.IsTrue)

	expand := childRuntime.Expand()
	c.Assert(expand, qt.IsNotNil)
	c.Assert(expand.PhaseLevel(), qt.Equals, PhaseExpand)

	child := parent.NewChildTopLevelEnvironment()
	c.Assert(child.Runtime().TopLevelEnv(), qt.Equals, child)
	c.Assert(child.Expand().PhaseLevel(), qt.Equals, PhaseExpand)

	sym := values.NewSymbol("test-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	report := parent.NewSchemeReportEnvironment()
	c.Assert(report.Runtime().GetGlobalIndex(sym), qt.IsNotNil)

	sym2 := values.NewSymbol("after-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym2, BindingTypeVariable)
	c.Assert(report.Runtime().GetGlobalIndex(sym2), qt.IsNil)
}

// Verify that symbols with the same name are structurally equal
// even when created independently (no interning needed).
func TestTopLevelEnvironment_SymbolEquality(t *testing.T) {
	c := qt.New(t)

	sym1 := values.NewSymbol("foo")
	sym2 := values.NewSymbol("foo")

	c.Assert(sym1.EqualTo(sym2), qt.IsTrue)
	c.Assert(sym1.Key, qt.Equals, sym2.Key)
}
