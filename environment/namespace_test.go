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
	"path"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
)

// testPathTracker is a minimal PathTracker for testing, avoiding an import of
// machine/compilation/sourceload (which would create a circular dependency).
type testPathTracker struct {
	paths []string
}

func (t *testPathTracker) Push(p string) {
	t.paths = append(t.paths, p)
}

func (t *testPathTracker) Pop() {
	if len(t.paths) > 0 {
		t.paths = t.paths[:len(t.paths)-1]
	}
}

func (t *testPathTracker) Current() string {
	if len(t.paths) == 0 {
		return ""
	}
	return t.paths[len(t.paths)-1]
}

func (t *testPathTracker) CurrentDir() string {
	c := t.Current()
	if c == "" {
		return ""
	}
	return path.Dir(c)
}

func (t *testPathTracker) Depth() int {
	return len(t.paths)
}

// testAuthorizer is a no-op security.Authorizer for testing.
type testAuthorizer struct{ name string }

func (p *testAuthorizer) Authorize(_ security.AccessRequest) error {
	return nil
}

// testLibrarySearcher is a minimal LibrarySearcher for testing.
type testLibrarySearcher struct{ paths []string }

func (p *testLibrarySearcher) GetSearchPaths() []string {
	return p.paths
}

func TestNewNamespace(t *testing.T) {
	c := qt.New(t)

	topLevel := NewNamespace()

	c.Assert(topLevel, qt.Not(qt.IsNil))
	c.Assert(topLevel.Runtime(), qt.Not(qt.IsNil))
	c.Assert(topLevel.Phases(), qt.Not(qt.IsNil))
	c.Assert(topLevel.Runtime().Namespace(), qt.Equals, topLevel)
}

func TestNamespace_Phases(t *testing.T) {
	c := qt.New(t)

	topLevel := NewNamespace()

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

	// All phases should share the same Namespace
	c.Assert(runtime.Namespace(), qt.Equals, topLevel)
	c.Assert(expand.Namespace(), qt.Equals, topLevel)
	c.Assert(compile.Namespace(), qt.Equals, topLevel)
}

func TestNamespace_LibraryRegistry(t *testing.T) {
	c := qt.New(t)

	topLevel := NewNamespace()

	// Initially nil
	c.Assert(topLevel.LibraryRegistry(), qt.IsNil)

	// Set a registry
	reg := &testLibrarySearcher{paths: []string{"/usr/local/lib"}}
	topLevel.SetLibraryRegistry(reg)
	c.Assert(topLevel.LibraryRegistry(), qt.Equals, reg)

	// Runtime frame should also see it via delegation
	c.Assert(topLevel.Runtime().LibraryRegistry(), qt.Equals, reg)
}

func TestNewEnvironmentFrameWithParent_InheritsTopLevel(t *testing.T) {
	c := qt.New(t)

	topLevel := NewNamespace()
	parent := topLevel.Runtime()

	// Create a child environment
	local := NewLocalEnvironment(1)
	child := NewEnvironmentFrameWithParent(local, parent)

	// Child should inherit topLevel
	c.Assert(child.Namespace(), qt.Equals, topLevel)
}

func TestNamespace_ChildSharesLoadPathStack(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetLoadPathStack(&testPathTracker{})
	child := parent.NewChildNamespace()

	// Verify both have non-nil stacks
	c.Assert(parent.LoadPathStack(), qt.Not(qt.IsNil))
	c.Assert(child.LoadPathStack(), qt.Not(qt.IsNil))

	// Verify they share the same stack (per-VM design)
	c.Assert(child.LoadPathStack(), qt.Equals, parent.LoadPathStack())

	// Push to parent, verify child sees it
	parent.LoadPathStack().Push("/parent/file.scm")
	c.Assert(child.LoadPathStack().Current(), qt.Equals, "/parent/file.scm")
	c.Assert(child.LoadPathStack().Depth(), qt.Equals, 1)

	// Push to child, verify parent sees it
	child.LoadPathStack().Push("/child/file.scm")
	c.Assert(parent.LoadPathStack().Current(), qt.Equals, "/child/file.scm")
	c.Assert(parent.LoadPathStack().Depth(), qt.Equals, 2)

	// Pop from child, verify parent sees it
	child.LoadPathStack().Pop()
	c.Assert(parent.LoadPathStack().Current(), qt.Equals, "/parent/file.scm")
	c.Assert(parent.LoadPathStack().Depth(), qt.Equals, 1)

	// Verify CurrentDir works through delegation
	c.Assert(child.LoadPathStack().CurrentDir(), qt.Equals, "/parent")
}

func TestNamespace_NestedChildSharesLoadPathStack(t *testing.T) {
	c := qt.New(t)

	root := NewNamespace()
	root.SetLoadPathStack(&testPathTracker{})
	child1 := root.NewChildNamespace()
	child2 := child1.NewChildNamespace()

	// All three should share the same stack
	c.Assert(child1.LoadPathStack(), qt.Equals, root.LoadPathStack())
	c.Assert(child2.LoadPathStack(), qt.Equals, root.LoadPathStack())

	// Push to deepest child
	child2.LoadPathStack().Push("/deep/file.scm")

	// All should see it
	c.Assert(root.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
	c.Assert(child1.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
	c.Assert(child2.LoadPathStack().Current(), qt.Equals, "/deep/file.scm")
}

func TestConstructorEquivalence(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	childRuntime := parent.NewChildRuntime()

	c.Assert(childRuntime.Namespace(), qt.Equals, parent)
	c.Assert(childRuntime.PhaseLevel(), qt.Equals, PhaseRuntime)
	c.Assert(childRuntime.GlobalEnvironment(), qt.IsNotNil)
	c.Assert(childRuntime.IsTopLevel(), qt.IsTrue)

	expand := childRuntime.Expand()
	c.Assert(expand, qt.IsNotNil)
	c.Assert(expand.PhaseLevel(), qt.Equals, PhaseExpand)

	child := parent.NewChildNamespace()
	c.Assert(child.Runtime().Namespace(), qt.Equals, child)
	c.Assert(child.Expand().PhaseLevel(), qt.Equals, PhaseExpand)

	sym := values.NewSymbol("test-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym, BindingTypeVariable)
	report := parent.NewSchemeReportNamespace()
	c.Assert(report.Runtime().GetGlobalIndex(sym), qt.IsNotNil)

	sym2 := values.NewSymbol("after-snap")
	parent.Runtime().MaybeCreateOwnGlobalBinding(sym2, BindingTypeVariable)
	c.Assert(report.Runtime().GetGlobalIndex(sym2), qt.IsNil)
}

func TestNamespace_RegistryField(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()
	c.Assert(ns.Registry(), qt.IsNil)

	ns.SetRegistry("test-registry")
	c.Assert(ns.Registry(), qt.Equals, "test-registry")
}

func TestNamespace_AuthorizerField(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()
	c.Assert(ns.Authorizer(), qt.IsNil)

	auth := &testAuthorizer{name: "test-authorizer"}
	ns.SetAuthorizer(auth)
	c.Assert(ns.Authorizer(), qt.Equals, auth)
}

func TestNamespace_Derive_SharesRegistryAndAuthorizer(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	auth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(auth)

	child := parent.Derive()

	// Same pointer — immutable registry and authorizer are shared
	c.Assert(child.Registry(), qt.Equals, parent.Registry())
	c.Assert(child.Authorizer(), qt.Equals, parent.Authorizer())
	c.Assert(child, qt.Not(qt.Equals), parent)
}

func TestNamespace_DeriveWith_OverrideRegistry(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	auth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(auth)

	child := parent.DeriveWith(func(cfg *NamespaceDeriveConfig) {
		cfg.Registry = "restricted-registry"
	})

	c.Assert(child.Registry(), qt.Equals, "restricted-registry")
	// Authorizer inherited when not overridden
	c.Assert(child.Authorizer(), qt.Equals, parent.Authorizer())
}

func TestNamespace_DeriveWith_OverrideAuthorizer(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	parentAuth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(parentAuth)

	childAuth := &testAuthorizer{name: "child-authorizer"}
	child := parent.DeriveWith(func(cfg *NamespaceDeriveConfig) {
		cfg.Authorizer = childAuth
	})

	// Registry inherited when not overridden
	c.Assert(child.Registry(), qt.Equals, parent.Registry())
	c.Assert(child.Authorizer(), qt.Equals, childAuth)
}

func TestNamespace_ModuleInstances(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()

	// No instance initially
	_, ok := ns.ModuleInstance("(scheme base)")
	c.Assert(ok, qt.IsFalse)

	// Register an instance
	inst := &ModuleInstance{
		Exports: make(map[string]*GlobalIndex),
	}
	ns.SetModuleInstance("(scheme base)", inst)

	got, ok := ns.ModuleInstance("(scheme base)")
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, qt.Equals, inst)
}

func TestNamespace_Derive_IsolatesModuleInstances(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetModuleInstance("(scheme base)", &ModuleInstance{})

	child := parent.Derive()

	// Derived namespace should not inherit module instances
	_, ok := child.ModuleInstance("(scheme base)")
	c.Assert(ok, qt.IsFalse)
}

func TestNamespace_AttachModule(t *testing.T) {
	c := qt.New(t)

	source := NewNamespace()
	inst := &ModuleInstance{}
	source.SetModuleInstance("(scheme write)", inst)

	target := NewNamespace()
	err := source.AttachModule("(scheme write)", target)
	c.Assert(err, qt.IsNil)

	got, ok := target.ModuleInstance("(scheme write)")
	c.Assert(ok, qt.IsTrue)
	c.Assert(got, qt.Equals, inst)

	// Attaching a non-existent module should error
	err = source.AttachModule("(scheme nonexistent)", target)
	c.Assert(err, qt.IsNotNil)
}

// Verify that symbols with the same name are structurally equal
// even when created independently (no interning needed).
func TestNamespace_SymbolEquality(t *testing.T) {
	c := qt.New(t)

	sym1 := values.NewSymbol("foo")
	sym2 := values.NewSymbol("foo")

	c.Assert(sym1.EqualTo(sym2), qt.IsTrue)
	c.Assert(sym1.Key, qt.Equals, sym2.Key)
}
