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

func (p *testPathTracker) Push(filePath string) {
	p.paths = append(p.paths, filePath)
}

func (p *testPathTracker) Pop() {
	if len(p.paths) > 0 {
		p.paths = p.paths[:len(p.paths)-1]
	}
}

func (p *testPathTracker) Current() string {
	if len(p.paths) == 0 {
		return ""
	}
	return p.paths[len(p.paths)-1]
}

func (p *testPathTracker) CurrentDir() string {
	c := p.Current()
	if c == "" {
		return ""
	}
	return path.Dir(c)
}

func (p *testPathTracker) Depth() int {
	return len(p.paths)
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

// TestNamespace_ChildRuntimePhasesNotFoldable pins the invariant that an
// EnvironmentFrame's phases registry is NOT derivable from its Namespace.
//
// A child runtime (NewChildRuntime, used for library loading) deliberately
// SHARES the parent Namespace — for syntax interning — while holding its OWN
// PhaseRegistry, so a library's phase environments stay isolated from the
// importer. Hence child.phases != child.namespace.phases, and AtPhase must read
// the per-frame phases field rather than namespace.phases.
//
// This guards against re-attempting the "phases is redundant with
// namespace.phases" fold sketched in
// memory/2026-06-02-environment-frame-hot-cold-layout.md (Phase 1/2). That fold
// is unsafe precisely because of this divergence: deriving phases from the
// Namespace would route a library's phase environments into the importer's
// registry and collapse the isolation NewChildRuntime exists to provide.
func TestNamespace_ChildRuntimePhasesNotFoldable(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	child := parent.NewChildRuntime()

	// The child shares the parent Namespace...
	c.Assert(child.Namespace(), qt.Equals, parent)
	// ...but holds its OWN phase registry, distinct from the Namespace's.
	c.Assert(child.phases, qt.Not(qt.Equals), child.namespace.phases)
	c.Assert(child.namespace.phases, qt.Equals, parent.phases)

	// Consequence: phase access via the frame resolves to the child's own
	// registry, NOT the parent's.
	c.Assert(child.AtPhase(PhaseExpand), qt.Not(qt.Equals), parent.AtPhase(PhaseExpand))
	c.Assert(child.AtPhase(PhaseExpand), qt.Equals, child.phases.GetOrCreate(PhaseExpand))

	// By contrast, a Namespace's own runtime frame DOES satisfy the equality —
	// which is what made the (incorrect) redundancy claim look plausible.
	rt := parent.Runtime()
	c.Assert(rt.phases, qt.Equals, rt.namespace.phases)
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

// TestNamespace_ChildSetLoadPathStackPropagatesToRoot is a regression
// test for the Phase 5 fix: pre-fix, SetLoadPathStack(s) on a child
// silently set the child's local field while LoadPathStack() delegated
// to the root, so the write was effectively dropped (a future read
// from the root or any sibling would not see it). Both sides now go
// through root() and a write on any namespace must be observable from
// every other namespace in the same tree.
func TestNamespace_ChildSetLoadPathStackPropagatesToRoot(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	child := parent.NewChildNamespace()
	grandchild := child.NewChildNamespace()

	// Setting on the deepest namespace must be observable everywhere.
	tracker := &testPathTracker{}
	grandchild.SetLoadPathStack(tracker)

	c.Assert(parent.LoadPathStack(), qt.Equals, tracker)
	c.Assert(child.LoadPathStack(), qt.Equals, tracker)
	c.Assert(grandchild.LoadPathStack(), qt.Equals, tracker)
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

func TestNamespace_ExportIndex(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()
	idx, built := ns.ExportIndex()
	c.Assert(idx, qt.IsNil)
	c.Assert(built, qt.IsFalse)

	ns.SetExportIndex("mock-export-index")
	idx, built = ns.ExportIndex()
	c.Assert(idx, qt.Equals, "mock-export-index")
	c.Assert(built, qt.IsTrue)
}

func TestNamespace_ExportIndex_DelegatesToParent(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetExportIndex("parent-export-index")

	child := parent.NewChildNamespace()
	idx, built := child.ExportIndex()
	c.Assert(idx, qt.Equals, "parent-export-index")
	c.Assert(built, qt.IsTrue)

	// Setting on child delegates to parent.
	child.SetExportIndex("updated-export-index")
	idx, _ = parent.ExportIndex()
	c.Assert(idx, qt.Equals, "updated-export-index")
	idx, _ = child.ExportIndex()
	c.Assert(idx, qt.Equals, "updated-export-index")
}

func TestNamespace_ExportIndex_NilStopsRetry(t *testing.T) {
	c := qt.New(t)

	ns := NewNamespace()
	// Storing nil marks as built — prevents redundant retry.
	ns.SetExportIndex(nil)
	idx, built := ns.ExportIndex()
	c.Assert(idx, qt.IsNil)
	c.Assert(built, qt.IsTrue)
}

func TestNamespace_NewChildNamespace_SharesRegistryAndAuthorizer(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	auth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(auth)

	child := parent.NewChildNamespace()

	// Same pointer — immutable registry and authorizer are shared
	c.Assert(child.Registry(), qt.Equals, parent.Registry())
	c.Assert(child.Authorizer(), qt.Equals, parent.Authorizer())
	c.Assert(child, qt.Not(qt.Equals), parent)
}

func TestNamespace_NewChildNamespace_OverrideRegistry(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	auth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(auth)

	child := parent.NewChildNamespace(WithChildRegistry("restricted-registry"))

	c.Assert(child.Registry(), qt.Equals, "restricted-registry")
	// Authorizer inherited when not overridden
	c.Assert(child.Authorizer(), qt.Equals, parent.Authorizer())
}

func TestNamespace_NewChildNamespace_OverrideAuthorizer(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetRegistry("parent-registry")
	parentAuth := &testAuthorizer{name: "parent-authorizer"}
	parent.SetAuthorizer(parentAuth)

	childAuth := &testAuthorizer{name: "child-authorizer"}
	child := parent.NewChildNamespace(WithChildAuthorizer(childAuth))

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

func TestNamespace_NewChildNamespace_IsolatesModuleInstances(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetModuleInstance("(scheme base)", &ModuleInstance{})

	child := parent.NewChildNamespace()

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

// TestNamespace_NewChildNamespace_InheritsEnvMap verifies that a child
// namespace inherits the parent's virtual env-var map. envMap is capability
// state — derived namespaces must not silently widen capability by reverting
// to nil (which would fall through to os.Getenv in the envvars primitives).
func TestNamespace_NewChildNamespace_InheritsEnvMap(t *testing.T) {
	c := qt.New(t)

	tests := []struct {
		name   string
		envMap map[string]string
		expect map[string]string
	}{
		{
			name:   "non-nil map propagates",
			envMap: map[string]string{"K": "V"},
			expect: map[string]string{"K": "V"},
		},
		{
			name:   "empty map propagates as empty (not nil)",
			envMap: map[string]string{},
			expect: map[string]string{},
		},
		{
			name:   "nil parent map yields nil child map",
			envMap: nil,
			expect: nil,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			parent := NewNamespace()
			parent.SetEnvMap(tc.envMap)

			child := parent.NewChildNamespace()
			c.Assert(child.EnvMap(), qt.DeepEquals, tc.expect)
		})
	}
}

// TestNamespace_NewSchemeReportNamespace_InheritsEnvMap mirrors the
// NewChildNamespace contract for the R7RS scheme-report-environment path.
// Same justification: capability state inherits.
func TestNamespace_NewSchemeReportNamespace_InheritsEnvMap(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetEnvMap(map[string]string{"X": "1"})

	child := parent.NewSchemeReportNamespace()
	c.Assert(child.EnvMap(), qt.DeepEquals, map[string]string{"X": "1"})
}

// TestNamespace_ChildEnvMapIsolatedFromParentMutation verifies that calling
// SetEnvMap on a child after creation does not retroactively affect the
// parent. SetEnvMap reassigns the field (defensively copies), so the shared
// reference at construction time is broken once either side mutates.
func TestNamespace_ChildEnvMapIsolatedFromParentMutation(t *testing.T) {
	c := qt.New(t)

	parent := NewNamespace()
	parent.SetEnvMap(map[string]string{"P": "parent"})

	child := parent.NewChildNamespace()
	child.SetEnvMap(map[string]string{"C": "child"})

	c.Assert(parent.EnvMap(), qt.DeepEquals, map[string]string{"P": "parent"})
	c.Assert(child.EnvMap(), qt.DeepEquals, map[string]string{"C": "child"})
}

// TestNamespace_ImmutableLiteralsSharedViaParent verifies that the
// engine-scoped immutable-literal set lives only on the root Namespace and is
// shared by children through root() delegation. A literal compiled under any
// child must be checkable by any mutator anywhere in the tree.
func TestNamespace_ImmutableLiteralsSharedViaParent(t *testing.T) {
	root := NewNamespace()

	if root.ImmutableLiterals() == nil {
		t.Fatalf("root must own a non-nil ImmutableLiterals")
	}

	child := root.NewChildNamespace()
	if child.ImmutableLiterals() != root.ImmutableLiterals() {
		t.Fatalf("child must delegate ImmutableLiterals to root")
	}

	report := root.NewSchemeReportNamespace()
	if report.ImmutableLiterals() != root.ImmutableLiterals() {
		t.Fatalf("scheme-report child must delegate ImmutableLiterals to root")
	}
}
