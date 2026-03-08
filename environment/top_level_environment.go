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
	"context"
	"fmt"
	"sync"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

// LibraryEnvFactory creates a fresh environment for an R7RS library.
// The returned environment must share the caller's TopLevelEnvironment
// for syntax interning, but have isolated bindings so library definitions
// don't leak.
//
// The libraryName parameter contains the library name parts (e.g.,
// ["scheme", "base"]) so the factory can implement per-library policies.
type LibraryEnvFactory func(ctx context.Context, callerEnv *EnvironmentFrame, libraryName []string) (*EnvironmentFrame, error)

var _ values.Value = (*TopLevelEnvironment)(nil)

// TopLevelEnvironment represents a complete Wile VM instance.
// It owns per-instance syntax interning, phase registry,
// and library registry. This enables multiple independent Wile VMs in a
// single Go process.
//
// Design: TopLevelEnvironment is the root of the environment hierarchy.
// Each EnvironmentFrame holds a reference back to its TopLevelEnvironment
// to access shared resources (interning, phases, libraries).
type TopLevelEnvironment struct {
	// Name is an optional descriptive name (e.g., "interaction-environment").
	Name string

	// parent is the parent TopLevelEnvironment for interning delegation.
	// When non-nil, InternSyntax delegates to the parent,
	// ensuring syntax identity across child environments.
	parent *TopLevelEnvironment

	// syntaxInterns is the per-instance syntax object interning table.
	syntaxInterns   map[values.Value]syntax.SyntaxValue
	syntaxInternsMu sync.RWMutex

	// loadPathStack tracks files currently being loaded for relative path
	// resolution. Only exists on the root TopLevelEnvironment (nil in children).
	// Children access via LoadPathStack() which delegates to parent.
	loadPathStack *LoadPathStack

	// phases is the phase registry for O(1) access to any phase environment.
	phases *PhaseRegistry

	// libraryRegistry is the R7RS library registry.
	// Stored as any to avoid circular dependency with machine package.
	// TODO: consider defining an interface for library registries.
	libraryRegistry any

	// libraryEnvFactory creates isolated library environments during
	// R7RS library loading. Per-instance (not global) so multiple engines
	// don't race on a shared function pointer.
	libraryEnvFactory LibraryEnvFactory

	// scopeRegistry maps library scopes to their defining environment frames.
	// When a macro's free identifier carries a library scope, the compiler
	// uses this registry to redirect binding lookup to the library's env.
	// Protected by scopeRegistryMu for concurrent library compilation.
	scopeRegistry   map[*syntax.Scope]*EnvironmentFrame
	scopeRegistryMu sync.RWMutex

	// runtime is the phase 0 (runtime) environment frame.
	runtime *EnvironmentFrame
}

// NewTopLevelEnvironment creates a new TopLevelEnvironment.
// This is the primary entry point for creating an isolated Wile VM instance.
func NewTopLevelEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		syntaxInterns: make(map[values.Value]syntax.SyntaxValue),
		loadPathStack: NewLoadPathStack(),
		scopeRegistry: make(map[*syntax.Scope]*EnvironmentFrame),
	}

	// Create the runtime (phase 0) environment frame
	global := newGlobalEnvironmentFrameWithTopLevel(q)
	q.runtime = &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   q,
	}

	// Create phase registry and register runtime as phase 0
	q.phases = newPhaseRegistryWithTopLevel(q)

	// Set the phases reference on the runtime frame
	q.runtime.phases = q.phases

	return q
}

// InternSyntax returns the canonical version of the given syntax value.
// If an equivalent syntax value has been seen before, it is returned.
// Otherwise, the value is added to the intern table and returned.
//
// When a parent TopLevelEnvironment exists, interning is delegated to the
// parent to maintain syntax identity across environments.
//
// This function is thread-safe.
func (p *TopLevelEnvironment) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	// Delegate to parent if this is a child environment
	if p.parent != nil {
		return p.parent.InternSyntax(k, v)
	}

	p.syntaxInternsMu.RLock()
	val, ok := p.syntaxInterns[k]
	if ok {
		p.syntaxInternsMu.RUnlock()
		return val
	}
	p.syntaxInternsMu.RUnlock()

	p.syntaxInternsMu.Lock()
	defer p.syntaxInternsMu.Unlock()

	// Double-check after acquiring write lock
	val, ok = p.syntaxInterns[k]
	if ok {
		return val
	}

	p.syntaxInterns[k] = v
	return v
}

// Runtime returns the runtime phase environment (phase 0).
// This is the main environment where top-level bindings live.
func (p *TopLevelEnvironment) Runtime() *EnvironmentFrame {
	return p.runtime
}

// AtPhase returns the environment for the given phase level, creating it if needed.
// Phase 0 is runtime, phase 1 is expansion (for-syntax), phase 2 is compile-time, etc.
// Negative phases (e.g., -1 for for-template) are also supported.
func (p *TopLevelEnvironment) AtPhase(phase int) *EnvironmentFrame {
	return p.phases.GetOrCreate(phase)
}

// Expand returns the expand phase environment (phase 1), creating it if needed.
// This is where syntax bindings from define-syntax are stored.
func (p *TopLevelEnvironment) Expand() *EnvironmentFrame {
	return p.AtPhase(PhaseExpand)
}

// Compile returns the compile phase environment (phase 2), creating it if needed.
// This is where compile-time procedures (syntax compilers) are stored.
func (p *TopLevelEnvironment) Compile() *EnvironmentFrame {
	return p.AtPhase(PhaseCompile)
}

// Phases returns the phase registry.
func (p *TopLevelEnvironment) Phases() *PhaseRegistry {
	return p.phases
}

// LibraryRegistry returns the library registry for R7RS library loading.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set.
func (p *TopLevelEnvironment) LibraryRegistry() any {
	return p.libraryRegistry
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
// The registry should be a *machine.LibraryRegistry.
func (p *TopLevelEnvironment) SetLibraryRegistry(registry any) {
	p.libraryRegistry = registry
}

// LibraryEnvFactory returns the factory for creating library environments.
// Returns nil if no factory has been set.
func (p *TopLevelEnvironment) LibraryEnvFactory() LibraryEnvFactory {
	return p.libraryEnvFactory
}

// SetLibraryEnvFactory sets the factory for creating library environments.
func (p *TopLevelEnvironment) SetLibraryEnvFactory(f LibraryEnvFactory) {
	p.libraryEnvFactory = f
}

// LoadPathStack returns the load path stack for tracking files currently
// being loaded. Delegates to parent when non-nil, ensuring child environments
// share the same stack as the root TopLevelEnvironment.
func (p *TopLevelEnvironment) LoadPathStack() *LoadPathStack {
	if p.parent != nil {
		return p.parent.LoadPathStack()
	}
	return p.loadPathStack
}

// RegisterLibraryScope associates a library scope with its defining environment.
// This enables cross-library macro hygiene: when a symbol carries a library
// scope, the compiler can redirect binding lookup to the library's env.
//
// This function is thread-safe.
func (p *TopLevelEnvironment) RegisterLibraryScope(scope *syntax.Scope, env *EnvironmentFrame) {
	if scope == nil || env == nil {
		return
	}
	// Delegate to root if this is a child environment
	if p.parent != nil {
		p.parent.RegisterLibraryScope(scope, env)
		return
	}
	p.scopeRegistryMu.Lock()
	defer p.scopeRegistryMu.Unlock()
	p.scopeRegistry[scope] = env
}

// LookupLibraryEnv returns the environment associated with the given library
// scope, or nil if not registered. This function is thread-safe.
func (p *TopLevelEnvironment) LookupLibraryEnv(scope *syntax.Scope) *EnvironmentFrame {
	if scope == nil {
		return nil
	}
	// Delegate to root if this is a child environment
	if p.parent != nil {
		return p.parent.LookupLibraryEnv(scope)
	}
	p.scopeRegistryMu.RLock()
	defer p.scopeRegistryMu.RUnlock()
	return p.scopeRegistry[scope]
}

// NewChildTopLevelEnvironment creates a new TopLevelEnvironment whose syntax
// interning is delegated to the receiver (the parent).
//
// # Ownership structure
//
// The child is a fully independent TopLevelEnvironment with its own:
//
//   - EnvironmentFrame (runtime, phase 0) — the root lexical scope
//   - GlobalEnvironmentFrame — isolated global bindings (define, set!, etc.)
//   - PhaseRegistry — isolated phase hierarchy (expand, compile created on demand)
//
// The child's GlobalEnvironmentFrame.topLevel points to the child (not the
// parent), so new global bindings created in the child are keyed against the
// child's GlobalEnvironmentFrame. This is what provides binding isolation:
// definitions in the child do not appear in the parent, and vice versa.
//
//	Parent TopLevelEnvironment (root)
//	+-----------------------------------------------+
//	| syntaxInterns: map[Value]SyntaxValue ◄────────────── all interning
//	| syntaxInternsMu  (mutex)                      |
//	| parent: nil                                   |
//	| phases: *PhaseRegistry ──► {0: envP}          |
//	| runtime: envP ─────────────────────────────┐  |
//	| libraryRegistry: *machine.LibraryRegistry  |  |
//	+--------------------------------------------│--+
//	                                             │
//	                                             ▼
//	                         EnvironmentFrame (envP, phase 0)
//	                         +-------------------------------+
//	                         | global: *GlobalEnvFrame ───┐  |
//	                         | topLevel: ──► parent TLE   |  |
//	                         +---------------------------│---+
//	                                                     ▼
//	                                  GlobalEnvironmentFrame
//	                                  +-------------------------+
//	                                  | keys: {x:0, y:1, ...}   |
//	                                  | bindings: [...]         |
//	                                  | topLevel: ──► parent TLE|
//	                                  +-------------------------+
//
//	Child TopLevelEnvironment (returned by this method)
//	+-----------------------------------------------+
//	| syntaxInterns: nil  (never accessed)          |
//	| parent: ──► parent TLE  (interning delegate)  |
//	| phases: *PhaseRegistry ──► {0: envC}          |
//	| runtime: envC ─────────────────────────────┐  |
//	| libraryRegistry: ──► same pointer as parent|  |
//	+--------------------------------------------│--+
//	                                             │
//	                                             ▼
//	                         EnvironmentFrame (envC, phase 0)
//	                         +-------------------------------+
//	                         | global: *GlobalEnvFrame ───┐  |
//	                         | topLevel: ──► child TLE    |  |
//	                         +---------------------------│---+
//	                                                     ▼
//	                                  GlobalEnvironmentFrame
//	                                  +-------------------------+
//	                                  | keys: {}  (empty)       |
//	                                  | bindings: []            |
//	                                  | topLevel: ──► child TLE |
//	                                  +-------------------------+
//
// # Interning delegation
//
// The child stores a parent pointer and has nil interning maps. InternSyntax
// checks for a non-nil parent and delegates recursively, ultimately reaching
// the root TopLevelEnvironment where the maps and mutexes live. This avoids
// sharing map pointers across structs with independent mutexes (which would
// be a data race).
//
// # Inherited state
//
// The child inherits the parent's libraryRegistry (the *machine.LibraryRegistry)
// by value copy. This allows the child to load libraries via (import ...) without
// requiring the caller to set the registry explicitly. The registry itself is a
// shared pointer; mutations to the registry (e.g., registering a new library)
// are visible to both parent and child.
//
// # Contrast with NewChildRuntime
//
// NewChildRuntime returns an *EnvironmentFrame that shares the parent's
// TopLevelEnvironment directly (same pointer). It is used for library loading,
// where the library environment should share the same TopLevelEnvironment
// for syntax interning. However, because it shares the TopLevelEnvironment,
// it cannot be returned as a standalone environment value — calling Runtime()
// on the shared TopLevelEnvironment returns the parent's runtime frame, not
// the child's.
//
//	NewChildRuntime:                NewChildTopLevelEnvironment:
//
//	  TopLevelEnvironment (shared)    Parent TLE        Child TLE
//	  +------------------+            +----------+      +----------+
//	  | runtime: envP    |            | runtime: |      | runtime: |
//	  +------------------+            | envP     |      | envC     |
//	          │                       +----------+      +----------+
//	          │                                            │
//	     ┌────┴────┐                                       ▼
//	     ▼         ▼                           EnvironmentFrame (envC)
//	   envP      envC ◄── new child            +---------------------+
//	   (parent   (has own Global-              | topLevel: child TLE |
//	    frame)    EnvFrame, but                +---------------------+
//	              topLevel points
//	              to shared TLE)
//
//	envC.TopLevelEnv() == parent    envC.TopLevelEnv() == child
//	TLE.Runtime() returns envP     child.Runtime() returns envC  ✓
//
// NewChildTopLevelEnvironment returns a new *TopLevelEnvironment that can be
// passed as a first-class Scheme value (e.g., returned from the (environment)
// primitive and accepted by eval). Its Runtime() returns the child's own
// runtime frame, and its AtPhase/Expand/Compile methods create phase
// environments scoped to the child.
//
// # Usage
//
// Used by PrimEnvironment and PrimNullEnvironment (R7RS §6.12) to create
// environments that are identity-compatible with the caller's symbol table
// while providing isolated bindings.
// TODO: review whether libraryRegistry should be copied here
// TODO: review for optimization/refactoring opportunities
func (p *TopLevelEnvironment) NewChildTopLevelEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		parent:            p,
	}

	// Create the runtime (phase 0) environment frame
	global := newGlobalEnvironmentFrameWithTopLevel(q)
	q.runtime = &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   q,
	}

	// Create phase registry and register runtime as phase 0
	q.phases = newPhaseRegistryWithTopLevel(q)

	// Set the phases reference on the runtime frame
	q.runtime.phases = q.phases

	return q
}

// NewSchemeReportEnvironment creates a new TopLevelEnvironment that is
// distinct from the receiver (so eq? returns #f) but contains a snapshot
// of the receiver's current global bindings at the time of the call.
//
// This implements R7RS §6.12 scheme-report-environment semantics: the returned
// environment is a separate object from interaction-environment and contains
// the standard bindings. User definitions added after this call are NOT
// visible in the returned environment.
func (p *TopLevelEnvironment) NewSchemeReportEnvironment() *TopLevelEnvironment {
	q := &TopLevelEnvironment{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		parent:            p,
	}

	// Copy the parent's global bindings and repoint topLevel to the child,
	// so that syntax interning delegates through q → p (parent chain).
	copiedGlobal := p.runtime.global.Copy().(*GlobalEnvironmentFrame)
	copiedGlobal.topLevel = q

	q.runtime = &EnvironmentFrame{
		parent:     nil,
		global:     copiedGlobal,
		phaseLevel: PhaseRuntime,
		topLevel:   q,
	}

	q.phases = newPhaseRegistryWithTopLevel(q)
	q.runtime.phases = q.phases

	return q
}

// NewChildRuntime creates a new runtime environment frame that shares this
// TopLevelEnvironment for syntax interning, but has its own
// GlobalEnvironmentFrame and PhaseRegistry for isolated bindings.
//
// This is used for library environments that need to:
//   - Share syntax interning
//   - Have isolated bindings (library definitions don't leak)
//   - Have their own phase hierarchy
func (p *TopLevelEnvironment) NewChildRuntime() *EnvironmentFrame {
	// Create a new global frame sharing this TopLevelEnvironment
	global := newGlobalEnvironmentFrameWithTopLevel(p)

	// Create the runtime frame for the child
	runtime := &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		topLevel:   p, // Share the TopLevelEnvironment
	}

	// Create a new phase registry for the child
	childPhases := &PhaseRegistry{
		envs:  make(map[int]*EnvironmentFrame),
		owner: p,
	}
	childPhases.envs[PhaseRuntime] = runtime
	runtime.phases = childPhases

	return runtime
}

// SyntaxInternCount returns the number of interned syntax objects.
// This is intended for testing and debugging purposes.
func (p *TopLevelEnvironment) SyntaxInternCount() int {
	p.syntaxInternsMu.RLock()
	defer p.syntaxInternsMu.RUnlock()
	return len(p.syntaxInterns)
}

// IsVoid returns true if the environment is nil.
func (p *TopLevelEnvironment) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the environments are the same object.
func (p *TopLevelEnvironment) EqualTo(v values.Value) bool {
	other, ok := v.(*TopLevelEnvironment)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the environment.
func (p *TopLevelEnvironment) SchemeString() string {
	if p.Name != "" {
		return fmt.Sprintf("#<environment %s>", p.Name)
	}
	return "#<environment>"
}

// newGlobalEnvironmentFrameWithTopLevel creates a new GlobalEnvironmentFrame
// that references the given TopLevelEnvironment.
func newGlobalEnvironmentFrameWithTopLevel(topLevel *TopLevelEnvironment) *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
		topLevel: topLevel,
	}
	return q
}

// newPhaseRegistryWithTopLevel creates a new PhaseRegistry owned by the given TopLevelEnvironment.
func newPhaseRegistryWithTopLevel(topLevel *TopLevelEnvironment) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[int]*EnvironmentFrame),
		owner: topLevel,
	}
	// TopLevel is phase 0 (runtime)
	q.envs[PhaseRuntime] = topLevel.runtime
	return q
}
