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
	"github.com/aalpar/wile/security"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// LibraryEnvFactory creates a fresh environment for an R7RS library.
// The returned environment must share the caller's Namespace
// for syntax interning, but have isolated bindings so library definitions
// don't leak.
//
// The libraryName parameter contains the library name parts (e.g.,
// ["scheme", "base"]) so the factory can implement per-library policies.
type LibraryEnvFactory func(ctx context.Context, callerEnv *EnvironmentFrame, libraryName []string) (*EnvironmentFrame, error)

var _ values.Value = (*Namespace)(nil)

// Namespace represents a complete Wile VM instance.
// It owns per-instance syntax interning, phase registry,
// and library registry. This enables multiple independent Wile VMs in a
// single Go process.
//
// Design: Namespace is the root of the environment hierarchy.
// Each EnvironmentFrame holds a reference back to its Namespace
// to access shared resources (syntax interning, phases, libraries).
type Namespace struct {
	// Name is an optional descriptive name (e.g., "interaction-environment").
	Name string

	// parent is the parent Namespace for interning delegation.
	// When non-nil, InternSyntax delegates to the parent,
	// ensuring syntax identity across child environments.
	parent *Namespace

	// syntaxInterns is the per-instance syntax object interning table.
	syntaxInterns   map[values.Value]syntax.SyntaxValue
	syntaxInternsMu sync.RWMutex

	// loadPathStack tracks files currently being loaded for relative path
	// resolution. Only exists on the root Namespace (nil in children).
	// Children access via LoadPathStack() which delegates to parent.
	loadPathStack PathTracker

	// phases is the phase registry for O(1) access to any phase environment.
	phases *PhaseRegistry

	// fileResolver resolves and opens files for include/load operations.
	// The concrete implementations live in machine/compilation/;
	// the interface is defined in this package to avoid a circular import.
	fileResolver FileResolver

	// libraryRegistry is the R7RS library registry.
	// Stored as LibrarySearcher (the minimum interface environment/ needs)
	// to avoid a circular import with machine/compilation/.
	// Callers needing the full *compilation.LibraryRegistry can type-assert.
	libraryRegistry LibrarySearcher

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

	// registry is the primitive registry.
	// Stored as any to avoid circular dependency with registry package.
	// The concrete type is *registry.Registry.
	registry any

	// authorizer is the security authorizer for this namespace.
	authorizer security.Authorizer

	// exportIndex is the cached library export index for searching
	// unloaded library exports. Stored as any to avoid a circular
	// import with machine/compilation/. The concrete type is
	// *compilation.LibraryExportIndex. Callers type-assert at use.
	// Protected by exportIndexMu for lazy initialization.
	exportIndex   any
	exportIndexMu sync.RWMutex

	// moduleInstances caches loaded and initialized library instances.
	// Keyed by resolved library path (e.g., "(scheme base)").
	// Nil until the first module is loaded.
	moduleInstances map[string]*ModuleInstance

	// runtime is the phase 0 (runtime) environment frame.
	runtime *EnvironmentFrame
}

// ModuleInstance represents a loaded and initialized library.
type ModuleInstance struct {
	Env     *EnvironmentFrame
	Exports map[string]*GlobalIndex
}

// NewNamespace creates a new Namespace.
// This is the primary entry point for creating an isolated Wile VM instance.
// Call SetLoadPathStack before any file loading operations.
func NewNamespace() *Namespace {
	q := &Namespace{
		syntaxInterns: make(map[values.Value]syntax.SyntaxValue),
		scopeRegistry: make(map[*syntax.Scope]*EnvironmentFrame),
	}
	initRuntimeFrame(q, newGlobalEnvironmentFrameForNamespace(q))
	return q
}

// InternSyntax returns the canonical version of the given syntax value.
// If an equivalent syntax value has been seen before, it is returned.
// Otherwise, the value is added to the intern table and returned.
//
// When a parent Namespace exists, interning is delegated to the
// parent to maintain syntax identity across environments.
//
// This function is thread-safe.
func (p *Namespace) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
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
func (p *Namespace) Runtime() *EnvironmentFrame {
	return p.runtime
}

// AtPhase returns the environment for the given phase level, creating it if needed.
// Phase 0 is runtime, phase 1 is expansion (for-syntax), phase 2 is compile-time, etc.
// Negative phases (e.g., -1 for for-template) are also supported.
func (p *Namespace) AtPhase(phase int) *EnvironmentFrame {
	return p.phases.GetOrCreate(phase)
}

// Expand returns the expand phase environment (phase 1), creating it if needed.
// This is where syntax bindings from define-syntax are stored.
func (p *Namespace) Expand() *EnvironmentFrame {
	return p.AtPhase(PhaseExpand)
}

// Compile returns the compile phase environment (phase 2), creating it if needed.
// This is where compile-time procedures (syntax compilers) are stored.
func (p *Namespace) Compile() *EnvironmentFrame {
	return p.AtPhase(PhaseCompile)
}

// Phases returns the phase registry.
func (p *Namespace) Phases() *PhaseRegistry {
	return p.phases
}

// FileResolver returns the file resolver for include/load operations.
// Delegates to parent when non-nil, so child environments share the
// root resolver. Returns nil if no resolver has been set.
func (p *Namespace) FileResolver() FileResolver {
	if p.parent != nil {
		return p.parent.FileResolver()
	}
	return p.fileResolver
}

// SetFileResolver sets the file resolver for include/load operations.
// Delegates to parent when non-nil, matching the getter's delegation,
// so the resolver is always stored on the root Namespace.
func (p *Namespace) SetFileResolver(resolver FileResolver) {
	if p.parent != nil {
		p.parent.SetFileResolver(resolver)
		return
	}
	p.fileResolver = resolver
}

// LibraryRegistry returns the library registry for R7RS library loading.
// Returns nil if no registry has been set.
// Callers needing the full *compilation.LibraryRegistry can type-assert.
func (p *Namespace) LibraryRegistry() LibrarySearcher {
	return p.libraryRegistry
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
func (p *Namespace) SetLibraryRegistry(registry LibrarySearcher) {
	p.libraryRegistry = registry
}

// LibraryEnvFactory returns the factory for creating library environments.
// Returns nil if no factory has been set.
func (p *Namespace) LibraryEnvFactory() LibraryEnvFactory {
	return p.libraryEnvFactory
}

// SetLibraryEnvFactory sets the factory for creating library environments.
func (p *Namespace) SetLibraryEnvFactory(f LibraryEnvFactory) {
	p.libraryEnvFactory = f
}

// SetLoadPathStack sets the load path tracker for this namespace.
// Must be called before any file loading operations.
func (p *Namespace) SetLoadPathStack(s PathTracker) {
	p.loadPathStack = s
}

// LoadPathStack returns the load path tracker for tracking files currently
// being loaded. Delegates to parent when non-nil, ensuring child environments
// share the same stack as the root Namespace.
func (p *Namespace) LoadPathStack() PathTracker {
	if p.parent != nil {
		return p.parent.LoadPathStack()
	}
	return p.loadPathStack
}

// Registry returns the primitive registry.
// The caller must type-assert to *registry.Registry.
func (p *Namespace) Registry() any {
	return p.registry
}

// SetRegistry sets the primitive registry.
func (p *Namespace) SetRegistry(reg any) {
	p.registry = reg
}

// Authorizer returns the security authorizer for this namespace.
func (p *Namespace) Authorizer() security.Authorizer {
	return p.authorizer
}

// SetAuthorizer sets the security authorizer for this namespace.
func (p *Namespace) SetAuthorizer(auth security.Authorizer) {
	p.authorizer = auth
}

// ExportIndex returns the cached library export index for searching
// unloaded library exports. Returns nil if no index has been built.
// Delegates to parent when non-nil, so child namespaces share the
// root's index. The concrete type is *compilation.LibraryExportIndex.
func (p *Namespace) ExportIndex() any {
	if p.parent != nil {
		return p.parent.ExportIndex()
	}
	p.exportIndexMu.RLock()
	defer p.exportIndexMu.RUnlock()
	return p.exportIndex
}

// SetExportIndex stores the library export index on this namespace.
// Delegates to parent when non-nil, matching the getter's delegation,
// so the index is always stored on the root Namespace.
func (p *Namespace) SetExportIndex(idx any) {
	if p.parent != nil {
		p.parent.SetExportIndex(idx)
		return
	}
	p.exportIndexMu.Lock()
	defer p.exportIndexMu.Unlock()
	p.exportIndex = idx
}

// ModuleInstance returns the cached module instance for the given path,
// or (nil, false) if not loaded.
func (p *Namespace) ModuleInstance(path string) (*ModuleInstance, bool) {
	if p.moduleInstances == nil {
		return nil, false
	}
	inst, ok := p.moduleInstances[path]
	return inst, ok
}

// SetModuleInstance caches a loaded module instance.
func (p *Namespace) SetModuleInstance(path string, inst *ModuleInstance) {
	if p.moduleInstances == nil {
		p.moduleInstances = make(map[string]*ModuleInstance)
	}
	p.moduleInstances[path] = inst
}

// AttachModule copies a module instance from this namespace to the target.
// Returns an error if the module is not loaded in this namespace.
func (p *Namespace) AttachModule(path string, target *Namespace) error {
	inst, ok := p.ModuleInstance(path)
	if !ok {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "attachModule: %s not loaded in source namespace", path)
	}
	target.SetModuleInstance(path, inst)
	return nil
}

// RegisterLibraryScope associates a library scope with its defining environment.
// This enables cross-library macro hygiene: when a symbol carries a library
// scope, the compiler can redirect binding lookup to the library's env.
//
// This function is thread-safe.
func (p *Namespace) RegisterLibraryScope(scope *syntax.Scope, env *EnvironmentFrame) {
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
func (p *Namespace) LookupLibraryEnv(scope *syntax.Scope) *EnvironmentFrame {
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

// NewChildNamespace creates a new Namespace whose syntax
// interning is delegated to the receiver (the parent).
//
// # Ownership structure
//
// The child is a fully independent Namespace with its own:
//
//   - EnvironmentFrame (runtime, phase 0) — the root lexical scope
//   - GlobalEnvironmentFrame — isolated global bindings (define, set!, etc.)
//   - PhaseRegistry — isolated phase hierarchy (expand, compile created on demand)
//
// The child's GlobalEnvironmentFrame.namespace points to the child (not the
// parent), so new global bindings created in the child are keyed against the
// child's GlobalEnvironmentFrame. This is what provides binding isolation:
// definitions in the child do not appear in the parent, and vice versa.
//
//	Parent Namespace (root)
//	+-----------------------------------------------+
//	| syntaxInterns: map[Value]SyntaxValue ◄────────────── all interning
//	| syntaxInternsMu  (mutex)                      |
//	| parent: nil                                   |
//	| phases: *PhaseRegistry ──► {0: envP}          |
//	| runtime: envP ─────────────────────────────┐  |
//	| libraryRegistry: LibrarySearcher           |  |
//	+--------------------------------------------│--+
//	                                             │
//	                                             ▼
//	                         EnvironmentFrame (envP, phase 0)
//	                         +-------------------------------+
//	                         | global: *GlobalEnvFrame ───┐  |
//	                         | namespace: ──► parent NS   |  |
//	                         +---------------------------│---+
//	                                                     ▼
//	                                  GlobalEnvironmentFrame
//	                                  +-------------------------+
//	                                  | keys: {x:0, y:1, ...}   |
//	                                  | bindings: [...]         |
//	                                  | namespace: ──► parent NS|
//	                                  +-------------------------+
//
//	Child Namespace (returned by this method)
//	+-----------------------------------------------+
//	| syntaxInterns: nil  (never accessed)          |
//	| parent: ──► parent NS  (interning delegate)  |
//	| phases: *PhaseRegistry ──► {0: envC}          |
//	| runtime: envC ─────────────────────────────┐  |
//	| libraryRegistry: ──► same pointer as parent|  |
//	+--------------------------------------------│--+
//	                                             │
//	                                             ▼
//	                         EnvironmentFrame (envC, phase 0)
//	                         +-------------------------------+
//	                         | global: *GlobalEnvFrame ───┐  |
//	                         | namespace: ──► child NS    |  |
//	                         +---------------------------│---+
//	                                                     ▼
//	                                  GlobalEnvironmentFrame
//	                                  +-------------------------+
//	                                  | keys: {}  (empty)       |
//	                                  | bindings: []            |
//	                                  | namespace: ──► child NS |
//	                                  +-------------------------+
//
// # Interning delegation
//
// The child stores a parent pointer and has nil interning maps. InternSyntax
// checks for a non-nil parent and delegates recursively, ultimately reaching
// the root Namespace where the maps and mutexes live. This avoids
// sharing map pointers across structs with independent mutexes (which would
// be a data race).
//
// # Inherited state
//
// The child inherits the parent's libraryRegistry (a LibrarySearcher, concretely
// *compilation.LibraryRegistry) by value copy. This allows the child to load
// libraries via (import ...) without requiring the caller to set the registry
// explicitly. The registry itself is a shared pointer; mutations to the registry
// (e.g., registering a new library) are visible to both parent and child.
//
// # Contrast with NewChildRuntime
//
// NewChildRuntime returns an *EnvironmentFrame that shares the parent's
// Namespace directly (same pointer). It is used for library loading,
// where the library environment should share the same Namespace
// for syntax interning. However, because it shares the Namespace,
// it cannot be returned as a standalone environment value — calling Runtime()
// on the shared Namespace returns the parent's runtime frame, not
// the child's.
//
//	NewChildRuntime:                NewChildNamespace:
//
//	  Namespace (shared)    Parent NS         Child NS
//	  +------------------+            +----------+      +----------+
//	  | runtime: envP    |            | runtime: |      | runtime: |
//	  +------------------+            | envP     |      | envC     |
//	          │                       +----------+      +----------+
//	          │                                            │
//	     ┌────┴────┐                                       ▼
//	     ▼         ▼                           EnvironmentFrame (envC)
//	   envP      envC ◄── new child            +----------------------+
//	   (parent   (has own Global-              | namespace: child NS  |
//	    frame)    EnvFrame, but                +----------------------+
//	              namespace points
//	              to shared NS)
//
//	envC.Namespace() == parent    envC.Namespace() == child
//	TLE.Runtime() returns envP     child.Runtime() returns envC  ✓
//
// NewChildNamespace returns a new *Namespace that can be
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
func (p *Namespace) NewChildNamespace() *Namespace {
	q := &Namespace{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		registry:          p.registry,
		authorizer:        p.authorizer,
		parent:            p,
	}
	initRuntimeFrame(q, newGlobalEnvironmentFrameForNamespace(q))
	return q
}

// NewSchemeReportNamespace creates a new Namespace that is
// distinct from the receiver (so eq? returns #f) but contains a snapshot
// of the receiver's current global bindings at the time of the call.
//
// This implements R7RS §6.12 scheme-report-environment semantics: the returned
// environment is a separate object from interaction-environment and contains
// the standard bindings. User definitions added after this call are NOT
// visible in the returned environment.
func (p *Namespace) NewSchemeReportNamespace() *Namespace {
	q := &Namespace{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		registry:          p.registry,
		authorizer:        p.authorizer,
		parent:            p,
	}

	// Copy the parent's global bindings and repoint namespace to the child,
	// so that syntax interning delegates through q → p (parent chain).
	copiedGlobal := p.runtime.global.Copy()
	copiedGlobal.namespace = q
	initRuntimeFrame(q, copiedGlobal)
	return q
}

// NewChildRuntime creates a new runtime environment frame that shares this
// Namespace for syntax interning, but has its own
// GlobalEnvironmentFrame and PhaseRegistry for isolated bindings.
//
// This is used for library environments that need to:
//   - Share syntax interning
//   - Have isolated bindings (library definitions don't leak)
//   - Have their own phase hierarchy
func (p *Namespace) NewChildRuntime() *EnvironmentFrame {
	global := newGlobalEnvironmentFrameForNamespace(p)
	runtime := &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		namespace:  p,
	}
	runtime.phases = newPhaseRegistryForChild(p, runtime)
	return runtime
}

// NamespaceDeriveOption configures a derived namespace.
type NamespaceDeriveOption func(*NamespaceDeriveConfig)

// NamespaceDeriveConfig holds options for DeriveWith.
// Zero value means "inherit everything from parent."
type NamespaceDeriveConfig struct {
	Registry   any                 // if non-nil, overrides parent's registry
	Authorizer security.Authorizer // if non-nil, overrides parent's authorizer
}

// Derive creates a child namespace that shares syntax interning with
// the parent but has isolated bindings. The parent's registry and
// authorizer are shared by pointer — safe because registries are
// immutable after construction and authorizers are stateless interfaces.
func (p *Namespace) Derive() *Namespace {
	child := p.NewChildNamespace()
	child.registry = p.registry
	child.authorizer = p.authorizer
	return child
}

// DeriveWith creates a child namespace with option overrides.
// Use this when the child needs a restricted registry or different
// authorizer.
func (p *Namespace) DeriveWith(opts ...NamespaceDeriveOption) *Namespace {
	cfg := &NamespaceDeriveConfig{}
	for _, opt := range opts {
		opt(cfg)
	}

	child := p.NewChildNamespace()

	if cfg.Registry != nil {
		child.registry = cfg.Registry
	} else {
		child.registry = p.registry
	}

	if cfg.Authorizer != nil {
		child.authorizer = cfg.Authorizer
	} else {
		child.authorizer = p.authorizer
	}

	return child
}

// SyntaxInternCount returns the number of interned syntax objects.
// This is intended for testing and debugging purposes.
func (p *Namespace) SyntaxInternCount() int {
	p.syntaxInternsMu.RLock()
	defer p.syntaxInternsMu.RUnlock()
	return len(p.syntaxInterns)
}

// IsVoid returns true if the environment is nil.
func (p *Namespace) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the environments are the same object.
func (p *Namespace) EqualTo(v values.Value) bool {
	other, ok := v.(*Namespace)
	if !ok {
		return false
	}
	return p == other
}

// SchemeString returns the Scheme representation of the environment.
func (p *Namespace) SchemeString() string {
	if p.Name != "" {
		return fmt.Sprintf("#<environment %s>", p.Name)
	}
	return "#<environment>"
}

// newGlobalEnvironmentFrameForNamespace creates a new GlobalEnvironmentFrame
// that references the given Namespace.
func newGlobalEnvironmentFrameForNamespace(ns *Namespace) *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings:  []*Binding{},
		keys:      map[values.Symbol]int{},
		namespace: ns,
	}
	return q
}

// newPhaseRegistryForNamespace creates a new PhaseRegistry owned by the given Namespace.
func newPhaseRegistryForNamespace(ns *Namespace) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[int]*EnvironmentFrame),
		owner: ns,
	}
	q.envs[PhaseRuntime] = ns.runtime
	return q
}

// initRuntimeFrame creates a runtime EnvironmentFrame with a GlobalEnvironmentFrame
// and PhaseRegistry wired to the given Namespace. Used by all Namespace
// constructors to eliminate boilerplate divergence.
func initRuntimeFrame(ns *Namespace, global *GlobalEnvironmentFrame) {
	ns.runtime = &EnvironmentFrame{
		parent:     nil,
		global:     global,
		phaseLevel: PhaseRuntime,
		namespace:  ns,
	}
	ns.phases = newPhaseRegistryForNamespace(ns)
	ns.runtime.phases = ns.phases
}

// newPhaseRegistryForChild creates a PhaseRegistry for a child environment
// that shares a Namespace. Unlike newPhaseRegistryForNamespace,
// it does NOT read ns.runtime (which belongs to the parent).
func newPhaseRegistryForChild(ns *Namespace, runtime *EnvironmentFrame) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[int]*EnvironmentFrame),
		owner: ns,
	}
	q.envs[PhaseRuntime] = runtime
	return q
}
