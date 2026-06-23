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
	"maps"
	"sort"
	"sync"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
//
// # Field inheritance policy
//
// Child namespaces (NewChildNamespace and NewSchemeReportNamespace)
// inherit fields from their parent in one of three ways. New fields
// MUST pick a policy explicitly — the existing per-field decisions are
// encoded here, not in the constructors.
//
//	Per-VM (each namespace has its own; no inheritance):
//	  Name, parent, phases, runtime, moduleInstances, syntaxInterns
//	    Note: syntaxInterns is nil in children; InternSyntax delegates
//	    to parent so symbol identity remains globally consistent.
//
//	Captured at construction (the child stores its own copy of the
//	parent's pointer or map header at fork time; later *reassignments*
//	on the parent — e.g. parent.SetRegistry(other) — do not flow to
//	existing children. Mutations *through* the captured pointer — e.g.
//	parent.Registry().AddPrimitive(...) — ARE visible to children
//	because the pointer they hold is the same Go object):
//	  libraryRegistry, libraryEnvFactory, registry, authorizer, envMap
//	    Rationale: these are capability state. Reassignments on the
//	    parent must not silently widen capability on existing children
//	    (envMap is the load-bearing example — see the SetEnvMap doc
//	    comment in this file for the discussion).
//
//	Delegated to the root (children read/write through to the topmost
//	parent; the field itself only ever lives on the root namespace):
//	  fileResolver, loadPathStack, scopeRegistry, exportIndex/exportIndexBuilt
//	    Rationale: these are per-VM resources whose identity must be
//	    shared across the entire namespace tree (file resolution must
//	    use the same paths in any child; library scopes registered by
//	    one child must be visible to all). The root() helper walks the
//	    parent chain in O(depth); per-method delegation prologues are
//	    not used.
//
// Adding a new field: choose a policy above, document it in this block,
// then:
//   - Captured: copy it from the parent in *both* NewChildNamespace and
//     NewSchemeReportNamespace (these are the two constructors that
//     populate child state from a parent; they share the same captured
//     set).
//   - Delegated: define accessors that go through p.root() to read or
//     write the field; do not store it on child namespaces.
//
// Do NOT mix the two for one field — the asymmetry is the bug source
// the policy table exists to prevent.
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

	// immutableLiterals is the engine-scoped set of immutable literal
	// pair/vector objects (R7RS §4.1.2). Delegated to root (see the field
	// inheritance policy comment above): only the root Namespace holds it
	// (nil in children); children read it via ImmutableLiterals() which
	// resolves through root(). Engine-scoped so a literal compiled under any
	// child is checkable by any mutator anywhere in the namespace tree.
	immutableLiterals *ImmutableLiterals

	// immutableTopLevel, when true, makes the engine treat a top-level define
	// that is defined-once and never set! within its compilation unit as
	// rebind-stable (BindingMeta.Stable) and then forbid a later set!/redefine
	// of such a binding. Opt-in (off by default = strict R7RS) via the
	// WithImmutableTopLevel engine option; it gates the frame-reclamation
	// optimizer's top-level payoff. Delegated to root like loadPathStack so the
	// compiler and validator see one engine-scoped setting.
	immutableTopLevel bool

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

	// inlineHOFTemplates holds the pre-validated inline-HOF loop templates
	// (callback specialization Strategy A), built per-Namespace at bootstrap.
	// Stored as an InlineHOFTemplateStore interface because the template type
	// (*validate.ValidatedLambda) lives above environment/ in the import graph;
	// compilation type-asserts. Mirrors libraryRegistry. Nil until built; not
	// inherited by child namespaces (template scopes are sealed-base-specific).
	inlineHOFTemplates InlineHOFTemplateStore

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
	// exportIndexBuilt is set to true after the first build attempt
	// (successful or non-transient failure) to prevent redundant retries.
	exportIndex      any
	exportIndexBuilt bool
	exportIndexMu    sync.RWMutex

	// moduleInstances caches loaded and initialized library instances.
	// Keyed by resolved library path (e.g., "(scheme base)").
	// Nil until the first module is loaded.
	moduleInstances map[string]*ModuleInstance

	// runtime is the phase 0 (runtime) environment frame.
	runtime *EnvironmentFrame

	// sealedBase is the per-NAMESPACE immutable runtime frame (phase 0): Go primitives
	// + sealed stdlib runtime procedures, parent nil. It is the lexical parent of
	// `runtime` (the mutable user global). Holds the optimizer's Stable anchors.
	// PER-NAMESPACE, NOT root-delegated: a profile child (NewProfileEnvironment) carves
	// its OWN sealed base populated by its own curated registry apply, so delegating to
	// root would corrupt the engine root's base. Every namespace OWNS its sealed base;
	// a report namespace copies the parent's into its own (see NewSchemeReportNamespace).
	sealedBase *EnvironmentFrame

	// envMap is an optional virtual environment variable map.
	// When non-nil, envvars primitives read from this map instead of
	// the process environment (os.Getenv/os.Environ), bypassing the
	// authorizer gate. Nil means "fall through to OS, gated by authorizer".
	// Set via WithEnvMap (Task 5).
	envMap map[string]string

	// extensionState holds opaque, namespace-scoped state for extensions
	// that need per-Namespace storage (e.g. the SAT solver's last model).
	// Each extension picks its own key (typically an unexported sentinel
	// type) so keys never collide across extensions. Because the storage
	// lives on the Namespace, it is collected together with the Namespace
	// — extensions get namespace lifetime for free without a teardown hook
	// and without a process-global map that would pin Namespace pointers.
	// The zero sync.Map is ready to use; no initialization is required.
	extensionState sync.Map // map[any]any
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
		syntaxInterns:     make(map[values.Value]syntax.SyntaxValue),
		scopeRegistry:     make(map[*syntax.Scope]*EnvironmentFrame),
		immutableLiterals: &ImmutableLiterals{},
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

// SealedBase returns this Namespace's immutable sealed-base runtime frame (phase 0),
// the lexical parent of the mutable runtime global. PER-NAMESPACE (NOT root-delegated,
// unlike immutableLiterals): each Namespace OWNS its sealed base so a profile child's
// curated apply does not write into the engine root's base. Report namespaces copy the
// parent's sealed base into their own (see NewSchemeReportNamespace).
func (p *Namespace) SealedBase() *EnvironmentFrame {
	return p.sealedBase
}

// BoundSymbolNames returns a freshly-consed list of every symbol bound in the
// namespace's runtime, spanning BOTH the mutable runtime global (user defines) and
// the sealed base (primitives + sealed stdlib procedures). It is the shared body of
// the environment-bound-names and namespace-bound-names primitives. Post-carve the
// sealed base must be included or primitives like `car` vanish from the result (the
// key map carries no parent walk). Iteration order is unspecified; a name shadowed in
// both frames appears once (deduped via the seen set).
func (p *Namespace) BoundSymbolNames() values.Value {
	seen := map[string]struct{}{}
	var result values.Value = values.EmptyList
	for _, frame := range []*EnvironmentFrame{p.runtime, p.sealedBase} {
		if frame == nil {
			continue
		}
		for key := range frame.GlobalEnvironment().Keys() {
			_, dup := seen[key.Key]
			if dup {
				continue
			}
			seen[key.Key] = struct{}{}
			result = values.NewCons(values.NewSymbol(key.Key), result)
		}
	}
	return result
}

// BoundNamesAcrossPhases returns a sorted, deduplicated list of every binding
// name visible across all instantiated phases (runtime, expand, compile) plus
// the sealed base. Unlike BoundSymbolNames — which spans only the runtime
// global and the sealed base, returning a Scheme list for the bound-names
// primitives — this also walks the expand and compile phases, so macro and
// special-form keywords appear. It is the set a REPL wants for tab completion.
// Iteration order across phases does not change the result set (only names are
// collected); the output is sorted for determinism.
func (p *Namespace) BoundNamesAcrossPhases() []string {
	seen := map[string]struct{}{}
	var names []string
	collect := func(frame *EnvironmentFrame) {
		if frame == nil {
			return
		}
		global := frame.GlobalEnvironment()
		if global == nil {
			return
		}
		for key := range global.Keys() {
			_, dup := seen[key.Key]
			if dup {
				continue
			}
			seen[key.Key] = struct{}{}
			names = append(names, key.Key)
		}
	}
	for _, phase := range p.phases.Phases() {
		collect(p.phases.Get(phase))
	}
	collect(p.sealedBase)
	sort.Strings(names)
	return names
}

// AtPhase returns the environment for the given phase level, creating it if needed.
// Phase 0 is runtime, phase 1 is expansion (for-syntax), phase 2 is compile-time, etc.
// Negative phases (e.g., -1 for for-template) are also supported.
func (p *Namespace) AtPhase(phase Phase) *EnvironmentFrame {
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

// root walks the parent chain and returns the topmost Namespace.
// Used by accessors for fields whose inheritance policy is "delegated to
// root" (see the field inheritance policy comment on Namespace). For a
// root namespace, p.root() == p in O(1).
//
// Precondition: p must be non-nil. Like every other accessor on
// *Namespace (e.g. LibraryRegistry, Authorizer), root deliberately
// does not guard against a nil receiver — Namespace is a value type
// whose nil-state is meaningless, and silently returning nil here would
// only push the inevitable panic deeper into the dependent accessor
// (which would then read .fileResolver / .loadPathStack / etc. off
// nil). EnvironmentFrame's nil-namespace shortcut guards exist because
// EnvironmentFrame can legitimately be constructed without a Namespace
// (newEnvironmentFrame test fixtures); Namespace itself cannot.
func (p *Namespace) root() *Namespace {
	for p.parent != nil {
		p = p.parent
	}
	return p
}

// FileResolver returns the file resolver for include/load operations.
// Returns nil if no resolver has been set. Delegated to root.
func (p *Namespace) FileResolver() FileResolver {
	return p.root().fileResolver
}

// SetFileResolver sets the file resolver for include/load operations.
// Delegated to root: the resolver always lives on the root Namespace.
func (p *Namespace) SetFileResolver(resolver FileResolver) {
	p.root().fileResolver = resolver
}

// ImmutableLiterals returns the engine-scoped set of immutable literal
// pair/vector objects (R7RS §4.1.2). Defined once on the root Namespace;
// children delegate through root(), so every mutator sees the same set.
func (p *Namespace) ImmutableLiterals() *ImmutableLiterals {
	return p.root().immutableLiterals
}

// ImmutableTopLevel reports whether top-level-define immutability is enforced for
// THIS namespace. It is a property of the engine's PRIMARY (root) namespace only —
// the home of compiled-program top-level defines, which are the frame-reclaim
// optimizer's Stable anchors. CHILD namespaces (parent != nil) are mutable
// interaction/eval scratch spaces — (environment ...), scheme-report-environment,
// profile children — modeled on Chez's mutable interaction-environment (see
// plans/2026-06-13-immutable-toplevel-by-default-scoping.local.md:357-370): a define
// there shadows/redefines freely and its bindings are never stamped Stable. This is
// the "compilation units only" scope: immutability is for the compiled program, not
// for interactive eval. Reclaim soundness is preserved because set! of a Stable
// anchor copied into a child is still rejected by the set!-gate, which keys on
// IsStable() directly (compile_validated.go) rather than on this flag.
func (p *Namespace) ImmutableTopLevel() bool {
	if p.parent != nil {
		return false
	}
	return p.immutableTopLevel
}

// SetImmutableTopLevel enables or disables top-level-define immutability for the
// engine. Set once at engine construction (WithImmutableTopLevel / WithMutableTopLevel).
// Stored on the root; child namespaces ignore it and are always mutable
// (see ImmutableTopLevel).
func (p *Namespace) SetImmutableTopLevel(on bool) {
	p.root().immutableTopLevel = on
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

// InlineHOFTemplateStore returns a pre-validated inline-HOF loop template by HOF
// name (callback specialization Strategy A). The returned template is a
// *validate.ValidatedLambda, exposed here as any because environment/ is below
// validate/ in the import graph; the compilation consumer type-asserts. Mirrors
// LibrarySearcher: the minimum environment/ needs to hold a compilation artifact.
type InlineHOFTemplateStore interface {
	InlineHOFTemplate(name string) (any, bool)
}

// InlineHOFTemplates returns the per-Namespace inline-HOF template store, or nil
// if templates have not been built for this Namespace (in which case the compiler
// performs no inline-HOF specialization).
func (p *Namespace) InlineHOFTemplates() InlineHOFTemplateStore {
	return p.inlineHOFTemplates
}

// SetInlineHOFTemplates installs the inline-HOF template store. Called once per
// Namespace at bootstrap, after the sealed base is loaded.
func (p *Namespace) SetInlineHOFTemplates(store InlineHOFTemplateStore) {
	p.inlineHOFTemplates = store
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
// Delegated to root: the tracker always lives on the root Namespace.
// Must be called before any file loading operations.
func (p *Namespace) SetLoadPathStack(s PathTracker) {
	p.root().loadPathStack = s
}

// LoadPathStack returns the load path tracker for tracking files currently
// being loaded. Delegated to root.
func (p *Namespace) LoadPathStack() PathTracker {
	return p.root().loadPathStack
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

// ExtensionState returns the namespace-scoped state stored under key, and a
// boolean reporting whether any value was present. Extensions use this for
// per-Namespace storage whose lifetime is tied to the Namespace. The key is
// extension-chosen; an unexported sentinel type avoids cross-extension
// collisions. Safe for concurrent use.
func (p *Namespace) ExtensionState(key any) (any, bool) {
	return p.extensionState.Load(key)
}

// SetExtensionState stores namespace-scoped extension state under key.
// Safe for concurrent use.
func (p *Namespace) SetExtensionState(key, value any) {
	p.extensionState.Store(key, value)
}

// DeleteExtensionState removes any namespace-scoped extension state stored
// under key. Safe for concurrent use.
func (p *Namespace) DeleteExtensionState(key any) {
	p.extensionState.Delete(key)
}

// Authorizer returns the security authorizer for this namespace.
func (p *Namespace) Authorizer() security.Authorizer {
	return p.authorizer
}

// EnvMap returns the virtual environment variable map for this namespace,
// or nil if none has been configured. When non-nil, envvars primitives
// read from this map instead of the process environment, bypassing the
// authorizer gate.
func (p *Namespace) EnvMap() map[string]string {
	return p.envMap
}

// SetEnvMap sets the virtual environment variable map.
// When set, envvars primitives read from this map instead of os.Getenv.
//
// The provided map is defensively copied so that subsequent mutation by the
// caller does not leak into the VM's sandbox state. A nil argument clears
// the virtual map (falls back to os.Getenv, gated by the authorizer).
//
// Note: EnvMap() still returns the internal map by reference for zero-cost
// primitive access. Callers who reach for EnvMap() must treat the result as
// read-only; mutating it bypasses the defensive copy applied here.
func (p *Namespace) SetEnvMap(m map[string]string) {
	if m == nil {
		p.envMap = nil
		return
	}
	p.envMap = make(map[string]string, len(m))
	maps.Copy(p.envMap, m)
}

// SetAuthorizer sets the security authorizer for this namespace.
func (p *Namespace) SetAuthorizer(auth security.Authorizer) {
	p.authorizer = auth
}

// ExportIndex returns the cached library export index and whether a
// build has been attempted. Returns (nil, false) if no build has run.
// Delegated to root. The concrete type is *compilation.LibraryExportIndex.
func (p *Namespace) ExportIndex() (any, bool) {
	r := p.root()
	r.exportIndexMu.RLock()
	defer r.exportIndexMu.RUnlock()
	return r.exportIndex, r.exportIndexBuilt
}

// SetExportIndex stores the library export index and marks it as built,
// preventing subsequent build attempts. Delegated to root.
func (p *Namespace) SetExportIndex(idx any) {
	r := p.root()
	r.exportIndexMu.Lock()
	defer r.exportIndexMu.Unlock()
	r.exportIndex = idx
	r.exportIndexBuilt = true
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
// Delegated to root: the registry always lives on the root Namespace.
//
// This function is thread-safe.
func (p *Namespace) RegisterLibraryScope(scope *syntax.Scope, env *EnvironmentFrame) {
	if scope == nil || env == nil {
		return
	}
	r := p.root()
	r.scopeRegistryMu.Lock()
	defer r.scopeRegistryMu.Unlock()
	r.scopeRegistry[scope] = env
}

// LookupLibraryEnv returns the environment associated with the given library
// scope, or nil if not registered. Delegated to root.
// This function is thread-safe.
func (p *Namespace) LookupLibraryEnv(scope *syntax.Scope) *EnvironmentFrame {
	if scope == nil {
		return nil
	}
	r := p.root()
	r.scopeRegistryMu.RLock()
	defer r.scopeRegistryMu.RUnlock()
	return r.scopeRegistry[scope]
}

// NamespaceOption configures a derived namespace at construction time.
// Use it with NewChildNamespace to override fields that would otherwise
// be inherited from the parent (e.g. a restricted registry or a
// different authorizer).
//
// The "*Set" booleans on namespaceConfig let WithChildRegistry(nil) and
// WithChildAuthorizer(nil) mean "explicitly set the field to nil" —
// distinct from "no override supplied" which means "inherit from
// parent."
type NamespaceOption func(*namespaceConfig)

// namespaceConfig collects override values supplied via NamespaceOption.
type namespaceConfig struct {
	registry      any
	registrySet   bool
	authorizer    security.Authorizer
	authorizerSet bool
}

// WithChildRegistry overrides the parent's primitive registry on a
// derived namespace. Use this to install a restricted or alternative
// registry in a child without mutating the parent.
//
// Distinct from wile.WithRegistry, which configures an EngineOption at
// the top level — the wile.* and environment.* option families operate
// on different config types and have different semantics.
func WithChildRegistry(r any) NamespaceOption {
	return func(cfg *namespaceConfig) {
		cfg.registry = r
		cfg.registrySet = true
	}
}

// WithChildAuthorizer overrides the parent's security authorizer on a
// derived namespace. Use this to give a child a stricter (or different)
// capability profile.
//
// Distinct from wile.WithAuthorizer, which configures an EngineOption
// at the top level.
func WithChildAuthorizer(a security.Authorizer) NamespaceOption {
	return func(cfg *namespaceConfig) {
		cfg.authorizer = a
		cfg.authorizerSet = true
	}
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
// The child's runtime EnvironmentFrame.namespace points to the child (not the
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
// The child also inherits the parent's envMap (virtual environment variable
// map) by reference. envMap is capability state — it constrains what the
// envvars primitives can read — so derived namespaces must not silently
// widen capability by acquiring a nil map that falls through to os.Getenv.
// The reference is safe to share because SetEnvMap always reassigns the
// field rather than mutating the existing map.
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
//	    frame)    EnvFrame; reaches            +----------------------+
//	              shared NS via the
//	              owning EnvFrame)
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
// environments that are identity-compatible with the caller's symbol
// table while providing isolated bindings. Optional NamespaceOption
// arguments override fields that would otherwise be inherited from the
// parent (currently registry and authorizer); see WithRegistry and
// WithAuthorizer.
//
// All captured fields (libraryRegistry, libraryEnvFactory, registry,
// authorizer, envMap) are copied from the parent in one place — adding
// a new captured field requires a single edit here, not a sweep of
// multiple constructors.
func (p *Namespace) NewChildNamespace(opts ...NamespaceOption) *Namespace {
	cfg := &namespaceConfig{}
	for _, opt := range opts {
		opt(cfg)
	}
	q := &Namespace{
		libraryRegistry:   p.libraryRegistry,
		libraryEnvFactory: p.libraryEnvFactory,
		registry:          p.registry,
		authorizer:        p.authorizer,
		envMap:            p.envMap,
		parent:            p,
	}
	if cfg.registrySet {
		q.registry = cfg.registry
	}
	if cfg.authorizerSet {
		q.authorizer = cfg.authorizer
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
		envMap:            p.envMap,
		parent:            p,
	}

	// R7RS scheme-report-environment is a frozen, INDEPENDENT snapshot (pre-carve it
	// deep-copied the whole merged global). Copy BOTH the parent's sealed base (standard
	// bindings: prims + sealed stdlib) and the parent's runtime (user defines made
	// before this call) into q's OWN two-level stack via the shared wireRuntimeFrames
	// topology. Pre-call user defines are visible, post-call ones are not, prims resolve
	// for eval — and q aliases nothing (a set! inside the report env touches only its own
	// copy). Syntax interning delegates through q → p via the Namespace.parent chain.
	wireRuntimeFrames(q, p.sealedBase.global.Copy(), p.runtime.global.Copy())
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
// for use within the given Namespace. The Namespace argument is retained
// for symmetry with the other Namespace-scoped helpers; the
// GlobalEnvironmentFrame itself does not store a back-reference (ownership
// flows through EnvironmentFrame).
func newGlobalEnvironmentFrameForNamespace(_ *Namespace) *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
	}
	return q
}

// newPhaseRegistryForNamespace creates a new PhaseRegistry owned by the given Namespace.
func newPhaseRegistryForNamespace(ns *Namespace) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[Phase]*EnvironmentFrame),
		owner: ns,
	}
	q.envs[PhaseRuntime] = ns.runtime
	return q
}

// initRuntimeFrame builds the per-Engine runtime layer for a freshly-constructed
// namespace: a fresh sealed-base global (primitives + sealed stdlib are applied into it
// later) and the caller-supplied mutableGlobal for the user runtime. Runtime() returns
// the mutable child; SealedBase() returns the parent. The two-frame topology and phase
// wiring live in wireRuntimeFrames.
func initRuntimeFrame(ns *Namespace, mutableGlobal *GlobalEnvironmentFrame) {
	wireRuntimeFrames(ns, newGlobalEnvironmentFrameForNamespace(ns), mutableGlobal)
}

// wireRuntimeFrames builds the two-level phase-0 stack and its shared PhaseRegistry: an
// immutable sealed-base frame (parent nil, sealedGlobal) parented by a mutable runtime
// frame (mutableGlobal). It is the single source of truth for the sealed-base/mutable-
// runtime topology and phase wiring; initRuntimeFrame and NewSchemeReportNamespace differ
// ONLY in whether the two globals are fresh (engine construction) or copied from a parent
// (the frozen scheme-report snapshot).
//
// Order matters: newPhaseRegistryForNamespace reads ns.runtime for envs[PhaseRuntime], so
// the runtime frame is set before it is called. The sealed base shares the same
// PhaseRegistry (ns.phases) so AtPhase/Expand/Compile resolve identically whether reached
// from the mutable child or the sealed base — the sealed base is reached only via the
// parent walk during global resolution.
func wireRuntimeFrames(ns *Namespace, sealedGlobal, mutableGlobal *GlobalEnvironmentFrame) {
	ns.sealedBase = &EnvironmentFrame{
		parent:     nil,
		global:     sealedGlobal,
		phaseLevel: PhaseRuntime,
		namespace:  ns,
	}
	ns.runtime = &EnvironmentFrame{
		parent:     ns.sealedBase,
		global:     mutableGlobal,
		phaseLevel: PhaseRuntime,
		namespace:  ns,
	}
	ns.phases = newPhaseRegistryForNamespace(ns) // envs[PhaseRuntime] = ns.runtime
	ns.runtime.phases = ns.phases
	ns.sealedBase.phases = ns.phases
}

// newPhaseRegistryForChild creates a PhaseRegistry for a child environment
// that shares a Namespace. Unlike newPhaseRegistryForNamespace,
// it does NOT read ns.runtime (which belongs to the parent).
func newPhaseRegistryForChild(ns *Namespace, runtime *EnvironmentFrame) *PhaseRegistry {
	q := &PhaseRegistry{
		envs:  make(map[Phase]*EnvironmentFrame),
		owner: ns,
	}
	q.envs[PhaseRuntime] = runtime
	return q
}
