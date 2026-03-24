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
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

// EnvironmentFrame represents an environment frame in the hierarchy.
//
// # Type Relationships
//
// The environment system has four types with distinct responsibilities:
//
//	┌─────────────────────────────────────────────────────────────────────────┐
//	│                            Namespace                                    │
//	│  (Per-VM instance: owns syntax interning, phases, libraries)            │
//	│                                                                         │
//	│  syntaxInterns ──── map[Value]SyntaxValue (thread-safe)                 │
//	│  phases ─────────── *PhaseRegistry                                      │
//	│  libraryRegistry ── any (*machine.LibraryRegistry)                      │
//	│  runtime ────────── *EnvironmentFrame (phase 0)                         │
//	└─────────────────────────────────────────────────────────────────────────┘
//	                                    │
//	                                    │ owns
//	                                    ▼
//	┌─────────────────────────────────────────────────────────────────────────┐
//	│                         EnvironmentFrame                                │
//	│  (Lexical scope node: links local/global bindings, parent chain)        │
//	│                                                                         │
//	│  parent ─────────── *EnvironmentFrame (lexical parent, nil at top)      │
//	│  local ──────────── LocalEnvironmentFrame (value; keys==nil → none)     │
//	│  global ─────────── *GlobalEnvironmentFrame (define bindings)           │
//	│  phaseLevel ─────── int (0=runtime, 1=expand, 2=compile)                │
//	│  phases ─────────── *PhaseRegistry (shared reference)                   │
//	│  namespace ───────── *Namespace (back-reference)                        │
//	└─────────────────────────────────────────────────────────────────────────┘
//	          │                                    │
//	          │ contains                           │ contains
//	          ▼                                    ▼
//	┌───────────────────────────┐    ┌────────────────────────────────────────┐
//	│  LocalEnvironmentFrame    │    │      GlobalEnvironmentFrame            │
//	│  (Single scope bindings)  │    │  (Phase-wide global bindings)          │
//	│                           │    │                                        │
//	│  keys ─── map[Symbol]int  │    │  keys ──────── map[Symbol]int          │
//	│  bindings ── []*Binding   │    │  bindings ──── []*Binding              │
//	└───────────────────────────┘    │  namespace ──── *Namespace             │
//	                                 └────────────────────────────────────────┘
//
// # Ownership and Sharing
//
//   - Namespace: Root owner. One per Wile VM instance.
//   - EnvironmentFrame: Many per VM. Share namespace and phases references.
//   - GlobalEnvironmentFrame: One per phase. Shares namespace reference.
//   - LocalEnvironmentFrame: One per lexical scope. No external references.
//
// # Lexical Hierarchy (parent chain)
//
//	(lambda (x)           ; EnvironmentFrame A: local={x}, parent=TopLevel
//	  (let ((y 1))        ; EnvironmentFrame B: local={y}, parent=A
//	    (lambda (z)       ; EnvironmentFrame C: local={z}, parent=B
//	      (+ x y z))))
//
// # Phase Hierarchy (via PhaseRegistry)
//
//	Namespace
//	└── PhaseRegistry
//	    ├── [0] Runtime EnvironmentFrame (normal execution)
//	    ├── [1] Expand EnvironmentFrame (macro expansion, for-syntax)
//	    ├── [2] Compile EnvironmentFrame (syntax compilers, for-meta 2)
//	    └── [-1] Template EnvironmentFrame (for-template, future)
//
// Each phase has its own GlobalEnvironmentFrame but shares the same
// Namespace for syntax interning.
//
// # Binding Lookup
//
// Two-phase search: first all locals up parent chain, then globals.
type EnvironmentFrame struct {
	// parent links to enclosing lexical scope (nil for root frame)
	parent *EnvironmentFrame
	// local holds local bindings for this frame (parameters, let-bound variables).
	// Embedded by value to eliminate a separate heap allocation per closure call.
	// Sentinel: local.keys == nil means "no local environment" (zero value).
	local LocalEnvironmentFrame
	// global holds global bindings for this phase
	global *GlobalEnvironmentFrame
	// phaseLevel indicates which phase this frame represents (0=runtime, 1=expand, etc.)
	phaseLevel int
	// phases is the shared phase registry, owned by Namespace
	phases *PhaseRegistry
	// namespace is the owning Namespace
	namespace *Namespace
}

// NewNamespaceFrame creates a new root environment frame via NewNamespace.
//
// Deprecated: Use NewNamespace().Runtime() instead for per-instance
// syntax interning. This function now internally uses NewNamespace()
// to provide proper isolation.
func NewNamespaceFrame() *EnvironmentFrame {
	return NewNamespace().Runtime()
}

// newEnvironmentFrame creates an isolated environment frame without a
// Namespace or PhaseRegistry. Calling AtPhase() on the result
// will panic. Use NewNamespace().Runtime() for full environments
// or NewEnvironmentFrameWithParent() for child scopes.
func newEnvironmentFrame(local *LocalEnvironmentFrame, global *GlobalEnvironmentFrame) *EnvironmentFrame {
	q := &EnvironmentFrame{
		global:     global,
		phaseLevel: PhaseRuntime,
		phases:     nil, // No phase registry for isolated environments
	}
	if local != nil {
		q.local = *local
	}
	return q
}

// NewEnvironmentFrameWithParent creates a new environment frame with the given local environment frame and parent environment frame.
// The global environment frame is inherited from the parent.
// This is used for creating child frames within a phase (e.g., lambda bodies, let-syntax).
// The phase level, registry, and namespace are inherited from the parent.
// Panics if parent is nil - use NewNamespaceFrame() instead.
func NewEnvironmentFrameWithParent(local *LocalEnvironmentFrame, parent *EnvironmentFrame) *EnvironmentFrame {
	if parent == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrNilParentEnvironment,
			"NewEnvironmentFrameWithParent called with nil parent - use NewNamespaceFrame() instead",
		))
	}
	q := &EnvironmentFrame{
		parent:     parent,
		global:     parent.global,
		phaseLevel: parent.phaseLevel,
		phases:     parent.phases,
		namespace:  parent.namespace,
	}
	if local != nil {
		q.local = *local
	}
	return q
}

// NewApplyFrame creates a new EnvironmentFrame for a closure application,
// fusing CopyForApply + NewEnvironmentFrameWithParent into one allocation.
// The source frame's local bindings are copied into the new frame, and the
// parent chain is set from the source's parent.
func (p *EnvironmentFrame) NewApplyFrame() *EnvironmentFrame {
	parent := p.parent
	if parent == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrNilParentEnvironment,
			"NewApplyFrame called on frame with nil parent - closure environments must have a parent",
		))
	}
	q := &EnvironmentFrame{
		parent:     parent,
		global:     parent.global,
		phaseLevel: parent.phaseLevel,
		phases:     parent.phases,
		namespace:  parent.namespace,
	}
	p.local.copyForApplyInto(&q.local)
	return q
}

// InitApplyFrame populates dst from p's closure environment without allocating
// a new EnvironmentFrame. The caller is responsible for providing dst (e.g.
// from a pool). This is the pooling-friendly counterpart of NewApplyFrame.
func (p *EnvironmentFrame) InitApplyFrame(dst *EnvironmentFrame) {
	parent := p.parent
	if parent == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrNilParentEnvironment,
			"InitApplyFrame called on frame with nil parent - closure environments must have a parent",
		))
	}
	dst.parent = parent
	dst.global = parent.global
	dst.phaseLevel = parent.phaseLevel
	dst.phases = parent.phases
	dst.namespace = parent.namespace
	p.local.copyForApplyInto(&dst.local)
}

// ResetForPool clears the EnvironmentFrame for return to the FreeList while
// preserving the local bindings backing array capacity. This mirrors the
// Stack pool pattern: clear full capacity (so GC can collect referenced
// values), zero the struct, then restore the slice header with len=0.
//
// After reset, the frame is a valid zero-value EnvironmentFrame whose
// local.bindings has cap > 0 but len == 0. The next copyForApplyInto call
// will reslice instead of allocating when cap >= n.
func (p *EnvironmentFrame) ResetForPool() {
	bindings := p.local.bindings
	full := bindings[:cap(bindings)]
	for i := range full {
		full[i] = Binding{}
	}
	*p = EnvironmentFrame{}
	p.local.bindings = full[:0]
}

// PreAllocateBindings sets the local bindings slice to a zero-length slice
// with the given capacity. Used by the env frame pool to ensure fresh frames
// have sufficient capacity for copyForApplyInto to reslice instead of allocate.
// Must only be called on freshly constructed frames (before any other use).
// n must be non-negative; negative values are clamped to 0.
func (p *EnvironmentFrame) PreAllocateBindings(n int) {
	if n < 0 {
		n = 0
	}
	p.local.bindings = make([]Binding, 0, n)
}

// LocalBindingsSlice returns the raw local bindings slice, bypassing the
// nil-keys check in LocalEnvironment(). This exposes the pre-allocated
// capacity that pooled frames retain across reset cycles.
func (p *EnvironmentFrame) LocalBindingsSlice() []Binding {
	return p.local.bindings
}

// IsTopLevel returns true if this is the top-level environment frame (no parent).
func (p *EnvironmentFrame) IsTopLevel() bool {
	return p.parent == nil
}

// TopLevel returns the top-level environment frame in the hierarchy.
func (p *EnvironmentFrame) TopLevel() *EnvironmentFrame {
	frame := p
	for frame.parent != nil {
		frame = frame.parent
	}
	return frame
}

// AtPhase returns the environment for the given phase level, creating it if needed.
// Phase 0 is runtime, phase 1 is expansion (for-syntax), phase 2 is compile-time, etc.
// Negative phases (e.g., -1 for for-template) are also supported.
//
// This is the primary method for cross-phase access with O(1) lookup time.
// The environment must have been created via NewNamespace().
func (p *EnvironmentFrame) AtPhase(phase int) *EnvironmentFrame {
	topLevel := p.TopLevel()
	if topLevel.phases == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrMissingPhaseRegistry,
			"AtPhase called on environment without PhaseRegistry - use NewNamespace()",
		))
	}
	return topLevel.phases.GetOrCreate(phase)
}

// PhaseLevel returns the phase level of this environment frame.
func (p *EnvironmentFrame) PhaseLevel() int {
	return p.phaseLevel
}

// Runtime returns the runtime phase environment (phase 0).
// This is the root environment where normal bindings live.
func (p *EnvironmentFrame) Runtime() *EnvironmentFrame {
	return p.AtPhase(PhaseRuntime)
}

// Expand returns the expand phase environment (phase 1), creating it if needed.
// This is where syntax bindings from define-syntax are stored.
func (p *EnvironmentFrame) Expand() *EnvironmentFrame {
	return p.AtPhase(PhaseExpand)
}

// Compile returns the compile phase environment (phase 2), creating it if needed.
// This is where compile-time procedures (syntax compilers) are stored.
func (p *EnvironmentFrame) Compile() *EnvironmentFrame {
	return p.AtPhase(PhaseCompile)
}

// Parent returns the parent environment frame.
func (p *EnvironmentFrame) Parent() *EnvironmentFrame {
	return p.parent
}

// GlobalEnvironment returns the global environment frame.
func (p *EnvironmentFrame) GlobalEnvironment() *GlobalEnvironmentFrame {
	return p.global
}

// FileResolver returns the file resolver from the Namespace.
// The caller must type-assert to machine.FileResolver.
// Returns nil if no resolver has been set or if namespace is nil.
func (p *EnvironmentFrame) FileResolver() any {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.FileResolver()
}

// SetFileResolver sets the file resolver on the Namespace.
// The resolver should be a machine.FileResolver.
// No-op if namespace is nil.
func (p *EnvironmentFrame) SetFileResolver(resolver any) {
	if p.namespace == nil {
		return
	}
	p.namespace.SetFileResolver(resolver)
}

// LibraryRegistry returns the library registry from the Namespace.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set or if namespace is nil.
func (p *EnvironmentFrame) LibraryRegistry() any {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.LibraryRegistry()
}

// SetLibraryRegistry sets the library registry on the Namespace.
// The registry should be a *machine.LibraryRegistry.
// No-op if namespace is nil.
func (p *EnvironmentFrame) SetLibraryRegistry(registry any) {
	if p.namespace == nil {
		return
	}
	p.namespace.SetLibraryRegistry(registry)
}

// LoadPathStack returns the load path stack for tracking files currently
// being loaded, or nil if this frame has no Namespace.
func (p *EnvironmentFrame) LoadPathStack() *LoadPathStack {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.LoadPathStack()
}

// hasLocal returns true if this frame has local bindings.
// The sentinel for "no local environment" is local.keys == nil (zero value).
func (p *EnvironmentFrame) hasLocal() bool {
	return p.local.keys != nil
}

// LocalEnvironment returns the local environment frame, or nil if none.
func (p *EnvironmentFrame) LocalEnvironment() *LocalEnvironmentFrame {
	if !p.hasLocal() {
		return nil
	}
	return &p.local
}

// resolveLocal walks local bindings up the parent chain, calling visitor
// for each binding that matches key and optionally passes scope filtering.
//
// If checkScopes is true, only bindings whose scopes are empty (pre-hygiene)
// or compatible via syntax.ScopesMatch are visited. If checkScopes is false,
// all bindings with the given key are visited (no scope filtering).
//
// The visitor receives the matching binding, its slot index within the
// local frame, and the depth (number of parent frames traversed). Return
// a non-nil value to stop the walk and propagate the result.
func (p *EnvironmentFrame) resolveLocal(
	key *values.Symbol,
	scopes []*syntax.Scope,
	checkScopes bool,
	visitor func(binding *Binding, slot int, depth int) any,
) any {
	env := p
	depth := 0
	for env != nil && env.hasLocal() {
		i, ok := env.local.keys[*key]
		if ok {
			binding := &env.local.bindings[i]
			if !checkScopes || scopesCompatible(binding, scopes) {
				result := visitor(binding, i, depth)
				if result != nil {
					return result
				}
			}
		}
		if env.IsTopLevel() {
			break
		}
		env = env.parent
		depth++
	}
	return nil
}

// scopesCompatible returns true if the binding's scopes are compatible
// with the reference scopes per Flatt's hygiene model.
// A binding with no scopes (pre-hygiene) is always compatible.
// Otherwise, the binding's scopes must be a subset of the reference scopes.
func scopesCompatible(binding *Binding, scopes []*syntax.Scope) bool {
	bs := binding.Scopes()
	if len(bs) == 0 {
		return true
	}
	return syntax.ScopesMatch(scopes, bs)
}

// resolveGlobal walks global bindings up the parent chain.
// The visitor receives the frame and slot index for each matching key.
// Returns the first non-nil visitor result.
// Thread-safe: uses RLock for each frame's global keys/bindings access.
func (p *EnvironmentFrame) resolveGlobal(
	key values.Symbol,
	visitor func(frame *GlobalEnvironmentFrame, slot int) any,
) any {
	ge := p
	for {
		// Lock this frame's global environment for reading
		ge.global.mu.RLock()
		i, ok := ge.global.keys[key]
		if ok {
			// Call visitor while holding lock - visitor may access bindings[i]
			result := visitor(ge.global, i)
			ge.global.mu.RUnlock()
			if result != nil {
				return result
			}
		} else {
			ge.global.mu.RUnlock()
		}

		if ge.IsTopLevel() {
			break
		}
		ge = ge.parent
	}
	return nil
}

// GetBinding returns the binding for the given symbol, searching local
// bindings first (up the parent chain), then global bindings.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetBinding(key *values.Symbol) *Binding {
	// Search locals (no scope filtering)
	result := p.resolveLocal(key, nil, false, func(binding *Binding, _ int, _ int) any {
		return binding
	})
	if result != nil {
		return result.(*Binding)
	}

	// Search globals
	gResult := p.resolveGlobal(*key, func(g *GlobalEnvironmentFrame, i int) any {
		return g.bindings[i]
	})
	if gResult != nil {
		return gResult.(*Binding)
	}
	return nil
}

// GetBindingWithScopes returns the binding for the given symbol that matches
// the provided scopes. This is used for hygienic variable resolution in macros.
// It searches local bindings first (walking up the parent chain), then globals,
// checking scope compatibility at each level.
//
// For hygiene to work correctly with nested bindings of the same name:
//   - Each let-bound variable has scopes from the binding site
//   - A macro free identifier carries scopes from its definition site
//   - We search ALL local bindings (not just innermost) to find one with matching scopes
func (p *EnvironmentFrame) GetBindingWithScopes(key *values.Symbol, scopes []*syntax.Scope) *Binding {
	// Search locals with scope matching
	result := p.resolveLocal(key, scopes, true, func(binding *Binding, _ int, _ int) any {
		return binding
	})
	if result != nil {
		return result.(*Binding)
	}

	// Search globals with scope matching
	gResult := p.resolveGlobal(*key, func(g *GlobalEnvironmentFrame, i int) any {
		binding := g.bindings[i]
		if binding != nil && scopesCompatible(binding, scopes) {
			return binding
		}
		return nil
	})
	if gResult != nil {
		return gResult.(*Binding)
	}

	return nil
}

// EnsureLocalBinding returns the local binding for the given key, creating it if
// it does not already exist. Returns (index, true) if a new binding was created,
// or (index, false) if the binding already existed.
// Returns (nil, false) if the receiver is nil or has no local environment.
func (p *EnvironmentFrame) EnsureLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	if p == nil || !p.hasLocal() {
		return nil, false
	}
	return p.local.EnsureLocalBinding(key, bt)
}

// MaybeCreateLocalBindingWithScopes creates a new local binding with associated scopes in the current local environment.
// It returns the LocalIndex of the new binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateLocalBindingWithScopes(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, source *syntax.SourceContext) (*LocalIndex, bool) {
	if p == nil || !p.hasLocal() {
		return nil, false
	}
	i, ok := p.local.keys[*key]
	if ok {
		// Binding already exists - update scopes/source if needed
		if p.local.bindings[i].Scopes() == nil && scopes != nil {
			p.local.bindings[i].SetScopes(scopes)
		}
		if p.local.bindings[i].Source() == nil && source != nil {
			p.local.bindings[i].SetSource(source)
		}
		return NewLocalIndex(i, 0), false
	}
	i = len(p.local.bindings)
	p.local.keys[*key] = i
	b := Binding{value: values.Void, bindingType: bt}
	if scopes != nil || source != nil {
		b.meta = &BindingMeta{Scopes: scopes, Source: source}
	}
	p.local.bindings = append(p.local.bindings, b)
	return NewLocalIndex(i, 0), true
}

// MaybeCreateLocalBinding creates a new local binding in the current local
// environment or any parent local environment if it does not already exist.
// It returns the LocalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
//
// The parent-chain walk delegates to resolveLocal; creation uses
// EnsureLocalBinding on the innermost frame.
func (p *EnvironmentFrame) MaybeCreateLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	if !p.hasLocal() {
		return nil, false
	}
	// Search existing bindings in current and parent frames
	result := p.resolveLocal(key, nil, false, func(_ *Binding, slot int, depth int) any {
		return NewLocalIndex(slot, depth)
	})
	if result != nil {
		return result.(*LocalIndex), false
	}
	// Not found — create in the current (innermost) frame
	return p.local.EnsureLocalBinding(key, bt)
}

// GetLocalIndex returns the LocalIndex of the binding for the given symbol,
// searching local bindings in the current and parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalIndex(key *values.Symbol) *LocalIndex {
	if p == nil || !p.hasLocal() {
		return nil
	}
	result := p.resolveLocal(key, nil, false, func(_ *Binding, slot int, depth int) any {
		return NewLocalIndex(slot, depth)
	})
	if result != nil {
		return result.(*LocalIndex)
	}
	return nil
}

// HasLocalVariableBinding reports whether sym has a local variable binding
// compatible with the given scopes. This is the shared implementation used by
// both the macro expander (to decide whether a local variable shadows a macro)
// and the validator (to decide whether a local variable shadows a special form).
//
// The check implements Flatt's hygiene rule: a binding matches a reference when
// bindingScopes ⊆ useScopes. Bindings with no scopes (user code) match any use.
// Only BindingTypeVariable bindings are considered; syntax/primitive bindings
// do not shadow.
func (p *EnvironmentFrame) HasLocalVariableBinding(sym *values.Symbol, scopes []*syntax.Scope) bool {
	if p == nil {
		return false
	}
	result := p.resolveLocal(sym, scopes, true, func(binding *Binding, _ int, _ int) any {
		if binding.BindingType() == BindingTypeVariable {
			return true
		}
		return nil
	})
	return result != nil
}

// GetLocalIndexWithScopes returns the LocalIndex of a local binding that
// matches the given scopes.
//
// This implements Flatt's "maximal" binding resolution: among all bindings
// whose scopes are a subset of the reference's scopes, we return the one
// with the LARGEST scope set. This ensures that more specific bindings are
// preferred over less specific ones.
//
// Returns nil if no matching local binding exists.
func (p *EnvironmentFrame) GetLocalIndexWithScopes(
	key *values.Symbol,
	scopes []*syntax.Scope,
) *LocalIndex {
	if p == nil || !p.hasLocal() {
		return nil
	}

	type candidate struct {
		index      *LocalIndex
		scopeCount int
	}
	var best candidate

	p.resolveLocal(key, scopes, true, func(binding *Binding, slot int, depth int) any {
		scopeCount := len(binding.Scopes())

		// Perfect match — stop walking
		if scopeCount > 0 && scopeCount == len(scopes) {
			best = candidate{NewLocalIndex(slot, depth), scopeCount}
			return true
		}

		// Better candidate than current best?
		if best.index == nil || scopeCount > best.scopeCount {
			best = candidate{NewLocalIndex(slot, depth), scopeCount}
		}
		return nil // continue collecting
	})

	return best.index
}

// GetLocalBinding returns the binding for the given LocalIndex.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalBinding(li *LocalIndex) *Binding {
	j := 0
	env := p
	for j < li[1] {
		if env == nil {
			return nil
		}
		env = env.parent
		j++
	}
	if !env.hasLocal() {
		return nil
	}
	return &env.local.bindings[li[0]]
}

// GetLocalBindingByIndex returns the local binding at the given index in the current local environment.
// It does not search parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalBindingByIndex(i int) *Binding {
	return &p.local.bindings[i]
}

// GetLocalBindingBySlotDepth returns the binding at the given slot and depth
// without requiring a *LocalIndex allocation. This is the hot-path variant
// used by the VM's OpLoadLocal dispatch.
func (p *EnvironmentFrame) GetLocalBindingBySlotDepth(slot, depth int) *Binding {
	env := p
	for range depth {
		if env == nil {
			return nil
		}
		env = env.parent
	}
	if env == nil || !env.hasLocal() {
		return nil
	}
	return &env.local.bindings[slot]
}

// SetLocalValue sets the value of the binding for the given LocalIndex.
// It returns an error if the binding does not exist.
func (p *EnvironmentFrame) SetLocalValue(li *LocalIndex, v values.Value) error {
	j := 0
	env := p
	for j < li[1] {
		env = env.parent
		j++
	}
	if !env.hasLocal() {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local binding %q", li)
	}
	env.local.bindings[li[0]].value = v
	return nil
}

// SetLocalValueBySlotDepth sets the value of the binding at the given slot and
// depth without requiring a *LocalIndex allocation. This is the hot-path variant
// used by the VM's OpStoreLocal dispatch.
func (p *EnvironmentFrame) SetLocalValueBySlotDepth(slot, depth int, v values.Value) error {
	env := p
	for range depth {
		if env == nil {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local binding %d:%d", slot, depth)
		}
		env = env.parent
	}
	if env == nil || !env.hasLocal() {
		return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local binding %d:%d", slot, depth)
	}
	env.local.bindings[slot].value = v
	return nil
}

// MaybeCreateOwnGlobalBinding creates a new global binding in the current
// global environment if it does not already exist.
// The key is interned before use (consistent with
// GlobalEnvironmentFrame.CreateGlobalBinding).
// It returns the GlobalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateOwnGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	// Delegate to GlobalEnvironmentFrame's thread-safe method
	return p.global.CreateGlobalBinding(key, bt)
}

// GetGlobalIndex returns the GlobalIndex of the binding for the given symbol,
// searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
//
// The returned GlobalIndex records the specific global frame where the binding
// was found, enabling cross-library macro hygiene (see GlobalIndex.Env).
func (p *EnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	result := p.resolveGlobal(*key, func(g *GlobalEnvironmentFrame, _ int) any {
		gi := NewGlobalIndex(key)
		gi.Env = g
		return gi
	})
	if result != nil {
		return result.(*GlobalIndex)
	}
	return nil
}

// GetGlobalBinding returns the binding for the given GlobalIndex, searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetGlobalBinding(key *GlobalIndex) *Binding {
	result := p.resolveGlobal(*key.Index, func(g *GlobalEnvironmentFrame, i int) any {
		return g.bindings[i]
	})
	if result != nil {
		return result.(*Binding)
	}
	return nil
}

// GetGlobalIndexAcrossPhases searches for a global binding across phases
// (runtime → expand → compile) using read-only phase access. Returns the
// first GlobalIndex found, or nil if not found in any phase.
//
// This is used during macro compilation to resolve free identifiers that may
// be defined in any phase (e.g., define in runtime, define-syntax in expand).
func (p *EnvironmentFrame) GetGlobalIndexAcrossPhases(key *values.Symbol) *GlobalIndex {
	phases := p.phases
	if phases == nil {
		// No phase registry — try runtime only
		return p.GetGlobalIndex(key)
	}

	// Search runtime (phase 0) first, then expand (1), then compile (2)
	for _, phase := range [3]int{PhaseRuntime, PhaseExpand, PhaseCompile} {
		phaseEnv := phases.Get(phase)
		if phaseEnv == nil {
			continue
		}
		gi := phaseEnv.GetGlobalIndex(key)
		if gi != nil {
			return gi
		}
	}
	return nil
}

// GetGlobalIndexFromLibraryScopes searches for a binding by checking each
// scope against the TLE's scope registry. For each scope that maps to a
// library env, performs a cross-phase lookup in that library's env.
// Returns the first match, or nil if no library binding is found.
func (p *EnvironmentFrame) GetGlobalIndexFromLibraryScopes(key *values.Symbol, scopes []*syntax.Scope) *GlobalIndex {
	if p.namespace == nil || len(scopes) == 0 {
		return nil
	}
	for _, scope := range scopes {
		libEnv := p.namespace.LookupLibraryEnv(scope)
		if libEnv == nil {
			continue
		}
		gi := libEnv.GetGlobalIndexAcrossPhases(key)
		if gi != nil {
			return gi
		}
	}
	return nil
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// It returns an error if the binding does not exist.
func (p *EnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	// Delegate to GlobalEnvironmentFrame's thread-safe method
	return p.global.SetOwnGlobalValue(gi, v)
}

// SetGlobalBindingByIndex sets the global binding at the given index in the current global environment.
// It does not search parent environments.
// Thread-safe: uses full Lock for write access.
func (p *EnvironmentFrame) SetGlobalBindingByIndex(i int, bd *Binding) {
	p.global.mu.Lock()
	p.global.bindings[i] = bd
	p.global.mu.Unlock()
}

// Copy creates a deep copy of the environment frame.
// The parent, phase registry, and namespace are shared between the original and the copy.
func (p *EnvironmentFrame) Copy() *EnvironmentFrame {
	q := &EnvironmentFrame{
		parent:     p.parent,
		global:     p.global.Copy(),
		phaseLevel: p.phaseLevel,
		phases:     p.phases,
		namespace:  p.namespace,
	}
	if p.hasLocal() {
		p.local.copyInto(&q.local)
	}
	return q
}

// SchemeString returns a string representation of the environment frame.
func (p *EnvironmentFrame) SchemeString() string {
	return "#<environment>"
}

// IsVoid returns true if the environment frame is nil.
func (p *EnvironmentFrame) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if the environment frame is equal to the given value.
// Two environment frames are equal if their local and global environments are equal,
// and their parent environments are either both nil or equal.
func (p *EnvironmentFrame) EqualTo(value values.Value) bool {
	v, ok := value.(*EnvironmentFrame)
	if !ok {
		return false
	}
	if p == nil || v == nil {
		return p == v
	}
	if !p.local.EqualTo(&v.local) {
		return false
	}
	if !p.global.EqualTo(v.global) {
		return false
	}
	if p.IsTopLevel() || v.IsTopLevel() {
		return p.parent == v.parent
	}
	return p.parent.EqualTo(v.parent)
}

// Namespace returns the Namespace for this frame.
func (p *EnvironmentFrame) Namespace() *Namespace {
	return p.namespace
}
