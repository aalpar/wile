// Copyright 2025 Aaron Alpar
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
)

// EnvironmentFrame represents an environment frame in the hierarchy.
//
// # Type Relationships
//
// The environment system has four types with distinct responsibilities:
//
//	┌─────────────────────────────────────────────────────────────────────────┐
//	│                        TopLevelEnvironment                              │
//	│  (Per-VM instance: owns symbol/syntax interning, phases, libraries)    │
//	│                                                                         │
//	│  symbolInterns ──── map[Symbol]*Symbol (thread-safe, per-instance)     │
//	│  syntaxInterns ──── map[wrt]SyntaxValue (thread-safe)                │
//	│  phases ─────────── *PhaseRegistry                                     │
//	│  libraryRegistry ── any (*machine.LibraryRegistry)                     │
//	│  runtime ────────── *EnvironmentFrame (phase 0)                        │
//	└─────────────────────────────────────────────────────────────────────────┘
//	                                    │
//	                                    │ owns
//	                                    ▼
//	┌─────────────────────────────────────────────────────────────────────────┐
//	│                         EnvironmentFrame                                │
//	│  (Lexical scope node: links local/global bindings, parent chain)       │
//	│                                                                         │
//	│  parent ─────────── *EnvironmentFrame (lexical parent, nil at top)     │
//	│  local ──────────── *LocalEnvironmentFrame (lambda params, let vars)   │
//	│  global ─────────── *GlobalEnvironmentFrame (define bindings)          │
//	│  phaseLevel ─────── int (0=runtime, 1=expand, 2=compile)               │
//	│  phases ─────────── *PhaseRegistry (shared reference)                  │
//	│  topLevel ───────── *TopLevelEnvironment (back-reference)              │
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
//	└───────────────────────────┘    │  topLevel ──── *TopLevelEnvironment    │
//	                                 └────────────────────────────────────────┘
//
// # Ownership and Sharing
//
//   - TopLevelEnvironment: Root owner. One per Wile VM instance.
//   - EnvironmentFrame: Many per VM. Share topLevel and phases references.
//   - GlobalEnvironmentFrame: One per phase. Shares topLevel reference.
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
//	TopLevelEnvironment
//	└── PhaseRegistry
//	    ├── [0] Runtime EnvironmentFrame (normal execution)
//	    ├── [1] Expand EnvironmentFrame (macro expansion, for-syntax)
//	    ├── [2] Compile EnvironmentFrame (syntax compilers, for-meta 2)
//	    └── [-1] Template EnvironmentFrame (for-template, future)
//
// Each phase has its own GlobalEnvironmentFrame but shares the same
// TopLevelEnvironment for symbol/syntax interning.
//
// # Binding Lookup
//
// Two-phase search: first all locals up parent chain, then globals.
type EnvironmentFrame struct {
	// parent links to enclosing lexical scope (nil for TopLevel)
	parent *EnvironmentFrame
	// local holds local bindings for this frame (parameters, let-bound variables)
	local *LocalEnvironmentFrame
	// global holds global bindings for this phase
	global *GlobalEnvironmentFrame
	// phaseLevel indicates which phase this frame represents (0=runtime, 1=expand, etc.)
	phaseLevel int
	// phases is the shared phase registry, owned by TopLevel
	phases *PhaseRegistry
	// topLevel is the owning TopLevelEnvironment (nil for legacy environments)
	topLevel *TopLevelEnvironment
}

// NewTopLevelEnvironmentFrame creates a new top-level global environment frame.
// This frame has no parent and contains the shared symbol/syntax interning maps.
// It also creates the PhaseRegistry for indexed phase access.
//
// Deprecated: Use NewTopLevelEnvironment().Runtime() instead for per-instance
// symbol interning. This function now internally uses NewTopLevelEnvironment()
// to provide proper isolation.
func NewTopLevelEnvironmentFrame() *EnvironmentFrame {
	return NewTopLevelEnvironment().Runtime()
}

// NewEnvironmentFrame creates a new environment frame with the given local and global environment frames.
// The parent field is set to nil. This is typically used for isolated environments.
func NewEnvironmentFrame(local *LocalEnvironmentFrame, global *GlobalEnvironmentFrame) *EnvironmentFrame {
	q := &EnvironmentFrame{
		local:      local,
		global:     global,
		phaseLevel: PhaseRuntime,
		phases:     nil, // No phase registry for isolated environments
	}
	return q
}

// NewEnvironmentFrameWithParent creates a new environment frame with the given local environment frame and parent environment frame.
// The global environment frame is inherited from the parent.
// This is used for creating child frames within a phase (e.g., lambda bodies, let-syntax).
// The phase level, registry, and topLevel are inherited from the parent.
// Panics if parent is nil - use NewTopLevelEnvironmentFrame() instead.
func NewEnvironmentFrameWithParent(local *LocalEnvironmentFrame, parent *EnvironmentFrame) *EnvironmentFrame {
	if parent == nil {
		panic("NewEnvironmentFrameWithParent called with nil parent - use NewTopLevelEnvironmentFrame() instead")
	}
	q := &EnvironmentFrame{
		parent:     parent,
		local:      local,
		global:     parent.global,
		phaseLevel: parent.phaseLevel,
		phases:     parent.phases,
		topLevel:   parent.topLevel,
	}
	return q
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
// The environment must have been created via NewTopLevelEnvironment().
func (p *EnvironmentFrame) AtPhase(phase int) *EnvironmentFrame {
	topLevel := p.TopLevel()
	if topLevel.phases == nil {
		panic("AtPhase called on environment without PhaseRegistry - use NewTopLevelEnvironment()")
	}
	return topLevel.phases.GetOrCreate(phase)
}

// PhaseLevel returns the phase level of this environment frame.
func (p *EnvironmentFrame) PhaseLevel() int {
	return p.phaseLevel
}

// Runtime returns the runtime phase environment (phase 0).
// This is the TopLevel environment where normal bindings live.
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

// LibraryRegistry returns the library registry from the top-level environment.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set.
func (p *EnvironmentFrame) LibraryRegistry() any {
	return p.TopLevel().global.LibraryRegistry()
}

// SetLibraryRegistry sets the library registry on the top-level environment.
// The registry should be a *machine.LibraryRegistry.
func (p *EnvironmentFrame) SetLibraryRegistry(registry any) {
	p.TopLevel().global.SetLibraryRegistry(registry)
}

// LocalEnvironment returns the local environment frame.
func (p *EnvironmentFrame) LocalEnvironment() *LocalEnvironmentFrame {
	return p.local
}

// GetBinding returns the binding for the given symbol, searching for local bindings first, then global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetBinding(key *values.Symbol) *Binding {
	cenv := p
	var (
		i  int
		ok bool
	)
	for cenv.local != nil {
		// always check local first

		i, ok = cenv.local.keys[*key]
		if ok {
			return cenv.local.bindings[i]
		}
		// move to parent
		if cenv.IsTopLevel() {
			break
		}
		cenv = cenv.parent
	}
	for cenv.global != nil {
		// then check global

		i, ok = cenv.global.keys[*key]
		if ok {
			return cenv.global.bindings[i]
		}
		// stop if at top-level
		if cenv.IsTopLevel() {
			break
		}
		cenv = cenv.parent
	}
	return nil
}

// GetIndex returns the index of the binding for the given symbol.
// It returns either a LocalIndex or GlobalIndex depending on where the binding is found.
// The boolean return value indicates whether the binding was found.
// Note: This function has known bugs (skips first frame in loops) and may need fixes.
func (p *EnvironmentFrame) GetIndex(key *values.Symbol) (*LocalIndex, *GlobalIndex, bool) {
	cenv := p
	var (
		i  int
		j  int
		ok bool
	)
	if cenv.local != nil {
		for {
			cenv = cenv.parent
			// always check local first
			i, ok = cenv.local.keys[*key]
			if ok {
				return &LocalIndex{i, j}, nil, true
			}
			j++
			if cenv.IsTopLevel() {
				break
			}
		}
	}
	if cenv.global != nil {
		for {
			cenv = cenv.parent
			// then check global
			_, ok = cenv.global.keys[*key]
			if ok {
				return nil, &GlobalIndex{Index: key}, true
			}
			// stop if at top-level
			if cenv.IsTopLevel() {
				break
			}
		}
	}
	return nil, nil, false
}

// GetBindingWithScopes returns the binding for the given symbol that matches the provided scopes.
// This is used for hygienic variable resolution in macros.
// It searches for local bindings first (walking up the parent chain), then global bindings,
// checking scope compatibility at each level.
//
// For hygiene to work correctly with nested bindings of the same name:
//   - Each let-bound variable has scopes from the binding site
//   - A macro free identifier carries scopes from its definition site
//   - We search ALL local bindings (not just innermost) to find one with matching scopes
func (p *EnvironmentFrame) GetBindingWithScopes(key *values.Symbol, scopes []*syntax.Scope) *Binding {
	// Search local bindings in parent chain, checking scopes at each level
	// This is critical for hygiene: inner bindings may not match the reference's scopes,
	// but an outer binding might.
	env := p
	for env != nil && env.local != nil {
		if i, ok := env.local.keys[*key]; ok {
			binding := env.local.bindings[i]
			if binding != nil {
				// Check if scopes match
				if binding.Scopes() == nil || len(binding.Scopes()) == 0 {
					// Binding has no scopes (top-level or pre-hygiene), accept it
					return binding
				}
				// Check scope compatibility using ScopesMatch from scope_utils
				if syntax.ScopesMatch(scopes, binding.Scopes()) {
					return binding
				}
				// Scopes don't match - continue searching parent frames
			}
		}
		if env.IsTopLevel() {
			break
		}
		env = env.parent
	}

	// Then try global bindings
	ge := p
	i, ok := ge.global.keys[*key]
	for !ok && !ge.IsTopLevel() {
		ge = ge.parent
		i, ok = ge.global.keys[*key]
	}
	if ok {
		binding := ge.global.bindings[i]
		if binding != nil {
			// Check if scopes match
			if binding.Scopes() == nil || len(binding.Scopes()) == 0 {
				// Binding has no scopes (top-level or pre-hygiene), accept it
				return binding
			}
			// Check scope compatibility
			if syntax.ScopesMatch(scopes, binding.Scopes()) {
				return binding
			}
		}
	}

	return nil
}

// CreateLocalBinding creates a new local binding in the current local environment.
// It returns the LocalIndex of the new binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) CreateLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	if p == nil || p.local == nil {
		return nil, false
	}
	return p.local.CreateLocalBinding(key, bt)
}

// MaybeCreateLocalBindingWithScopes creates a new local binding with associated scopes in the current local environment.
// It returns the LocalIndex of the new binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateLocalBindingWithScopes(key *values.Symbol, bt BindingType, scopes []*syntax.Scope) (*LocalIndex, bool) {
	if p == nil || p.local == nil {
		return nil, false
	}
	i, ok := p.local.keys[*key]
	if ok {
		// Binding already exists - update scopes if needed
		binding := p.local.bindings[i]
		if binding.Scopes() == nil && scopes != nil {
			binding.SetScopes(scopes)
		}
		return NewLocalIndex(i, 0), false
	}
	i = len(p.local.bindings)
	p.local.keys[*key] = i
	p.local.bindings = append(p.local.bindings, NewBindingWithScopes(values.Void, bt, scopes))
	return NewLocalIndex(i, 0), true
}

// MaybeCreateLocalBinding creates a new local binding in the current local environment or any parent local environment if it does not already exist.
// It returns the LocalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	env := p
	if env.local == nil {
		return nil, false
	}
	ks := env.local.keys
	i, ok := ks[*key]
	j := 0
	for !ok && !env.IsTopLevel() && env.parent.local != nil {
		env = env.parent
		ks = env.local.keys
		i, ok = ks[*key]
		j++
	}
	if ok {
		return NewLocalIndex(i, j), false
	}
	i = len(p.local.bindings)
	p.local.keys[*key] = i
	p.local.bindings = append(p.local.bindings, NewBinding(values.Void, bt))
	return NewLocalIndex(i, 0), true
}

// GetLocalIndex returns the LocalIndex of the binding for the given symbol, searching local bindings in the current and parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalIndex(key *values.Symbol) *LocalIndex {
	if p == nil || p.local == nil {
		return nil
	}
	env := p
	ks := env.local.keys
	i, ok := ks[*key]
	j := 0
	for !ok && !env.IsTopLevel() && env.parent.local != nil {
		env = env.parent
		ks = env.local.keys
		i, ok = ks[*key]
		j++
	}
	if !ok {
		return nil
	}
	return NewLocalIndex(i, j)
}

// GetLocalIndexWithScopes returns the LocalIndex of a local binding that matches the given scopes.
// This implements Flatt's "maximal" binding resolution: among all bindings whose scopes
// are a subset of the reference's scopes, we return the one with the LARGEST scope set.
// This ensures that more specific bindings are preferred over less specific ones.
// Returns nil if no matching local binding exists.
func (p *EnvironmentFrame) GetLocalIndexWithScopes(key *values.Symbol, scopes []*syntax.Scope) *LocalIndex {
	if p == nil || p.local == nil {
		return nil
	}

	// Collect all matching bindings with their scope counts
	type candidate struct {
		index      *LocalIndex
		scopeCount int
	}
	var candidates []candidate

	env := p
	j := 0
	for env != nil && env.local != nil {
		if i, ok := env.local.keys[*key]; ok {
			binding := env.local.bindings[i]
			if binding != nil {
				bindingScopes := binding.Scopes()
				// Check if scopes match
				if len(bindingScopes) == 0 {
					// Binding has no scopes (top-level or pre-hygiene)
					// This is a valid candidate with scope count 0
					candidates = append(candidates, candidate{NewLocalIndex(i, j), 0})
				} else if syntax.ScopesMatch(scopes, bindingScopes) {
					// Scopes match - count how many scopes are in common
					// (which equals len(bindingScopes) since it's a subset)
					candidates = append(candidates, candidate{NewLocalIndex(i, j), len(bindingScopes)})
				}
				// If scopes don't match, skip this binding
			}
		}
		if env.IsTopLevel() {
			break
		}
		env = env.parent
		j++
	}

	if len(candidates) == 0 {
		return nil
	}

	// Find the candidate with the maximum scope count (most specific binding)
	best := candidates[0]
	for _, c := range candidates[1:] {
		if c.scopeCount > best.scopeCount {
			best = c
		}
	}

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
	if env.local == nil {
		return nil
	}
	i := li[0]
	q := env.local.bindings[i]
	return q
}

// GetLocalBindingByIndex returns the local binding at the given index in the current local environment.
// It does not search parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalBindingByIndex(i int) *Binding {
	return p.local.bindings[i]
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
	if env.local == nil {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such local binding %q", li)
	}
	i := li[0]
	bd := env.local.bindings[i]
	bd.value = v
	return nil
}

// CreateGlobalBinding creates a new global binding in the current global environment.
// It returns the GlobalIndex of the new binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	r := p
	_, ok := r.global.keys[*key]
	if ok {
		q := NewGlobalIndex(key)
		return q, false
	}
	i := len(p.global.bindings)
	p.global.keys[*key] = i
	// append the new binding at index i
	p.global.SetBindings(append(p.global.Bindings(), NewBinding(values.Void, bt)))
	q := NewGlobalIndex(key)
	return q, true
}

// MaybeCreateOwnGlobalBinding creates a new global binding in the current or parent global environments if it does not already exist.
// It returns the GlobalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateOwnGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	_, ok := p.global.keys[*key]
	if ok {
		return NewGlobalIndex(key), false
	}
	i := len(p.global.bindings)
	p.global.keys[*key] = i
	p.global.SetBindings(append(p.global.Bindings(), NewBinding(values.Void, bt)))
	q := NewGlobalIndex(key)
	return q, true
}

// GetGlobalIndex returns the GlobalIndex of the binding for the given symbol, searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
// The returned GlobalIndex records the specific global frame where the binding
// was found, enabling cross-library macro hygiene (see GlobalIndex.Env).
func (p *EnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	ge := p
	_, ok := ge.global.keys[*key]
	for !ok && !ge.IsTopLevel() {
		ge = ge.parent
		_, ok = ge.global.keys[*key]
	}
	if !ok {
		return nil
	}
	gi := NewGlobalIndex(key)
	gi.Env = ge.global
	return gi
}

// GetGlobalBinding returns the binding for the given GlobalIndex, searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetGlobalBinding(key *GlobalIndex) *Binding {
	ge := p
	i, ok := ge.global.keys[*key.Index]
	for !ok && !ge.IsTopLevel() {
		ge = ge.parent
		i, ok = ge.global.keys[*key.Index]
	}
	if !ok {
		return nil
	}
	return ge.global.bindings[i]
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// It returns an error if the binding does not exist.
func (p *EnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p
	i, ok := ge.global.keys[*gi.Index]
	if !ok {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such global binding %q", gi.Index)
	}
	ge.global.bindings[i].value = v
	return nil
}

// SetGlobalBindingByIndex sets the global binding at the given index in the current global environment.
// It does not search parent environments.
func (p *EnvironmentFrame) SetGlobalBindingByIndex(i int, bd *Binding) {
	p.global.bindings[i] = bd
}

// Copy creates a deep copy of the environment frame.
// The parent, phase registry, and topLevel are shared between the original and the copy.
func (p *EnvironmentFrame) Copy() *EnvironmentFrame {
	q := &EnvironmentFrame{
		parent:     p.parent,
		local:      p.local.Copy().(*LocalEnvironmentFrame),
		global:     p.global.Copy().(*GlobalEnvironmentFrame),
		phaseLevel: p.phaseLevel,
		phases:     p.phases,
		topLevel:   p.topLevel,
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
	if !p.local.EqualTo(v.local) {
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

// InternSymbol interns the given symbol.
// Delegates to the TopLevelEnvironment for this frame.
// Per R7RS §6.5: "Two symbols are identical (in the sense of eq?) if and only
// if their names are spelled the same way."
// Panics if topLevel is nil (legacy environments no longer supported).
func (p *EnvironmentFrame) InternSymbol(q *values.Symbol) *values.Symbol {
	if p.topLevel == nil {
		panic("InternSymbol called on environment without TopLevelEnvironment - use NewTopLevelEnvironment()")
	}
	return p.topLevel.InternSymbol(q)
}

// TopLevelEnv returns the TopLevelEnvironment for this frame.
// Returns nil for legacy environments created without TopLevelEnvironment.
func (p *EnvironmentFrame) TopLevelEnv() *TopLevelEnvironment {
	return p.topLevel
}

// InternSyntax interns the given syntax value.
// Delegates to the TopLevelEnvironment for this frame.
// Panics if topLevel is nil (legacy environments no longer supported).
func (p *EnvironmentFrame) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	if p.topLevel == nil {
		panic("InternSyntax called on environment without TopLevelEnvironment - use NewTopLevelEnvironment()")
	}
	return p.topLevel.InternSyntax(k, v)
}
