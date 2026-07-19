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
	"math"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
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
//	│  libraryRegistry ── LibrarySearcher (*compilation.LibraryRegistry)       │
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
//	│  phaseLevel ─────── Phase (-1=template, 0=runtime, 1=expand, 2=compile) │
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
//	│  keys ── map[Symbol][]int │    │  keys ──────── map[Symbol]int          │
//	│  bindings ── []*Binding   │    │  bindings ──── []*Binding              │
//	└───────────────────────────┘    └────────────────────────────────────────┘
//
// # Ownership and Sharing
//
//   - Namespace: Root owner. One per Wile VM instance.
//   - EnvironmentFrame: Many per VM. Share namespace and phases references.
//   - GlobalEnvironmentFrame: One per phase. Owned by EnvironmentFrame; no
//     direct Namespace back-reference (reach Namespace via the owning frame).
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
	// phaseLevel indicates which phase this frame represents
	// (PhaseTemplate=-1, PhaseRuntime=0, PhaseExpand=1, PhaseCompile=2)
	phaseLevel Phase
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
// parent chain is set from the source's parent. It is the allocating
// counterpart of InitApplyFrame (the pooling-friendly form); both share the
// same parent-copy logic.
func (p *EnvironmentFrame) NewApplyFrame() *EnvironmentFrame {
	q := &EnvironmentFrame{}
	p.InitApplyFrame(q)
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
func (p *EnvironmentFrame) AtPhase(phase Phase) *EnvironmentFrame {
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
func (p *EnvironmentFrame) PhaseLevel() Phase {
	return p.phaseLevel
}

// Runtime returns the runtime phase environment (phase 0).
// This is the root environment where normal bindings live.
func (p *EnvironmentFrame) Runtime() *EnvironmentFrame {
	return p.AtPhase(PhaseRuntime)
}

// MutableRuntime returns the per-Engine MUTABLE runtime global of this frame's
// namespace — the user top level where user defines land and where eval/load and
// SRFI-18 threads store top-level state. It is the lexical CHILD of the immutable
// sealed base; resolution from it reaches sealed primitives via the parent walk.
//
// Use this, NOT TopLevel(), when a primitive needs the frame for user-visible
// top-level mutations: after the layered-environment carve TopLevel() returns the
// immutable sealed-base root (home of the optimizer's Stable anchors), so storing a
// user define or thread state through TopLevel() would target the frozen base. This
// names the recurring intent that was previously spelled `.Namespace().Runtime()` at
// every call site. (It resolves the namespace's runtime, which for a flat library
// frame is the engine's mutable global rather than the library's own transient frame —
// unlike the receiver-relative Runtime().)
func (p *EnvironmentFrame) MutableRuntime() *EnvironmentFrame {
	return p.namespace.Runtime()
}

// MutableRuntimeOrNil resolves the namespace's mutable runtime by walking the
// lexical parent chain, returning nil if no frame in the chain carries a namespace
// (rather than panicking like MutableRuntime). Some transient execution frames — a
// procedure body frame entered while running a call-with-values producer, say — are
// detached (nil parent, nil namespace); their owning namespace is only reachable via
// the MachineContext's parentMC, not the lexical chain. NewSubContext uses this to
// fall back to the parent context when the local env cannot resolve a namespace.
func (p *EnvironmentFrame) MutableRuntimeOrNil() *EnvironmentFrame {
	for e := p; e != nil; e = e.parent {
		if e.namespace != nil {
			return e.namespace.Runtime()
		}
	}
	return nil
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

// NextPhaseChecked returns the sibling frame one phase up from base. The climb
// is computed in int and rejected if it leaves the int8 phase range, so a
// runaway self-referential macro hits a wrapped error instead of overflowing
// int8 (127+1 -> -128). base is explicit (not p.phaseLevel) so the ceiling is
// testable without constructing a phase-127 frame.
func (p *EnvironmentFrame) NextPhaseChecked(base Phase) (*EnvironmentFrame, error) {
	next := int(base) + 1
	if next > int(math.MaxInt8) {
		return nil, werr.WrapForeignErrorf(werr.ErrInvalidArgument,
			"NextPhaseChecked: phase %d+1 exceeds int8 ceiling", int(base))
	}
	return p.AtPhase(Phase(next)), nil
}

// NextPhase returns the sibling frame one phase up from this frame's own level.
// Climbing the macro tower: a transformer body compiled against this frame
// expands as phase (phaseLevel+1) code, so define-syntax storage and macro
// lookup relative to it climb rather than collapsing into the single expand
// phase. At phaseLevel 0 this equals Expand(), so top-level behavior is
// unchanged (level-0 identity). Panics (wrapped) only on the impossible int8
// overflow, which NextPhaseChecked rejects.
func (p *EnvironmentFrame) NextPhase() *EnvironmentFrame {
	q, err := p.NextPhaseChecked(p.phaseLevel)
	if err != nil {
		panic(werr.WrapForeignErrorf(err, "NextPhase: phase index overflow"))
	}
	return q
}

// Parent returns the parent environment frame.
func (p *EnvironmentFrame) Parent() *EnvironmentFrame {
	return p.parent
}

// GlobalEnvironment returns the global environment frame.
func (p *EnvironmentFrame) GlobalEnvironment() *GlobalEnvironmentFrame {
	return p.global
}

// The five methods below — FileResolver, SetFileResolver, LibraryRegistry,
// SetLibraryRegistry, LoadPathStack — are ergonomic shortcuts for
// p.Namespace().X(). The state itself is owned by the Namespace (and,
// for FileResolver / LoadPathStack, ultimately delegated to the root
// namespace per the field-inheritance policy documented on the
// Namespace type). EnvironmentFrame does not store these values; the
// methods exist only to spare common call sites the
//
//	frame.Namespace().FileResolver()
//
// dance.
//
// Nil-namespace handling: getters return zero values on a nil-namespace
// frame because reads on un-namespaced frames are benign and have a
// well-defined "no value here" answer. Setters PANIC: an
// un-namespaced frame has no configuration storage, so a setter call
// would silently disappear and that is a programmer error worth
// surfacing immediately rather than at the next failed read. Frames
// built outside the standard constructors (e.g. newEnvironmentFrame
// test fixtures) must not be configured through these shortcuts.
//
// When adding a new Namespace-owned capability that callers reach via
// EnvironmentFrame, follow the same pattern: thin pass-through here,
// authoritative storage on Namespace.

// FileResolver returns the file resolver. Shortcut for
// p.Namespace().FileResolver(); see the comment block above.
func (p *EnvironmentFrame) FileResolver() FileResolver {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.FileResolver()
}

// SetFileResolver sets the file resolver. Shortcut for
// p.Namespace().SetFileResolver(); see the comment block above.
// Panics if the frame has no namespace (configuration on an
// un-namespaced frame would be silently dropped — a programmer error).
func (p *EnvironmentFrame) SetFileResolver(resolver FileResolver) {
	if p.namespace == nil {
		panic(werr.WrapForeignErrorf(werr.ErrUnexpectedNil,
			"EnvironmentFrame.SetFileResolver: frame has no namespace"))
	}
	p.namespace.SetFileResolver(resolver)
}

// LibraryRegistry returns the library registry. Shortcut for
// p.Namespace().LibraryRegistry(); see the comment block above.
// Callers needing the full *compilation.LibraryRegistry can type-assert.
func (p *EnvironmentFrame) LibraryRegistry() LibrarySearcher {
	if p.namespace == nil {
		return nil
	}
	return p.namespace.LibraryRegistry()
}

// SetLibraryRegistry sets the library registry. Shortcut for
// p.Namespace().SetLibraryRegistry(); see the comment block above.
// Panics if the frame has no namespace (see SetFileResolver).
func (p *EnvironmentFrame) SetLibraryRegistry(registry LibrarySearcher) {
	if p.namespace == nil {
		panic(werr.WrapForeignErrorf(werr.ErrUnexpectedNil,
			"EnvironmentFrame.SetLibraryRegistry: frame has no namespace"))
	}
	p.namespace.SetLibraryRegistry(registry)
}

// LoadPathStack returns the load path tracker. Shortcut for
// p.Namespace().LoadPathStack(); see the comment block above.
func (p *EnvironmentFrame) LoadPathStack() PathTracker {
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
// for each binding that matches key and passes scope filtering.
//
// Nil scopes means "match any" — this replaces the former checkScopes=false
// pattern. Non-nil scopes (even empty) are checked via ScopesCompatible.
//
// When a key maps to multiple slots (same-name bindings with different
// scope sets from hygienic expansion), all compatible slots are visited.
//
// The visitor receives the matching binding, its slot index within the
// local frame, and the depth (number of parent frames traversed). Return
// a non-nil value to stop the walk and propagate the result.
func (p *EnvironmentFrame) resolveLocal(
	key *values.Symbol,
	scopes []*syntax.Scope,
	visitor func(binding *Binding, slot int, depth int) any,
) any {
	env := p
	depth := 0
	matchAny := scopes == nil
	for env != nil && env.hasLocal() {
		for _, i := range env.local.keys[*key] {
			binding := &env.local.bindings[i]
			if matchAny || syntax.ScopesCompatible(binding.Scopes(), scopes) {
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

// resolveGlobal walks global bindings up the parent chain.
// The visitor receives the frame and slot index for each matching key.
// Returns the first non-nil visitor result.
// Thread-safe: uses RLock for each frame's global keys/bindings access.
// Within a frame the best scope-set match wins (Flatt's maximal resolution);
// across frames the first frame yielding any match wins, which is what preserves
// shadowing of a sealed-base binding by a user redefinition.
//
// matchAny selects any binding of the name regardless of scopes. It is NOT the
// same as passing nil scopes, which means the empty scope set: a reference
// written outside any macro expansion must not reach a binder introduced inside
// one.
func (p *EnvironmentFrame) resolveGlobal(
	key values.Symbol,
	scopes []*syntax.Scope,
	matchAny bool,
	visitor func(frame *GlobalEnvironmentFrame, slot int) any,
) any {
	ge := p
	for {
		// Lock this frame's global environment for reading
		ge.global.mu.RLock()
		i, ok := ge.global.bestSlotLocked(key, scopes, matchAny)
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

// GetBinding returns the binding for the given symbol that matches the
// provided scopes. It searches local bindings first (walking up the parent
// chain), then globals.
//
// Nil scopes means "match any" (no scope filtering). Non-nil scopes enables
// hygienic resolution per Flatt's model with maximal binding selection
// (consistent with GetLocalIndex).
func (p *EnvironmentFrame) GetBinding(key *values.Symbol, scopes []*syntax.Scope) *Binding {
	if scopes == nil {
		// Fast path: nil scopes — return first match
		result := p.resolveLocal(key, nil, func(binding *Binding, _ int, _ int) any {
			return binding
		})
		if result != nil {
			return result.(*Binding)
		}
	} else {
		// Scoped path: maximal binding resolution (Flatt model).
		// See bestOf in best_of.go. Allocation here is trivial — the
		// candidate is just the existing *Binding pointer — so we record
		// unconditionally on shouldRecord = true.
		var best bestOf[*Binding]
		target := len(scopes)
		p.resolveLocal(key, scopes, func(binding *Binding, _ int, _ int) any {
			sc := len(binding.Scopes())
			rec, done := best.shouldRecord(sc, target)
			if rec {
				best.record(binding, sc)
			}
			if done {
				return true
			}
			return nil
		})

		item, ok := best.Result()
		if ok {
			return item
		}
	}

	// The scope filter now lives in bestSlotLocked, which both selects the
	// maximal match and rejects incompatible candidates.
	gResult := p.resolveGlobal(*key, scopes, scopes == nil, func(g *GlobalEnvironmentFrame, i int) any {
		binding := g.bindings[i]
		if binding != nil {
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

// MaybeCreateLocalBinding creates a local binding with scope-aware
// deduplication. Two bindings with the same key but incompatible scopes
// get separate slots; compatible scopes reuse the existing slot.
//
// Nil scopes means "match any" during dedup (pre-hygiene callers).
// Returns (index, true) if created, (index, false) if already existed.
func (p *EnvironmentFrame) MaybeCreateLocalBinding(
	key *values.Symbol, bt BindingType,
	scopes []*syntax.Scope, source *syntax.SourceContext,
) (*LocalIndex, bool) {
	if p == nil || !p.hasLocal() {
		return nil, false
	}
	return p.local.MaybeCreateLocalBinding(key, bt, scopes, source)
}

// GetLocalIndex returns the LocalIndex of the binding for the given symbol
// that matches the given scopes. Nil scopes means "match any".
//
// When scopes are provided, this implements Flatt's "maximal" binding
// resolution: among all bindings whose scopes are a subset of the
// reference's scopes, the one with the LARGEST scope set is returned.
//
// Returns nil if no matching local binding exists.
func (p *EnvironmentFrame) GetLocalIndex(key *values.Symbol, scopes []*syntax.Scope) *LocalIndex {
	if p == nil || !p.hasLocal() {
		return nil
	}

	// Fast path: nil scopes — return first match (no maximal resolution needed)
	if scopes == nil {
		result := p.resolveLocal(key, nil, func(_ *Binding, slot int, depth int) any {
			return NewLocalIndex(slot, depth)
		})
		if result != nil {
			return result.(*LocalIndex)
		}
		return nil
	}

	// Scoped path: maximal binding resolution.
	// See bestOf in best_of.go. Splitting shouldRecord/record lets us
	// defer NewLocalIndex(slot, depth) — an allocation — to the cases
	// where the candidate actually becomes the new best, instead of
	// allocating on every parent-chain visit.
	var best bestOf[*LocalIndex]
	target := len(scopes)
	p.resolveLocal(key, scopes, func(binding *Binding, slot int, depth int) any {
		sc := len(binding.Scopes())
		rec, done := best.shouldRecord(sc, target)
		if rec {
			best.record(NewLocalIndex(slot, depth), sc)
		}
		if done {
			return true
		}
		return nil
	})

	item, _ := best.Result()
	return item
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
	result := p.resolveLocal(sym, scopes, func(binding *Binding, _ int, _ int) any {
		if binding.BindingType() == BindingTypeVariable {
			return true
		}
		return nil
	})
	return result != nil
}

// GetLocalBinding returns the binding for the given LocalIndex.
// It returns nil if the binding does not exist.
func (p *EnvironmentFrame) GetLocalBinding(li *LocalIndex) *Binding {
	env := p
	for range li[1] {
		if env == nil {
			return nil
		}
		env = env.parent
	}
	// env == nil guard is required in addition to the in-loop check: a depth
	// that consumes the final frame leaves env nil at loop exit, and hasLocal
	// dereferences its receiver. Mirrors GetLocalBindingBySlotDepth.
	if env == nil || !env.hasLocal() {
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
	env := p
	for range li[1] {
		if env == nil {
			return werr.WrapForeignErrorf(werr.ErrNoSuchBinding, "no such local binding %q", li)
		}
		env = env.parent
	}
	// env == nil guard mirrors SetLocalValueBySlotDepth: a caller-built
	// LocalIndex whose depth walks past the frame chain must return an error,
	// not panic in hasLocal on a nil frame.
	if env == nil || !env.hasLocal() {
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
//
// scopes become part of the binding's identity in the frame; a nil set is the
// ordinary user-written top-level define.
func (p *EnvironmentFrame) MaybeCreateOwnGlobalBinding(key *values.Symbol, bt BindingType, scopes []*syntax.Scope) (*GlobalIndex, bool) {
	// Delegate to GlobalEnvironmentFrame's thread-safe method
	return p.global.CreateGlobalBinding(key, bt, scopes)
}

// GetGlobalIndex returns the GlobalIndex of the binding for the given symbol,
// searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
//
// The returned GlobalIndex records the specific global frame where the binding
// was found, enabling cross-library macro hygiene (see GlobalIndex.Env).
//
// This is the WILDCARD form — see GlobalEnvironmentFrame.GetGlobalIndex.
// Compiler callers want GetGlobalIndexWithScopes.
func (p *EnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	result := p.resolveGlobal(*key, nil, true, func(g *GlobalEnvironmentFrame, i int) any {
		return newResolvedGlobalIndex(key, g, i)
	})
	if result != nil {
		return result.(*GlobalIndex)
	}
	return nil
}

// GetGlobalIndexWithScopes is GetGlobalIndex with hygienic resolution: the
// binding whose scope set maximally matches scopes wins. A nil scopes slice
// means the empty scope set, not "any".
func (p *EnvironmentFrame) GetGlobalIndexWithScopes(key *values.Symbol, scopes []*syntax.Scope) *GlobalIndex {
	result := p.resolveGlobal(*key, scopes, false, func(g *GlobalEnvironmentFrame, i int) any {
		return newResolvedGlobalIndex(key, g, i)
	})
	if result != nil {
		return result.(*GlobalIndex)
	}
	return nil
}

// GetGlobalBinding returns the binding for the given GlobalIndex, searching global bindings in the current and parent environments.
// It returns nil if the binding does not exist.
// A deferred index (Env == nil) carries the reference's scope set, so this
// execution-time walk resolves hygienically rather than by bare name.
func (p *EnvironmentFrame) GetGlobalBinding(key *GlobalIndex) *Binding {
	result := p.resolveGlobal(*key.Index, key.Scopes, key.Scopes == nil, func(g *GlobalEnvironmentFrame, i int) any {
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
//
// The phase-0 (runtime) search reaching the mutable runtime frame's OWN defines
// is DELIBERATE and load-bearing — it is NOT the accidental parent-chain leak
// the phase-frame reparent (createPhaseEnv) closed, and must NOT be routed
// through SealedBaseTarget() to "seal" it. A macro-generating-macro introduces
// a phase-0 define that a generated inner macro references by scope-aware
// identifier; only searching the runtime frame resolves that intro-scoped
// binding at compile time. Sealing it breaks R7RS §4.3 referential transparency
// — concretely, the jabberwocky/march-hare case in
// integration/testdata/r7rs-tests.scm:
//
//	(define-syntax jabberwocky
//	  (syntax-rules ()
//	    ((_ hatter)
//	     (begin (define march-hare 42)
//	            (define-syntax hatter (syntax-rules () ((_) march-hare)))))))
//	(jabberwocky mad-hatter) (mad-hatter)  ; => 42; sealing gives "no such binding march-hare"
//
// (Verified 2026-07-10: hermeticizing the phase-0 search passes the
// compilation/machine/wile suites but fails the integration R7RS conformance
// suite here. Investigated as a possible "second hermeticity hole"; it is not.)
func (p *EnvironmentFrame) GetGlobalIndexAcrossPhases(key *values.Symbol) *GlobalIndex {
	phases := p.phases
	if phases == nil {
		// No phase registry — try runtime only
		return p.GetGlobalIndex(key)
	}

	// Search runtime (phase 0) first, then expand (1), then compile (2)
	for _, phase := range [3]Phase{PhaseRuntime, PhaseExpand, PhaseCompile} {
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
	// Maintain the "in a global frame => has an atomicCell" invariant: any
	// binding published here becomes thread-shared via the lock-free cache.
	// Migrate before it is reachable from p.global.bindings (still unraced).
	bd.ensureGlobalCell()
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

// SchemeString returns a Scheme-level string for this environment frame.
// EnvironmentFrame reaches the value plumbing because closures capture
// environments and store them as template literals (see
// machine.NativeTemplate.MaybeAppendLiteral); this method exists to satisfy
// values.Value, not because environment frames are ever printed by Scheme
// programs.
func (p *EnvironmentFrame) SchemeString() string {
	return "#<environment>"
}

// IsVoid reports whether this environment frame pointer is nil.
// Required by values.Value (see SchemeString comment).
func (p *EnvironmentFrame) IsVoid() bool {
	return p == nil
}

// EqualTo implements values.Value. R7RS §6.12 specifies that environments
// compare by eq? (pointer identity), not by structural equality of their
// bindings — the prior structural implementation was a latent correctness
// trap that no caller actually exercised. Use pointer identity here.
func (p *EnvironmentFrame) EqualTo(value values.Value) bool {
	v, ok := value.(*EnvironmentFrame)
	if !ok {
		return false
	}
	return p == v
}

// Namespace returns the Namespace for this frame.
func (p *EnvironmentFrame) Namespace() *Namespace {
	return p.namespace
}

// SealedBaseTarget returns the frame that should receive sealed (immutable) runtime
// bindings — primitives and bootstrap procedures — when a registry is applied with this
// frame as its target. For a namespace-owning runtime frame (this frame == its
// namespace's Runtime()) that is the namespace's sealed base; for a flat library frame
// (NewChildRuntime, which shares its parent's namespace and has no sealed-base parent to
// reach) it is the frame itself. This single predicate keeps the carve decision in one
// place across the engine-root, profile-child, and library-env apply paths.
func (p *EnvironmentFrame) SealedBaseTarget() *EnvironmentFrame {
	ns := p.namespace
	if ns != nil && ns.runtime == p {
		return ns.sealedBase
	}
	return p
}
