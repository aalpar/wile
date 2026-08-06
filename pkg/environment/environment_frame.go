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
	"slices"

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
//	│  libraryRegistry ── LibrarySearcher (*compilation.LibraryRegistry)      │
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
//	│  (Single scope bindings)  │    │  (The owner's whole binding store)     │
//	│                           │    │                                        │
//	│  keys ── map[Symbol][]int │    │  keys ────── map[Symbol][]slotRef      │
//	│  bindings ── []Binding    │    │  bindings ──── []*Binding              │
//	└───────────────────────────┘    └────────────────────────────────────────┘
//
// # Ownership and Sharing
//
//   - Namespace: Root owner. One per Wile VM instance.
//   - EnvironmentFrame: Many per VM. Share namespace and phases references.
//   - GlobalEnvironmentFrame: One per OWNER, not one per phase. Every phase view
//     and every sealed-write view is a thin frame over the SAME store, differing
//     only in the (phase, rank) coordinates its reads probe and its writes stamp.
//     No direct Namespace back-reference (reach Namespace via the owning frame).
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
// These are VIEWS, not owners: every entry shares the one GlobalEnvironmentFrame
// and the one Namespace. Phase separation is key disjointness in that store — a
// phase-N read is a candidate only against slots at exactly phase N or the
// ambient coordinate — not a per-phase store and not a parent link.
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
	// rank says which tier a write through this frame lands in. Every sealed-write
	// view (newPhaseRegistry mints one per sealedAxis row) carries writeRankSealed;
	// every other frame is the zero value writeRankMutable. Placed next to
	// phaseLevel: both are single bytes that share the padding already required
	// before the next pointer field, so this costs nothing in EnvironmentFrame's
	// size (see layout_size_test.go).
	rank writeRank
	// phases is the shared phase registry, owned by Namespace
	phases *PhaseRegistry
	// namespace is the owning Namespace
	namespace *Namespace
}

// writeRank says which tier a write through this frame lands in. The zero value
// is mutable — every ordinary frame; the sealed rank is carried only by the
// sealed-write views — permanent entries in PhaseRegistry.sealedViews, one per
// sealedAxis row, that every registration writes through (there are no seal
// frames; that topology is gone). It is a property of the VIEW, not an
// argument, because LoadBootstrapCore COMPILES stdlib Scheme whose defines
// must land sealed, and the compiler cannot thread a sealing parameter through
// define compilation (design Q2, decided by force majeure).
type writeRank uint8

const (
	writeRankMutable writeRank = iota
	writeRankSealed
)

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
// The phase level, registry, and namespace are inherited from the parent. rank is
// NOT inherited — the new frame is always writeRankMutable (the zero value),
// because a lexical child (a lambda body) is never a registration target, only a
// sealed-write view is.
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
	p.InitApplyFrameWithParent(dst, p.parent)
}

// InitApplyFrameWithParent is InitApplyFrame with the runtime parent supplied
// separately, for callers that hold the parameter SHAPE and the parent as two
// values rather than as one materialized frame. A closure built by
// OpMakeClosure is exactly that: p is the lambda's compile-time frame (a
// template constant shared by every evaluation) and parent is the runtime
// environment captured at closure creation. Splitting them here is what lets
// MachineClosure skip materializing an intermediate frame per closure.
//
// Only p.local is read off p. Everything else (global, phase, namespace) comes
// from parent, which is why substituting a different parent is sound.
func (p *EnvironmentFrame) InitApplyFrameWithParent(dst *EnvironmentFrame, parent *EnvironmentFrame) {
	if parent == nil {
		panic(werr.WrapForeignErrorf(
			werr.ErrNilParentEnvironment,
			"InitApplyFrameWithParent: nil parent - a closure's apply frame must hang from a runtime environment",
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

// TopLevel returns the frame the lexical parent chain terminates at: the PHASE
// VIEW this frame's scope chain hangs from. Views have no parent — the store
// fold removed the layer edges the chain used to climb — so this is a purely
// structural "which top level am I under" walk.
//
// It answers "the owner's view at MY phase", not "where user defines land":
// under a phase-1 lexical chain it is the phase-1 view, and under a bootstrap
// closure it is the sealed-write root. Use MutableRuntime() when the question is
// the namespace's user global.
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
// A climb rooted at a SEALED-WRITE view stays sealed wherever the target phase has
// a sealed-write view of its own; every other receiver, and every phase without
// one, resolves through the shared PhaseRegistry. That is what routes a
// bootstrap-macro define-syntax (compiled with env == the sealed-write root, so
// NextPhase() lands here at phase 1) into the phase-1 sealed-write view, while user
// code (env == the ordinary root view) writes the mutable tier. Above phase 1 there
// is no sealed-write view, so a transformer body that defines a macro climbs off
// the sealed axis.
//
// This is the primary method for cross-phase access with O(1) lookup time.
// The environment must have been created via NewNamespace().
func (p *EnvironmentFrame) AtPhase(phase Phase) *EnvironmentFrame {
	// A sealed-write view's climb yields the sealed-write view for the target
	// phase — the design §4.5 inheritance rule, which is exactly what routed a
	// bootstrap macro's NextPhase() into the phase-1 seal, re-expressed as a
	// write mode instead of topology. It asks the frame's OWN registry: the rows
	// are the same for every owner, the frames are not.
	if phase > p.phaseLevel && p.rank == writeRankSealed && p.phases != nil {
		sealed, ok := p.phases.sealedViewAt(phase)
		if ok {
			return sealed
		}
	}
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
// This is the mutable user top level where normal bindings live: the owner's ROOT
// VIEW, whose writes land in the mutable tier at phase 0.
func (p *EnvironmentFrame) Runtime() *EnvironmentFrame {
	return p.AtPhase(PhaseRuntime)
}

// MutableRuntime returns the per-Engine MUTABLE runtime view of this frame's
// namespace — the user top level where user defines land and where eval/load and
// SRFI-18 threads store top-level state. Its writes land in the mutable tier at
// phase 0; a read through it still reaches sealed primitives, by tier order.
//
// Use this, NOT TopLevel(), when a primitive needs the frame for user-visible
// top-level mutations: TopLevel() answers "whichever view this lexical chain hangs
// from", which under a phase-1 chain or a bootstrap closure is not the user global
// at all, so storing a user define or thread state through it would miss. This
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
// A wildcard query (q.IsAll) means "match any" — this replaces the former
// checkScopes=false pattern. A specific or empty query (even the empty set) is
// checked via ScopesCompatible.
//
// When a key maps to multiple slots (same-name bindings with different
// scope sets from hygienic expansion), all compatible slots are visited.
//
// The visitor receives the matching binding, its slot index within the
// local frame, and the depth (number of parent frames traversed). Return
// a non-nil value to stop the walk and propagate the result.
func (p *EnvironmentFrame) resolveLocal(
	key *values.Symbol,
	q syntax.ScopeSet,
	visitor func(binding *Binding, slot int, depth int) any,
) any {
	env := p
	depth := 0
	matchAny := q.IsAll()
	scopes := q.Scopes()
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

// resolveGlobal resolves a global binding visible from this frame and hands the
// store and slot to the visitor.
//
// One ranked probe replaces the parent-chain walk this used to be: every frame
// of an owner shares the one store, so the layers the walk visited are now tiers
// in it, and the walk's ordering (first frame wins, maximal cardinality within
// one) is the probe's (rank-major, cardinality-minor). Incidentally this stops
// re-hashing the same map once per lexical depth — consecutive frames always
// shared the .global pointer.
//
// A wildcard query (q.IsAll) selects any binding of the name regardless of
// scopes, taking the highest tier's first live slot. It is NOT the same as the
// empty-set query: a reference written outside any macro expansion must not
// reach a binder introduced inside one.
//
// The RLock is released by defer because the probe can panic mid-hold on an
// ambiguous binding (P8).
func (p *EnvironmentFrame) resolveGlobal(
	key values.Symbol,
	q syntax.ScopeSet,
	visitor func(frame *GlobalEnvironmentFrame, ref slotRef) any,
) any {
	p.global.mu.RLock()
	defer p.global.mu.RUnlock()

	ref, ok := p.global.resolveRankedLocked(key, q, p.phaseLevel)
	if !ok {
		return nil
	}
	return visitor(p.global, ref)
}

// GetBinding returns the binding for the given symbol that matches the
// provided query. It searches local bindings first (walking up the parent
// chain), then globals.
//
// A wildcard query (AllScopes) means "match any" (no scope filtering). A
// specific or empty query enables hygienic resolution per Flatt's model with
// maximal binding selection (consistent with GetLocalIndex).
//
// Panics with a wrapped werr.ErrAmbiguousBinding when two incomparable scope
// sets tie for the maximal match (Racket's "ambiguous binding"); the tie is
// refused, never broken by order.
func (p *EnvironmentFrame) GetBinding(key *values.Symbol, q syntax.ScopeSet) *Binding {
	if q.IsAll() {
		// Fast path: wildcard query — return first match
		result := p.resolveLocal(key, q, func(binding *Binding, _ int, _ int) any {
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
		var best scopedBestOf[*Binding]
		target := len(q.Scopes())
		p.resolveLocal(key, q, func(binding *Binding, _ int, _ int) any {
			sc := binding.Scopes()
			rec, done := best.shouldRecord(sc, target)
			if rec {
				best.record(binding, sc)
			}
			if done {
				return true
			}
			return nil
		})

		if best.Ambiguous() {
			panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
				"GetBinding: identifier %q resolves ambiguously among incomparable hygienic scope sets",
				key.Key))
		}
		item, ok := best.Result()
		if ok {
			return item
		}
	}

	// The scope filter lives in the ranked probe, which both selects the maximal
	// match within the winning tier and rejects incompatible candidates.
	gResult := p.resolveGlobal(*key, q, func(g *GlobalEnvironmentFrame, ref slotRef) any {
		binding := g.bindings[ref.slot]
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
// deduplication. A slot is reused only by a binder carrying the SAME scope
// set; any other scope set, even a compatible one, is a different variable and
// gets its own slot (see scopeSetsEqual).
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
// that matches the given query. A wildcard query (AllScopes) means "match any".
//
// For a specific or empty query, this implements Flatt's "maximal" binding
// resolution: among all bindings whose scopes are a subset of the
// reference's scopes, the one with the LARGEST scope set is returned.
//
// Returns nil if no matching local binding exists.
//
// Panics with a wrapped werr.ErrAmbiguousBinding when two incomparable scope
// sets tie for the maximal match (Racket's "ambiguous binding"); the tie is
// refused, never broken by order.
func (p *EnvironmentFrame) GetLocalIndex(key *values.Symbol, q syntax.ScopeSet) *LocalIndex {
	if p == nil || !p.hasLocal() {
		return nil
	}

	// Fast path: wildcard query — return first match (no maximal resolution needed)
	if q.IsAll() {
		result := p.resolveLocal(key, q, func(_ *Binding, slot int, depth int) any {
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
	var best scopedBestOf[*LocalIndex]
	target := len(q.Scopes())
	p.resolveLocal(key, q, func(binding *Binding, slot int, depth int) any {
		sc := binding.Scopes()
		rec, done := best.shouldRecord(sc, target)
		if rec {
			best.record(NewLocalIndex(slot, depth), sc)
		}
		if done {
			return true
		}
		return nil
	})

	if best.Ambiguous() {
		panic(werr.WrapForeignErrorf(werr.ErrAmbiguousBinding,
			"GetLocalIndex: identifier %q resolves ambiguously among incomparable hygienic scope sets",
			key.Key))
	}
	item, _ := best.Result()
	return item
}

// HasLocalVariableBinding reports whether sym has a local variable binding
// satisfying the scope-set query q. This is the shared implementation used by
// both the macro expander (to decide whether a local variable shadows a macro)
// and the validator (to decide whether a local variable shadows a special form).
//
// The check implements Flatt's hygiene rule: a binding matches a reference when
// bindingScopes ⊆ useScopes. Bindings with no scopes (user code) match any use.
// A wildcard query (syntax.AllScopes) matches any binding of the name; pass
// syntax.ScopesOf(ref.Scopes()) for a hygienic reference-scoped check. Only
// BindingTypeVariable bindings are considered; syntax/primitive bindings do not
// shadow.
func (p *EnvironmentFrame) HasLocalVariableBinding(sym *values.Symbol, q syntax.ScopeSet) bool {
	if p == nil {
		return false
	}
	result := p.resolveLocal(sym, q, func(binding *Binding, _ int, _ int) any {
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
	// step up the parent chain for local_index[1]
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
// It panics if i is out of range for this frame's local bindings; callers must
// have obtained i from this frame.
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

// writeCoordinates derives the store coordinates a write through this view lands
// at: a sealed write at phase 0 is the ambient set — (ANY, sealed), the startup
// bindings every phase reaches — and every other write is exact-phase at the
// view's rank.
//
// This is where the pre-fold topology went. The phase-0 seal's global was minted
// ambient and the phase-1 seal's exact, because a phase frame's parent chain ran
// through the phase-0 seal and no further; the same fact is now one branch here.
func (p *EnvironmentFrame) writeCoordinates() (PhaseKey, bool) {
	sealed := p.rank == writeRankSealed
	if sealed && p.phaseLevel == PhaseRuntime {
		return AnyPhase(), true
	}
	return ExactPhase(p.phaseLevel), sealed
}

// MaybeCreateOwnGlobalBinding creates a new global binding in the owner store at
// THIS view's coordinates if it does not already exist there.
// It returns the GlobalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
//
// scopes become part of the binding's identity; a nil set is the ordinary
// user-written top-level define. Coordinates are the other half of that identity
// (see CreateGlobalBindingAt): a phase-0 define of a sealed name is a new slot,
// a define-for-syntax over the expand-phase registry copy is the same slot.
func (p *EnvironmentFrame) MaybeCreateOwnGlobalBinding(key *values.Symbol, bt BindingType, scopes []*syntax.Scope) (*GlobalIndex, bool) {
	phase, sealed := p.writeCoordinates()
	return p.global.CreateGlobalBindingAt(key, bt, scopes, phase, sealed)
}

// OwnGlobalIndex returns a PINNED GlobalIndex for the binding of key that
// resolves under q at THIS view's own write coordinates, or nil if there is
// none.
//
// This is the write path's re-resolve — "the binding I just created here" — and
// it is deliberately NOT the ranked read. Over one merged store a scoped read of
// a name that exists both sealed and mutable is answered by tier order; a
// coordinate-blind scoped lookup instead takes the first-seen of two equally
// unscoped candidates, so a top-level (define car …) would stamp the SEALED
// car's metadata and then refuse itself as a redefine of a Stable anchor.
func (p *EnvironmentFrame) OwnGlobalIndex(key *values.Symbol, q syntax.ScopeSet) *GlobalIndex {
	phase, sealed := p.writeCoordinates()
	p.global.mu.RLock()
	defer p.global.mu.RUnlock()

	i, ok := p.global.resolveAtCoordsLocked(*key, q, phase, sealed)
	if !ok {
		return nil
	}
	return newScopeKeyedGlobalIndex(key, p.global, slotRef{slot: i, phase: phase, sealed: sealed}, q)
}

// DeleteOwnGlobal removes the binding of sym that a scoped read at THIS view's
// own coordinates resolves to, returning whether one was removed.
//
// Coordinates rather than tier order, for the reason DeleteBindingAt gives: a
// ranked delete through the mutable runtime view would reach the sealed
// primitive whenever no user shadow existed.
func (p *EnvironmentFrame) DeleteOwnGlobal(sym *values.Symbol, scopes []*syntax.Scope) bool {
	phase, sealed := p.writeCoordinates()
	return p.global.DeleteBindingAt(sym, scopes, phase, sealed)
}

// SetDeferredGlobalValue writes through a DEFERRED index (Env == nil): the
// compile-time-unbound define/set! fallback. It resolves in the MUTABLE tier at
// this frame's phase and nowhere else.
//
// The restriction is G13 preserved, not new caution — but "before the fold this
// wrote through the executing frame's OWN global, which held exactly the
// mutable layer" is true only for USER code, not in general. A bootstrap
// closure's captured frame IS the sealed base (parent nil), and pre-fold that
// frame's OWN .global was the sealed base's OWN store — the sealed layer, not
// the mutable one — so a deferred write executing there would have landed
// sealed, the opposite of the claim. The merged store holds the sealed tiers a
// user-frame walk could never reach, and a ranked resolve here would let set!
// of an unshadowed primitive name mutate the sealed entry in place under
// WithMutableTopLevel — where nothing is Stable to refuse it — a behavior
// change and a P1 breach; restricting to the mutable tier closes that for
// BOTH frame kinds, which is why this is not narrowed to "user frames only".
// A PINNED index is a different question and keeps its reach
// (machine_context.go's other branch).
//
// Whether the sealed-rank case this guards against has a live trigger at all is
// a separate question from whether the guard is correct: this round's review
// measurement (task-6 report, "Fix round 1") found no compiler path that
// constructs a deferred (Env == nil) GlobalIndex for a STORE instruction —
// emitDefineStore and CompileValidatedSetBang, the only two literal-producing
// sites for OpStoreGlobal, both resolve through OwnGlobalIndex /
// GetGlobalIndexWithScopes, which pin whenever they succeed and raise a compile
// error otherwise. That is evidence of no KNOWN live trigger, not a proof of
// unreachability (a hand-built literal, or a future compiler path, could still
// reach this branch), so the restriction stays as the fail-closed answer either
// way.
func (p *EnvironmentFrame) SetDeferredGlobalValue(gi *GlobalIndex, v values.Value) error {
	return p.global.setValueAtCoords(gi.Index, gi.query, ExactPhase(p.phaseLevel), false, v)
}

// IsOwnerRoot reports whether this frame is one of its NAMESPACE's own root
// views — the mutable root or the sealed-write root.
//
// This is the honest form of the old (ns.Runtime() == p.env || ns.SealedBase()
// == p.env) comparison: the immutable-top-level define gate must fire for user
// top-level compilation AND for bootstrap compilation through the sealed-write
// view (that path is what stamps Stable onto bootstrap procedures — the
// optimizer's anchors), and must NOT fire for a library env's root views, which
// share the namespace pointer but are different frames (a library body keeps
// cross-form define/set! mutable, R2).
func (p *EnvironmentFrame) IsOwnerRoot() bool {
	if p.namespace == nil {
		return false
	}
	return p == p.namespace.runtime || p == p.namespace.sealedWriteRoot
}

// DefineOwnGlobal creates (or reuses) the binding for key under scopes in this
// frame's own global environment, then writes v to that binding.
//
// It exists because create and write disagree about what a nil scope set means:
// creation treats nil as the EXACT empty set (so a macro-introduced binder gets
// its own slot), while a GlobalIndex built from a bare symbol resolves MATCH ANY
// (the name's first live slot, whatever its hygiene). Pairing the two by hand
// therefore creates one binding and writes a different one as soon as any macro
// has introduced the same name — the host's value lands on the macro's variable
// and the host's own binding stays void.
//
// Callers that create a global and immediately give it a value should use this
// instead of pairing MaybeCreateOwnGlobalBinding with SetOwnGlobalValue, so the
// nil question cannot be asked wrongly at the call site.
func (p *EnvironmentFrame) DefineOwnGlobal(key *values.Symbol, bt BindingType, scopes []*syntax.Scope, v values.Value) error {
	p.MaybeCreateOwnGlobalBinding(key, bt, scopes)

	// Re-resolve under the creation key AND at the creation coordinates.
	// CreateGlobalBindingAt hands back a DEFERRED index carrying neither frame
	// nor scopes, which the write path would resolve wildcard; and over one
	// merged store even a scoped resolve must be told which tier it wrote into.
	phase, sealed := p.writeCoordinates()
	err := p.global.setValueAtCoords(key, syntax.ScopesOf(scopes), phase, sealed, v)
	if err != nil {
		return werr.WrapForeignErrorf(err, "DefineOwnGlobal: write to %q failed after creation", key.Key)
	}
	return nil
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
	result := p.resolveGlobal(*key, syntax.AllScopes(), func(g *GlobalEnvironmentFrame, ref slotRef) any {
		return newResolvedGlobalIndex(key, g, ref)
	})
	if result != nil {
		return result.(*GlobalIndex)
	}
	return nil
}

// GetGlobalIndexWithScopes is GetGlobalIndex with hygienic resolution: the
// binding whose scope set maximally matches the query wins. The empty query
// (EmptyScopes) resolves under the empty scope set, not "any" — pass AllScopes
// for wildcard resolution.
func (p *EnvironmentFrame) GetGlobalIndexWithScopes(key *values.Symbol, q syntax.ScopeSet) *GlobalIndex {
	result := p.resolveGlobal(*key, q, func(g *GlobalEnvironmentFrame, ref slotRef) any {
		return newScopeKeyedGlobalIndex(key, g, ref, q)
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
	result := p.resolveGlobal(*key.Index, key.query, func(g *GlobalEnvironmentFrame, ref slotRef) any {
		return g.bindings[ref.slot]
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
// scopes is the REFERENCE's scope set and each phase is searched hygienically
// (maximal subset match), not by bare name. Nil means the empty set, not "any" —
// the same convention as GetGlobalIndexWithScopes. Bare-name search was correct
// only while a name owned one slot per frame: once a macro-generating macro is
// expanded twice, each expansion's phase-0 binder carries its own intro scope, so
// the name owns two slots and a wildcard walk hands BOTH generated inner macros
// whichever slot was created first. See the two-expansion jabberwocky case in
// pkg/wile/toplevel_binder_scope_test.go, which the single-expansion case below
// cannot detect.
//
// The phase-0 (runtime) search reaching the mutable runtime frame's OWN defines
// is DELIBERATE and load-bearing — it is NOT the accidental parent-chain leak
// the phase-frame reparent (createPhaseEnv) closed, and must NOT be routed to
// the phase-0 seal. A macro-generating-macro introduces a phase-0
// define that a generated inner macro references by scope-aware
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
func (p *EnvironmentFrame) GetGlobalIndexAcrossPhases(key *values.Symbol, scopes []*syntax.Scope) *GlobalIndex {
	if p.phases == nil {
		// No phase registry — try runtime only
		return p.GetGlobalIndexWithScopes(key, syntax.ScopesOf(scopes))
	}

	// Non-negative phases present, ascending. Ascending is what makes the first
	// hit the LOWEST phase, preserving today's runtime-first precedence; the
	// {0,1,2} literal truncated the tower (a phase-3 library binding was not
	// exportable). PhaseTemplate (-1) stays excluded DELIBERATELY: including it
	// would rank for-template bindings ahead of runtime ones, a precedence
	// change this commit does not make — it lifts a ceiling only.
	//
	// The probe runs against the store at each phase rather than through
	// p.phases.Get(phase), which answers nil for a phase the registry has never
	// instantiated — the very phases PresentPhases now adds by consulting the
	// store. Every phase view is a view over this one store differing only in
	// which phase it probes at, so this is the same resolution without minting a
	// view as a side effect of a search.
	q := syntax.ScopesOf(scopes)
	for _, phase := range p.PresentPhases() {
		gi := p.globalIndexAtPhase(key, q, phase)
		if gi != nil {
			return gi
		}
	}
	return nil
}

// globalIndexAtPhase is GetGlobalIndexWithScopes with the probe phase supplied
// rather than taken from the receiver's own level.
func (p *EnvironmentFrame) globalIndexAtPhase(key *values.Symbol, q syntax.ScopeSet, phase Phase) *GlobalIndex {
	p.global.mu.RLock()
	defer p.global.mu.RUnlock()

	ref, ok := p.global.resolveRankedLocked(*key, q, phase)
	if !ok {
		return nil
	}
	return newScopeKeyedGlobalIndex(key, p.global, ref, q)
}

// PresentPhases returns the non-negative phases worth searching for this frame's
// owner (the engine's Namespace, or a library env's own store —
// EnvironmentFrame.phases is shared by every view over one owner, per "One Store
// Per Owner" in the package doc), ascending. It is the shared basis for every
// cross-phase search that must reach the whole macro tower rather than a fixed
// {0,1,2} guess: GetGlobalIndexAcrossPhases above, and machine/compilation's
// findLibraryBinding (export resolution), which cannot reach p.phases directly
// since it lives outside this package.
//
// It is the UNION of two sets, because neither alone is the question. The
// registry's instantiated VIEWS are phases something has looked at; the store's
// exact slot coordinates are phases something is BOUND at, and the two come
// apart in both directions. GlobalEnvironmentFrame.Copy carries a store's slots
// without carrying the source's views — NewSchemeReportNamespace is exactly
// that — so a copied namespace's phase-2 bindings are present in every
// enumeration (LiveSlots) and were invisible to every cross-phase SEARCH while
// this consulted the views alone.
//
// PhaseTemplate (-1) is excluded: callers here rank a reference by RUNTIME-first
// precedence, and a for-template binding is a different axis, not a lower rung
// of this one.
// presentPhasesHint sizes the union so the ordinary case does not regrow: the
// four named phases from the registry plus the same phases from the store, with
// room for a couple of tower rungs. Overshooting costs one small slice; the
// duplicates are removed after the sort.
const presentPhasesHint = 12

func (p *EnvironmentFrame) PresentPhases() []Phase {
	// One allocation for the union. Both sources append into it: taking the
	// registry's own exactly-sized slice and then appending the store's phases to
	// it forces a second, and this sits on the macro-compilation path.
	phaseList := make([]Phase, 0, presentPhasesHint)
	if p.phases != nil {
		// Takes the registry's read lock; called once here rather than per phase.
		phaseList = p.phases.appendPhases(phaseList)
	}
	if p.global != nil {
		phaseList = p.global.appendExactPhases(phaseList)
	}
	slices.Sort(phaseList)
	phaseList = slices.Compact(phaseList)
	i := 0
	for i < len(phaseList) && phaseList[i] < PhaseRuntime {
		i++
	}
	return phaseList[i:]
}

// GetGlobalIndexFromLibraryScopes searches for a binding by checking each scope
// against the root Namespace's scope registry. For each scope that maps to a
// library env, it searches that library AT THE REFERRING PHASE — this frame's own
// phaseLevel — and at no other.
//
// The phase comes off the receiver rather than a parameter because every frame
// inherits phaseLevel from its lexical parent (NewEnvironmentFrameWithParent), so a
// lambda body inside a begin-for-syntax already carries phase 1, just as the body
// frame does.
//
// Searching every phase is what made a library body's phase separation vanish:
// phase-0 code resolved a phase-1 define and phase-1 code resolved a phase-0 one,
// where the top level refuses both — silently, producing a wrong value in the first
// case and a #!void from a predeclared-unwritten slot in the second. See
// plans/2026-08-04-library-phase-isolation-design.local.md §1.
//
// Reaching the library's PRIMITIVES from phase 1 is not this search's job — it
// falls out of GetGlobalIndexWithScopes's own reach, not a mechanism this
// function adds. A library env (NewChildRuntime) shares its store across every
// phase view, and GetGlobalIndexWithScopes's ranked probe (resolveGlobal →
// resolveRankedLocked) already includes tiers T2/T3 (sealed, at the referring
// phase and ambient) at every call — there is no parent walk post-fold, one
// store answers for all of a library's phase views. Before the store fold this
// same reach came from the phase frame's PARENT LINK to the library's own
// sealed base; the fold changed the mechanism (link → tier) without changing
// which primitives a phase-1 body can see. Before library envs owned their own
// sealed axis at all, this arm's phase-0 reach was the ONLY such route, which is
// why narrowing it alone would have stranded every begin-for-syntax body in
// every library (design E4).
//
// The other GetGlobalIndexAcrossPhases caller (compile_syntax_rules.go) is untouched:
// it carries the R7RS §4.3 free-template-identifier carve-out, and the
// jabberwocky/march-hare case reaches it through that caller, not this one.
func (p *EnvironmentFrame) GetGlobalIndexFromLibraryScopes(key *values.Symbol, scopes []*syntax.Scope) *GlobalIndex {
	if p.namespace == nil || len(scopes) == 0 {
		return nil
	}
	for _, scope := range scopes {
		libEnv := p.namespace.LookupLibraryEnv(scope)
		if libEnv == nil {
			continue
		}
		phaseEnv := libEnv.AtPhase(p.phaseLevel)
		gi := phaseEnv.GetGlobalIndexWithScopes(key, syntax.ScopesOf(scopes))
		if gi != nil {
			return gi
		}
	}
	return nil
}

// SetOwnGlobalValue sets the value of the binding for the given GlobalIndex.
// It returns an error if the binding does not exist.
//
// A PINNED index carries its own coordinates and goes straight to the store. A
// DEFERRED one (Env == nil — what a create hands back) carries a symbol and a
// hygiene key and nothing else, so THIS VIEW supplies the coordinates, the same
// way DefineOwnGlobal does after creating. The store cannot do that for itself:
// it holds every phase and every rank at once and has no view to ask. Before the
// coordinates existed this fell through to a name lookup that took the first live
// slot of the name at ANY coordinates — for a bare (set! car …) shape, the sealed
// primitive.
//
// This is not SetDeferredGlobalValue, whose reach is narrower on purpose (mutable
// tier only, G13): that one serves the VM's deferred OpStoreGlobal, where the
// executing frame is whatever a closure captured. Here the caller chose the view.
func (p *EnvironmentFrame) SetOwnGlobalValue(gi *GlobalIndex, v values.Value) error {
	if gi != nil && gi.Env == nil {
		phase, sealed := p.writeCoordinates()
		return p.global.setValueAtCoords(gi.Index, gi.query, phase, sealed, v)
	}
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

// The sealed-write-view routing seam (SealedWriteViewAt, unsealedTargetAt,
// IsNamespaceRuntime) lives in sealed_write_view.go.
