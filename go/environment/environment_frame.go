package environment

import (
	"skeme/syntax"
	"skeme/values"
)

// EnvironmentFrame represents an environment frame in the hierarchy.
//
// Design: EnvironmentFrame owns two hierarchy axes:
//   - parent: lexical scoping chain (lambda bodies → enclosing scope → ... → TopLevel)
//   - meta: phase chain (Runtime → Expand → Compile)
//
// LocalEnvironmentFrame and GlobalEnvironmentFrame have no hierarchy of their own.
//
// Phase Hierarchy (R7RS) - chain via meta field:
//
//	TopLevel EnvironmentFrame (= Runtime)
//	│   parent: nil
//	│   global: primitives, user defines, symbol/syntax interning
//	│   meta: → Expand
//	│           │   global: syntax bindings (define-syntax)
//	│           │   meta: → Compile
//	│                       global: compile-time procedures
//
// Each phase has its own GlobalEnvironmentFrame. Symbol/syntax interning maps
// are shared from TopLevel's GlobalEnvironmentFrame.
//
// Binding lookup is two-phase: first all locals up parent chain, then all globals.
type EnvironmentFrame struct {
	// parent links to enclosing lexical scope (nil for TopLevel)
	parent *EnvironmentFrame
	// local holds local bindings for this frame (parameters, let-bound variables)
	local *LocalEnvironmentFrame
	// global holds global bindings for this phase
	global *GlobalEnvironmentFrame
	// meta links to next phase environment (Expand from Runtime, Compile from Expand)
	meta *EnvironmentFrame
	// Library support (future use)
	aliases map[values.Symbol]values.Symbol
	exports map[values.Symbol]struct{}
	imports map[values.Symbol]struct{}
}

// NewTopLevelEnvironmentFrame creates a new top-level global environment frame.
// Deprecated: Use NewTipTopEnvironmentFrame for the new phase-based hierarchy.
func NewTopLevelEnvironmentFrame() *EnvironmentFrame {
	global := NewTopLevelGlobalEnvironment()
	q := &EnvironmentFrame{
		local:  nil,
		global: global,
	}
	return q
}

// NewTipTopEnvironmentFrame creates the universal tip-top environment frame.
// This is the root of the environment hierarchy. Its GlobalEnvironmentFrame
// holds shared symbol and syntax interning maps used by all phases.
// Phase environments (runtime, expand, compile) are created lazily via accessors.
func NewTipTopEnvironmentFrame() *EnvironmentFrame {
	global := NewTopLevelGlobalEnvironment()
	return &EnvironmentFrame{
		parent: nil,
		local:  nil,
		global: global,
	}
}

// NewPhaseEnvironmentFrame creates an environment frame for a specific phase.
// It has its own GlobalEnvironmentFrame for phase-specific bindings and
// parents to the given tip-top frame for shared interning access.
func NewPhaseEnvironmentFrame(tenv *EnvironmentFrame) *EnvironmentFrame {
	// Create a new GlobalEnvironmentFrame for this phase.
	// Share the interning maps from tip-top.
	global := NewGlobalEnvironment(
		tenv.global.symbolInterns,
		tenv.global.syntaxInterns,
	)
	return &EnvironmentFrame{
		parent: tenv,
		local:  nil,
		global: global,
	}
}

// NewEnvironmentFrame creates a new environment frame with the given local and global environment frames.
// The parent field is set to nil.
func NewEnvironmentFrame(local *LocalEnvironmentFrame, global *GlobalEnvironmentFrame) *EnvironmentFrame {
	q := &EnvironmentFrame{
		local:  local,
		global: global,
	}
	return q
}

// NewEnvironmentFrameWithParent creates a new environment frame with the given local environment frame and parent environment frame.
// The global environment frame is inherited from the parent, or set to a new top-level global environment frame if the parent is nil.
// This is used for creating child frames within a phase (e.g., lambda bodies).
func NewEnvironmentFrameWithParent(local *LocalEnvironmentFrame, parent *EnvironmentFrame) *EnvironmentFrame {
	q := &EnvironmentFrame{
		parent: parent,
		local:  local,
	}
	if parent == nil {
		q.global = NewTopLevelGlobalEnvironment()
	} else {
		q.global = parent.global
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

// Runtime returns the runtime phase environment, creating it if needed.
// This should only be called on the tip-top frame; other frames delegate to tip-top.
func (p *EnvironmentFrame) Runtime() *EnvironmentFrame {
	return p.TopLevel()
}

// Expand returns the expand phase environment, creating it if needed.
// This is where syntax bindings from define-syntax are stored.
// This should only be called on the tip-top frame; other frames delegate to tip-top.
func (p *EnvironmentFrame) Expand() *EnvironmentFrame {
	cenv := p.Runtime()
	if cenv.meta == nil {
		cenv.meta = NewPhaseEnvironmentFrame(cenv)
	}
	return cenv.meta
}

// Compile returns the compile phase environment, creating it if needed.
// This is where compile-time procedures are stored.
// This should only be called on the tip-top frame; other frames delegate to tip-top.
func (p *EnvironmentFrame) Compile() *EnvironmentFrame {
	cenv := p.Expand()
	if cenv.meta == nil {
		cenv.meta = NewPhaseEnvironmentFrame(cenv)
	}
	return cenv.meta
}

// Meta returns the expand phase environment for backward compatibility.
// Deprecated: Use Expand() instead for clarity.
func (p *EnvironmentFrame) Meta() *EnvironmentFrame {
	return p.Expand()
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
	for {
		// always check local first
		if cenv.local == nil {
			break
		}
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
	for {
		// then check global
		if cenv.global == nil {
			break
		}
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
// It searches for local bindings first, then global bindings, checking scope compatibility.
func (p *EnvironmentFrame) GetBindingWithScopes(key *values.Symbol, scopes []*syntax.Scope) *Binding {
	// First, try local bindings
	li := p.GetLocalIndex(key)
	if li != nil {
		binding := p.GetLocalBinding(li)
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
		}
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

// MaybeCreateGlobalBinding creates a new global binding in the current or parent global environments if it does not already exist.
// It returns the GlobalIndex of the binding and a boolean indicating whether
// the binding was created (true) or already existed (false).
func (p *EnvironmentFrame) MaybeCreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	ge := p
	_, ok := ge.global.keys[*key]
	for !ok && !ge.IsTopLevel() {
		ge = ge.parent
		_, ok = ge.global.keys[*key]
	}
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
	return NewGlobalIndex(key)
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

// SetGlobalValue sets the value of the binding for the given GlobalIndex.
// It returns an error if the binding does not exist.
func (p *EnvironmentFrame) SetGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p
	i, ok := ge.global.keys[*gi.Index]
	for !ok && !ge.IsTopLevel() {
		ge = ge.parent
		i, ok = ge.global.keys[*gi.Index]
	}
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
// The parent environment frame is shared between the original and the copy.
func (p *EnvironmentFrame) Copy() *EnvironmentFrame {
	q := &EnvironmentFrame{
		parent: p.parent,
		local:  p.local.Copy().(*LocalEnvironmentFrame),
		global: p.global.Copy().(*GlobalEnvironmentFrame),
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

// InternSymbol interns the given symbol using the tip-top's shared interning map.
// This ensures symbol identity is consistent across all phases.
func (p *EnvironmentFrame) InternSymbol(q *values.Symbol) *values.Symbol {
	return p.TopLevel().global.InternSymbol(q)
}

// InternSyntax interns the given syntax value using the tip-top's shared interning map.
// This ensures syntax object identity is consistent across all phases.
func (p *EnvironmentFrame) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	return p.TopLevel().global.InternSyntax(k, v)
}
