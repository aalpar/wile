package environment

import (
	"fmt"
	"maps"
	"skeme/syntax"
	"skeme/values"
	"slices"
)

// GlobalIndex identifies a global binding by its symbol key.
// Unlike LocalIndex which uses numeric indices, GlobalIndex uses the symbol
// directly since global bindings are accessed by name at runtime.
type GlobalIndex struct {
	Index *values.Symbol
}

// NewGlobalIndex creates a new GlobalIndex for the given symbol.
func NewGlobalIndex(key *values.Symbol) *GlobalIndex {
	return &GlobalIndex{Index: key}
}

// SchemeString returns a string representation of this global index.
func (p *GlobalIndex) SchemeString() string {
	return fmt.Sprintf("<global-index %s>", p.Index.SchemeString())
}

// IsVoid returns true if this global index is nil.
func (p *GlobalIndex) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this global index equals the given value.
func (p *GlobalIndex) EqualTo(value values.Value) bool {
	if value == nil || p == nil {
		return value == nil && p == nil
	}
	v, ok := value.(*GlobalIndex)
	if !ok {
		return false
	}
	if v.Index.EqualTo(p.Index) {
		return true
	}
	return false
}

// GlobalEnvironmentFrame represents global bindings for a single phase.
// It holds bindings and optionally symbol/syntax interning maps.
//
// Design: GlobalEnvironmentFrame has no hierarchy of its own. The environment
// hierarchy is managed by EnvironmentFrame via its parent field. Each phase
// (runtime, expand, compile) has its own GlobalEnvironmentFrame. The tip-top
// environment's GlobalEnvironmentFrame holds shared interning maps.
type GlobalEnvironmentFrame struct {
	// symbol to binding index lookup map
	keys     map[values.Symbol]int
	bindings []*Binding
	// symbol canonicalization map (typically only on tip-top's global)
	symbolInterns map[values.Symbol]*values.Symbol
	// syntax object interning map (typically only on tip-top's global)
	syntaxInterns map[values.Value]syntax.SyntaxValue
	// library registry for R7RS library loading (typically only on tip-top's global)
	// Stored as any to avoid circular dependency with machine package.
	libraryRegistry any
}

// NewTopLevelGlobalEnvironment creates a new global environment with fresh interning maps.
// Use this for creating the tip-top environment that holds shared interning state.
func NewTopLevelGlobalEnvironment() *GlobalEnvironmentFrame {
	return NewGlobalEnvironment(nil, nil)
}

// NewGlobalEnvironment creates a new global environment frame.
// If symInterns or synInterns are nil, new maps are created.
// Pass existing maps to share interning state between phases.
func NewGlobalEnvironment(symInterns map[values.Symbol]*values.Symbol, synInterns map[values.Value]syntax.SyntaxValue) *GlobalEnvironmentFrame {
	q := &GlobalEnvironmentFrame{
		bindings: []*Binding{},
		keys:     map[values.Symbol]int{},
	}
	q.symbolInterns = symInterns
	if q.symbolInterns == nil {
		q.symbolInterns = map[values.Symbol]*values.Symbol{}
	}
	q.syntaxInterns = synInterns
	if q.syntaxInterns == nil {
		q.syntaxInterns = map[values.Value]syntax.SyntaxValue{}
	}
	return q
}

// Copy creates a deep copy of the global environment frame.
// Note that the parent is not copied.
func (p *GlobalEnvironmentFrame) Copy() values.Value {
	if p == nil {
		return (*GlobalEnvironmentFrame)(nil)
	}
	q := &GlobalEnvironmentFrame{}
	q.bindings = slices.Clone(p.bindings)
	for i := range p.bindings {
		q.bindings[i] = p.bindings[i].Copy().(*Binding)
	}
	if p.keys != nil {
		q.keys = make(map[values.Symbol]int)
		maps.Copy(q.keys, p.keys)
	}
	if p.symbolInterns != nil {
		q.symbolInterns = make(map[values.Symbol]*values.Symbol)
		maps.Copy(q.symbolInterns, p.symbolInterns)
	}
	if p.syntaxInterns != nil {
		q.syntaxInterns = make(map[values.Value]syntax.SyntaxValue)
		maps.Copy(q.syntaxInterns, p.syntaxInterns)
	}
	return q
}

// Bindings returns the slice of bindings in this global environment.
func (p *GlobalEnvironmentFrame) Bindings() []*Binding {
	return p.bindings
}

// SetBindings replaces the bindings slice in this global environment.
func (p *GlobalEnvironmentFrame) SetBindings(vs []*Binding) {
	p.bindings = vs
}

// Keys returns the symbol-to-index mapping for this global environment.
func (p *GlobalEnvironmentFrame) Keys() map[values.Symbol]int {
	return p.keys
}

// CreateGlobalBinding creates a new global binding with the given key and type.
// The key is interned before use. Returns the GlobalIndex and whether a new
// binding was created (false if the binding already existed).
func (p *GlobalEnvironmentFrame) CreateGlobalBinding(key *values.Symbol, bt BindingType) (*GlobalIndex, bool) {
	r := p
	key = p.InternSymbol(key)
	_, ok := r.keys[*key]
	if ok {
		q := NewGlobalIndex(key)
		return q, false
	}
	i := len(p.bindings)
	p.keys[*key] = i
	// append the new binding at index i
	p.SetBindings(append(p.Bindings(), NewBinding(values.Void, bt)))
	q := NewGlobalIndex(key)
	return q, true
}

// GetGlobalIndex returns the GlobalIndex for the given symbol.
// Returns nil if the symbol is not bound in this global environment.
func (p *GlobalEnvironmentFrame) GetGlobalIndex(key *values.Symbol) *GlobalIndex {
	ge := p
	key = p.InternSymbol(key)
	_, ok := ge.keys[*key]
	if !ok {
		return nil
	}
	q := NewGlobalIndex(key)
	return q
}

// GetGlobalBinding returns the binding for the given GlobalIndex.
// Returns nil if the binding does not exist.
func (p *GlobalEnvironmentFrame) GetGlobalBinding(gi *GlobalIndex) *Binding {
	ge := p
	key := p.InternSymbol(gi.Index)
	i, ok := ge.keys[*key]
	if !ok {
		return nil
	}
	bd := ge.bindings[i]
	return bd
}

// SetGlobalValue sets the value of the binding for the given GlobalIndex.
// Returns an error if the binding does not exist.
func (p *GlobalEnvironmentFrame) SetGlobalValue(gi *GlobalIndex, v values.Value) error {
	ge := p
	i, ok := ge.keys[*gi.Index]
	if !ok {
		return values.WrapForeignErrorf(values.ErrNoSuchBinding, "no such global binding %q", gi.Index)
	}
	ge.bindings[i].value = v
	return nil
}

// IsVoid returns true if this global environment frame is nil.
func (p *GlobalEnvironmentFrame) IsVoid() bool {
	return p == nil
}

// SchemeString returns a string representation of this global environment.
func (p *GlobalEnvironmentFrame) SchemeString() string {
	return "#<global-environment>"
}

// EqualTo returns true if this global environment equals the given value.
// Two global environments are equal if they have the same bindings.
func (p *GlobalEnvironmentFrame) EqualTo(o values.Value) bool {
	if p == nil || o == nil {
		return p == nil && o == nil
	}
	v, ok := o.(*GlobalEnvironmentFrame)
	if !ok {
		return false
	}
	if p == v {
		return true
	}
	if len(p.bindings) != len(v.bindings) {
		return false
	}
	for k, i := range p.keys {
		j, ok := v.keys[k]
		if !ok || i != j {
			return false
		}
		if !p.bindings[i].EqualTo(v.bindings[j]) {
			return false
		}
	}
	return true
}

// InternSymbol returns the canonical version of the given symbol.
// If this symbol has been seen before, the previously interned pointer is returned.
// Otherwise, the symbol is added to the intern map and returned.
// This ensures symbol identity for eq? comparisons.
func (p *GlobalEnvironmentFrame) InternSymbol(q *values.Symbol) *values.Symbol {
	v, ok := p.symbolInterns[*q]
	if ok {
		return v
	}
	p.symbolInterns[*q] = q
	return q
}

// InternSyntax returns the canonical version of the given syntax value.
// If an equivalent syntax value has been seen before, it is returned.
// Otherwise, the value is added to the intern map and returned.
func (p *GlobalEnvironmentFrame) InternSyntax(k values.Value, v syntax.SyntaxValue) syntax.SyntaxValue {
	val, ok := p.syntaxInterns[k]
	if ok {
		return val
	}
	p.syntaxInterns[k] = v
	return v
}

// LibraryRegistry returns the library registry for R7RS library loading.
// The caller must type-assert to *machine.LibraryRegistry.
// Returns nil if no registry has been set.
func (p *GlobalEnvironmentFrame) LibraryRegistry() any {
	return p.libraryRegistry
}

// SetLibraryRegistry sets the library registry for R7RS library loading.
// The registry should be a *machine.LibraryRegistry.
func (p *GlobalEnvironmentFrame) SetLibraryRegistry(registry any) {
	p.libraryRegistry = registry
}
