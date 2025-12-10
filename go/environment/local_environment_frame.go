package environment

import (
	"fmt"
	"maps"
	"skeme/values"
	"slices"
)

// LocalEnvironmentFrame stores local variable bindings for a single scope.
// It maps symbols to binding indices for efficient lookup. Local environments
// are created for lambda parameters and let-bound variables.
// Note: LocalEnvironmentFrame has no hierarchy of its own; the hierarchy is
// managed by EnvironmentFrame via its parent field.
type LocalEnvironmentFrame struct {
	keys     map[values.Symbol]int
	bindings []*Binding
}

// NewLocalEnvironment creates a new local environment frame with pre-allocated
// slots for the given parameter count. Each slot is initialized with a void
// binding of unknown type.
func NewLocalEnvironment(pcnt int) *LocalEnvironmentFrame {
	q := &LocalEnvironmentFrame{
		keys:     make(map[values.Symbol]int),
		bindings: make([]*Binding, pcnt),
	}
	for i := 0; i < pcnt; i++ {
		q.bindings[i] = NewBinding(values.Void, BindingTypeUnknown)
	}
	return q
}

// Bindings returns the slice of bindings in this local environment.
func (p *LocalEnvironmentFrame) Bindings() []*Binding {
	return p.bindings
}

// SetBindings replaces the bindings slice in this local environment.
func (p *LocalEnvironmentFrame) SetBindings(v []*Binding) {
	p.bindings = v
}

// Keys returns the symbol-to-index mapping for this local environment.
func (p *LocalEnvironmentFrame) Keys() map[values.Symbol]int {
	return p.keys
}

// CreateLocalBinding creates a new local binding with the given key and binding type.
func (p *LocalEnvironmentFrame) CreateLocalBinding(key *values.Symbol, bt BindingType) (*LocalIndex, bool) {
	// FIXME: this is a MaybeCreate function, not a Create function
	i, ok := p.keys[*key]
	if ok {
		return &LocalIndex{i, 0}, false
	}
	i = len(p.bindings)
	p.keys[*key] = i
	p.bindings = append(p.bindings, NewBinding(values.Void, bt))
	return &LocalIndex{i, 0}, true
}

// GetLocalIndex returns the LocalIndex for the given symbol in this local environment.
// Returns nil if the symbol is not bound in this environment.
func (p *LocalEnvironmentFrame) GetLocalIndex(key *values.Symbol) *LocalIndex {
	i, ok := p.keys[*key]
	if !ok {
		return nil
	}
	return &LocalIndex{i, 0}
}

// GetLocalBinding returns the binding at the given LocalIndex.
func (p *LocalEnvironmentFrame) GetLocalBinding(li *LocalIndex) *Binding {
	lb := p.bindings[li[0]]
	return lb
}

// SetLocalValue sets the value of the binding at the given LocalIndex.
func (p *LocalEnvironmentFrame) SetLocalValue(li *LocalIndex, v values.Value) error {
	lb := p.bindings[li[0]]
	lb.value = v
	return nil
}

// SchemeString returns a string representation of this local environment.
func (p *LocalEnvironmentFrame) SchemeString() string {
	return fmt.Sprintf("#<Local-environment>")
}

// IsVoid returns true if this local environment frame is nil.
func (p *LocalEnvironmentFrame) IsVoid() bool {
	return p == nil
}

// EqualTo returns true if this local environment is equal to the given value.
// Two local environments are equal if they have the same bindings.
func (p *LocalEnvironmentFrame) EqualTo(o values.Value) bool {
	v, ok := o.(*LocalEnvironmentFrame)
	if !ok {
		return false
	}
	if p == nil || v == nil {
		return p == v
	}
	if len(p.bindings) != len(v.bindings) {
		return false
	}
	for i := range p.bindings {
		if !p.bindings[i].EqualTo(v.bindings[i]) {
			return false
		}
	}
	return true
}

// Copy creates a deep copy of this local environment frame.
func (p *LocalEnvironmentFrame) Copy() values.Value {
	if p == nil {
		return (*LocalEnvironmentFrame)(nil)
	}
	q := &LocalEnvironmentFrame{}
	q.bindings = slices.Clone(p.bindings)
	for i := range p.bindings {
		q.bindings[i] = p.bindings[i].Copy().(*Binding)
	}
	if p.keys != nil {
		q.keys = make(map[values.Symbol]int, len(p.keys))
		maps.Copy(q.keys, p.keys)
	}
	return q
}
