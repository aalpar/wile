package environment

import (
	"fmt"
)

const (
	// indexOver is the index within a local environment frame (binding slot number)
	indexOver = iota
	// indexUp is how many parent frames to traverse to find the binding
	indexUp
)

// LocalIndex represents the location of a local binding as [slot, depth].
// The first element (slot/over) is the index within the local environment frame.
// The second element (depth/up) is how many parent frames to traverse.
// Example: [2, 1] means "binding at slot 2 in the parent frame".
type LocalIndex [2]int

// NewLocalIndex creates a new LocalIndex with the given slot and depth.
func NewLocalIndex(over, up int) *LocalIndex {
	return &LocalIndex{over, up}
}

// Over returns the slot index within the local environment frame.
func (p *LocalIndex) Over() int {
	return p[indexOver]
}

// Up returns the depth (number of parent frames to traverse).
func (p *LocalIndex) Up() int {
	return p[indexUp]
}

// GetBinding retrieves the binding at this index from the given environment.
func (p *LocalIndex) GetBinding(env *EnvironmentFrame) *Binding {
	return env.GetLocalBinding(p)
}

// String returns a string representation in "slot:depth" format.
func (p *LocalIndex) String() string {
	return fmt.Sprintf("%d:%d", p[0], p[1])
}

// SchemeString returns a Scheme-style string representation.
func (p *LocalIndex) SchemeString() string {
	return fmt.Sprintf("<local-index %s>", p)
}

// EqualTo returns true if this index equals the given index.
func (p *LocalIndex) EqualTo(i *LocalIndex) bool {
	if p == nil || i == nil {
		return p == i
	}
	return p[0] == i[0] && p[1] == i[1]
}
