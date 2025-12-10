package define_syntax

import "wile/values"

// MacroOpNext advances the current position to the next element in the list.
//
// This operation moves from the current pair to its cdr (tail). It is used
// to traverse a list sequentially during pattern matching.
//
// # Behavior
//
// Before: mm.curr points to a pair (car . cdr)
// After:  mm.curr points to cdr (which must be a pair)
//
// # Errors
//
// Returns values.ErrNotAPair if the cdr is not a pair. This typically happens
// when reaching an improper list terminator or when the pattern expects more
// elements than the input provides.
//
// # Example
//
// Matching pattern (a b c) against input (1 2 3):
//
//	Initial: curr = (1 2 3)
//	After MacroOpNext: curr = (2 3)
//	After MacroOpNext: curr = (3)
type MacroOpNext struct{}

// NewMacroOpNext creates a new MacroOpNext operation.
func NewMacroOpNext() *MacroOpNext {
	return &MacroOpNext{}
}

// Apply advances to the next element in the current list.
// Increments the program counter on success.
func (p *MacroOpNext) Apply(mm *MacroMachine) error {
	var ok bool
	cdr := mm.curr.Cdr()
	mm.curr, ok = cdr.(*values.Pair)
	if !ok {
		return values.ErrNotAPair
	}
	mm.pc++
	return nil
}
