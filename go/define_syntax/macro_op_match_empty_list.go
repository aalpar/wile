package define_syntax

import (
	"skeme/match"
	"skeme/values"
)

// MacroOpMatchEmptyList asserts that the current position is at the end of a list.
//
// This operation verifies that mm.curr.Cdr() is the empty list, indicating
// that all elements in the current list have been consumed by the pattern.
// It's used to ensure patterns match the exact structure of the input.
//
// # Behavior
//
// Checks if the cdr of the current pair is values.EmptyList. If so, advances
// the program counter. Otherwise, returns a match error.
//
// # Example
//
// For pattern (a b) matching input (1 2):
//
//	MacroOpCapture(a)      // Match 'a' against 1
//	MacroOpNext            // Advance to (2)
//	MacroOpCapture(b)      // Match 'b' against 2
//	MacroOpMatchEmptyList  // Verify no more elements
//
// If the input were (1 2 3), MacroOpMatchEmptyList would fail because
// after matching 'b', the cdr would be (3), not the empty list.
type MacroOpMatchEmptyList struct{}

// NewMacroOpMatchEmptyList creates a new MacroOpMatchEmptyList operation.
func NewMacroOpMatchEmptyList() *MacroOpMatchEmptyList {
	return &MacroOpMatchEmptyList{}
}

// Apply verifies the current list ends here (cdr is empty list).
// Returns match.ErrNotAMatch if there are remaining elements.
// Increments the program counter on success.
func (p *MacroOpMatchEmptyList) Apply(mm *MacroMachine) error {
	if !values.IsEmptyList(mm.curr.Cdr()) {
		return match.ErrNotAMatch
	}
	mm.pc++
	return nil
}
