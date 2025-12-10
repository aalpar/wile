package define_syntax

// MacroOpMatchUntil matches elements until a termination condition.
//
// This operation supports ellipsis patterns in syntax-rules. An ellipsis (...)
// indicates that the preceding pattern element may be repeated zero or more times.
// MacroOpMatchUntil iterates through the input, matching the repeated pattern
// until it encounters a non-matching element or the end of the list.
//
// # Behavior
//
// Currently a placeholder that advances the program counter.
// Full implementation would:
//  1. Save the current pattern position
//  2. Attempt to match the repeated pattern against successive elements
//  3. Collect bound values for each successful match
//  4. Stop when the pattern fails to match or the list ends
//  5. Restore position for the next pattern element
//
// # Example
//
// For pattern (a ...) matching (1 2 3):
//   - MacroOpMatchUntil matches 1, 2, 3 as repetitions of 'a'
//   - 'a' is bound to a list of captured values: (1 2 3)
//
// For pattern ((name val) ...) matching ((x 1) (y 2)):
//   - MacroOpMatchUntil matches each pair
//   - 'name' is bound to (x y)
//   - 'val' is bound to (1 2)
type MacroOpMatchUntil struct{}

// NewMacroOpMatchUntil creates a new MacroOpMatchUntil operation.
func NewMacroOpMatchUntil() *MacroOpMatchUntil {
	return &MacroOpMatchUntil{}
}

// Apply matches repeated pattern elements.
// Currently a placeholder that only advances the program counter.
func (p *MacroOpMatchUntil) Apply(mm *MacroMachine) error {
	mm.pc++
	return nil
}
