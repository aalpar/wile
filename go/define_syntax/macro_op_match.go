package define_syntax

import (
	"skeme/match"
	"skeme/values"
)

// MacroOpMatch matches the current element against a literal value.
//
// This operation is used for syntax-rules literals - identifiers that must
// match exactly rather than being treated as pattern variables. Per R7RS,
// literals are specified in the literals list of syntax-rules.
//
// # Behavior
//
// Compares mm.curr.Car() against the stored literal using structural equality
// (EqualTo). If they match, advances the program counter. Otherwise, returns
// a match error.
//
// # Example
//
// For syntax-rules with literal 'else':
//
//	(syntax-rules (else)
//	  ((cond (else e)) e))
//
// The 'else' in the pattern compiles to MacroOpMatch with literal 'else'.
// When matching (cond (else 42)), the 'else' in the input must equal the
// literal 'else' symbol exactly.
type MacroOpMatch struct {
	lit values.Value
}

// NewMacroOpMatch creates a new MacroOpMatch operation for the given literal.
// The literal is the value that must be matched exactly at the current position.
func NewMacroOpMatch(lit values.Value) *MacroOpMatch {
	return &MacroOpMatch{lit: lit}
}

// Apply matches the current element against the literal.
// Returns match.ErrNotAMatch if the values are not equal.
// Increments the program counter on success.
func (p *MacroOpMatch) Apply(mm *MacroMachine) error {
	car := mm.curr.Car()
	if !p.lit.EqualTo(car) {
		return match.ErrNotAMatch
	}
	mm.pc++
	return nil
}
