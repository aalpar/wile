// Package define_syntax provides a virtual machine for macro pattern matching.
//
// This package implements the pattern matching engine used by syntax-rules macros.
// It provides a stack-based virtual machine (MacroMachine) that executes pattern
// matching operations (MacroOp) against Scheme syntax trees.
//
// # Architecture
//
// The macro system compiles syntax-rules patterns into a sequence of MacroOp
// instructions. These instructions are executed by the MacroMachine to:
//  1. Match literal values and keywords
//  2. Capture pattern variables into bindings
//  3. Navigate nested list structures
//  4. Handle ellipsis patterns (repetition)
//
// # Operation Types
//
// Pattern matching operations:
//   - MacroOpMatch: Match a literal value exactly
//   - MacroOpCapture: Capture a value bound to a pattern variable
//   - MacroOpMatchEmptyList: Assert the current position is at end of list
//
// Navigation operations:
//   - MacroOpNext: Advance to the next element in the current list
//   - MacroOpStartList: Descend into a nested list, pushing context
//   - MacroOpEndList: Return from a nested list, popping context
//
// Binding operations:
//   - MacroOpBind: Bind a captured value to a symbol
//   - MacroOpMatchUntil: Match elements until a condition (for ellipsis)
//
// # Example
//
// For a pattern like ((a b) c), the compiled operations might be:
//
//	MacroOpStartList   // Enter outer list
//	MacroOpStartList   // Enter (a b)
//	MacroOpCapture(a)  // Capture first element as 'a'
//	MacroOpNext        // Move to second element
//	MacroOpCapture(b)  // Capture second element as 'b'
//	MacroOpMatchEmptyList // Assert end of (a b)
//	MacroOpEndList     // Exit (a b)
//	MacroOpNext        // Move to c
//	MacroOpCapture(c)  // Capture as 'c'
//	MacroOpMatchEmptyList // Assert end of outer list
//	MacroOpEndList     // Exit outer list
package define_syntax

import "wile/values"

// ErrMacroHalt is returned when the macro machine has no more operations to execute.
// This is a normal termination condition, not an error.
var (
	ErrMacroHalt = values.NewStaticError("macro halt: no more operations to run")
)

// MacroOp is the interface for macro pattern matching operations.
// Each operation implements a single step in the pattern matching process,
// modifying the MacroMachine state and advancing the program counter.
//
// Operations should return nil on success, or an error (typically match.ErrNotAMatch
// or values.ErrNotAList) on failure.
type MacroOp interface {
	Apply(*MacroMachine) error
}
