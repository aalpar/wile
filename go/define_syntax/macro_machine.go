// Copyright 2025 Aaron Alpar
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


package define_syntax

import (
	"wile/machine"
	"wile/values"
)

// LiteralIndex is the index of a literal value in the MacroMachine's literals pool.
// Literals are values that must be matched exactly during pattern matching,
// as specified in the syntax-rules literals list.
type LiteralIndex int

// KeywordIndex is the index of a keyword in the MacroMachine's keywords list.
// Keywords are the identifiers that trigger macro expansion (e.g., 'let', 'cond').
type KeywordIndex int

// TreeEntry stores pattern variable bindings captured during matching.
//
// During pattern matching, values bound to pattern variables are stored in a
// tree structure. This tree mirrors the nesting structure of the pattern,
// with bindings at each level and children for nested sub-patterns.
//
// For ellipsis patterns, multiple values may be bound to a single variable,
// represented as a slice in the bindings map.
//
// # Example
//
// For pattern ((name val) ...) matching ((x 1) (y 2) (z 3)):
//
//	TreeEntry {
//	  bindings: {
//	    name: [x, y, z],
//	    val:  [1, 2, 3],
//	  },
//	  children: [
//	    TreeEntry{bindings: {name: [x], val: [1]}},
//	    TreeEntry{bindings: {name: [y], val: [2]}},
//	    TreeEntry{bindings: {name: [z], val: [3]}},
//	  ],
//	}
type TreeEntry struct {
	bindings map[values.Symbol][]values.Value
	children []*TreeEntry
}

// NewTreeEntry creates a new empty TreeEntry with initialized maps and slices.
func NewTreeEntry() *TreeEntry {
	q := &TreeEntry{
		bindings: map[values.Symbol][]values.Value{},
		children: []*TreeEntry{},
	}
	return q
}

// MacroMachine is a stack-based virtual machine for macro pattern matching.
//
// The MacroMachine executes a sequence of MacroOp operations to match a
// syntax-rules pattern against input syntax. It maintains:
//   - A current position (curr) in the input being matched
//   - A stack of saved positions for nested list matching
//   - A tree of captured bindings for pattern variables
//   - A program counter for operation sequencing
//
// # Execution Model
//
// The Run() method executes operations sequentially until:
//   - All operations complete successfully (match succeeded)
//   - An operation returns an error (match failed)
//
// Operations modify the machine state (curr, stack, tree) and advance the
// program counter. The machine is stateful and should be reset via SetTarget()
// before matching a new input.
//
// # Literals and Keywords
//
// The literals pool contains values that must be matched exactly (from the
// syntax-rules literals list). The keywords list contains the macro identifiers.
// Both use deduplication to avoid storing the same value multiple times.
type MacroMachine struct {
	target     *values.Pair           // The input wrapped in a list for uniform handling
	literals   machine.MultipleValues // Pool of literal values for exact matching
	keywords   []*values.Symbol       // Macro keyword identifiers
	tree       *TreeEntry             // Root of the binding tree
	curr       *values.Pair           // Current position in input being matched
	stack      []*values.Pair         // Stack of saved positions for nested lists
	operations []MacroOp              // Sequence of pattern matching operations
	pc         int                    // Program counter (current operation index)
}

// NewMacroMachine creates a new MacroMachine with the given keywords, literals, and operations.
//
// Parameters:
//   - keywords: The macro keyword identifiers (e.g., [let, let*])
//   - literals: Values that must match exactly per syntax-rules literals list
//   - ops: The compiled pattern matching operations
func NewMacroMachine(keywords []*values.Symbol, literals machine.MultipleValues, ops []MacroOp) *MacroMachine {
	q := &MacroMachine{
		keywords:   keywords,
		literals:   literals,
		operations: ops,
		tree:       NewTreeEntry(),
	}
	return q
}

// SetTarget prepares the machine to match against a new input.
//
// This method:
//  1. Wraps the target in a list for uniform handling (the pattern matcher
//     expects to start by extracting the car of the current pair)
//  2. Resets the program counter to 0
//  3. Creates a fresh binding tree
//  4. Sets curr to the wrapped target
//
// Must be called before Run() for each new input to match.
func (p *MacroMachine) SetTarget(targ *values.Pair) {
	p.target = values.List(targ)
	p.pc = 0
	p.tree = NewTreeEntry()
	p.curr = p.target
}

// AppendOperations adds operations to the end of the operation sequence.
// Used during pattern compilation to build up the operation list.
func (p *MacroMachine) AppendOperations(mos ...MacroOp) {
	p.operations = append(p.operations, mos...)
}

// Run executes the macro machine until completion or error.
//
// Iterates through operations, calling Apply() on each. Each operation
// modifies the machine state and advances the program counter.
//
// Returns nil if all operations complete successfully (pattern matched).
// Returns an error (typically match.ErrNotAMatch) if matching fails.
func (p *MacroMachine) Run() error {
	for p.pc < len(p.operations) {
		inst := p.operations[p.pc]
		err := inst.Apply(p)
		if err != nil {
			return err
		}
	}
	return nil
}

// MaybeAppendLiteral adds a literal to the pool if not already present.
//
// Uses structural equality (EqualTo) to check for duplicates. This ensures
// that identical literals share the same index, reducing memory usage and
// enabling fast equality checks during matching.
//
// Returns the index of the literal (existing or newly added).
func (p *MacroMachine) MaybeAppendLiteral(v values.Value) LiteralIndex {
	for i, l := range p.literals {
		if l.EqualTo(v) {
			return LiteralIndex(i)
		}
	}
	l := len(p.literals)
	p.literals = append(p.literals, v)
	return LiteralIndex(l)
}

// MaybeAppendKeyword adds a keyword to the keywords list if not already present.
//
// Uses structural equality to check for duplicates. Returns the canonical
// symbol (either the existing one or the newly added one).
//
// Note: Currently appends to literals instead of keywords - this may be a bug
// or intentional for shared storage.
func (p *MacroMachine) MaybeAppendKeyword(v *values.Symbol) *values.Symbol {
	for _, lit := range p.keywords {
		if lit.EqualTo(v) {
			return lit
		}
	}
	p.literals = append(p.literals, v)
	return v
}
