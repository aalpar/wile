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
	"wile/match"
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMacroMachine1(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
}

func TestMacroMachine2(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	ki0 := mm.MaybeAppendKeyword(values.NewSymbol("foo"))
	ki1 := mm.MaybeAppendKeyword(values.NewSymbol("bar"))
	ki2 := mm.MaybeAppendKeyword(values.NewSymbol("foo"))
	qt.Assert(t, ki0, values.SchemeEquals, ki2)
	qt.Assert(t, ki0, qt.Not(qt.Equals), ki1)
}

func TestMacroMachine3(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	ki0 := mm.MaybeAppendLiteral(values.NewSymbol("foo"))
	ki1 := mm.MaybeAppendLiteral(values.NewSymbol("bar"))
	ki2 := mm.MaybeAppendLiteral(values.NewSymbol("foo"))
	qt.Assert(t, ki0, qt.Equals, ki2)
	qt.Assert(t, ki0, qt.Not(qt.Equals), ki1)
}

func TestMacroMachine4(t *testing.T) {
	// Successful match
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("bar")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
	//
	// Negative case
	//
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)
}

func TestMacroMachine5(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(
		values.List(
			values.List(
				values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")),
			values.NewSymbol("qux")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("bar")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
		NewMacroOpMatch(values.NewSymbol("qux")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
	//
	// Negative case
	//
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(
		values.List(
			values.List(
				values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")),
			values.NewSymbol("qux")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)
}

func TestMacroMachine6(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(
		values.List(
			values.List(
				values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")),
			values.NewSymbol("qux")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("bar")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
		NewMacroOpMatch(values.NewSymbol("qux")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
	//
	// Negative case
	//
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(
		values.List(
			values.List(
				values.NewSymbol("foo"), values.NewSymbol("bar"), values.NewSymbol("baz")),
			values.NewSymbol("qux")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("foo")),
		NewMacroOpNext(),
		NewMacroOpMatch(values.NewSymbol("baz")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(),
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)
}

// TestMacroOpCapture tests the MacroOpCapture operation which verifies
// that a pattern variable appears at the expected position.
func TestMacroOpCapture(t *testing.T) {
	// Successful capture - symbol matches
	mm := NewMacroMachine(nil, nil, nil)
	sym := values.NewSymbol("x")
	mm.SetTarget(values.List(sym))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpCapture(sym),
		NewMacroOpMatchEmptyList(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)

	// Failed capture - symbols don't match
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("y")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpCapture(values.NewSymbol("x")),
		NewMacroOpMatchEmptyList(),
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)

	// Failed capture - not a symbol
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewInteger(42)))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpCapture(values.NewSymbol("x")),
		NewMacroOpMatchEmptyList(),
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)
}

// TestMacroOpBind tests the MacroOpBind operation (currently a placeholder).
func TestMacroOpBind(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("x")))
	mm.AppendOperations(
		NewMacroOpBind(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestMacroOpMatchUntil tests the MacroOpMatchUntil operation (currently a placeholder).
func TestMacroOpMatchUntil(t *testing.T) {
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("x"), values.NewSymbol("y"), values.NewSymbol("z")))
	mm.AppendOperations(
		NewMacroOpMatchUntil(),
	)
	err := mm.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestMacroOpNext_ErrorCases tests error conditions for MacroOpNext.
func TestMacroOpNext_ErrorCases(t *testing.T) {
	// Test when cdr is an improper list terminator
	mm := NewMacroMachine(nil, nil, nil)
	improperList := values.NewCons(values.NewSymbol("x"), values.NewInteger(42))
	mm.SetTarget(values.List(improperList))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("x")),
		NewMacroOpNext(), // This should fail because cdr is 42, not a pair
	)
	err := mm.Run()
	qt.Assert(t, err, qt.ErrorIs, values.ErrNotAPair)
}

// TestMacroOpStartList_ErrorCases tests error conditions for MacroOpStartList.
func TestMacroOpStartList_ErrorCases(t *testing.T) {
	// Test when trying to descend into a non-list (symbol)
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("x")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(), // This should fail because car is a symbol, not a list
	)
	err := mm.Run()
	qt.Assert(t, err, qt.ErrorIs, values.ErrNotAList)

	// Test when trying to descend into a number
	mm = NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewInteger(42)))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(), // This should fail because car is an integer
	)
	err = mm.Run()
	qt.Assert(t, err, qt.ErrorIs, values.ErrNotAList)
}

// TestMacroOpEndList_ErrorCases tests error conditions for MacroOpEndList.
func TestMacroOpEndList_ErrorCases(t *testing.T) {
	// Test when the structure after a nested list is not a pair (improper list)
	mm := NewMacroMachine(nil, nil, nil)
	// Create an improper list: ((x) . 42)
	innerList := values.List(values.NewSymbol("x"))
	improperList := values.NewCons(innerList, values.NewInteger(42))
	mm.SetTarget(values.List(improperList))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("x")),
		NewMacroOpMatchEmptyList(),
		NewMacroOpEndList(), // This should fail because cdr of saved position is 42, not a pair
	)
	err := mm.Run()
	qt.Assert(t, err, qt.Not(qt.IsNil))
}

// TestMacroOpMatchEmptyList_ErrorCases tests error conditions for MacroOpMatchEmptyList.
func TestMacroOpMatchEmptyList_ErrorCases(t *testing.T) {
	// Test when list has remaining elements
	mm := NewMacroMachine(nil, nil, nil)
	mm.SetTarget(values.List(values.NewSymbol("x"), values.NewSymbol("y")))
	mm.AppendOperations(
		NewMacroOpStartList(),
		NewMacroOpMatch(values.NewSymbol("x")),
		NewMacroOpMatchEmptyList(), // This should fail because there's still "y"
	)
	err := mm.Run()
	qt.Assert(t, err, qt.ErrorIs, match.ErrNotAMatch)
}

// TestMaybeAppendKeyword_AppendPath tests that MaybeAppendKeyword actually
// appends a new keyword when it's not already present.
func TestMaybeAppendKeyword_AppendPath(t *testing.T) {
	// First, pre-populate keywords to test the deduplication path
	keywords := []*values.Symbol{values.NewSymbol("foo")}
	mm := NewMacroMachine(keywords, nil, nil)

	// Duplicate keyword should return the existing one from keywords
	sym1 := values.NewSymbol("foo")
	result1 := mm.MaybeAppendKeyword(sym1)
	qt.Assert(t, result1, values.SchemeEquals, keywords[0])

	// New keyword should be appended to literals (note: not keywords - see implementation)
	sym2 := values.NewSymbol("bar")
	result2 := mm.MaybeAppendKeyword(sym2)
	qt.Assert(t, result2, values.SchemeEquals, sym2)
	qt.Assert(t, len(mm.literals), qt.Equals, 1)

	// Another duplicate of the one we just added (now in literals, not keywords)
	// This will NOT deduplicate because it only checks keywords, not literals
	sym3 := values.NewSymbol("bar")
	result3 := mm.MaybeAppendKeyword(sym3)
	qt.Assert(t, result3, values.SchemeEquals, sym3)
	qt.Assert(t, len(mm.literals), qt.Equals, 2)
}
