package define_syntax

import (
	"skeme/match"
	"skeme/values"
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
