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


package machine

import (
	"wile/values"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestMultipleValues_NewMultipleValues(t *testing.T) {
	mv := NewMultipleValues(
		values.NewInteger(1),
		values.NewInteger(2),
		values.NewInteger(3),
	)

	qt.Assert(t, mv, qt.HasLen, 3)
	qt.Assert(t, mv[0], values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, mv[1], values.SchemeEquals, values.NewInteger(2))
	qt.Assert(t, mv[2], values.SchemeEquals, values.NewInteger(3))
}

func TestMultipleValues_Length(t *testing.T) {
	mv := NewMultipleValues(
		values.NewInteger(1),
		values.NewInteger(2),
	)

	qt.Assert(t, mv.Length(), qt.Equals, 2)

	empty := NewMultipleValues()
	qt.Assert(t, empty.Length(), qt.Equals, 0)
}

func TestMultipleValues_Copy(t *testing.T) {
	mv := NewMultipleValues(
		values.NewInteger(1),
		values.NewInteger(2),
	)

	cp := mv.Copy()

	qt.Assert(t, cp.Length(), qt.Equals, 2)
	qt.Assert(t, cp[0], values.SchemeEquals, values.NewInteger(1))
	qt.Assert(t, cp[1], values.SchemeEquals, values.NewInteger(2))
	// Verify it's a different slice
	qt.Assert(t, &cp[0] != &mv[0], qt.IsTrue)
}

func TestMultipleValues_IsVoid(t *testing.T) {
	// Nil is void
	var nilMv MultipleValues
	qt.Assert(t, nilMv.IsVoid(), qt.IsTrue)

	// Empty is void
	empty := NewMultipleValues()
	qt.Assert(t, empty.IsVoid(), qt.IsTrue)

	// Single Void value is void
	singleVoid := NewMultipleValues(values.Void)
	qt.Assert(t, singleVoid.IsVoid(), qt.IsTrue)

	// Single non-void value is not void
	singleValue := NewMultipleValues(values.NewInteger(42))
	qt.Assert(t, singleValue.IsVoid(), qt.IsFalse)

	// Multiple values is not void
	multiple := NewMultipleValues(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, multiple.IsVoid(), qt.IsFalse)
}

func TestMultipleValues_SchemeString(t *testing.T) {
	// Empty
	empty := NewMultipleValues()
	qt.Assert(t, empty.SchemeString(), qt.Equals, values.SpecialVoid)

	// Single void
	singleVoid := NewMultipleValues(values.Void)
	qt.Assert(t, singleVoid.SchemeString(), qt.Equals, values.SpecialVoid)

	// Single value
	single := NewMultipleValues(values.NewInteger(42))
	qt.Assert(t, single.SchemeString(), qt.Equals, "42")

	// Two values - special case to test with just 2
	two := NewMultipleValues(values.NewInteger(1), values.NewInteger(2))
	qt.Assert(t, two.SchemeString(), qt.Equals, "12")

	// Multiple values - the implementation concatenates first element without space,
	// then adds space between subsequent elements
	multiple := NewMultipleValues(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))
	qt.Assert(t, multiple.SchemeString(), qt.Equals, "12 3")
}

func TestMultipleValues_EqualTo(t *testing.T) {
	mv1 := NewMultipleValues(values.NewInteger(1), values.NewInteger(2))
	mv2 := NewMultipleValues(values.NewInteger(1), values.NewInteger(2))
	mv3 := NewMultipleValues(values.NewInteger(1), values.NewInteger(3))
	mv4 := NewMultipleValues(values.NewInteger(1))

	// Equal
	qt.Assert(t, mv1.EqualTo(mv2), qt.IsTrue)

	// Different values
	qt.Assert(t, mv1.EqualTo(mv3), qt.IsFalse)

	// Different length
	qt.Assert(t, mv1.EqualTo(mv4), qt.IsFalse)

	// Different type
	qt.Assert(t, mv1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}
