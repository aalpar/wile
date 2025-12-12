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


package values

import (
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSymbol_EqualTo(t *testing.T) {
	s0 := NewSymbol("a")
	s1 := NewSymbol("b")
	s2 := NewSymbol("a")
	// check pointer equality first
	qt.Assert(t, s0.EqualTo(s1), qt.Equals, false)
	qt.Assert(t, s0.EqualTo(s2), qt.Equals, true)
	qt.Assert(t, s0 == s1, qt.Equals, false)
	qt.Assert(t, s0 == s2, qt.Equals, false)
	qt.Assert(t, *s1 == *s2, qt.Equals, false)
	qt.Assert(t, *s0 == *s2, qt.Equals, true)
}

func TestSymbol_Datum(t *testing.T) {
	s := NewSymbol("test")
	qt.Assert(t, s.Datum(), qt.Equals, "test")
}

func TestSymbol_Copy(t *testing.T) {
	s := NewSymbol("original")
	copyVal := s.Copy()
	copy, ok := copyVal.(*Symbol)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, copy.Key, qt.Equals, "original")
	qt.Assert(t, copy, qt.Not(qt.Equals), s)
}

func TestSymbol_SchemeString(t *testing.T) {
	s := NewSymbol("lambda")
	qt.Assert(t, s.SchemeString(), qt.Equals, "lambda")
}

func TestSymbol_IsVoid(t *testing.T) {
	s := NewSymbol("test")
	qt.Assert(t, s.IsVoid(), qt.IsFalse)

	var nilSym *Symbol
	qt.Assert(t, nilSym.IsVoid(), qt.IsTrue)
}
