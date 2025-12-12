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
	qt "github.com/frankban/quicktest"
	"testing"
)

func TestString_EqualTo(t *testing.T) {
	s1 := NewString("hello")
	s2 := NewString("hello")
	qt.Assert(t, s1.EqualTo(s2), qt.IsTrue)

	s3 := NewString("world")
	qt.Assert(t, s1.EqualTo(s3), qt.IsFalse)

	i := NewInteger(42)
	qt.Assert(t, s1.EqualTo(i), qt.IsFalse)
}

func TestString_Datum(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.Datum(), qt.Equals, "hello")
}

func TestString_String(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.String(), qt.Equals, "hello")
}

func TestString_SchemeString(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.SchemeString(), qt.Equals, `"hello"`)
}

func TestString_IsVoid(t *testing.T) {
	s := NewString("hello")
	qt.Assert(t, s.IsVoid(), qt.IsFalse)

	var nilString *String
	qt.Assert(t, nilString.IsVoid(), qt.IsTrue)
}
