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
	"errors"
	"strings"
	"testing"
)

func TestError_NewError(t *testing.T) {
	e := NewError("test error")
	qt.Assert(t, e, qt.Not(qt.IsNil))
	qt.Assert(t, e.Error(), qt.Equals, "test error")
}

func TestError_NewErrorf(t *testing.T) {
	e := NewErrorf("error: %s %d", "test", 42)
	qt.Assert(t, e.Error(), qt.Equals, "error: test 42")
}

func TestError_WrapErrorf(t *testing.T) {
	base := errors.New("base error")
	e := WrapErrorf(base, "wrapped: %s", "context")
	qt.Assert(t, strings.Contains(e.Error(), "wrapped: context"), qt.IsTrue)
}

func TestError_Unwrap(t *testing.T) {
	e := NewError("test message")
	unwrapped := e.Unwrap()
	qt.Assert(t, unwrapped, SchemeEquals, NewString("test message"))
}

func TestError_IsVoid(t *testing.T) {
	e := NewError("test")
	qt.Assert(t, e.IsVoid(), qt.IsFalse)

	var nilErr *Error
	qt.Assert(t, nilErr.IsVoid(), qt.IsTrue)
}

func TestError_Datum(t *testing.T) {
	e := NewError("test")
	datum := e.Datum()
	qt.Assert(t, datum, qt.Equals, "test")
}

func TestError_EqualTo(t *testing.T) {
	e1 := NewError("test")
	qt.Assert(t, e1.EqualTo(e1), qt.IsTrue)

	e2 := NewError("test")
	qt.Assert(t, e1.EqualTo(e2), qt.IsFalse)

	i := NewInteger(42)
	qt.Assert(t, e1.EqualTo(i), qt.IsFalse)
}

func TestError_SchemeString(t *testing.T) {
	e := NewError("test error")
	s := e.SchemeString()
	qt.Assert(t, strings.Contains(s, "test error"), qt.IsTrue)
}

func TestForeignError_NewForeignError(t *testing.T) {
	e := NewForeignError("foreign error")
	qt.Assert(t, e, qt.Not(qt.IsNil))
	qt.Assert(t, e.Error(), qt.Equals, "foreign error")
}

func TestForeignError_NewForeignErrorf(t *testing.T) {
	e := NewForeignErrorf("error: %s", "formatted")
	qt.Assert(t, strings.Contains(e.Error(), "formatted"), qt.IsTrue)
}

func TestForeignError_WrapForeignErrorf(t *testing.T) {
	base := errors.New("base")
	e := WrapForeignErrorf(base, "wrapped")
	qt.Assert(t, strings.Contains(e.Error(), "wrapped"), qt.IsTrue)
	qt.Assert(t, strings.Contains(e.Error(), "base"), qt.IsTrue)
}

func TestForeignError_Unwrap(t *testing.T) {
	base := errors.New("base")
	e := WrapForeignErrorf(base, "wrapped")
	unwrapped := e.Unwrap()
	qt.Assert(t, unwrapped, qt.Equals, base)
}

func TestForeignError_Datum(t *testing.T) {
	base := errors.New("base")
	e := WrapForeignErrorf(base, "wrapped")
	datum := e.Datum()
	qt.Assert(t, datum, qt.Equals, base)

	e2 := NewForeignError("test")
	datum2 := e2.Datum()
	qt.Assert(t, datum2, qt.IsNil)
}
