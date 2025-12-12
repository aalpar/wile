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
	"errors"
	"wile/syntax"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestSchemeError_NewSchemeError(t *testing.T) {
	c := qt.New(t)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}
	err := NewSchemeError("test error", source, "stack trace here")

	c.Assert(err.Message, qt.Equals, "test error")
	c.Assert(err.Source, qt.Equals, source)
	c.Assert(err.StackTrace, qt.Equals, "stack trace here")
	c.Assert(err.Cause, qt.IsNil)
}

func TestSchemeError_NewSchemeErrorWithCause(t *testing.T) {
	c := qt.New(t)

	cause := errors.New("underlying error")
	err := NewSchemeErrorWithCause("wrapper error", nil, "", cause)

	c.Assert(err.Message, qt.Equals, "wrapper error")
	c.Assert(err.Cause, qt.Equals, cause)
}

func TestSchemeError_Error_WithSource(t *testing.T) {
	c := qt.New(t)

	source := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(10, 5, 100),
	}
	err := NewSchemeError("undefined variable", source, "")

	s := err.Error()
	c.Assert(strings.Contains(s, "test.scm"), qt.IsTrue)
	c.Assert(strings.Contains(s, "10"), qt.IsTrue)
	c.Assert(strings.Contains(s, "5"), qt.IsTrue)
	c.Assert(strings.Contains(s, "undefined variable"), qt.IsTrue)
}

func TestSchemeError_Error_WithStackTrace(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("error message", nil, "Stack trace:\n  at foo")

	s := err.Error()
	c.Assert(strings.Contains(s, "error message"), qt.IsTrue)
	c.Assert(strings.Contains(s, "Stack trace:"), qt.IsTrue)
	c.Assert(strings.Contains(s, "at foo"), qt.IsTrue)
}

func TestSchemeError_Error_NoSource(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("simple error", nil, "")

	c.Assert(err.Error(), qt.Equals, "simple error")
}

func TestSchemeError_Unwrap(t *testing.T) {
	c := qt.New(t)

	cause := errors.New("root cause")
	err := NewSchemeErrorWithCause("wrapper", nil, "", cause)

	c.Assert(errors.Unwrap(err), qt.Equals, cause)
}

func TestSchemeError_Unwrap_NoCause(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("no cause", nil, "")

	c.Assert(errors.Unwrap(err), qt.IsNil)
}

func TestSchemeError_SchemeString(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("test error", nil, "")

	c.Assert(err.SchemeString(), qt.Equals, "#<error: test error>")
}

func TestSchemeError_IsVoid(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("test", nil, "")

	c.Assert(err.IsVoid(), qt.IsFalse)
}

func TestSchemeError_EqualTo(t *testing.T) {
	c := qt.New(t)

	err1 := NewSchemeError("same message", nil, "")
	err2 := NewSchemeError("same message", nil, "different trace")
	err3 := NewSchemeError("different message", nil, "")

	c.Assert(err1.EqualTo(err2), qt.IsTrue)  // Same message
	c.Assert(err1.EqualTo(err3), qt.IsFalse) // Different message
}

func TestSchemeError_EqualTo_NonSchemeError(t *testing.T) {
	c := qt.New(t)

	err := NewSchemeError("test", nil, "")

	c.Assert(err.EqualTo(nil), qt.IsFalse)
}
