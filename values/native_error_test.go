// Copyright 2026 Aaron Alpar
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

package values_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/werr"
)

func TestNativeError_EqualTo(t *testing.T) {
	c := qt.New(t)

	a := values.NewErrorObject("test")
	b := values.NewErrorObject("test")
	c.Assert(a.EqualTo(b), qt.IsTrue)

	// Different messages
	d := values.NewErrorObject("other")
	c.Assert(a.EqualTo(d), qt.IsFalse)
}

func TestNativeError_SourceAndStackTrace_ZeroValues(t *testing.T) {
	c := qt.New(t)

	ne := values.NewErrorObject("test error", values.NewInteger(42))

	// Initially zero values
	c.Assert(ne.SourceLocation(), qt.Equals, "")
	c.Assert(ne.StackTraceValue(), qt.IsNil)
}

func TestNativeError_SourceAndStackTrace_Roundtrip(t *testing.T) {
	c := qt.New(t)

	ne := values.NewErrorObject("test error", values.NewInteger(42))

	// Set source location
	ne.SetSourceLocation("test.scm:5:3")
	c.Assert(ne.SourceLocation(), qt.Equals, "test.scm:5:3")

	// Set stack trace (as arbitrary Value — will be a Scheme list in practice)
	traceList := values.List(values.NewInteger(1))
	ne.SetStackTraceValue(traceList)
	c.Assert(ne.StackTraceValue(), qt.IsNotNil)
}

func TestNativeError_EqualTo_IgnoresSourceAndTrace(t *testing.T) {
	c := qt.New(t)

	a := values.NewErrorObject("test")
	b := values.NewErrorObject("test")

	a.SetSourceLocation("a.scm:1:0")
	b.SetSourceLocation("b.scm:2:0")

	a.SetStackTraceValue(values.List(values.NewInteger(1)))
	b.SetStackTraceValue(values.List(values.NewInteger(2)))

	// Source and stack trace do not affect equality
	c.Assert(a.EqualTo(b), qt.IsTrue)
}

func TestNativeError_NilReceiver_SourceAndTrace(t *testing.T) {
	c := qt.New(t)

	var ne *values.NativeError

	// Nil receiver returns zero values, does not panic
	c.Assert(ne.SourceLocation(), qt.Equals, "")
	c.Assert(ne.StackTraceValue(), qt.IsNil)

	// Setters on nil receiver do not panic
	ne.SetSourceLocation("x.scm:1:0")
	ne.SetStackTraceValue(values.NewInteger(1))
}

func TestNewFileError_SetsKindFile(t *testing.T) {
	c := qt.New(t)
	err := values.NewFileError("file not found", values.NewString("/tmp/foo"))
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindFile)
	c.Assert(err.IsFileError(), qt.IsTrue)
	c.Assert(err.IsReadError(), qt.IsFalse)
}

func TestNewReadError_SetsKindRead(t *testing.T) {
	c := qt.New(t)
	err := values.NewReadError("unexpected token")
	c.Assert(err.Kind(), qt.Equals, values.NativeErrorKindRead)
	c.Assert(err.IsReadError(), qt.IsTrue)
	c.Assert(err.IsFileError(), qt.IsFalse)
}

func TestNewErrorObjectWithCauseAndKind(t *testing.T) {
	c := qt.New(t)
	tests := []struct {
		name string
		kind values.NativeErrorKind
		file bool
		read bool
	}{
		{"generic", values.NativeErrorKindGeneric, false, false},
		{"file", values.NativeErrorKindFile, true, false},
		{"read", values.NativeErrorKindRead, false, true},
	}
	for _, tt := range tests {
		c.Run(tt.name, func(c *qt.C) {
			cause := werr.NewForeignErrorf("underlying error")
			err := values.NewErrorObjectWithCauseAndKind("msg", cause, tt.kind)
			c.Assert(err.Kind(), qt.Equals, tt.kind)
			c.Assert(err.IsFileError(), qt.Equals, tt.file)
			c.Assert(err.IsReadError(), qt.Equals, tt.read)
			c.Assert(err.Unwrap(), qt.Equals, cause)
		})
	}
}
