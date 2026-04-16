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

package machine

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"
)

func TestErrorContext_Value(t *testing.T) {
	c := qt.New(t)

	src := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(0, 0, 1),
		End:   syntax.NewSourceIndexes(4, 4, 1),
	}
	trace := StackTrace{
		{FunctionName: "f", CurrentLoc: src},
	}
	ctx := NewErrorContext(src, trace, nil)

	c.Assert(ctx.IsVoid(), qt.IsFalse)
	c.Assert(ctx.SchemeString(), qt.Matches, `#<error-context.*>`)
	c.Assert(ctx.EqualTo(ctx), qt.IsTrue)

	other := NewErrorContext(nil, nil, nil)
	c.Assert(ctx.EqualTo(other), qt.IsFalse)
}

func TestErrorContext_Accessors(t *testing.T) {
	c := qt.New(t)

	src := &syntax.SourceContext{
		File:  "test.scm",
		Start: syntax.NewSourceIndexes(0, 3, 5),
	}
	trace := StackTrace{
		{FunctionName: "f", CurrentLoc: src},
		{FunctionName: "g", CallSite: src},
	}
	ctx := NewErrorContext(src, trace, nil)

	c.Assert(ctx.Source(), qt.Equals, src)
	c.Assert(ctx.StackTraceFrames(), qt.HasLen, 2)
	c.Assert(ctx.Marks(), qt.IsNil)

	// Source location string uses SourceContext.Location() format: "file:line:col"
	c.Assert(ctx.SourceLocation(), qt.Equals, "test.scm:5:3")
}

func TestErrorContext_NilSource(t *testing.T) {
	c := qt.New(t)

	ctx := NewErrorContext(nil, nil, nil)
	c.Assert(ctx.SourceLocation(), qt.Equals, "")
	c.Assert(ctx.StackTraceFrames(), qt.HasLen, 0)
}

func TestErrorContext_NilReceiver(t *testing.T) {
	c := qt.New(t)

	var ctx *ErrorContext
	c.Assert(ctx.IsVoid(), qt.IsTrue)
	c.Assert(ctx.SchemeString(), qt.Equals, "#<error-context>")
	c.Assert(ctx.Source(), qt.IsNil)
	c.Assert(ctx.SourceLocation(), qt.Equals, "")
	c.Assert(ctx.StackTraceFrames(), qt.HasLen, 0)
	c.Assert(ctx.Marks(), qt.IsNil)
	c.Assert(ctx.EqualTo(ctx), qt.IsTrue)
}

func TestErrorContext_EqualTo_PointerIdentity(t *testing.T) {
	c := qt.New(t)

	a := NewErrorContext(nil, nil, nil)
	b := NewErrorContext(nil, nil, nil)

	// Same pointer => equal
	c.Assert(a.EqualTo(a), qt.IsTrue)
	// Different pointer => not equal (even with identical contents)
	c.Assert(a.EqualTo(b), qt.IsFalse)
	// Different type => not equal
	c.Assert(a.EqualTo(values.TrueValue), qt.IsFalse)
}

func TestErrorContext_WithMarks(t *testing.T) {
	c := qt.New(t)

	marks := &ContinuationMarkSet{}
	ctx := NewErrorContext(nil, nil, marks)

	c.Assert(ctx.Marks(), qt.Equals, marks)
}

func TestErrorContextKey(t *testing.T) {
	c := qt.New(t)

	// Key is not nil
	c.Assert(ErrorContextKey(), qt.IsNotNil)

	// Same key returned each time (singleton)
	c.Assert(ErrorContextKey(), qt.Equals, ErrorContextKey())
}
