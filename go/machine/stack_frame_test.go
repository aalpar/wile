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
	"wile/syntax"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestStackFrame_String_WithCurrentLoc(t *testing.T) {
	c := qt.New(t)

	frame := StackFrame{
		FunctionName: "my-function",
		CurrentLoc: &syntax.SourceContext{
			File:  "test.scm",
			Start: syntax.NewSourceIndexes(10, 5, 100),
		},
	}

	s := frame.String()
	c.Assert(strings.Contains(s, "my-function"), qt.IsTrue)
	c.Assert(strings.Contains(s, "test.scm"), qt.IsTrue)
	c.Assert(strings.Contains(s, "10"), qt.IsTrue)
	c.Assert(strings.Contains(s, "5"), qt.IsTrue)
}

func TestStackFrame_String_WithCallSite(t *testing.T) {
	c := qt.New(t)

	frame := StackFrame{
		FunctionName: "my-function",
		CallSite: &syntax.SourceContext{
			File:  "caller.scm",
			Start: syntax.NewSourceIndexes(20, 3, 200),
		},
	}

	s := frame.String()
	c.Assert(strings.Contains(s, "my-function"), qt.IsTrue)
	c.Assert(strings.Contains(s, "caller.scm"), qt.IsTrue)
	c.Assert(strings.Contains(s, "called from"), qt.IsTrue)
}

func TestStackFrame_String_Anonymous(t *testing.T) {
	c := qt.New(t)

	frame := StackFrame{
		FunctionName: "",
		CurrentLoc: &syntax.SourceContext{
			File:  "test.scm",
			Start: syntax.NewSourceIndexes(1, 1, 0),
		},
	}

	s := frame.String()
	c.Assert(strings.Contains(s, "<anonymous>"), qt.IsTrue)
}

func TestStackFrame_String_NoSource(t *testing.T) {
	c := qt.New(t)

	frame := StackFrame{
		FunctionName: "my-function",
	}

	s := frame.String()
	c.Assert(strings.Contains(s, "my-function"), qt.IsTrue)
	c.Assert(strings.Contains(s, "at"), qt.IsTrue)
}

func TestStackTrace_String_Empty(t *testing.T) {
	c := qt.New(t)

	var trace StackTrace
	c.Assert(trace.String(), qt.Equals, "")
}

func TestStackTrace_String_SingleFrame(t *testing.T) {
	c := qt.New(t)

	trace := StackTrace{
		{
			FunctionName: "main",
			CurrentLoc: &syntax.SourceContext{
				File:  "main.scm",
				Start: syntax.NewSourceIndexes(1, 1, 0),
			},
		},
	}

	s := trace.String()
	c.Assert(strings.Contains(s, "Stack trace:"), qt.IsTrue)
	c.Assert(strings.Contains(s, "main"), qt.IsTrue)
}

func TestStackTrace_String_MultipleFrames(t *testing.T) {
	c := qt.New(t)

	trace := StackTrace{
		{
			FunctionName: "inner",
			CurrentLoc: &syntax.SourceContext{
				File:  "lib.scm",
				Start: syntax.NewSourceIndexes(50, 1, 500),
			},
		},
		{
			FunctionName: "outer",
			CurrentLoc: &syntax.SourceContext{
				File:  "lib.scm",
				Start: syntax.NewSourceIndexes(10, 1, 100),
			},
		},
		{
			FunctionName: "main",
			CurrentLoc: &syntax.SourceContext{
				File:  "main.scm",
				Start: syntax.NewSourceIndexes(5, 1, 50),
			},
		},
	}

	s := trace.String()
	c.Assert(strings.Contains(s, "Stack trace:"), qt.IsTrue)
	c.Assert(strings.Contains(s, "inner"), qt.IsTrue)
	c.Assert(strings.Contains(s, "outer"), qt.IsTrue)
	c.Assert(strings.Contains(s, "main"), qt.IsTrue)

	// Verify order (inner should appear before outer)
	innerIdx := strings.Index(s, "inner")
	outerIdx := strings.Index(s, "outer")
	mainIdx := strings.Index(s, "main")
	c.Assert(innerIdx < outerIdx, qt.IsTrue)
	c.Assert(outerIdx < mainIdx, qt.IsTrue)
}
