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
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

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

// TestStackTraceToSchemeListOmitsAbsentPositions covers VERDICTS 93, a
// CONFIRMED provenance defect that belonged to no wave and no plan.
//
// stackFrameToAlist tested its source context for NIL rather than for a
// LOCATION. A context that exists and carries no position — every
// foreign-call boundary frame, and every frame of a datum built at runtime
// and handed to eval, which mints a zero-value context — therefore emitted
// (file . "") (line . 0) (column . 0). That is a fabricated line 0,
// indistinguishable from a real one.
//
// It contradicted two things at once. The textual trace printed a bare
// "at <anonymous>" for the same frame, because StackFrame.String asks the
// right question. And both primitives' shipped docstrings promise file, line
// and column are "present only when source information is available" — the
// docstring and the omission branch landed in the same commit, so omission was
// the intended behaviour and this was the branch that missed.
//
// It also contradicted itself: the bare ((name . ...)) form was already
// emitted when BOTH contexts were nil, so one result list interleaved two
// renderings of the same state.
func TestStackTraceToSchemeListOmitsAbsentPositions(t *testing.T) {
	located := syntax.NewSourceContext("(f)", "prog.scm",
		syntax.NewSourceIndexes(10, 3, 5), syntax.NewSourceIndexes(13, 3, 8))
	// A context that EXISTS and has no position. This is the shape the nil
	// test admitted: eval mints exactly this for a runtime-built datum.
	positionless := syntax.NewSourceContext("", "",
		syntax.NewSourceIndexes(0, 0, 0), syntax.NewSourceIndexes(0, 0, 0))
	// Positioned but unnamed: the stdin CLI mode and any embedder calling
	// EvalMultiple without a name. "" is not a filename.
	unnamed := syntax.NewSourceContext("(f)", "",
		syntax.NewSourceIndexes(4, 1, 4), syntax.NewSourceIndexes(7, 1, 7))

	tcs := []struct {
		name      string
		frame     StackFrame
		wantKeys  []string
		wantFalse bool // the file key must be #f rather than a string
	}{
		{
			name:     "located frame keeps all four keys",
			frame:    StackFrame{FunctionName: "f", CurrentLoc: located},
			wantKeys: []string{"name", "file", "line", "column"},
		},
		{
			name:     "position-less CurrentLoc falls through to a located CallSite",
			frame:    StackFrame{FunctionName: "f", CurrentLoc: positionless, CallSite: located},
			wantKeys: []string{"name", "file", "line", "column"},
		},
		{
			name:     "position-less on both sides carries the name alone",
			frame:    StackFrame{FunctionName: "f", CurrentLoc: positionless, CallSite: positionless},
			wantKeys: []string{"name"},
		},
		{
			name:     "nil on both sides carries the name alone",
			frame:    StackFrame{FunctionName: "f"},
			wantKeys: []string{"name"},
		},
		{
			name:      "positioned but unnamed reports file as #f",
			frame:     StackFrame{FunctionName: "f", CurrentLoc: unnamed},
			wantKeys:  []string{"name", "file", "line", "column"},
			wantFalse: true,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			lst := StackTraceToSchemeList(StackTrace{tc.frame})
			alist, ok := lst.Car().(values.Tuple)
			if !ok {
				t.Fatalf("frame entry is %T, want a list", lst.Car())
			}
			var keys []string
			var fileVal values.Value
			cur := values.Value(alist)
			for !values.IsEmptyList(cur) {
				pr, ok := cur.(values.Tuple)
				if !ok {
					t.Fatalf("alist is improper at %v", cur)
				}
				entry, ok := pr.Car().(values.Tuple)
				if !ok {
					t.Fatalf("alist entry is %T, want a pair", pr.Car())
				}
				sym, ok := entry.Car().(*values.Symbol)
				if !ok {
					t.Fatalf("alist key is %T, want a symbol", entry.Car())
				}
				keys = append(keys, sym.Key)
				if sym.Key == "file" {
					fileVal = entry.Cdr()
				}
				cur = pr.Cdr()
			}
			if len(keys) != len(tc.wantKeys) {
				t.Fatalf("keys = %v, want %v", keys, tc.wantKeys)
			}
			for i, want := range tc.wantKeys {
				if keys[i] != want {
					t.Errorf("key %d = %q, want %q", i, keys[i], want)
				}
			}
			if tc.wantFalse {
				if fileVal != values.Value(values.FalseValue) {
					t.Errorf("file = %v, want #f", fileVal)
				}
			}
		})
	}
}
