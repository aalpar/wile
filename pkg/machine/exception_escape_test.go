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

	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// ErrExceptionEscape is the uncaught-exception carrier (piece E): handler dispatch
// no longer flows through it, so it carries only Condition / Source / StackTrace.
// (The Continuable / Continuation / Handled fields and their tests were removed.)

func TestErrExceptionEscape_Error_NilCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: nil,
	}

	c.Assert(err.Error(), qt.Equals, "exception: <nil>")
}

func TestErrExceptionEscape_Error_WithCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewString("test error"),
	}

	c.Assert(err.Error(), qt.Equals, "exception: \"test error\"")
}

func TestErrExceptionEscape_Error_WithSymbol(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewSymbol("error-type"),
	}

	c.Assert(err.Error(), qt.Equals, "exception: error-type")
}

func TestErrExceptionEscape_Error_WithInteger(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewInteger(42),
	}

	c.Assert(err.Error(), qt.Equals, "exception: 42")
}

func TestErrExceptionEscape_Unwrap(t *testing.T) {
	c := qt.New(t)

	// A NativeError condition implements error, so Unwrap exposes it for
	// errors.Is/errors.As sentinel matching from Go callers.
	ne := values.NewErrorObject("boom")
	err := &ErrExceptionEscape{Condition: ne}
	c.Assert(err.Unwrap(), qt.Equals, error(ne))

	// A non-error condition yields a nil unwrap.
	err2 := &ErrExceptionEscape{Condition: values.NewSymbol("sym")}
	c.Assert(err2.Unwrap(), qt.IsNil)
}

func TestErrExceptionEscape_Error_WithSource(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		err  *ErrExceptionEscape
		want string
	}{
		{
			"NativeError with source",
			&ErrExceptionEscape{
				Condition: values.NewErrorObject("division by zero"),
				Source: syntax.NewSourceContext("", "example.scm",
					syntax.NewSourceIndexes(0, 3, 5),
					syntax.NewSourceIndexes(0, 10, 5)),
			},
			"example.scm:5:3: error: division by zero",
		},
		{
			"non-error condition with source",
			&ErrExceptionEscape{
				Condition: values.NewSymbol("my-error"),
				Source: syntax.NewSourceContext("", "test.scm",
					syntax.NewSourceIndexes(0, 1, 10),
					syntax.NewSourceIndexes(0, 5, 10)),
			},
			"test.scm:10:1: exception: my-error",
		},
		{
			"NativeError without source falls back",
			&ErrExceptionEscape{
				Condition: values.NewErrorObject("boom"),
			},
			`exception: #<error-object "boom">`,
		},
		{
			"nil condition with source",
			&ErrExceptionEscape{
				Condition: nil,
				Source: syntax.NewSourceContext("", "nil.scm",
					syntax.NewSourceIndexes(0, 0, 1),
					syntax.NewSourceIndexes(0, 0, 1)),
			},
			"nil.scm:1:0: exception: <nil>",
		},
		{
			"backward compat: no source no stack",
			&ErrExceptionEscape{
				Condition: values.NewInteger(42),
			},
			"exception: 42",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c.Assert(tc.err.Error(), qt.Equals, tc.want)
		})
	}
}

func TestErrExceptionEscape_Error_WithStackTrace(t *testing.T) {
	c := qt.New(t)

	source := syntax.NewSourceContext("", "example.scm",
		syntax.NewSourceIndexes(0, 3, 5),
		syntax.NewSourceIndexes(0, 10, 5))

	trace := StackTrace{
		{
			FunctionName: "f",
			CurrentLoc:   source,
		},
	}

	err := &ErrExceptionEscape{
		Condition:  values.NewErrorObject("boom"),
		Source:     source,
		StackTrace: trace,
	}

	got := err.Error()
	c.Assert(got, qt.Contains, "example.scm:5:3: error: boom")
	c.Assert(got, qt.Contains, "Stack trace:")
	c.Assert(got, qt.Contains, "at f (example.scm:5:3)")
}
