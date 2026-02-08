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
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestErrExceptionEscape_Error_NilCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   nil,
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: <nil>")
}

func TestErrExceptionEscape_Error_WithCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewString("test error"),
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: \"test error\"")
}

func TestErrExceptionEscape_Error_WithSymbol(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewSymbol("error-type"),
		Continuable: true,
	}

	c.Assert(err.Error(), qt.Equals, "exception: error-type")
}

func TestErrExceptionEscape_Error_WithInteger(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewInteger(42),
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: 42")
}

func TestErrExceptionEscape_Continuable(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewString("continuable"), //nolint:govet
		Continuable: true,
	}

	c.Assert(err.Continuable, qt.IsTrue)
}

func TestErrExceptionEscape_WithContinuation(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	err := &ErrExceptionEscape{
		Condition:    values.NewString("error"), //nolint:govet
		Continuable:  true,                      //nolint:govet
		Continuation: cont,
	}

	c.Assert(err.Continuation, qt.Equals, cont)
}

func TestErrExceptionEscape_Handled(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewString("handled"), //nolint:govet
		Handled:   true,
	}

	c.Assert(err.Handled, qt.IsTrue)
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
