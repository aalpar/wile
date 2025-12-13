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

	"wile/environment"
	"wile/values"

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
		Condition:   values.NewString("continuable"),
		Continuable: true,
	}

	c.Assert(err.Continuable, qt.IsTrue)
}

func TestErrExceptionEscape_WithContinuation(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	err := &ErrExceptionEscape{
		Condition:    values.NewString("error"),
		Continuable:  true,
		Continuation: cont,
	}

	c.Assert(err.Continuation, qt.Equals, cont)
}

func TestErrExceptionEscape_Handled(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewString("handled"),
		Handled:   true,
	}

	c.Assert(err.Handled, qt.IsTrue)
}
