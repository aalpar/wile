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

	"wile/values"

	qt "github.com/frankban/quicktest"
)

func TestNewExceptionHandler(t *testing.T) {
	c := qt.New(t)

	handler := values.NewString("handler-proc")
	eh := NewExceptionHandler(handler, nil)

	c.Assert(eh, qt.IsNotNil)
	c.Assert(eh.Handler(), qt.Equals, handler)
	c.Assert(eh.Parent(), qt.IsNil)
}

func TestExceptionHandler_WithParent(t *testing.T) {
	c := qt.New(t)

	parentHandler := values.NewString("parent-handler")
	parent := NewExceptionHandler(parentHandler, nil)

	childHandler := values.NewString("child-handler")
	child := NewExceptionHandler(childHandler, parent)

	c.Assert(child.Handler(), qt.Equals, childHandler)
	c.Assert(child.Parent(), qt.Equals, parent)
	c.Assert(child.Parent().Handler(), qt.Equals, parentHandler)
}

func TestExceptionHandler_Chain(t *testing.T) {
	c := qt.New(t)

	h1 := NewExceptionHandler(values.NewString("h1"), nil)
	h2 := NewExceptionHandler(values.NewString("h2"), h1)
	h3 := NewExceptionHandler(values.NewString("h3"), h2)

	// Walk the chain
	c.Assert(h3.Parent(), qt.Equals, h2)
	c.Assert(h3.Parent().Parent(), qt.Equals, h1)
	c.Assert(h3.Parent().Parent().Parent(), qt.IsNil)
}
