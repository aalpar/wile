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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"
)

// TestContextWithCallerHandlersReadsTheCallerOnlyWhenARootDoes: starting an
// expansion reads none of the caller's chain; a root reads the caller's
// dynamic environment the first time it needs a mark. So an eval that reads no
// mark costs the same at any depth: equal allocations at depth 10 and 10000
// (a snapshot taken up front grows with the chain's distinct marks), and
// nothing read until the root resolves a parameter, which then sees the
// caller's value.
func TestContextWithCallerHandlersReadsTheCallerOnlyWhenARootDoes(t *testing.T) {
	c := qt.New(t)
	env := environment.NewNamespace().Runtime()
	param := NewParameter(values.NewInteger(1), nil, ImmutableBase)
	caller := func(depth int) *MachineContext {
		mc := newContMarkTestContext()
		mc.cont = NewMachineContinuation(nil, namedTemplate("outermost"), env)
		mc.cont.marks = []markEntry{{key: param, val: values.NewInteger(2)}}
		for i := range depth {
			mc.cont = NewMachineContinuation(mc.cont, namedTemplate("f"), env)
			mc.cont.marks = []markEntry{{key: values.NewInteger(int64(i)), val: values.TrueValue}}
		}
		return mc
	}
	shallow, deep := caller(10), caller(10000)
	c.Assert(testing.AllocsPerRun(20, func() {
		deep.ContextWithCallerHandlers()
	}), qt.Equals, testing.AllocsPerRun(20, func() {
		shallow.ContextWithCallerHandlers()
	}))

	ctx := deep.ContextWithCallerHandlers()
	root := NewMachineContext(ctx, NewMachineContinuation(nil, namedTemplate("root"), env))
	c.Assert(root.inheritCallerHandlers(ctx), qt.IsTrue)
	c.Assert(root.caller.read, qt.IsFalse)
	c.Assert(root.ResolveParameterValue(param), qt.Equals, values.NewInteger(2))
	c.Assert(root.caller.read, qt.IsTrue)
}
