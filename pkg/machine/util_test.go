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

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/values"

	qt "github.com/frankban/quicktest"
)

// newNamespace creates a minimal namespace with core special form bindings.
// Does NOT register syntax compilers or primitive expanders (those moved to
// compilation/). Tests that need the full pipeline should be external tests
// using machine/testutil.
func newNamespace(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	for _, name := range []string{
		"if", "lambda", "quote", "quasiquote", "define",
		"set!", "begin", "meta", "include", "include-ci",
	} {
		env.MaybeCreateOwnGlobalBinding(
			values.NewSymbol(name),
			environment.BindingTypePrimitive,
			nil,
		)
	}
	return env
}

func TestNewForeignClosure(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	fn := func(_ CallContext) error {
		return nil
	}

	closure := NewForeignClosure(env, 2, false, fn)

	qt.Assert(t, closure, qt.IsNotNil)
	qt.Assert(t, closure.ParameterCount(), qt.Equals, 2)
	qt.Assert(t, closure.IsVariadic(), qt.IsFalse)
	qt.Assert(t, closure.Fn(), qt.IsNotNil)
	qt.Assert(t, closure.Env(), qt.IsNotNil)

	// Verify it satisfies the Closure interface
	var _ Closure = closure
}

func TestNewForeignClosure_Variadic(t *testing.T) {
	env := environment.NewNamespace().Runtime()

	fn := func(_ CallContext) error {
		return nil
	}

	closure := NewForeignClosure(env, 1, true, fn)

	qt.Assert(t, closure, qt.IsNotNil)
	qt.Assert(t, closure.ParameterCount(), qt.Equals, 1)
	qt.Assert(t, closure.IsVariadic(), qt.IsTrue)
}
