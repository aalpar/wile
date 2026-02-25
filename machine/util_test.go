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
	"bufio"
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/internal/syntax"

	qt "github.com/frankban/quicktest"
)

// parseSchemeExpr is a test helper to parse Scheme code into syntax.
func parseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)
	sv, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)
	return sv
}

func TestNewForeignClosure(t *testing.T) {
	env := environment.NewTopLevelEnvironment().Runtime()

	fn := func(mc *MachineContext) error {
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
	env := environment.NewTopLevelEnvironment().Runtime()

	fn := func(mc *MachineContext) error {
		return nil
	}

	closure := NewForeignClosure(env, 1, true, fn)

	qt.Assert(t, closure, qt.IsNotNil)
	qt.Assert(t, closure.ParameterCount(), qt.Equals, 1)
	qt.Assert(t, closure.IsVariadic(), qt.IsTrue)
}
