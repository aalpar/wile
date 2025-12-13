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
	"bufio"
	"context"
	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// parseSchemeExpr is a test helper to parse Scheme code into syntax.
func parseSchemeExpr(t *testing.T, env *environment.EnvironmentFrame, code string) syntax.SyntaxValue {
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, reader)
	sv, err := p.ReadSyntax(nil)
	qt.Assert(t, err, qt.IsNil)
	return sv
}

func TestNewForeignClosure(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	fn := func(ctx context.Context, mc *MachineContext) error {
		return nil
	}

	closure := NewForeignClosure(env, 2, false, fn)

	qt.Assert(t, closure, qt.IsNotNil)
	qt.Assert(t, closure.Template().ParameterCount(), qt.Equals, 2)
	qt.Assert(t, closure.Template().IsVariadic(), qt.IsFalse)

	// Verify it has the right operations
	ops := closure.Template().Operations()
	qt.Assert(t, len(ops), qt.Equals, 2)
	_, ok1 := ops[0].(*OperationForeignFunctionCall)
	qt.Assert(t, ok1, qt.IsTrue)
	_, ok2 := ops[1].(*OperationRestoreContinuation)
	qt.Assert(t, ok2, qt.IsTrue)
}

func TestNewForeignClosure_Variadic(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()

	fn := func(ctx context.Context, mc *MachineContext) error {
		return nil
	}

	closure := NewForeignClosure(env, 1, true, fn)

	qt.Assert(t, closure, qt.IsNotNil)
	qt.Assert(t, closure.Template().ParameterCount(), qt.Equals, 1)
	qt.Assert(t, closure.Template().IsVariadic(), qt.IsTrue)
}
