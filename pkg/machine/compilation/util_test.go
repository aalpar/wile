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

package compilation

import (
	"bufio"
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"

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

// newTopLevelThunk creates a top-level thunk from a parsed expression.
func newTopLevelThunk(prog syntax.SyntaxValue, env *environment.EnvironmentFrame) (*machine.MachineContinuation, error) {
	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ectx := context.Background()
	econt := NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator())
	prog, err := econt.ExpandExpression(prog)
	if err != nil {
		return nil, err
	}
	cnt := NewCompileTimeCallContext(context.Background(), false)
	err = cctx.CompileExpression(cnt, prog)
	if err != nil {
		return nil, err
	}
	return machine.NewMachineContinuation(nil, tpl, env), nil
}

// newNamespace sets up a minimal namespace with core special form bindings.
func newNamespace(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	for _, name := range []string{"if", "lambda", "quote", "quasiquote", "define", "set!", "begin", "meta", "include", "include-ci"} {
		env.MaybeCreateOwnGlobalBinding(values.NewSymbol(name), environment.BindingTypePrimitive)
	}
	err := RegisterAllPhaseHandlers(env)
	if err != nil {
		panic("newNamespace: " + err.Error())
	}
	return env
}
