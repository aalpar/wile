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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func TestERMacroTransformer_IsValue(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert := machine.NewERMacroTransformer(cls, env)

	// Satisfies values.Value
	var v values.Value = ert
	c.Assert(v, qt.Not(qt.IsNil))
	c.Assert(ert.SchemeString(), qt.Equals, "#<er-macro-transformer>")
	c.Assert(ert.IsVoid(), qt.IsFalse)

	// Accessors
	c.Assert(ert.Closure(), qt.Equals, cls)
	c.Assert(ert.DefEnv(), qt.Equals, env)
}

func TestERMacroTransformer_EqualTo(t *testing.T) {
	c := qt.New(t)

	env := environment.NewTopLevelEnvironment().Runtime()
	tpl := machine.NewNativeTemplate(3, 0, false)
	cls := machine.NewClosureWithTemplate(tpl, env)

	ert1 := machine.NewERMacroTransformer(cls, env)
	ert2 := machine.NewERMacroTransformer(cls, env)

	// Identity semantics
	c.Assert(ert1.EqualTo(ert1), qt.IsTrue)
	c.Assert(ert1.EqualTo(ert2), qt.IsFalse)
	c.Assert(ert1.EqualTo(values.TrueValue), qt.IsFalse)
}

func TestCompileERMacroTransformer(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Parse a define-syntax using er-macro-transformer with a simple identity lambda.
	// The lambda body is just 'form' (the first parameter) since cadr is not available
	// in the minimal test environment.
	form := parseString(t, env, `
		(define-syntax my-id
		  (er-macro-transformer
		    (lambda (form rename compare) form)))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false)
	args := extractDefineSyntaxArgs(t, form)
	err := ctc.CompileDefineSyntax(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Verify the binding is an ERMacroTransformer
	expandEnv := env.Expand()
	bnd := expandEnv.GetBinding(values.NewSymbol("my-id"))
	c.Assert(bnd, qt.Not(qt.IsNil))

	_, ok := bnd.Value().(*machine.ERMacroTransformer)
	c.Assert(ok, qt.IsTrue)
}

func TestERMacro_EndToEnd_Constant(t *testing.T) {
	c := qt.New(t)

	env := createHygieneTestEnv()

	// Define: (define-syntax my-const (er-macro-transformer (lambda (form rename compare) 42)))
	// The transformer ignores the input form and returns a constant.
	form := parseString(t, env, `
		(define-syntax my-const
		  (er-macro-transformer
		    (lambda (form rename compare) 42)))
	`)

	ctc := machine.NewCompiletimeContinuation(machine.NewNativeTemplate(0, 0, false), env)
	ctctx := machine.NewCompileTimeCallContext(context.Background(), false)
	args := extractDefineSyntaxArgs(t, form)
	err := ctc.CompileDefineSyntax(ctctx, args)
	c.Assert(err, qt.IsNil)

	// Expand: (my-const anything) — transformer returns 42 regardless
	testForm := parseString(t, env, `(my-const anything)`)
	etc := machine.NewExpanderTimeContinuation(context.Background(), env)
	expanded, err := etc.ExpandExpression(testForm)
	c.Assert(err, qt.IsNil)

	// The expanded result should be 42, wrapped in syntax notation
	t.Logf("Expanded: %s", expanded.SchemeString())
	unwrapped := expanded.UnwrapAll()
	c.Assert(unwrapped.SchemeString(), qt.Equals, "42")
}
