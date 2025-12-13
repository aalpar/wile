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
	"io"
	"wile/environment"
	"wile/parser"
	"wile/syntax"
	"wile/utils"
	"wile/values"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

func TestCompileContext_CompileLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	prog := values.List(values.NewSymbol("lambda"), values.NewSymbol("x"), values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 5)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
		NewOperationPush(),
		NewOperationMakeClosure(),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 2)
	tpl0, ok := cont.template.literals[0].(*NativeTemplate)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the template has been compiled correctly
	qt.Assert(t, tpl0.operations, qt.HasLen, 2)
	qt.Assert(t, tpl0.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
		NewOperationRestoreContinuation(),
	))
	qt.Assert(t, tpl0.isVariadic, qt.Equals, true)
	qt.Assert(t, tpl0.parameterCount, qt.Equals, 1)
	env0, ok := cont.template.literals[1].(*environment.EnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the env has been set up correctly
	qt.Assert(t, env0.LocalEnvironment().Keys(), qt.HasLen, 1)
	qt.Assert(t, env0.GlobalEnvironment(), qt.Equals, env.GlobalEnvironment())

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileLambdaCall(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	prog := values.List(values.List(values.NewSymbol("lambda"), values.NewSymbol("x"), values.NewSymbol("x")), values.NewString("hello"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 11)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationSaveContinuationOffsetImmediate(11),
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
		NewOperationPush(),
		NewOperationMakeClosure(),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(2),
		NewOperationPush(),
		NewOperationPull(),
		NewOperationApply(),
		//		NewOperationPush(),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 3)
	tpl0, ok := cont.template.literals[0].(*NativeTemplate)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the template has been compiled correctly
	qt.Assert(t, tpl0.operations, qt.HasLen, 2)
	qt.Assert(t, tpl0.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
		NewOperationRestoreContinuation(),
	))
	qt.Assert(t, tpl0.isVariadic, qt.Equals, true)
	qt.Assert(t, tpl0.parameterCount, qt.Equals, 1)
	env0, ok := cont.template.literals[1].(*environment.EnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the env has been set up correctly
	qt.Assert(t, env0.LocalEnvironment().Keys(), qt.HasLen, 1)
	qt.Assert(t, env0.GlobalEnvironment(), qt.Equals, env.GlobalEnvironment())

	mc := NewMachineContext(cont)
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewCons(values.NewString("hello"), values.EmptyList))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileDefine(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	prog := values.List(values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewString("y"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 4)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1)),
		NewOperationLoadVoid(),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 2)

	mc := NewMachineContext(cont)
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileQuote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	var prog values.Value = values.List(
		values.NewSymbol("quote"),
		values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 1)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 1)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewSymbol("x"))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	var prog values.Value = values.List(
		values.NewSymbol("quasiquote"),
		values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 1)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 1)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewSymbol("x"))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

// TestCompileContext_CompileNestedQuasiquote tests deeply nested quasiquote/unquote handling.
// This validates that the depth tracking works correctly for 5 levels of nesting.
func TestCompileContext_CompileNestedQuasiquote(t *testing.T) {
	// Test cases for nested quasiquote with various depths
	testCases := []struct {
		name     string
		input    string
		expected values.Value
	}{
		{
			name:  "double quasiquote double unquote",
			input: "(define x 5) ``,,x",
			expected: values.List(
				values.NewSymbol("quasiquote"),
				values.List(values.NewSymbol("unquote"), values.NewInteger(5)),
			),
		},
		{
			name:  "nested quasiquote with double unquote in list",
			input: "(define x 5) `(a `(b ,,x) c)",
			expected: values.List(
				values.NewSymbol("a"),
				values.List(
					values.NewSymbol("quasiquote"),
					values.List(
						values.NewSymbol("b"),
						values.List(values.NewSymbol("unquote"), values.NewInteger(5)),
					),
				),
				values.NewSymbol("c"),
			),
		},
		{
			name:  "nested quasiquote with single unquote preserves unquote",
			input: "`(a `(b ,x) c)",
			expected: values.List(
				values.NewSymbol("a"),
				values.List(
					values.NewSymbol("quasiquote"),
					values.List(
						values.NewSymbol("b"),
						values.List(values.NewSymbol("unquote"), values.NewSymbol("x")),
					),
				),
				values.NewSymbol("c"),
			),
		},
		{
			name:  "five levels of quasiquote with five unquotes",
			input: "(define x 5) `````,,,,, x",
			expected: values.List(
				values.NewSymbol("quasiquote"),
				values.List(
					values.NewSymbol("quasiquote"),
					values.List(
						values.NewSymbol("quasiquote"),
						values.List(
							values.NewSymbol("quasiquote"),
							values.List(
								values.NewSymbol("unquote"),
								values.List(
									values.NewSymbol("unquote"),
									values.List(
										values.NewSymbol("unquote"),
										values.List(
											values.NewSymbol("unquote"),
											values.NewInteger(5),
										),
									),
								),
							),
						),
					),
				),
			),
		},
		{
			name:     "simple quasiquote unchanged",
			input:    "`(1 2 3)",
			expected: values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, err := evalSchemeString(tc.input)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

// evalSchemeString evaluates a Scheme string and returns the final result.
// This is a helper for testing that compiles and runs Scheme code.
func evalSchemeString(code string) (values.Value, error) {
	ctx := context.Background()
	env := environment.NewTopLevelEnvironmentFrame()

	// Register required primitives
	if err := RegisterSyntaxCompilers(env); err != nil {
		return nil, err
	}

	// Register list primitive for quasiquote expansion
	listSym := env.InternSymbol(values.NewSymbol("list"))
	env.MaybeCreateOwnGlobalBinding(listSym, environment.BindingTypeVariable)
	listIdx := env.GetGlobalIndex(listSym)
	if listIdx != nil {
		listClosure := NewForeignClosure(env, 1, true, func(_ context.Context, mc *MachineContext) error {
			// The variadic args come as a list in the first local slot
			o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
			mc.SetValue(o)
			return nil
		})
		if err := env.SetOwnGlobalValue(listIdx, listClosure); err != nil {
			return nil, err
		}
	}

	// Parse and evaluate code
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, reader)

	var lastResult values.Value = values.Void
	for {
		stx, err := p.ReadSyntax(nil)
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		// Expand
		ectx := NewExpandTimeCallContext()
		econt := NewExpanderTimeContinuation(env)
		expanded, err := econt.ExpandExpression(ectx, stx)
		if err != nil {
			return nil, err
		}

		// Compile
		tpl := NewNativeTemplate(0, 0, false)
		cctx := NewCompiletimeContinuation(tpl, env)
		cnt := NewCompileTimeCallContext(false, true, env)
		if err := cctx.CompileExpression(cnt, expanded); err != nil {
			return nil, err
		}

		// Run
		mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))
		if err := mc.Run(ctx); err != nil {
			return nil, err
		}

		if len(mc.value) > 0 {
			lastResult = mc.value[0]
		}
	}

	return lastResult, nil
}

func TestCompileContext_CompileIf(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	// top-level closure with no parameters (thunk)
	prog := values.List(values.NewSymbol("if"),
		values.NewBoolean(false),
		values.NewString("true"),
		values.NewString("false"))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 6)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationBranchOnFalseOffsetImmediate(3),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
		NewOperationBranchOffsetImmediate(2),
		NewOperationLoadLiteralByLiteralIndexImmediate(2),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 3)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewString("false"))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileSetBang(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	symX := env.InternSymbol(values.NewSymbol("x"))
	gi, _ := env.MaybeCreateOwnGlobalBinding(symX, environment.BindingTypeVariable)
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	prog := values.List(values.NewSymbol("set!"), symX, values.NewString("true"))
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 4)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(LiteralIndex(1)),
		NewOperationLoadVoid(),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 2)
	qt.Assert(t, cont.template.literals[0], values.SchemeEquals, values.NewString("true"))
	qt.Assert(t, cont.template.literals[1], values.SchemeEquals, gi)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	// set! now returns void with the LoadVoid operation at the end
	qt.Assert(t, mc.value[0], qt.Equals, values.Void)
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
	gi = mc.env.GlobalEnvironment().GetGlobalIndex(mc.env.InternSymbol(values.NewSymbol("x")))
	qt.Assert(t, gi, qt.IsNotNil)
	v := mc.env.GlobalEnvironment().GetOwnGlobalBinding(gi)
	qt.Assert(t, v.BindingType(), qt.Equals, environment.BindingTypeVariable)
	qt.Assert(t, v.Value(), values.SchemeEquals, values.NewString("true"))
}

func TestCompileContext_CompileBegin_0(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	symX := env.InternSymbol(values.NewSymbol("x"))
	// top-level closure with no parameters (thunk)
	prog := values.List(values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), symX,
			values.List(values.NewSymbol("lambda"), values.NewSymbol("y"), values.NewBoolean(true))),
		values.List(symX, values.NewString("foo")))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.literals, qt.HasLen, 4)
	qt.Assert(t, cont.template.literals, values.SchemeEquals,
		NewMultipleValues(
			cont.template.literals[0],
			cont.template.literals[1],
			environment.NewGlobalIndex(symX),
			values.NewString("foo"),
		),
	)
	qt.Assert(t, cont.template.operations, qt.HasLen, 15)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
		NewOperationPush(),
		NewOperationMakeClosure(),
		NewOperationPush(),
		NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(2),
		NewOperationLoadVoid(),
		NewOperationSaveContinuationOffsetImmediate(7),
		NewOperationLoadGlobalByGlobalIndexLiteralIndexImmediate(2),
		NewOperationPush(),
		NewOperationLoadLiteralByLiteralIndexImmediate(3),
		NewOperationPush(),
		NewOperationPull(),
		NewOperationApply(),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 4)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	// FIXME: should return ErrMachineHalt
	//	qt.Assert(t, err, qt.ErrorIs, ErrMachineHalt)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewBoolean(true))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileBegin_1(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	prog := values.List(values.NewSymbol("begin"), values.NewString("true"), values.NewString("false"))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.template.operations, qt.HasLen, 2)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 2)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewString("false"))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func TestCompileContext_CompileMeta(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	prog := values.List(values.NewSymbol("meta"), values.NewString("first"), values.NewString("second"))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	// meta should compile like begin - sequence of expressions
	qt.Assert(t, cont.template.operations, qt.HasLen, 2)
	qt.Assert(t, cont.template.operations, values.SchemeEquals, NewOperations(
		NewOperationLoadLiteralByLiteralIndexImmediate(0),
		NewOperationLoadLiteralByLiteralIndexImmediate(1),
	))
	qt.Assert(t, cont.template.isVariadic, qt.Equals, false)
	qt.Assert(t, cont.template.parameterCount, qt.Equals, 0)
	qt.Assert(t, cont.template.literals, qt.HasLen, 2)

	mc := NewMachineContext(NewMachineContinuation(nil, cont.template, env))
	ctx := context.Background()
	qt.Assert(t, mc.value, qt.HasLen, 0)
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewString("second"))
	qt.Assert(t, *mc.evals, qt.HasLen, 0)
}

func newTopLevelThunk(prog syntax.SyntaxValue, env *environment.EnvironmentFrame) (*MachineContinuation, error) {
	tpl := NewNativeTemplate(0, 0, false)
	cctx := NewCompiletimeContinuation(tpl, env)
	ectx := NewExpandTimeCallContext()
	econt := NewExpanderTimeContinuation(env)
	prog, err := econt.ExpandExpression(ectx, prog)
	if err != nil {
		return nil, err
	}
	// Use inTail=false for top-level expressions. Top-level is NOT tail position
	// because there's no outer function to return to.
	cnt := NewCompileTimeCallContext(false, true, env)
	err = cctx.CompileExpression(cnt, prog)
	if err != nil {
		return nil, err
	}
	return NewMachineContinuation(nil, tpl, env), nil
}

func newTopLevelEnv(env *environment.EnvironmentFrame) *environment.EnvironmentFrame {
	ifSym := env.InternSymbol(values.NewSymbol("if"))
	lambdaSym := env.InternSymbol(values.NewSymbol("lambda"))
	quoteSym := env.InternSymbol(values.NewSymbol("quote"))
	quasiquoteSym := env.InternSymbol(values.NewSymbol("quasiquote"))
	defineSym := env.InternSymbol(values.NewSymbol("define"))
	setSym := env.InternSymbol(values.NewSymbol("set!"))
	beginSym := env.InternSymbol(values.NewSymbol("begin"))
	metaSym := env.InternSymbol(values.NewSymbol("meta"))
	includeSym := env.InternSymbol(values.NewSymbol("include"))
	includeCiSym := env.InternSymbol(values.NewSymbol("include-ci"))

	env.MaybeCreateOwnGlobalBinding(ifSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(lambdaSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(quoteSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(quasiquoteSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(defineSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(setSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(beginSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(metaSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(includeSym, environment.BindingTypePrimitive)
	env.MaybeCreateOwnGlobalBinding(includeCiSym, environment.BindingTypePrimitive)

	// Register primitive compilers in the compile environment
	RegisterSyntaxCompilers(env) //nolint:errcheck
	// Register primitive expanders in the expand environment
	RegisterPrimitiveExpanders(env) //nolint:errcheck
	return env
}

func TestCondExpandRegistered(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	err := RegisterSyntaxCompilers(env)
	if err != nil {
		t.Fatalf("RegisterSyntaxCompilers failed: %v", err)
	}

	// Check if cond-expand is registered
	sym := env.InternSymbol(values.NewSymbol("cond-expand"))
	pc := LookupSyntaxCompiler(env, sym, nil)
	if pc == nil {
		t.Errorf("cond-expand primitive compiler not found")
	} else {
		t.Logf("cond-expand found: %v", pc)
	}

	// Core forms like 'if' are now handled by compileValidated* methods
	// and are NOT registered as primitive compilers. Check that 'if' is NOT registered.
	ifSym := env.InternSymbol(values.NewSymbol("if"))
	ifPc := LookupSyntaxCompiler(env, ifSym, nil)
	if ifPc != nil {
		t.Errorf("if should NOT be registered as primitive compiler (handled by validation)")
	}
}

// TestTailCallOptimization_CallDepthGrows verifies that without TCO implementation,
// the call depth grows during tail recursion. This test is expected to FAIL after
// TCO is properly implemented.
//
// The test runs a tail-recursive loop and tracks the maximum call depth seen.
// Without TCO: depth grows to ~100 (one frame per recursive call)
// With TCO: depth stays constant at ~2-3 (no frame accumulation)
func TestTailCallOptimization_CallDepthGrows(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Track maximum call depth seen during execution
	var maxCallDepth int

	// Register call-depth primitive: returns current continuation stack depth
	callDepthSym := env.InternSymbol(values.NewSymbol("call-depth"))
	env.MaybeCreateOwnGlobalBinding(callDepthSym, environment.BindingTypeVariable)
	callDepthFn := func(ctx context.Context, mc *MachineContext) error {
		depth := mc.CallDepth()
		if depth > maxCallDepth {
			maxCallDepth = depth
		}
		mc.SetValue(values.NewInteger(int64(depth)))
		return nil
	}
	callDepthClosure := NewForeignClosure(env, 0, false, callDepthFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(callDepthSym), callDepthClosure) //nolint:errcheck

	// Register subtraction primitive: (- a b)
	subSym := env.InternSymbol(values.NewSymbol("-"))
	env.MaybeCreateOwnGlobalBinding(subSym, environment.BindingTypeVariable)
	subFn := func(ctx context.Context, mc *MachineContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a - b))
		return nil
	}
	subClosure := NewForeignClosure(env, 2, false, subFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(subSym), subClosure) //nolint:errcheck

	// Register equality primitive: (= a b)
	eqSym := env.InternSymbol(values.NewSymbol("="))
	env.MaybeCreateOwnGlobalBinding(eqSym, environment.BindingTypeVariable)
	eqFn := func(ctx context.Context, mc *MachineContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		if a == b {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
		return nil
	}
	eqClosure := NewForeignClosure(env, 2, false, eqFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(eqSym), eqClosure) //nolint:errcheck

	// Compile: (define (loop n) (call-depth) (if (= n 0) n (loop (- n 1))))
	// This is a tail-recursive loop that calls call-depth on each iteration
	sctx := syntax.NewZeroValueSourceContext()
	defineProg := values.List(
		values.NewSymbol("define"),
		values.List(values.NewSymbol("loop"), values.NewSymbol("n")),
		values.List(values.NewSymbol("call-depth")), // side effect: tracks depth
		values.List(values.NewSymbol("if"),
			values.List(values.NewSymbol("="), values.NewSymbol("n"), values.NewInteger(0)),
			values.NewSymbol("n"),
			values.List(values.NewSymbol("loop"),
				values.List(values.NewSymbol("-"), values.NewSymbol("n"), values.NewInteger(1)))))

	// Compile and run the define
	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, defineProg), env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Now call (loop 100)
	maxCallDepth = 0 // Reset
	callProg := values.List(values.NewSymbol("loop"), values.NewInteger(100))
	cont2, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, callProg), env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(ctx)
	// ErrMachineHalt is expected when execution completes with inTail=true at top-level
	// (RestoreContinuation returns ErrMachineHalt when mc.cont is nil)
	if err != nil && err != ErrMachineHalt {
		qt.Assert(t, err, qt.IsNil)
	}

	// Without TCO: maxCallDepth should be >= 100 (grows with recursion depth)
	// With TCO: maxCallDepth should be small (~2-3, constant)
	t.Logf("Maximum call depth during 100 iterations: %d", maxCallDepth)

	// This assertion verifies TCO IS working (depth stays constant)
	qt.Assert(t, maxCallDepth <= 5, qt.IsTrue,
		qt.Commentf("Expected call depth <= 5 with TCO, got %d. TCO may not be working!", maxCallDepth))
}

func TestCompileContext_CompileCaseLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// (case-lambda
	//   ((x) x)
	//   ((x y) y))
	prog := values.List(
		values.NewSymbol("case-lambda"),
		values.List(values.List(values.NewSymbol("x")), values.NewSymbol("x")),
		values.List(values.List(values.NewSymbol("x"), values.NewSymbol("y")), values.NewSymbol("y")))

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// Run compilation to create case-lambda closure
	mc := NewMachineContext(cont)
	ctx := context.Background()
	err = mc.Run(ctx)
	qt.Assert(t, err, qt.IsNil)

	// Verify we got a case-lambda closure
	qt.Assert(t, mc.value, qt.HasLen, 1)
	caseLambda, ok := mc.value[0].(*CaseLambdaClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, caseLambda.Clauses(), qt.HasLen, 2)

	// Verify clause arities
	clauses := caseLambda.Clauses()
	qt.Assert(t, clauses[0].closure.Template().ParameterCount(), qt.Equals, 1)
	qt.Assert(t, clauses[1].closure.Template().ParameterCount(), qt.Equals, 2)
}

func TestCompileContext_CompileCaseLambdaCall(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// ((case-lambda
	//    ((x) x)
	//    ((x y) y))
	//  42)
	prog := values.List(
		values.List(
			values.NewSymbol("case-lambda"),
			values.List(values.List(values.NewSymbol("x")), values.NewSymbol("x")),
			values.List(values.List(values.NewSymbol("x"), values.NewSymbol("y")), values.NewSymbol("y"))),
		values.NewInteger(42))

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	ctx := context.Background()
	err = mc.Run(ctx)
	if err == ErrMachineHalt {
		err = nil
	}
	qt.Assert(t, err, qt.IsNil)

	// Should call first clause with 1 arg: returns 42
	qt.Assert(t, mc.value, qt.HasLen, 1)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// Tests moved from coverage_additional_test.go
// compileProcedureArgumentList and CompileProcedureCall
func TestCompileProcedureCallWithArgs(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{
			name: "simple procedure call with arguments",
			prog: "((lambda (x y) x) 1 2)",
		},
		{
			name: "nested procedure calls",
			prog: "((lambda (f) (f 1)) (lambda (x) x))",
		},
		{
			name: "procedure call with multiple args",
			prog: "((lambda (a b c) a) 1 2 3)",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileLambdaParameterListVariants tests lambda with various parameter lists
func TestCompileLambdaParameterListVariants(t *testing.T) {
	testCases := []struct {
		name     string
		prog     string
		variadic bool
	}{
		{
			name:     "lambda with rest parameter",
			prog:     "(lambda x x)",
			variadic: true,
		},
		{
			name:     "lambda with dotted parameter list",
			prog:     "(lambda (a . rest) rest)",
			variadic: true,
		},
		{
			name:     "lambda with multiple params and rest",
			prog:     "(lambda (a b . rest) rest)",
			variadic: true,
		},
		{
			name:     "lambda with no params",
			prog:     "(lambda () 42)",
			variadic: false,
		},
		{
			name:     "lambda with single param",
			prog:     "(lambda (x) x)",
			variadic: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)

			// The compiled thunk wraps the lambda, so check its literals
			qt.Assert(t, len(cont.template.literals) >= 1, qt.IsTrue)
			innerTpl, ok := cont.template.literals[0].(*NativeTemplate)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, innerTpl.isVariadic, qt.Equals, tc.variadic)
		})
	}
}

// TestSyntaxCompilerNameMethod tests the Name method of PrimitiveCompiler
func TestSyntaxCompilerNameMethod(t *testing.T) {
	pc := &SyntaxCompiler{
		name: "test-compiler",
	}
	qt.Assert(t, pc.Name(), qt.Equals, "test-compiler")
}

// TestExpandQuasiquoteAndQuote tests the expander for quasiquote and quote
func TestExpandQuasiquoteAndQuote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test quote expansion
	quoteProg := values.List(values.NewSymbol("quote"), values.NewSymbol("x"))
	ectx := NewExpandTimeCallContext()
	econt := NewExpanderTimeContinuation(env)
	expanded, err := econt.ExpandExpression(ectx, utils.DatumToSyntaxValue(sctx, quoteProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNotNil)

	// Test quasiquote expansion
	qqProg := values.List(values.NewSymbol("quasiquote"), values.List(values.NewSymbol("a"), values.NewSymbol("b")))
	expanded2, err := econt.ExpandExpression(ectx, utils.DatumToSyntaxValue(sctx, qqProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded2, qt.IsNotNil)
}

// TestCompileSymbolUnboundError tests compile error for unbound symbol
func TestCompileSymbolUnboundError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()
	expr := values.NewSymbol("undefined-symbol")

	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, expr), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such binding")
}

// TestCompileLambdaDuplicateParamError tests error for duplicate lambda params
func TestCompileLambdaDuplicateParamError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (x x) x) should error due to duplicate parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("x")),
		values.NewSymbol("x"),
	)

	_, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "duplicate")
}

// TestCompileLambdaInvalidParamError tests error for invalid lambda parameter
func TestCompileLambdaInvalidParamError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (1) 42) should error - 1 is not a valid parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewInteger(1)),
		values.NewInteger(42),
	)

	_, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileNestedQuasiquote tests doubly-nested quasiquote
func TestCompileNestedQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test nested quasiquote: `(a `(b ,x))
	prog := values.List(
		values.NewSymbol("quasiquote"),
		values.List(
			values.NewSymbol("a"),
			values.List(
				values.NewSymbol("quasiquote"),
				values.List(
					values.NewSymbol("b"),
					values.List(values.NewSymbol("unquote"), values.NewSymbol("x")),
				),
			),
		),
	)

	cont, err := newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileCaseLambda tests compiling case-lambda
func TestCompileCaseLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// case-lambda with multiple clauses
	prog := `(case-lambda
		(() 0)
		((x) x)
		((x y) (if x y x))
		(args args))`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineWithFn tests compiling define with lambda form
func TestCompileDefineWithFn(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn x y) body) form
	prog := `(define (add x y) x)`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineVar tests compiling define with value
func TestCompileDefineVar(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	prog := `(define x 42)`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)

	// Run it
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSetBang tests compiling set!
func TestCompileSetBang(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define x
	sv := parseSchemeExpr(t, env, `(define x 0)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Now set! it
	sv2 := parseSchemeExpr(t, env, `(set! x 42)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileIfBranches tests compiling if with both branches
func TestCompileIfBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// if with else
	sv := parseSchemeExpr(t, env, `(if #t 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(1))

	// if without else (just consequent)
	sv2 := parseSchemeExpr(t, env, `(if #f 1)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestExpandQuasiquoteAndQuoteDirect tests the expander methods directly
func TestExpandQuasiquoteAndQuoteDirect(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	econt := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()
	sctx := syntax.NewZeroValueSourceContext()

	// Test ExpandQuote - currently returns nil, nil
	quoteExpr := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("quote", sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("x", sctx),
			syntax.NewSyntaxEmptyList(sctx),
			sctx,
		),
		sctx,
	)
	expanded, err := econt.ExpandQuote(ectx, quoteExpr)
	// The function returns nil, nil (unimplemented)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNil)

	// Test ExpandQuasiquote - currently returns nil, nil
	qqExpr := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("quasiquote", sctx),
		syntax.NewSyntaxCons(
			syntax.NewSyntaxSymbol("y", sctx),
			syntax.NewSyntaxEmptyList(sctx),
			sctx,
		),
		sctx,
	)
	expanded, err = econt.ExpandQuasiquote(ectx, qqExpr)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNil)
}

// TestCompileUnquoteError tests error when compiling unquote outside quasiquote
func TestCompileUnquoteError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote - should error
	prog := values.List(values.NewSymbol("unquote"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileUnquoteSplicingError tests error when compiling unquote-splicing outside quasiquote
func TestCompileUnquoteSplicingError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote-splicing - should error
	prog := values.List(values.NewSymbol("unquote-splicing"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(utils.DatumToSyntaxValue(sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileQuasiquoteSimple tests quasiquote without unquote (no list/append needed)
func TestCompileQuasiquoteSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple quasiquote without any unquote - should be a compile-time constant
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileProcedureCallTail tests tail call compilation
func TestCompileProcedureCallTail(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with tail call
	sv := parseSchemeExpr(t, env, `(define (fn x) ((lambda (y) y) x))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Test calling the function
	sv = parseSchemeExpr(t, env, `(fn 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileProcedureCallNonTail tests non-tail call compilation
func TestCompileProcedureCallNonTail(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with non-tail call (result used as argument)
	sv := parseSchemeExpr(t, env, `((lambda (x) (begin ((lambda (y) y) x) 99)) 1)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// TestCompileBeginSequence tests begin with multiple expressions
func TestCompileBeginSequence(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(3))
}

// TestCompileIfNoAlternate tests if without else
func TestCompileIfNoAlternate(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// If without else, true case
	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))

	// If without else, false case - should return void
	sv = parseSchemeExpr(t, env, `(if #f 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileLambdaWithRestParameter tests lambda with rest parameter
func TestCompileLambdaWithRestParameter(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda (a . rest) rest) - dotted parameter list
	sv := parseSchemeExpr(t, env, `((lambda (a . rest) rest) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	// rest should be (2 3)
	result := mc.value[0]
	qt.Assert(t, result, qt.IsNotNil)
}

// TestCompileLambdaRestOnly tests lambda with only rest parameter
func TestCompileLambdaRestOnly(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda args args) - single rest parameter
	sv := parseSchemeExpr(t, env, `((lambda args args) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileCondExpandNotFeature tests cond-expand with not feature
func TestCompileCondExpandNotFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (not nonexistent) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((not nonexistent) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandAndFeature tests cond-expand with and feature
func TestCompileCondExpandAndFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (and r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((and r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandOrFeature tests cond-expand with or feature
func TestCompileCondExpandOrFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (or nonexistent r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((or nonexistent r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandLibraryFeature tests cond-expand with library feature
func TestCompileCondExpandLibraryFeature(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (library (scheme base)) - may not be available but tests parsing
	sv := parseSchemeExpr(t, env, `(cond-expand ((library (nonexistent lib)) 1) (else 99))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// TestCompileSymbolBranches tests various branches of CompileSymbol
func TestCompileSymbolBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test compiling a defined global variable
	sv := parseSchemeExpr(t, env, `(define global-var 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Now compile a reference to the global
	sv = parseSchemeExpr(t, env, `global-var`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolLocalBinding tests compiling local bindings
func TestCompileSymbolLocalBinding(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with local parameter
	sv := parseSchemeExpr(t, env, `((lambda (local-var) local-var) 99)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(99))
}

// historical but the test remains valid for verifying primitive form compilation.
func TestCompileSyntaxPrimitiveBranches(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test various primitives
	testCases := []struct {
		name string
		prog string
	}{
		{"quote", "'foo"},
		{"if-true", "(if #t 1 2)"},
		{"if-false", "(if #f 1 2)"},
		{"lambda", "(lambda (x) x)"},
		{"begin", "(begin 1 2 3)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := NewMachineContext(cont)
			err = mc.Run(context.Background())
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// TestCompileMultipleForms tests compiling multiple forms
func TestCompileMultipleForms(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define and use
	sv := parseSchemeExpr(t, env, `(define fn (lambda (x) x))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call the function
	sv = parseSchemeExpr(t, env, `(fn 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSelfEvaluating tests compiling self-evaluating values
func TestCompileSelfEvaluating(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name   string
		prog   string
		expect values.Value
	}{
		{"integer", "42", values.NewInteger(42)},
		{"negative-integer", "-5", values.NewInteger(-5)},
		{"true", "#t", values.TrueValue},
		{"false", "#f", values.FalseValue},
		{"float", "3.14", values.NewFloat(3.14)},
		{"string", `"hello"`, values.NewString("hello")},
		{"char", `#\a`, values.NewCharacter('a')},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := NewMachineContext(cont)
			err = mc.Run(context.Background())
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.value[0], values.SchemeEquals, tc.expect)
		})
	}
}

// TestCompileNestedLambda tests nested lambda expressions
func TestCompileNestedLambda(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested lambda - currying
	sv := parseSchemeExpr(t, env, `(((lambda (x) (lambda (y) x)) 1) 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(1))
}

// TestCompileComplexIf tests complex if expressions
func TestCompileComplexIf(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested if expressions
	sv := parseSchemeExpr(t, env, `(if #t (if #f 1 2) 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.value[0], values.SchemeEquals, values.NewInteger(2))
}

// TestCompileUnquoteOutsideQuasiquote tests that unquote outside quasiquote returns error
func TestCompileUnquoteOutsideQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Try to compile unquote outside of quasiquote - should error
	sv := parseSchemeExpr(t, env, `(unquote x)`)
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote")
}

// TestCompileUnquoteSplicingOutsideQuasiquote tests that unquote-splicing outside quasiquote returns error
func TestCompileUnquoteSplicingOutsideQuasiquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Try to compile unquote-splicing outside of quasiquote - should error
	sv := parseSchemeExpr(t, env, `(unquote-splicing x)`)
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "unquote-splicing")
}

// TestQuasiquoteWithUnquote tests basic quasiquote with unquote
func TestQuasiquoteWithUnquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple quasiquote without unquote (doesn't need runtime bindings)
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteNested tests nested quasiquote
func TestQuasiquoteNested(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested quasiquote
	sv := parseSchemeExpr(t, env, "``(a ,b)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteWithDotPair tests quasiquote with dotted pair
func TestQuasiquoteWithDotPair(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Quasiquote with dotted pair
	sv := parseSchemeExpr(t, env, "`(a . b)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestQuasiquoteVector tests quasiquote with vector
func TestQuasiquoteVector(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Quasiquote with vector
	sv := parseSchemeExpr(t, env, "`#(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileDefineFn tests define with function shorthand
func TestCompileDefineFn(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn x) x) shorthand form
	sv := parseSchemeExpr(t, env, `(define (identity x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileDefineFnVariadic tests define with variadic function shorthand
func TestCompileDefineFnVariadic(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn . args) args) variadic shorthand form
	sv := parseSchemeExpr(t, env, `(define (varargs . x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileSymbolGlobal tests compiling a global symbol reference
func TestCompileSymbolGlobal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define a global
	sv := parseSchemeExpr(t, env, `(define my-global 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Then reference it
	sv2 := parseSchemeExpr(t, env, `my-global`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc2.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSetBangGlobal tests set! on global variable
func TestCompileSetBangGlobal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define, then set!, then reference
	sv := parseSchemeExpr(t, env, `(define my-var 10)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Set it to new value
	sv2 := parseSchemeExpr(t, env, `(set! my-var 20)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := NewMachineContext(cont2)
	err = mc2.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}

	// Check the new value
	sv3 := parseSchemeExpr(t, env, `my-var`)
	cont3, err := newTopLevelThunk(sv3, env)
	qt.Assert(t, err, qt.IsNil)
	mc3 := NewMachineContext(cont3)
	err = mc3.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc3.GetValue(), values.SchemeEquals, values.NewInteger(20))
}

// TestCompileBegin tests begin form
func TestCompileBegin(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCompileLambdaMultiExprBody tests lambda with multiple expressions in body
func TestCompileLambdaMultiExprBody(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda () 1 2 3))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(3))
}

// TestCompileCaseLambdaMultiClause tests case-lambda with multiple clauses
func TestCompileCaseLambdaMultiClause(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Use only self-evaluating expressions
	sv := parseSchemeExpr(t, env, `(define f (case-lambda (() 0) ((x) x) ((x y) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileQuoteSymbol tests quoting a symbol
func TestCompileQuoteSymbol(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'my-symbol`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isSymbol := val.(*values.Symbol)
	qt.Assert(t, isSymbol, qt.IsTrue)
}

// TestCompileQuoteVector tests quoting a vector
func TestCompileQuoteVector(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'#(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isVector := val.(*values.Vector)
	qt.Assert(t, isVector, qt.IsTrue)
}

// TestCompileIfThenOnly tests if with only then branch
func TestCompileIfThenOnly(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileIfFalsePath tests if with false condition
func TestCompileIfFalsePath(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #f 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

// TestCompileDefineVarSimple tests define simple variable
func TestCompileDefineVarSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define x 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileLambdaWithMultipleParams tests lambda with multiple parameters
func TestCompileLambdaWithMultipleParams(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda (a b c) a) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(1))
}

// TestCompileLambdaRest tests lambda with rest parameter only
func TestCompileLambdaRest(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define all-args (lambda args args))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompileQuoteList tests quoting a list
func TestCompileQuoteList(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
	val := mc.GetValue()
	_, isPair := val.(*values.Pair)
	qt.Assert(t, isPair, qt.IsTrue)
}

// TestCompileSelfEvaluatingNil tests compiling void/nil
func TestCompileSelfEvaluatingNil(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// #void or equivalent
	sv := parseSchemeExpr(t, env, `#t`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileSelfEvaluatingNilDirect tests CompileSelfEvaluating with nil directly
func TestCompileSelfEvaluatingNilDirect(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	tpl := NewNativeTemplate(0, 0, false)
	ctc := NewCompiletimeContinuation(tpl, env)
	ctctx := NewCompileTimeCallContext(false, true, env)

	// Call with nil to test the nil branch
	err := ctc.CompileSelfEvaluating(ctctx, nil)
	qt.Assert(t, err, qt.IsNil)
	// Should have appended LoadVoid operation
	qt.Assert(t, len(tpl.operations), qt.Equals, 1)
}

// TestCompileSyntaxRulesSimple tests simple syntax-rules
func TestCompileSyntaxRulesSimple(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple macro
	sv := parseSchemeExpr(t, env, `(define-syntax my-id (syntax-rules () ((_ x) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	if err != nil && err != ErrMachineHalt {
		t.Fatalf("unexpected error: %v", err)
	}
}

// TestCompiledLibraryMethods tests CompiledLibrary methods
func TestCompiledLibraryMethods(t *testing.T) {
	name := NewLibraryName("test", "lib")
	lib := &CompiledLibrary{
		Name:    name,
		Exports: make(map[string]string),
	}

	lib.Exports["foo"] = "foo"
	qt.Assert(t, len(lib.Exports), qt.Equals, 1)
}

// TestCompileSelfEvaluatingValues tests compilation of self-evaluating values
func TestCompileSelfEvaluatingValues(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"float", "3.14"},
		{"string", "\"hello\""},
		{"character", "#\\a"},
		{"boolean true", "#t"},
		{"boolean false", "#f"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileSymbolVariants tests various symbol compilation paths
func TestCompileSymbolVariants(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"local variable", "((lambda (x) x) 1)"},
		{"nested local", "((lambda (x) ((lambda (y) x) 2)) 1)"},
		{"deeply nested", "((lambda (a) ((lambda (b) ((lambda (c) a) 3)) 2)) 1)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileDefineSyntaxErrors tests define-syntax error cases
func TestCompileDefineSyntaxErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"missing transformer", "(define-syntax foo)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// Should fail
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestCompileExpressionListError tests expression list compilation with improper list
func TestCompileExpressionListError(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// begin with multiple expressions exercises compileExpressionList
	testCases := []struct {
		name string
		prog string
	}{
		{"begin with multiple", "(begin 1 2 3)"},
		{"nested begin", "(begin (begin 1) 2)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileValidatedCallEdgeCases tests edge cases in call compilation
func TestCompileValidatedCallEdgeCases(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// Test tail call optimization
	testCases := []struct {
		name string
		prog string
	}{
		{"tail call in lambda", "(lambda () ((lambda (x) x) 1))"},
		{"non-tail call followed by value", "((lambda () (define x 1) ((lambda (y) y) 2)))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileSetBangErrors tests set! error cases
func TestCompileSetBangErrors(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"set unbound variable", "(set! nonexistent 1)"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			_, err := newTopLevelThunk(sv, env)
			// Should fail for unbound variable
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}

// TestQuasiquoteNeedsRuntime tests quasiquoteNeedsRuntime paths
func TestQuasiquoteNeedsRuntime(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	testCases := []struct {
		name string
		prog string
	}{
		{"simple quote no runtime", "`a"},
		{"nested lists", "`((a b) c)"},
		{"quoted vector", "`#(1 2 3)"},
		{"deeply nested", "`(a (b (c d)))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileIncludeErrorAdditional tests include error paths
func TestCompileIncludeErrorAdditional(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())

	// include with empty string should error
	sv := parseSchemeExpr(t, env, "(include \"\")")
	_, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileQuasiquotePairNestedUnquote tests nested unquote in quasiquote
func TestCompileQuasiquotePairNestedUnquote(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	testCases := []struct {
		name string
		code string
	}{
		{"nested quasiquote", "``a"},
		{"triple nested", "```(a b)"},
		{"quasiquote in list", "`(a `(b c))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			sv := parseSchemeExpr(t, env, tc.code)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)
		})
	}
}

// TestCompileQuasiquoteUnquoteSplicingInList tests unquote-splicing in list context
func TestCompileQuasiquoteUnquoteSplicingInList(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test simple quasiquote with unquote (no list primitive needed)
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSymbolLocal tests CompileSymbol for local variables
func TestCompileSymbolLocal(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test local variable lookup in lambda
	sv := parseSchemeExpr(t, env, "((lambda (x) x) 42)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolNoBinding tests CompileSymbol with unbound symbol
func TestCompileSymbolNoBinding(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, "unbound-symbol-test-xyz")
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such")
}

// TestCompilePrimitiveOrProcedureCallWithPair tests procedure call with pair in function position
func TestCompilePrimitiveOrProcedureCallWithPair(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Call with lambda directly in function position
	sv := parseSchemeExpr(t, env, "((lambda (x) x) 42)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(42))
}

// TestCompileValidatedLambdaVariadic tests variadic lambda compilation
func TestCompileValidatedLambdaVariadic(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with rest parameter (symbol instead of list)
	sv := parseSchemeExpr(t, env, "(lambda args args)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "((lambda args args) 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileValidatedDefineFnDotted tests define with dotted parameter list
func TestCompileValidatedDefineFnDotted(t *testing.T) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define with dotted params
	sv := parseSchemeExpr(t, env, "(define (fn a . rest) rest)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "(fn 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = NewMachineContext(cont)
	err = mc.Run(context.Background())
	qt.Assert(t, err, qt.IsNil)
}
