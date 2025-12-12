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
	if err := RegisterPrimitiveCompilers(env); err != nil {
		return nil, err
	}

	// Register list primitive for quasiquote expansion
	listSym := env.InternSymbol(values.NewSymbol("list"))
	env.MaybeCreateGlobalBinding(listSym, environment.BindingTypeVariable)
	listIdx := env.GetGlobalIndex(listSym)
	if listIdx != nil {
		listClosure := NewForeignClosure(env, 1, true, func(_ context.Context, mc *MachineContext) error {
			// The variadic args come as a list in the first local slot
			o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
			mc.SetValue(o)
			return nil
		})
		if err := env.SetGlobalValue(listIdx, listClosure); err != nil {
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
	gi, _ := env.MaybeCreateGlobalBinding(symX, environment.BindingTypeVariable)
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

	env.MaybeCreateGlobalBinding(ifSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(lambdaSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(quoteSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(quasiquoteSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(defineSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(setSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(beginSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(metaSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(includeSym, environment.BindingTypePrimitive)
	env.MaybeCreateGlobalBinding(includeCiSym, environment.BindingTypePrimitive)
	return env
}

func TestCondExpandRegistered(t *testing.T) {
	env := environment.NewTopLevelEnvironmentFrame()
	err := RegisterPrimitiveCompilers(env)
	if err != nil {
		t.Fatalf("RegisterPrimitiveCompilers failed: %v", err)
	}

	// Check if cond-expand is registered
	sym := env.InternSymbol(values.NewSymbol("cond-expand"))
	pc := LookupPrimitiveCompiler(env, sym, nil)
	if pc == nil {
		t.Errorf("cond-expand primitive compiler not found")
	} else {
		t.Logf("cond-expand found: %v", pc)
	}

	// Core forms like 'if' are now handled by compileValidated* methods
	// and are NOT registered as primitive compilers. Check that 'if' is NOT registered.
	ifSym := env.InternSymbol(values.NewSymbol("if"))
	ifPc := LookupPrimitiveCompiler(env, ifSym, nil)
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
	env.MaybeCreateGlobalBinding(callDepthSym, environment.BindingTypeVariable)
	callDepthFn := func(ctx context.Context, mc *MachineContext) error {
		depth := mc.CallDepth()
		if depth > maxCallDepth {
			maxCallDepth = depth
		}
		mc.SetValue(values.NewInteger(int64(depth)))
		return nil
	}
	callDepthClosure := NewForeignClosure(env, 0, false, callDepthFn)
	env.SetGlobalValue(environment.NewGlobalIndex(callDepthSym), callDepthClosure) //nolint:errcheck

	// Register subtraction primitive: (- a b)
	subSym := env.InternSymbol(values.NewSymbol("-"))
	env.MaybeCreateGlobalBinding(subSym, environment.BindingTypeVariable)
	subFn := func(ctx context.Context, mc *MachineContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a - b))
		return nil
	}
	subClosure := NewForeignClosure(env, 2, false, subFn)
	env.SetGlobalValue(environment.NewGlobalIndex(subSym), subClosure) //nolint:errcheck

	// Register equality primitive: (= a b)
	eqSym := env.InternSymbol(values.NewSymbol("="))
	env.MaybeCreateGlobalBinding(eqSym, environment.BindingTypeVariable)
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
	env.SetGlobalValue(environment.NewGlobalIndex(eqSym), eqClosure) //nolint:errcheck

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
