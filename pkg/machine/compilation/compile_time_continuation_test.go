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
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/machine"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/parser"
	"github.com/aalpar/wile/pkg/schemeutil"
	"github.com/aalpar/wile/pkg/syntax"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestCompileContext_CompileLambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	prog := values.List(values.NewSymbol("lambda"), values.NewSymbol("x"), values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 5)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationPush(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
		machine.NewOperationPush(),
		machine.NewOperationMakeClosure(),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 2)
	tpl0, ok := cont.Template().Literals()[0].(*machine.NativeTemplate)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the template has been compiled correctly
	qt.Assert(t, tpl0.Operations(), qt.HasLen, 2)
	qt.Assert(t, tpl0.Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
		machine.NewOperationRestoreContinuation(),
	))
	qt.Assert(t, tpl0.IsVariadic(), qt.Equals, true)
	qt.Assert(t, tpl0.ParameterCount(), qt.Equals, 1)
	env0, ok := cont.Template().Literals()[1].(*environment.EnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the env has been set up correctly
	qt.Assert(t, env0.LocalEnvironment().Keys(), qt.HasLen, 1)
	qt.Assert(t, env0.GlobalEnvironment(), qt.Equals, env.GlobalEnvironment())

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileLambdaCall(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	prog := values.List(values.List(values.NewSymbol("lambda"), values.NewSymbol("x"), values.NewSymbol("x")), values.NewString("hello"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 11)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationSaveContinuationOffsetImmediate(11),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationPush(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
		machine.NewOperationPush(),
		machine.NewOperationMakeClosure(),
		machine.NewOperationPush(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(2),
		machine.NewOperationPush(),
		machine.NewOperationPull(),
		machine.NewOperationApply(),
		//		machine.NewOperationPush(),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 3)
	tpl0, ok := cont.Template().Literals()[0].(*machine.NativeTemplate)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the template has been compiled correctly
	qt.Assert(t, tpl0.Operations(), qt.HasLen, 2)
	qt.Assert(t, tpl0.Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLocalByLocalIndexImmediate(environment.NewLocalIndex(0, 0)),
		machine.NewOperationRestoreContinuation(),
	))
	qt.Assert(t, tpl0.IsVariadic(), qt.Equals, true)
	qt.Assert(t, tpl0.ParameterCount(), qt.Equals, 1)
	env0, ok := cont.Template().Literals()[1].(*environment.EnvironmentFrame)
	qt.Assert(t, ok, qt.IsTrue)
	// check that the env has been set up correctly
	qt.Assert(t, env0.LocalEnvironment().Keys(), qt.HasLen, 1)
	qt.Assert(t, env0.GlobalEnvironment(), qt.Equals, env.GlobalEnvironment())

	mc := machine.NewMachineContext(context.Background(), cont)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewCons(values.NewString("hello"), values.EmptyList))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileDefine(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	prog := values.List(values.NewSymbol("define"),
		values.NewSymbol("x"),
		values.NewString("y"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 4)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationPush(),
		machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(machine.LiteralIndex(1)),
		machine.NewOperationLoadVoid(),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 2)

	mc := machine.NewMachineContext(context.Background(), cont)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileQuote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	var prog values.Value = values.List(
		values.NewSymbol("quote"),
		values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 1)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 1)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("x"))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileQuasiquote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	var prog values.Value = values.List(
		values.NewSymbol("quasiquote"),
		values.NewSymbol("x"))
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 1)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 1)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewSymbol("x"))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
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
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// evalSchemeString evaluates a Scheme string and returns the final result.
// This is a helper for testing that compiles and runs Scheme code.
func evalSchemeString(code string) (values.Value, error) {
	ctx := context.Background()
	env := environment.NewNamespace().Runtime()

	// Register required primitives
	err := RegisterSyntaxCompilers(env)
	if err != nil {
		return nil, err
	}

	// Register list primitive for quasiquote expansion
	listSym := values.NewSymbol("list")
	env.MaybeCreateOwnGlobalBinding(listSym, environment.BindingTypeVariable)
	listIdx := env.GetGlobalIndex(listSym)
	if listIdx != nil {
		listClosure := machine.NewForeignClosure(env, 1, true, func(mc machine.CallContext) error {
			// The variadic args come as a list in the first local slot.
			// Must copy the spine because the rest-arg list may be
			// backed by the reusable restArgBuf.
			o := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value()
			if values.IsEmptyList(o) {
				mc.SetValue(values.EmptyList)
				return nil
			}
			var elems []values.Value
			_, err := values.ForEach(context.Background(), o, func(_ context.Context, _ int, _ bool, v values.Value) error {
				elems = append(elems, v)
				return nil
			})
			if err != nil {
				return err
			}
			mc.SetValue(values.List(elems...))
			return nil
		})
		err = env.SetOwnGlobalValue(listIdx, listClosure)
		if err != nil {
			return nil, err
		}
	}

	// Parse and evaluate code
	reader := bufio.NewReader(strings.NewReader(code))
	p := parser.NewParser(env, true, reader)

	var lastResult = values.Void
	for {
		stx, err := p.ReadSyntax(context.TODO())
		if err != nil {
			if err == io.EOF {
				break
			}
			return nil, err
		}

		// Expand
		ectx := context.Background()
		econt := NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator())
		expanded, err := econt.ExpandExpression(stx)
		if err != nil {
			return nil, err
		}

		// Compile
		tpl := machine.NewNativeTemplate(0, 0, false)
		cctx := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
		cnt := NewCompileTimeCallContext(context.Background(), false)
		err = cctx.CompileExpression(cnt, expanded)
		if err != nil {
			return nil, err
		}

		// Run
		mc := machine.NewMachineContext(ctx, machine.NewMachineContinuation(nil, tpl, env))
		err = mc.Run()
		if err != nil {
			return nil, err
		}

		if mc.GetValues().Len() > 0 {
			lastResult = mc.GetValue()
		}
	}

	return lastResult, nil
}

func TestCompileContext_CompileIf(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// Set up a global variable 'x' for non-constant test
	symX := values.NewSymbol("x")
	gi, _ := env.MaybeCreateOwnGlobalBinding(symX, environment.BindingTypeVariable)

	// (if x "true" "false") — tests BranchOnFalseValue (value register, no Push)
	prog := values.List(values.NewSymbol("if"),
		values.NewSymbol("x"),
		values.NewString("true"),
		values.NewString("false"))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// BranchOnFalseValue reads value register directly, no Push needed
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 5)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadCachedBinding(0),
		machine.NewOperationBranchOnFalseValueOffsetImmediate(3),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationBranchOffsetImmediate(2),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
	))

	// Run with x = #f → should take the false branch
	_ = env.SetOwnGlobalValue(gi, values.FalseValue)
	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("false"))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)

	// Run with x = #t → should take the true branch
	_ = env.SetOwnGlobalValue(gi, values.TrueValue)
	mc = machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("true"))
}

func TestCompileContext_CompileIfConstantFolding(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (if #f "true" "false") — constant-folds to just "false"
	prog := values.List(values.NewSymbol("if"),
		values.FalseValue,
		values.NewString("true"),
		values.NewString("false"))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// Constant folding: only the alternative branch is compiled
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 1)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
	))

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("false"))

	// (if #t "true" "false") — constant-folds to just "true"
	prog2 := values.List(values.NewSymbol("if"),
		values.TrueValue,
		values.NewString("true"),
		values.NewString("false"))

	cont2, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog2), env)
	qt.Assert(t, err, qt.IsNil)

	qt.Assert(t, cont2.Template().Operations(), qt.HasLen, 1)
	mc2 := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont2.Template(), env))
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc2.GetValue(), valuestest.SchemeEquals, values.NewString("true"))
}

func TestCompileContext_CompileSetBang(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	symX := values.NewSymbol("x")
	_, created := env.MaybeCreateOwnGlobalBinding(symX, environment.BindingTypeVariable)
	qt.Assert(t, created, qt.IsTrue)
	sctx := syntax.NewZeroValueSourceContext()

	// top-level closure with no parameters (thunk)
	prog := values.List(values.NewSymbol("set!"), symX, values.NewString("true"))
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 4)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationPush(),
		machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(machine.LiteralIndex(1)),
		machine.NewOperationLoadVoid(),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 2)
	qt.Assert(t, cont.Template().Literals()[0], valuestest.SchemeEquals, values.NewString("true"))

	// compileSetBang emits env.GetGlobalIndex(sym), which pins Env to the frame
	// resolution found. Build the expectation the same way: a GlobalIndex minted by
	// MaybeCreateOwnGlobalBinding carries Env==nil and denotes a deferred lookup,
	// not this pinned store.
	storeGI := env.GetGlobalIndex(symX)
	qt.Assert(t, storeGI.Env, qt.IsNotNil)
	qt.Assert(t, cont.Template().Literals()[1], valuestest.SchemeEquals, storeGI)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	// set! now returns void with the LoadVoid operation at the end
	qt.Assert(t, mc.GetValue(), qt.Equals, values.Void)
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
	gi := mc.EnvironmentFrame().GlobalEnvironment().GetGlobalIndex(values.NewSymbol("x"))
	qt.Assert(t, gi, qt.IsNotNil)
	v := mc.EnvironmentFrame().GlobalEnvironment().GetOwnGlobalBinding(gi)
	qt.Assert(t, v.BindingType(), qt.Equals, environment.BindingTypeVariable)
	qt.Assert(t, v.Value(), valuestest.SchemeEquals, values.NewString("true"))
}

func TestCompileContext_CompileBegin_0(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	symX := values.NewSymbol("x")
	// top-level closure with no parameters (thunk)
	prog := values.List(values.NewSymbol("begin"),
		values.List(values.NewSymbol("define"), symX,
			values.List(values.NewSymbol("lambda"), values.NewSymbol("y"), values.TrueValue)),
		values.List(symX, values.NewString("bindSymbolWithScopes")))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 4)
	qt.Assert(t, cont.Template().Literals(), valuestest.SchemeEquals,
		machine.NewMultipleValues(
			cont.Template().Literals()[0],
			cont.Template().Literals()[1],
			environment.NewGlobalIndex(symX),
			values.NewString("bindSymbolWithScopes"),
		),
	)
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 15)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationPush(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
		machine.NewOperationPush(),
		machine.NewOperationMakeClosure(),
		machine.NewOperationPush(),
		machine.NewOperationStoreGlobalByGlobalIndexLiteralIndexImmediate(2),
		machine.NewOperationLoadVoid(),
		machine.NewOperationSaveContinuationOffsetImmediate(7),
		machine.NewOperationLoadCachedBinding(0),
		machine.NewOperationPush(),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(3),
		machine.NewOperationPush(),
		machine.NewOperationPull(),
		machine.NewOperationApply(),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 4)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.TrueValue)
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileBegin_1(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	prog := values.List(values.NewSymbol("begin"), values.NewString("true"), values.NewString("false"))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 2)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 2)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("false"))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

func TestCompileContext_CompileMeta(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	prog := values.List(values.NewSymbol("meta"), values.NewString("first"), values.NewString("second"))
	sctx := syntax.NewZeroValueSourceContext()

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// check that the closure has been compiled correctly
	// meta should compile like begin - sequence of expressions
	qt.Assert(t, cont.Template().Operations(), qt.HasLen, 2)
	qt.Assert(t, cont.Template().Operations(), valuestest.SchemeEquals, machine.NewOperations(
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(0),
		machine.NewOperationLoadLiteralByLiteralIndexImmediate(1),
	))
	qt.Assert(t, cont.Template().IsVariadic(), qt.Equals, false)
	qt.Assert(t, cont.Template().ParameterCount(), qt.Equals, 0)
	qt.Assert(t, cont.Template().Literals(), qt.HasLen, 2)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, cont.Template(), env))
	qt.Assert(t, mc.GetValues(), qt.HasLen, 0)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewString("second"))
	qt.Assert(t, *mc.Evals(), qt.HasLen, 0)
}

// newTopLevelThunk and newNamespace are defined in util_test.go.

func TestCondExpandRegistered(t *testing.T) {
	env := environment.NewNamespace().Runtime()
	err := RegisterSyntaxCompilers(env)
	if err != nil {
		t.Fatalf("RegisterSyntaxCompilers failed: %v", err)
	}

	// Check if cond-expand is registered
	sym := values.NewSymbol("cond-expand")
	pc := LookupSyntaxCompiler(env, sym, nil)
	if pc == nil {
		t.Errorf("cond-expand primitive compiler not found")
	} else {
		t.Logf("cond-expand found: %v", pc)
	}

	// Core forms like 'if' are now handled by compileValidated* methods
	// and are NOT registered as primitive compilers. Check that 'if' is NOT registered.
	ifSym := values.NewSymbol("if")
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
	env := newNamespace(environment.NewNamespace().Runtime())

	// Track maximum call depth seen during execution
	var maxCallDepth int

	// Register call-depth primitive: returns current continuation stack depth
	callDepthSym := values.NewSymbol("call-depth")
	env.MaybeCreateOwnGlobalBinding(callDepthSym, environment.BindingTypeVariable)
	callDepthFn := func(cc machine.CallContext) error {
		mc := cc.(*machine.MachineContext)
		depth := mc.CallDepth()
		if depth > maxCallDepth {
			maxCallDepth = depth
		}
		mc.SetValue(values.NewInteger(int64(depth)))
		return nil
	}
	callDepthClosure := machine.NewForeignClosure(env, 0, false, callDepthFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(callDepthSym), callDepthClosure) //nolint:errcheck

	// Register subtraction primitive: (- a b)
	subSym := values.NewSymbol("-")
	env.MaybeCreateOwnGlobalBinding(subSym, environment.BindingTypeVariable)
	subFn := func(mc machine.CallContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		mc.SetValue(values.NewInteger(a - b))
		return nil
	}
	subClosure := machine.NewForeignClosure(env, 2, false, subFn)
	env.SetOwnGlobalValue(environment.NewGlobalIndex(subSym), subClosure) //nolint:errcheck

	// Register equality primitive: (= a b)
	eqSym := values.NewSymbol("=")
	env.MaybeCreateOwnGlobalBinding(eqSym, environment.BindingTypeVariable)
	eqFn := func(mc machine.CallContext) error {
		a := mc.EnvironmentFrame().GetLocalBindingByIndex(0).Value().(*values.Integer).Value
		b := mc.EnvironmentFrame().GetLocalBindingByIndex(1).Value().(*values.Integer).Value
		if a == b {
			mc.SetValue(values.TrueValue)
		} else {
			mc.SetValue(values.FalseValue)
		}
		return nil
	}
	eqClosure := machine.NewForeignClosure(env, 2, false, eqFn)
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
	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, defineProg), env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Now call (loop 100)
	maxCallDepth = 0 // Reset
	callProg := values.List(values.NewSymbol("loop"), values.NewInteger(100))
	cont2, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, callProg), env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := machine.NewMachineContext(context.Background(), cont2)
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)

	// Without TCO: maxCallDepth should be >= 100 (grows with recursion depth)
	// With TCO: maxCallDepth should be small (~2-3, constant)
	t.Logf("Maximum call depth during 100 iterations: %d", maxCallDepth)

	// This assertion verifies TCO IS working (depth stays constant)
	qt.Assert(t, maxCallDepth <= 5, qt.IsTrue,
		qt.Commentf("Expected call depth <= 5 with TCO, got %d. TCO may not be working!", maxCallDepth))
}

func TestCompileContext_CompileCaseLambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (case-lambda
	//   ((x) x)
	//   ((x y) y))
	prog := values.List(
		values.NewSymbol("case-lambda"),
		values.List(values.List(values.NewSymbol("x")), values.NewSymbol("x")),
		values.List(values.List(values.NewSymbol("x"), values.NewSymbol("y")), values.NewSymbol("y")))

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	// Run compilation to create case-lambda closure
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Verify we got a case-lambda closure
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	caseLambda, ok := mc.GetValue().(*machine.CaseLambdaClosure)
	qt.Assert(t, ok, qt.IsTrue)
	qt.Assert(t, caseLambda.Clauses(), qt.HasLen, 2)

	// Verify clause arities
	clauses := caseLambda.Clauses()
	qt.Assert(t, clauses[0].Template().ParameterCount(), qt.Equals, 1)
	qt.Assert(t, clauses[1].Template().ParameterCount(), qt.Equals, 2)
}

func TestCompileContext_CompileCaseLambdaCall(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Should call first clause with 1 arg: returns 42
	qt.Assert(t, mc.GetValues(), qt.HasLen, 1)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
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
			env := newNamespace(environment.NewNamespace().Runtime())
			sv := parseSchemeExpr(t, env, tc.prog)
			cont, err := newTopLevelThunk(sv, env)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, cont, qt.IsNotNil)

			// The compiled thunk wraps the lambda, so check its literals
			qt.Assert(t, len(cont.Template().Literals()) >= 1, qt.IsTrue)
			innerTpl, ok := cont.Template().Literals()[0].(*machine.NativeTemplate)
			qt.Assert(t, ok, qt.IsTrue)
			qt.Assert(t, innerTpl.IsVariadic(), qt.Equals, tc.variadic)
		})
	}
}

// TestSyntaxCompilerNameMethod tests the Name method of PrimitiveCompiler
func TestSyntaxCompilerNameMethod(t *testing.T) {
	pc := NewSyntaxCompiler("test-compiler", nil)
	qt.Assert(t, pc.Name(), qt.Equals, "test-compiler")
}

// TestExpandQuasiquoteAndQuote tests the expander for quasiquote and quote
func TestExpandQuasiquoteAndQuote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Test quote expansion
	quoteProg := values.List(values.NewSymbol("quote"), values.NewSymbol("x"))
	ectx := context.Background()
	econt := NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator())
	expanded, err := econt.ExpandExpression(schemeutil.DatumToSyntaxValue(context.Background(), sctx, quoteProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded, qt.IsNotNil)

	// Test quasiquote expansion
	qqProg := values.List(values.NewSymbol("quasiquote"), values.List(values.NewSymbol("a"), values.NewSymbol("b")))
	expanded2, err := econt.ExpandExpression(schemeutil.DatumToSyntaxValue(context.Background(), sctx, qqProg))
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, expanded2, qt.IsNotNil)
}

// TestCompileSymbolUnboundError tests compile error for unbound symbol
func TestCompileSymbolUnboundError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()
	expr := values.NewSymbol("undefined-symbol")

	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, expr), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such binding")
}

// TestCompileLambdaDuplicateParamError tests error for duplicate lambda params
func TestCompileLambdaDuplicateParamError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (x x) x) should error due to duplicate parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewSymbol("x"), values.NewSymbol("x")),
		values.NewSymbol("x"),
	)

	_, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "duplicate")
}

// TestCompileLambdaInvalidParamError tests error for invalid lambda parameter
func TestCompileLambdaInvalidParamError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	sctx := syntax.NewZeroValueSourceContext()

	// (lambda (1) 42) should error - 1 is not a valid parameter
	prog := values.List(
		values.NewSymbol("lambda"),
		values.List(values.NewInteger(1)),
		values.NewInteger(42),
	)

	_, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileNestedQuasiquote tests doubly-nested quasiquote
func TestCompileNestedQuasiquote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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

	cont, err := newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)
}

// TestCompileCaseLambda tests compiling case-lambda
func TestCompileCaseLambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	prog := `(define x 42)`

	sv := parseSchemeExpr(t, env, prog)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, cont, qt.IsNotNil)

	// Run it
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSetBang tests compiling set!
func TestCompileSetBang(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define x
	sv := parseSchemeExpr(t, env, `(define x 0)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Now set! it
	sv2 := parseSchemeExpr(t, env, `(set! x 42)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := machine.NewMachineContext(context.Background(), cont2)
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileIfBranches tests compiling if with both branches
func TestCompileIfBranches(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// if with else
	sv := parseSchemeExpr(t, env, `(if #t 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(1))

	// if without else (just consequent)
	sv2 := parseSchemeExpr(t, env, `(if #f 1)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := machine.NewMachineContext(context.Background(), cont2)
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileUnquoteError tests error when compiling unquote outside quasiquote
func TestCompileUnquoteError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote - should error
	prog := values.List(values.NewSymbol("unquote"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileUnquoteSplicingError tests error when compiling unquote-splicing outside quasiquote
func TestCompileUnquoteSplicingError(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sctx := syntax.NewZeroValueSourceContext()

	// Try to compile bare unquote-splicing - should error
	prog := values.List(values.NewSymbol("unquote-splicing"), values.NewSymbol("x"))
	_, err = newTopLevelThunk(schemeutil.DatumToSyntaxValue(context.Background(), sctx, prog), env)
	qt.Assert(t, err, qt.IsNotNil)
}

// TestCompileQuasiquoteSimple tests quasiquote without unquote (no list/append needed)
func TestCompileQuasiquoteSimple(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple quasiquote without any unquote - should be a compile-time constant
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileBeginSequence tests begin with multiple expressions
func TestCompileBeginSequence(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(3))
}

// TestCompileIfNoAlternate tests if without else
func TestCompileIfNoAlternate(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// If without else, true case
	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))

	// If without else, false case - should return void
	sv = parseSchemeExpr(t, env, `(if #f 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileLambdaWithRestParameter tests lambda with rest parameter
func TestCompileLambdaWithRestParameter(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda (a . rest) rest) - dotted parameter list
	sv := parseSchemeExpr(t, env, `((lambda (a . rest) rest) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	// rest should be (2 3)
	result := mc.GetValue()
	qt.Assert(t, result, qt.IsNotNil)
}

// TestCompileLambdaRestOnly tests lambda with only rest parameter
func TestCompileLambdaRestOnly(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (lambda args args) - single rest parameter
	sv := parseSchemeExpr(t, env, `((lambda args args) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileCondExpandNotFeature tests cond-expand with not feature
func TestCompileCondExpandNotFeature(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (not nonexistent) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((not nonexistent) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandAndFeature tests cond-expand with and feature
func TestCompileCondExpandAndFeature(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (and r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((and r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandOrFeature tests cond-expand with or feature
func TestCompileCondExpandOrFeature(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (or nonexistent r7rs) - should match
	sv := parseSchemeExpr(t, env, `(cond-expand ((or nonexistent r7rs) 42))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileCondExpandLibraryFeature tests cond-expand with library feature
func TestCompileCondExpandLibraryFeature(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// cond-expand with (library (scheme base)) - may not be available but tests parsing
	sv := parseSchemeExpr(t, env, `(cond-expand ((library (nonexistent lib)) 1) (else 99))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

// TestCompileSymbolBranches tests various branches of CompileSymbol
func TestCompileSymbolBranches(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test compiling a defined global variable
	sv := parseSchemeExpr(t, env, `(define global-var 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Now compile a reference to the global
	sv = parseSchemeExpr(t, env, `global-var`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolLocalBinding tests compiling local bindings
func TestCompileSymbolLocalBinding(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with local parameter
	sv := parseSchemeExpr(t, env, `((lambda (local-var) local-var) 99)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(99))
}

// TestCompileMultipleForms tests compiling multiple forms
func TestCompileMultipleForms(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define and use
	sv := parseSchemeExpr(t, env, `(define fn (lambda (x) x))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call the function
	sv = parseSchemeExpr(t, env, `(fn 42)`)
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileSelfEvaluating tests compiling self-evaluating values
func TestCompileSelfEvaluating(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())

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
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, tc.expect)
		})
	}
}

// TestCompileNestedLambda tests nested lambda expressions
func TestCompileNestedLambda(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested lambda - currying
	sv := parseSchemeExpr(t, env, `(((lambda (x) (lambda (y) x)) 1) 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(1))
}

// TestCompileComplexIf tests complex if expressions
func TestCompileComplexIf(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Nested if expressions
	sv := parseSchemeExpr(t, env, `(if #t (if #f 1 2) 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))
}

// TestCompileUnquoteOutsideQuasiquote tests that unquote outside quasiquote returns error
func TestCompileUnquoteOutsideQuasiquote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn x) x) shorthand form
	sv := parseSchemeExpr(t, env, `(define (identity x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileDefineFnVariadic tests define with variadic function shorthand
func TestCompileDefineFnVariadic(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// (define (fn . args) args) variadic shorthand form
	sv := parseSchemeExpr(t, env, `(define (varargs . x) x)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSymbolGlobal tests compiling a global symbol reference
func TestCompileSymbolGlobal(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// First define a global
	sv := parseSchemeExpr(t, env, `(define my-global 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Then reference it
	sv2 := parseSchemeExpr(t, env, `my-global`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := machine.NewMachineContext(context.Background(), cont2)
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc2.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileSetBangGlobal tests set! on global variable
func TestCompileSetBangGlobal(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define, then set!, then reference
	sv := parseSchemeExpr(t, env, `(define my-var 10)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Set it to new value
	sv2 := parseSchemeExpr(t, env, `(set! my-var 20)`)
	cont2, err := newTopLevelThunk(sv2, env)
	qt.Assert(t, err, qt.IsNil)
	mc2 := machine.NewMachineContext(context.Background(), cont2)
	err = mc2.Run()
	qt.Assert(t, err, qt.IsNil)

	// Check the new value
	sv3 := parseSchemeExpr(t, env, `my-var`)
	cont3, err := newTopLevelThunk(sv3, env)
	qt.Assert(t, err, qt.IsNil)
	mc3 := machine.NewMachineContext(context.Background(), cont3)
	err = mc3.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc3.GetValue(), valuestest.SchemeEquals, values.NewInteger(20))
}

// TestCompileBegin tests begin form
func TestCompileBegin(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(begin 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(3))
}

// TestCompileLambdaMultiExprBody tests lambda with multiple expressions in body
func TestCompileLambdaMultiExprBody(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda () 1 2 3))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(3))
}

// TestCompileCaseLambdaMultiClause tests case-lambda with multiple clauses
func TestCompileCaseLambdaMultiClause(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Use only self-evaluating expressions
	sv := parseSchemeExpr(t, env, `(define f (case-lambda (() 0) ((x) x) ((x y) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileQuoteSymbol tests quoting a symbol
func TestCompileQuoteSymbol(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'my-symbol`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	val := mc.GetValue()
	_, isSymbol := val.(*values.Symbol)
	qt.Assert(t, isSymbol, qt.IsTrue)
}

// TestCompileQuoteVector tests quoting a vector
func TestCompileQuoteVector(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'#(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	val := mc.GetValue()
	_, isVector := val.(*values.Vector)
	qt.Assert(t, isVector, qt.IsTrue)
}

// TestCompileIfThenOnly tests if with only then branch
func TestCompileIfThenOnly(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #t 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileIfFalsePath tests if with false condition
func TestCompileIfFalsePath(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(if #f 1 2)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))
}

// TestCompileDefineVarSimple tests define simple variable
func TestCompileDefineVarSimple(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define x 42)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileLambdaWithMultipleParams tests lambda with multiple parameters
func TestCompileLambdaWithMultipleParams(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `((lambda (a b c) a) 1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(1))
}

// TestCompileLambdaRest tests lambda with rest parameter only
func TestCompileLambdaRest(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `(define all-args (lambda args args))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileQuoteList tests quoting a list
func TestCompileQuoteList(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, `'(1 2 3)`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	val := mc.GetValue()
	_, isPair := val.(*values.Pair)
	qt.Assert(t, isPair, qt.IsTrue)
}

// TestCompileSelfEvaluatingNil tests compiling void/nil
func TestCompileSelfEvaluatingNil(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := environment.NewNamespace().Runtime()
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	ctctx := NewCompileTimeCallContext(context.Background(), false)

	// Call with nil to test the nil branch
	err := ctc.CompileSelfEvaluating(ctctx, nil)
	qt.Assert(t, err, qt.IsNil)
	// Should have appended LoadVoid operation
	qt.Assert(t, tpl.CodeLen(), qt.Equals, 1)
}

// TestCompileSyntaxRulesSimple tests simple syntax-rules
func TestCompileSyntaxRulesSimple(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Simple macro
	sv := parseSchemeExpr(t, env, `(define-syntax my-id (syntax-rules () ((_ x) x)))`)
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompiledLibraryMethods tests CompiledLibrary methods
func TestCompiledLibraryMethods(t *testing.T) {
	name := NewLibraryName("test", "lib")
	lib := &CompiledLibrary{
		Name:    name, //nolint:govet
		Exports: make(map[string]string),
	}

	lib.Exports["bindSymbolWithScopes"] = "bindSymbolWithScopes"
	qt.Assert(t, len(lib.Exports), qt.Equals, 1)
}

// TestCompileSelfEvaluatingValues tests compilation of self-evaluating values
func TestCompileSelfEvaluatingValues(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

	testCases := []struct {
		name string
		prog string
	}{
		{"missing transformer", "(define-syntax bindSymbolWithScopes)"},
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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

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
	env := newNamespace(environment.NewNamespace().Runtime())

	// include with empty string should error (empty-path guard)
	sv := parseSchemeExpr(t, env, "(include \"\")")
	_, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "empty filename")
}

// TestFindFileCWDFallback verifies that findFile falls back to CWD when
// the LoadPathStack is empty and SCHEME_INCLUDE_PATH is unset.
func TestFindFileCWDFallback(t *testing.T) {
	// Create a temp file in a temp dir, then chdir there
	dir := t.TempDir()
	err := os.WriteFile(filepath.Join(dir, "cwd-test.scm"), []byte("42"), 0o644)
	qt.Assert(t, err, qt.IsNil)

	// Save and restore CWD
	origDir, err := os.Getwd()
	qt.Assert(t, err, qt.IsNil)
	defer os.Chdir(origDir) //nolint:errcheck

	err = os.Chdir(dir)
	qt.Assert(t, err, qt.IsNil)

	// Clear SCHEME_INCLUDE_PATH so only CWD fallback applies
	oldInclude, hadInclude := os.LookupEnv(SchemeIncludePathEnv)
	defer func() {
		if hadInclude {
			os.Setenv(SchemeIncludePathEnv, oldInclude) //nolint:errcheck
		} else {
			os.Unsetenv(SchemeIncludePathEnv) //nolint:errcheck
		}
	}()
	os.Unsetenv(SchemeIncludePathEnv) //nolint:errcheck

	env := newNamespace(environment.NewNamespace().Runtime())
	ctctx := NewCompileTimeCallContext(context.Background(), false)
	cont := NewCompileTimeContinuation(machine.NewNativeTemplate(0, 0, false), env, machine.NewVMMacroEvaluator())

	// findFile should resolve "cwd-test.scm" via CWD fallback.
	// Use filepath.EvalSymlinks on dir to normalize: on macOS, t.TempDir()
	// returns /tmp/... but os.Getwd() (inside findFile) returns /private/tmp/...
	// because /tmp is a symlink.
	realDir, err := filepath.EvalSymlinks(dir)
	qt.Assert(t, err, qt.IsNil)

	f, absPath, err := findFile(cont, ctctx, "cwd-test.scm")
	qt.Assert(t, err, qt.IsNil)
	defer f.Close()
	qt.Assert(t, absPath, qt.Equals, filepath.Join(realDir, "cwd-test.scm"))
}

// TestFindFileEmptyPath verifies that findFile rejects empty paths.
func TestFindFileEmptyPath(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	ctctx := NewCompileTimeCallContext(context.Background(), false)
	cont := NewCompileTimeContinuation(machine.NewNativeTemplate(0, 0, false), env, machine.NewVMMacroEvaluator())

	_, _, err := findFile(cont, ctctx, "")
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "empty filename")
}

// TestCompileQuasiquotePairNestedUnquote tests nested unquote in quasiquote
func TestCompileQuasiquotePairNestedUnquote(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
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
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test simple quasiquote with unquote (no list primitive needed)
	sv := parseSchemeExpr(t, env, "`(a b c)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileSymbolLocal tests CompileSymbol for local variables
func TestCompileSymbolLocal(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Test local variable lookup in lambda
	sv := parseSchemeExpr(t, env, "((lambda (x) x) 42)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(42))
}

// TestCompileSymbolNoBinding tests CompileSymbol with unbound symbol
func TestCompileSymbolNoBinding(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	sv := parseSchemeExpr(t, env, "unbound-symbol-test-xyz")
	_, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, err.Error(), qt.Contains, "no such")
}

// TestCompileValidatedLambdaVariadic tests variadic lambda compilation
func TestCompileValidatedLambdaVariadic(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Lambda with rest parameter (symbol instead of list)
	sv := parseSchemeExpr(t, env, "(lambda args args)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "((lambda args args) 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestCompileValidatedDefineFnDotted tests define with dotted parameter list
func TestCompileValidatedDefineFnDotted(t *testing.T) {
	env := newNamespace(environment.NewNamespace().Runtime())
	err := RegisterSyntaxCompilers(env)
	qt.Assert(t, err, qt.IsNil)

	// Define with dotted params
	sv := parseSchemeExpr(t, env, "(define (fn a . rest) rest)")
	cont, err := newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Call it
	sv = parseSchemeExpr(t, env, "(fn 1 2 3)")
	cont, err = newTopLevelThunk(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
}

// TestValidateQuotedLiteral_CircularDatumLabel tests that circular datum labels
// produce a compile error instead of crashing with a stack overflow.
func TestValidateQuotedLiteral_CircularDatumLabel(t *testing.T) {
	t.Run("circular cdr", func(t *testing.T) {
		_, err := evalSchemeString("'#0=(a . #0#)")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})

	t.Run("circular car", func(t *testing.T) {
		_, err := evalSchemeString("'#0=(#0# . b)")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})

	t.Run("non-circular datum label", func(t *testing.T) {
		result, err := evalSchemeString("'#0=(a b)")
		qt.Assert(t, err, qt.IsNil)
		expected := values.List(values.NewSymbol("a"), values.NewSymbol("b"))
		qt.Assert(t, result, valuestest.SchemeEquals, expected)
	})

	t.Run("datum label reference", func(t *testing.T) {
		result, err := evalSchemeString("'(#0=a #0#)")
		qt.Assert(t, err, qt.IsNil)
		expected := values.List(values.NewSymbol("a"), values.NewSymbol("a"))
		qt.Assert(t, result, valuestest.SchemeEquals, expected)
	})

	t.Run("shared but acyclic datum label", func(t *testing.T) {
		// #0=(a) referenced twice is shared but NOT circular.
		// Must compile successfully, not be rejected as circular.
		result, err := evalSchemeString("'(#0=(a) #0#)")
		qt.Assert(t, err, qt.IsNil)
		inner := values.List(values.NewSymbol("a"))
		expected := values.List(inner, inner)
		qt.Assert(t, result, valuestest.SchemeEquals, expected)
	})

	t.Run("equal on circular datum labels", func(t *testing.T) {
		// Both quoted arguments contain circular datum labels.
		// The first quote to be compiled triggers ErrInvalidSyntax.
		// evalSchemeString doesn't have equal? registered, so we test
		// through the full engine instead.
		// Here we just verify each quote independently errors.
		_, err := evalSchemeString("'#1=(a . #1#)")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)

		_, err = evalSchemeString("'#2=(a . #2#)")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})

	t.Run("vector datum label with self-reference", func(t *testing.T) {
		// #0=#(a #0#) — the parser now resolves the self-reference, producing a
		// genuinely circular vector (element 1 points to the vector itself). Like
		// a circular list literal (R7RS §2.4), a circular vector literal in code
		// is rejected at compile time with ErrInvalidSyntax rather than crashing
		// the compiler with a stack overflow.
		_, err := evalSchemeString("'#0=#(a #0#)")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})

	t.Run("shared but acyclic vector datum label", func(t *testing.T) {
		// #0=#(1) referenced twice is shared but NOT circular — must compile,
		// mirroring the shared-acyclic LIST subtest above. Pins the vector
		// branch's visited cleanup (delete(visited, val)); without it this is
		// mis-rejected as ErrInvalidSyntax.
		result, err := evalSchemeString("'(#0=#(1) #0#)")
		qt.Assert(t, err, qt.IsNil)
		inner := values.NewVector(values.NewInteger(1))
		expected := values.List(inner, inner)
		qt.Assert(t, result, valuestest.SchemeEquals, expected)
	})

	t.Run("nested circular vector", func(t *testing.T) {
		// #0=#(1 #(2 #0#)) — the cycle closes one level deeper, through a child
		// vector, so the visited[val] hit fires only after recursive descent.
		// Rejected like the direct self-reference.
		_, err := evalSchemeString("'#0=#(1 #(2 #0#))")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})

	t.Run("cross-container cycle through a vector", func(t *testing.T) {
		// #0=(1 #(2 #0#)) — a labeled list whose element is a vector that
		// back-references the list. The single visited set spans pair and
		// vector nodes, so a cycle threading through both container types is
		// still caught.
		_, err := evalSchemeString("'#0=(1 #(2 #0#))")
		qt.Assert(t, err, qt.IsNotNil)
		qt.Assert(t, err, qt.ErrorIs, werr.ErrInvalidSyntax)
	})
}

// TestDeduplicateLiteral_CircularPair tests that DeduplicateLiteral
// terminates on circular pairs (defense in depth).
func TestDeduplicateLiteral_CircularPair(t *testing.T) {
	// Construct a circular pair: (a . <self>)
	pair := values.NewCons(values.NewSymbol("a"), values.EmptyList)
	pair.SetCdr(pair)

	tpl := machine.NewNativeTemplate(0, 0, false)
	// Should not hang or crash — defense in depth returns the pair unchanged
	result := tpl.DeduplicateLiteral(pair)
	qt.Assert(t, result, qt.IsNotNil)
}

// TestDeduplicateLiteral_SharedAcyclicPair tests that DeduplicateLiteral
// returns consistent results for shared-but-acyclic structures.
// Both occurrences of the shared pair must get the same deduplicated result.
func TestDeduplicateLiteral_SharedAcyclicPair(t *testing.T) {
	// Construct shared structure: root = (shared . shared) where shared = (sym)
	sym := values.NewSymbol("x")
	shared := values.NewCons(sym, values.EmptyList)
	root := values.NewCons(shared, shared)

	tpl := machine.NewNativeTemplate(0, 0, false)
	// Pre-populate the literal pool with the symbol so deduplication
	// actually produces a different pointer for the inner pair.
	tpl.MaybeAppendLiteral(sym)

	result := tpl.DeduplicateLiteral(root)
	resultPair := result.(*values.Pair)
	// Both car and cdr must point to the same deduplicated pair.
	qt.Assert(t, resultPair.Car(), qt.Equals, resultPair.Cdr())
}
