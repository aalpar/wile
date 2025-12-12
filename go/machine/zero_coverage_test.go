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
	"context"
	"testing"

	"wile/environment"
	"wile/syntax"
	"wile/values"

	qt "github.com/frankban/quicktest"
)

// =============================================================================
// ErrExceptionEscape Tests (exception_escape.go)
// =============================================================================

func TestErrExceptionEscape_Error_NilCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   nil,
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: <nil>")
}

func TestErrExceptionEscape_Error_WithCondition(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewString("test error"),
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: \"test error\"")
}

func TestErrExceptionEscape_Error_WithSymbol(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewSymbol("error-type"),
		Continuable: true,
	}

	c.Assert(err.Error(), qt.Equals, "exception: error-type")
}

func TestErrExceptionEscape_Error_WithInteger(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewInteger(42),
		Continuable: false,
	}

	c.Assert(err.Error(), qt.Equals, "exception: 42")
}

func TestErrExceptionEscape_Continuable(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition:   values.NewString("continuable"),
		Continuable: true,
	}

	c.Assert(err.Continuable, qt.IsTrue)
}

func TestErrExceptionEscape_WithContinuation(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	cont := NewMachineContinuation(nil, tpl, env)

	err := &ErrExceptionEscape{
		Condition:    values.NewString("error"),
		Continuable:  true,
		Continuation: cont,
	}

	c.Assert(err.Continuation, qt.Equals, cont)
}

func TestErrExceptionEscape_Handled(t *testing.T) {
	c := qt.New(t)

	err := &ErrExceptionEscape{
		Condition: values.NewString("handled"),
		Handled:   true,
	}

	c.Assert(err.Handled, qt.IsTrue)
}

// =============================================================================
// ExceptionHandler Tests (exception_handler.go)
// =============================================================================

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

// =============================================================================
// ExpanderContext Tests (expander_context.go)
// =============================================================================

func TestNewExpanderContext(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	c.Assert(ctx, qt.IsNotNil)
	c.Assert(ctx.Env(), qt.Equals, env)
}

func TestExpanderContext_IntroductionScope(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	// Initially nil
	c.Assert(ctx.IntroductionScope(), qt.IsNil)

	// Set scope
	scope := syntax.NewScope(nil)
	ctx.SetIntroductionScope(scope)
	c.Assert(ctx.IntroductionScope(), qt.Equals, scope)
}

func TestExpanderContext_UseSiteScope(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	expander := NewExpanderTimeContinuation(env)
	ectx := NewExpandTimeCallContext()

	ctx := NewExpanderContext(env, expander, ectx)

	// Initially nil
	c.Assert(ctx.UseSiteScope(), qt.IsNil)

	// Set scope
	scope := syntax.NewScope(nil)
	ctx.SetUseSiteScope(scope)
	c.Assert(ctx.UseSiteScope(), qt.Equals, scope)
}

// =============================================================================
// OperationBuildSyntaxList Tests (operation_build_syntax.go)
// =============================================================================

func TestNewOperationBuildSyntaxList(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)

	c.Assert(op, qt.IsNotNil)
	c.Assert(op.Count, qt.Equals, 3)
}

func TestOperationBuildSyntaxList_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.String(), qt.Equals, "BuildSyntaxList")
}

func TestOperationBuildSyntaxList_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:build-syntax-list>")
}

func TestOperationBuildSyntaxList_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBuildSyntaxList(3)
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationBuildSyntaxList_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationBuildSyntaxList(3)
	op2 := NewOperationBuildSyntaxList(3)
	op3 := NewOperationBuildSyntaxList(5)

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(op3), qt.IsFalse)
	c.Assert(op1.EqualTo(values.NewInteger(3)), qt.IsFalse)
}

func TestOperationBuildSyntaxList_Apply_Empty(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationBuildSyntaxList(0)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	// Should produce empty list
	val := mc.GetValue()
	c.Assert(val, qt.IsNotNil)
}

func TestOperationBuildSyntaxList_Apply_WithSyntaxValues(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push syntax values to stack
	stx1 := syntax.NewSyntaxSymbol("a", nil)
	stx2 := syntax.NewSyntaxSymbol("b", nil)
	mc.evals.Push(stx1)
	mc.evals.Push(stx2)

	op := NewOperationBuildSyntaxList(2)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)

	// Result should be a syntax list
	val := mc.GetValue()
	c.Assert(val, qt.IsNotNil)
}

func TestOperationBuildSyntaxList_Apply_WithValues(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push regular values to stack
	mc.evals.Push(values.NewInteger(1))
	mc.evals.Push(values.NewInteger(2))

	op := NewOperationBuildSyntaxList(2)
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
}

// =============================================================================
// OperationDrop Tests (operation_drop.go)
// =============================================================================

func TestNewOperationDrop(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationDrop_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.SchemeString(), qt.Equals, "#<machine-operation-drop>")
}

func TestOperationDrop_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	c.Assert(op.IsVoid(), qt.IsFalse)

	var nilOp *OperationDrop
	c.Assert(nilOp.IsVoid(), qt.IsTrue)
}

func TestOperationDrop_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationDrop()
	op2 := NewOperationDrop()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationDrop_EqualTo_NilCases(t *testing.T) {
	c := qt.New(t)

	op := NewOperationDrop()
	var nilOp *OperationDrop

	c.Assert(op.EqualTo(nilOp), qt.IsFalse)
	c.Assert(nilOp.EqualTo(op), qt.IsFalse)
	c.Assert(nilOp.EqualTo(nilOp), qt.IsTrue)
}

func TestOperationDrop_Apply(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Push value to stack
	mc.evals.Push(values.NewInteger(42))
	c.Assert(mc.evals.Length(), qt.Equals, 1)

	op := NewOperationDrop()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(mc.evals.Length(), qt.Equals, 0)
	c.Assert(mc.pc, qt.Equals, 1)
}

// =============================================================================
// OperationPopEnv Tests (operation_pop_env.go)
// =============================================================================

func TestNewOperationPopEnv(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationPopEnv_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.String(), qt.Equals, "PopEnv")
}

func TestOperationPopEnv_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:pop-env>")
}

func TestOperationPopEnv_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationPopEnv()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationPopEnv_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationPopEnv()
	op2 := NewOperationPopEnv()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationPopEnv_Apply_Success(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	parentEnv := environment.NewEnvironmentFrame(nil, genv)
	childEnv := environment.NewEnvironmentFrameWithParent(nil, parentEnv)

	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, childEnv))

	op := NewOperationPopEnv()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(mc.env, qt.Equals, parentEnv)
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationPopEnv_Apply_Error_NoParent(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	// env has no parent (Parent() returns nil)

	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationPopEnv()
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "PopEnv")
}

// =============================================================================
// syntaxCaseClause Tests (compile_syntax_case.go)
// =============================================================================

func TestSyntaxCaseClause_EqualTo(t *testing.T) {
	c := qt.New(t)

	clause := &syntaxCaseClause{
		patternVars: map[string]struct{}{"x": {}},
	}

	c.Assert(clause.EqualTo(clause), qt.IsFalse) // Always returns false
	c.Assert(clause.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestSyntaxCaseClause_IsVoid(t *testing.T) {
	c := qt.New(t)

	clause := &syntaxCaseClause{}
	c.Assert(clause.IsVoid(), qt.IsFalse)
}

func TestSyntaxCaseClause_SchemeString(t *testing.T) {
	c := qt.New(t)

	clause := &syntaxCaseClause{}
	c.Assert(clause.SchemeString(), qt.Equals, "#<syntax-case-clause>")
}

// =============================================================================
// OperationSyntaxCaseMatch Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationSyntaxCaseMatch(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseMatch()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationSyntaxCaseMatch_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseMatch()
	c.Assert(op.String(), qt.Equals, "SyntaxCaseMatch")
}

func TestOperationSyntaxCaseMatch_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseMatch()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:syntax-case-match>")
}

func TestOperationSyntaxCaseMatch_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationSyntaxCaseMatch()
	op2 := NewOperationSyntaxCaseMatch()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationSyntaxCaseMatch_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseMatch()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

// =============================================================================
// OperationBindPatternVars Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationBindPatternVars(t *testing.T) {
	c := qt.New(t)

	vars := map[string]struct{}{"x": {}, "y": {}}
	op := NewOperationBindPatternVars(vars)

	c.Assert(op, qt.IsNotNil)
	c.Assert(len(op.PatternVars), qt.Equals, 2)
}

func TestOperationBindPatternVars_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBindPatternVars(map[string]struct{}{"x": {}})
	c.Assert(op.String(), qt.Equals, "BindPatternVars")
}

func TestOperationBindPatternVars_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBindPatternVars(map[string]struct{}{"x": {}})
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:bind-pattern-vars>")
}

func TestOperationBindPatternVars_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationBindPatternVars(map[string]struct{}{"x": {}})
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationBindPatternVars_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationBindPatternVars(map[string]struct{}{"x": {}})
	op2 := NewOperationBindPatternVars(map[string]struct{}{"x": {}})
	op3 := NewOperationBindPatternVars(map[string]struct{}{"y": {}})
	op4 := NewOperationBindPatternVars(map[string]struct{}{"x": {}, "y": {}})

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(op3), qt.IsFalse) // Different var name
	c.Assert(op1.EqualTo(op4), qt.IsFalse) // Different count
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

// =============================================================================
// OperationSyntaxCaseNoMatch Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationSyntaxCaseNoMatch(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseNoMatch()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationSyntaxCaseNoMatch_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseNoMatch()
	c.Assert(op.String(), qt.Equals, "SyntaxCaseNoMatch")
}

func TestOperationSyntaxCaseNoMatch_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseNoMatch()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:syntax-case-no-match>")
}

func TestOperationSyntaxCaseNoMatch_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationSyntaxCaseNoMatch()
	op2 := NewOperationSyntaxCaseNoMatch()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationSyntaxCaseNoMatch_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxCaseNoMatch()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationSyntaxCaseNoMatch_Apply(t *testing.T) {
	c := qt.New(t)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationSyntaxCaseNoMatch()
	_, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case: no matching clause")
}

// =============================================================================
// OperationSyntaxTemplateExpand Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationSyntaxTemplateExpand(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxTemplateExpand()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationSyntaxTemplateExpand_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxTemplateExpand()
	c.Assert(op.String(), qt.Equals, "SyntaxTemplateExpand")
}

func TestOperationSyntaxTemplateExpand_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxTemplateExpand()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:syntax-template-expand>")
}

func TestOperationSyntaxTemplateExpand_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationSyntaxTemplateExpand()
	op2 := NewOperationSyntaxTemplateExpand()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationSyntaxTemplateExpand_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationSyntaxTemplateExpand()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

// =============================================================================
// OperationStoreSyntaxCaseInput Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationStoreSyntaxCaseInput(t *testing.T) {
	c := qt.New(t)

	op := NewOperationStoreSyntaxCaseInput()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationStoreSyntaxCaseInput_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationStoreSyntaxCaseInput()
	c.Assert(op.String(), qt.Equals, "StoreSyntaxCaseInput")
}

func TestOperationStoreSyntaxCaseInput_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationStoreSyntaxCaseInput()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:store-syntax-case-input>")
}

func TestOperationStoreSyntaxCaseInput_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationStoreSyntaxCaseInput()
	op2 := NewOperationStoreSyntaxCaseInput()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationStoreSyntaxCaseInput_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationStoreSyntaxCaseInput()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationStoreSyntaxCaseInput_Apply_SyntaxValue(t *testing.T) {
	c := qt.New(t)

	// Clear any previous state
	currentSyntaxCaseInput = nil

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	stx := syntax.NewSyntaxSymbol("test", nil)
	mc.SetValue(stx)

	op := NewOperationStoreSyntaxCaseInput()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(currentSyntaxCaseInput, qt.IsNotNil)
	c.Assert(mc.pc, qt.Equals, 1)
}

func TestOperationStoreSyntaxCaseInput_Apply_NonSyntaxValue(t *testing.T) {
	c := qt.New(t)

	// Clear any previous state
	currentSyntaxCaseInput = nil

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	// Set a non-syntax value
	mc.SetValue(values.NewInteger(42))

	op := NewOperationStoreSyntaxCaseInput()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(currentSyntaxCaseInput, qt.IsNotNil)
}

// =============================================================================
// OperationClearSyntaxCaseInput Tests (operation_syntax_case.go)
// =============================================================================

func TestNewOperationClearSyntaxCaseInput(t *testing.T) {
	c := qt.New(t)

	op := NewOperationClearSyntaxCaseInput()
	c.Assert(op, qt.IsNotNil)
}

func TestOperationClearSyntaxCaseInput_String(t *testing.T) {
	c := qt.New(t)

	op := NewOperationClearSyntaxCaseInput()
	c.Assert(op.String(), qt.Equals, "ClearSyntaxCaseInput")
}

func TestOperationClearSyntaxCaseInput_SchemeString(t *testing.T) {
	c := qt.New(t)

	op := NewOperationClearSyntaxCaseInput()
	c.Assert(op.SchemeString(), qt.Equals, "#<operation:clear-syntax-case-input>")
}

func TestOperationClearSyntaxCaseInput_EqualTo(t *testing.T) {
	c := qt.New(t)

	op1 := NewOperationClearSyntaxCaseInput()
	op2 := NewOperationClearSyntaxCaseInput()

	c.Assert(op1.EqualTo(op2), qt.IsTrue)
	c.Assert(op1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestOperationClearSyntaxCaseInput_IsVoid(t *testing.T) {
	c := qt.New(t)

	op := NewOperationClearSyntaxCaseInput()
	c.Assert(op.IsVoid(), qt.IsFalse)
}

func TestOperationClearSyntaxCaseInput_Apply(t *testing.T) {
	c := qt.New(t)

	// Set up some input
	currentSyntaxCaseInput = syntax.NewSyntaxSymbol("test", nil)

	genv := environment.NewTopLevelGlobalEnvironmentFrame()
	env := environment.NewEnvironmentFrame(nil, genv)
	tpl := NewNativeTemplate(0, 0, false)
	mc := NewMachineContext(NewMachineContinuation(nil, tpl, env))

	op := NewOperationClearSyntaxCaseInput()
	result, err := op.Apply(context.Background(), mc)

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.IsNotNil)
	c.Assert(currentSyntaxCaseInput, qt.IsNil)
	c.Assert(mc.pc, qt.Equals, 1)
}

// =============================================================================
// CompileSyntax Tests (compile_syntax_form.go)
// =============================================================================

func TestCompileSyntax_SingleArg(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax foo) -> (foo)
	template := syntax.NewSyntaxSymbol("foo", nil)
	expr := syntax.NewSyntaxCons(template, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(len(tpl.operations) > 0, qt.IsTrue)
}

func TestCompileSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
}

func TestCompileSyntax_Error_TooManyArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (syntax foo bar) -> (foo bar)
	template := syntax.NewSyntaxSymbol("foo", nil)
	extra := syntax.NewSyntaxSymbol("bar", nil)
	expr := syntax.NewSyntaxCons(template,
		syntax.NewSyntaxCons(extra, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax")
}

func TestTemplateContainsEllipsis_NoEllipsis(t *testing.T) {
	c := qt.New(t)

	// Simple symbol
	stx := syntax.NewSyntaxSymbol("foo", nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsFalse)
}

func TestTemplateContainsEllipsis_WithEllipsis(t *testing.T) {
	c := qt.New(t)

	// Symbol named "..."
	stx := syntax.NewSyntaxSymbol("...", nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsTrue)
}

func TestTemplateContainsEllipsis_InList(t *testing.T) {
	c := qt.New(t)

	// (foo ...)
	ellipsis := syntax.NewSyntaxSymbol("...", nil)
	foo := syntax.NewSyntaxSymbol("foo", nil)
	list := syntax.NewSyntaxCons(foo,
		syntax.NewSyntaxCons(ellipsis, syntax.NewSyntaxEmptyList(nil), nil), nil)

	c.Assert(templateContainsEllipsis(list), qt.IsTrue)
}

func TestTemplateContainsEllipsis_EmptyList(t *testing.T) {
	c := qt.New(t)

	stx := syntax.NewSyntaxEmptyList(nil)
	c.Assert(templateContainsEllipsis(stx), qt.IsFalse)
}

// =============================================================================
// CompileQuasisyntax Tests (compile_quasisyntax.go)
// =============================================================================

func TestCompileQuasisyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileQuasisyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "quasisyntax")
}

func TestCompileUnsyntax_Error(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileUnsyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unsyntax")
}

func TestCompileUnsyntaxSplicing_Error(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileUnsyntaxSplicing(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unsyntax-splicing")
}

// =============================================================================
// CompileWithSyntax Tests (compile_with_syntax.go)
// =============================================================================

func TestCompileWithSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_Error_NoBody(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (()) - empty bindings list, no body
	bindings := syntax.NewSyntaxEmptyList(nil)
	expr := syntax.NewSyntaxCons(bindings, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "with-syntax")
}

func TestCompileWithSyntax_EmptyBindings(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (() body) - empty bindings, simple body
	bindings := syntax.NewSyntaxEmptyList(nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(bindings,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileWithSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
}

// =============================================================================
// CompileSyntaxCase Tests (compile_syntax_case.go)
// =============================================================================

func TestCompileSyntaxCase_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

func TestCompileSyntaxCase_Error_NoLiterals(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input) - missing literals and clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	expr := syntax.NewSyntaxCons(input, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

func TestCompileSyntaxCase_Error_NoClauses(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (input ()) - missing clauses
	input := syntax.NewSyntaxSymbol("x", nil)
	literals := syntax.NewSyntaxEmptyList(nil)
	expr := syntax.NewSyntaxCons(input,
		syntax.NewSyntaxCons(literals, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileSyntaxCase(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "syntax-case")
}

// =============================================================================
// CompileBeginForSyntax Tests (compile_begin_for_syntax.go)
// =============================================================================

func TestCompileBeginForSyntax_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, nil), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Error_NotPair(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Not a pair
	expr := syntax.NewSyntaxSymbol("bad", nil)

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "begin-for-syntax")
}

func TestCompileBeginForSyntax_Empty(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Use an empty SyntaxPair (which returns true for IsSyntaxEmptyList)
	exprPair := &syntax.SyntaxPair{}

	err := ccnt.CompileBeginForSyntax(NewCompileTimeCallContext(false, true, env), exprPair)
	c.Assert(err, qt.IsNil)
}

// =============================================================================
// CompileDefineForSyntax Tests (compile_define_for_syntax.go)
// =============================================================================

func TestCompileDefineForSyntax_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileDefineForSyntax(NewCompileTimeCallContext(false, true, nil), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "define-for-syntax")
}

func TestCompileDefineForSyntax_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileDefineForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "define-for-syntax")
}

func TestCompileDefineForSyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileDefineForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "define-for-syntax")
}

func TestCompileDefineForSyntax_Error_MissingExpression(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (name) - missing expression
	name := syntax.NewSyntaxSymbol("x", nil)
	expr := syntax.NewSyntaxCons(name, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileDefineForSyntax(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "define-for-syntax")
}

// =============================================================================
// CompileEvalWhen Tests (compile_eval_when.go)
// =============================================================================

func TestCompileEvalWhen_Error_NilEnv(t *testing.T) {
	c := qt.New(t)

	tpl := NewNativeTemplate(0, 0, false)
	ccnt := &CompileTimeContinuation{
		template: tpl,
		env:      nil,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, nil), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NilTemplate(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	ccnt := &CompileTimeContinuation{
		template: nil,
		env:      env,
	}

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "eval-when")
}

func TestCompileEvalWhen_EmptyBody(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((run)) - phases with empty body, should emit void
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	expr := syntax.NewSyntaxCons(phases, syntax.NewSyntaxEmptyList(nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil) // Empty body is valid, emits void
}

func TestCompileEvalWhen_Error_UnknownPhase(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((unknown) body)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("unknown", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unknown phase")
}

func TestCompileEvalWhen_RunPhase(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// ((run) 42)
	phases := syntax.NewSyntaxCons(
		syntax.NewSyntaxSymbol("run", nil),
		syntax.NewSyntaxEmptyList(nil), nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
	c.Assert(len(tpl.operations) > 0, qt.IsTrue)
}

func TestCompileEvalWhen_EmptyPhases(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironmentFrame())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// (() 42) - no phases, should emit void
	phases := syntax.NewSyntaxEmptyList(nil)
	body := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expr := syntax.NewSyntaxCons(phases,
		syntax.NewSyntaxCons(body, syntax.NewSyntaxEmptyList(nil), nil), nil)

	err := ccnt.CompileEvalWhen(NewCompileTimeCallContext(false, true, env), expr)
	c.Assert(err, qt.IsNil)
}
