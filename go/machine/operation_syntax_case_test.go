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
// syntaxCaseClause Tests
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
// OperationSyntaxCaseMatch Tests
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
// OperationBindPatternVars Tests
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
// OperationSyntaxCaseNoMatch Tests
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
// OperationSyntaxTemplateExpand Tests
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
// OperationStoreSyntaxCaseInput Tests
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
// OperationClearSyntaxCaseInput Tests
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
