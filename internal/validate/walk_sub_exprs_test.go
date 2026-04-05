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

package validate

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/internal/syntax"
)

type childEntry struct {
	expr ValidatedExpr
	role ChildRole
}

func collectChildren(expr ValidatedExpr) []childEntry {
	var result []childEntry
	WalkSubExprs(expr, func(child ValidatedExpr, role ChildRole) {
		result = append(result, childEntry{child, role})
	})
	return result
}

func TestWalkSubExprs_Call(t *testing.T) {
	c := qt.New(t)
	proc := symRef("f")
	arg1 := symRef("x")
	arg2 := lit()
	expr := call(proc, arg1, arg2)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	c.Assert(children[0].expr, qt.Equals, proc)
	c.Assert(children[0].role, qt.Equals, RoleCallProc)
	c.Assert(children[1].expr, qt.Equals, arg1)
	c.Assert(children[1].role, qt.Equals, RoleNormal)
	c.Assert(children[2].expr, qt.Equals, arg2)
	c.Assert(children[2].role, qt.Equals, RoleNormal)
}

func TestWalkSubExprs_Apply(t *testing.T) {
	c := qt.New(t)
	proc := symRef("f")
	prefix := symRef("x")
	final := lit()
	expr := applyExpr(proc, []ValidatedExpr{prefix}, final)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	c.Assert(children[0].role, qt.Equals, RoleCallProc)
	c.Assert(children[1].role, qt.Equals, RoleNormal)
	c.Assert(children[2].role, qt.Equals, RoleNormal)
}

func TestWalkSubExprs_Lambda(t *testing.T) {
	c := qt.New(t)
	b1 := lit()
	b2 := symRef("x")
	expr := lam(b1, b2)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 2)
	c.Assert(children[0].role, qt.Equals, RoleClosureBody)
	c.Assert(children[1].role, qt.Equals, RoleClosureBody)
}

func TestWalkSubExprs_CaseLambda(t *testing.T) {
	c := qt.New(t)
	b1 := symRef("x")
	expr := caseLam(b1)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 1)
	c.Assert(children[0].role, qt.Equals, RoleClosureBody)
}

func TestWalkSubExprs_If(t *testing.T) {
	c := qt.New(t)
	test := lit()
	conseq := symRef("x")
	alt := symRef("y")
	expr := &ValidatedIf{
		validatedBase: validatedBase{formName: "if"},
		Test:          test,
		Conseq:        conseq,
		Alt:           alt,
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.role, qt.Equals, RoleNormal)
	}
}

func TestWalkSubExprs_Begin(t *testing.T) {
	c := qt.New(t)
	b1 := lit()
	b2 := symRef("x")
	expr := &ValidatedBegin{
		validatedBase: validatedBase{formName: "begin"},
		body:          []ValidatedExpr{b1, b2},
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 2)
	for _, ch := range children {
		c.Assert(ch.role, qt.Equals, RoleNormal)
	}
}

func TestWalkSubExprs_SetBang(t *testing.T) {
	c := qt.New(t)
	val := symRef("x")
	expr := setBang("f", val)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 1)
	c.Assert(children[0].expr, qt.Equals, val)
	c.Assert(children[0].role, qt.Equals, RoleNormal)
}

func TestWalkSubExprs_Symbol(t *testing.T) {
	c := qt.New(t)
	children := collectChildren(symRef("x"))
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Literal(t *testing.T) {
	c := qt.New(t)
	children := collectChildren(lit())
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Quote(t *testing.T) {
	c := qt.New(t)
	expr := &ValidatedQuote{
		validatedBase: validatedBase{formName: "quote"},
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Quasiquote(t *testing.T) {
	c := qt.New(t)
	expr := &ValidatedQuasiquote{
		validatedBase: validatedBase{formName: "quasiquote"},
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 0)
}

func TestWalkSubExprs_Let(t *testing.T) {
	c := qt.New(t)
	init1 := lit()
	init2 := symRef("x")
	body1 := symRef("y")
	bindings := []ValidatedLetBinding{
		{Name: syntax.NewSyntaxSymbol("a", nil), Init: init1},
		{Name: syntax.NewSyntaxSymbol("b", nil), Init: init2},
	}
	expr := nestedLet(bindings, body1)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.role, qt.Equals, RoleNormal)
	}
}

func TestWalkSubExprs_DynamicWind(t *testing.T) {
	c := qt.New(t)
	expr := &ValidatedDynamicWind{
		validatedBase: validatedBase{formName: "dynamic-wind"},
		Before:        symRef("a"),
		Thunk:         symRef("b"),
		After:         symRef("c"),
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.role, qt.Equals, RoleNormal)
	}
}

func TestWalkSubExprs_WithContinuationMark(t *testing.T) {
	c := qt.New(t)
	expr := &ValidatedWithContinuationMark{
		validatedBase: validatedBase{formName: "with-continuation-mark"},
		Key:           lit(),
		Val:           lit(),
		Body:          symRef("x"),
	}
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 3)
	for _, ch := range children {
		c.Assert(ch.role, qt.Equals, RoleNormal)
	}
}

func TestWalkSubExprs_DefineFunction(t *testing.T) {
	c := qt.New(t)
	b1 := symRef("x")
	expr := defineFn("f", b1)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 1)
	c.Assert(children[0].expr, qt.Equals, b1)
	c.Assert(children[0].role, qt.Equals, RoleClosureBody)
}

func TestWalkSubExprs_DefineValue(t *testing.T) {
	c := qt.New(t)
	val := symRef("x")
	expr := defineVal("f", val)
	children := collectChildren(expr)
	c.Assert(len(children), qt.Equals, 1)
	c.Assert(children[0].expr, qt.Equals, val)
	c.Assert(children[0].role, qt.Equals, RoleNormal)
}

func TestWalkSubExprs_Nil(t *testing.T) {
	WalkSubExprs(nil, func(child ValidatedExpr, role ChildRole) {
		t.Fatal("should not be called")
	})
}
