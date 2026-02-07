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

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/syntax"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// Test helpers

func makeTestSyntaxSymbol(name string) *syntax.SyntaxSymbol {
	return syntax.NewSyntaxSymbol(name, nil)
}

func makeTestSyntaxList(elems ...syntax.SyntaxValue) *syntax.SyntaxPair {
	if len(elems) == 0 {
		return syntax.NewSyntaxEmptyList(nil)
	}
	result := syntax.NewSyntaxEmptyList(nil)
	for i := len(elems) - 1; i >= 0; i-- {
		result = syntax.NewSyntaxCons(elems[i], result, nil)
	}
	return result
}

func makeTestUnsyntax(expr syntax.SyntaxValue) *syntax.SyntaxPair {
	return makeTestSyntaxList(
		makeTestSyntaxSymbol("unsyntax"),
		expr,
	)
}

func makeTestUnsyntaxSplicing(expr syntax.SyntaxValue) *syntax.SyntaxPair {
	return makeTestSyntaxList(
		makeTestSyntaxSymbol("unsyntax-splicing"),
		expr,
	)
}

func makeTestQuasisyntax(body syntax.SyntaxValue) *syntax.SyntaxPair {
	return makeTestSyntaxList(
		makeTestSyntaxSymbol("quasisyntax"),
		body,
	)
}

func newTestCompiler() (*CompileTimeContinuation, *environment.EnvironmentFrame) {
	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)
	return ccnt, env
}

// Original error tests

func TestCompileQuasisyntax_Error_NoArgs(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	// Empty args
	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileQuasisyntax(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "quasisyntax")
}

func TestCompileUnsyntax_Error(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileUnsyntax(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unsyntax")
}

func TestCompileUnsyntaxSplicing_Error(t *testing.T) {
	c := qt.New(t)

	env := newTopLevelEnv(environment.NewTopLevelEnvironment().Runtime())
	tpl := NewNativeTemplate(0, 0, false)
	ccnt := NewCompiletimeContinuation(tpl, env)

	expr := syntax.NewSyntaxEmptyList(nil)

	err := ccnt.CompileUnsyntaxSplicing(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "unsyntax-splicing")
}

// Section 1: quasisyntaxNeedsRuntime tests

func TestQuasisyntaxNeedsRuntime_PureLiteral(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// Pure symbol - no unsyntax
	stx := makeTestSyntaxSymbol("foo")
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_PureList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// List with no special forms
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxSymbol("b"),
		makeTestSyntaxSymbol("c"),
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxAtDepth1(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,b) at depth 1 - has unsyntax
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntax(makeTestSyntaxSymbol("b")),
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,b) at depth 2 - unsyntax not at this depth
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntax(makeTestSyntaxSymbol("b")),
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxSplicing(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,@b) at depth 1
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("b")),
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)
}

func TestQuasisyntaxNeedsRuntime_NestedQuasisyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (quasisyntax (a #,b)) at depth 1
	// Inner unsyntax is at depth 2 relative to outer, so needs runtime
	innerBody := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntax(makeTestSyntaxSymbol("b")),
	)
	stx := makeTestQuasisyntax(innerBody)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)
}

func TestQuasisyntaxNeedsRuntime_EmptyList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := syntax.NewSyntaxEmptyList(nil)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_Number(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// Numbers are not runtime-dependent
	stx := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsFalse)
}

// Section 2: expandQuasisyntax tests

func TestExpandQuasisyntax_SimpleSymbol(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// foo -> (syntax foo)
	stx := makeTestSyntaxSymbol("foo")
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be a list starting with 'syntax'
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

func TestExpandQuasisyntax_SimpleList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a b) -> (list (syntax a) (syntax b))
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxSymbol("b"),
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be (list ...)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntax_UnsyntaxAtDepth1(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,expr) at depth 1 -> (list (syntax a) expr)
	exprSym := makeTestSyntaxSymbol("expr")
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntax(exprSym),
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be (list (syntax a) expr)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)

	// Check first element is 'list'
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntax_UnsyntaxAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// #,x at depth 2 becomes literal (list (syntax unsyntax) (syntax x))
	exprSym := makeTestSyntaxSymbol("x")
	stx := makeTestUnsyntax(exprSym)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 2)

	// Should produce (list (syntax unsyntax) (syntax x))
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntax_EmptyList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// () -> (syntax ())
	stx := syntax.NewSyntaxEmptyList(nil)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

func TestExpandQuasisyntax_NestedQuasisyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (quasisyntax #,x) at depth 1 -> (list (syntax quasisyntax) x)
	innerBody := makeTestUnsyntax(makeTestSyntaxSymbol("x"))
	stx := makeTestQuasisyntax(innerBody)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntax_NestedList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a (b c)) -> (list (syntax a) (list (syntax b) (syntax c)))
	inner := makeTestSyntaxList(
		makeTestSyntaxSymbol("b"),
		makeTestSyntaxSymbol("c"),
	)
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		inner,
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be (list ...)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")

	// Check second element is also a list form (the nested (b c))
	cdr := pair.SyntaxCdr().(*syntax.SyntaxPair)
	cdr = cdr.SyntaxCdr().(*syntax.SyntaxPair) // skip (syntax a)
	secondElem := cdr.SyntaxCar()
	secondPair, ok := secondElem.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	secondCar := secondPair.SyntaxCar()
	secondCarSym, ok := secondCar.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(secondCarSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntax_UnsyntaxSplicing(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,@xs b) -> delegates to expandQuasisyntaxList which produces append form
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
		makeTestSyntaxSymbol("b"),
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should produce append form (handled by expandQuasisyntaxList)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntax_MultipleSplice(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (#,@a #,@b) -> (append a b)
	stx := makeTestSyntaxList(
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("a")),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("b")),
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be (append a b)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")

	// Check we have two args: a and b
	cdr := pair.SyntaxCdr().(*syntax.SyntaxPair)
	firstArg := cdr.SyntaxCar()
	firstArgSym, ok := firstArg.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(firstArgSym.Sym.Key, qt.Equals, "a")

	cdr = cdr.SyntaxCdr().(*syntax.SyntaxPair)
	secondArg := cdr.SyntaxCar()
	secondArgSym, ok := secondArg.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(secondArgSym.Sym.Key, qt.Equals, "b")
}

func TestExpandQuasisyntax_MixedSplice(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (x #,@ys z) -> (append (list (syntax x)) ys (list (syntax z)))
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("x"),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("ys")),
		makeTestSyntaxSymbol("z"),
	)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	// Should be (append ...)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")

	// Check structure: (append (list ...) ys (list ...))
	cdr := pair.SyntaxCdr().(*syntax.SyntaxPair)

	// First arg should be (list (syntax x))
	firstArg := cdr.SyntaxCar()
	firstArgPair, ok := firstArg.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	firstArgCar := firstArgPair.SyntaxCar()
	firstArgCarSym, ok := firstArgCar.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(firstArgCarSym.Sym.Key, qt.Equals, "list")

	// Second arg should be ys (the splice expr)
	cdr = cdr.SyntaxCdr().(*syntax.SyntaxPair)
	secondArg := cdr.SyntaxCar()
	secondArgSym, ok := secondArg.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(secondArgSym.Sym.Key, qt.Equals, "ys")

	// Third arg should be (list (syntax z))
	cdr = cdr.SyntaxCdr().(*syntax.SyntaxPair)
	thirdArg := cdr.SyntaxCar()
	thirdArgPair, ok := thirdArg.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	thirdArgCar := thirdArgPair.SyntaxCar()
	thirdArgCarSym, ok := thirdArgCar.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(thirdArgCarSym.Sym.Key, qt.Equals, "list")
}

// Section 2b: Additional expandQuasisyntax tests for uncovered paths

// expandQuasisyntax: SyntaxObject (non-symbol, non-pair) default case
func TestExpandQuasisyntax_SyntaxObject(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// A number wrapped in SyntaxObject -> (syntax 42)
	stx := syntax.NewSyntaxObject(values.NewInteger(42), nil)
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// expandQuasisyntax: unsyntax with wrong arg count at depth 1
func TestExpandQuasisyntax_UnsyntaxWrongArgCountDepth1(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (unsyntax) at depth 1 - Length != 2
	// Falls through both Length==2 checks, wraps in (syntax ...)
	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// expandQuasisyntax: unsyntax at depth > 1 with wrong arg count
func TestExpandQuasisyntax_UnsyntaxWrongArgCountDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (unsyntax) at depth 2 - Length != 2, wraps in (syntax ...)
	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 2)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// expandQuasisyntax: unsyntax-splicing at depth > 1
func TestExpandQuasisyntax_UnsyntaxSplicingAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (unsyntax-splicing x) at depth 2 -> (list (syntax unsyntax-splicing) (syntax x))
	stx := makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("x"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 2)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")

	// Second element should be (syntax unsyntax-splicing)
	cdr := pair.SyntaxCdr().(*syntax.SyntaxPair)
	secondArg := cdr.SyntaxCar()
	secondArgPair, ok := secondArg.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	innerCar := secondArgPair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(innerCar.Sym.Key, qt.Equals, "syntax")
	innerCdr := secondArgPair.SyntaxCdr().(*syntax.SyntaxPair)
	innerSym := innerCdr.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(innerSym.Sym.Key, qt.Equals, "unsyntax-splicing")
}

// expandQuasisyntax: unsyntax-splicing at depth 1 with wrong arg count
func TestExpandQuasisyntax_UnsyntaxSplicingWrongArgCount(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (unsyntax-splicing) at depth 1 - Length == 1, not 2
	// depth > 1 check fails, falls to (syntax ...) wrapping
	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax-splicing"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// expandQuasisyntax: unsyntax-splicing at depth > 1 with wrong arg count
func TestExpandQuasisyntax_UnsyntaxSplicingWrongArgCountDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (unsyntax-splicing) at depth 2 - Length != 2, wraps in (syntax ...)
	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax-splicing"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 2)

	// At depth > 1 but Length != 2, falls through to (syntax ...)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// expandQuasisyntax: quasisyntax with wrong arg count
func TestExpandQuasisyntax_QuasisyntaxWrongArgCount(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (quasisyntax) - no arg, wraps in (syntax ...)
	stx := makeTestSyntaxList(makeTestSyntaxSymbol("quasisyntax"))
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "syntax")
}

// Section 3: expandQuasisyntaxList tests

func TestExpandQuasisyntaxList_SimpleList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a b c) -> (list ...)
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxSymbol("b"),
		makeTestSyntaxSymbol("c"),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntaxList_WithSplice(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a #,@xs b) -> (append (list (syntax a)) xs (list (syntax b)))
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
		makeTestSyntaxSymbol("b"),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntaxList_OnlySplice(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (#,@xs) -> (append xs)
	stx := makeTestSyntaxList(
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntaxList_MultipleSplices(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (#,@a #,@b #,@c) -> (append a b c)
	stx := makeTestSyntaxList(
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("a")),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("b")),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("c")),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntaxList_ImproperList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (a . b) -> (list* (syntax a) (syntax b))
	// Create improper list manually: (a . b)
	stx := syntax.NewSyntaxCons(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxSymbol("b"), // cdr is not a list - makes it improper
		nil,
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	// Should be (list* ...)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list*")

	// Verify we have two elements: (syntax a) and (syntax b)
	cdr := pair.SyntaxCdr().(*syntax.SyntaxPair)
	firstArg := cdr.SyntaxCar()
	firstArgPair, ok := firstArg.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	firstArgCar := firstArgPair.SyntaxCar()
	firstArgCarSym, ok := firstArgCar.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(firstArgCarSym.Sym.Key, qt.Equals, "syntax")

	cdr = cdr.SyntaxCdr().(*syntax.SyntaxPair)
	secondArg := cdr.SyntaxCar()
	secondArgPair, ok := secondArg.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	secondArgCar := secondArgPair.SyntaxCar()
	secondArgCarSym, ok := secondArgCar.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(secondArgCarSym.Sym.Key, qt.Equals, "syntax")
}

// Section 4: compileQuasisyntaxTemplate tests

func TestCompileQuasisyntaxTemplate_PureLiteral(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	// Pure literal should emit only LoadLiteral
	stx := makeTestSyntaxSymbol("foo")
	ctctx := NewCompileTimeCallContext(context.Background(), false, true, env)

	err := ccnt.compileQuasisyntaxTemplate(ctctx, stx, 1)
	c.Assert(err, qt.IsNil)

	// Check that only one operation was emitted (LoadLiteral)
	ops := ccnt.template.Operations()
	c.Assert(len(ops), qt.Equals, 1)
	_, ok := ops[0].(*OperationLoadLiteralByLiteralIndexImmediate)
	c.Assert(ok, qt.IsTrue)
}

func TestCompileQuasisyntaxTemplate_WithUnsyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// With unsyntax should need runtime (not pure literal)
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestUnsyntax(makeTestSyntaxSymbol("b")),
	)

	// Verify it needs runtime evaluation
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)

	// Verify expansion produces list form
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestCompileQuasisyntaxTemplate_NestedPure(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	// Nested but no unsyntax -> still pure literal
	stx := makeTestSyntaxList(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxList(
			makeTestSyntaxSymbol("b"),
			makeTestSyntaxSymbol("c"),
		),
	)
	ctctx := NewCompileTimeCallContext(context.Background(), false, true, env)

	err := ccnt.compileQuasisyntaxTemplate(ctctx, stx, 1)
	c.Assert(err, qt.IsNil)

	// Should be single LoadLiteral
	ops := ccnt.template.Operations()
	c.Assert(len(ops), qt.Equals, 1)
	_, ok := ops[0].(*OperationLoadLiteralByLiteralIndexImmediate)
	c.Assert(ok, qt.IsTrue)
}

func TestCompileQuasisyntaxTemplate_NestedWithUnsyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	// (quasisyntax #,x) at depth 1 needs runtime
	innerBody := makeTestUnsyntax(makeTestSyntaxSymbol("x"))
	stx := makeTestQuasisyntax(innerBody)

	// Verify it needs runtime (nested quasisyntax at depth 1)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)

	// Verify expansion produces list form with syntax quasisyntax
	expanded := ccnt.expandQuasisyntax(context.Background(), stx, 1)
	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	car := pair.SyntaxCar()
	carSym, ok := car.(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

// Section 5: Additional coverage for CompileQuasisyntax entry point

func TestCompileQuasisyntax_Error_NonPairArg(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	expr := makeTestSyntaxSymbol("foo")
	err := ccnt.CompileQuasisyntax(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "quasisyntax")
}

func TestCompileQuasisyntax_Error_ExtraArgs(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	expr := makeTestSyntaxList(
		makeTestSyntaxSymbol("template"),
		makeTestSyntaxSymbol("extra"),
	)
	err := ccnt.CompileQuasisyntax(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "quasisyntax")
}

func TestCompileQuasisyntax_Error_ImproperArgsList(t *testing.T) {
	c := qt.New(t)
	ccnt, env := newTestCompiler()

	expr := syntax.NewSyntaxCons(
		makeTestSyntaxSymbol("template"),
		makeTestSyntaxSymbol("extra"),
		nil,
	)
	err := ccnt.CompileQuasisyntax(NewCompileTimeCallContext(context.Background(), false, true, env), expr)
	c.Assert(err, qt.IsNotNil)
	c.Assert(err.Error(), qt.Contains, "quasisyntax")
}

// Section 6: Additional quasisyntaxNeedsRuntime coverage

func TestQuasisyntaxNeedsRuntime_UnsyntaxWrongArgCount(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax"))
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxSplicingAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("x"))
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxSplicingAtDepth2WithNestedUnsyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	inner := makeTestUnsyntax(makeTestSyntaxSymbol("y"))
	stx := makeTestUnsyntaxSplicing(inner)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsTrue)
}

func TestQuasisyntaxNeedsRuntime_UnsyntaxSplicingWrongArgCount(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(makeTestSyntaxSymbol("unsyntax-splicing"))
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_QuasisyntaxAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestQuasisyntax(makeTestSyntaxSymbol("x"))
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_QuasisyntaxWrongArgCount(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(makeTestSyntaxSymbol("quasisyntax"))
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 2)
	c.Assert(needs, qt.IsFalse)
}

func TestQuasisyntaxNeedsRuntime_DeepNestedUnsyntaxInCar(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(
		makeTestUnsyntax(makeTestSyntaxSymbol("x")),
		makeTestSyntaxSymbol("b"),
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsTrue)
}

func TestQuasisyntaxNeedsRuntime_ImproperList(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := syntax.NewSyntaxCons(
		makeTestSyntaxSymbol("a"),
		makeTestSyntaxSymbol("b"),
		nil,
	)
	needs := ccnt.quasisyntaxNeedsRuntime(stx, 1)
	c.Assert(needs, qt.IsFalse)
}

// Section 7: Additional expandQuasisyntaxList coverage

func TestExpandQuasisyntaxList_NonPairInSpliceContext(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(
		syntax.NewSyntaxObject(values.NewInteger(42), nil),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntaxList_SpliceAtDepth2(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 2)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list")
}

func TestExpandQuasisyntaxList_ImproperWithUnsyntax(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := syntax.NewSyntaxCons(
		makeTestUnsyntax(makeTestSyntaxSymbol("x")),
		makeTestSyntaxSymbol("y"),
		nil,
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "list*")
}

func TestExpandQuasisyntaxList_ImproperWithSplice(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := syntax.NewSyntaxCons(
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
		makeTestSyntaxSymbol("y"),
		nil,
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}

func TestExpandQuasisyntaxList_SpliceWithNonSplicePairCar(t *testing.T) {
	c := qt.New(t)
	ccnt, _ := newTestCompiler()

	stx := makeTestSyntaxList(
		makeTestSyntaxList(
			makeTestSyntaxSymbol("a"),
			makeTestSyntaxSymbol("b"),
		),
		makeTestUnsyntaxSplicing(makeTestSyntaxSymbol("xs")),
	)
	expanded := ccnt.expandQuasisyntaxList(context.Background(), stx, 1)

	pair, ok := expanded.(*syntax.SyntaxPair)
	c.Assert(ok, qt.IsTrue)
	carSym, ok := pair.SyntaxCar().(*syntax.SyntaxSymbol)
	c.Assert(ok, qt.IsTrue)
	c.Assert(carSym.Sym.Key, qt.Equals, "append")
}
