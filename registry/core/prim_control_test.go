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

package core_test

import (
	"context"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/internal/bootstrap"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

func TestCallCC(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "call/cc with normal return",
			// (call/cc (lambda (k) 42))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.NewInteger(42))),
			out: values.NewInteger(42),
		},
		{
			name: "call/cc with escape",
			// (call/cc (lambda (k) (k 42)))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(values.NewSymbol("k"), values.NewInteger(42)))),
			out: values.NewInteger(42),
		},
		{
			name: "call/cc escape skips remaining computation",
			// (+ 1 (call/cc (lambda (k) (+ 2 (k 10)))))
			prog: values.List(values.NewSymbol("+"),
				values.NewInteger(1),
				values.List(values.NewSymbol("call/cc"),
					values.List(values.NewSymbol("lambda"),
						values.List(values.NewSymbol("k")),
						values.List(values.NewSymbol("+"),
							values.NewInteger(2),
							values.List(values.NewSymbol("k"), values.NewInteger(10)))))),
			out: values.NewInteger(11), // 1 + 10, not 1 + 2 + 10
		},
		{
			name: "call/cc normal return continues computation",
			// (+ 1 (call/cc (lambda (k) 10)))
			prog: values.List(values.NewSymbol("+"),
				values.NewInteger(1),
				values.List(values.NewSymbol("call/cc"),
					values.List(values.NewSymbol("lambda"),
						values.List(values.NewSymbol("k")),
						values.NewInteger(10)))),
			out: values.NewInteger(11), // 1 + 10
		},
		{
			name: "call-with-current-continuation alias",
			// (call-with-current-continuation (lambda (k) (k 99)))
			prog: values.List(values.NewSymbol("call-with-current-continuation"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(values.NewSymbol("k"), values.NewInteger(99)))),
			out: values.NewInteger(99),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestCallCCWithHigherOrderFunctions(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "call/cc escape from apply",
			// (call/cc (lambda (return) (apply (lambda (a b) (if (> b 5) (return 'big) (+ a b))) '(1 10))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("apply"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("a"), values.NewSymbol("b")),
							values.List(values.NewSymbol("if"),
								values.List(values.NewSymbol(">"), values.NewSymbol("b"), values.NewInteger(5)),
								values.List(values.NewSymbol("return"),
									values.List(values.NewSymbol("quote"), values.NewSymbol("big"))),
								values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b")))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(10)))))),
			out: values.NewSymbol("big"),
		},
		{
			name: "call/cc escape from map",
			// (call/cc (lambda (return) (map (lambda (x) (if (> x 3) (return 'found) (* x x))) '(1 2 3 4 5))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("map"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("x")),
							values.List(values.NewSymbol("if"),
								values.List(values.NewSymbol(">"), values.NewSymbol("x"), values.NewInteger(3)),
								values.List(values.NewSymbol("return"),
									values.List(values.NewSymbol("quote"), values.NewSymbol("found"))),
								values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewSymbol("x")))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3), values.NewInteger(4), values.NewInteger(5)))))),
			out: values.NewSymbol("found"),
		},
		{
			name: "call/cc no escape from map returns list",
			// (call/cc (lambda (return) (map (lambda (x) (* x x)) '(1 2 3))))
			prog: values.List(values.NewSymbol("call/cc"),
				values.List(values.NewSymbol("lambda"),
					values.List(values.NewSymbol("return")),
					values.List(values.NewSymbol("map"),
						values.List(values.NewSymbol("lambda"),
							values.List(values.NewSymbol("x")),
							values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewSymbol("x"))),
						values.List(values.NewSymbol("quote"),
							values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)))))),
			out: values.List(values.NewInteger(1), values.NewInteger(4), values.NewInteger(9)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMapMultipleLists(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single list",
			// (map (lambda (x) (* x 2)) '(1 2 3))
			prog: values.List(
				values.NewSymbol("map"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewInteger(2)),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.List(values.NewInteger(2), values.NewInteger(4), values.NewInteger(6)),
		},
		{
			name: "two lists with +",
			// (map + '(1 2 3) '(10 20 30))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
			),
			out: values.List(values.NewInteger(11), values.NewInteger(22), values.NewInteger(33)),
		},
		{
			name: "three lists",
			// (map (lambda (a b c) (+ a b c)) '(1 2 3) '(10 20 30) '(100 200 300))
			prog: values.List(
				values.NewSymbol("map"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
					values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(100), values.NewInteger(200), values.NewInteger(300))),
			),
			out: values.List(values.NewInteger(111), values.NewInteger(222), values.NewInteger(333)),
		},
		{
			name: "map list constructor",
			// (map list '(a b c) '(1 2 3))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("list"),
				values.List(values.NewSymbol("quote"), values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c"))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.List(
				values.List(values.NewSymbol("a"), values.NewInteger(1)),
				values.List(values.NewSymbol("b"), values.NewInteger(2)),
				values.List(values.NewSymbol("c"), values.NewInteger(3)),
			),
		},
		{
			name: "empty lists",
			// (map + '() '())
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
			out: values.EmptyList,
		},
		{
			name: "unequal length lists (stops at shortest)",
			// (map + '(1 2 3) '(10 20))
			prog: values.List(
				values.NewSymbol("map"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20))),
			),
			out: values.List(values.NewInteger(11), values.NewInteger(22)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestForEachMultipleLists(t *testing.T) {
	// for-each returns void, so we test that it doesn't error
	tcs := []struct {
		name string
		prog values.Value
	}{
		{
			name: "single list",
			// (for-each (lambda (x) x) '(1 2 3))
			prog: values.List(
				values.NewSymbol("for-each"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.NewSymbol("x"),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
		},
		{
			name: "two lists",
			// (for-each (lambda (x y) (+ x y)) '(1 2 3) '(10 20 30))
			prog: values.List(
				values.NewSymbol("for-each"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x"), values.NewSymbol("y")),
					values.List(values.NewSymbol("+"), values.NewSymbol("x"), values.NewSymbol("y")),
				),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(10), values.NewInteger(20), values.NewInteger(30))),
			),
		},
		{
			name: "empty lists",
			// (for-each + '() '())
			prog: values.List(
				values.NewSymbol("for-each"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.EmptyList),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

func TestApplyMultipleArgs(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "simple apply",
			// (apply + '(1 2 3))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3))),
			),
			out: values.NewInteger(6),
		},
		{
			name: "apply with prefix args",
			// (apply + 1 2 '(3 4 5))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(3), values.NewInteger(4), values.NewInteger(5))),
			),
			out: values.NewInteger(15),
		},
		{
			name: "apply with many prefix args",
			// (apply + 1 2 3 4 '(5 6))
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
				values.NewInteger(4),
				values.List(values.NewSymbol("quote"), values.List(values.NewInteger(5), values.NewInteger(6))),
			),
			out: values.NewInteger(21),
		},
		{
			name: "apply with empty final list",
			// (apply + 1 2 3 '())
			prog: values.List(
				values.NewSymbol("apply"),
				values.NewSymbol("+"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
				values.List(values.NewSymbol("quote"), values.EmptyList),
			),
			out: values.NewInteger(6),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestValues(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single value",
			// (values 42)
			prog: values.List(
				values.NewSymbol("values"),
				values.NewInteger(42),
			),
			out: values.NewInteger(42),
		},
		{
			name: "multiple values returns first",
			// (values 1 2 3) - GetValue() returns first value
			prog: values.List(
				values.NewSymbol("values"),
				values.NewInteger(1),
				values.NewInteger(2),
				values.NewInteger(3),
			),
			out: values.NewInteger(1),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestCallWithValues(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "single value producer",
			// (call-with-values (lambda () 42) (lambda (x) (* x 2)))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.NewInteger(42),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("x")),
					values.List(values.NewSymbol("*"), values.NewSymbol("x"), values.NewInteger(2)),
				),
			),
			out: values.NewInteger(84),
		},
		{
			name: "multiple values",
			// (call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("values"), values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
					values.List(values.NewSymbol("+"), values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
				),
			),
			out: values.NewInteger(6),
		},
		{
			name: "consumer builds list",
			// (call-with-values (lambda () (values 'a 'b 'c)) list)
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(
						values.NewSymbol("values"),
						values.List(values.NewSymbol("quote"), values.NewSymbol("a")),
						values.List(values.NewSymbol("quote"), values.NewSymbol("b")),
						values.List(values.NewSymbol("quote"), values.NewSymbol("c")),
					),
				),
				values.NewSymbol("list"),
			),
			out: values.List(values.NewSymbol("a"), values.NewSymbol("b"), values.NewSymbol("c")),
		},
		{
			name: "no values producer",
			// (call-with-values (lambda () (values)) (lambda () 'done))
			prog: values.List(
				values.NewSymbol("call-with-values"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("values")),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("done")),
				),
			),
			out: values.NewSymbol("done"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestDynamicWind(t *testing.T) {
	tcs := []struct {
		name string
		prog values.Value
		out  values.Value
	}{
		{
			name: "basic - returns thunk result",
			// (dynamic-wind (lambda () 'before) (lambda () 42) (lambda () 'after))
			prog: values.List(
				values.NewSymbol("dynamic-wind"),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("before")),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.NewInteger(42),
				),
				values.List(
					values.NewSymbol("lambda"),
					values.EmptyList,
					values.List(values.NewSymbol("quote"), values.NewSymbol("after")),
				),
			),
			out: values.NewInteger(42),
		},
		{
			name: "before runs first",
			// ((lambda (v)
			//   (dynamic-wind
			//     (lambda () (vector-set! v 0 1))
			//     (lambda () (vector-ref v 0))
			//     (lambda () (vector-set! v 0 2))))
			//  (make-vector 1 0))
			prog: values.List(
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("v")),
					values.List(
						values.NewSymbol("dynamic-wind"),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-set!"), values.NewSymbol("v"), values.NewInteger(0), values.NewInteger(1)),
						),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-ref"), values.NewSymbol("v"), values.NewInteger(0)),
						),
						values.List(
							values.NewSymbol("lambda"),
							values.EmptyList,
							values.List(values.NewSymbol("vector-set!"), values.NewSymbol("v"), values.NewInteger(0), values.NewInteger(2)),
						),
					),
				),
				values.List(values.NewSymbol("make-vector"), values.NewInteger(1), values.NewInteger(0)),
			),
			out: values.NewInteger(1), // thunk sees value set by before
		},
		{
			name: "escape returns correct value",
			// (call/cc (lambda (k) (dynamic-wind (lambda () #f) (lambda () (k 77)) (lambda () #f))))
			prog: values.List(
				values.NewSymbol("call/cc"),
				values.List(
					values.NewSymbol("lambda"),
					values.List(values.NewSymbol("k")),
					values.List(
						values.NewSymbol("dynamic-wind"),
						values.List(values.NewSymbol("lambda"), values.EmptyList, values.FalseValue),
						values.List(values.NewSymbol("lambda"), values.EmptyList,
							values.List(values.NewSymbol("k"), values.NewInteger(77))),
						values.List(values.NewSymbol("lambda"), values.EmptyList, values.FalseValue),
					),
				),
			),
			out: values.NewInteger(77),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

// TestDynamicWindEscape tests that after is called on continuation escape.
// This test uses runSchemeCodeWithEnv to load bootstrap macros for 'let'.
func TestDynamicWindEscape(t *testing.T) {
	env, err := bootstrap.NewTopLevelEnvironmentFrameTiny(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	// Parse and run:
	// (let ((v (make-vector 1 0)))
	//   (call/cc (lambda (k)
	//     (dynamic-wind
	//       (lambda () (vector-set! v 0 1))
	//       (lambda () (k 99))
	//       (lambda () (vector-set! v 0 2)))))
	//   (vector-ref v 0))
	prog := `(let ((v (make-vector 1 0)))
		(call/cc (lambda (k)
			(dynamic-wind
				(lambda () (vector-set! v 0 1))
				(lambda () (k 99))
				(lambda () (vector-set! v 0 2)))))
		(vector-ref v 0))`

	p := parser.NewParser(env, true, strings.NewReader(prog))
	stx, err := p.ReadSyntax(context.TODO())
	qt.Assert(t, err, qt.IsNil)

	ectx := context.Background()
	expanded, err := machine.NewExpanderTimeContinuation(ectx, env).ExpandExpression(stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(context.Background(), false, true)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	err = mc.RunWithEscapeHandling()
	qt.Assert(t, err, qt.IsNil)

	// After should have run, setting v[0] to 2
	qt.Assert(t, mc.GetValue(), valuestest.SchemeEquals, values.NewInteger(2))
}

func TestDynamicWindBasic(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, `
		(let ((result '()))
			(dynamic-wind
				(lambda () (set! result (cons 'before result)))
				(lambda () (set! result (cons 'during result)) 42)
				(lambda () (set! result (cons 'after result))))
			result)
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, qt.IsNotNil)
}

func TestCallWithValuesExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "call-with-values single value",
			Code:     `(call-with-values (lambda () 42) (lambda (x) x))`,
			Expected: values.NewInteger(42),
		},
		{
			Name:     "call-with-values two values",
			Code:     `(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))`,
			Expected: values.NewInteger(3),
		},
		{
			Name:     "call-with-values returning no values",
			Code:     `(call-with-values (lambda () (values)) (lambda () 'done))`,
			Expected: values.NewSymbol("done"),
		},
		{
			Name:     "call-with-values returning three values",
			Code:     `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))`,
			Expected: values.NewInteger(6),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallCCMultiInvoke(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "continuation invoked after call/cc returns",
			Code: `(let ((k-saved #f))
				(let ((result (call/cc (lambda (k) (set! k-saved k) 1))))
					(if (= result 1)
						(k-saved 2)
						result)))`,
			Expected: values.NewInteger(2),
		},
		{
			Name: "continuation invoked multiple times",
			Code: `(let ((k-saved #f) (count 0))
				(let ((result (call/cc (lambda (k) (set! k-saved k) 'first))))
					(set! count (+ count 1))
					(if (< count 3)
						(k-saved count)
						count)))`,
			Expected: values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// TestCallCCSubContextReentry tests that continuations captured inside
// Go-implemented primitives (map, for-each, call-with-values) that use
// NewSubContext() can be re-entered and resume the full computation,
// including the primitive's iteration loop and the outer continuation.
//
// Currently these tests FAIL because the Go for-loop in PrimMap/PrimForEach
// is not part of the captured Scheme continuation. When a continuation captured
// inside map's callback is re-invoked, only the callback body re-executes —
// the map iteration and outer computation are lost.
//
// Fixing this requires either:
//   - Implementing map/for-each in Scheme (so iteration is Scheme frames)
//   - Adding delimited continuations (shift/reset or prompts)
//   - CPS-transforming the Go primitives to save iteration state
//
// R7RS §6.10: "call-with-current-continuation packages the current
// continuation as an escape procedure." The "current continuation" at
// a call/cc inside map's callback includes the rest of the map iteration
// and the outer computation.
func TestCallCCSubContextReentry(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			// Re-entering a continuation captured inside map should
			// resume the map iteration from that element onward.
			Name: "map continuation re-entry resumes iteration",
			Code: `(let ((k #f))
				(let ((result
					(map (lambda (x)
						(if (and (= x 2) (not k))
							(call/cc (lambda (c) (set! k c) 200))
							(* x 10)))
						(list 1 2 3))))
					(if k
						(let ((saved-k k))
							(set! k #f)
							(saved-k 999))
						result)))`,
			// When re-entered with 999: map callback for x=2 returns 999,
			// map continues to x=3 producing 30, map returns (10 999 30).
			// Outer let rebinds result, k is now #f, returns result.
			Expected: values.List(
				values.NewInteger(10),
				values.NewInteger(999),
				values.NewInteger(30)),
		},
		{
			// Re-entering a continuation captured inside for-each should
			// resume the for-each iteration for remaining elements.
			Name: "for-each continuation re-entry resumes iteration",
			Code: `(let ((k #f) (count 0))
				(for-each (lambda (x)
					(set! count (+ count 1))
					(if (and (= x 2) (not k))
						(call/cc (lambda (c) (set! k c)))))
					(list 1 2 3))
				(if (and k (< count 5))
					(let ((saved-k k))
						(set! k #f)
						(saved-k #f))
					count))`,
			// First pass: count goes 1, 2 (captures k here), 3.
			// Re-entry at call/cc in x=2: (set! count) already ran, count=3.
			// call/cc returns #f, k is not captured again (k is truthy).
			// for-each continues to x=3: count becomes 4.
			// k was cleared, returns count = 4.
			Expected: values.NewInteger(4),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.Code, 5*time.Second)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestDynamicWindExceptionInThunks(t *testing.T) {
	errorTcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "exception in before thunk", Code: `(dynamic-wind (lambda () (error "before-fail")) (lambda () 1) (lambda () 2))`},
		{Name: "exception in after thunk", Code: `(dynamic-wind (lambda () #f) (lambda () 42) (lambda () (error "after-fail")))`},
	}
	for _, tc := range errorTcs {
		t.Run(tc.Name, func(t *testing.T) {
			_, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNotNil)
		})
	}

	successTcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "after thunk runs on body exception",
			Code: `(let ((after-ran #f))
				(guard (e (#t after-ran))
					(dynamic-wind
						(lambda () #f)
						(lambda () (error "body-fail"))
						(lambda () (set! after-ran #t)))))`,
			Expected: values.TrueValue,
		},
	}
	for _, tc := range successTcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestApplyExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name:     "apply with prefix args",
			Code:     `(apply + 1 2 '(3 4))`,
			Expected: values.NewInteger(10),
		},
		{
			Name:     "apply with empty list",
			Code:     `(apply + '())`,
			Expected: values.NewInteger(0),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestApply_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "improper list as final argument", Code: `(apply + '(1 . 2))`},
		{Name: "non-list as final argument", Code: `(apply + 42)`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestMap_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "exception in mapped procedure", Code: `(map (lambda (x) (if (= x 2) (error "boom") x)) '(1 2 3))`},
		{Name: "improper list argument", Code: `(map (lambda (x) x) '(1 . 2))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

func TestForEachExtended(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{
			Name: "unequal length lists stops at shortest",
			Code: `(let ((count 0))
				(for-each (lambda (x y) (set! count (+ count 1))) '(1 2 3) '(10 20))
				count)`,
			Expected: values.NewInteger(2),
		},
		{
			Name:     "return value is void",
			Code:     `(for-each (lambda (x) x) '(1 2 3))`,
			Expected: values.Void,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

func TestCallWithValues_Errors(t *testing.T) {
	tcs := []testhelpers.SchemeCodeErrorTestCase{
		{Name: "exception in producer", Code: `(call-with-values (lambda () (error "fail")) (lambda (x) x))`},
		{Name: "exception in consumer", Code: `(call-with-values (lambda () 42) (lambda (x) (error "fail")))`},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			testhelpers.RunSchemeCodeExpectError(t, tc.Code)
		})
	}
}

// TestCallCCCoroutines tests cooperative coroutines built on call/cc.
// This is a regression test for a bug where continuations captured inside
// nested call/cc sub-contexts had truncated continuation chains, causing
// top-level forms after (scheduler-run) to be silently dropped.
func TestCallCCCoroutines(t *testing.T) {
	tcs := []struct {
		name    string
		code    string
		check   func(t *testing.T, result values.Value)
		timeout time.Duration
	}{
		{
			name: "yield resumes after scheduler-run",
			// The primary regression: forms after (scheduler-run) must execute.
			code: `(let ((p (open-output-string)))
				(define *queue* '())
				(define (enqueue! thunk) (set! *queue* (append *queue* (list thunk))))
				(define (dequeue!) (let ((n (car *queue*))) (set! *queue* (cdr *queue*)) n))
				(define (scheduler-run) (if (not (null? *queue*)) ((dequeue!))))
				(define (spawn thunk) (enqueue! (lambda () (thunk) (scheduler-run))))
				(define (yield) (call/cc (lambda (k) (enqueue! (lambda () (k #f))) (scheduler-run))))

				(spawn (lambda () (display "A1 " p) (yield) (display "A2 " p)))
				(spawn (lambda () (display "B1 " p) (yield) (display "B2 " p)))

				(scheduler-run)
				(display "done" p)
				(get-output-string p))`,
			check: func(t *testing.T, result values.Value) {
				t.Helper()
				s, ok := result.(*values.String)
				qt.Assert(t, ok, qt.IsTrue)
				// "done" must appear — this is the regression condition.
				qt.Assert(t, strings.Contains(s.Value, "done"), qt.IsTrue,
					qt.Commentf("expected 'done' in output, got: %q", s.Value))
				// All coroutine steps must appear at least once.
				for _, step := range []string{"A1", "B1", "A2", "B2"} {
					qt.Assert(t, strings.Contains(s.Value, step), qt.IsTrue,
						qt.Commentf("expected %q in output, got: %q", step, s.Value))
				}
			},
			timeout: 5 * time.Second,
		},
		{
			name: "single coroutine yield and resume",
			code: `(let ((p (open-output-string)))
				(define *queue* '())
				(define (enqueue! thunk) (set! *queue* (append *queue* (list thunk))))
				(define (dequeue!) (let ((n (car *queue*))) (set! *queue* (cdr *queue*)) n))
				(define (scheduler-run) (if (not (null? *queue*)) ((dequeue!))))
				(define (spawn thunk) (enqueue! (lambda () (thunk) (scheduler-run))))
				(define (yield) (call/cc (lambda (k) (enqueue! (lambda () (k #f))) (scheduler-run))))

				(spawn (lambda () (display "1 " p) (yield) (display "2 " p)))

				(scheduler-run)
				(display "end" p)
				(get-output-string p))`,
			check: func(t *testing.T, result values.Value) {
				t.Helper()
				s, ok := result.(*values.String)
				qt.Assert(t, ok, qt.IsTrue)
				qt.Assert(t, strings.Contains(s.Value, "1"), qt.IsTrue)
				qt.Assert(t, strings.Contains(s.Value, "2"), qt.IsTrue)
				qt.Assert(t, strings.Contains(s.Value, "end"), qt.IsTrue,
					qt.Commentf("expected 'end' in output, got: %q", s.Value))
			},
			timeout: 5 * time.Second,
		},
		{
			name: "cross-context continuation escape",
			// A's continuation invoked from inside B's call/cc context.
			code: `(let ((p (open-output-string)))
				(define *queue* '())
				(define (enqueue! thunk) (set! *queue* (append *queue* (list thunk))))
				(define (dequeue!) (let ((n (car *queue*))) (set! *queue* (cdr *queue*)) n))
				(define (scheduler-run) (if (not (null? *queue*)) ((dequeue!))))
				(define (spawn thunk) (enqueue! (lambda () (thunk) (scheduler-run))))
				(define (yield) (call/cc (lambda (k) (enqueue! (lambda () (k #f))) (scheduler-run))))

				(spawn (lambda () (display "X " p) (yield) (display "Y " p)))
				(spawn (lambda () (display "P " p) (yield) (display "Q " p)))

				(scheduler-run)
				(get-output-string p))`,
			check: func(t *testing.T, result values.Value) {
				t.Helper()
				s, ok := result.(*values.String)
				qt.Assert(t, ok, qt.IsTrue)
				for _, step := range []string{"X", "P", "Y", "Q"} {
					qt.Assert(t, strings.Contains(s.Value, step), qt.IsTrue,
						qt.Commentf("expected %q in output, got: %q", step, s.Value))
				}
			},
			timeout: 5 * time.Second,
		},
		{
			name: "call/cc inside nested function",
			// call/cc inside a helper function, not at top level.
			code: `(begin
				(define (f) (+ 1 (call/cc (lambda (k) (k 10)))))
				(f))`,
			check: func(t *testing.T, result values.Value) {
				t.Helper()
				qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(11))
			},
			timeout: 5 * time.Second,
		},
		{
			name: "saved continuation invoked later",
			code: `(begin
				(define k-saved #f)
				(define count 0)
				(let ((result (call/cc (lambda (k) (set! k-saved k) 'first))))
					(set! count (+ count 1))
					(if (< count 3)
						(k-saved 'again)
						count)))`,
			check: func(t *testing.T, result values.Value) {
				t.Helper()
				qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(3))
			},
			timeout: 5 * time.Second,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCodeWithTimeout(t, tc.code, tc.timeout)
			qt.Assert(t, err, qt.IsNil)
			tc.check(t, result)
		})
	}
}

func TestApplyWithParameter(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			"apply parameter get",
			`(let ((p (make-parameter 42)))
			   (apply p '()))`,
			values.NewInteger(42),
		},
		{
			"apply parameter set then get",
			`(let ((p (make-parameter 0)))
			   (apply p '(99))
			   (p))`,
			values.NewInteger(99),
		},
		{
			"apply parameter with converter",
			`(let ((p (make-parameter 0 (lambda (x) (* x 2)))))
			   (apply p '(5))
			   (p))`,
			values.NewInteger(10),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result, valuestest.SchemeEquals, tc.want)
		})
	}
}
