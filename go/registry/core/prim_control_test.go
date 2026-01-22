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

package core_test

import (
	"context"
	"strings"
	"testing"

	"wile/machine"
	"wile/parser"
	"wile/runtime"
	"wile/values"

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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			_, err := runProgramAST(t, tc.prog)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
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
			result, err := runProgramAST(t, tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

// TestDynamicWindEscape tests that after is called on continuation escape.
// This test uses runSchemeCodeWithEnv to load bootstrap macros for 'let'.
func TestDynamicWindEscape(t *testing.T) {
	env, err := runtime.NewTopLevelEnvironmentFrameTiny(context.TODO())
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

	ectx := machine.NewExpandTimeCallContext()
	expanded, err := machine.NewExpanderTimeContinuation(env).ExpandExpression(ectx, stx)
	qt.Assert(t, err, qt.IsNil)

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := machine.NewCompileTimeCallContext(false, true, env)
	err = machine.NewCompiletimeContinuation(tpl, env).CompileExpression(cctx, expanded)
	qt.Assert(t, err, qt.IsNil)

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// After should have run, setting v[0] to 2
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(2))
}

func TestDynamicWindBasic(t *testing.T) {
	result, err := runSchemeCode(t, `
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
	tcs := []schemeCodeTestCase{
		{
			name:     "call-with-values single value",
			code:     `(call-with-values (lambda () 42) (lambda (x) x))`,
			expected: values.NewInteger(42),
		},
		{
			name:     "call-with-values two values",
			code:     `(call-with-values (lambda () (values 1 2)) (lambda (x y) (+ x y)))`,
			expected: values.NewInteger(3),
		},
		{
			name:     "call-with-values returning no values",
			code:     `(call-with-values (lambda () (values)) (lambda () 'done))`,
			expected: values.NewSymbol("done"),
		},
		{
			name:     "call-with-values returning three values",
			code:     `(call-with-values (lambda () (values 1 2 3)) (lambda (a b c) (+ a b c)))`,
			expected: values.NewInteger(6),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}

func TestApplyExtended(t *testing.T) {
	tcs := []schemeCodeTestCase{
		{
			name:     "apply with prefix args",
			code:     `(apply + 1 2 '(3 4))`,
			expected: values.NewInteger(10),
		},
		{
			name:     "apply with empty list",
			code:     `(apply + '())`,
			expected: values.NewInteger(0),
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.expected)
		})
	}
}
