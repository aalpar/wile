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

package wile

import (
	"context"
	"errors"
	"math/big"
	"strings"
	"testing"
	"time"

	"github.com/aalpar/wile/extensions/exceptions"
	"github.com/aalpar/wile/registry"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// Value constructors

func TestNewFloat(t *testing.T) {
	c := qt.New(t)
	v := NewFloat(3.14)
	c.Assert(v.SchemeString(), qt.Equals, "3.14")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewString(t *testing.T) {
	c := qt.New(t)
	v := NewString("hello")
	c.Assert(v.SchemeString(), qt.Equals, `"hello"`)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewSymbol(t *testing.T) {
	c := qt.New(t)
	v := NewSymbol("foo")
	c.Assert(v.SchemeString(), qt.Equals, "foo")
}

func TestNewBoolean(t *testing.T) {
	c := qt.New(t)
	c.Assert(NewBoolean(true).SchemeString(), qt.Equals, "#t")
	c.Assert(NewBoolean(false).SchemeString(), qt.Equals, "#f")
}

func TestNewList(t *testing.T) {
	c := qt.New(t)

	t.Run("empty list", func(t *testing.T) {
		v := NewList()
		c.Assert(v.SchemeString(), qt.Equals, "()")
	})

	t.Run("non-empty list", func(t *testing.T) {
		v := NewList(NewInteger(1), NewInteger(2), NewInteger(3))
		c.Assert(v.SchemeString(), qt.Equals, "(1 2 3)")
	})
}

func TestNewBigInteger(t *testing.T) {
	c := qt.New(t)
	bigInt := big.NewInt(123456789)
	v := NewBigInteger(bigInt)
	c.Assert(v.SchemeString(), qt.Equals, "123456789")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigIntegerFromInt64(t *testing.T) {
	c := qt.New(t)
	v := NewBigIntegerFromInt64(9223372036854775807)
	c.Assert(v.SchemeString(), qt.Equals, "9223372036854775807")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigIntegerFromString(t *testing.T) {
	c := qt.New(t)

	t.Run("valid base 10", func(t *testing.T) {
		v := NewBigIntegerFromString("123456789012345678901234567890", 10)
		c.Assert(v, qt.IsNotNil)
		c.Assert(v.SchemeString(), qt.Equals, "123456789012345678901234567890")
	})

	t.Run("valid base 16", func(t *testing.T) {
		v := NewBigIntegerFromString("DEADBEEF", 16)
		c.Assert(v, qt.IsNotNil)
		c.Assert(v.SchemeString(), qt.Equals, "3735928559")
	})

	t.Run("invalid string", func(t *testing.T) {
		v := NewBigIntegerFromString("not a number", 10)
		c.Assert(v, qt.IsNil)
	})
}

func TestNewBigFloat(t *testing.T) {
	c := qt.New(t)
	bigFloat := big.NewFloat(3.141592653589793)
	v := NewBigFloat(bigFloat)
	c.Assert(v, qt.IsNotNil)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigFloatFromFloat64(t *testing.T) {
	c := qt.New(t)
	v := NewBigFloatFromFloat64(2.718281828459045)
	c.Assert(v, qt.IsNotNil)
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewBigFloatFromString(t *testing.T) {
	c := qt.New(t)

	t.Run("valid float string", func(t *testing.T) {
		v := NewBigFloatFromString("1.23456789012345678901234567890")
		c.Assert(v, qt.IsNotNil)
	})

	t.Run("scientific notation", func(t *testing.T) {
		v := NewBigFloatFromString("1.23e10")
		c.Assert(v, qt.IsNotNil)
	})

	t.Run("invalid string", func(t *testing.T) {
		v := NewBigFloatFromString("not a float")
		c.Assert(v, qt.IsNil)
	})
}

func TestNewRational(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		name     string
		num, den int64
		wantStr  string
	}{
		{"3/4", 3, 4, "3/4"},
		{"negative", -1, 2, "-1/2"},
		{"whole number reduces", 6, 3, "2"},
		{"zero numerator", 0, 5, "0"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := NewRational(tc.num, tc.den)
			c.Assert(v.SchemeString(), qt.Equals, tc.wantStr)
			c.Assert(v.IsVoid(), qt.IsFalse)
		})
	}
}

func TestNewRationalFromBigInt(t *testing.T) {
	c := qt.New(t)
	v := NewRationalFromBigInt(big.NewInt(7), big.NewInt(3))
	c.Assert(v.SchemeString(), qt.Equals, "7/3")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewComplex(t *testing.T) {
	c := qt.New(t)
	tcs := []struct {
		name    string
		val     complex128
		wantStr string
	}{
		{"1+2i", 1 + 2i, "1.0+2.0i"},
		{"pure imaginary", 0 + 3i, "0.0+3.0i"},
		{"real only", 5 + 0i, "5.0+0.0i"},
		{"negative imaginary", 1 - 2i, "1.0-2.0i"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			v := NewComplex(tc.val)
			c.Assert(v.SchemeString(), qt.Equals, tc.wantStr)
			c.Assert(v.IsVoid(), qt.IsFalse)
		})
	}
}

func TestNewComplexFromParts(t *testing.T) {
	c := qt.New(t)
	v := NewComplexFromParts(3.0, 4.0)
	c.Assert(v.SchemeString(), qt.Equals, "3.0+4.0i")
	c.Assert(v.IsVoid(), qt.IsFalse)
}

func TestNewVector(t *testing.T) {
	c := qt.New(t)

	t.Run("empty vector", func(t *testing.T) {
		v := NewVector()
		c.Assert(v.SchemeString(), qt.Equals, "#()")
		c.Assert(v.IsVoid(), qt.IsFalse)
	})

	t.Run("with elements", func(t *testing.T) {
		v := NewVector(NewInteger(1), NewString("two"), NewBoolean(true))
		c.Assert(v.SchemeString(), qt.Equals, `#( 1 "two" #t )`)
	})

	t.Run("single element", func(t *testing.T) {
		v := NewVector(NewFloat(3.14))
		c.Assert(v.SchemeString(), qt.Equals, "#( 3.14 )")
	})
}

func TestValueConstructors_RoundTrip(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)
	ctx := context.Background()

	tcs := []struct {
		name    string
		val     Value
		code    string
		wantStr string
	}{
		{"rational arithmetic", NewRational(1, 3), "(+ x 1/3)", "2/3"},
		{"complex arithmetic", NewComplex(1 + 2i), "(+ x 3+4i)", "4+6i"},
		{"vector-ref", NewVector(NewInteger(10), NewInteger(20), NewInteger(30)), "(vector-ref x 1)", "20"},
		{"vector-length", NewVector(NewInteger(1), NewInteger(2)), "(vector-length x)", "2"},
		{"big integer arithmetic", NewBigInteger(big.NewInt(1000000000000)), "(+ x 1)", "1000000000001"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err = engine.Define("x", tc.val)
			c.Assert(err, qt.IsNil)
			result, err := engine.Eval(ctx, tc.code)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.wantStr)
		})
	}
}

// Value methods

func TestValue_IsVoid(t *testing.T) {
	c := qt.New(t)
	c.Assert(Void.IsVoid(), qt.IsTrue)
	c.Assert(NewInteger(1).IsVoid(), qt.IsFalse)
	c.Assert(EmptyList.IsVoid(), qt.IsFalse)
}

func TestValue_Internal(t *testing.T) {
	c := qt.New(t)
	v := NewInteger(42)
	c.Assert(v.Internal(), qt.IsNotNil)
	c.Assert(v.Internal().SchemeString(), qt.Equals, "42")
}

// Engine accessors

func TestEngine_Environment(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(engine.Environment(), qt.IsNotNil)
}

func TestEngine_TopLevelEnvironment(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)
	c.Assert(engine.TopLevelEnvironment(), qt.IsNotNil)
}

// CompiledCode.String

func TestCompiledCode_String(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	compiled, err := engine.Compile(context.Background(), "(+ 1 2)")
	c.Assert(err, qt.IsNil)
	c.Assert(compiled.String(), qt.Equals, "#<compiled-code>")
}

// WithRegistry option

func TestWithRegistry(t *testing.T) {
	c := qt.New(t)
	reg := registry.NewRegistry()
	// Engine with empty registry — should still create
	engine, err := NewEngine(context.Background(), WithRegistry(reg))
	c.Assert(err, qt.IsNil)
	c.Assert(engine, qt.IsNotNil)
}

// WithExtensions option

func TestWithExtensions(t *testing.T) {
	c := qt.New(t)
	// WithExtensions with no extensions should work fine
	engine, err := NewEngine(context.Background(), WithExtensions())
	c.Assert(err, qt.IsNil)
	c.Assert(engine, qt.IsNotNil)
}

// Call with different callable types

func TestCall_Lambda(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `(define (add a b) (+ a b))`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("add")
	c.Assert(ok, qt.IsTrue)

	result, err := engine.Call(ctx, proc, NewInteger(3), NewInteger(4))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "7")
}

func TestCall_CaseLambda(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `
		(define f
			(case-lambda
				((x) (* x x))
				((x y) (+ x y))))
	`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("f")
	c.Assert(ok, qt.IsTrue)

	// 1-arg clause
	result, err := engine.Call(ctx, proc, NewInteger(5))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "25")

	// 2-arg clause
	result, err = engine.Call(ctx, proc, NewInteger(3), NewInteger(7))
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "10")
}

func TestCall_Parameter(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `(define p (make-parameter 42))`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("p")
	c.Assert(ok, qt.IsTrue)

	// 0-arg: get current value
	result, err := engine.Call(ctx, proc)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "42")

	// 1-arg: set new value
	result, err = engine.Call(ctx, proc, NewInteger(99))
	c.Assert(err, qt.IsNil)
	c.Assert(result.IsVoid(), qt.IsTrue)

	// verify the set took effect
	result, err = engine.Call(ctx, proc)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "99")
}

func TestCall_ParameterWithConverter(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	// Parameter with converter that doubles the input
	_, err = engine.EvalMultiple(ctx, `
		(define p (make-parameter 0 (lambda (x) (* x 2))))
	`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("p")
	c.Assert(ok, qt.IsTrue)

	// Initial value was passed through converter: 0 * 2 = 0
	result, err := engine.Call(ctx, proc)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "0")

	// Set with converter: 5 * 2 = 10
	_, err = engine.Call(ctx, proc, NewInteger(5))
	c.Assert(err, qt.IsNil)

	result, err = engine.Call(ctx, proc)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "10")
}

func TestCall_NotAProcedure(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.Call(ctx, NewInteger(42))

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Message, qt.Equals, "not a procedure")
}

func TestCall_ParameterTooManyArgs(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `(define p (make-parameter 0))`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("p")
	c.Assert(ok, qt.IsTrue)

	_, err = engine.Call(ctx, proc, NewInteger(1), NewInteger(2))
	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Message, qt.Contains, "expected 0 or 1 arguments")
}

func TestCall_ComposableContinuation(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	// Capture a composable continuation into a variable
	_, err = engine.EvalMultiple(ctx, `
		(define saved-k #f)
		(define tag (make-continuation-prompt-tag 'test))
		(call-with-continuation-prompt
			(lambda ()
				(call-with-composable-continuation
					(lambda (k) (set! saved-k k) 0)
					tag))
			tag
			(lambda (v) v))
	`)
	c.Assert(err, qt.IsNil)

	proc, ok := engine.Get("saved-k")
	c.Assert(ok, qt.IsTrue)

	_, err = engine.Call(ctx, proc, NewInteger(1))
	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Message, qt.Equals, "cannot call composable continuation from Go")
}

func TestCall_CallCCNonTailEscape(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()

	tcs := []struct {
		name string
		code string
		want string
	}{
		{
			"tail position",
			`(define (esc-tail) (call/cc (lambda (k) (k 42))))`,
			"42",
		},
		{
			"non-tail position",
			`(define (esc-nontail) (+ 1 (call/cc (lambda (k) (k 42)))))`,
			"43",
		},
		{
			"escape skips subsequent code",
			`(define (esc-skip) (call/cc (lambda (k) (k 42) 99)))`,
			"42",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			_, err := engine.EvalMultiple(ctx, tc.code)
			c.Assert(err, qt.IsNil)

			// Extract the procedure name from the define form
			name := tc.code[len(`(define (`):len(tc.code)]
			idx := strings.Index(name, ")")
			c.Assert(idx >= 0, qt.IsTrue, qt.Commentf("malformed define in test case: %s", tc.code))
			name = name[:idx]

			proc, ok := engine.Get(name)
			c.Assert(ok, qt.IsTrue)

			result, err := engine.Call(ctx, proc)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// Structured error types

func TestCompilationError(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.Eval(ctx, "(")

	var compErr *CompilationError
	c.Assert(errors.As(err, &compErr), qt.IsTrue)
	c.Assert(compErr.Cause, qt.IsNotNil)
}

func TestCompilationError_Format(t *testing.T) {
	c := qt.New(t)

	t.Run("with cause", func(t *testing.T) {
		e := &CompilationError{Message: "parse error", Cause: errors.New("unexpected EOF")}
		c.Assert(e.Error(), qt.Equals, "parse error: unexpected EOF")
	})

	t.Run("without cause", func(t *testing.T) {
		e := &CompilationError{Message: "parse error"}
		c.Assert(e.Error(), qt.Equals, "parse error")
	})
}

func TestCompilationError_Unwrap(t *testing.T) {
	c := qt.New(t)
	cause := errors.New("root")
	e := &CompilationError{Message: "msg", Cause: cause}
	c.Assert(e.Unwrap(), qt.Equals, cause)
}

func TestRuntimeError_DivisionByZero(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.Eval(ctx, "(/ 1 0)")

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Cause, qt.IsNotNil)
}

func TestRuntimeError_SchemeRaise(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background(), WithExtension(exceptions.Extension))
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.Eval(ctx, `(raise "boom")`)

	var rtErr *RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Condition, qt.IsNotNil)
	c.Assert(rtErr.Condition.SchemeString(), qt.Equals, `"boom"`)
}

func TestRuntimeError_IsSchemeException(t *testing.T) {
	c := qt.New(t)

	t.Run("true when condition set", func(t *testing.T) {
		e := &RuntimeError{Message: "raised", Condition: NewInteger(42)}
		c.Assert(e.IsSchemeException(), qt.IsTrue)
	})

	t.Run("false when no condition", func(t *testing.T) {
		e := &RuntimeError{Message: "go error"}
		c.Assert(e.IsSchemeException(), qt.IsFalse)
	})

	t.Run("false on nil receiver", func(t *testing.T) {
		var e *RuntimeError
		c.Assert(e.IsSchemeException(), qt.IsFalse)
	})
}

func TestRuntimeError_Format(t *testing.T) {
	c := qt.New(t)

	t.Run("with cause", func(t *testing.T) {
		e := &RuntimeError{Message: "runtime error", Cause: errors.New("division by zero")}
		c.Assert(e.Error(), qt.Equals, "runtime error: division by zero")
	})

	t.Run("without cause", func(t *testing.T) {
		e := &RuntimeError{Message: "runtime error"}
		c.Assert(e.Error(), qt.Equals, "runtime error")
	})
}

func TestRuntimeError_Unwrap(t *testing.T) {
	c := qt.New(t)
	cause := errors.New("root")
	e := &RuntimeError{Message: "msg", Cause: cause}
	c.Assert(e.Unwrap(), qt.Equals, cause)
}

// Value helpers

func TestIsList(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsList(NewList(NewInteger(1), NewInteger(2))), qt.IsTrue)
	c.Assert(IsList(EmptyList), qt.IsTrue)
	c.Assert(IsList(NewInteger(5)), qt.IsFalse)
}

func TestIsPair(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsPair(NewList(NewInteger(1))), qt.IsTrue)
	c.Assert(IsPair(EmptyList), qt.IsFalse)
	c.Assert(IsPair(NewInteger(5)), qt.IsFalse)
}

func TestIsNull(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsNull(EmptyList), qt.IsTrue)
	c.Assert(IsNull(NewList(NewInteger(1))), qt.IsFalse)
	c.Assert(IsNull(NewInteger(5)), qt.IsFalse)
}

func TestIsSymbol(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsSymbol(NewSymbol("foo")), qt.IsTrue)
	c.Assert(IsSymbol(NewString("foo")), qt.IsFalse)
}

func TestIsString(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsString(NewString("hello")), qt.IsTrue)
	c.Assert(IsString(NewInteger(42)), qt.IsFalse)
}

func TestIsNumber(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsNumber(NewInteger(1)), qt.IsTrue)
	c.Assert(IsNumber(NewFloat(1.5)), qt.IsTrue)
	c.Assert(IsNumber(NewString("1")), qt.IsFalse)
}

func TestIsBoolean(t *testing.T) {
	c := qt.New(t)
	c.Assert(IsBoolean(True), qt.IsTrue)
	c.Assert(IsBoolean(False), qt.IsTrue)
	c.Assert(IsBoolean(NewInteger(0)), qt.IsFalse)
}

func TestIsProcedure(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.EvalMultiple(ctx, `
		(define (f x) x)
		(define g (case-lambda ((x) x) ((x y) (+ x y))))
		(define p (make-parameter 0))
	`)
	c.Assert(err, qt.IsNil)

	f, _ := engine.Get("f")
	g, _ := engine.Get("g")
	p, _ := engine.Get("p")

	c.Assert(IsProcedure(f), qt.IsTrue)
	c.Assert(IsProcedure(g), qt.IsTrue)
	c.Assert(IsProcedure(p), qt.IsTrue)
	c.Assert(IsProcedure(NewInteger(42)), qt.IsFalse)
}

func TestCar(t *testing.T) {
	c := qt.New(t)

	v, ok := Car(NewList(NewInteger(1), NewInteger(2)))
	c.Assert(ok, qt.IsTrue)
	c.Assert(v.SchemeString(), qt.Equals, "1")

	_, ok = Car(EmptyList)
	c.Assert(ok, qt.IsFalse)

	_, ok = Car(NewInteger(5))
	c.Assert(ok, qt.IsFalse)
}

func TestCdr(t *testing.T) {
	c := qt.New(t)

	v, ok := Cdr(NewList(NewInteger(1), NewInteger(2)))
	c.Assert(ok, qt.IsTrue)
	c.Assert(v.SchemeString(), qt.Equals, "(2)")

	_, ok = Cdr(EmptyList)
	c.Assert(ok, qt.IsFalse)
}

func TestToSlice(t *testing.T) {
	c := qt.New(t)

	t.Run("proper list", func(t *testing.T) {
		sl, ok := ToSlice(context.Background(), NewList(NewInteger(1), NewInteger(2), NewInteger(3)))
		c.Assert(ok, qt.IsTrue)
		c.Assert(len(sl), qt.Equals, 3)
		c.Assert(sl[0].SchemeString(), qt.Equals, "1")
		c.Assert(sl[2].SchemeString(), qt.Equals, "3")
	})

	t.Run("empty list", func(t *testing.T) {
		sl, ok := ToSlice(context.Background(), EmptyList)
		c.Assert(ok, qt.IsTrue)
		c.Assert(len(sl), qt.Equals, 0)
	})

	t.Run("not a list", func(t *testing.T) {
		_, ok := ToSlice(context.Background(), NewInteger(5))
		c.Assert(ok, qt.IsFalse)
	})
}

func TestToGoString(t *testing.T) {
	c := qt.New(t)

	s, ok := ToGoString(NewString("hello"))
	c.Assert(ok, qt.IsTrue)
	c.Assert(s, qt.Equals, "hello")

	_, ok = ToGoString(NewInteger(42))
	c.Assert(ok, qt.IsFalse)
}

func TestToGoInt(t *testing.T) {
	c := qt.New(t)

	n, ok := ToGoInt(NewInteger(42))
	c.Assert(ok, qt.IsTrue)
	c.Assert(n, qt.Equals, int64(42))

	_, ok = ToGoInt(NewFloat(3.14))
	c.Assert(ok, qt.IsFalse)

	_, ok = ToGoInt(NewString("42"))
	c.Assert(ok, qt.IsFalse)
}

func TestToGoFloat(t *testing.T) {
	c := qt.New(t)

	f, ok := ToGoFloat(NewFloat(3.14))
	c.Assert(ok, qt.IsTrue)
	c.Assert(f, qt.Equals, 3.14)

	_, ok = ToGoFloat(NewInteger(42))
	c.Assert(ok, qt.IsFalse)
}

func TestToGoBool(t *testing.T) {
	c := qt.New(t)

	b, ok := ToGoBool(True)
	c.Assert(ok, qt.IsTrue)
	c.Assert(b, qt.IsTrue)

	b, ok = ToGoBool(False)
	c.Assert(ok, qt.IsTrue)
	c.Assert(b, qt.IsFalse)

	_, ok = ToGoBool(NewInteger(1))
	c.Assert(ok, qt.IsFalse)
}

// Context cancellation

func TestEval_ContextCancellation(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithTimeout(context.Background(), 500*time.Millisecond)
	defer cancel()

	_, err = engine.Eval(ctx, "(let loop () (loop))")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, context.DeadlineExceeded), qt.IsTrue)
}

func TestEval_AlreadyCancelledContext(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	_, err = engine.Eval(ctx, "(+ 1 2)")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue)
}

func TestEval_CancelDuringComputation(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		time.Sleep(200 * time.Millisecond)
		cancel()
	}()

	_, err = engine.Eval(ctx, "(let loop () (loop))")
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue)
}

func TestEval_CancelNestedCalls(t *testing.T) {
	c := qt.New(t)
	engine, err := NewEngine(context.Background())
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithTimeout(context.Background(), 500*time.Millisecond)
	defer cancel()

	_, err = engine.EvalMultiple(ctx, `
		(define (spin n) (spin (+ n 1)))
		(spin 0)
	`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(errors.Is(err, context.DeadlineExceeded), qt.IsTrue)
}

// mockCloseableExtension is a test helper that implements both Extension and Closeable.
type mockCloseableExtension struct {
	name     string
	closed   bool
	closeErr error
}

func (m *mockCloseableExtension) Name() string {
	return m.name
}

func (m *mockCloseableExtension) AddToRegistry(_ *registry.Registry) error {
	return nil
}

func (m *mockCloseableExtension) Close() error {
	m.closed = true
	return m.closeErr
}

func TestEngineClose(t *testing.T) {
	c := qt.New(t)

	t.Run("no closers", func(t *testing.T) {
		engine, err := NewEngine(context.Background())
		c.Assert(err, qt.IsNil)
		err = engine.Close()
		c.Assert(err, qt.IsNil)
	})

	t.Run("closeable extension called", func(t *testing.T) {
		ext := &mockCloseableExtension{name: "test"}
		engine, err := NewEngine(context.Background(), WithExtension(ext))
		c.Assert(err, qt.IsNil)
		c.Assert(ext.closed, qt.IsFalse)

		err = engine.Close()
		c.Assert(err, qt.IsNil)
		c.Assert(ext.closed, qt.IsTrue)
	})

	t.Run("collects errors from closers", func(t *testing.T) {
		ext1 := &mockCloseableExtension{name: "a", closeErr: errors.New("err-a")}
		ext2 := &mockCloseableExtension{name: "b", closeErr: errors.New("err-b")}
		engine, err := NewEngine(context.Background(),
			WithExtension(ext1),
			WithExtension(ext2),
		)
		c.Assert(err, qt.IsNil)

		err = engine.Close()
		c.Assert(err, qt.IsNotNil)
		c.Assert(ext1.closed, qt.IsTrue)
		c.Assert(ext2.closed, qt.IsTrue)
	})

	t.Run("double close returns error", func(t *testing.T) {
		engine, err := NewEngine(context.Background())
		c.Assert(err, qt.IsNil)
		err = engine.Close()
		c.Assert(err, qt.IsNil)
		err = engine.Close()
		c.Assert(err, qt.IsNotNil)
		c.Assert(errors.Is(err, ErrEngineClosed), qt.IsTrue)
	})
}

func TestWithMaxCallDepth(t *testing.T) {
	tests := []struct {
		name        string
		code        string
		depth       uint64
		wantErr     bool
		errSentinel error
	}{
		{
			name:        "deep recursion exceeds limit",
			code:        "(let loop ((n 20000)) (if (= n 0) 0 (+ 1 (loop (- n 1)))))",
			depth:       100,
			wantErr:     true,
			errSentinel: values.ErrCallDepthExceeded,
		},
		{
			name:    "recursion within limit succeeds",
			code:    "(let loop ((n 50)) (if (= n 0) 0 (+ 1 (loop (- n 1)))))",
			depth:   100,
			wantErr: false,
		},
		{
			name:    "zero means unlimited",
			code:    "(let loop ((n 5000)) (if (= n 0) 0 (+ 1 (loop (- n 1)))))",
			depth:   0,
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng, engErr := NewEngine(context.Background(), WithMaxCallDepth(tt.depth))
			if engErr != nil {
				t.Fatalf("NewEngine: %v", engErr)
			}
			_, err := eng.Eval(context.Background(), tt.code)
			if tt.wantErr {
				if err == nil {
					t.Fatal("expected error, got nil")
				}
				if !errors.Is(err, tt.errSentinel) {
					t.Fatalf("expected %v, got: %v", tt.errSentinel, err)
				}
			} else if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}
		})
	}
}
