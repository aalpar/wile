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

package wile_test

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"

	"github.com/aalpar/wile"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine()
	if err != nil {
		t.Fatal(err)
	}
	return engine
}

func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	ctx := context.Background()
	result, err := engine.Eval(ctx, code)
	if err != nil {
		t.Fatalf("eval %q: %v", code, err)
	}
	return result
}

func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	ctx := context.Background()
	_, err := engine.Eval(ctx, code)
	if err == nil {
		t.Fatalf("eval %q: expected error, got nil", code)
	}
}

// --- Registration validation errors ---

func TestRegisterFuncValidation(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		fn   any
	}{
		{"not a function", 42},
		{"nil", nil},
		{"string", "hello"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := engine.RegisterFunc("bad", tc.fn)
			if err == nil {
				t.Fatal("expected error")
			}
			var wileErr *wile.Error
			if !errors.As(err, &wileErr) {
				t.Fatalf("expected *wile.Error, got %T", err)
			}
		})
	}
}

func TestRegisterFuncUnsupportedTypes(t *testing.T) {
	engine := newEngine(t)

	tcs := []struct {
		name string
		fn   any
	}{
		{"complex128 param", func(c complex128) float64 { return real(c) }},
		{"map param", func(m map[string]int) int { return len(m) }},
		{"unsupported return", func() complex128 { return 0 }},
		{"three returns", func() (int64, int64, error) { return 0, 0, nil }},
		{"error not last", func() (error, int64) { return nil, 0 }}, //nolint:staticcheck // intentionally wrong signature to test validation
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			err := engine.RegisterFunc("bad", tc.fn)
			if err == nil {
				t.Fatal("expected error for unsupported type")
			}
		})
	}
}

func TestRegisterFuncContextNotFirst(t *testing.T) {
	engine := newEngine(t)
	err := engine.RegisterFunc("bad", func(n int64, ctx context.Context) int64 {
		_ = ctx
		return n
	})
	if err == nil {
		t.Fatal("expected error for context.Context not in first position")
	}
}

// --- Type round-trip tests ---

func TestRegisterFuncTypeRoundTrips(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Register functions for each supported type.
	err := engine.RegisterFunc("identity-int64", func(n int64) int64 {
		return n
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-int", func(n int) int {
		return n
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-float64", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-string", func(s string) string {
		return s
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-bool", func(b bool) bool {
		return b
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-bytes", func(bs []byte) []byte {
		return bs
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("identity-value", func(v wile.Value) wile.Value {
		return v
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"int64", "(identity-int64 42)", "42"},
		{"int64 negative", "(identity-int64 -7)", "-7"},
		{"int", "(identity-int 99)", "99"},
		{"float64", "(identity-float64 3.14)", "3.14"},
		{"string", `(identity-string "hello")`, `"hello"`},
		{"bool true", "(identity-bool #t)", "#t"},
		{"bool false", "(identity-bool #f)", "#f"},
		{"bytes", "(identity-bytes #u8(1 2 3))", "#u8( 1 2 3 )"},
		{"value pass-through", "(identity-value '(1 2 3))", "(1 2 3)"},
		{"value symbol", "(identity-value 'foo)", "foo"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Float promotion ---

func TestRegisterFuncFloatPromotion(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("as-float", func(f float64) float64 {
		return f
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"integer to float", "(as-float 3)", "3.0"},
		{"float stays float", "(as-float 3.5)", "3.5"},
		{"rational to float", "(as-float 1/2)", "0.5"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Void returns ---

func TestRegisterFuncVoid(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	var called bool
	err := engine.RegisterFunc("side-effect", func() {
		called = true
	})
	c.Assert(err, qt.IsNil)

	result := eval(t, engine, "(side-effect)")
	c.Assert(result.IsVoid(), qt.IsTrue)
	c.Assert(called, qt.IsTrue)
}

// --- Error returns ---

func TestRegisterFuncErrorReturns(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("maybe-fail", func(fail bool) (string, error) {
		if fail {
			return "", fmt.Errorf("intentional failure")
		}
		return "ok", nil
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("void-or-fail", func(fail bool) error {
		if fail {
			return fmt.Errorf("void failure")
		}
		return nil
	})
	c.Assert(err, qt.IsNil)

	t.Run("success path", func(t *testing.T) {
		result := eval(t, engine, "(maybe-fail #f)")
		c.Assert(result.SchemeString(), qt.Equals, `"ok"`)
	})

	t.Run("error path", func(t *testing.T) {
		evalExpectError(t, engine, "(maybe-fail #t)")
	})

	t.Run("void success", func(t *testing.T) {
		result := eval(t, engine, "(void-or-fail #f)")
		c.Assert(result.IsVoid(), qt.IsTrue)
	})

	t.Run("void error", func(t *testing.T) {
		evalExpectError(t, engine, "(void-or-fail #t)")
	})
}

// --- Type mismatch errors ---

func TestRegisterFuncTypeMismatch(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("need-int", func(n int64) int64 {
		return n
	})
	if err != nil {
		t.Fatal(err)
	}

	err = engine.RegisterFunc("need-string", func(s string) string {
		return s
	})
	if err != nil {
		t.Fatal(err)
	}

	err = engine.RegisterFunc("need-bool", func(b bool) bool {
		return b
	})
	if err != nil {
		t.Fatal(err)
	}

	err = engine.RegisterFunc("need-bytes", func(bs []byte) []byte {
		return bs
	})
	if err != nil {
		t.Fatal(err)
	}

	tcs := []struct {
		name string
		code string
	}{
		{"string to int", `(need-int "hello")`},
		{"bool to int", `(need-int #t)`},
		{"int to string", `(need-string 42)`},
		{"float to string", `(need-string 3.14)`},
		{"int to bool", `(need-bool 1)`},
		{"string to bool", `(need-bool "yes")`},
		{"string to bytes", `(need-bytes "abc")`},
		{"int to bytes", `(need-bytes 42)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- Type mismatch error message ---

func TestRegisterFuncTypeMismatchMessage(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("need-int", func(n int64) int64 {
		return n
	})
	c.Assert(err, qt.IsNil)

	ctx := context.Background()
	_, err = engine.Eval(ctx, `(need-int "hello")`)
	c.Assert(err, qt.IsNotNil)

	// The runtime error should contain the type conversion message.
	var rtErr *wile.RuntimeError
	c.Assert(errors.As(err, &rtErr), qt.IsTrue)
	c.Assert(rtErr.Error(), qt.Contains, "type conversion failed")
	c.Assert(rtErr.Error(), qt.Contains, "expected integer")
}

// --- Variadic functions ---

func TestRegisterFuncVariadic(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("sum", func(nums ...int64) int64 {
		var total int64
		for _, n := range nums {
			total += n
		}
		return total
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("join", func(sep string, parts ...string) string {
		var b strings.Builder
		for i, p := range parts {
			if i > 0 {
				b.WriteString(sep)
			}
			b.WriteString(p)
		}
		return b.String()
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"sum no args", "(sum)", "0"},
		{"sum one arg", "(sum 10)", "10"},
		{"sum multiple", "(sum 1 2 3 4 5)", "15"},
		{"join one", `(join ", " "a")`, `"a"`},
		{"join multiple", `(join "-" "x" "y" "z")`, `"x-y-z"`},
		{"join empty rest", `(join ", ")`, `""`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- context.Context forwarding ---

func TestRegisterFuncContext(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("ctx-check", func(ctx context.Context) bool {
		return ctx != nil
	})
	c.Assert(err, qt.IsNil)

	result := eval(t, engine, "(ctx-check)")
	c.Assert(result.SchemeString(), qt.Equals, "#t")
}

func TestRegisterFuncContextWithArgs(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("ctx-double", func(ctx context.Context, n int64) int64 {
		_ = ctx
		return n * 2
	})
	c.Assert(err, qt.IsNil)

	result := eval(t, engine, "(ctx-double 21)")
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

// --- Arithmetic operations ---

func TestRegisterFuncArithmetic(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("double", func(n int64) int64 {
		return n * 2
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("add-floats", func(a, b float64) float64 {
		return a + b
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"double", "(double 21)", "42"},
		{"double zero", "(double 0)", "0"},
		{"double negative", "(double -5)", "-10"},
		{"add floats", "(add-floats 1.5 2.5)", "4.0"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Scheme interop (map, apply, higher-order) ---

func TestRegisterFuncSchemeInterop(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("double", func(n int64) int64 {
		return n * 2
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("negate", func(n int64) int64 {
		return -n
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"map double", "(map double '(1 2 3))", "(2 4 6)"},
		{"apply double", "(apply double '(5))", "10"},
		{"composed", "(double (double 3))", "12"},
		{"map negate", "(map negate '(1 -2 3))", "(-1 2 -3)"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Int64 from inexact integer ---

func TestRegisterFuncInt64FromInexact(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("identity-int64", func(n int64) int64 {
		return n
	})
	c.Assert(err, qt.IsNil)

	// 7.0 is an inexact integer — should be accepted by int64 converter.
	result := eval(t, engine, "(identity-int64 7.0)")
	c.Assert(result.SchemeString(), qt.Equals, "7")
}

func TestRegisterFuncInt64RejectsNonIntegerFloat(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("identity-int64", func(n int64) int64 {
		return n
	})
	if err != nil {
		t.Fatal(err)
	}

	// 3.5 is not an integer — should be rejected.
	evalExpectError(t, engine, "(identity-int64 3.5)")
}

// --- Multiple FFI functions in one engine ---

func TestRegisterFuncMultipleFunctions(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("inc", func(n int64) int64 {
		return n + 1
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("dec", func(n int64) int64 {
		return n - 1
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("greet", func(name string) string {
		return "hello, " + name
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"inc", "(inc 5)", "6"},
		{"dec", "(dec 5)", "4"},
		{"greet", `(greet "world")`, `"hello, world"`},
		{"combined", "(inc (dec (inc 10)))", "11"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
