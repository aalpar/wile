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
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background())
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
			if !errors.Is(err, values.ErrFFIRegistration) {
				t.Fatalf("expected ErrFFIRegistration, got %T: %v", err, err)
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
		{"unsupported map key", func(m map[float64]int) int { return len(m) }},
		{"unsupported return", func() complex128 { return 0 }},
		{"three returns", func() (int64, int64, error) { return 0, 0, nil }},
		{"error not last", func() (error, int64) { return nil, 0 }}, //nolint:staticcheck // intentionally wrong signature to test validation
		{"unsupported callback param", func(f func(complex128)) { f(0) }},
		{"unsupported return map key", func() map[float64]string { return nil }},
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
		{"bytes", "(identity-bytes #u8(1 2 3))", "#u8(1 2 3)"},
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

// --- Nil Value return produces Void ---

func TestRegisterFuncNilValueReturn(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("maybe-nil", func(returnNil bool) wile.Value {
		if returnNil {
			return nil
		}
		return wile.NewInteger(42)
	})
	c.Assert(err, qt.IsNil)

	t.Run("non-nil", func(t *testing.T) {
		result := eval(t, engine, "(maybe-nil #f)")
		c.Assert(result.SchemeString(), qt.Equals, "42")
	})

	t.Run("nil returns void", func(t *testing.T) {
		result := eval(t, engine, "(maybe-nil #t)")
		c.Assert(result.IsVoid(), qt.IsTrue)
	})
}

// --- Concrete error type rejected at registration ---

func TestRegisterFuncConcreteErrorTypeRejected(t *testing.T) {
	engine := newEngine(t)

	// *fmt.Stringer is not the error interface, but a func returning
	// (int, *os.PathError) should be rejected because *os.PathError
	// implements error but isn't the exact error interface type.
	// We test with (T, T) where second T is not error interface.
	err := engine.RegisterFunc("bad", func() (int64, string) {
		return 0, ""
	})
	if err == nil {
		t.Fatal("expected registration error for non-error second return")
	}
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

// --- Typed slices ---

func TestRegisterFuncSlices(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("sum-list", func(nums []int64) int64 {
		var total int64
		for _, n := range nums {
			total += n
		}
		return total
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("join-list", func(parts []string) string {
		return strings.Join(parts, ",")
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("all-true", func(bools []bool) bool {
		for _, b := range bools {
			if !b {
				return false
			}
		}
		return true
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("avg-list", func(nums []float64) float64 {
		if len(nums) == 0 {
			return 0.0
		}
		var sum float64
		for _, n := range nums {
			sum += n
		}
		return sum / float64(len(nums))
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("make-ints", func(n int64) []int64 {
		result := make([]int64, n)
		for i := range result {
			result[i] = int64(i) + 1
		}
		return result
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("make-strings", func(n int64) []string {
		result := make([]string, n)
		for i := range result {
			result[i] = fmt.Sprintf("s%d", i)
		}
		return result
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("empty-slice", func() []int64 {
		return nil
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"int64 list", "(sum-list '(1 2 3 4 5))", "15"},
		{"int64 empty list", "(sum-list '())", "0"},
		{"string list", `(join-list '("a" "b" "c"))`, `"a,b,c"`},
		{"bool list all true", "(all-true '(#t #t #t))", "#t"},
		{"bool list has false", "(all-true '(#t #f #t))", "#f"},
		{"float64 list", "(avg-list '(1.0 2.0 3.0))", "2.0"},
		{"return int slice", "(make-ints 3)", "(1 2 3)"},
		{"return string slice", "(make-strings 2)", `("s0" "s1")`},
		{"return nil slice", "(empty-slice)", "()"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Nested slices ---

func TestRegisterFuncSliceNested(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("flatten", func(matrix [][]int64) []int64 {
		var result []int64
		for _, row := range matrix {
			result = append(result, row...)
		}
		return result
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("make-matrix", func() [][]int64 {
		return [][]int64{{1, 2}, {3, 4}}
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"flatten nested list", "(flatten '((1 2) (3 4) (5 6)))", "(1 2 3 4 5 6)"},
		{"flatten single row", "(flatten '((10 20)))", "(10 20)"},
		{"flatten empty", "(flatten '())", "()"},
		{"return nested slice", "(make-matrix)", "((1 2) (3 4))"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Maps ---

func TestRegisterFuncMaps(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("map-size", func(m map[string]int64) int64 {
		return int64(len(m))
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("map-get", func(m map[string]string, key string) string {
		return m[key]
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("make-map", func() map[string]int64 {
		return map[string]int64{"a": 1, "b": 2}
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("empty-map", func() map[string]int64 {
		return nil
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("int-key-map", func(m map[int64]string) string {
		return m[1]
	})
	c.Assert(err, qt.IsNil)

	// Helper: build a hashtable in Scheme using set!
	mkht := func(sets string) string {
		return `(let ((ht (make-hashtable))) ` + sets + ` ht)`
	}

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"map size 3", `(map-size ` + mkht(`(hashtable-set! ht "x" 1) (hashtable-set! ht "y" 2) (hashtable-set! ht "z" 3)`) + `)`, "3"},
		{"map size empty", `(map-size (make-hashtable))`, "0"},
		{"map get", `(map-get ` + mkht(`(hashtable-set! ht "hello" "world")`) + ` "hello")`, `"world"`},
		{"return map size", `(hashtable-size (make-map))`, "2"},
		{"return nil map size", `(hashtable-size (empty-map))`, "0"},
		{"int key map", `(int-key-map ` + mkht(`(hashtable-set! ht 1 "one")`) + `)`, `"one"`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Nested maps ---

func TestRegisterFuncMapNested(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("nested-sum", func(m map[string][]int64) int64 {
		var total int64
		for _, vs := range m {
			for _, v := range vs {
				total += v
			}
		}
		return total
	})
	c.Assert(err, qt.IsNil)

	code := `(let ((ht (make-hashtable)))
		(hashtable-set! ht "a" '(1 2 3))
		(hashtable-set! ht "b" '(4 5))
		(nested-sum ht))`
	result := eval(t, engine, code)
	c.Assert(result.SchemeString(), qt.Equals, "15")
}

// --- Structs ---

type testPerson struct {
	Name string
	Age  int64
}

type testPoint struct {
	X float64
	Y float64
}

type testNested struct {
	Label  string
	Coords testPoint
}

type testWithSlice struct {
	Name   string
	Scores []int64
}

func TestRegisterFuncStructs(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("greet-person", func(p testPerson) string {
		return fmt.Sprintf("Hello, %s (age %d)!", p.Name, p.Age)
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("make-person", func(name string, age int64) testPerson {
		return testPerson{Name: name, Age: age}
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("point-dist", func(p testPoint) float64 {
		return p.X*p.X + p.Y*p.Y
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("nested-label", func(n testNested) string {
		return fmt.Sprintf("%s@(%.0f,%.0f)", n.Label, n.Coords.X, n.Coords.Y)
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("with-slice", func(ws testWithSlice) int64 {
		var total int64
		for _, s := range ws.Scores {
			total += s
		}
		return total
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"alist to struct", `(greet-person '((Name . "Alice") (Age . 30)))`, `"Hello, Alice (age 30)!"`},
		{"struct with missing field", `(greet-person '((Name . "Bob")))`, `"Hello, Bob (age 0)!"`},
		{"struct with extra key", `(greet-person '((Name . "Eve") (Age . 25) (Extra . "ignored")))`, `"Hello, Eve (age 25)!"`},
		// Struct alist: (Name . "Charlie") is a dotted pair, (Age . 40) is a dotted pair.
		// Scheme prints these as-is since cdr is not a pair/list.
		{"return struct", `(make-person "Charlie" 40)`, `((Name . "Charlie") (Age . 40))`},
		{"float struct", `(point-dist '((X . 3.0) (Y . 4.0)))`, "25.0"},
		{"nested struct", `(nested-label '((Label . "origin") (Coords . ((X . 0.0) (Y . 0.0)))))`, `"origin@(0,0)"`},
		{"struct with slice field", `(with-slice '((Name . "test") (Scores . (10 20 30))))`, "60"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Struct error cases ---

func TestRegisterFuncStructErrors(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("need-struct", func(p testPerson) string {
		return p.Name
	})
	if err != nil {
		t.Fatal(err)
	}

	tcs := []struct {
		name string
		code string
	}{
		{"non-alist element", `(need-struct '(42))`},
		{"non-symbol key", `(need-struct '((42 . "Alice")))`},
		{"type mismatch in field", `(need-struct '((Name . 42)))`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- Callbacks ---

func TestRegisterFuncCallbacks(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// func(int64) int64 callback
	err := engine.RegisterFunc("apply-f", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	c.Assert(err, qt.IsNil)

	// void callback
	err = engine.RegisterFunc("call-void", func(f func(int64)) {
		f(42)
	})
	c.Assert(err, qt.IsNil)

	// callback with error return
	err = engine.RegisterFunc("try-callback", func(f func(int64) (int64, error), n int64) (int64, error) {
		return f(n)
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"double via callback", "(apply-f (lambda (x) (* x 2)) 5)", "10"},
		{"square via callback", "(apply-f (lambda (x) (* x x)) 7)", "49"},
		{"identity callback", "(apply-f (lambda (x) x) 42)", "42"},
		{"callback success path", "(try-callback (lambda (x) (* x 3)) 10)", "30"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Callback Scheme interop ---

func TestRegisterFuncCallbackSchemeInterop(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("apply-f", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	c.Assert(err, qt.IsNil)

	// Define a Scheme function, then pass it as a callback.
	eval(t, engine, "(define (double x) (* x 2))")
	result := eval(t, engine, "(apply-f double 21)")
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

// --- Callback error cases ---

func TestRegisterFuncCallbackErrors(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("apply-f", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	if err != nil {
		t.Fatal(err)
	}

	tcs := []struct {
		name string
		code string
	}{
		{"non-procedure", `(apply-f 42 5)`},
		{"string not procedure", `(apply-f "hello" 5)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- Callback exception → error ---

func TestRegisterFuncCallbackExceptionToError(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("try-callback", func(f func(int64) (int64, error), n int64) (int64, error) {
		return f(n)
	})
	if err != nil {
		t.Fatal(err)
	}

	// Scheme exception should become a Go error.
	evalExpectError(t, engine, `(try-callback (lambda (x) (error "boom" x)) 5)`)
}

func TestRegisterFuncCallbackErrorSentinels(t *testing.T) {
	engine := newEngine(t)

	// Register a function where the Go callback has an error return.
	// When the Scheme procedure raises an exception, the FFI layer wraps
	// it with ErrFFICallbackError and returns it via the error slot.
	regErr := engine.RegisterFunc("sentinel-callback",
		func(f func(int64) (int64, error), n int64) (int64, error) {
			return f(n)
		})
	if regErr != nil {
		t.Fatal(regErr)
	}

	// Trigger a runtime error in the callback via division by zero.
	ctx := context.Background()
	_, evalErr := engine.Eval(ctx, `(sentinel-callback (lambda (x) (/ x 0)) 5)`)
	if evalErr == nil {
		t.Fatal("expected error, got nil")
	}
	// The FFI sentinel should be in the error chain.
	if !errors.Is(evalErr, values.ErrFFICallbackError) {
		t.Errorf("expected errors.Is(err, ErrFFICallbackError) = true, got false\nerror: %v", evalErr)
	}
	// The error message should contain the original error's context.
	if !strings.Contains(evalErr.Error(), "division by zero") {
		t.Errorf("expected error to contain original error context, got: %v", evalErr)
	}
}

// --- Callback panic-to-error recovery ---

// TestRegisterFuncCallbackPanicToError verifies that a Scheme error inside a
// no-error-return callback is recovered as an error (not a process-killing panic).
// The Go signature func(func(int64) int64, int64) int64 has no error slot,
// so the FFI layer panics with *ForeignError, which makeWrapper recovers.
func TestRegisterFuncCallbackPanicToError(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("apply-f", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	if err != nil {
		t.Fatal(err)
	}

	ctx := context.Background()
	_, evalErr := engine.Eval(ctx, `(apply-f (lambda (x) (/ x 0)) 5)`)
	if evalErr == nil {
		t.Fatal("expected error from callback panic, got nil")
	}
	if !errors.Is(evalErr, values.ErrFFICallbackError) {
		t.Errorf("expected errors.Is(err, ErrFFICallbackError) = true, got false\nerror: %v", evalErr)
	}
	if !strings.Contains(evalErr.Error(), "division by zero") {
		t.Errorf("expected error to mention division by zero, got: %v", evalErr)
	}
}

// TestRegisterFuncCallbackResultConversionPanicToError verifies that a result
// conversion failure in a no-error-return callback is recovered as an error.
// Passing a lambda that returns a string where int64 is expected triggers the
// conversion panic path in callbackSuccessResult.
func TestRegisterFuncCallbackResultConversionPanicToError(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("apply-f", func(f func(int64) int64, n int64) int64 {
		return f(n)
	})
	if err != nil {
		t.Fatal(err)
	}

	ctx := context.Background()
	_, evalErr := engine.Eval(ctx, `(apply-f (lambda (x) "not-a-number") 5)`)
	if evalErr == nil {
		t.Fatal("expected error from callback result conversion, got nil")
	}
	if !errors.Is(evalErr, values.ErrCallbackResultConversion) {
		t.Errorf("expected errors.Is(err, ErrCallbackResultConversion) = true, got false\nerror: %v", evalErr)
	}
}

// --- Parameter as callback ---

func TestRegisterFuncParameterAsCallback(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Register a function that calls a 0-arg callback to get a value.
	err := engine.RegisterFunc("call-getter", func(f func() int64) int64 {
		return f()
	})
	c.Assert(err, qt.IsNil)

	// Register a function that calls a 1-arg callback to set a value.
	err = engine.RegisterFunc("call-setter", func(f func(int64)) {
		f(99)
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"parameter get", `(let ((p (make-parameter 42))) (call-getter p))`, "42"},
		{"parameter set then get", `(let ((p (make-parameter 0))) (call-setter p) (p))`, "99"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Composite round-trip ---

func TestRegisterFuncCompositeRoundTrip(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	type record struct {
		Name   string
		Values []int64
	}

	err := engine.RegisterFunc("identity-record", func(r record) record {
		return r
	})
	c.Assert(err, qt.IsNil)

	result := eval(t, engine, `(identity-record '((Name . "test") (Values . (1 2 3))))`)
	// (Values . (1 2 3)) and (Values 1 2 3) are the same structure;
	// Scheme prints the compact form.
	c.Assert(result.SchemeString(), qt.Equals, `((Name . "test") (Values 1 2 3))`)
}

// --- Named scalar types ---

type myInt int64
type myFloat float64
type myString string
type myBool bool

func TestRegisterFuncNamedScalarTypes(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFunc("named-int", func(n myInt) myInt {
		return n * 2
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("named-float", func(f myFloat) myFloat {
		return f + 1.0
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("named-string", func(s myString) myString {
		return "hello-" + s
	})
	c.Assert(err, qt.IsNil)

	err = engine.RegisterFunc("named-bool", func(b myBool) myBool {
		return !b
	})
	c.Assert(err, qt.IsNil)

	tcs := []struct {
		name string
		code string
		want string
	}{
		{"named int64", "(named-int 21)", "42"},
		{"named float64", "(named-float 2.5)", "3.5"},
		{"named string", `(named-string "world")`, `"hello-world"`},
		{"named bool", "(named-bool #t)", "#f"},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// --- Non-list to slice/struct converter errors ---

func TestRegisterFuncNonListToComposite(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFunc("need-list", func(nums []int64) int64 {
		var total int64
		for _, n := range nums {
			total += n
		}
		return total
	})
	if err != nil {
		t.Fatal(err)
	}

	err = engine.RegisterFunc("need-struct", func(p testPerson) string {
		return p.Name
	})
	if err != nil {
		t.Fatal(err)
	}

	tcs := []struct {
		name string
		code string
	}{
		{"integer to slice", `(need-list 42)`},
		{"string to slice", `(need-list "hello")`},
		{"boolean to slice", `(need-list #t)`},
		{"integer to struct", `(need-struct 42)`},
		{"string to struct", `(need-struct "hello")`},
		{"boolean to struct", `(need-struct #f)`},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- RegisterFuncs batch registration ---

func TestRegisterFuncs(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFuncs(map[string]any{
		"inc": func(n int64) int64 {
			return n + 1
		},
		"dec": func(n int64) int64 {
			return n - 1
		},
		"greet": func(name string) string {
			return "hello, " + name
		},
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

func TestRegisterFuncsFailFast(t *testing.T) {
	engine := newEngine(t)

	err := engine.RegisterFuncs(map[string]any{
		"good": func(n int64) int64 {
			return n
		},
		"bad": 42, // not a function
	})
	if err == nil {
		t.Fatal("expected error from RegisterFuncs with non-function value")
	}

	if !errors.Is(err, values.ErrFFIRegistration) {
		t.Fatalf("expected ErrFFIRegistration, got %T: %v", err, err)
	}
	if !strings.Contains(err.Error(), "bad") {
		t.Errorf("expected error to mention binding name %q, got: %v", "bad", err)
	}

	// Verify that "good" was registered despite the error (if it was iterated first).
	// Since map order is non-deterministic, "good" may or may not be registered.
	// We only check that calling it doesn't panic — it either works or returns an error.
	_, _ = engine.Eval(context.Background(), "(good 5)")
}

func TestRegisterFuncsEmpty(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFuncs(map[string]any{})
	c.Assert(err, qt.IsNil)
}

func TestRegisterFuncsNil(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	err := engine.RegisterFuncs(nil)
	c.Assert(err, qt.IsNil)
}
