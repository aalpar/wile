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

package all_test

import (
	"testing"

	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// --- Case-insensitive comparison ---

func TestStringCiEq(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"same case", `(string-ci=? "abc" "abc")`, values.TrueValue},
		{"different case", `(string-ci=? "abc" "ABC")`, values.TrueValue},
		{"mixed case", `(string-ci=? "Hello" "hELLO")`, values.TrueValue},
		{"not equal", `(string-ci=? "abc" "abd")`, values.FalseValue},
		{"different lengths", `(string-ci=? "abc" "ab")`, values.FalseValue},
		{"empty strings", `(string-ci=? "" "")`, values.TrueValue},
		{"variadic three equal", `(string-ci=? "abc" "ABC" "Abc")`, values.TrueValue},
		{"variadic not all equal", `(string-ci=? "abc" "ABC" "abd")`, values.FalseValue},
		{"single arg vacuous true", `(string-ci=? "abc")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"wrong type", `(string-ci=? "abc" 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestStringCiLt(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"less than", `(string-ci<? "abc" "DEF")`, values.TrueValue},
		{"equal not less", `(string-ci<? "abc" "ABC")`, values.FalseValue},
		{"greater not less", `(string-ci<? "def" "ABC")`, values.FalseValue},
		{"variadic ascending", `(string-ci<? "abc" "DEF" "ghi")`, values.TrueValue},
		{"variadic not ascending", `(string-ci<? "abc" "DEF" "def")`, values.FalseValue},
		{"prefix is less", `(string-ci<? "ab" "ABC")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestStringCiGt(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"greater than", `(string-ci>? "def" "ABC")`, values.TrueValue},
		{"equal not greater", `(string-ci>? "abc" "ABC")`, values.FalseValue},
		{"variadic descending", `(string-ci>? "ghi" "DEF" "abc")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestStringCiLe(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"less than", `(string-ci<=? "abc" "DEF")`, values.TrueValue},
		{"equal", `(string-ci<=? "abc" "ABC")`, values.TrueValue},
		{"greater", `(string-ci<=? "def" "ABC")`, values.FalseValue},
		{"variadic non-decreasing", `(string-ci<=? "abc" "ABC" "def")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestStringCiGe(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"greater than", `(string-ci>=? "def" "ABC")`, values.TrueValue},
		{"equal", `(string-ci>=? "abc" "ABC")`, values.TrueValue},
		{"less", `(string-ci>=? "abc" "DEF")`, values.FalseValue},
		{"variadic non-increasing", `(string-ci>=? "def" "ABC" "abc")`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// --- Case mapping ---

func TestStringUpcase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"lowercase", `(string-upcase "hello")`, values.NewString("HELLO")},
		{"already upper", `(string-upcase "HELLO")`, values.NewString("HELLO")},
		{"mixed", `(string-upcase "Hello World")`, values.NewString("HELLO WORLD")},
		{"empty", `(string-upcase "")`, values.NewString("")},
		{"digits unchanged", `(string-upcase "abc123")`, values.NewString("ABC123")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(string-upcase 42)`)
	})
}

func TestStringDowncase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"uppercase", `(string-downcase "HELLO")`, values.NewString("hello")},
		{"already lower", `(string-downcase "hello")`, values.NewString("hello")},
		{"mixed", `(string-downcase "Hello World")`, values.NewString("hello world")},
		{"empty", `(string-downcase "")`, values.NewString("")},
		{"digits unchanged", `(string-downcase "ABC123")`, values.NewString("abc123")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}
}

func TestStringFoldcase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"uppercase folds", `(string-foldcase "HELLO")`, values.NewString("hello")},
		{"lowercase stays", `(string-foldcase "hello")`, values.NewString("hello")},
		{"mixed folds", `(string-foldcase "Hello World")`, values.NewString("hello world")},
		{"empty", `(string-foldcase "")`, values.NewString("")},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	// R7RS: (string-ci=? (string-foldcase s1) (string-foldcase s2)) = (string-ci=? s1 s2)
	t.Run("foldcase is idempotent", func(t *testing.T) {
		result := eval(t, engine, `(string=? (string-foldcase (string-foldcase "HeLLo"))
		                                      (string-foldcase "HeLLo"))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})
}

// --- string-copy! ---

func TestStringCopyTo(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			"basic copy",
			`(let ((s (string-copy "aaaaa"))) (string-copy! s 0 "hello") s)`,
			values.NewString("hello"),
		},
		{
			"copy at offset",
			`(let ((s (string-copy "aaaaa"))) (string-copy! s 2 "xy") s)`,
			values.NewString("aaxy" + "a"),
		},
		{
			"copy with start",
			`(let ((s (string-copy "aaaaa"))) (string-copy! s 0 "hello" 1) s)`,
			values.NewString("elloa"),
		},
		{
			"copy with start and end",
			`(let ((s (string-copy "aaaaa"))) (string-copy! s 1 "hello" 1 3) s)`,
			values.NewString("aelaa"),
		},
		{
			"copy zero-length range",
			`(let ((s (string-copy "hello"))) (string-copy! s 0 "xyz" 1 1) s)`,
			values.NewString("hello"),
		},
		{
			"copy entire source",
			`(let ((s (string-copy "xxxxx"))) (string-copy! s 0 "abcde") s)`,
			values.NewString("abcde"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"destination out of bounds", `(let ((s (string-copy "abc"))) (string-copy! s 2 "hello"))`},
		{"negative at", `(let ((s (string-copy "abc"))) (string-copy! s -1 "x"))`},
		{"invalid source range", `(let ((s (string-copy "abc"))) (string-copy! s 0 "hello" 3 1))`},
		{"wrong type dest", `(string-copy! 42 0 "abc")`},
		{"wrong type source", `(let ((s (string-copy "abc"))) (string-copy! s 0 42))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- string-fill! ---

func TestStringFill(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			"fill entire string",
			`(let ((s (string-copy "hello"))) (string-fill! s #\x) s)`,
			values.NewString("xxxxx"),
		},
		{
			"fill with start",
			`(let ((s (string-copy "hello"))) (string-fill! s #\x 2) s)`,
			values.NewString("hexxx"),
		},
		{
			"fill with start and end",
			`(let ((s (string-copy "hello"))) (string-fill! s #\x 1 3) s)`,
			values.NewString("hxxlo"),
		},
		{
			"fill zero-length range",
			`(let ((s (string-copy "hello"))) (string-fill! s #\x 2 2) s)`,
			values.NewString("hello"),
		},
		{
			"fill empty string",
			`(let ((s (string-copy ""))) (string-fill! s #\x) s)`,
			values.NewString(""),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"invalid range", `(let ((s (string-copy "hello"))) (string-fill! s #\x 3 1))`},
		{"out of bounds", `(let ((s (string-copy "abc"))) (string-fill! s #\x 0 5))`},
		{"wrong type string", `(string-fill! 42 #\x)`},
		{"wrong type char", `(let ((s (string-copy "abc"))) (string-fill! s "x"))`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- string-map ---

func TestStringMap(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			"upcase via map",
			`(string-map char-upcase "hello")`,
			values.NewString("HELLO"),
		},
		{
			"identity",
			`(string-map (lambda (c) c) "hello")`,
			values.NewString("hello"),
		},
		{
			"empty string",
			`(string-map char-upcase "")`,
			values.NewString(""),
		},
		{
			"two strings min length",
			// With two strings of different lengths, operates on min length.
			// This lambda takes two chars and returns the first.
			`(string-map (lambda (a b) a) "abcde" "xyz")`,
			values.NewString("abc"),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a procedure", `(string-map 42 "hello")`},
		{"not a string", `(string-map char-upcase 42)`},
		{"no strings", `(string-map char-upcase)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// --- string-for-each ---

func TestStringForEach(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{
			"accumulate chars",
			// Use a mutable string to accumulate characters visited.
			`(let ((acc (string-copy "")))
			   (string-for-each
			     (lambda (c) (set! acc (string-append acc (string c))))
			     "abc")
			   acc)`,
			values.NewString("abc"),
		},
		{
			"empty string no-op",
			`(let ((count 0))
			   (string-for-each (lambda (c) (set! count (+ count 1))) "")
			   count)`,
			values.NewInteger(0),
		},
		{
			"counts characters",
			`(let ((count 0))
			   (string-for-each (lambda (c) (set! count (+ count 1))) "hello")
			   count)`,
			values.NewInteger(5),
		},
		{
			"two strings min length",
			`(let ((count 0))
			   (string-for-each (lambda (a b) (set! count (+ count 1))) "abcde" "xy")
			   count)`,
			values.NewInteger(2),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), values.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"not a procedure", `(string-for-each 42 "hello")`},
		{"not a string", `(string-for-each (lambda (c) c) 42)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}
