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
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with all standard extensions loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.New(t).Assert(err, qt.IsNotNil)
}

// --- Case-insensitive comparison ---

func TestCharCiEq(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"same case", `(char-ci=? #\a #\a)`, values.TrueValue},
		{"different case", `(char-ci=? #\a #\A)`, values.TrueValue},
		{"not equal", `(char-ci=? #\a #\b)`, values.FalseValue},
		{"variadic three equal", `(char-ci=? #\A #\a #\A)`, values.TrueValue},
		{"variadic not all equal", `(char-ci=? #\a #\A #\b)`, values.FalseValue},
		{"non-alpha characters", `(char-ci=? #\1 #\1)`, values.TrueValue},
		{"single arg vacuous true", `(char-ci=? #\a)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(char-ci=? #\a 42)`)
	})
}

func TestCharCiLt(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"less than", `(char-ci<? #\a #\B)`, values.TrueValue},
		{"equal not less", `(char-ci<? #\a #\A)`, values.FalseValue},
		{"greater not less", `(char-ci<? #\b #\A)`, values.FalseValue},
		{"variadic ascending", `(char-ci<? #\a #\B #\c)`, values.TrueValue},
		{"variadic not strictly ascending", `(char-ci<? #\a #\B #\b)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharCiGt(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"greater than", `(char-ci>? #\b #\A)`, values.TrueValue},
		{"equal not greater", `(char-ci>? #\a #\A)`, values.FalseValue},
		{"variadic descending", `(char-ci>? #\c #\B #\a)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharCiLe(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"less than", `(char-ci<=? #\a #\B)`, values.TrueValue},
		{"equal", `(char-ci<=? #\a #\A)`, values.TrueValue},
		{"greater", `(char-ci<=? #\b #\A)`, values.FalseValue},
		{"variadic non-decreasing", `(char-ci<=? #\a #\A #\b)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharCiGe(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"greater than", `(char-ci>=? #\b #\A)`, values.TrueValue},
		{"equal", `(char-ci>=? #\a #\A)`, values.TrueValue},
		{"less", `(char-ci>=? #\a #\B)`, values.FalseValue},
		{"variadic non-increasing", `(char-ci>=? #\b #\A #\a)`, values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// --- Classification predicates ---

func TestCharAlphabeticQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"lowercase letter", `(char-alphabetic? #\a)`, values.TrueValue},
		{"uppercase letter", `(char-alphabetic? #\Z)`, values.TrueValue},
		{"digit", `(char-alphabetic? #\0)`, values.FalseValue},
		{"space", `(char-alphabetic? #\space)`, values.FalseValue},
		{"unicode greek alpha", "(char-alphabetic? #\\α)", values.TrueValue},
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
		{"wrong type", `(char-alphabetic? "a")`},
		{"wrong arity", `(char-alphabetic? #\a #\b)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestCharNumericQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"digit 5", `(char-numeric? #\5)`, values.TrueValue},
		{"zero", `(char-numeric? #\0)`, values.TrueValue},
		{"nine", `(char-numeric? #\9)`, values.TrueValue},
		{"letter", `(char-numeric? #\a)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharWhitespaceQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"space", `(char-whitespace? #\space)`, values.TrueValue},
		{"tab", `(char-whitespace? #\tab)`, values.TrueValue},
		{"newline", `(char-whitespace? #\newline)`, values.TrueValue},
		{"letter", `(char-whitespace? #\a)`, values.FalseValue},
		{"digit", `(char-whitespace? #\0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharUpperCaseQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"uppercase", `(char-upper-case? #\A)`, values.TrueValue},
		{"lowercase", `(char-upper-case? #\a)`, values.FalseValue},
		{"digit", `(char-upper-case? #\0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestCharLowerCaseQ(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"lowercase", `(char-lower-case? #\a)`, values.TrueValue},
		{"uppercase", `(char-lower-case? #\A)`, values.FalseValue},
		{"digit", `(char-lower-case? #\0)`, values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

// --- Case mapping ---

func TestCharUpcase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"lowercase to uppercase", `(char-upcase #\a)`, values.NewCharacter('A')},
		{"already uppercase", `(char-upcase #\A)`, values.NewCharacter('A')},
		{"digit unchanged", `(char-upcase #\5)`, values.NewCharacter('5')},
		{"space unchanged", `(char-upcase #\space)`, values.NewCharacter(' ')},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(char-upcase "a")`)
	})
}

func TestCharDowncase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"uppercase to lowercase", `(char-downcase #\A)`, values.NewCharacter('a')},
		{"already lowercase", `(char-downcase #\a)`, values.NewCharacter('a')},
		{"digit unchanged", `(char-downcase #\5)`, values.NewCharacter('5')},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}
}

func TestCharFoldcase(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		{"uppercase folds to lowercase", `(char-foldcase #\A)`, values.NewCharacter('a')},
		{"lowercase stays", `(char-foldcase #\a)`, values.NewCharacter('a')},
		{"digit unchanged", `(char-foldcase #\5)`, values.NewCharacter('5')},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	// R7RS: (char-foldcase (char-foldcase c)) = (char-foldcase c)
	t.Run("foldcase is idempotent", func(t *testing.T) {
		result := eval(t, engine, `(char=? (char-foldcase (char-foldcase #\Z)) (char-foldcase #\Z))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})
}

// --- digit-value ---

func TestDigitValue(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// ASCII digits 0-9
		{"ascii 0", `(digit-value #\0)`, values.NewInteger(0)},
		{"ascii 1", `(digit-value #\1)`, values.NewInteger(1)},
		{"ascii 2", `(digit-value #\2)`, values.NewInteger(2)},
		{"ascii 3", `(digit-value #\3)`, values.NewInteger(3)},
		{"ascii 4", `(digit-value #\4)`, values.NewInteger(4)},
		{"ascii 5", `(digit-value #\5)`, values.NewInteger(5)},
		{"ascii 6", `(digit-value #\6)`, values.NewInteger(6)},
		{"ascii 7", `(digit-value #\7)`, values.NewInteger(7)},
		{"ascii 8", `(digit-value #\8)`, values.NewInteger(8)},
		{"ascii 9", `(digit-value #\9)`, values.NewInteger(9)},

		// Non-digits return #f
		{"letter", `(digit-value #\a)`, values.FalseValue},
		{"space", `(digit-value #\space)`, values.FalseValue},

		// Unicode decimal digit scripts
		{"arabic-indic zero U+0660", "(digit-value #\\٠)", values.NewInteger(0)},
		{"arabic-indic five U+0665", "(digit-value #\\٥)", values.NewInteger(5)},
		{"devanagari zero U+0966", "(digit-value #\\०)", values.NewInteger(0)},
		{"devanagari nine U+096F", "(digit-value #\\९)", values.NewInteger(9)},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), valuestest.SchemeEquals, tc.want)
		})
	}

	errs := []struct {
		name string
		code string
	}{
		{"wrong type", `(digit-value 5)`},
		{"wrong arity", `(digit-value #\0 #\1)`},
	}
	for _, tc := range errs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

func TestCharCiOrderingEdgeCases(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	// Test capital sharp S (ẞ U+1E9E) which folds to lowercase sharp s (ß U+00DF)
	t.Run("capital sharp S", func(t *testing.T) {
		tcs := []struct {
			name string
			code string
			want values.Value
		}{
			{"ẞ equals ß", `(char-ci=? #\ẞ #\ß)`, values.TrueValue},
			{"ẞ not less than ß", `(char-ci<? #\ẞ #\ß)`, values.FalseValue},
			{"ẞ not greater than ß", `(char-ci>? #\ẞ #\ß)`, values.FalseValue},
			{"ẞ <= ß", `(char-ci<=? #\ẞ #\ß)`, values.TrueValue},
			{"ẞ >= ß", `(char-ci>=? #\ẞ #\ß)`, values.TrueValue},
		}
		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				result := eval(t, engine, tc.code)
				c.Assert(result.Internal(), qt.Equals, tc.want)
			})
		}
	})

	t.Run("consistency with char-foldcase", func(t *testing.T) {
		// R7RS: char-ci comparisons should use char-foldcase semantics
		tcs := []struct {
			name string
			code string
		}{
			{
				"A vs a",
				`(eq? (char-ci<? #\A #\a)
				      (char<? (char-foldcase #\A)
				             (char-foldcase #\a)))`,
			},
			{
				"Z vs z",
				`(eq? (char-ci<? #\Z #\z)
				      (char<? (char-foldcase #\Z)
				             (char-foldcase #\z)))`,
			},
			{
				"ẞ vs ß",
				`(eq? (char-ci<? #\ẞ #\ß)
				      (char<? (char-foldcase #\ẞ)
				             (char-foldcase #\ß)))`,
			},
		}
		for _, tc := range tcs {
			t.Run(tc.name, func(t *testing.T) {
				result := eval(t, engine, tc.code)
				c.Assert(result.Internal(), qt.Equals, values.TrueValue)
			})
		}
	})
}
