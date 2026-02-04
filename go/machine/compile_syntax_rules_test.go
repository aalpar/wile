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

package machine_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile/go/machine"
	"github.com/aalpar/wile/go/values"

	qt "github.com/frankban/quicktest"
)

// TestCompileSyntaxRules_RoundTrip tests syntax-rules compilation and execution
// through round-trip define-syntax + macro usage.
//
// R7RS 4.3.2: syntax-rules transformers with pattern matching and template expansion.
func TestCompileSyntaxRules_RoundTrip(t *testing.T) {
	testCases := []struct {
		name      string
		macroCode string
		useCode   string
		expected  values.Value
	}{
		{
			"identity macro",
			`(define-syntax m (syntax-rules () ((m x) x)))`,
			"(m 42)",
			values.NewInteger(42),
		},
		{
			"rewrite with arithmetic",
			`(define-syntax m (syntax-rules () ((m x) (+ x 1))))`,
			"(m 10)",
			values.NewInteger(11),
		},
		{
			"ellipsis capture",
			`(define-syntax m (syntax-rules () ((m x ...) (list x ...))))`,
			"(m 1 2 3)",
			values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
		{
			"literals match",
			`(define-syntax m (syntax-rules (lit) ((m lit x) x) ((m other x) (+ x 1))))`,
			"(m lit 42)",
			values.NewInteger(42),
		},
		{
			"multi-clause two args",
			`(define-syntax m (syntax-rules () ((m) 0) ((m x) x) ((m x y) (+ x y))))`,
			"(m 10 20)",
			values.NewInteger(30),
		},
		{
			"custom ellipsis",
			`(define-syntax m (syntax-rules ::: () ((m x :::) (list x :::))))`,
			"(m 1 2)",
			values.List(values.NewInteger(1), values.NewInteger(2)),
		},
		{
			"nested pattern",
			`(define-syntax m (syntax-rules () ((m (a b)) (+ a b))))`,
			"(m (3 4))",
			values.NewInteger(7),
		},
		{
			"ellipsis in literals disables ellipsis",
			`(define-syntax m (syntax-rules ... () ((m x) x)))`,
			"(m 42)",
			values.NewInteger(42),
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)

			// Define the macro
			sv := parseSchemeExprExt(t, env, tc.macroCode)
			cont, err := newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)

			// Use the macro
			sv = parseSchemeExprExt(t, env, tc.useCode)
			cont, err = newTopLevelThunkExt(sv, env)
			qt.Assert(t, err, qt.IsNil)
			mc = machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, mc.GetValue(), values.SchemeEquals, tc.expected)
		})
	}
}

// TestCompileSyntaxRules_LiteralsNonMatch tests that the second clause of a
// syntax-rules with literals is selected when the literal does not match.
//
// R7RS 4.3.2: literals are matched with bound-identifier=? semantics.
func TestCompileSyntaxRules_LiteralsNonMatch(t *testing.T) {
	env := newFullRuntimeEnv(t)

	// Define a macro with a literal keyword
	sv := parseSchemeExprExt(t, env, `(define-syntax m
		(syntax-rules (lit)
			((m lit x) x)
			((m other x) (+ x 1))))`)
	cont, err := newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc := machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)

	// Use with a non-literal identifier; should match the second clause
	sv = parseSchemeExprExt(t, env, "(m foo 10)")
	cont, err = newTopLevelThunkExt(sv, env)
	qt.Assert(t, err, qt.IsNil)
	mc = machine.NewMachineContext(context.Background(), cont)
	err = mc.Run()
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, mc.GetValue(), values.SchemeEquals, values.NewInteger(11))
}

// TestCompileSyntaxRules_Errors tests error conditions during syntax-rules
// compilation and expansion.
func TestCompileSyntaxRules_Errors(t *testing.T) {
	testCases := []struct {
		name string
		code string
	}{
		{"missing literals list", "(define-syntax m (syntax-rules))"},
		{"non-list literals", "(define-syntax m (syntax-rules bad-literal 42))"},
		{"no clauses", "(define-syntax m (syntax-rules ()))"},
		{"invalid clause not a list", "(define-syntax m (syntax-rules () 42))"},
		{"missing template in clause", "(define-syntax m (syntax-rules () ((m x))))"},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			env := newFullRuntimeEnv(t)
			sv := parseSchemeExprExt(t, env, tc.code)
			cont, err := newTopLevelThunkExt(sv, env)
			if err != nil {
				return // Error during expansion/compilation -- expected
			}
			mc := machine.NewMachineContext(context.Background(), cont)
			err = mc.Run()
			qt.Assert(t, err, qt.IsNotNil)
		})
	}
}
