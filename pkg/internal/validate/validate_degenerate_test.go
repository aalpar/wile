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

package validate_test

import (
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// degenerateErrorCase tests that a degenerate form produces an error
// through the full pipeline (string -> tokenize -> parse -> expand -> compile -> run).
// WantErr, if non-empty, checks that the error message contains the substring.
type degenerateErrorCase struct {
	Name    string
	Code    string
	WantErr string
}

func runDegenerateErrorTests(t *testing.T, tcs []degenerateErrorCase) {
	t.Helper()
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			err := testhelpers.RunSchemeCodeExpectError(t, tc.Code)
			if err == nil {
				t.Fatalf("expected error for %q but got nil", tc.Code)
			}
			if tc.WantErr != "" {
				qt.Assert(t, err.Error(), qt.Contains, tc.WantErr)
			}
		})
	}
}

// ============================================================================
// if — degenerate forms
// ============================================================================

func TestIf_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(if)`, WantErr: "if"},
		{Name: "test only", Code: `(if #t)`, WantErr: "if"},
		{Name: "too many args", Code: `(if #t 1 2 3)`, WantErr: "if"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// set! — degenerate forms
// ============================================================================

func TestSetBang_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(set!)`, WantErr: "set!"},
		{Name: "missing value", Code: `(begin (define x 0) (set! x))`, WantErr: "set!"},
		{Name: "non-symbol target", Code: `(set! 42 1)`, WantErr: "set!"},
		{Name: "too many args", Code: `(begin (define x 0) (set! x 1 2))`, WantErr: "set!"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// quote — degenerate forms
// ============================================================================

func TestQuote_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(quote)`, WantErr: "quote"},
		{Name: "too many args", Code: `(quote a b)`, WantErr: "quote"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// quasiquote — degenerate forms
// ============================================================================

func TestQuasiquote_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(quasiquote)`, WantErr: "quasiquote"},
		{Name: "too many args", Code: `(quasiquote a b)`, WantErr: "quasiquote"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// dynamic-wind — degenerate forms
// ============================================================================

func TestDynamicWind_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(dynamic-wind)`, WantErr: "dynamic-wind"},
		{Name: "one arg", Code: `(dynamic-wind (lambda () #f))`, WantErr: "dynamic-wind"},
		{Name: "two args", Code: `(dynamic-wind (lambda () #f) (lambda () #f))`, WantErr: "dynamic-wind"},
		{Name: "four args", Code: `(dynamic-wind (lambda () #f) (lambda () #f) (lambda () #f) (lambda () #f))`, WantErr: "dynamic-wind"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// with-continuation-mark — degenerate forms
// ============================================================================

func TestWithContinuationMark_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(with-continuation-mark)`, WantErr: "with-continuation-mark"},
		{Name: "one arg", Code: `(with-continuation-mark 'key)`, WantErr: "with-continuation-mark"},
		{Name: "two args", Code: `(with-continuation-mark 'key 'val)`, WantErr: "with-continuation-mark"},
		{Name: "four args", Code: `(with-continuation-mark 'key 'val 1 2)`, WantErr: "with-continuation-mark"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// apply — degenerate forms (syntax validation)
// ============================================================================

func TestApplySyntax_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(apply)`, WantErr: "apply"},
		{Name: "one arg", Code: `(apply +)`, WantErr: "apply"},
	}
	runDegenerateErrorTests(t, tcs)
}

// ============================================================================
// begin — valid degenerate form
// ============================================================================

func TestBegin_Degenerate(t *testing.T) {
	// (begin) is valid per R7RS; its value is unspecified. This implementation returns void.
	result, err := testhelpers.RunSchemeCode(t, `(begin)`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.Void)
}

// ============================================================================
// and — valid degenerate forms (R7RS §4.2.1)
// ============================================================================

func TestAnd_Degenerate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no args returns true", Code: `(and)`, Expected: values.TrueValue},
		{Name: "single arg returns it", Code: `(and 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// or — valid degenerate forms (R7RS §4.2.1)
// ============================================================================

func TestOr_Degenerate(t *testing.T) {
	tcs := []testhelpers.SchemeCodeTestCase{
		{Name: "no args returns false", Code: `(or)`, Expected: values.FalseValue},
		{Name: "single arg returns it", Code: `(or 42)`, Expected: values.NewInteger(42)},
	}
	for _, tc := range tcs {
		t.Run(tc.Name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.Code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.Expected)
		})
	}
}

// ============================================================================
// Macro-based derived forms — degenerate inputs
//
// All macros below are syntax-rules definitions in bootstrap_macros.scm.
// When no clause matches, the error is:
//   "syntax-rules: no matching clause for input"
// ============================================================================

func TestCond_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no clauses", Code: `(cond)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestCase_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(case)`, WantErr: "no matching clause"},
		{Name: "key only", Code: `(case 1)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestWhen_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(when)`, WantErr: "no matching clause"},
		{Name: "test only no body", Code: `(when #t)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestUnless_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(unless)`, WantErr: "no matching clause"},
		{Name: "test only no body", Code: `(unless #f)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDo_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(do)`, WantErr: "no matching clause"},
		{Name: "bindings only", Code: `(do ())`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestGuard_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(guard)`, WantErr: "no matching clause"},
		{Name: "var only", Code: `(guard (e))`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestParameterize_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(parameterize)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDelay_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(delay)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDelayForce_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(delay-force)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDefineRecordType_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(define-record-type)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestLetValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(let-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestLetStarValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(let*-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}

func TestDefineValues_Degenerate(t *testing.T) {
	tcs := []degenerateErrorCase{
		{Name: "no args", Code: `(define-values)`, WantErr: "no matching clause"},
	}
	runDegenerateErrorTests(t, tcs)
}
