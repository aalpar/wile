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
	"testing"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// AtomicBox Primitives Tests
// ----------------------------------------------------------------------------

func TestAtomicQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "atomic? on atomic object",
			code: "(atomic? (make-atomic 0))",
			out:  values.TrueValue,
		},
		{
			name: "atomic? on integer",
			code: "(atomic? 42)",
			out:  values.FalseValue,
		},
		{
			name: "atomic? on mutex",
			code: "(atomic? (make-mutex))",
			out:  values.FalseValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}

func TestMakeAtomic(t *testing.T) {
	result, err := testhelpers.RunSchemeCode(t, "(atomic? (make-atomic 42))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestAtomicLoad(t *testing.T) {
	code := "(atomic-load (make-atomic 42))"
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestAtomicLoadError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(atomic-load 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicStore(t *testing.T) {
	code := `
		(let ((a (make-atomic 0)))
			(atomic-store! a 100)
			(atomic-load a))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(100))
}

func TestAtomicStoreError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(atomic-store! 42 100)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicSwap(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-swap! a 'new))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("old"))
}

func TestAtomicSwapUpdatesValue(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-swap! a 'new)
			(atomic-load a))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("new"))
}

func TestAtomicSwapError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(atomic-swap! 42 'new)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicCompareAndSwapSuccess(t *testing.T) {
	// CAS uses Go's atomic.Value.CompareAndSwap (pointer comparison),
	// so we must load the value first to get the same pointer.
	code := `
		(let ((a (make-atomic 'old)))
			(let ((old (atomic-load a)))
				(atomic-compare-and-swap! a old 'new)))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestAtomicCompareAndSwapFailure(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'wrong 'new))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.FalseValue)
}

func TestAtomicCompareAndSwapUpdatesOnSuccess(t *testing.T) {
	// CAS uses Go's atomic.Value.CompareAndSwap (pointer comparison),
	// so we must load the value first to get the same pointer.
	code := `
		(let ((a (make-atomic 'old)))
			(let ((old (atomic-load a)))
				(atomic-compare-and-swap! a old 'new)
				(atomic-load a)))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("new"))
}

func TestAtomicCompareAndSwapNoUpdateOnFailure(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'wrong 'new)
			(atomic-load a))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("old"))
}

func TestAtomicCompareAndSwapError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(atomic-compare-and-swap! 42 'old 'new)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicWithDifferentTypes(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "atomic with integer",
			code: "(atomic-load (make-atomic 42))",
			out:  values.NewInteger(42),
		},
		{
			name: "atomic with string",
			code: `(atomic-load (make-atomic "hello"))`,
			out:  values.NewString("hello"),
		},
		{
			name: "atomic with symbol",
			code: "(atomic-load (make-atomic 'test))",
			out:  values.NewSymbol("test"),
		},
		{
			name: "atomic with list",
			code: "(atomic-load (make-atomic '(1 2 3)))",
			out:  values.List(values.NewInteger(1), values.NewInteger(2), values.NewInteger(3)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := testhelpers.RunSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.out)
		})
	}
}
