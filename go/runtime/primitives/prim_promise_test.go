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

package primitives_test

import (
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// Promise Tests
// ----------------------------------------------------------------------------

func TestPromiseQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "promise? on delayed value",
			code: "(promise? (delay 1))",
			out:  values.TrueValue,
		},
		{
			name: "promise? on non-promise",
			code: "(promise? 1)",
			out:  values.FalseValue,
		},
		{
			name: "promise? on list",
			code: "(promise? '(1 2 3))",
			out:  values.FalseValue,
		},
		{
			name: "promise? on string",
			code: `(promise? "hello")`,
			out:  values.FalseValue,
		},
		{
			name: "promise? on make-promise",
			code: "(promise? (make-promise 42))",
			out:  values.TrueValue,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMakePromise(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "make-promise on value",
			code: "(promise? (make-promise 1))",
			out:  values.TrueValue,
		},
		{
			name: "make-promise on promise returns promise",
			code: "(promise? (make-promise (delay 1)))",
			out:  values.TrueValue,
		},
		{
			name: "force make-promise returns value",
			code: "(force (make-promise 42))",
			out:  values.NewInteger(42),
		},
		{
			name: "make-promise wraps non-promise",
			code: "(force (make-promise (+ 1 2)))",
			out:  values.NewInteger(3),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestForce(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "force delayed arithmetic",
			code: "(force (delay (+ 1 2)))",
			out:  values.NewInteger(3),
		},
		{
			name: "force delayed value",
			code: "(force (delay 42))",
			out:  values.NewInteger(42),
		},
		{
			name: "force non-promise returns value",
			code: "(force 5)",
			out:  values.NewInteger(5),
		},
		{
			name: "force cached promise",
			code: "((lambda (p) (force p) (force p)) (delay (+ 1 2)))",
			out:  values.NewInteger(3),
		},
		{
			name: "force nested delay",
			code: "(force (delay (delay 10)))",
			out:  values.NewInteger(10),
		},
		{
			name: "force with list operations",
			code: "(force (delay (cons 1 (cons 2 '()))))",
			out:  values.List(values.NewInteger(1), values.NewInteger(2)),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestDelayForce(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "delay-force simple case",
			code: "(force (delay-force (make-promise 5)))",
			out:  values.NewInteger(5),
		},
		{
			name: "delay-force with delay",
			code: "(force (delay-force (delay 10)))",
			out:  values.NewInteger(10),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
