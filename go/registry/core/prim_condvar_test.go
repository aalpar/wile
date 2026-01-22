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
	"testing"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// SRFI-18 Condition Variable Primitives Tests
// ----------------------------------------------------------------------------

func TestConditionVariableQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "condition-variable? on condition-variable object",
			code: "(condition-variable? (make-condition-variable))",
			out:  values.TrueValue,
		},
		{
			name: "condition-variable? on integer",
			code: "(condition-variable? 42)",
			out:  values.FalseValue,
		},
		{
			name: "condition-variable? on string",
			code: `(condition-variable? "cv")`,
			out:  values.FalseValue,
		},
		{
			name: "condition-variable? on mutex",
			code: "(condition-variable? (make-mutex))",
			out:  values.FalseValue,
		},
		{
			name: "condition-variable? on thread",
			code: "(condition-variable? (make-thread (lambda () 42)))",
			out:  values.FalseValue,
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

func TestMakeConditionVariable(t *testing.T) {
	// make-condition-variable should return a condition-variable
	result, err := runSchemeCode(t, "(condition-variable? (make-condition-variable))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestMakeConditionVariableWithName(t *testing.T) {
	code := `(condition-variable-name (make-condition-variable "test-cv"))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewString("test-cv"))
}

func TestConditionVariableName(t *testing.T) {
	code := `(condition-variable-name (make-condition-variable "my-cv"))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewString("my-cv"))
}

func TestConditionVariableNameError(t *testing.T) {
	_, err := runSchemeCode(t, "(condition-variable-name 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestConditionVariableSpecific(t *testing.T) {
	// Initially condition-variable-specific returns void
	code := `(void? (condition-variable-specific (make-condition-variable)))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestConditionVariableSpecificSet(t *testing.T) {
	code := `
		(let ((cv (make-condition-variable)))
			(condition-variable-specific-set! cv 'my-data)
			(condition-variable-specific cv))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("my-data"))
}

func TestConditionVariableSpecificError(t *testing.T) {
	_, err := runSchemeCode(t, "(condition-variable-specific 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestConditionVariableSpecificSetError(t *testing.T) {
	_, err := runSchemeCode(t, "(condition-variable-specific-set! 42 'data)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestConditionVariableSignal(t *testing.T) {
	// signal should return void
	code := `
		(let ((cv (make-condition-variable)))
			(condition-variable-signal! cv))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestConditionVariableSignalError(t *testing.T) {
	_, err := runSchemeCode(t, "(condition-variable-signal! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestConditionVariableBroadcast(t *testing.T) {
	// broadcast should return void
	code := `
		(let ((cv (make-condition-variable)))
			(condition-variable-broadcast! cv))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestConditionVariableBroadcastError(t *testing.T) {
	_, err := runSchemeCode(t, "(condition-variable-broadcast! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}
