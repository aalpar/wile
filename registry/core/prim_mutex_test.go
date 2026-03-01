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
	"time"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// SRFI-18 Mutex Primitives Tests
// ----------------------------------------------------------------------------

func TestMutexQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "mutex? on mutex object",
			code: "(mutex? (make-mutex))",
			out:  values.TrueValue,
		},
		{
			name: "mutex? on integer",
			code: "(mutex? 42)",
			out:  values.FalseValue,
		},
		{
			name: "mutex? on string",
			code: `(mutex? "mutex")`,
			out:  values.FalseValue,
		},
		{
			name: "mutex? on thread",
			code: "(mutex? (make-thread (lambda () 42)))",
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

func TestMakeMutex(t *testing.T) {
	// make-mutex should return a mutex
	result, err := testhelpers.RunSchemeCode(t, "(mutex? (make-mutex))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMakeMutexWithName(t *testing.T) {
	code := `(mutex-name (make-mutex "test-mutex"))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewString("test-mutex"))
}

func TestMutexName(t *testing.T) {
	code := `(mutex-name (make-mutex "my-mutex"))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewString("my-mutex"))
}

func TestMutexNameError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-name 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexSpecific(t *testing.T) {
	// Initially mutex-specific returns void
	code := `(void? (mutex-specific (make-mutex)))`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMutexSpecificSet(t *testing.T) {
	code := `
		(let ((m (make-mutex)))
			(mutex-specific-set! m 'my-data)
			(mutex-specific m))
	`
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("my-data"))
}

func TestMutexSpecificError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-specific 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexSpecificSetError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-specific-set! 42 'data)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexState(t *testing.T) {
	// New mutex should be not-owned
	code := "(mutex-state (make-mutex))"
	result, err := testhelpers.RunSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewSymbol("not-owned"))
}

func TestMutexStateError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-state 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexLockAndUnlock(t *testing.T) {
	code := `
		(let ((m (make-mutex)))
			(mutex-lock! m)
			(mutex-unlock! m)
			#t)
	`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMutexLockReturnsTrue(t *testing.T) {
	code := "(mutex-lock! (make-mutex))"
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMutexLockError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-lock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexUnlockError(t *testing.T) {
	_, err := testhelpers.RunSchemeCode(t, "(mutex-unlock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestMutexLockWithTimeout(t *testing.T) {
	// Lock with timeout 0 should return immediately
	code := `
		(let ((m (make-mutex)))
			(mutex-lock! m 0))
	`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	// Should succeed since mutex is available
	qt.Assert(t, result, valuestest.SchemeEquals, values.TrueValue)
}

func TestMutexProtectsCriticalSection(t *testing.T) {
	// Test that mutex properly serializes access
	code := `
		(let ((m (make-mutex))
		      (counter 0))
			(mutex-lock! m)
			(set! counter (+ counter 1))
			(mutex-unlock! m)
			counter)
	`
	result, err := testhelpers.RunSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, valuestest.SchemeEquals, values.NewInteger(1))
}
