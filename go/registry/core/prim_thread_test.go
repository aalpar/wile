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
	"time"

	"wile/values"

	qt "github.com/frankban/quicktest"
)

// ----------------------------------------------------------------------------
// SRFI-18 Thread Primitives Tests
// ----------------------------------------------------------------------------

func TestThreadQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "thread? on thread object",
			code: "(thread? (make-thread (lambda () 42)))",
			out:  values.TrueValue,
		},
		{
			name: "thread? on integer",
			code: "(thread? 42)",
			out:  values.FalseValue,
		},
		{
			name: "thread? on string",
			code: `(thread? "thread")`,
			out:  values.FalseValue,
		},
		{
			name: "thread? on lambda",
			code: "(thread? (lambda () 42))",
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

func TestMakeThread(t *testing.T) {
	// make-thread should return a thread
	result, err := runSchemeCode(t, "(thread? (make-thread (lambda () 'done)))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestMakeThreadWithName(t *testing.T) {
	// make-thread with name should preserve the name
	code := `(thread-name (make-thread (lambda () 'done) "test-thread"))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewString("test-thread"))
}

func TestThreadName(t *testing.T) {
	code := `(thread-name (make-thread (lambda () 42) "my-thread"))`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewString("my-thread"))
}

func TestThreadNameError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-name 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadSpecific(t *testing.T) {
	// Initially thread-specific returns void
	code := `
		(let ((th (make-thread (lambda () 42))))
			(void? (thread-specific th)))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestThreadSpecificSet(t *testing.T) {
	code := `
		(let ((th (make-thread (lambda () 42))))
			(thread-specific-set! th 'my-data)
			(thread-specific th))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("my-data"))
}

func TestThreadSpecificError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-specific 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadSpecificSetError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-specific-set! 42 'data)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadStartAndJoin(t *testing.T) {
	// Start a thread and join it to get the result
	code := `
		(let ((th (make-thread (lambda () (+ 1 2 3)))))
			(thread-start! th)
			(thread-join! th))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(6))
}

func TestThreadStartReturnsThread(t *testing.T) {
	code := `
		(let ((th (make-thread (lambda () 42))))
			(eq? th (thread-start! th)))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestThreadStartError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-start! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadYield(t *testing.T) {
	// thread-yield! should return void and not error
	result, err := runSchemeCode(t, "(thread-yield!)")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestThreadSleepWithNumber(t *testing.T) {
	// Sleep for a short time (0 seconds)
	result, err := runSchemeCodeWithTimeout(t, "(thread-sleep! 0)", 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestThreadSleepWithFloat(t *testing.T) {
	// Sleep for 0.001 seconds
	result, err := runSchemeCodeWithTimeout(t, "(thread-sleep! 0.001)", 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result.IsVoid(), qt.IsTrue)
}

func TestThreadSleepError(t *testing.T) {
	_, err := runSchemeCode(t, `(thread-sleep! "one")`)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadTerminate(t *testing.T) {
	code := `
		(let ((th (make-thread (lambda () (thread-sleep! 10)))))
			(thread-start! th)
			(thread-terminate! th)
			#t)
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestThreadTerminateError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-terminate! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestThreadJoinError(t *testing.T) {
	_, err := runSchemeCode(t, "(thread-join! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestCurrentThread(t *testing.T) {
	// current-thread returns something (primordial thread or actual thread)
	result, err := runSchemeCode(t, "(current-thread)")
	qt.Assert(t, err, qt.IsNil)
	// In test context, it returns 'primordial symbol
	qt.Assert(t, result, qt.IsNotNil)
}

func TestThreadJoinWithTimeout(t *testing.T) {
	// Test thread-join! with timeout that expires
	code := `
		(let ((th (make-thread (lambda () (thread-sleep! 10)))))
			(thread-start! th)
			(thread-join! th 0.01 'timed-out))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("timed-out"))
}

func TestMultipleThreads(t *testing.T) {
	// Test that multiple threads can run concurrently
	code := `
		(let ((th1 (make-thread (lambda () 1)))
		      (th2 (make-thread (lambda () 2)))
		      (th3 (make-thread (lambda () 3))))
			(thread-start! th1)
			(thread-start! th2)
			(thread-start! th3)
			(+ (thread-join! th1)
			   (thread-join! th2)
			   (thread-join! th3)))
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(6))
}
