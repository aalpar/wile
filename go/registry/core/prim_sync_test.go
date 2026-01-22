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
// WaitGroup Primitives Tests
// ----------------------------------------------------------------------------

func TestWaitGroupQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "wait-group? on wait-group object",
			code: "(wait-group? (make-wait-group))",
			out:  values.TrueValue,
		},
		{
			name: "wait-group? on integer",
			code: "(wait-group? 42)",
			out:  values.FalseValue,
		},
		{
			name: "wait-group? on mutex",
			code: "(wait-group? (make-mutex))",
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

func TestMakeWaitGroup(t *testing.T) {
	result, err := runSchemeCode(t, "(wait-group? (make-wait-group))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestWaitGroupAddDoneWait(t *testing.T) {
	code := `
		(let ((wg (make-wait-group)))
			(wait-group-add! wg 1)
			(wait-group-done! wg)
			(wait-group-wait! wg)
			#t)
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestWaitGroupAddError(t *testing.T) {
	_, err := runSchemeCode(t, "(wait-group-add! 42 1)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestWaitGroupDoneError(t *testing.T) {
	_, err := runSchemeCode(t, "(wait-group-done! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestWaitGroupWaitError(t *testing.T) {
	_, err := runSchemeCode(t, "(wait-group-wait! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

// ----------------------------------------------------------------------------
// RWMutex Primitives Tests
// ----------------------------------------------------------------------------

func TestRWMutexQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "rw-mutex? on rw-mutex object",
			code: "(rw-mutex? (make-rw-mutex))",
			out:  values.TrueValue,
		},
		{
			name: "rw-mutex? on integer",
			code: "(rw-mutex? 42)",
			out:  values.FalseValue,
		},
		{
			name: "rw-mutex? on regular mutex",
			code: "(rw-mutex? (make-mutex))",
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

func TestMakeRWMutex(t *testing.T) {
	result, err := runSchemeCode(t, "(rw-mutex? (make-rw-mutex))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestMakeRWMutexWithName(t *testing.T) {
	// Just test that it doesn't error - RWMutex may not have name accessor
	result, err := runSchemeCode(t, "(rw-mutex? (make-rw-mutex \"test\"))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestRWMutexReadLockUnlock(t *testing.T) {
	code := `
		(let ((rwm (make-rw-mutex)))
			(rw-mutex-read-lock! rwm)
			(rw-mutex-read-unlock! rwm)
			#t)
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestRWMutexWriteLockUnlock(t *testing.T) {
	code := `
		(let ((rwm (make-rw-mutex)))
			(rw-mutex-write-lock! rwm)
			(rw-mutex-write-unlock! rwm)
			#t)
	`
	result, err := runSchemeCodeWithTimeout(t, code, 5*time.Second)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestRWMutexTryReadLock(t *testing.T) {
	code := `
		(let ((rwm (make-rw-mutex)))
			(rw-mutex-try-read-lock! rwm))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestRWMutexTryWriteLock(t *testing.T) {
	code := `
		(let ((rwm (make-rw-mutex)))
			(rw-mutex-try-write-lock! rwm))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestRWMutexReadLockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-read-lock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRWMutexReadUnlockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-read-unlock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRWMutexWriteLockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-write-lock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRWMutexWriteUnlockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-write-unlock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRWMutexTryReadLockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-try-read-lock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestRWMutexTryWriteLockError(t *testing.T) {
	_, err := runSchemeCode(t, "(rw-mutex-try-write-lock! 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

// ----------------------------------------------------------------------------
// Once Primitives Tests
// ----------------------------------------------------------------------------

func TestOnceQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "once? on once object",
			code: "(once? (make-once))",
			out:  values.TrueValue,
		},
		{
			name: "once? on integer",
			code: "(once? 42)",
			out:  values.FalseValue,
		},
		{
			name: "once? on mutex",
			code: "(once? (make-mutex))",
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

func TestMakeOnce(t *testing.T) {
	result, err := runSchemeCode(t, "(once? (make-once))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOnceDo(t *testing.T) {
	// First call should execute and return #t
	code := `
		(let ((o (make-once)))
			(once-do! o (lambda () 'executed)))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestOnceDoOnlyOnce(t *testing.T) {
	// Second call should return #f
	code := `
		(let ((o (make-once)))
			(once-do! o (lambda () 'first))
			(once-do! o (lambda () 'second)))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestOnceDoError(t *testing.T) {
	_, err := runSchemeCode(t, "(once-do! 42 (lambda () 'x))")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestOnceDoneQ(t *testing.T) {
	tcs := []struct {
		name string
		code string
		out  values.Value
	}{
		{
			name: "once-done? before execution",
			code: "(once-done? (make-once))",
			out:  values.FalseValue,
		},
		{
			name: "once-done? after execution",
			code: `
				(let ((o (make-once)))
					(once-do! o (lambda () 'done))
					(once-done? o))
			`,
			out: values.TrueValue,
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

func TestOnceDoneQError(t *testing.T) {
	_, err := runSchemeCode(t, "(once-done? 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

// ----------------------------------------------------------------------------
// Atomic Primitives Tests
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}

func TestMakeAtomic(t *testing.T) {
	result, err := runSchemeCode(t, "(atomic? (make-atomic 42))")
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestAtomicLoad(t *testing.T) {
	code := "(atomic-load (make-atomic 42))"
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(42))
}

func TestAtomicLoadError(t *testing.T) {
	_, err := runSchemeCode(t, "(atomic-load 42)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicStore(t *testing.T) {
	code := `
		(let ((a (make-atomic 0)))
			(atomic-store! a 100)
			(atomic-load a))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewInteger(100))
}

func TestAtomicStoreError(t *testing.T) {
	_, err := runSchemeCode(t, "(atomic-store! 42 100)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicSwap(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-swap! a 'new))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("old"))
}

func TestAtomicSwapUpdatesValue(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-swap! a 'new)
			(atomic-load a))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("new"))
}

func TestAtomicSwapError(t *testing.T) {
	_, err := runSchemeCode(t, "(atomic-swap! 42 'new)")
	qt.Assert(t, err, qt.IsNotNil)
}

func TestAtomicCompareAndSwapSuccess(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'old 'new))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.TrueValue)
}

func TestAtomicCompareAndSwapFailure(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'wrong 'new))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.FalseValue)
}

func TestAtomicCompareAndSwapUpdatesOnSuccess(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'old 'new)
			(atomic-load a))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("new"))
}

func TestAtomicCompareAndSwapNoUpdateOnFailure(t *testing.T) {
	code := `
		(let ((a (make-atomic 'old)))
			(atomic-compare-and-swap! a 'wrong 'new)
			(atomic-load a))
	`
	result, err := runSchemeCode(t, code)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, result, values.SchemeEquals, values.NewSymbol("old"))
}

func TestAtomicCompareAndSwapError(t *testing.T) {
	_, err := runSchemeCode(t, "(atomic-compare-and-swap! 42 'old 'new)")
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
			result, err := runSchemeCode(t, tc.code)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, result, values.SchemeEquals, tc.out)
		})
	}
}
