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

package bootstrap

import (
	"context"
	"strings"
	"testing"

	"github.com/aalpar/wile/environment"
	"github.com/aalpar/wile/internal/parser"
	"github.com/aalpar/wile/machine"
	"github.com/aalpar/wile/machine/compilation"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"

	qt "github.com/frankban/quicktest"
)

// Helper to run scheme code and return the result
func evalScheme(t *testing.T, env *environment.EnvironmentFrame, code string) (values.Value, error) {
	t.Helper()
	p := parser.NewParser(env, true, strings.NewReader(code))
	stx, err := p.ReadSyntax(context.TODO())
	if err != nil {
		return nil, err
	}

	ectx := context.Background()
	expanded, err := compilation.NewExpanderTimeContinuation(ectx, env, machine.NewVMMacroEvaluator()).ExpandExpression(stx)
	if err != nil {
		return nil, err
	}

	tpl := machine.NewNativeTemplate(0, 0, false)
	cctx := compilation.NewCompileTimeCallContext(context.Background(), false)
	err = compilation.NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator()).CompileExpression(cctx, expanded)
	if err != nil {
		return nil, err
	}

	mc := machine.NewMachineContext(context.Background(), machine.NewMachineContinuation(nil, tpl, env))
	err = mc.Run()
	if err != nil {
		return nil, err
	}

	return mc.GetValue(), nil
}

// ===========================================================================
// Channel Tests
// ===========================================================================

func TestChannelBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Test make-channel and channel?
	result, err := evalScheme(t, env, `(channel? (make-channel))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(channel? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestChannelWithBufferSize(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Test buffered channel
	result, err := evalScheme(t, env, `
		(let ((ch (make-channel 5)))
		  (channel-capacity ch))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(5))
}

func TestChannelTrySendReceive(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Test try-send on buffered channel
	result, err := evalScheme(t, env, `
		(let ((ch (make-channel 1)))
		  (channel-try-send! ch 42))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Test try-send on full unbuffered channel
	result, err = evalScheme(t, env, `
		(let ((ch (make-channel)))
		  (channel-try-send! ch 42))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestChannelLength(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((ch (make-channel 5)))
		  (channel-try-send! ch 1)
		  (channel-try-send! ch 2)
		  (channel-length ch))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(2))
}

func TestChannelClose(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((ch (make-channel)))
		  (channel-close! ch)
		  (channel-closed? ch))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `
		(let ((ch (make-channel)))
		  (channel-closed? ch))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

// ===========================================================================
// Mutex Tests
// ===========================================================================

func TestMutexBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Test make-mutex and mutex?
	result, err := evalScheme(t, env, `(mutex? (make-mutex))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(mutex? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestMutexWithName(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((m (make-mutex "test-mutex")))
		  (mutex-name m))
	`)
	c.Assert(err, qt.IsNil)
	str, ok := result.(*values.String)
	c.Assert(ok, qt.IsTrue)
	c.Assert(str.Value, qt.Equals, "test-mutex")
}

func TestMutexSpecific(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((m (make-mutex)))
		  (mutex-specific-set! m 42)
		  (mutex-specific m))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))
}

// ===========================================================================
// Condition Variable Tests
// ===========================================================================

func TestConditionVariableBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(condition-variable? (make-condition-variable))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(condition-variable? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestConditionVariableWithName(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((cv (make-condition-variable "test-cv")))
		  (condition-variable-name cv))
	`)
	c.Assert(err, qt.IsNil)
	str, ok := result.(*values.String)
	c.Assert(ok, qt.IsTrue)
	c.Assert(str.Value, qt.Equals, "test-cv")
}

func TestConditionVariableSpecific(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((cv (make-condition-variable)))
		  (condition-variable-specific-set! cv "hello")
		  (condition-variable-specific cv))
	`)
	c.Assert(err, qt.IsNil)
	str, ok := result.(*values.String)
	c.Assert(ok, qt.IsTrue)
	c.Assert(str.Value, qt.Equals, "hello")
}

// ===========================================================================
// Time Tests
// ===========================================================================

func TestTimeBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(time? (current-time))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(time? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestTimeConversion(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(time? (seconds->time 1000))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(> (time->seconds (current-time)) 0)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)
}

// ===========================================================================
// WaitGroup Tests
// ===========================================================================

func TestWaitGroupBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(wait-group? (make-wait-group))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(wait-group? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

// ===========================================================================
// RWMutex Tests
// ===========================================================================

func TestRWMutexBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(rw-mutex? (make-rw-mutex))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(rw-mutex? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestRWMutexWithName(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Just test creation with a name doesn't error
	result, err := evalScheme(t, env, `(rw-mutex? (make-rw-mutex "test-rwm"))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)
}

func TestRWMutexTryLock(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Try acquiring a read lock should succeed
	result, err := evalScheme(t, env, `
		(let ((rwm (make-rw-mutex)))
		  (rw-mutex-try-read-lock! rwm))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	// Try acquiring a write lock should succeed
	result, err = evalScheme(t, env, `
		(let ((rwm (make-rw-mutex)))
		  (rw-mutex-try-write-lock! rwm))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)
}

// ===========================================================================
// Once Tests
// ===========================================================================

func TestOnceBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(once? (make-once))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(once? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestOnceDone(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// Fresh once should not be done
	result, err := evalScheme(t, env, `
		(let ((o (make-once)))
		  (once-done? o))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

// ===========================================================================
// AtomicBox Tests
// ===========================================================================

func TestAtomicBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(atomic? (make-atomic 0))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(atomic? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestAtomicLoadStore(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((a (make-atomic 42)))
		  (atomic-load a))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))

	result, err = evalScheme(t, env, `
		(let ((a (make-atomic 0)))
		  (atomic-store! a 100)
		  (atomic-load a))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(100))
}

func TestAtomicSwap(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((a (make-atomic 42)))
		  (atomic-swap! a 100))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, valuestest.SchemeEquals, values.NewInteger(42))
}

func TestAtomicCompareAndSwap(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	// CAS succeeds when comparing with the same reference
	// Note: Go's atomic.wrt.CompareAndSwap compares by identity, not value
	result, err := evalScheme(t, env, `
		(let* ((val 42)
		       (a (make-atomic val)))
		  (atomic-compare-and-swap! a val 100))
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	// CAS fails when comparing with a different reference (even if same value).
	// Uses (+ 99999 1) to produce a fresh integer object at runtime, since
	// core let compiles in a single template where literal deduplication may
	// share the pointer for identical large integers.
	result, err = evalScheme(t, env, `
		(let ((a (make-atomic 100000)))
		  (atomic-compare-and-swap! a (+ 99999 1) 100))
	`)
	c.Assert(err, qt.IsNil)
	// This should fail because (+ 99999 1) is a new integer object, not the same reference
	c.Assert(result, qt.Equals, values.FalseValue)
}

// ===========================================================================
// Thread Tests
// ===========================================================================

func TestThreadBasic(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `(thread? (make-thread (lambda () 42)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.TrueValue)

	result, err = evalScheme(t, env, `(thread? 42)`)
	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, values.FalseValue)
}

func TestThreadWithName(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((t (make-thread (lambda () 42) "test-thread")))
		  (thread-name t))
	`)
	c.Assert(err, qt.IsNil)
	str, ok := result.(*values.String)
	c.Assert(ok, qt.IsTrue)
	c.Assert(str.Value, qt.Equals, "test-thread")
}

func TestThreadSpecific(t *testing.T) {
	c := qt.New(t)
	env, err := NewNamespaceFrame(context.TODO())
	c.Assert(err, qt.IsNil)

	result, err := evalScheme(t, env, `
		(let ((t (make-thread (lambda () 42))))
		  (thread-specific-set! t "data")
		  (thread-specific t))
	`)
	c.Assert(err, qt.IsNil)
	str, ok := result.(*values.String)
	c.Assert(ok, qt.IsTrue)
	c.Assert(str.Value, qt.Equals, "data")
}

// ===========================================================================
// Error Handling Tests
// ===========================================================================

func TestChannelTypeErrors(t *testing.T) {
	env, err := NewNamespaceFrame(context.TODO())
	if err != nil {
		t.Fatal(err)
	}

	// Test that type errors are properly reported
	_, err = evalScheme(t, env, `(channel-send! 42 "value")`)
	if err == nil {
		t.Error("Expected error for channel-send! with non-channel")
	}

	_, err = evalScheme(t, env, `(channel-receive 42)`)
	if err == nil {
		t.Error("Expected error for channel-receive with non-channel")
	}
}

func TestMutexTypeErrors(t *testing.T) {
	env, err := NewNamespaceFrame(context.TODO())
	if err != nil {
		t.Fatal(err)
	}

	_, err = evalScheme(t, env, `(mutex-lock! 42)`)
	if err == nil {
		t.Error("Expected error for mutex-lock! with non-mutex")
	}
}

func TestAtomicTypeErrors(t *testing.T) {
	env, err := NewNamespaceFrame(context.TODO())
	if err != nil {
		t.Fatal(err)
	}

	_, err = evalScheme(t, env, `(atomic-load 42)`)
	if err == nil {
		t.Error("Expected error for atomic-load with non-atomic")
	}

	_, err = evalScheme(t, env, `(atomic-store! 42 100)`)
	if err == nil {
		t.Error("Expected error for atomic-store! with non-atomic")
	}
}
