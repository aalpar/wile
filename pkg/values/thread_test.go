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

package values_test

import (
	"context"
	"errors"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/werr"
)

func TestThread_NewThread(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test-thread")
	qt.Assert(t, th, qt.Not(qt.IsNil))
	qt.Assert(t, th.Name(), qt.Equals, "test-thread")
	qt.Assert(t, th.ID() > 0, qt.IsTrue)
	qt.Assert(t, th.State(), qt.Equals, values.ThreadNew)
}

func TestThread_DefaultName(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "")
	qt.Assert(t, strings.HasPrefix(th.Name(), "thread-"), qt.IsTrue)
}

func TestThread_Specific(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	qt.Assert(t, th.Specific() == nil, qt.IsTrue)

	th.SetSpecific(values.NewInteger(42))
	qt.Assert(t, th.Specific(), valuestest.SchemeEquals, values.NewInteger(42))
}

func TestThread_StateSymbol(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	sym := th.StateSymbol()
	qt.Assert(t, sym.Key, qt.Equals, "new")
}

func TestThreadState_String(t *testing.T) {
	tcs := []struct {
		state values.ThreadState
		str   string
	}{
		{values.ThreadNew, "new"},
		{values.ThreadRunnable, "runnable"},
		{values.ThreadBlocked, "blocked"},
		{values.ThreadTerminated, "terminated"},
		{values.ThreadState(99), "unknown"},
	}
	for _, tc := range tcs {
		t.Run(tc.str, func(t *testing.T) {
			qt.Assert(t, tc.state.String(), qt.Equals, tc.str)
		})
	}
}

func TestThread_StartNoRunFunc(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	err := th.Start(context.Background())
	qt.Assert(t, err, qt.Not(qt.IsNil))
	qt.Assert(t, strings.Contains(err.Error(), "no run function"), qt.IsTrue)
}

func TestThread_StartAlreadyStarted(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	th.RunFunc = func(_ context.Context, _ values.Callable) (values.Value, error) {
		return nil, nil
	}
	err := th.Start(context.Background())
	qt.Assert(t, err, qt.IsNil)
	<-th.Done()

	err = th.Start(context.Background())
	qt.Assert(t, errors.Is(err, werr.ErrThreadAlreadyStarted), qt.IsTrue)
}

func TestThread_IsVoid(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	qt.Assert(t, th.IsVoid(), qt.IsFalse)

	var nilTh *values.Thread
	qt.Assert(t, nilTh.IsVoid(), qt.IsTrue)
}

func TestThread_EqualTo(t *testing.T) {
	th1 := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "a")
	th2 := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "b")
	qt.Assert(t, th1.EqualTo(th1), qt.IsTrue)
	qt.Assert(t, th1.EqualTo(th2), qt.IsFalse)
	qt.Assert(t, th1.EqualTo(values.NewInteger(1)), qt.IsFalse)
}

func TestThread_SchemeString(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "my-thread")
	s := th.SchemeString()
	qt.Assert(t, strings.Contains(s, "my-thread"), qt.IsTrue)
	qt.Assert(t, strings.Contains(s, "new"), qt.IsTrue)

	var nilTh *values.Thread
	qt.Assert(t, nilTh.SchemeString(), qt.Equals, "#<thread:void>")
}

func TestThread_Done(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test")
	qt.Assert(t, th.Done(), qt.Not(qt.IsNil))
}

// --- Thread Exception Types ---

func TestJoinTimeoutException_Error(t *testing.T) {
	e := &values.JoinTimeoutException{}
	qt.Assert(t, e.Error(), qt.Equals, "thread-join!: timeout")
}

func TestTerminatedThreadException_Error(t *testing.T) {
	th := values.NewThread(newStubCallable(values.NewSymbol("thunk")), "test-thread")
	e := &values.TerminatedThreadException{Thread: th}
	qt.Assert(t, strings.Contains(e.Error(), "test-thread"), qt.IsTrue)

	e2 := &values.TerminatedThreadException{Thread: nil}
	qt.Assert(t, e2.Error(), qt.Equals, "thread terminated")
}

func TestUncaughtThreadException_Error(t *testing.T) {
	cause := errors.New("something broke")
	e := &values.UncaughtThreadException{Reason: cause}
	qt.Assert(t, strings.Contains(e.Error(), "something broke"), qt.IsTrue)

	e2 := &values.UncaughtThreadException{Reason: nil}
	qt.Assert(t, e2.Error(), qt.Equals, "uncaught exception in thread")
}

func TestUncaughtThreadException_Unwrap(t *testing.T) {
	cause := errors.New("root cause")
	e := &values.UncaughtThreadException{Reason: cause}
	qt.Assert(t, errors.Unwrap(e), qt.Equals, cause)
}

func TestAbandonedMutexException_Error(t *testing.T) {
	e := &values.AbandonedMutexException{}
	qt.Assert(t, e.Error(), qt.Equals, "mutex abandoned by terminated thread")
}
