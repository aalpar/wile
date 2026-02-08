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

package gointerop_test

import (
	"context"
	"testing"

	"github.com/aalpar/wile"
	extgointerop "github.com/aalpar/wile/internal/extensions/gointerop"
	"github.com/aalpar/wile/values"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with the gointerop extension loaded.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(
		wile.WithExtension(extgointerop.Extension),
	)
	qt.New(t).Assert(err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and asserts that it produces an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	_, err := engine.Eval(context.Background(), code)
	qt.New(t).Assert(err, qt.IsNotNil)
}

func TestChannelBasics(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-channel and channel?
		{"make-channel unbuffered", `(channel? (make-channel))`, values.TrueValue},
		{"make-channel buffered", `(channel? (make-channel 5))`, values.TrueValue},
		{"channel? false", `(channel? 42)`, values.FalseValue},
		{"channel? string", `(channel? "hello")`, values.FalseValue},
		{"channel? boolean", `(channel? #t)`, values.FalseValue},

		// channel-capacity
		{"capacity unbuffered", `(= (channel-capacity (make-channel)) 0)`, values.TrueValue},
		{"capacity buffered", `(= (channel-capacity (make-channel 5)) 5)`, values.TrueValue},
		{"capacity negative clamps", `(= (channel-capacity (make-channel -1)) 0)`, values.TrueValue},

		// channel-length on empty
		{"length empty", `(= (channel-length (make-channel 5)) 0)`, values.TrueValue},

		// buffered send + receive round-trip
		{"send receive integer",
			`(let ((ch (make-channel 1)))
			   (channel-send! ch 42)
			   (= (channel-receive ch) 42))`,
			values.TrueValue},
		{"send receive string",
			`(let ((ch (make-channel 1)))
			   (channel-send! ch "hello")
			   (equal? (channel-receive ch) "hello"))`,
			values.TrueValue},
		{"send receive boolean",
			`(let ((ch (make-channel 1)))
			   (channel-send! ch #t)
			   (channel-receive ch))`,
			values.TrueValue},
		{"send receive list",
			`(let ((ch (make-channel 1)))
			   (channel-send! ch '(1 2 3))
			   (equal? (channel-receive ch) '(1 2 3)))`,
			values.TrueValue},

		// channel-length after send
		{"length after send",
			`(let ((ch (make-channel 5)))
			   (channel-send! ch 1)
			   (channel-send! ch 2)
			   (= (channel-length ch) 2))`,
			values.TrueValue},

		// multiple send/receive preserves FIFO order
		{"fifo order",
			`(let ((ch (make-channel 3)))
			   (channel-send! ch 1)
			   (channel-send! ch 2)
			   (channel-send! ch 3)
			   (and (= (channel-receive ch) 1)
			        (= (channel-receive ch) 2)
			        (= (channel-receive ch) 3)))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestChannelTryOperations(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// channel-try-send! on buffered channel with space
		{"try-send succeeds",
			`(let ((ch (make-channel 1)))
			   (channel-try-send! ch 42))`,
			values.TrueValue},

		// channel-try-send! on full buffered channel
		{"try-send full",
			`(let ((ch (make-channel 1)))
			   (channel-try-send! ch 1)
			   (channel-try-send! ch 2))`,
			values.FalseValue},

		// channel-try-send! on unbuffered (no receiver = would block)
		{"try-send unbuffered",
			`(let ((ch (make-channel)))
			   (channel-try-send! ch 42))`,
			values.FalseValue},

		// channel-try-receive with data available (3-value return)
		{"try-receive with data",
			`(let ((ch (make-channel 1)))
			   (channel-send! ch 42)
			   (call-with-values
			     (lambda () (channel-try-receive ch))
			     (lambda (val received? open?)
			       (and (= val 42) received? open?))))`,
			values.TrueValue},

		// channel-try-receive on empty open channel
		{"try-receive empty open",
			`(let ((ch (make-channel 1)))
			   (call-with-values
			     (lambda () (channel-try-receive ch))
			     (lambda (val received? open?)
			       (and (not received?) open?))))`,
			values.TrueValue},

		// channel-try-receive on closed empty channel
		{"try-receive closed empty",
			`(let ((ch (make-channel 1)))
			   (channel-close! ch)
			   (call-with-values
			     (lambda () (channel-try-receive ch))
			     (lambda (val received? open?)
			       (and (not received?) (not open?)))))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestChannelClose(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// channel-closed? on open channel
		{"not closed initially",
			`(channel-closed? (make-channel))`,
			values.FalseValue},

		// channel-close! then channel-closed?
		{"closed after close",
			`(let ((ch (make-channel)))
			   (channel-close! ch)
			   (channel-closed? ch))`,
			values.TrueValue},

		// channel-closed? on buffered channel
		{"buffered closed",
			`(let ((ch (make-channel 5)))
			   (channel-close! ch)
			   (channel-closed? ch))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestWaitGroup(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-wait-group and wait-group?
		{"make-wait-group", `(wait-group? (make-wait-group))`, values.TrueValue},
		{"wait-group? false", `(wait-group? 42)`, values.FalseValue},
		{"wait-group? string", `(wait-group? "hello")`, values.FalseValue},

		// add, done, wait sequence (counter goes 0 -> 1 -> 0, wait returns immediately)
		{"add done wait",
			`(let ((wg (make-wait-group)))
			   (wait-group-add! wg 1)
			   (wait-group-done! wg)
			   (wait-group-wait! wg)
			   #t)`,
			values.TrueValue},

		// add multiple, done multiple
		{"add multiple done multiple",
			`(let ((wg (make-wait-group)))
			   (wait-group-add! wg 3)
			   (wait-group-done! wg)
			   (wait-group-done! wg)
			   (wait-group-done! wg)
			   (wait-group-wait! wg)
			   #t)`,
			values.TrueValue},

		// wait on fresh wait-group (counter already 0)
		{"wait on fresh",
			`(let ((wg (make-wait-group)))
			   (wait-group-wait! wg)
			   #t)`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestRWMutex(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-rw-mutex and rw-mutex?
		{"make-rw-mutex", `(rw-mutex? (make-rw-mutex))`, values.TrueValue},
		{"make-rw-mutex named", `(rw-mutex? (make-rw-mutex "my-lock"))`, values.TrueValue},
		{"make-rw-mutex symbol name", `(rw-mutex? (make-rw-mutex 'my-lock))`, values.TrueValue},
		{"rw-mutex? false", `(rw-mutex? 42)`, values.FalseValue},
		{"rw-mutex? string", `(rw-mutex? "hello")`, values.FalseValue},

		// read lock/unlock
		{"read lock unlock",
			`(let ((m (make-rw-mutex)))
			   (rw-mutex-read-lock! m)
			   (rw-mutex-read-unlock! m)
			   #t)`,
			values.TrueValue},

		// write lock/unlock
		{"write lock unlock",
			`(let ((m (make-rw-mutex)))
			   (rw-mutex-write-lock! m)
			   (rw-mutex-write-unlock! m)
			   #t)`,
			values.TrueValue},

		// multiple concurrent read locks (allowed by RWMutex)
		{"multiple read locks",
			`(let ((m (make-rw-mutex)))
			   (rw-mutex-read-lock! m)
			   (rw-mutex-read-lock! m)
			   (rw-mutex-read-unlock! m)
			   (rw-mutex-read-unlock! m)
			   #t)`,
			values.TrueValue},

		// try-read-lock on unlocked mutex
		{"try-read-lock succeeds",
			`(let ((m (make-rw-mutex)))
			   (let ((got (rw-mutex-try-read-lock! m)))
			     (rw-mutex-read-unlock! m)
			     got))`,
			values.TrueValue},

		// try-write-lock on unlocked mutex
		{"try-write-lock succeeds",
			`(let ((m (make-rw-mutex)))
			   (let ((got (rw-mutex-try-write-lock! m)))
			     (rw-mutex-write-unlock! m)
			     got))`,
			values.TrueValue},

		// try-write-lock while read-locked (should fail)
		{"try-write-lock while read-locked",
			`(let ((m (make-rw-mutex)))
			   (rw-mutex-read-lock! m)
			   (let ((got (rw-mutex-try-write-lock! m)))
			     (rw-mutex-read-unlock! m)
			     got))`,
			values.FalseValue},

		// try-read-lock while write-locked (should fail)
		{"try-read-lock while write-locked",
			`(let ((m (make-rw-mutex)))
			   (rw-mutex-write-lock! m)
			   (let ((got (rw-mutex-try-read-lock! m)))
			     (rw-mutex-write-unlock! m)
			     got))`,
			values.FalseValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestOnce(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-once and once?
		{"make-once", `(once? (make-once))`, values.TrueValue},
		{"once? false", `(once? 42)`, values.FalseValue},
		{"once? string", `(once? "hello")`, values.FalseValue},

		// once-done? on fresh Once
		{"not done initially", `(once-done? (make-once))`, values.FalseValue},

		// once-do! first call returns #t
		{"first do returns true",
			`(let ((o (make-once)))
			   (once-do! o (lambda () #t)))`,
			values.TrueValue},

		// once-do! second call returns #f
		{"second do returns false",
			`(let ((o (make-once)))
			   (once-do! o (lambda () #t))
			   (once-do! o (lambda () #t)))`,
			values.FalseValue},

		// once-done? after execution
		{"done after do",
			`(let ((o (make-once)))
			   (once-do! o (lambda () #t))
			   (once-done? o))`,
			values.TrueValue},

		// once-do! executes thunk only once (side-effect via channel)
		{"executes only once",
			`(let ((o (make-once))
			       (ch (make-channel 2)))
			   (once-do! o (lambda () (channel-send! ch 1)))
			   (once-do! o (lambda () (channel-send! ch 2)))
			   (= (channel-length ch) 1))`,
			values.TrueValue},

		// once-do! with non-closure still marks as done
		{"non-closure marks done",
			`(let ((o (make-once)))
			   (once-do! o 42)
			   (once-done? o))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestAtomic(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
		want values.Value
	}{
		// make-atomic and atomic?
		{"make-atomic", `(atomic? (make-atomic 0))`, values.TrueValue},
		{"atomic? false", `(atomic? 42)`, values.FalseValue},
		{"atomic? string", `(atomic? "hello")`, values.FalseValue},

		// atomic-load returns initial value
		{"load initial integer",
			`(= (atomic-load (make-atomic 42)) 42)`,
			values.TrueValue},
		{"load initial string",
			`(equal? (atomic-load (make-atomic "hello")) "hello")`,
			values.TrueValue},
		{"load initial boolean",
			`(atomic-load (make-atomic #t))`,
			values.TrueValue},

		// atomic-store! then load
		{"store then load",
			`(let ((a (make-atomic 0)))
			   (atomic-store! a 99)
			   (= (atomic-load a) 99))`,
			values.TrueValue},
		{"store string then load",
			`(let ((a (make-atomic "old")))
			   (atomic-store! a "new")
			   (equal? (atomic-load a) "new"))`,
			values.TrueValue},

		// atomic-swap! returns old value and stores new
		{"swap returns old",
			`(let ((a (make-atomic 42)))
			   (= (atomic-swap! a 99) 42))`,
			values.TrueValue},
		{"swap stores new",
			`(let ((a (make-atomic 42)))
			   (atomic-swap! a 99)
			   (= (atomic-load a) 99))`,
			values.TrueValue},

		// atomic-compare-and-swap! with pointer identity
		// CAS uses Go's atomic.Value.CompareAndSwap (pointer comparison),
		// so we must load the value first to get the same pointer.
		{"cas succeeds with loaded ref",
			`(let ((a (make-atomic 42)))
			   (let ((old (atomic-load a)))
			     (atomic-compare-and-swap! a old 99)))`,
			values.TrueValue},
		{"cas updates value",
			`(let ((a (make-atomic 42)))
			   (let ((old (atomic-load a)))
			     (atomic-compare-and-swap! a old 99)
			     (= (atomic-load a) 99)))`,
			values.TrueValue},

		// multiple store/load cycles
		{"multiple stores",
			`(let ((a (make-atomic 0)))
			   (atomic-store! a 1)
			   (atomic-store! a 2)
			   (atomic-store! a 3)
			   (= (atomic-load a) 3))`,
			values.TrueValue},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			result := eval(t, engine, tc.code)
			c.Assert(result.Internal(), qt.Equals, tc.want)
		})
	}
}

func TestGoInteropErrors(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		// channel type errors
		{"channel-send! not channel", `(channel-send! 42 1)`},
		{"channel-receive not channel", `(channel-receive 42)`},
		{"channel-try-send! not channel", `(channel-try-send! 42 1)`},
		{"channel-try-receive not channel", `(channel-try-receive 42)`},
		{"channel-close! not channel", `(channel-close! 42)`},
		{"channel-closed? not channel", `(channel-closed? 42)`},
		{"channel-length not channel", `(channel-length 42)`},
		{"channel-capacity not channel", `(channel-capacity 42)`},

		// double close
		{"double close",
			`(let ((ch (make-channel)))
			   (channel-close! ch)
			   (channel-close! ch))`},

		// send on closed channel
		{"send on closed",
			`(let ((ch (make-channel 1)))
			   (channel-close! ch)
			   (channel-send! ch 42))`},

		// try-send on closed channel
		{"try-send on closed",
			`(let ((ch (make-channel 1)))
			   (channel-close! ch)
			   (channel-try-send! ch 42))`},

		// wait-group type errors
		{"wait-group-add! not wg", `(wait-group-add! 42 1)`},
		{"wait-group-add! not integer", `(wait-group-add! (make-wait-group) "one")`},
		{"wait-group-done! not wg", `(wait-group-done! 42)`},
		{"wait-group-wait! not wg", `(wait-group-wait! 42)`},

		// rw-mutex type errors
		{"rw-mutex-read-lock! not mutex", `(rw-mutex-read-lock! 42)`},
		{"rw-mutex-read-unlock! not mutex", `(rw-mutex-read-unlock! 42)`},
		{"rw-mutex-write-lock! not mutex", `(rw-mutex-write-lock! 42)`},
		{"rw-mutex-write-unlock! not mutex", `(rw-mutex-write-unlock! 42)`},
		{"rw-mutex-try-read-lock! not mutex", `(rw-mutex-try-read-lock! 42)`},
		{"rw-mutex-try-write-lock! not mutex", `(rw-mutex-try-write-lock! 42)`},

		// once type errors
		{"once-do! not once", `(once-do! 42 (lambda () #t))`},
		{"once-done? not once", `(once-done? 42)`},

		// atomic type errors
		{"atomic-load not atomic", `(atomic-load 42)`},
		{"atomic-store! not atomic", `(atomic-store! 42 99)`},
		{"atomic-swap! not atomic", `(atomic-swap! 42 99)`},
		{"atomic-compare-and-swap! not atomic", `(atomic-compare-and-swap! 42 0 99)`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}
