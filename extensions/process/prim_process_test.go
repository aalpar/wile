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

package process_test

import (
	"context"
	"errors"
	"os/exec"
	"runtime"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"

	extprocess "github.com/aalpar/wile/extensions/process"
	extthreads "github.com/aalpar/wile/extensions/threads"
	extio "github.com/aalpar/wile/pkg/extensions/io"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/registry/core"
	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/testutil"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// eval runs Scheme code and returns the result.
func eval(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.EvalMultiple(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// evalExpectError runs Scheme code and expects an error.
func evalExpectError(t *testing.T, engine *wile.Engine, code string) {
	t.Helper()
	expr, err := engine.Parse(context.Background(), code)
	if err != nil {
		return
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

func TestSystem(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("system uses /bin/sh")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns zero for success", func(t *testing.T) {
		result := eval(t, engine, `(system "true")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("returns nonzero for failure", func(t *testing.T) {
		result := eval(t, engine, `(system "false")`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(system 42)`)
	})
}

func TestProcessSpawn(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a process", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((p (process-spawn "echo" "hello")))
			  (process-wait p)
			  (process? p))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("pipe ports are named for the command's base name and the stream", func(t *testing.T) {
		tests := []struct{ accessor, want string }{
			{"process-stdin", "cat stdin"},
			{"process-stdout", "cat stdout"},
			{"process-stderr", "cat stderr"},
		}
		for _, tc := range tests {
			result := eval(t, engine, `
				(let* ((proc (process-spawn "/bin/cat")) (p (`+tc.accessor+` proc)))
				  (close-port (process-stdin proc))
				  (process-wait proc)
				  p)
			`)
			port, ok := result.Internal().(*values.PortObject)
			c.Assert(ok, qt.IsTrue)
			c.Assert(port.PortName(), qt.Equals, tc.want)
		}
	})

	t.Run("can read stdout", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "echo" "hello")))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "hello")
	})

	t.Run("can write stdin and read stdout", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "cat")))
			  (display "ping" (process-stdin proc))
			  (close-output-port (process-stdin proc))
			  (let ((line (read-line (process-stdout proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "ping")
	})

	t.Run("process-wait returns exit code", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Equals, int64(0))
	})

	t.Run("process-wait returns nonzero on failure", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "false")))
			  (process-wait proc))
		`)
		c.Assert(result.Internal().(*values.Integer).Value, qt.Not(qt.Equals), int64(0))
	})
}

func TestProcessKill(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix signals")
	}
	engine := newEngine(t)

	t.Run("kill terminates process", func(t *testing.T) {
		eval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'kill)
			  (process-wait proc))
		`)
	})

	t.Run("term terminates process", func(t *testing.T) {
		eval(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (process-kill proc 'term)
			  (process-wait proc))
		`)
	})

	t.Run("unknown signal names the host and the offered set", func(t *testing.T) {
		// The signal lookup precedes the not-running check, so a reaped
		// process reaches it without any cleanup.
		expr, err := engine.Parse(context.Background(), `
			(let ((proc (process-spawn "true")))
			  (process-wait proc)
			  (process-kill proc 'bogus))
		`)
		qt.Assert(t, err, qt.IsNil)
		_, err = engine.Eval(context.Background(), expr)
		qt.Assert(t, errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue)
		qt.Assert(t, err, qt.ErrorMatches, `(?s).*unknown signal 'bogus' on `+runtime.GOOS+` \(expected .*\).*`)
	})

	t.Run("invalid signal", func(t *testing.T) {
		// Spawn, attempt invalid signal, then clean up the process.
		evalExpectError(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (guard (exn (#t (process-kill proc 'kill)
			                  (process-wait proc)
			                  (raise exn)))
			    (process-kill proc 'bogus)))
		`)
	})
}

func TestProcessStderr(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("can read stderr", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "sh" "-c" "echo oops >&2")))
			  (let ((line (read-line (process-stderr proc))))
			    (process-wait proc)
			    line))
		`)
		c.Assert(result.Internal().(*values.String).Value, qt.Equals, "oops")
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(process-stderr 42)`)
	})
}

func TestProcessAccessorErrors(t *testing.T) {
	engine := newEngine(t)

	t.Run("process-stdout wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(process-stdout "not-a-process")`)
	})

	t.Run("process-stdin wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(process-stdin #t)`)
	})
}

func TestProcessSpawnErrors(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	engine := newEngine(t)

	t.Run("non-string argument", func(t *testing.T) {
		evalExpectError(t, engine, `(process-spawn "echo" 42)`)
	})

	t.Run("wrong type for command", func(t *testing.T) {
		evalExpectError(t, engine, `(process-spawn 42)`)
	})

	t.Run("nonexistent command", func(t *testing.T) {
		evalExpectError(t, engine, `
			(let ((proc (process-spawn "/nonexistent/binary/zzz")))
			  (process-wait proc))
		`)
	})
}

func TestProcessSpawnSecurityDenied(t *testing.T) {
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
		wile.WithAuthorizer(security.DenyAll()),
	)
	qt.Assert(t, err, qt.IsNil)

	t.Run("process-spawn denied", func(t *testing.T) {
		evalExpectError(t, engine, `(process-spawn "echo" "hello")`)
	})

	t.Run("system denied", func(t *testing.T) {
		evalExpectError(t, engine, `(system "true")`)
	})
}

func TestProcessWaitErrors(t *testing.T) {
	engine := newEngine(t)

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(process-wait 42)`)
	})
}

func TestProcessKillErrors(t *testing.T) {
	engine := newEngine(t)

	t.Run("wrong type for process", func(t *testing.T) {
		evalExpectError(t, engine, `(process-kill 42 'term)`)
	})

	t.Run("wrong type for signal", func(t *testing.T) {
		if runtime.GOOS == "windows" {
			t.Skip("uses Unix commands")
		}
		evalExpectError(t, engine, `
			(let ((proc (process-spawn "sleep" "60")))
			  (guard (exn (#t (process-kill proc 'kill)
			                  (process-wait proc)
			                  (raise exn)))
			    (process-kill proc "not-a-symbol")))
		`)
	})
}

func TestProcessPredicate(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("process? true for process", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((proc (process-spawn "true")))
			  (process-wait proc)
			  (process? proc))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("process? false for non-process", func(t *testing.T) {
		result := eval(t, engine, `(process? 42)`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})
}

// TestEngineCloseKillsSpawnedProcesses pins the closer registered by
// addPrimitives: closing an engine must kill and reap the children that engine
// spawned, rather than orphaning them onto the host.
//
// cmd.ProcessState is the gate. It is nil for a running child and is set only by
// a completed Wait, so the assertion cannot pass unless the closer both killed
// and reaped. Without the closer the child stays alive: `ps -axo pid,command`
// still lists `sleep 97` after Close returns. No watchdog is needed — the closer
// only waits on a process it has already killed.
//
// ExitCode() == -1 (and Exited() == false) is the signalled shape: on Unix a
// process terminated by a signal did not "exit", so Exited() reports false. That
// distinguishes the closer's SIGKILL from a child that ran to completion.
func TestEngineCloseKillsSpawnedProcesses(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
	)
	c.Assert(err, qt.IsNil)

	result := eval(t, engine, `(process-spawn "sleep" "97")`)
	proc, ok := result.Internal().(*values.Process)
	c.Assert(ok, qt.IsTrue, qt.Commentf("process-spawn returned %T", result.Internal()))
	cmd := proc.Cmd()
	c.Assert(cmd, qt.IsNotNil)
	c.Assert(cmd.ProcessState, qt.IsNil, qt.Commentf("child must still be running before Close"))

	err = engine.Close()
	c.Assert(err, qt.IsNil)

	c.Assert(cmd.ProcessState, qt.IsNotNil,
		qt.Commentf("Engine.Close must kill and reap the processes the engine spawned"))
	c.Assert(cmd.ProcessState.Exited(), qt.IsFalse)
	c.Assert(cmd.ProcessState.ExitCode(), qt.Equals, -1)
}

// TestEngineCloseReapsOwnProcessesOnSharedRegistry is the cross-engine arm: with
// a registry shared through WithRegistry, registry.Apply is first-wins, so
// engine2 runs engine1's process-spawn. A tracker captured in that closure files
// engine2's child under engine1, and the two consequences are both wrong in the
// same run: closing the engine that spawned the child leaves it alive (measured
// before the fix: ProcessState still nil after engine2.Close()), and closing an
// UNRELATED engine SIGKILLs a live child of a still-running one (ProcessState
// "signal: killed" after engine1.Close()). Keying the tracker on the calling
// namespace makes the closing engine the owner.
//
// No io extension here: a second engine over a shared registry cannot re-run
// io's bootstrap (it redefines call-with-port on an immutable top level), and
// process-spawn needs none of it.
func TestEngineCloseReapsOwnProcessesOnSharedRegistry(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)

	shared := registry.NewRegistry()
	c.Assert(core.AddToRegistry(shared), qt.IsNil)

	newShared := func() *wile.Engine {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithRegistry(shared),
			wile.WithExtension(extprocess.Extension),
		)
		c.Assert(err, qt.IsNil)
		return engine
	}
	engine1 := newShared()
	engine2 := newShared()

	spawned := func(engine *wile.Engine, secs string) *exec.Cmd {
		result, err := engine.EvalMultiple(context.Background(), `(process-spawn "sleep" "`+secs+`")`)
		c.Assert(err, qt.IsNil)
		proc, ok := result.Internal().(*values.Process)
		c.Assert(ok, qt.IsTrue, qt.Commentf("process-spawn returned %T", result.Internal()))
		cmd := proc.Cmd()
		c.Assert(cmd, qt.IsNotNil)
		c.Assert(cmd.ProcessState, qt.IsNil)
		return cmd
	}
	cmd1 := spawned(engine1, "96")
	cmd2 := spawned(engine2, "97")

	c.Assert(engine2.Close(), qt.IsNil)
	c.Assert(cmd2.ProcessState, qt.IsNotNil,
		qt.Commentf("an engine must reap the child IT spawned, even on a shared registry"))
	c.Assert(cmd1.ProcessState, qt.IsNil,
		qt.Commentf("closing engine2 must not kill a live child of a still-running engine1"))

	c.Assert(engine1.Close(), qt.IsNil)
	c.Assert(cmd1.ProcessState, qt.IsNotNil)
}

// TestProcessWaitDoesNotRaceEngineClose pins the serialisation added to
// values.Process: (process-wait p) on an SRFI-18 thread and Engine.Close's
// reaper both need the child's exit status, and exec.Cmd.Wait is not safe for
// concurrent use — both callers read a nil c.ProcessState, both wait4 the same
// pid, and both write c.ProcessState back. Run under -race this failed with
// "WARNING: DATA RACE ... os/exec.(*Cmd).Wait ... liveProcs.Close ... Previous
// read ... PrimProcessWait"; the visible consequence without the detector is a
// bogus exit code out of (process-wait p), since one of the two wait4 calls
// loses the child.
//
// Meaningful only under -race for the race itself, but the ordering it exercises
// (Close reaping a child another goroutine is already waiting on) is worth
// running always.
func TestProcessWaitDoesNotRaceEngineClose(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extprocess.Extension),
		wile.WithExtension(extthreads.Extension),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(context.Background(), `
		(define p (process-spawn "sleep" "97"))
		(define waiter (make-thread (lambda () (process-wait p))))
		(thread-start! waiter)
		p`)
	c.Assert(err, qt.IsNil)
	proc, ok := result.Internal().(*values.Process)
	c.Assert(ok, qt.IsTrue, qt.Commentf("expected the process, got %T", result.Internal()))

	// Let the waiter reach cmd.Wait before Close reaps underneath it.
	testutil.PollUntil(t, func() bool {
		return proc.Cmd().Process != nil
	}, 2*time.Second)
	time.Sleep(200 * time.Millisecond)

	c.Assert(engine.Close(), qt.IsNil)
	c.Assert(proc.Reaped(), qt.IsTrue,
		qt.Commentf("Close must have reaped the child through the serialised wait"))
	c.Assert(proc.Cmd().ProcessState, qt.IsNotNil)
}

// errProcessCancelCause is a cancellation cause no context package error matches,
// so a pin can tell "the cause survived" from "ctx.Err() survived".
var errProcessCancelCause = werr.NewStaticError("process test: cancelled by the host")

// newCancelEngine returns a process engine, a ctx, and a `cancel!` primitive that
// cancels that ctx with errProcessCancelCause when the program calls it, so the
// cancellation lands at a known point in the run, before the wait it ends.
func newCancelEngine(t *testing.T) (*wile.Engine, context.Context) {
	t.Helper()
	engine := newEngine(t)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	ctx, cancel := context.WithCancelCause(context.Background())
	t.Cleanup(func() {
		cancel(nil)
	})
	err := engine.RegisterPrimitive(wile.PrimitiveSpec{
		Name:       "cancel!",
		ParamCount: 0,
		Impl: func(mc wile.CallContext) error {
			cancel(errProcessCancelCause)
			mc.SetValue(values.Void)
			return nil
		},
	})
	qt.Assert(t, err, qt.IsNil)
	return engine, ctx
}

// evalWithin runs code under ctx and fails the test if it has not returned
// within d: the failure these pins guard against is a wait that outlives its
// cancellation, which would otherwise surface only as a slow pass.
func evalWithin(ctx context.Context, t *testing.T, engine *wile.Engine, code string, d time.Duration) (wile.Value, error) {
	t.Helper()
	type outcome struct {
		value wile.Value
		err   error
	}
	done := make(chan outcome, 1)
	go func() {
		v, err := engine.EvalMultiple(ctx, code)
		done <- outcome{v, err}
	}()
	timer := time.NewTimer(d)
	defer timer.Stop()
	select {
	case o := <-done:
		return o.value, o.err
	case <-timer.C:
		t.Fatalf("Eval did not return within %v: process-wait outlived its cancellation", d)
		return nil, nil
	}
}

// TestProcessWaitCancelledRaisesAndLeavesTheChild pins process-wait under a
// cancelled ctx on a child spawned under another one (an earlier run's), which
// the cancellation therefore does not kill. The wait raises ErrOperationCancelled
// carrying the cause, as the SRFI-18 waits do, well inside the child's 30 s; it
// used to park until the child exited. The child is left running: a later run
// under a live ctx kills it (process-kill fails on a reaped child, so success
// shows it was still alive) and waits for it.
func TestProcessWaitCancelledRaisesAndLeavesTheChild(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine, ctx := newCancelEngine(t)
	eval(t, engine, `(define p (process-spawn "sleep" "30"))`)

	_, err := evalWithin(ctx, t, engine, `(begin (cancel!) (process-wait p))`, 10*time.Second)
	c.Assert(errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue, qt.Commentf("got %v", err))
	c.Assert(errors.Is(err, errProcessCancelCause), qt.IsTrue, qt.Commentf("the cause must survive, got %v", err))
	c.Assert(errors.Is(err, context.Canceled), qt.IsTrue, qt.Commentf("ctx.Err() must survive, got %v", err))
	c.Assert(err.Error(), qt.Contains, "process-wait: cancelled before the wait completed")

	result := eval(t, engine, `(begin (process-kill p 'kill) (process-wait p))`)
	c.Assert(result.SchemeString(), qt.Equals, "-1", qt.Commentf("a signalled child's exit code"))
}

// TestProcessWaitUnderWithTimeout pins process-wait as a pure wait under a
// with-timeout: the timer's expiry ends the wait, the handler runs at once, and
// resuming its continuation re-issues the wait rather than refusing it. The rows
// use the wall clock (WithTimerFiringAfter cannot fire inside a wait), but no
// answer depends on when the 50 ms timer fires: every child outlives it.
//
//   - A child spawned before the with-timeout is not killed by the expiry. The
//     handler kills it and resumes, and the re-issued wait answers the signalled
//     exit code. The wait used to park for the child's 30 s, then the handler's
//     kill failed on the reaped child.
//
// A child spawned inside the thunk dies by the timer's expiry, not by the thunk's
// return; that contract is the spawn's, pinned by
// TestChildSpawnedInATimedThunkEndsOnlyByExpiry (pkg/machine), and
// TestProcessSpawnCtxBoundsTheChild pins the host's half without a timer.
func TestProcessWaitUnderWithTimeout(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	tcs := []struct {
		name string
		code string
		want string
	}{
		{"a child spawned outside the thunk survives the expiry",
			`(define p (process-spawn "sleep" "30"))
(with-timeout 50 (lambda (k) (process-kill p 'kill) (list 'resumed (k))) (lambda () (process-wait p)))`,
			"(resumed -1)"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newEngine(t)
			t.Cleanup(func() {
				_ = engine.Close()
			})
			result, err := evalWithin(context.Background(), t, engine, tc.code, 10*time.Second)
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestProcessSpawnCtxBoundsTheChild pins the other half of process-wait's
// cancellation contract: the wait does not kill the child, but the ctx the child
// was spawned under does (exec.CommandContext). A child spawned in a run whose ctx
// then ends is killed, and a wait on it in a later run under a live ctx answers
// the killed status. Under a with-timeout the spawn ctx is the one below the
// timer, which the timer's expiry also ends but its thunk's return does not
// (machine.SpawnContext).
func TestProcessSpawnCtxBoundsTheChild(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses Unix commands")
	}
	c := qt.New(t)
	engine, ctx := newCancelEngine(t)
	_, err := evalWithin(ctx, t, engine, `(begin (define p (process-spawn "sleep" "30")) (cancel!))`, 10*time.Second)
	c.Assert(errors.Is(err, werr.ErrOperationCancelled), qt.IsTrue, qt.Commentf("got %v", err))

	result, err := evalWithin(context.Background(), t, engine, `(process-wait p)`, 10*time.Second)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "-1", qt.Commentf("the spawn's ctx must have killed the child"))
}
