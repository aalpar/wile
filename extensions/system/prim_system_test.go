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

package system_test

import (
	"context"
	"os"
	"os/exec"
	"testing"
	"time"

	extsystem "github.com/aalpar/wile/extensions/system"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// newEngine creates a Wile engine with core + system extensions.
func newEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extsystem.Extension),
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
		return // parse error counts as expected error
	}
	_, err = engine.Eval(context.Background(), expr)
	qt.Assert(t, err, qt.IsNotNil)
}

// runExitSubprocess re-invokes the current test binary as a subprocess to test
// os.Exit()-calling primitives. The subprocess evaluates the given Scheme
// expression and the parent checks the resulting exit code.
func runExitSubprocess(t *testing.T, testName, schemeExpr string, expectedCode int) {
	t.Helper()
	if os.Getenv("WILE_TEST_EXIT_SUBPROCESS") == "1" {
		engine, _ := wile.NewEngine(context.Background(), wile.WithExtension(extsystem.Extension))
		_, _ = engine.EvalMultiple(context.Background(), schemeExpr)
		return
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run="+testName)
	cmd.Env = append(os.Environ(), "WILE_TEST_EXIT_SUBPROCESS=1")
	err := cmd.Run()

	if expectedCode == 0 {
		qt.Assert(t, err, qt.IsNil)
	} else {
		exitErr, ok := err.(*exec.ExitError)
		qt.Assert(t, ok, qt.IsTrue)
		qt.Assert(t, exitErr.ExitCode(), qt.Equals, expectedCode)
	}
}

// exitErrorSentinel is the exit code the child in runExitErrorSubprocess uses to
// signal that the evaluated expression RETURNED an error rather than calling
// os.Exit. It must not collide with any status a test's Scheme expression exits
// with (the exit tests use 0/1/42).
const exitErrorSentinel = 7

// runExitErrorSubprocess re-invokes the test binary as a subprocess evaluating
// schemeExpr, and asserts the primitive rejected the input with an error instead
// of exiting the process. The child exits exitErrorSentinel iff the eval returned
// an error; if the primitive instead calls os.Exit (e.g. the pre-fix silent
// coercion of a malformed status to code 0), the child exits with THAT code and
// never reaches the sentinel. The parent then sees a different (or zero) exit
// code and the assertions fail -- this is the RED that a plain in-process test
// cannot produce, because a pre-fix os.Exit(0) would tear down the test runner.
func runExitErrorSubprocess(t *testing.T, testName, schemeExpr string) {
	t.Helper()
	if os.Getenv("WILE_TEST_EXIT_ERROR_SUBPROCESS") == "1" {
		engine, _ := wile.NewEngine(context.Background(), wile.WithExtension(extsystem.Extension))
		_, err := engine.EvalMultiple(context.Background(), schemeExpr)
		if err != nil {
			os.Exit(exitErrorSentinel)
		}
		return
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run="+testName)
	cmd.Env = append(os.Environ(), "WILE_TEST_EXIT_ERROR_SUBPROCESS=1")
	err := cmd.Run()
	exitErr, ok := err.(*exec.ExitError)
	qt.Assert(t, ok, qt.IsTrue) // pre-fix os.Exit(0) makes cmd.Run() return nil -> RED here
	qt.Assert(t, exitErr.ExitCode(), qt.Equals, exitErrorSentinel)
}

func TestCommandLine(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a list", func(t *testing.T) {
		result := eval(t, engine, `(list? (command-line))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("first element is program name", func(t *testing.T) {
		// os.Args[0] is always set during test execution
		result := eval(t, engine, `(string? (car (command-line)))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("all elements are strings", func(t *testing.T) {
		result := eval(t, engine, `
			(let loop ((args (command-line)) (ok #t))
			  (if (null? args)
			      ok
			      (loop (cdr args) (and ok (string? (car args))))))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(command-line 42)`)
	})
}

func TestCurrentSecond(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a real number", func(t *testing.T) {
		result := eval(t, engine, `(real? (current-second))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("returns inexact", func(t *testing.T) {
		result := eval(t, engine, `(inexact? (current-second))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("reasonable timestamp", func(t *testing.T) {
		// Unix timestamp should be > 1700000000 (Nov 2023) and < 2000000000 (May 2033)
		result := eval(t, engine, `
			(let ((now (current-second)))
			  (and (> now 1700000000) (< now 2000000000)))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("monotonic", func(t *testing.T) {
		result := eval(t, engine, `(<= (current-second) (current-second))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(current-second 42)`)
	})
}

func TestCurrentJiffy(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns an exact integer", func(t *testing.T) {
		result := eval(t, engine, `(exact? (current-jiffy))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("returns an integer", func(t *testing.T) {
		result := eval(t, engine, `(integer? (current-jiffy))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("positive", func(t *testing.T) {
		result := eval(t, engine, `(> (current-jiffy) 0)`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("monotonic", func(t *testing.T) {
		result := eval(t, engine, `(<= (current-jiffy) (current-jiffy))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(current-jiffy 42)`)
	})
}

func TestJiffiesPerSecond(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns 1 billion", func(t *testing.T) {
		result := eval(t, engine, `(jiffies-per-second)`)
		c.Assert(result.Internal(), valuestest.SchemeEquals, values.NewInteger(1_000_000_000))
	})

	t.Run("exact integer", func(t *testing.T) {
		result := eval(t, engine, `(exact? (jiffies-per-second))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("wrong argument count", func(t *testing.T) {
		evalExpectError(t, engine, `(jiffies-per-second 42)`)
	})
}

// TestExit tests the (exit) primitive using subprocess execution.
// Since exit calls os.Exit(), we must run it in a subprocess.
func TestExit(t *testing.T) {
	t.Run("exit with no args exits 0", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_no_args_exits_0", `(exit)`, 0)
	})

	t.Run("exit with 0 exits 0", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_0_exits_0", `(exit 0)`, 0)
	})

	t.Run("exit with 1 exits 1", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_1_exits_1", `(exit 1)`, 1)
	})

	t.Run("exit with 42 exits 42", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_42_exits_42", `(exit 42)`, 42)
	})

	t.Run("exit with false exits 1", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_false_exits_1", `(exit #f)`, 1)
	})

	t.Run("exit with true exits 0", func(t *testing.T) {
		runExitSubprocess(t, "TestExit/exit_with_true_exits_0", `(exit #t)`, 0)
	})
}

// TestExitMalformedStatus verifies that a non-integer, non-boolean status is
// rejected as a catchable error rather than silently coerced to exit code 0.
// These run in-process precisely because the error path does NOT call os.Exit.
func TestExitMalformedStatus(t *testing.T) {
	engine := newEngine(t)
	tcs := []struct {
		name string
		code string
	}{
		{"string status", `(exit "done")`},
		{"symbol status", `(exit 'ok)`},
		{"float status", `(exit 2.0)`},
		{"emergency-exit string status", `(emergency-exit "done")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			evalExpectError(t, engine, tc.code)
		})
	}
}

// TestExitMalformedStatusDoesNotExit is the subprocess counterpart to
// TestExitMalformedStatus. It proves the malformed status is rejected WITHOUT
// terminating the process: the child exits exitErrorSentinel only if the eval
// returned an error. Pre-fix, (exit "done") called os.Exit(0), so the child
// exited 0 and runExitErrorSubprocess's assertions went RED -- a failure a plain
// in-process test cannot observe, since os.Exit(0) would kill the test runner.
func TestExitMalformedStatusDoesNotExit(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{"exit", `(exit "done")`},
		{"emergency", `(emergency-exit "done")`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			runExitErrorSubprocess(t, "TestExitMalformedStatusDoesNotExit/"+tc.name, tc.code)
		})
	}
}

// TestEmergencyExit tests the (emergency-exit) primitive using subprocess execution.
func TestEmergencyExit(t *testing.T) {
	t.Run("emergency-exit with no args exits 0", func(t *testing.T) {
		runExitSubprocess(t, "TestEmergencyExit/emergency-exit_with_no_args_exits_0", `(emergency-exit)`, 0)
	})

	t.Run("emergency-exit with 1 exits 1", func(t *testing.T) {
		runExitSubprocess(t, "TestEmergencyExit/emergency-exit_with_1_exits_1", `(emergency-exit 1)`, 1)
	})

	t.Run("emergency-exit with false exits 1", func(t *testing.T) {
		runExitSubprocess(t, "TestEmergencyExit/emergency-exit_with_false_exits_1", `(emergency-exit #f)`, 1)
	})
}

// TestTimingConsistency verifies that current-jiffy and jiffies-per-second
// produce values consistent with current-second.
func TestTimingConsistency(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("jiffy difference is positive over work", func(t *testing.T) {
		result := eval(t, engine, `
			(let ((start (current-jiffy)))
			  ;; do some trivial work
			  (let loop ((i 0))
			    (if (< i 1000) (loop (+ i 1))))
			  (let ((end (current-jiffy)))
			    (> end start)))
		`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})
}
