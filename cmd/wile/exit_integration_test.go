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

package main

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

// TestCLI_ExitInAThreadEndsTheProgram pins that exit called in an SRFI-18 thread
// ends the whole program with that thread's status, as in Racket: the main thread
// does not continue, and its dynamic-wind after thunk does not run. The main
// thread waits on a condition variable nobody signals, so the program can end
// only through the thread's exit, whatever the scheduling. The deadline guards a
// hang; nothing asserts how long the run took. A behaviour kept on purpose, so
// this pin passes on the commit that added it. Whether the calling thread's own
// after thunks run is not pinned: none do today, an R7RS §6.14 gap
// (docs/reference/r7rs-differences.md item 22), not a decision.
func TestCLI_ExitInAThreadEndsTheProgram(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()
	binPath := buildWileBinary(c, dir)

	schemePath := filepath.Join(dir, "prog.scm")
	err := os.WriteFile(schemePath, []byte(`
(define m (make-mutex))
(define cv (make-condition-variable))
(dynamic-wind
  (lambda () #f)
  (lambda ()
    (thread-start! (make-thread (lambda () (exit 3))))
    (mutex-lock! m)
    (mutex-unlock! m cv)
    (display "main still running")
    (newline))
  (lambda ()
    (display "main after thunk")
    (newline)))
`), 0o644)
	c.Assert(err, qt.IsNil)

	ctx, cancel := context.WithTimeout(context.Background(), time.Minute)
	defer cancel()
	runOut, err := exec.CommandContext(ctx, binPath, "--file", schemePath).Output()
	c.Assert(ctx.Err(), qt.IsNil, qt.Commentf("the program did not end: %s", runOut))
	c.Assert(exitStatus(c, err), qt.Equals, 3, qt.Commentf("run output: %s", runOut))
	c.Assert(strings.Contains(string(runOut), "main still running"), qt.IsFalse, qt.Commentf("run output: %s", runOut))
	c.Assert(strings.Contains(string(runOut), "main after thunk"), qt.IsFalse, qt.Commentf("run output: %s", runOut))
}
