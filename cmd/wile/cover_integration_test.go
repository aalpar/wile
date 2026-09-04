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
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// buildWileBinary compiles this package into dir and returns the binary path.
func buildWileBinary(c *qt.C, dir string) string {
	c.Helper()
	binPath := filepath.Join(dir, "wile")
	buildCmd := exec.CommandContext(context.Background(), "go", "build", "-o", binPath, ".")
	// Match GOWORK=off so the test works within the worktree.
	buildCmd.Env = append(os.Environ(), "GOWORK=off")
	buildOut, err := buildCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("build output: %s", buildOut))
	return binPath
}

// exitStatus returns the process status a finished exec call reported: 0 for a
// nil error, the child's code for an *exec.ExitError.
func exitStatus(c *qt.C, err error) int {
	c.Helper()
	if err == nil {
		return 0
	}
	var exitErr *exec.ExitError
	c.Assert(errors.As(err, &exitErr), qt.IsTrue, qt.Commentf("run error: %v", err))
	return exitErr.ExitCode()
}

// TestCLI_CoverFlag_WritesGoCoverFormat compiles the binary, runs a tiny
// Scheme file under --cover, and asserts the output has the Go-cover-v1
// header and at least one entry for the run file.
//
// The (exit ...) rows pin that the profile is still written when the program
// terminates through the exit primitive rather than by returning: every real
// suite ends in (test-exit), so a profile lost on that path is a sweep of
// nothing. The status must survive too, or a failing suite would read as green.
func TestCLI_CoverFlag_WritesGoCoverFormat(t *testing.T) {
	c := qt.New(t)
	binPath := buildWileBinary(c, c.TempDir())

	tests := []struct {
		name       string
		source     string
		wantStatus int
	}{
		{"program returns", "(+ 1 2)\n", 0},
		{"program calls (exit 0)", "(+ 1 2)\n(exit 0)\n", 0},
		{"program calls (exit 3)", "(+ 1 2)\n(exit 3)\n", 3},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			dir := c.TempDir()
			schemePath := filepath.Join(dir, "prog.scm")
			err := os.WriteFile(schemePath, []byte(tt.source), 0o644)
			c.Assert(err, qt.IsNil)
			covPath := filepath.Join(dir, "cov.out")

			runCmd := exec.CommandContext(context.Background(), binPath, "--cover", covPath, "--file", schemePath)
			runOut, err := runCmd.CombinedOutput()
			c.Assert(exitStatus(c, err), qt.Equals, tt.wantStatus, qt.Commentf("run output: %s", runOut))

			data, err := os.ReadFile(covPath)
			c.Assert(err, qt.IsNil)
			content := string(data)

			c.Assert(strings.HasPrefix(content, "mode: set\n"), qt.IsTrue, qt.Commentf("got: %s", content))
			c.Assert(strings.Contains(content, schemePath), qt.IsTrue, qt.Commentf("got: %s", content))
			c.Assert(strings.Contains(content, " 1 1"), qt.IsTrue, qt.Commentf("executed sexpr should have Count=1; got: %s", content))
		})
	}
}

// TestCLI_CPUProfile_WrittenWhenProgramExits pins the same loss for
// --cpuprofile, whose file is finalized by StopCPUProfile: a program that
// terminates through (exit) must still leave a non-empty profile behind.
func TestCLI_CPUProfile_WrittenWhenProgramExits(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()
	binPath := buildWileBinary(c, dir)

	schemePath := filepath.Join(dir, "prog.scm")
	err := os.WriteFile(schemePath, []byte("(+ 1 2)\n(exit 0)\n"), 0o644)
	c.Assert(err, qt.IsNil)
	profPath := filepath.Join(dir, "cpu.out")

	runCmd := exec.CommandContext(context.Background(), binPath, "--cpuprofile", profPath, "--file", schemePath)
	runOut, err := runCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("run output: %s", runOut))

	info, err := os.Stat(profPath)
	c.Assert(err, qt.IsNil)
	c.Assert(info.Size() > 0, qt.IsTrue, qt.Commentf("CPU profile is empty: StopCPUProfile never ran"))
}
