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
	"strconv"
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
// The uncaught-error row pins the CLI's own failure exit: an error the program
// never handles ends the process through fail, which is a third route to
// os.Exit and must flush what the run collected before it got there.
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
		{"program raises uncaught error", "(+ 1 2)\n(error \"boom\")\n", 1},
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

// libraryCoverCounts returns, for every profile entry attributed to file,
// the entry's start line paired with its hit count.
func libraryCoverCounts(c *qt.C, profile, file string) map[int][]int {
	c.Helper()
	counts := map[int][]int{}
	for line := range strings.SplitSeq(profile, "\n") {
		if !strings.HasPrefix(line, file+":") {
			continue
		}
		// file:startLine.startCol,endLine.endCol numStmts count
		pos := strings.TrimPrefix(line, file+":")
		startLine, _, ok := strings.Cut(pos, ".")
		c.Assert(ok, qt.IsTrue, qt.Commentf("malformed entry: %s", line))
		fields := strings.Fields(line)
		c.Assert(len(fields), qt.Equals, 3, qt.Commentf("malformed entry: %s", line))
		ln, err := strconv.Atoi(startLine)
		c.Assert(err, qt.IsNil, qt.Commentf("malformed entry: %s", line))
		count, err := strconv.Atoi(fields[2])
		c.Assert(err, qt.IsNil, qt.Commentf("malformed entry: %s", line))
		counts[ln] = append(counts[ln], count)
	}
	return counts
}

// TestCLI_CoverFlag_TracksImportedLibraryBodies pins that a library loaded by
// (import ...) is instrumented like the program that imports it. The library
// body is compiled by the loader, not by either program compile path, so it
// used to reach the profile only through macro-template attributions; the
// definitions themselves appeared nowhere, not even at count 0.
//
// The called definition's body must read 1 and the uncalled one's 0, which
// also pins that instrumentation happens BEFORE the library body executes:
// hooking the import observer, which fires after execution, would leave every
// top-level library form reading 0.
func TestCLI_CoverFlag_TracksImportedLibraryBodies(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()
	binPath := buildWileBinary(c, dir)

	libDir := filepath.Join(dir, "lib")
	err := os.MkdirAll(filepath.Join(libDir, "my"), 0o755)
	c.Assert(err, qt.IsNil)
	libPath := filepath.Join(libDir, "my", "lib.sld")
	libSource := "(define-library (my lib)\n" +
		"  (export called uncalled)\n" +
		"  (import (scheme base))\n" +
		"  (begin\n" +
		"    (define (called) (+ 1 1))\n" + // line 5
		"    (define (uncalled) (+ 2 2))))\n" // line 6
	err = os.WriteFile(libPath, []byte(libSource), 0o644)
	c.Assert(err, qt.IsNil)

	schemePath := filepath.Join(dir, "prog.scm")
	err = os.WriteFile(schemePath, []byte("(import (my lib))\n(called)\n"), 0o644)
	c.Assert(err, qt.IsNil)
	covPath := filepath.Join(dir, "cov.out")

	runCmd := exec.CommandContext(context.Background(), binPath, "--cover", covPath, "-L", libDir, "--file", schemePath)
	runOut, err := runCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("run output: %s", runOut))

	data, err := os.ReadFile(covPath)
	c.Assert(err, qt.IsNil)
	content := string(data)
	counts := libraryCoverCounts(c, content, libPath)

	c.Assert(counts[5], qt.Not(qt.HasLen), 0, qt.Commentf("called definition missing from profile: %s", content))
	c.Assert(counts[6], qt.Not(qt.HasLen), 0, qt.Commentf("uncalled definition missing from profile: %s", content))
	c.Assert(counts[5], qt.Not(qt.Contains), 0, qt.Commentf("called definition has an unexecuted entry: %s", content))
	c.Assert(counts[6], qt.Contains, 0, qt.Commentf("uncalled definition body should read 0: %s", content))
}

// TestCLI_CoverStdlib_GatesEmbeddedLibraryBodies pins that an embedded stdlib
// library body imported by the program lands in the profile, and that the
// --cover-stdlib flag is what un-filters it: instrumenting library bodies must
// not leak stdlib rows into the default report.
func TestCLI_CoverStdlib_GatesEmbeddedLibraryBodies(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()
	binPath := buildWileBinary(c, dir)

	schemePath := filepath.Join(dir, "prog.scm")
	err := os.WriteFile(schemePath, []byte("(import (srfi 1))\n(first '(1 2))\n"), 0o644)
	c.Assert(err, qt.IsNil)

	tests := []struct {
		name       string
		extraArgs  []string
		wantStdlib bool
	}{
		{"default filters stdlib", nil, false},
		{"--cover-stdlib includes stdlib", []string{"--cover-stdlib"}, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			covPath := filepath.Join(c.TempDir(), "cov.out")
			args := append([]string{"--cover", covPath}, tt.extraArgs...)
			args = append(args, "--file", schemePath)

			runCmd := exec.CommandContext(context.Background(), binPath, args...)
			runOut, err := runCmd.CombinedOutput()
			c.Assert(err, qt.IsNil, qt.Commentf("run output: %s", runOut))

			data, err := os.ReadFile(covPath)
			c.Assert(err, qt.IsNil)
			content := string(data)
			c.Assert(strings.Contains(content, "\nsrfi/1"), qt.Equals, tt.wantStdlib, qt.Commentf("got: %s", content))
		})
	}
}
