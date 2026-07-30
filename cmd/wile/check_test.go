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
	"os"
	"path/filepath"
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// writeTempSchemeNamed writes content to a named file in a fresh temp dir, for
// the multi-file cases where two sources must coexist.
func writeTempSchemeNamed(t *testing.T, dir string, name string, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	err := os.WriteFile(path, []byte(content), 0644)
	if err != nil {
		t.Fatalf("failed to write %s: %v", name, err)
	}
	return path
}

func TestCheckFlag(t *testing.T) {
	t.Run("clean file exits zero", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(define (f x) x)\n(f 1)\n")
		result := runCLI(t, "-q", "--check", path)
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(result.stdout, qt.Equals, "",
			qt.Commentf("--check prints nothing on success"))
	})

	t.Run("unbound binding exits one with location", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(define (g) (nope 1))\n")
		result := runCLI(t, "-q", "--check", path)
		c.Assert(result.exitCode, qt.Equals, 1)
		c.Assert(strings.Contains(result.stderr, `no such binding "nope"`), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(strings.Contains(result.stderr, "test.scm:"), qt.IsTrue,
			qt.Commentf("stderr must carry file:line:col — stderr: %q", result.stderr))
	})

	t.Run("does not run top level", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, `(display "RAN")`+"\n")
		result := runCLI(t, "-q", "--check", path)
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(strings.Contains(result.stdout, "RAN"), qt.IsFalse,
			qt.Commentf("stdout: %q", result.stdout))
	})

	t.Run("checks -e expressions", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "-q", "--check", "-e", "(define (g) (nope 1))")
		c.Assert(result.exitCode, qt.Equals, 1)
		c.Assert(strings.Contains(result.stderr, `no such binding "nope"`), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
	})

	t.Run("-e result is not printed", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "-q", "--check", "-e", "(+ 1 2)")
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(result.stdout, qt.Equals, "",
			qt.Commentf("nothing ran, so there is no result to print"))
	})

	// Every non-last file loads through a silent path that runs it, distinct
	// from the last file's runFile path. A --check implementation that patched
	// only runFile would execute this one.
	t.Run("does not run earlier files", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		first := writeTempSchemeNamed(t, dir, "first.scm", `(display "FIRST RAN")`+"\n")
		second := writeTempSchemeNamed(t, dir, "second.scm", "(define (h) 1)\n")
		result := runCLI(t, "-q", "--check", "-f", first, "-f", second)
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(strings.Contains(result.stdout, "FIRST RAN"), qt.IsFalse,
			qt.Commentf("stdout: %q", result.stdout))
	})

	// Same hazard on the -e path: files preceding -e also load through the
	// silent running path.
	t.Run("does not run files when -e is present", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, `(display "FILE RAN")`+"\n")
		result := runCLI(t, "-q", "--check", "-f", path, "-e", "(+ 1 2)")
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(strings.Contains(result.stdout, "FILE RAN"), qt.IsFalse,
			qt.Commentf("stdout: %q", result.stdout))
	})

	t.Run("later file sees earlier file definitions", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		first := writeTempSchemeNamed(t, dir, "first.scm", "(define (shared x) x)\n")
		second := writeTempSchemeNamed(t, dir, "second.scm", "(define (h) (shared 1))\n")
		result := runCLI(t, "-q", "--check", "-f", first, "-f", second)
		c.Assert(result.exitCode, qt.Equals, 0,
			qt.Commentf("stderr: %q", result.stderr))
	})

	t.Run("reports the first failing file", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		first := writeTempSchemeNamed(t, dir, "first.scm", "(define (g) (nope 1))\n")
		second := writeTempSchemeNamed(t, dir, "second.scm", "(define (k) (alsonope 1))\n")
		result := runCLI(t, "-q", "--check", "-f", first, "-f", second)
		c.Assert(result.exitCode, qt.Equals, 1)
		c.Assert(strings.Contains(result.stderr, `no such binding "nope"`), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
		c.Assert(strings.Contains(result.stderr, "alsonope"), qt.IsFalse,
			qt.Commentf("checking stops at the first failure — stderr: %q", result.stderr))
	})

	t.Run("reads stdin", func(t *testing.T) {
		c := qt.New(t)
		result := runCLIStdin(t, "(define (g) (nope 1))\n", "-q", "--check", "-")
		c.Assert(result.exitCode, qt.Equals, 1)
		c.Assert(strings.Contains(result.stderr, `no such binding "nope"`), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
	})

	t.Run("conflicts with interactive", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(+ 1 2)\n")
		result := runCLI(t, "-q", "--check", "-i", path)
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
		c.Assert(strings.Contains(result.stderr, "--check"), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
	})

	t.Run("conflicts with mcp", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "-q", "--check", "--mcp")
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
		c.Assert(strings.Contains(result.stderr, "--check"), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
	})

	// Without an explicit rejection this falls through to the REPL, which is
	// the one thing --check must never do.
	t.Run("requires input", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "-q", "--check")
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
		c.Assert(strings.Contains(result.stderr, "--check"), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr))
	})
}
