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
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"slices"
	"strings"
	"syscall"
	"testing"
	"time"

	qt "github.com/frankban/quicktest"
)

// Subprocess environment variable names.
const (
	// envSubprocess triggers TestMain to decode CLI args and call main().
	envSubprocess = "WILE_CLI_TEST_SUBPROCESS"
	// envCLIArgs carries base64(json(args)) for the subprocess.
	envCLIArgs = "WILE_CLI_TEST_ARGS"
	// envExitSubprocess triggers per-test subprocess branches (Failf, Printf).
	envExitSubprocess = "WILE_CLI_EXIT_SUBPROCESS"
)

// ---------------------------------------------------------------------------
// Subprocess dispatcher
// ---------------------------------------------------------------------------

// TestMain intercepts subprocess invocations for CLI integration tests.
// When envSubprocess is set, it decodes CLI args from envCLIArgs, sets
// os.Args, and calls main() — exercising the real CLI entry point in an
// isolated process.
func TestMain(m *testing.M) {
	if os.Getenv(envSubprocess) == "1" {
		encoded := os.Getenv(envCLIArgs)
		argsJSON, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			fmt.Fprintf(os.Stderr, "subprocess: failed to decode args: %v\n", err)
			os.Exit(2)
		}
		var args []string
		err = json.Unmarshal(argsJSON, &args)
		if err != nil {
			fmt.Fprintf(os.Stderr, "subprocess: failed to unmarshal args: %v\n", err)
			os.Exit(2)
		}
		os.Args = args
		main()
		return
	}
	os.Exit(m.Run())
}

// ---------------------------------------------------------------------------
// Infrastructure types and helpers
// ---------------------------------------------------------------------------

// cliResult captures stdout, stderr, and exit code from a subprocess.
type cliResult struct {
	stdout   string
	stderr   string
	exitCode int
}

// runCLI spawns a subprocess that calls main() with the given CLI args.
func runCLI(t *testing.T, args ...string) cliResult {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	// Encode args as base64(json(["scheme", args...]))
	fullArgs := append([]string{"scheme"}, args...)
	argsJSON, err := json.Marshal(fullArgs)
	if err != nil {
		t.Fatalf("failed to marshal args: %v", err)
	}
	encoded := base64.StdEncoding.EncodeToString(argsJSON)

	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^$")
	cmd.Env = append(os.Environ(),
		envSubprocess+"=1",
		envCLIArgs+"="+encoded,
	)

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()

	result := cliResult{
		stdout:   stdout.String(),
		stderr:   stderr.String(),
		exitCode: 0,
	}

	if err != nil {
		if ctx.Err() != nil {
			t.Fatalf("subprocess timed out after 10s")
		}
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			result.exitCode = exitErr.ExitCode()
		} else {
			t.Fatalf("unexpected error running subprocess: %v", err)
		}
	}

	return result
}

// runExitSubprocess spawns a subprocess to run a specific test function.
// The target test function should check envExitSubprocess == "1" at its top
// and call the function under test directly.
func runExitSubprocess(t *testing.T, testName string) cliResult {
	t.Helper()

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^"+testName+"$")
	cmd.Env = append(os.Environ(), envExitSubprocess+"=1")

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()

	result := cliResult{
		stdout:   stdout.String(),
		stderr:   stderr.String(),
		exitCode: 0,
	}

	if err != nil {
		if ctx.Err() != nil {
			t.Fatalf("subprocess timed out")
		}
		var exitErr *exec.ExitError
		if errors.As(err, &exitErr) {
			result.exitCode = exitErr.ExitCode()
		} else {
			t.Fatalf("unexpected error: %v", err)
		}
	}

	return result
}

// writeTempScheme writes Scheme source to a temp .scm file and returns its path.
func writeTempScheme(t *testing.T, content string) string {
	t.Helper()
	dir := t.TempDir()
	path := filepath.Join(dir, "test.scm")
	err := os.WriteFile(path, []byte(content), 0644)
	if err != nil {
		t.Fatalf("failed to write temp scheme file: %v", err)
	}
	return path
}

// ---------------------------------------------------------------------------
// Existing unit tests (no subprocess needed)
// ---------------------------------------------------------------------------

func TestBuildLibraryPaths(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name         string
		envPath      string
		cmdLinePath  string
		wantContains []string // Paths that must be present (unordered)
	}{
		{
			name:         "no paths",
			envPath:      "",
			cmdLinePath:  "",
			wantContains: nil, // No user-specified paths
		},
		{
			name:         "env var only",
			envPath:      "/usr/share/wile" + string(os.PathListSeparator) + "/opt/wile",
			cmdLinePath:  "",
			wantContains: []string{"/usr/share/wile", "/opt/wile"},
		},
		{
			name:         "command line only",
			envPath:      "",
			cmdLinePath:  "/home/user/.scheme" + string(os.PathListSeparator) + "/tmp/libs",
			wantContains: []string{"/home/user/.scheme", "/tmp/libs"},
		},
		{
			name:         "both env and command line",
			envPath:      "/usr/share/wile",
			cmdLinePath:  "/home/user/.scheme",
			wantContains: []string{"/usr/share/wile", "/home/user/.scheme"},
		},
		{
			name:         "empty components in env",
			envPath:      string(os.PathListSeparator) + "/usr/share/wile" + string(os.PathListSeparator) + string(os.PathListSeparator),
			cmdLinePath:  "",
			wantContains: []string{"/usr/share/wile"},
		},
		{
			name:         "empty components in command line",
			envPath:      "",
			cmdLinePath:  string(os.PathListSeparator) + "/home/user/.scheme" + string(os.PathListSeparator) + string(os.PathListSeparator),
			wantContains: []string{"/home/user/.scheme"},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// Set up environment
			oldEnv := os.Getenv(SchemeLibraryPathEnv)
			if tc.envPath != "" {
				t.Setenv(SchemeLibraryPathEnv, tc.envPath)
			} else {
				os.Unsetenv(SchemeLibraryPathEnv)
			}
			defer func() {
				if oldEnv != "" {
					os.Setenv(SchemeLibraryPathEnv, oldEnv)
				} else {
					os.Unsetenv(SchemeLibraryPathEnv)
				}
			}()

			// Set command line flag
			oldOpts := opts
			opts.LibraryPath = tc.cmdLinePath
			defer func() {
				opts = oldOpts
			}()

			paths := buildLibraryPaths()

			// Check that all expected paths are present
			for _, wantPath := range tc.wantContains {
				c.Assert(slices.Contains(paths, wantPath), qt.IsTrue, qt.Commentf("expected path %q not found in %v", wantPath, paths))
			}
		})
	}
}

func TestLibraryPathPriority(t *testing.T) {
	c := qt.New(t)

	// Set up environment variable
	envPath := "/env/path1" + string(os.PathListSeparator) + "/env/path2"
	t.Setenv(SchemeLibraryPathEnv, envPath)

	// Set command line flag
	cmdPath := "/cmd/path1" + string(os.PathListSeparator) + "/cmd/path2"
	oldOpts := opts
	opts.LibraryPath = cmdPath
	defer func() {
		opts = oldOpts
	}()

	paths := buildLibraryPaths()

	// Command line paths should appear before env paths (higher priority)
	cmdPaths := []string{"/cmd/path1", "/cmd/path2"}
	envPaths := []string{"/env/path1", "/env/path2"}

	for _, cp := range cmdPaths {
		c.Assert(slices.Contains(paths, cp), qt.IsTrue, qt.Commentf("command line path %q not found", cp))
	}

	for _, ep := range envPaths {
		c.Assert(slices.Contains(paths, ep), qt.IsTrue, qt.Commentf("env path %q not found", ep))
	}

	// Verify command-line paths come before env paths in the slice
	cmdIdx := slices.Index(paths, "/cmd/path1")
	envIdx := slices.Index(paths, "/env/path1")
	c.Assert(cmdIdx < envIdx, qt.IsTrue,
		qt.Commentf("command line paths should precede env paths: cmd at %d, env at %d, paths=%v", cmdIdx, envIdx, paths))
}

func TestSchemeLibraryPathEnvConstant(t *testing.T) {
	c := qt.New(t)
	c.Assert(SchemeLibraryPathEnv, qt.Equals, "SCHEME_LIBRARY_PATH")
}

func TestBuildVariables(t *testing.T) {
	// BuildSHA and BuildVersion are set via -ldflags at build time
	// They may be empty in test builds
	// Just verify they are string variables
	_ = BuildSHA
	_ = BuildVersion

	// Verify they can be set (simulating -ldflags)
	oldSHA := BuildSHA
	oldVer := BuildVersion
	defer func() {
		BuildSHA = oldSHA
		BuildVersion = oldVer
	}()

	BuildSHA = "abc123"
	BuildVersion = "v1.0.0"

	if !strings.Contains(BuildSHA, "abc123") {
		t.Errorf("BuildSHA assignment failed")
	}
	if !strings.Contains(BuildVersion, "v1.0.0") {
		t.Errorf("BuildVersion assignment failed")
	}
}

func TestResolveVersion(t *testing.T) {
	oldSHA := BuildSHA
	oldVer := BuildVersion
	defer func() {
		BuildSHA = oldSHA
		BuildVersion = oldVer
	}()

	t.Run("ldflags take priority", func(t *testing.T) {
		BuildVersion = "v9.9.9"
		BuildSHA = "deadbeef"
		v, s := resolveVersion()
		if v != "v9.9.9" {
			t.Errorf("expected v9.9.9, got %s", v)
		}
		if s != "deadbeef" {
			t.Errorf("expected deadbeef, got %s", s)
		}
	})

	t.Run("fallback when ldflags empty", func(t *testing.T) {
		BuildVersion = ""
		BuildSHA = ""
		v, s := resolveVersion()
		// In test binaries, ReadBuildInfo returns module info.
		// We just verify the function doesn't panic and returns strings.
		_ = v
		_ = s
	})
}

// ---------------------------------------------------------------------------
// Phase 2: Flag parsing tests
// ---------------------------------------------------------------------------

func TestMainFlags(t *testing.T) {
	t.Run("version", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "--version")
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(
			strings.Contains(result.stdout, "Wile Scheme"), qt.IsTrue,
			qt.Commentf("stdout: %q", result.stdout),
		)
	})

	t.Run("help", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "--help")
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(
			strings.Contains(result.stdout, "Usage:"), qt.IsTrue,
			qt.Commentf("stdout: %q", result.stdout),
		)
	})

	t.Run("invalid flag", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "--bogus")
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
		c.Assert(result.stderr, qt.Not(qt.Equals), "")
	})

	t.Run("quiet suppresses log", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(+ 1 2)")
		result := runCLI(t, "-q", "-f", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stderr, qt.Equals, "")
	})
}

// ---------------------------------------------------------------------------
// Phase 3: File execution tests
// ---------------------------------------------------------------------------

func TestRunFile(t *testing.T) {
	tcs := []struct {
		name    string
		content string
		stdout  string
	}{
		{
			name:    "single expression",
			content: "(+ 1 2 3)",
			stdout:  "6\n",
		},
		{
			name:    "multiple expressions",
			content: "(define x 10)\n(+ x 20)",
			stdout:  "30\n",
		},
		{
			name:    "void result",
			content: "(define z 42)",
			stdout:  "",
		},
		{
			name:    "empty file",
			content: "",
			stdout:  "",
		},
		{
			name:    "comment only",
			content: "; just a comment\n",
			stdout:  "",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			path := writeTempScheme(t, tc.content)
			result := runCLI(t, "-q", "-f", path)
			c.Assert(result.exitCode, qt.Equals, 0)
			c.Assert(result.stdout, qt.Equals, tc.stdout)
		})
	}

	t.Run("multiple files share environment", func(t *testing.T) {
		c := qt.New(t)
		file1 := writeTempScheme(t, "(define x 10)")
		file2 := writeTempScheme(t, "(+ x 20)")
		result := runCLI(t, "-f", file1, "-f", file2)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, "30\n")
	})

	t.Run("positional arg", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(+ 1 2)")
		result := runCLI(t, "-q", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, "3\n")
	})
}

// ---------------------------------------------------------------------------
// Phase 3b: Shebang and command-line argument tests
// ---------------------------------------------------------------------------

func TestShebang(t *testing.T) {
	t.Run("shebang line skipped for positional arg", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "#!/usr/bin/env scheme\n(+ 1 2)")
		result := runCLI(t, "-q", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, "3\n")
	})

	t.Run("shebang not skipped for -f flag", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "#!/usr/bin/env scheme\n(+ 1 2)")
		result := runCLI(t, "-q", "-f", path)
		// #! is not valid Scheme when fed via -f, so this should fail
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
	})

	t.Run("file without shebang still works as positional arg", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(+ 10 20)")
		result := runCLI(t, "-q", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, "30\n")
	})

	t.Run("shebang with script arguments via command-line", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "#!/usr/bin/env scheme\n(display (length (command-line)))\n(newline)")
		result := runCLI(t, "-q", path, "arg1", "arg2")
		c.Assert(result.exitCode, qt.Equals, 0)
		// (command-line) returns (script-name arg1 arg2) => length 3
		c.Assert(result.stdout, qt.Equals, "3\n")
	})

	t.Run("command-line first element is script name", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "#!/usr/bin/env scheme\n(display (car (command-line)))\n(newline)")
		result := runCLI(t, "-q", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, path+"\n")
	})
}

// ---------------------------------------------------------------------------
// Phase 4: Error tests
// ---------------------------------------------------------------------------

func TestRunFileErrors(t *testing.T) {
	tcs := []struct {
		name    string
		content string
	}{
		{
			name:    "unterminated string",
			content: `"unterminated`,
		},
		{
			name:    "undefined variable",
			content: "(+ undefined-var 1)",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			path := writeTempScheme(t, tc.content)
			result := runCLI(t, "-q", "-f", path)
			c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
			c.Assert(
				strings.Contains(result.stderr, "Error:"), qt.IsTrue,
				qt.Commentf("stderr: %q", result.stderr),
			)
		})
	}

	t.Run("nonexistent file", func(t *testing.T) {
		c := qt.New(t)
		result := runCLI(t, "-q", "-f", "/no/such/file.scm")
		c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
		c.Assert(
			strings.Contains(result.stderr, "Error:"), qt.IsTrue,
			qt.Commentf("stderr: %q", result.stderr),
		)
	})
}

// ---------------------------------------------------------------------------
// Phase 5: Direct function tests (per-test subprocess branches)
// ---------------------------------------------------------------------------

func TestFailfNilNoMessage(t *testing.T) {
	if os.Getenv(envExitSubprocess) == "1" {
		Failf(nil)
		return
	}
	c := qt.New(t)
	result := runExitSubprocess(t, "TestFailfNilNoMessage")
	c.Assert(result.exitCode, qt.Equals, 0)
	c.Assert(result.stderr, qt.Equals, "")
}

func TestFailfWithError(t *testing.T) {
	if os.Getenv(envExitSubprocess) == "1" {
		Failf(fmt.Errorf("boom"), "ctx")
		return
	}
	c := qt.New(t)
	result := runExitSubprocess(t, "TestFailfWithError")
	c.Assert(result.exitCode, qt.Equals, 1)
	c.Assert(
		strings.Contains(result.stderr, "Error: boom: ctx"), qt.IsTrue,
		qt.Commentf("stderr: %q", result.stderr),
	)
}

func TestFailfNilWithMessage(t *testing.T) {
	if os.Getenv(envExitSubprocess) == "1" {
		Failf(nil, "msg")
		return
	}
	c := qt.New(t)
	result := runExitSubprocess(t, "TestFailfNilWithMessage")
	c.Assert(result.exitCode, qt.Equals, 1)
	c.Assert(
		strings.Contains(result.stderr, "Error: msg"), qt.IsTrue,
		qt.Commentf("stderr: %q", result.stderr),
	)
}

// TestSetupSignalsDirect calls setupSignals directly to cover the function body.
// The goroutine body (SIGQUIT handler) cannot be exercised in tests without
// sending SIGQUIT, so we cover the outer function structure.
func TestSetupSignalsDirect(t *testing.T) {
	t.Cleanup(func() {
		signal.Reset(syscall.SIGQUIT)
	})
	// quiet=false covers both the signal registration and the if-!quiet print
	setupSignals(false)
}

// TestResolveVersionPartialLdflags covers the case where only one ldflags
// variable is set, causing the function to fall through to ReadBuildInfo.
// ---------------------------------------------------------------------------
// Phase 6: LoadPathStack tests (include resolution from CLI)
// ---------------------------------------------------------------------------

func TestRunFilePopulatesLoadPathStack(t *testing.T) {
	t.Run("relative include resolved via file directory", func(t *testing.T) {
		c := qt.New(t)

		// Create dir/main.scm that includes dir/helper.scm via relative path
		dir := t.TempDir()
		err := os.WriteFile(
			filepath.Join(dir, "helper.scm"),
			[]byte(`(define lps-test-val 99)`),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		mainPath := filepath.Join(dir, "main.scm")
		err = os.WriteFile(
			mainPath,
			[]byte("(include \"helper.scm\")\n(display lps-test-val)\n(newline)"),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		result := runCLI(t, "-q", "-f", mainPath)
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		c.Assert(result.stdout, qt.Equals, "99\n")
	})

	t.Run("nested include resolved via intermediate file directory", func(t *testing.T) {
		c := qt.New(t)

		// Create dir/main.scm -> dir/sub/mid.scm -> dir/sub/leaf.scm
		dir := t.TempDir()
		subDir := filepath.Join(dir, "sub")
		err := os.Mkdir(subDir, 0o755)
		c.Assert(err, qt.IsNil)

		err = os.WriteFile(
			filepath.Join(subDir, "leaf.scm"),
			[]byte(`(define nested-val 77)`),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		err = os.WriteFile(
			filepath.Join(subDir, "mid.scm"),
			[]byte(`(include "leaf.scm")`),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		mainPath := filepath.Join(dir, "main.scm")
		err = os.WriteFile(
			mainPath,
			[]byte("(include \"sub/mid.scm\")\n(display nested-val)\n(newline)"),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		result := runCLI(t, "-q", "-f", mainPath)
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		c.Assert(result.stdout, qt.Equals, "77\n")
	})

	t.Run("current-load-path returns file path", func(t *testing.T) {
		c := qt.New(t)
		dir := t.TempDir()
		mainPath := filepath.Join(dir, "test.scm")
		err := os.WriteFile(
			mainPath,
			[]byte(`(display (current-load-path))(newline)`),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		result := runCLI(t, "-q", "-f", mainPath)
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		c.Assert(result.stdout, qt.Equals, mainPath+"\n")
	})

	t.Run("load path stack empty after file completes", func(t *testing.T) {
		c := qt.New(t)

		// After loading a file, the stack should be empty.
		// Verify by loading two files: second file should see empty stack.
		dir := t.TempDir()
		file1 := filepath.Join(dir, "first.scm")
		file2 := filepath.Join(dir, "second.scm")

		err := os.WriteFile(file1, []byte(`(define x 1)`), 0o644)
		c.Assert(err, qt.IsNil)
		err = os.WriteFile(
			file2,
			[]byte(`(display (current-load-path))(newline)`),
			0o644,
		)
		c.Assert(err, qt.IsNil)

		// file2 is the last file, so it goes through runFile; file1 goes through runtime.Load
		result := runCLI(t, "-q", "-f", file1, "-f", file2)
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		// file2 should see its own path (not file1's)
		c.Assert(result.stdout, qt.Equals, file2+"\n")
	})
}

// ---------------------------------------------------------------------------
// Phase 7: -e/--eval expression evaluation tests
// ---------------------------------------------------------------------------

func TestRunEval(t *testing.T) {
	tcs := []struct {
		name   string
		args   []string
		stdout string
	}{
		{
			name:   "single expression",
			args:   []string{"-q", "-e", "(+ 1 2)"},
			stdout: "3\n",
		},
		{
			name:   "multiple -e flags joined",
			args:   []string{"-q", "-e", "(define x 10)", "-e", "(+ x 20)"},
			stdout: "30\n",
		},
		{
			name:   "void result produces no output",
			args:   []string{"-q", "-e", "(define z 42)"},
			stdout: "",
		},
		{
			name:   "display side effect",
			args:   []string{"-q", "-e", `(display "hello")(newline)`},
			stdout: "hello\n",
		},
		{
			name:   "multiple expressions in single -e",
			args:   []string{"-q", "-e", "(define a 3)(define b 4)(+ a b)"},
			stdout: "7\n",
		},
		{
			name:   "string result",
			args:   []string{"-q", "-e", `(string-append "foo" "bar")`},
			stdout: "\"foobar\"\n",
		},
		{
			name:   "boolean result",
			args:   []string{"-q", "-e", "(< 1 2)"},
			stdout: "#t\n",
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := runCLI(t, tc.args...)
			c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
			c.Assert(result.stdout, qt.Equals, tc.stdout)
		})
	}
}

func TestRunEvalWithFile(t *testing.T) {
	t.Run("file defines, -e uses", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(define setup-val 99)")
		result := runCLI(t, "-q", "-f", path, "-e", "(+ setup-val 1)")
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		c.Assert(result.stdout, qt.Equals, "100\n")
	})

	t.Run("file loads silently when -e present", func(t *testing.T) {
		c := qt.New(t)
		// With -e present, even the last file goes through Load (silent),
		// not runFile (which would print the result).
		path := writeTempScheme(t, "(+ 1 2)")
		result := runCLI(t, "-q", "-f", path, "-e", "(+ 3 4)")
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		// Only the -e result should appear, not the file's result
		c.Assert(result.stdout, qt.Equals, "7\n")
	})

	t.Run("multiple files then -e", func(t *testing.T) {
		c := qt.New(t)
		file1 := writeTempScheme(t, "(define a 10)")
		file2 := writeTempScheme(t, "(define b 20)")
		result := runCLI(t, "-q", "-f", file1, "-f", file2, "-e", "(+ a b)")
		c.Assert(result.exitCode, qt.Equals, 0, qt.Commentf("stderr: %s", result.stderr))
		c.Assert(result.stdout, qt.Equals, "30\n")
	})
}

func TestRunEvalErrors(t *testing.T) {
	tcs := []struct {
		name string
		args []string
	}{
		{
			name: "parse error",
			args: []string{"-q", "-e", "(+ 1 . 2 . 3)"},
		},
		{
			name: "undefined variable",
			args: []string{"-q", "-e", "(+ no-such-var 1)"},
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			result := runCLI(t, tc.args...)
			c.Assert(result.exitCode, qt.Not(qt.Equals), 0)
			c.Assert(
				strings.Contains(result.stderr, "Error:"), qt.IsTrue,
				qt.Commentf("stderr: %q", result.stderr),
			)
		})
	}
}

func TestResolveVersionPartialLdflags(t *testing.T) {
	oldSHA := BuildSHA
	oldVer := BuildVersion
	defer func() {
		BuildSHA = oldSHA
		BuildVersion = oldVer
	}()

	t.Run("only version set reads SHA from build info", func(t *testing.T) {
		BuildVersion = "v1.2.3"
		BuildSHA = "" // missing — falls through to ReadBuildInfo
		v, s := resolveVersion()
		if v != "v1.2.3" {
			t.Errorf("expected v1.2.3, got %s", v)
		}
		// s may or may not be set depending on vcs.revision in build info
		_ = s
	})

	t.Run("only sha set reads version from build info", func(t *testing.T) {
		BuildVersion = "" // missing — falls through to ReadBuildInfo
		BuildSHA = "abc1234"
		v, s := resolveVersion()
		if s != "abc1234" {
			t.Errorf("expected abc1234, got %s", s)
		}
		_ = v
	})
}
