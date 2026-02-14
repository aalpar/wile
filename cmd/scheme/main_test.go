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
	"path/filepath"
	"slices"
	"strings"
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

func TestInitLibraryRegistry(t *testing.T) {
	c := qt.New(t)

	tcs := []struct {
		name         string
		envPath      string
		cmdLinePath  string
		wantPaths    []string // Expected search paths (in order)
		wantContains []string // Paths that must be present (unordered)
	}{
		{
			name:         "no paths",
			envPath:      "",
			cmdLinePath:  "",
			wantContains: []string{".", "./lib"}, // Default paths
		},
		{
			name:         "env var only",
			envPath:      "/usr/share/scheme" + string(os.PathListSeparator) + "/opt/scheme",
			cmdLinePath:  "",
			wantContains: []string{".", "./lib", "/usr/share/scheme", "/opt/scheme"},
		},
		{
			name:         "command line only",
			envPath:      "",
			cmdLinePath:  "/home/user/.scheme" + string(os.PathListSeparator) + "/tmp/libs",
			wantContains: []string{".", "./lib", "/home/user/.scheme", "/tmp/libs"},
		},
		{
			name:         "both env and command line",
			envPath:      "/usr/share/scheme",
			cmdLinePath:  "/home/user/.scheme",
			wantContains: []string{".", "./lib", "/usr/share/scheme", "/home/user/.scheme"},
		},
		{
			name:         "empty components in env",
			envPath:      string(os.PathListSeparator) + "/usr/share/scheme" + string(os.PathListSeparator) + string(os.PathListSeparator),
			cmdLinePath:  "",
			wantContains: []string{".", "./lib", "/usr/share/scheme"},
		},
		{
			name:         "empty components in command line",
			envPath:      "",
			cmdLinePath:  string(os.PathListSeparator) + "/home/user/.scheme" + string(os.PathListSeparator) + string(os.PathListSeparator),
			wantContains: []string{".", "./lib", "/home/user/.scheme"},
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

			// Create registry
			registry := initLibraryRegistry(context.Background())

			// Get actual search paths
			paths := registry.GetSearchPaths()

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

	registry := initLibraryRegistry(context.Background())
	paths := registry.GetSearchPaths()

	// Command line paths should appear after env paths
	// (they are added last to the registry, but searched first)
	cmdPaths := []string{"/cmd/path1", "/cmd/path2"}
	envPaths := []string{"/env/path1", "/env/path2"}

	for _, cmdPath := range cmdPaths {
		c.Assert(slices.Contains(paths, cmdPath), qt.IsTrue, qt.Commentf("command line path %q not found", cmdPath))
	}

	for _, envPath := range envPaths {
		c.Assert(slices.Contains(paths, envPath), qt.IsTrue, qt.Commentf("env path %q not found", envPath))
	}
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

	t.Run("positional arg", func(t *testing.T) {
		c := qt.New(t)
		path := writeTempScheme(t, "(+ 1 2)")
		result := runCLI(t, "-q", path)
		c.Assert(result.exitCode, qt.Equals, 0)
		c.Assert(result.stdout, qt.Equals, "3\n")
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
