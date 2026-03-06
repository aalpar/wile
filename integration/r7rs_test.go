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

package integration_test

import (
	"bytes"
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
	"time"
)

// getProjectRoot returns the path to the project root directory.
func getProjectRoot() string {
	_, filename, _, _ := runtime.Caller(0)
	// integration/r7rs_test.go -> wile/
	return filepath.Join(filepath.Dir(filename), "..")
}

// getSchemeBinary returns the path to the scheme binary for the current platform.
func getSchemeBinary() string {
	return filepath.Join(getProjectRoot(), "dist", runtime.GOOS, runtime.GOARCH, "scheme")
}

// getLibPath returns the path to the lib/ directory.
func getLibPath() string {
	return filepath.Join(getProjectRoot(), "lib")
}

// getTestDataPath returns the path to the testdata directory.
func getTestDataPath() string {
	_, filename, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(filename), "testdata")
}

// TestR7RSConformance runs the comprehensive R7RS test suite.
// This test executes r7rs-tests.scm which tests all R7RS procedures and syntax.
//
// Requirements:
//   - The scheme binary must be built (make build)
//   - The test uses (chibi test) library from lib/chibi/test.sld
//
// The test suite uses (test-exit) which calls (exit 0) on success or (exit 1) on failure.
func TestR7RSConformance(t *testing.T) {
	// Check that the scheme binary exists
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testFile := filepath.Join(getTestDataPath(), "r7rs-tests.scm")
	_, err = os.Stat(testFile)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testFile)
	}

	// Set up timeout context - R7RS tests may take a while
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()

	// Build command with library path
	cmd := exec.CommandContext(ctx, schemeBin, "--file", testFile)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	// Capture output
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	// Run the test
	err = cmd.Run()

	// Get output for logging
	output := stdout.String()
	errOutput := stderr.String()

	// Check for context timeout
	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("R7RS test suite timed out after 5 minutes\n\nOutput:\n%s\n\nStderr:\n%s", output, errOutput)
	}

	// Check exit code
	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			// Test suite failed (exit code non-zero)
			// Extract summary from output if available
			summary := extractTestSummary(output)
			t.Fatalf("R7RS test suite failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run R7RS test suite: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, output, errOutput)
	}

	// Success - optionally log the summary
	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("R7RS test suite passed:\n%s", summary)
	}
}

// TestMicroKanren runs the microKanren integration tests.
// Tests the (wile microkanren) library for unification, goals, and streams.
func TestMicroKanren(t *testing.T) {
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testFile := filepath.Join(getTestDataPath(), "microkanren-tests.scm")
	_, err = os.Stat(testFile)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testFile)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", testFile)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	output := stdout.String()
	errOutput := stderr.String()

	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("microKanren tests timed out\n\nOutput:\n%s\n\nStderr:\n%s", output, errOutput)
	}

	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			summary := extractTestSummary(output)
			t.Fatalf("microKanren tests failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run microKanren tests: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, output, errOutput)
	}

	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("microKanren tests passed:\n%s", summary)
	}
}

// TestKanren runs the miniKanren macro layer integration tests.
// Tests fresh, conde, run, run*, reification, and classic relations.
func TestKanren(t *testing.T) {
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testFile := filepath.Join(getTestDataPath(), "kanren-tests.scm")
	_, err = os.Stat(testFile)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testFile)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", testFile)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	output := stdout.String()
	errOutput := stderr.String()

	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("kanren tests timed out\n\nOutput:\n%s\n\nStderr:\n%s", output, errOutput)
	}

	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			summary := extractTestSummary(output)
			t.Fatalf("kanren tests failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run kanren tests: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, output, errOutput)
	}

	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("kanren tests passed:\n%s", summary)
	}
}

// extractTestSummary extracts the test summary from chibi-test output.
func extractTestSummary(output string) string {
	// Look for "Test Summary:" section
	idx := strings.Index(output, "Test Summary:")
	if idx == -1 {
		// Try to find pass/fail counts directly
		var lines []string
		for _, line := range strings.Split(output, "\n") {
			if strings.Contains(line, "Passed:") || strings.Contains(line, "Failed:") {
				lines = append(lines, strings.TrimSpace(line))
			}
		}
		if len(lines) > 0 {
			return strings.Join(lines, "\n")
		}
		return ""
	}
	return strings.TrimSpace(output[idx:])
}
