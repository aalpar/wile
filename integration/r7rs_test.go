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
	return filepath.Join(getProjectRoot(), "dist", runtime.GOOS, runtime.GOARCH, "wile")
}

// getLibPath returns the path to the stdlib/lib/ directory.
func getLibPath() string {
	return filepath.Join(getProjectRoot(), "stdlib", "lib")
}

// getTestDataPath returns the path to the testdata directory.
func getTestDataPath() string {
	_, filename, _, _ := runtime.Caller(0)
	return filepath.Join(filepath.Dir(filename), "testdata")
}

// runSchemeTest runs a Scheme test file via the scheme binary and reports results.
func runSchemeTest(t *testing.T, testFile string, timeout time.Duration, label string) {
	t.Helper()

	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	testPath := filepath.Join(getTestDataPath(), testFile)
	_, err = os.Stat(testPath)
	if os.IsNotExist(err) {
		t.Fatalf("test file not found at %s", testPath)
	}

	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", testPath)
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	output := stdout.String()
	errOutput := stderr.String()

	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("%s timed out\n\nOutput:\n%s\n\nStderr:\n%s", label, output, errOutput)
	}

	if err != nil {
		exitErr, ok := err.(*exec.ExitError)
		if ok {
			summary := extractTestSummary(output)
			t.Fatalf("%s failed with exit code %d\n\nSummary:\n%s\n\nFull Output:\n%s\n\nStderr:\n%s",
				label, exitErr.ExitCode(), summary, output, errOutput)
		}
		t.Fatalf("failed to run %s: %v\n\nOutput:\n%s\n\nStderr:\n%s", label, err, output, errOutput)
	}

	summary := extractTestSummary(output)
	if summary != "" {
		t.Logf("%s passed:\n%s", label, summary)
	}
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
	runSchemeTest(t, "r7rs-tests.scm", 5*time.Minute, "R7RS test suite")
}

// TestMicroKanren runs the microKanren integration tests.
// Tests the (wile microkanren) library for unification, goals, and streams.
func TestMicroKanren(t *testing.T) {
	runSchemeTest(t, "microkanren-tests.scm", 2*time.Minute, "microKanren tests")
}

// TestKanren runs the miniKanren macro layer integration tests.
// Tests fresh, conde, run, run*, reification, and classic relations.
func TestKanren(t *testing.T) {
	runSchemeTest(t, "kanren-tests.scm", 2*time.Minute, "kanren tests")
}

// TestSRFI132 runs the SRFI-132 sort library integration tests.
func TestSRFI132(t *testing.T) {
	runSchemeTest(t, "srfi-132-tests.scm", 2*time.Minute, "SRFI-132 sort library")
}

// TestSRFI13Phase1 runs the SRFI-13 Phase-1 (top-five) integration tests.
func TestSRFI13Phase1(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase1.scm", 2*time.Minute, "SRFI-13 Phase 1 (top-five)")
}

// TestSRFI13Phase2 runs the SRFI-13 Phase-2 (predicates, selection, prefix/suffix family) tests.
func TestSRFI13Phase2(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase2.scm", 2*time.Minute, "SRFI-13 Phase 2 (predicates + selection)")
}

// TestSRFI13Phase3 runs the SRFI-13 Phase-3 (search family) tests.
func TestSRFI13Phase3(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase3.scm", 2*time.Minute, "SRFI-13 Phase 3 (search family)")
}

// TestSRFI13Phase4 runs the SRFI-13 Phase-4 (trim and pad) tests.
func TestSRFI13Phase4(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase4.scm", 2*time.Minute, "SRFI-13 Phase 4 (trim + pad)")
}

// TestSRFI13Phase5 runs the SRFI-13 Phase-5 (binary comparison family) tests.
func TestSRFI13Phase5(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase5.scm", 2*time.Minute, "SRFI-13 Phase 5 (comparison family)")
}

// TestSRFI13Phase6 runs the SRFI-13 Phase-6 (reverse, replace/tokenize, fold/map) tests.
func TestSRFI13Phase6(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase6.scm", 2*time.Minute, "SRFI-13 Phase 6 (reverse + replace + fold)")
}

// TestSRFI13Phase7 runs the SRFI-13 Phase-7 (mutating case forms) tests.
func TestSRFI13Phase7(t *testing.T) {
	runSchemeTest(t, "srfi-13-tests-phase7.scm", 2*time.Minute, "SRFI-13 Phase 7 (mutating case)")
}

// TestSRFI14Predicates runs the SRFI-14 Phase-0 (char-set? predicate stub) smoke tests.
func TestSRFI14Predicates(t *testing.T) {
	runSchemeTest(t, "srfi-14-tests-predicates.scm", 2*time.Minute, "SRFI-14 Phase 0 (char-set? predicate)")
}

// TestSRFI14Constructors runs the SRFI-14 Phase-1 (constructors, membership, querying) integration tests.
func TestSRFI14Constructors(t *testing.T) {
	runSchemeTest(t, "srfi-14-tests-constructors.scm", 2*time.Minute, "SRFI-14 Phase 1 (constructors)")
}

// TestSRFI14Algebra runs the SRFI-14 Phase-2 (set algebra, !-aliases, derived ops) integration tests.
func TestSRFI14Algebra(t *testing.T) {
	runSchemeTest(t, "srfi-14-tests-algebra.scm", 2*time.Minute, "SRFI-14 Phase 2 (algebra)")
}

// TestSRFI14Iteration runs the SRFI-14 Phase-3 (iteration / query layer) integration tests.
func TestSRFI14Iteration(t *testing.T) {
	runSchemeTest(t, "srfi-14-tests-iteration.scm", 2*time.Minute, "SRFI-14 Phase 3 (iteration)")
}

// TestWileStrings runs the (wile strings) extras integration tests.
func TestWileStrings(t *testing.T) {
	runSchemeTest(t, "wile-strings-tests.scm", 2*time.Minute, "(wile strings) extras")
}

// extractTestSummary extracts the test summary from chibi-test output.
func extractTestSummary(output string) string {
	// Look for "Test Summary:" section
	idx := strings.Index(output, "Test Summary:")
	if idx == -1 {
		// Try to find pass/fail counts directly
		var lines []string
		for line := range strings.SplitSeq(output, "\n") {
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
