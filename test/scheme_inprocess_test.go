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

package test

import (
	"context"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// The in-process Scheme suite driver.
//
// The sibling TestSchemeTestSuite (scheme_test.go) runs the same .scm files by
// exec'ing the built binary. That is the right shape for exercising the shipped
// CLI, and the wrong shape for coverage: Go's coverage instrumentation does not
// cross a process boundary, so every Go line the Scheme suite exercises is
// invisible to `go test -coverprofile`, and Makefile's covercheck gates ≥80%
// per package against the Go profile alone (build/scheme-coverage.out is
// produced separately and deliberately not merged).
//
// That asymmetry sets the price of moving a test from Go to Scheme: the
// assertion survives, the coverage does not, and covercheck fails the package.
// This driver removes the price by evaluating each .scm through an in-process
// Engine, so the same run both asserts and instruments.
//
// Both drivers are kept. They cover different surfaces — the subprocess one
// covers the CLI, this one covers the embedding path — and neither subsumes the
// other.

// errExitDenied is the authorizer's refusal of (exit). It is a sentinel rather
// than a message so runInProcess can tell "the suite finished and called
// test-exit" apart from "the suite raised".
var errExitDenied = werr.NewStaticError("scheme test driver: (exit) is not available in-process")

// testSummaryMarker is the line (chibi test)'s test-exit prints immediately
// before it calls (exit). Its presence in a file's output is the driver's proof
// that the suite ran to the end; see runSchemeFileInProcess.
const testSummaryMarker = "Test Summary:"

// denyProcessExit allows every operation except process:exit.
//
// Every file in the suite ends with (test-exit), which is (exit 0) or (exit 1),
// and extensions/system's exitWithCode calls os.Exit directly — in-process that
// would terminate the go test binary mid-run. exitWithCode consults the
// authorizer before it reaches os.Exit, so refusing process:exit converts the
// process kill into an ordinary catchable error. This is the existing security
// seam doing exactly its job; no production change is needed.
//
// Refusing only exit (rather than reusing ConsoleAuthorizer, which also confines
// file access to /tmp) keeps the in-process suite's capabilities identical to
// the subprocess suite's, so a test cannot pass under one driver and fail under
// the other for authorization reasons.
func denyProcessExit(req security.AccessRequest) error {
	if req.Resource == security.ResourceProcess && req.Action == security.ActionExit {
		return errExitDenied
	}
	return nil
}

// discoverSchemeTests returns every *-test.scm under the given roots, sorted, as
// paths relative to the repository root. It mirrors the discovery in
// test/run-all.sh so the two drivers always run the same set; a file that only
// one of them sees is a silent coverage hole.
func discoverSchemeTests(t *testing.T, repoRoot string, roots ...string) []string {
	t.Helper()
	var q []string
	for _, root := range roots {
		walkErr := filepath.WalkDir(filepath.Join(repoRoot, root), func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() || !strings.HasSuffix(d.Name(), "-test.scm") {
				return nil
			}
			rel, relErr := filepath.Rel(repoRoot, path)
			if relErr != nil {
				return relErr
			}
			q = append(q, rel)
			return nil
		})
		qt.Assert(t, walkErr, qt.IsNil, qt.Commentf("walking %s", root))
	}
	return q
}

// captureStdout redirects os.Stdout for the duration of fn and returns whatever
// was written.
//
// The io extension's NewState captures os.Stdout by value when the engine is
// built (pkg/extensions/io/state.go), so the swap must bracket engine
// construction, not just evaluation. There is no engine option for this today;
// if one is added, prefer it and delete this.
//
// Process-global, therefore this driver must stay serial. Do not add
// t.Parallel() to the subtests without first giving the io extension a writer
// seam.
func captureStdout(t *testing.T, fn func()) string {
	t.Helper()
	r, w, pipeErr := os.Pipe()
	qt.Assert(t, pipeErr, qt.IsNil)

	saved := os.Stdout
	os.Stdout = w

	done := make(chan string, 1)
	go func() {
		var sb strings.Builder
		_, _ = io.Copy(&sb, r)
		done <- sb.String()
	}()

	defer func() {
		os.Stdout = saved
		_ = w.Close()
	}()

	fn()

	os.Stdout = saved
	_ = w.Close()
	out := <-done
	_ = r.Close()
	return out
}

// runSchemeFileInProcess evaluates one .scm through a fresh Engine and reports
// the chibi-test failure count it left behind.
//
// One engine per file, deliberately. A shared engine would let one file's
// top-level defines and parameter mutations leak into the next, and the
// namespace-init hooks that install per-engine state are only idempotent within
// an engine, not across files (see the io per-engine State work). Reuse is a
// speed optimization to consider only after the suite is green.
func runSchemeFileInProcess(t *testing.T, repoRoot, relPath string) (failures int64, output string) {
	t.Helper()

	source, readErr := os.ReadFile(filepath.Join(repoRoot, relPath))
	qt.Assert(t, readErr, qt.IsNil)

	absPath, absErr := filepath.Abs(filepath.Join(repoRoot, relPath))
	qt.Assert(t, absErr, qt.IsNil)

	var runErr error
	var countVal wile.Value
	output = captureStdout(t, func() {
		ctx := context.Background()
		// Mirrors cmd/wile's engine (main.go), with one deviation: the
		// repository root is passed as an explicit library path instead of
		// relying on the default ".". run-all.sh cd's to the root before
		// invoking the binary; `go test` runs with test/ as the working
		// directory, so the search path has to be made absolute here or every
		// (import (wile ...)) resolves against the wrong directory.
		eng, engErr := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceOS(),
			wile.WithLibraryPaths(repoRoot),
			wile.WithAuthorizer(security.AuthorizerFunc(denyProcessExit)),
		)
		if engErr != nil {
			runErr = engErr
			return
		}

		// EvalProgram wraps the file in one (begin ...) so top-level defines are
		// mutually visible, matching cmd/wile's runFile. ContextWithLoadPath
		// puts the file's directory on the load path so relative include/load
		// resolves. It replaced Engine.WithLoadPath, which this file predates:
		// the stack moved from the Namespace (shared by every SRFI-18 thread)
		// to the context, so two load chains cannot substitute each other's
		// sources. The derived context's reach IS the push's extent, so there
		// is no callback to balance.
		loadCtx, loadErr := eng.ContextWithLoadPath(ctx, absPath)
		if loadErr != nil {
			runErr = loadErr
			return
		}
		_, runErr = eng.EvalProgram(loadCtx, string(source), relPath)

		// test-exit was refused rather than executed, so the counters survive.
		// test-failure-count is a parameter holding a thunk, hence the double
		// application. Read it whether or not the program errored: a file that
		// raises mid-suite still has real failures worth reporting.
		countVal, _ = eng.EvalProgram(ctx, `((test-failure-count))`, relPath+" [failure-count]")
	})

	// Completion is judged by the summary marker, not by runErr.
	//
	// test-exit prints the summary and then calls (exit), which the authorizer
	// refuses — so a fully successful file *always* ends in an error, and runErr
	// alone cannot separate "finished" from "crashed halfway". The marker can:
	// it is printed on the line before the exit call, so its presence proves the
	// suite reached the end and its absence proves it did not.
	//
	// This also decouples the driver from the guard/call-cc unwind defect
	// recorded in the header comment, which corrupts the error identity on the
	// way out of an otherwise-passing file.
	completed := strings.Contains(output, testSummaryMarker)
	if !completed && runErr != nil && !errors.Is(runErr, errExitDenied) {
		t.Errorf("%s: evaluation failed before completing: %v", relPath, runErr)
		return -1, output
	}

	if countVal == nil {
		// The file never imported (chibi test) — a smoke file or a fixture.
		// Not an error; there is simply no count to check.
		return 0, output
	}
	n, ok := countVal.Internal().(*values.Integer)
	if !ok {
		t.Errorf("%s: (test-failure-count) returned %s, want an integer", relPath, countVal.SchemeString())
		return -1, output
	}
	return n.Value, output
}

// TestSchemeSuiteInProcess runs the Scheme test suite through an in-process
// Engine so its coverage is attributed to the Go packages it exercises.
//
// Assertion is on (test-failure-count), not on an exit status: the subprocess
// driver can only see 0 or 1, while the counter reports how many assertions
// actually failed and survives a file that raises partway through.
func TestSchemeSuiteInProcess(t *testing.T) {
	repoRoot, rootErr := filepath.Abs("..")
	qt.Assert(t, rootErr, qt.IsNil)

	files := discoverSchemeTests(t, repoRoot, "test", "pkg/stdlib/lib")
	qt.Assert(t, len(files) > 0, qt.IsTrue, qt.Commentf("no *-test.scm files discovered"))
	t.Logf("discovered %d Scheme test file(s)", len(files))

	for _, rel := range files {
		t.Run(rel, func(t *testing.T) {
			failures, output := runSchemeFileInProcess(t, repoRoot, rel)
			if failures != 0 {
				t.Errorf("%s: %d assertion(s) failed", rel, failures)
			}
			if t.Failed() {
				t.Logf("output:\n%s", output)
			}
		})
	}
}
