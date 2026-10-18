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
	"strings"
	"testing"
	"time"
)

// TestSchelogSuite runs examples/logic/schelog/run-all-tests.scm from the repo
// root, which is how its README tells a user to run it (the suite's include
// paths are root-relative).
//
// The suite includes schelog.scm, a third-party library with top-level set!
// evidence that spans the whole file. Include splices at expand time, so the
// file is one validation unit; before that the unit boundary hid the evidence
// and the include died with "set!: cannot mutate immutable top-level binding".
//
// Requirements: the scheme binary must be built (make build).
func TestSchelogSuite(t *testing.T) {
	schemeBin := getSchemeBinary()
	_, err := os.Stat(schemeBin)
	if os.IsNotExist(err) {
		t.Fatalf("scheme binary not found at %s - run 'make build' first", schemeBin)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	cmd := exec.CommandContext(ctx, schemeBin, "--file", "examples/logic/schelog/run-all-tests.scm")
	cmd.Dir = getProjectRoot()
	cmd.Env = append(os.Environ(), "SCHEME_LIBRARY_PATH="+getLibPath())

	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err = cmd.Run()
	if ctx.Err() == context.DeadlineExceeded {
		t.Fatalf("schelog suite timed out\n\nOutput:\n%s\n\nStderr:\n%s", stdout.String(), stderr.String())
	}
	if err != nil {
		t.Fatalf("schelog suite failed: %v\n\nOutput:\n%s\n\nStderr:\n%s", err, stdout.String(), stderr.String())
	}
	if !strings.Contains(stdout.String(), "Failed: 0") {
		t.Fatalf("schelog suite reported failures\n\nOutput:\n%s", stdout.String())
	}
	// A pass count guards against a block that silently stops running.
	if !strings.Contains(stdout.String(), "Passed: 22\n") {
		t.Fatalf("schelog suite did not report 22 passes\n\nOutput:\n%s", stdout.String())
	}
}
