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

	qt "github.com/frankban/quicktest"
)

// TestCLI_CoverFlag_WritesGoCoverFormat compiles the binary, runs a
// tiny Scheme file under --cover, and asserts the output has the
// Go-cover-v1 header and at least one entry for the run file.
func TestCLI_CoverFlag_WritesGoCoverFormat(t *testing.T) {
	c := qt.New(t)
	dir := c.TempDir()

	schemePath := filepath.Join(dir, "prog.scm")
	err := os.WriteFile(schemePath, []byte("(+ 1 2)\n"), 0o644)
	c.Assert(err, qt.IsNil)

	covPath := filepath.Join(dir, "cov.out")

	ctx := context.Background()

	binPath := filepath.Join(dir, "wile")
	buildCmd := exec.CommandContext(ctx, "go", "build", "-o", binPath, ".")
	// Match GOWORK=off so the test works within the worktree.
	buildCmd.Env = append(os.Environ(), "GOWORK=off")
	buildOut, err := buildCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("build output: %s", buildOut))

	runCmd := exec.CommandContext(ctx, binPath, "--cover", covPath, "--file", schemePath)
	runOut, err := runCmd.CombinedOutput()
	c.Assert(err, qt.IsNil, qt.Commentf("run output: %s", runOut))

	data, err := os.ReadFile(covPath)
	c.Assert(err, qt.IsNil)
	content := string(data)

	c.Assert(strings.HasPrefix(content, "mode: set\n"), qt.IsTrue, qt.Commentf("got: %s", content))
	c.Assert(strings.Contains(content, schemePath), qt.IsTrue, qt.Commentf("got: %s", content))
	c.Assert(strings.Contains(content, " 1 1"), qt.IsTrue, qt.Commentf("executed sexpr should have Count=1; got: %s", content))
}
