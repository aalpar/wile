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

package wile_test

import (
	"context"
	"debug/elf"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestGoInstall verifies that `go install github.com/aalpar/wile/cmd/wile`
// succeeds using local module resolution, producing a working binary in GOBIN.
// This exercises the same code path as `go install ... @latest` but resolves
// against local source rather than the module proxy.
func TestGoInstall(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping go install test in short mode")
	}

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()

	gobin := t.TempDir()

	cmd := exec.CommandContext(ctx, "go", "install", "github.com/aalpar/wile/cmd/wile")
	cmd.Env = append(os.Environ(), "GOBIN="+gobin)

	output, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("go install failed:\n%s", output)
	}

	binary := filepath.Join(gobin, "wile")
	_, err = os.Stat(binary)
	if err != nil {
		t.Fatalf("installed binary not found at %s: %v", binary, err)
	}

	// Smoke test: evaluate a Scheme expression
	evalCmd := exec.CommandContext(ctx, binary, "-e", "(+ 1 2)")
	evalOutput, err := evalCmd.Output()
	if err != nil {
		t.Fatalf("wile -e '(+ 1 2)' failed: %v", err)
	}

	got := strings.TrimSpace(string(evalOutput))
	if got != "3" {
		t.Errorf("wile -e '(+ 1 2)' = %q, want %q", got, "3")
	}
}

// TestCrossBuildLinux verifies that `go build ./cmd/wile` succeeds for
// linux/amd64 and linux/arm64 with CGO_ENABLED=0. This exercises the same
// code path as `go install github.com/aalpar/wile/cmd/wile@latest` and
// validates the pure-Go (no CGo) claim from the README.
//
// Each resulting binary is verified to be a valid 64-bit ELF executable
// targeting the correct architecture.
func TestCrossBuildLinux(t *testing.T) {
	if testing.Short() {
		t.Skip("skipping cross-compilation test in short mode")
	}

	targets := []struct {
		goarch  string
		machine elf.Machine
	}{
		{"amd64", elf.EM_X86_64},
		{"arm64", elf.EM_AARCH64},
	}

	for _, target := range targets {
		t.Run("linux/"+target.goarch, func(t *testing.T) {
			t.Parallel()

			ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
			defer cancel()

			outPath := filepath.Join(t.TempDir(), "wile")

			// Use the full import path (not "./cmd/wile") so the build
			// resolves regardless of this test's working directory — the
			// package now lives at pkg/wile/, not the module root.
			cmd := exec.CommandContext(ctx, "go", "build", "-o", outPath, "github.com/aalpar/wile/cmd/wile")
			cmd.Env = append(os.Environ(),
				"GOOS=linux",
				"GOARCH="+target.goarch,
				"CGO_ENABLED=0",
			)

			output, err := cmd.CombinedOutput()
			if err != nil {
				t.Fatalf("go build failed for linux/%s:\n%s", target.goarch, output)
			}

			f, err := elf.Open(outPath)
			if err != nil {
				t.Fatalf("output is not a valid ELF binary: %v", err)
			}
			defer f.Close()

			if f.Machine != target.machine {
				t.Errorf("ELF machine = %v, want %v", f.Machine, target.machine)
			}
			if f.Class != elf.ELFCLASS64 {
				t.Errorf("ELF class = %v, want %v", f.Class, elf.ELFCLASS64)
			}
		})
	}
}
