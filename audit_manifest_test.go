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

// Manifest generator for axis-b analyzer (Phase 3.A).
//
// See plans/2026-04-19-axis-b-analyzer-design.md §6.2, §8.A.
//
// Writes plans/axis-b-manifest.scm — an S-expression list of
// (name declared-return-type go-function-name go-source-location) tuples.
// Run with AXIS_B_UPDATE=1 to regenerate after adding/removing primitives.

package wile

import (
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// manifestEntry is a single primitive's line in the manifest.
type manifestEntry struct {
	Name       string
	ReturnType string
	GoFunction string
	SourceFile string
	SourceLine int
}

// buildManifest enumerates every primitive and returns one entry per primitive,
// sorted by primitive name.
func buildManifest(t *testing.T) []manifestEntry {
	t.Helper()
	return nil
}

// formatManifest renders entries as a Scheme S-expression list.
func formatManifest(entries []manifestEntry) string {
	return "()\n"
}

func TestBuildAxisBManifest(t *testing.T) {
	entries := buildManifest(t)
	if len(entries) != 0 {
		t.Fatalf("expected empty manifest from scaffold, got %d entries", len(entries))
	}
	out := formatManifest(entries)
	if out != "()\n" {
		t.Fatalf("expected scaffold output %q, got %q", "()\n", out)
	}
}

// repoRoot returns the absolute path of the wile repo root, inferred from
// this test file's location.
func repoRoot() string {
	_, thisFile, _, _ := runtime.Caller(0)
	return filepath.Dir(thisFile)
}

// stripRoot strips the repo root prefix from an absolute path, yielding a
// repo-relative path such as "registry/core/lists.go".
func stripRoot(abs string) string {
	root := repoRoot()
	trimmed := strings.TrimPrefix(abs, root)
	return strings.TrimPrefix(trimmed, string(filepath.Separator))
}
