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
	"context"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strconv"
	"strings"
	"testing"

	"github.com/aalpar/wile/values"
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
	ctx := context.Background()
	eng, err := NewEngine(ctx, WithProfile(KitchenSink), WithLibraryPaths())
	if err != nil {
		t.Fatalf("new engine: %v", err)
	}
	prims := eng.Registry().Primitives()

	entries := make([]manifestEntry, 0, len(prims))
	for _, pr := range prims {
		goName, absFile, line := resolveImpl(pr.Spec.Impl)
		entries = append(entries, manifestEntry{
			Name:       pr.Spec.Name,
			ReturnType: renderManifestType(pr.Spec.ReturnType),
			GoFunction: goName,
			SourceFile: stripRoot(absFile),
			SourceLine: line,
		})
	}
	sort.Slice(entries, func(i, j int) bool {
		return entries[i].Name < entries[j].Name
	})
	return entries
}

// renderManifestType mirrors renderType in audit_annotations_test.go, but we
// name it distinctly to avoid colliding if both files are edited in the same
// session. "" (not "<nil>") is emitted for unspecified return types so the
// S-expression reader sees an empty string rather than a literal name.
func renderManifestType(t values.TypeConstraint) string {
	if t == nil {
		return ""
	}
	return t.Name()
}

// resolveImpl returns the fully-qualified Go function name and absolute
// source file:line for a primitive's Impl function. If reflection cannot
// recover either piece (e.g., the Impl is a closure with no source info),
// the returned strings are empty and line is 0.
func resolveImpl(fn interface{}) (name, file string, line int) {
	if fn == nil {
		return "", "", 0
	}
	v := reflect.ValueOf(fn)
	if v.Kind() != reflect.Func {
		return "", "", 0
	}
	pc := v.Pointer()
	rf := runtime.FuncForPC(pc)
	if rf == nil {
		return "", "", 0
	}
	file, line = rf.FileLine(pc)
	return rf.Name(), file, line
}

// formatManifest renders entries as a Scheme S-expression list, one tuple
// per line. Each tuple is (name return-type go-function source-location)
// with all fields quoted as Scheme strings. Double quotes and backslashes
// in any field are escaped.
func formatManifest(entries []manifestEntry) string {
	if len(entries) == 0 {
		return "()\n"
	}
	var b strings.Builder
	b.WriteByte('(')
	for i, e := range entries {
		if i > 0 {
			b.WriteString("\n ")
		}
		b.WriteByte('(')
		writeSchemeString(&b, e.Name)
		b.WriteByte(' ')
		writeSchemeString(&b, e.ReturnType)
		b.WriteByte(' ')
		writeSchemeString(&b, e.GoFunction)
		b.WriteByte(' ')
		loc := e.SourceFile
		if e.SourceLine > 0 {
			loc = e.SourceFile + ":" + strconv.Itoa(e.SourceLine)
		}
		writeSchemeString(&b, loc)
		b.WriteByte(')')
	}
	b.WriteString(")\n")
	return b.String()
}

// writeSchemeString writes s as a Scheme string literal into b, escaping
// embedded double quotes and backslashes.
func writeSchemeString(b *strings.Builder, s string) {
	b.WriteByte('"')
	for _, r := range s {
		if r == '"' || r == '\\' {
			b.WriteByte('\\')
		}
		b.WriteRune(r)
	}
	b.WriteByte('"')
}

const axisBManifestPath = "plans/axis-b-manifest.scm"

func TestBuildAxisBManifest(t *testing.T) {
	entries := buildManifest(t)
	if len(entries) < 400 {
		t.Fatalf("expected at least 400 primitives, got %d", len(entries))
	}

	seen := make(map[string]int, len(entries))
	for i, e := range entries {
		if e.Name == "" {
			t.Errorf("entry %d has empty Name", i)
		}
		prev, dup := seen[e.Name]
		if dup {
			t.Errorf("duplicate primitive name %q at entries[%d] and entries[%d]",
				e.Name, prev, i)
		}
		seen[e.Name] = i

		// Binding-only primitives (nil Impl — e.g., assoc, member, map,
		// caar, boolean=?) are registered for symbol resolution but have
		// no Go body. They stay in the manifest with empty GoFunction;
		// the wile-goast analyzer skips them. Only non-empty GoFunction
		// values must be package-qualified.
		if e.GoFunction == "" {
			continue
		}
		if !strings.Contains(e.GoFunction, "/") {
			t.Errorf("entry %d (%q) GoFunction %q lacks package path",
				i, e.Name, e.GoFunction)
		}
		if e.SourceFile == "" {
			t.Errorf("entry %d (%q) has populated GoFunction but empty SourceFile",
				i, e.Name)
		}
		if filepath.IsAbs(e.SourceFile) {
			t.Errorf("entry %d (%q) SourceFile %q is absolute (should be repo-relative)",
				i, e.Name, e.SourceFile)
		}
		if e.SourceLine <= 0 {
			t.Errorf("entry %d (%q) SourceLine %d is not positive",
				i, e.Name, e.SourceLine)
		}
		if !strings.HasSuffix(e.SourceFile, ".go") {
			t.Errorf("entry %d (%q) SourceFile %q is not a .go file",
				i, e.Name, e.SourceFile)
		}
	}

	for i := 1; i < len(entries); i++ {
		if entries[i-1].Name > entries[i].Name {
			t.Errorf("entries not sorted: %q > %q at positions %d, %d",
				entries[i-1].Name, entries[i].Name, i-1, i)
			break
		}
	}

	t.Logf("manifest: %d entries", len(entries))
	for _, name := range []string{"car", "cdr", "cons", "+"} {
		idx, ok := seen[name]
		if !ok {
			continue
		}
		e := entries[idx]
		t.Logf("  %-10s return=%-12s fn=%s loc=%s:%d",
			e.Name, e.ReturnType, e.GoFunction, e.SourceFile, e.SourceLine)
	}

	generated := formatManifest(entries)
	path := filepath.Join(repoRoot(), axisBManifestPath)

	if os.Getenv("AXIS_B_UPDATE") != "" {
		err := os.WriteFile(path, []byte(generated), 0644)
		if err != nil {
			t.Fatalf("write manifest: %v", err)
		}
		t.Logf("updated %s (%d entries)", axisBManifestPath, len(entries))
		return
	}

	committed, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v (run with AXIS_B_UPDATE=1 to generate)",
			axisBManifestPath, err)
	}
	if string(committed) != generated {
		t.Errorf("%s is out of date\nrun: AXIS_B_UPDATE=1 go test -run TestBuildAxisBManifest .",
			axisBManifestPath)
	}
}

func TestFormatManifest(t *testing.T) {
	tcs := []struct {
		name     string
		input    []manifestEntry
		expected string
	}{
		{
			name:     "empty",
			input:    nil,
			expected: "()\n",
		},
		{
			name: "single entry",
			input: []manifestEntry{
				{
					Name:       "car",
					ReturnType: "any",
					GoFunction: "github.com/aalpar/wile/registry/core.primCar",
					SourceFile: "registry/core/lists.go",
					SourceLine: 42,
				},
			},
			expected: "(" +
				`("car" "any" "github.com/aalpar/wile/registry/core.primCar" "registry/core/lists.go:42")` +
				")\n",
		},
		{
			name: "multiple entries",
			input: []manifestEntry{
				{Name: "a", ReturnType: "x", GoFunction: "pkg.A", SourceFile: "a.go", SourceLine: 1},
				{Name: "b", ReturnType: "", GoFunction: "pkg.B", SourceFile: "b.go", SourceLine: 2},
			},
			expected: `(("a" "x" "pkg.A" "a.go:1")` + "\n" +
				` ("b" "" "pkg.B" "b.go:2"))` + "\n",
		},
		{
			name: "escapes double-quote and backslash in names",
			input: []manifestEntry{
				{Name: `weird"name\here`, ReturnType: "x", GoFunction: "pkg.F", SourceFile: "f.go", SourceLine: 1},
			},
			expected: `(("weird\"name\\here" "x" "pkg.F" "f.go:1"))` + "\n",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := formatManifest(tc.input)
			if got != tc.expected {
				t.Errorf("formatManifest mismatch\nwant: %q\ngot:  %q", tc.expected, got)
			}
		})
	}
}

// TestManifestSanity spot-checks well-known primitives to catch
// regressions where a primitive's Impl resolves to the wrong function
// (e.g., a helper or closure). If this test starts failing, the
// underlying cause is almost certainly that a primitive was re-registered
// through a closure or wrapper layer.
func TestManifestSanity(t *testing.T) {
	entries := buildManifest(t)
	byName := make(map[string]manifestEntry, len(entries))
	for _, e := range entries {
		byName[e.Name] = e
	}

	tcs := []struct {
		name           string
		pkgSubstr      string // must appear in GoFunction
		sourceContains string // must appear in SourceFile
	}{
		{name: "car", pkgSubstr: "wile/registry/core", sourceContains: "registry/core/"},
		{name: "cdr", pkgSubstr: "wile/registry/core", sourceContains: "registry/core/"},
		{name: "cons", pkgSubstr: "wile/registry/core", sourceContains: "registry/core/"},
		{name: "+", pkgSubstr: "wile/registry/core", sourceContains: "registry/core/"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			e, ok := byName[tc.name]
			if !ok {
				t.Fatalf("primitive %q not in manifest", tc.name)
			}
			if !strings.Contains(e.GoFunction, tc.pkgSubstr) {
				t.Errorf("%s GoFunction %q does not contain %q",
					tc.name, e.GoFunction, tc.pkgSubstr)
			}
			if !strings.Contains(e.SourceFile, tc.sourceContains) {
				t.Errorf("%s SourceFile %q does not contain %q",
					tc.name, e.SourceFile, tc.sourceContains)
			}
		})
	}
}

// TestManifestStability asserts buildManifest is deterministic across
// repeated invocations in the same process. Non-determinism here would
// cause AXIS_B_UPDATE runs to produce unstable diffs.
func TestManifestStability(t *testing.T) {
	first := buildManifest(t)
	second := buildManifest(t)
	if len(first) != len(second) {
		t.Fatalf("count differs across runs: %d vs %d", len(first), len(second))
	}
	for i := range first {
		if first[i] != second[i] {
			t.Errorf("entry %d differs: %+v vs %+v", i, first[i], second[i])
			break
		}
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
