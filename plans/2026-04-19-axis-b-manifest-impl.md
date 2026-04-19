# Axis B Manifest Generator — Implementation Plan (Phase 3.A)

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce `plans/axis-b-manifest.scm` — an S-expression list of `(name declared-return-type go-function-name go-source-location)` tuples, one per primitive — plus a Go test that regenerates it on demand and verifies the committed file is current.

**Architecture:** A test at the wile repo root walks `Engine.Registry().Primitives()` under `WithProfile(KitchenSink)`, resolves each primitive's Impl function to a Go function name and source location via `reflect.ValueOf(Impl).Pointer()` + `runtime.FuncForPC`, sorts entries alphabetically by name, and emits them as a Scheme S-expression list. The test compares generated output against the committed file, failing if they differ; setting `AXIS_B_UPDATE=1` rewrites the file.

**Tech Stack:** Go 1.24 standard library only — `reflect`, `runtime`, `path/filepath`, `strings`, `sort`, `os`, `testing`, plus the wile public API (`wile.NewEngine`, `WithProfile`, `Registry().Primitives()`) and `values.TypeConstraint.Name()`.

**Parent design:** `plans/2026-04-19-axis-b-analyzer-design.md` §6.2, §8.A.

**Project conventions observed:**
- Package `wile` (root test) — sibling to `audit_annotations_test.go`.
- `qt` (quicktest) for assertions where helpful, `t.Fatalf`/`t.Errorf` otherwise (matches Phase 1 harness).
- No `if x := ...; cond` compound assignments (ruleguard `noCompoundIf`).
- Multi-line function bodies only (CLAUDE.md imperative).
- No `fmt.Errorf` / `errors.New` in production code — test code is exempt.
- Do not commit without the user's explicit approval (CLAUDE.md).

---

## File Structure

**Create:**
- `audit_manifest_test.go` (repo root) — the test + generator + helpers.
- `plans/axis-b-manifest.scm` — generated; committed after first successful run.

**No modifications** to existing files.

**Responsibility boundary:** The test file holds both the generator logic (building the in-memory manifest) and the assertion logic (comparing against the committed file). Keeping both in the same file is consistent with Phase 1's `audit_annotations_test.go`, which also consolidates a runtime pass + assertions in one file. The manifest file is purely generated data; no human authoring.

---

## Task 1: Scaffold test file with empty manifest and round-trip helpers

**Files:**
- Create: `audit_manifest_test.go`

- [ ] **Step 1: Write the failing test**

Create `/Users/aalpar/projects/wile-workspace/wile/audit_manifest_test.go`:

```go
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
	"os"
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
```

- [ ] **Step 2: Run test to verify it passes (scaffold is green)**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: PASS. (At the scaffold stage the test only verifies the empty-manifest contract so that the next task has a well-defined starting point.)

- [ ] **Step 3: Commit — ask user first**

Per CLAUDE.md (`NEVER commit changes without asking first`), pause and ask the user:

> "Task 1 complete — scaffolded `audit_manifest_test.go` with empty-manifest contract. Test passes. Want me to commit?"

If yes:
```bash
git add audit_manifest_test.go
git commit -m "feat(audit): scaffold axis-b manifest generator test

Creates audit_manifest_test.go with buildManifest and formatManifest
stubs. Test verifies the empty-manifest contract; subsequent tasks
populate the real implementation.

See plans/2026-04-19-axis-b-manifest-impl.md Task 1."
```

---

## Task 2: Populate manifest entries from the registry

**Files:**
- Modify: `audit_manifest_test.go` — replace `buildManifest` body; add `renderType` helper

- [ ] **Step 1: Update the test to expect a populated manifest**

Replace the `TestBuildAxisBManifest` body with:

```go
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
		if prev, dup := seen[e.Name]; dup {
			t.Errorf("duplicate primitive name %q at entries[%d] and entries[%d]",
				e.Name, prev, i)
		}
		seen[e.Name] = i
	}

	for i := 1; i < len(entries); i++ {
		if entries[i-1].Name > entries[i].Name {
			t.Errorf("entries not sorted: %q > %q at positions %d, %d",
				entries[i-1].Name, entries[i].Name, i-1, i)
			break
		}
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: FAIL with `expected at least 400 primitives, got 0`. The failure confirms the test now exercises the real contract; Step 3 makes it pass.

- [ ] **Step 3: Implement buildManifest and renderType**

Add to the imports:

```go
import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"
	"testing"

	"github.com/aalpar/wile/values"
)
```

Replace `buildManifest` with:

```go
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
		entries = append(entries, manifestEntry{
			Name:       pr.Spec.Name,
			ReturnType: renderManifestType(pr.Spec.ReturnType),
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
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: PASS. The log line should include the primitive count (475 at time of writing).

- [ ] **Step 5: Commit — ask user first**

> "Task 2 complete — manifest populated from registry, 475+ primitives enumerated, sorted, no duplicates. Go function name and source location still stubbed. Want me to commit?"

```bash
git add audit_manifest_test.go
git commit -m "feat(audit): populate axis-b manifest from registry

buildManifest now walks eng.Registry().Primitives() under KitchenSink
and emits one entry per primitive with Name and ReturnType fields.
Sorted by name; duplicate detection in test.

GoFunction and SourceFile fields remain empty pending the next task.

See plans/2026-04-19-axis-b-manifest-impl.md Task 2."
```

---

## Task 3: Resolve Go function names via reflect + runtime.FuncForPC

**Files:**
- Modify: `audit_manifest_test.go` — add `resolveImpl` helper; call it from `buildManifest`

- [ ] **Step 1: Extend the test to verify Go function names are populated**

Add this assertion to `TestBuildAxisBManifest`, immediately before the sort check:

```go
for i, e := range entries {
	if e.GoFunction == "" {
		t.Errorf("entry %d (%q) has empty GoFunction", i, e.Name)
	}
	if !strings.Contains(e.GoFunction, "/") {
		t.Errorf("entry %d (%q) GoFunction %q lacks package path",
			i, e.Name, e.GoFunction)
	}
}
```

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: FAIL with 475 `empty GoFunction` errors (one per primitive).

- [ ] **Step 3: Implement resolveImpl and wire it into buildManifest**

Add below `renderManifestType`:

```go
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
```

Update `buildManifest`'s loop body to call it:

```go
for _, pr := range prims {
	goName, _, _ := resolveImpl(pr.Spec.Impl)
	entries = append(entries, manifestEntry{
		Name:       pr.Spec.Name,
		ReturnType: renderManifestType(pr.Spec.ReturnType),
		GoFunction: goName,
	})
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: PASS. Every entry has a non-empty GoFunction containing a `/` (the module path `github.com/aalpar/wile/...`).

- [ ] **Step 5: Commit — ask user first**

> "Task 3 complete — Go function names resolved via reflect. Source location still stubbed. Want me to commit?"

```bash
git add audit_manifest_test.go
git commit -m "feat(audit): resolve Go function names for manifest entries

resolveImpl uses reflect.ValueOf(fn).Pointer() + runtime.FuncForPC to
recover the fully-qualified Go function name for each primitive Impl.
Test asserts every entry has a non-empty, package-qualified name.

See plans/2026-04-19-axis-b-manifest-impl.md Task 3."
```

---

## Task 4: Resolve source file:line and strip repo-root prefix

**Files:**
- Modify: `audit_manifest_test.go` — use `resolveImpl`'s file/line returns; call `stripRoot`

**Note on binding-only primitives** (discovered during Task 3 implementation):
47 primitives (`assoc`, `member`, `map`, `for-each`, `ca...r` variants, `boolean=?`, etc.)
are registered with `Impl == nil` — they're binding-only entries for symbol resolution,
with no Go body to analyze. Task 3's test skips GoFunction assertions for entries with
`GoFunction == ""`. Task 4 inherits the same skip: an entry without a GoFunction also has
no SourceFile, and the axis-b analyzer skips both together.

- [ ] **Step 1: Extend the test to verify source locations are populated and relative**

Fold the assertions INTO the existing per-entry loop (right after the GoFunction block,
still inside the `continue`-on-empty guard). Do NOT add a second independent loop:

```go
// After the existing GoFunction package-path check, still inside the loop body:
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
```

Because the `if e.GoFunction == "" { continue }` guard from Task 3 sits above these
assertions, binding-only primitives (nil Impl) skip the entire source-file block —
which is correct, since they have no Go source location to record.

- [ ] **Step 2: Run test to verify it fails**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: FAIL with 475 `empty SourceFile` errors.

- [ ] **Step 3: Wire source location into buildManifest**

Replace the loop body in `buildManifest`:

```go
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
```

- [ ] **Step 4: Run test to verify it passes**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: PASS. Every entry has a repo-relative `.go` path with a positive line number.

- [ ] **Step 5: Sanity-check a known primitive**

Run an ad-hoc probe:
```bash
go test -v -run TestBuildAxisBManifest . -count=1
```

Manually inspect the logged entry for a well-known primitive like `car` (which lives in `registry/core/` — exact file varies but should match the tree). If paths look wrong, investigate before proceeding.

Add a diagnostic log near the end of `TestBuildAxisBManifest` (before closing brace):

```go
t.Logf("manifest: %d entries", len(entries))
for _, name := range []string{"car", "cdr", "cons", "+"} {
	if i, ok := seen[name]; ok {
		e := entries[i]
		t.Logf("  %-10s return=%-12s fn=%s loc=%s:%d",
			e.Name, e.ReturnType, e.GoFunction, e.SourceFile, e.SourceLine)
	}
}
```

Re-run:
```bash
go test -v -run TestBuildAxisBManifest . -count=1
```

Expected: the log lines show `github.com/aalpar/wile/registry/core.<something>` function names and `registry/core/*.go:<n>` source locations.

- [ ] **Step 6: Commit — ask user first**

> "Task 4 complete — source file:line resolved, repo-relative paths verified for core primitives. Want me to commit?"

```bash
git add audit_manifest_test.go
git commit -m "feat(audit): populate source file:line in manifest entries

resolveImpl's file/line returns are wired into buildManifest; paths
are made repo-relative via stripRoot. Test asserts every entry has a
repo-relative .go path and positive line number. Diagnostic log
prints the core primitives for spot-checking.

See plans/2026-04-19-axis-b-manifest-impl.md Task 4."
```

---

## Task 5: Format entries as an S-expression list

**Files:**
- Modify: `audit_manifest_test.go` — implement `formatManifest` properly; add format test

- [ ] **Step 1: Write a formatter test**

Add this test function after `TestBuildAxisBManifest`:

```go
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
```

- [ ] **Step 2: Run formatter test to verify it fails**

Run: `go test -v -run TestFormatManifest .`

Expected: three subtests FAIL (empty passes because stub returns `"()\n"`).

- [ ] **Step 3: Implement formatManifest**

Replace the stub `formatManifest` with:

```go
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
```

Add `"strconv"` to the imports.

- [ ] **Step 4: Run tests to verify they pass**

Run: `go test -v -run 'TestFormatManifest|TestBuildAxisBManifest' .`

Expected: PASS for all four subtests of TestFormatManifest and for TestBuildAxisBManifest.

- [ ] **Step 5: Commit — ask user first**

> "Task 5 complete — formatManifest emits valid Scheme with escaped strings, four unit tests cover empty/single/multi/escaping. Want me to commit?"

```bash
git add audit_manifest_test.go
git commit -m "feat(audit): implement manifest S-expression formatter

formatManifest emits (name return-type go-function source:line) tuples
as a Scheme list. writeSchemeString escapes double quotes and backslashes.
Table-driven TestFormatManifest covers empty/single/multi/escaping cases.

See plans/2026-04-19-axis-b-manifest-impl.md Task 5."
```

---

## Task 6: Round-trip the manifest against `plans/axis-b-manifest.scm`

**Files:**
- Modify: `audit_manifest_test.go` — extend test to compare against committed file; support `AXIS_B_UPDATE=1`
- Create: `plans/axis-b-manifest.scm` (via update flow)

- [ ] **Step 1: Extend the test to round-trip against the committed file**

Replace `TestBuildAxisBManifest` entirely with:

```go
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

		// Binding-only primitives (nil Impl — assoc, member, map, caar,
		// boolean=?, etc.) are kept in the manifest but have no Go body
		// to analyze. Skip the source-resolution assertions for them.
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
			t.Errorf("entry %d (%q) SourceFile %q is absolute", i, e.Name, e.SourceFile)
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
```

- [ ] **Step 2: Run test to verify it fails (manifest file doesn't exist yet)**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: FAIL with `read plans/axis-b-manifest.scm: open ...: no such file or directory (run with AXIS_B_UPDATE=1 to generate)`.

- [ ] **Step 3: Generate the manifest file**

Run: `AXIS_B_UPDATE=1 go test -v -run TestBuildAxisBManifest .`

Expected: PASS with log line `updated plans/axis-b-manifest.scm (475 entries)` (the exact count is whatever the current registry holds).

- [ ] **Step 4: Verify the file was written and has plausible content**

```bash
ls -l plans/axis-b-manifest.scm
head -5 plans/axis-b-manifest.scm
wc -l plans/axis-b-manifest.scm
```

Expected:
- File exists, size > 30KB.
- First line begins with `(("` (the open paren of the list, then the open paren of the first tuple, then the open quote).
- Line count equals entry count (one tuple per line after the first).

- [ ] **Step 5: Re-run the test without `AXIS_B_UPDATE` to confirm round-trip passes**

Run: `go test -v -run TestBuildAxisBManifest .`

Expected: PASS. No log about updating.

- [ ] **Step 6: Commit — ask user first**

> "Task 6 complete — manifest round-trips. First generated `plans/axis-b-manifest.scm` is 475 entries, ~ N lines, X KB. Want me to commit both the test change and the generated manifest?"

```bash
git add audit_manifest_test.go plans/axis-b-manifest.scm
git commit -m "feat(audit): commit axis-b manifest + round-trip test

plans/axis-b-manifest.scm is the generated S-expression list consumed
by the wile-goast axis-b analyzer (Phase 3.B, separate plan).

TestBuildAxisBManifest compares the runtime-generated manifest against
the committed file; AXIS_B_UPDATE=1 rewrites the file when primitives
are added, removed, or renamed.

See plans/2026-04-19-axis-b-analyzer-design.md for the overall audit design.
See plans/2026-04-19-axis-b-manifest-impl.md Task 6."
```

---

## Task 7: Verify manifest quality and edge cases

**Files:**
- Modify: `audit_manifest_test.go` — add targeted sanity tests

- [ ] **Step 1: Add sanity-check test for known primitives**

Add the following test after `TestFormatManifest`:

```go
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
```

- [ ] **Step 2: Run new tests to verify they pass**

Run: `go test -v -run 'TestManifestSanity|TestManifestStability' .`

Expected: PASS for all sanity subtests and stability.

If any sanity subtest fails, investigate the resolveImpl path: the primitive's Impl is probably being wrapped by a closure during registration, so `runtime.FuncForPC` reports the wrapper rather than the real body. If confirmed, document the discovered wrapping site in a comment in `resolveImpl` and decide whether to (a) unwrap statically or (b) flag the primitive as `wrapped` in the manifest and let the analyzer handle it. Do not silently change the sanity check to match the incorrect name.

- [ ] **Step 3: Commit — ask user first**

> "Task 7 complete — sanity checks for core primitives pass; manifest is deterministic across runs. Want me to commit?"

```bash
git add audit_manifest_test.go
git commit -m "test(audit): add sanity + stability checks for axis-b manifest

TestManifestSanity spot-checks that car/cdr/cons/+ resolve to
functions in registry/core, catching any registration wrapping that
would obscure real Impl locations.

TestManifestStability asserts buildManifest is deterministic so that
AXIS_B_UPDATE produces stable diffs.

See plans/2026-04-19-axis-b-manifest-impl.md Task 7."
```

---

## Task 8: Finalize — lint and coverage

**Files:**
- No code changes expected. This task runs the project's standard build-clean gates.

- [ ] **Step 1: Run make lint**

Run: `make lint`

Expected: PASS. If it fails, read the message; typical fixes are:
- Missing goimports grouping → re-run `goimports -w audit_manifest_test.go`
- `noCompoundIf` → split any `if x := ...; cond { ... }` into two lines
- Unused import → remove it

Repeat until clean.

- [ ] **Step 2: Run make covercheck**

Run: `make covercheck`

Expected: PASS. The audit test file contributes test-only code and should not move coverage significantly; if covercheck complains, the root cause is elsewhere in the tree, not in this task.

- [ ] **Step 3: Final full-suite run**

Run: `go test ./... -count=1`

Expected: PASS overall. Manifest tests should take <3s.

- [ ] **Step 4: Commit — ask user first**

If any lint/formatting changes were made in Steps 1-2:

> "Task 8 complete — lint and covercheck pass. Want me to commit the lint fixup?"

```bash
git add audit_manifest_test.go
git commit -m "style(audit): satisfy make lint for axis-b manifest test"
```

If no changes were made in those steps, there's nothing to commit for Task 8.

---

## Task 9: PR / handoff

**Files:**
- No code changes. This is the project handoff step.

- [ ] **Step 1: Review what's on the branch**

```bash
git log --oneline origin/master..HEAD
git diff origin/master..HEAD --stat
```

Expected: Commits from Tasks 1-7 (and 8 if lint fixes were needed). Files touched: `audit_manifest_test.go` and `plans/axis-b-manifest.scm`.

- [ ] **Step 2: Ask the user about PR creation**

> "Phase 3.A complete:
>   - `audit_manifest_test.go` with TestBuildAxisBManifest, TestFormatManifest, TestManifestSanity, TestManifestStability (N lines)
>   - `plans/axis-b-manifest.scm` with N entries
>   - lint and covercheck pass
>
> Open a PR against master, or continue to Phase 3.B (wile-goast analyzer) design?"

Do not open a PR without explicit approval (CLAUDE.md).

---

## Self-review checklist (for the plan author)

- [x] Every step has exact file paths.
- [x] Every code step shows actual code (no "implement the function" without the function body).
- [x] Every test step says how to run it and what to expect.
- [x] No compound-if statements in generated code (ruleguard compliant).
- [x] No single-line function bodies (CLAUDE.md imperative).
- [x] Commits are asked-for, not auto-taken (CLAUDE.local.md imperative).
- [x] `make lint` and `make covercheck` are run at the end.
- [x] Types and names are consistent across tasks (`manifestEntry`, `buildManifest`, `formatManifest`, `resolveImpl`, `stripRoot`, `repoRoot`, `renderManifestType`, `writeSchemeString`, `axisBManifestPath`).
- [x] Every spec requirement (§6.2, §8.A of the design doc) maps to a task:
  - Walk `Registry.Primitives()` under `AllExtensions()` / `KitchenSink` → Task 2
  - Record `(name, declared-ReturnType, go-function-name, go-source-location)` → Tasks 2, 3, 4
  - Resolve via `runtime.FuncForPC(reflect.ValueOf(spec.Impl).Pointer())` + `Func.FileLine(pc)` → Tasks 3, 4
  - Output `plans/axis-b-manifest.scm` as S-expression list → Tasks 5, 6
  - Runs under `make test` (cheap, no SSA) → Task 6 (the round-trip is the test)
  - Manifest committed, diffs act as review signal for primitive churn → Task 6 commit
