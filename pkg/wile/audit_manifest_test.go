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

// Manifest generator for axis-b analyzer (Phase 3.A + Phase 5.A).
//
// See memory/2026-04-19-axis-b-analyzer-design.md §6.2, §8.A for the
// return-type pass, and memory/2026-04-20-paramtypes-audit-design.md
// §3.1 for the ParamTypes extension.
//
// Writes testdata/axis-b-manifest.scm — an S-expression list of
// (name return-type param-types go-function go-source-location) tuples.
// Run with WILE_AXIS_B_UPDATE=1 to regenerate after adding/removing primitives.
//
// ParamTypes slot format:
//   - One string per fixed parameter slot, containing the TypeConstraint.Name().
//   - For variadic primitives, the last slot is prefixed "..." to mark it as
//     the per-element type of the rest list (convention per registry.go:34 and
//     memory/2026-04-20-paramtypes-audit-design.md §7.1).
//   - nil TypeConstraint emitted as "" (unspecified slot, analogous to the
//     return-type convention).
//   - Primitives with no ParamTypes (len==0) emit the empty list ().

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

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/werr"
)

var errResolveImpl = werr.NewStaticError("audit manifest: resolveImpl")

const (
	// axisBManifestPath is the repo-relative path of the committed manifest.
	// It lives under testdata/ — the Go-conventional home for golden
	// fixtures — because this test both generates it (WILE_AXIS_B_UPDATE=1)
	// and golden-asserts the committed copy. The audit/ analyzer scripts
	// consume it as their default input dataset.
	axisBManifestPath = "testdata/axis-b-manifest.scm"

	// axisBUpdateEnvVar toggles regeneration of the committed manifest.
	// Set to any non-empty value to overwrite axisBManifestPath with the
	// freshly-computed contents instead of comparing for equality.
	axisBUpdateEnvVar = "WILE_AXIS_B_UPDATE"

	// maxBindingOnlyPrimitives bounds the expected number of primitives
	// with nil Impl. The current live count is 0: procedures that were
	// once binding-only registry shells (CxR accessors, assoc, map,
	// for-each, boolean=?, etc.) are now pure Scheme defines in
	// bootstrap_procedures.scm with no PrimitiveSpec. The cap is kept as
	// a guard — a non-zero count would mean real Impls were accidentally
	// nulled, and the test fails rather than silently skipping validation
	// for all of them.
	maxBindingOnlyPrimitives = 60

	// maxReportedDiffs caps how many differences are reported from
	// order- and stability-check loops before the loop breaks. Keeps
	// failure output readable while still exposing systemic drift.
	maxReportedDiffs = 5
)

// manifestEntry is a single primitive's line in the manifest.
//
// ParamTypes is the pre-formatted per-slot list: each entry is a
// TypeConstraint.Name() string (or "" for a nil slot). For variadic
// primitives, the last element is prefixed "..." to distinguish the
// rest-element annotation from a fixed slot.
type manifestEntry struct {
	Name       string
	ReturnType string
	ParamTypes []string
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
		t.Fatalf("new engine (profile=KitchenSink, no library paths): %v", err)
	}
	prims := eng.Registry().Primitives()
	root := repoRoot(t)

	q := make([]manifestEntry, 0, len(prims))
	for _, pr := range prims {
		returnType := ""
		if pr.Spec.ReturnType != nil {
			returnType = pr.Spec.ReturnType.Name()
		}
		goName, absFile, line, impErr := resolveImpl(pr.Spec.Impl)
		if impErr != nil {
			// resolveImpl already signals the failure via the returned
			// error; recording it as an entry would conflate it with
			// binding-only primitives (empty GoFunction), potentially
			// tripping maxBindingOnlyPrimitives with a misleading
			// secondary error. Skip the append — the test has already
			// failed, and the manifest diff against the committed file
			// will surface the missing primitive if the caller proceeds
			// with WILE_AXIS_B_UPDATE.
			t.Errorf("resolveImpl(%q): %v", pr.Spec.Name, impErr)
			continue
		}
		q = append(q, manifestEntry{
			Name:       pr.Spec.Name,
			ReturnType: returnType,
			ParamTypes: formatParamTypes(pr.Spec.ParamTypes, pr.Spec.IsVariadic),
			GoFunction: goName,
			SourceFile: stripRoot(t, root, absFile),
			SourceLine: line,
		})
	}
	sort.Slice(q, func(i, j int) bool {
		return q[i].Name < q[j].Name
	})
	return q
}

// formatParamTypes renders a PrimitiveSpec.ParamTypes slice into per-slot
// strings using the Phase 5.A convention:
//
//   - Each slot's string is the TypeConstraint.Name() value, or "" for a nil
//     TypeConstraint (unspecified slot).
//   - When isVariadic is true, the last slot is prefixed "..." to mark it as
//     the per-element type of the rest list (not the rest list itself).
//     See registry/registry.go:34 and memory/2026-04-20-paramtypes-audit-design.md §7.1.
//   - An empty input returns nil; formatManifest renders it as "()".
//
// Survey (2026-04-20) of the 484-primitive registry found zero short variadic
// cases — every variadic primitive has len(ParamTypes) == ParamCount when
// ParamTypes is populated. The permissive validation range [1, ParamCount] in
// registry.PrimitiveSpec.Validate is future-proofing; this formatter handles the
// short case correctly but it isn't currently exercised.
func formatParamTypes(types []values.TypeConstraint, isVariadic bool) []string {
	if len(types) == 0 {
		return nil
	}
	q := make([]string, len(types))
	for i, tc := range types {
		name := ""
		if tc != nil {
			name = tc.Name()
		}
		if isVariadic && i == len(types)-1 {
			name = "..." + name
		}
		q[i] = name
	}
	return q
}

// resolveImpl returns the fully-qualified Go function name and absolute
// source file:line for a primitive's Impl function.
//
// Returns ("", "", 0, nil) legitimately for binding-only primitives —
// either untyped nil or a typed-nil ForeignFunction value. The wile
// registry uses typed-nil Impls for names like assoc, member, caar,
// boolean=? that are declared but have no Go body. A typed nil passed
// through an interface satisfies fn != nil but v.IsNil() == true.
//
// Returns a non-nil error for unexpected states — wrong kind, missing
// runtime symbol for a non-nil function, empty FileLine — each of
// which signals that something upstream is broken rather than a
// binding-only registration. Callers should fail the test on non-nil
// err while still recording the entry so the manifest diff surfaces
// which primitive triggered the failure.
func resolveImpl(fn any) (name, file string, line int, err error) {
	if fn == nil {
		return "", "", 0, nil
	}
	v := reflect.ValueOf(fn)
	if v.Kind() != reflect.Func {
		return "", "", 0, werr.WrapForeignErrorf(errResolveImpl,
			"Impl is not a function: kind=%s", v.Kind())
	}
	if v.IsNil() {
		return "", "", 0, nil
	}
	pc := v.Pointer()
	rf := runtime.FuncForPC(pc)
	if rf == nil {
		return "", "", 0, werr.WrapForeignErrorf(errResolveImpl,
			"runtime.FuncForPC returned nil for pc=%x", pc)
	}
	file, line = rf.FileLine(pc)
	if file == "" || line == 0 {
		return rf.Name(), "", 0, werr.WrapForeignErrorf(errResolveImpl,
			"FileLine empty/zero for %s", rf.Name())
	}
	return rf.Name(), file, line, nil
}

// formatManifest renders entries as a Scheme S-expression list, one tuple
// per line. Each tuple is:
//
//	(name return-type (param-types...) go-function source-location)
//
// All scalar fields are quoted as Scheme strings. The param-types slot is a
// parenthesized list of quoted strings — empty "()" for primitives with no
// ParamTypes annotation. Double quotes and backslashes in any field are
// escaped.
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
		writeParamTypesList(&b, e.ParamTypes)
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

// writeParamTypesList writes the param-types slot as "(s1 s2 ... sN)" with
// each string quoted. Empty input produces "()".
func writeParamTypesList(b *strings.Builder, types []string) {
	b.WriteByte('(')
	for i, s := range types {
		if i > 0 {
			b.WriteByte(' ')
		}
		writeSchemeString(b, s)
	}
	b.WriteByte(')')
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

func TestBuildAxisBManifest(t *testing.T) {
	entries := buildManifest(t)
	if len(entries) < 400 {
		t.Fatalf("expected at least 400 primitives, got %d", len(entries))
	}

	seen := make(map[string]int, len(entries))
	bindingOnly := 0
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
			bindingOnly++
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

	if bindingOnly > maxBindingOnlyPrimitives {
		t.Errorf("binding-only primitives (%d) exceeds expected max (%d) — "+
			"possible Impl registration regression",
			bindingOnly, maxBindingOnlyPrimitives)
	}

	sortDiffs := 0
	for i := 1; i < len(entries); i++ {
		if entries[i-1].Name > entries[i].Name {
			t.Errorf("entries not sorted: %q > %q at positions %d, %d",
				entries[i-1].Name, entries[i].Name, i-1, i)
			sortDiffs++
			if sortDiffs >= maxReportedDiffs {
				t.Errorf("... additional sort violations suppressed after %d",
					maxReportedDiffs)
				break
			}
		}
	}

	t.Logf("manifest: %d entries (%d binding-only)", len(entries), bindingOnly)
	for _, name := range []string{"car", "cdr", "cons", "+"} {
		idx, ok := seen[name]
		if !ok {
			t.Logf("  %-10s (not found in manifest)", name)
			continue
		}
		e := entries[idx]
		t.Logf("  %-10s return=%-12s params=%-30s fn=%s loc=%s:%d",
			e.Name, e.ReturnType, "("+strings.Join(e.ParamTypes, " ")+")",
			e.GoFunction, e.SourceFile, e.SourceLine)
	}

	generated := formatManifest(entries)
	path := filepath.Join(repoRoot(t), axisBManifestPath)

	if os.Getenv(axisBUpdateEnvVar) != "" {
		err := os.WriteFile(path, []byte(generated), 0644)
		if err != nil {
			t.Fatalf("write manifest: %v", err)
		}
		t.Logf("updated %s (%d entries)", axisBManifestPath, len(entries))
		return
	}

	committed, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v (run with %s=1 to generate)",
			axisBManifestPath, err, axisBUpdateEnvVar)
	}
	if string(committed) != generated {
		t.Errorf("%s is out of date\nrun: %s=1 go test -run TestBuildAxisBManifest .",
			axisBManifestPath, axisBUpdateEnvVar)
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
			name: "single entry no ParamTypes",
			input: []manifestEntry{
				{
					Name:       "car",
					ReturnType: "any",
					GoFunction: "github.com/aalpar/wile/pkg/registry/core.primCar",
					SourceFile: "registry/core/lists.go",
					SourceLine: 42,
				},
			},
			expected: "(" +
				`("car" "any" () "github.com/aalpar/wile/pkg/registry/core.primCar" "registry/core/lists.go:42")` +
				")\n",
		},
		{
			name: "fixed ParamTypes",
			input: []manifestEntry{
				{
					Name: "vector-set!", ReturnType: "void",
					ParamTypes: []string{"vector", "integer", "any"},
					GoFunction: "pkg.PrimVectorSet", SourceFile: "f.go", SourceLine: 1,
				},
			},
			expected: `(("vector-set!" "void" ("vector" "integer" "any") "pkg.PrimVectorSet" "f.go:1"))` + "\n",
		},
		{
			name: "variadic rest-slot prefix",
			input: []manifestEntry{
				{
					Name: "+", ReturnType: "number",
					ParamTypes: []string{"...number"},
					GoFunction: "pkg.PrimAdd", SourceFile: "a.go", SourceLine: 28,
				},
				{
					Name: "-", ReturnType: "number",
					ParamTypes: []string{"number", "...number"},
					GoFunction: "pkg.PrimSub", SourceFile: "a.go", SourceLine: 29,
				},
			},
			expected: `(("+" "number" ("...number") "pkg.PrimAdd" "a.go:28")` + "\n" +
				` ("-" "number" ("number" "...number") "pkg.PrimSub" "a.go:29"))` + "\n",
		},
		{
			name: "multiple entries",
			input: []manifestEntry{
				{Name: "a", ReturnType: "x", ParamTypes: []string{"y"}, GoFunction: "pkg.A", SourceFile: "a.go", SourceLine: 1},
				{Name: "b", ReturnType: "", GoFunction: "pkg.B", SourceFile: "b.go", SourceLine: 2},
			},
			expected: `(("a" "x" ("y") "pkg.A" "a.go:1")` + "\n" +
				` ("b" "" () "pkg.B" "b.go:2"))` + "\n",
		},
		{
			name: "escapes double-quote and backslash in names",
			input: []manifestEntry{
				{Name: `weird"name\here`, ReturnType: "x", GoFunction: "pkg.F", SourceFile: "f.go", SourceLine: 1},
			},
			expected: `(("weird\"name\\here" "x" () "pkg.F" "f.go:1"))` + "\n",
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

func TestFormatParamTypes(t *testing.T) {
	tcs := []struct {
		name       string
		types      []values.TypeConstraint
		isVariadic bool
		want       []string
	}{
		{
			name:       "empty",
			types:      nil,
			isVariadic: false,
			want:       nil,
		},
		{
			name:       "non-variadic single slot",
			types:      []values.TypeConstraint{values.TypeInteger},
			isVariadic: false,
			want:       []string{"integer"},
		},
		{
			name:       "non-variadic multiple slots",
			types:      []values.TypeConstraint{values.TypeVector, values.TypeInteger, values.TypeAny},
			isVariadic: false,
			want:       []string{"vector", "integer", "any"},
		},
		{
			name:       "variadic sole rest slot",
			types:      []values.TypeConstraint{values.TypeNumber},
			isVariadic: true,
			want:       []string{"...number"},
		},
		{
			name:       "variadic fixed + rest",
			types:      []values.TypeConstraint{values.TypeNumber, values.TypeNumber},
			isVariadic: true,
			want:       []string{"number", "...number"},
		},
		{
			name:       "nil slot rendered as empty string",
			types:      []values.TypeConstraint{values.TypeInteger, nil},
			isVariadic: false,
			want:       []string{"integer", ""},
		},
		{
			name:       "nil rest slot still gets prefix",
			types:      []values.TypeConstraint{nil},
			isVariadic: true,
			want:       []string{"..."},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := formatParamTypes(tc.types, tc.isVariadic)
			if !reflect.DeepEqual(got, tc.want) {
				t.Errorf("formatParamTypes mismatch\nwant: %v\ngot:  %v", tc.want, got)
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
		pkgSubstr      string   // must appear in GoFunction
		sourceContains string   // must appear in SourceFile
		paramTypes     []string // nil = skip check; otherwise exact match
	}{
		// Core primitives — registered as plain Go functions.
		{name: "car", pkgSubstr: "wile/pkg/registry/core", sourceContains: "pkg/registry/core/", paramTypes: []string{"pair"}},
		{name: "cdr", pkgSubstr: "wile/pkg/registry/core", sourceContains: "pkg/registry/core/", paramTypes: []string{"pair"}},
		{name: "cons", pkgSubstr: "wile/pkg/registry/core", sourceContains: "pkg/registry/core/", paramTypes: []string{"any", "any"}},
		// `+` is variadic: ParamCount=1, IsVariadic=true, ParamTypes=[TypeNumber].
		// The lone slot is the rest-element type; rendered with "..." prefix.
		{name: "+", pkgSubstr: "wile/pkg/registry/core", sourceContains: "pkg/registry/core/", paramTypes: []string{"...number"}},

		// Extension primitives — one per major extension package. If any
		// of these starts resolving to the wrong package, it means the
		// extension changed how it registers its primitive (e.g., through
		// a new wrapper helper whose source location masks the real Impl).
		// sin in particular goes through makeComplexPrimitive.func10; the
		// package substring still holds because closures inherit their
		// enclosing package, but a registration refactor that moved the
		// wrapper out of extensions/math would surface here.
		{name: "file-exists?", pkgSubstr: "wile/extensions/files", sourceContains: "extensions/files/"},
		{name: "sin", pkgSubstr: "wile/extensions/math", sourceContains: "extensions/math/"},
		{name: "read-char", pkgSubstr: "wile/pkg/extensions/io", sourceContains: "pkg/extensions/io/"},
		{name: "current-time", pkgSubstr: "wile/extensions/threads", sourceContains: "extensions/threads/"},
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
			if tc.paramTypes != nil && !reflect.DeepEqual(e.ParamTypes, tc.paramTypes) {
				t.Errorf("%s ParamTypes mismatch\nwant: %v\ngot:  %v",
					tc.name, tc.paramTypes, e.ParamTypes)
			}
		})
	}
}

// TestManifestStability asserts buildManifest is deterministic across
// repeated invocations in the same process. Non-determinism here would
// cause WILE_AXIS_B_UPDATE runs to produce unstable diffs.
func TestManifestStability(t *testing.T) {
	first := buildManifest(t)
	second := buildManifest(t)
	if len(first) != len(second) {
		t.Fatalf("count differs across runs: %d vs %d", len(first), len(second))
	}
	diffs := 0
	for i := range first {
		// ParamTypes is a slice, so struct != doesn't compile.
		// reflect.DeepEqual compares by value including slice contents.
		if !reflect.DeepEqual(first[i], second[i]) {
			t.Errorf("entry %d differs: %+v vs %+v", i, first[i], second[i])
			diffs++
			if diffs >= maxReportedDiffs {
				t.Errorf("... additional diffs suppressed after %d", maxReportedDiffs)
				break
			}
		}
	}
}

// repoRoot returns the absolute path of the wile repo root, inferred from
// this test file's location. This package lives at pkg/wile/, so the module
// root is two directories up. Fails the test if runtime.Caller reports no
// symbol info — otherwise stripRoot would silently corrupt every source
// path in the generated manifest.
func repoRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller(0) failed — cannot infer repo root")
	}
	return filepath.Join(filepath.Dir(thisFile), "..", "..")
}

// stripRoot returns abs as a repo-relative, forward-slash-separated path.
// If abs is empty (the legitimate no-source case from resolveImpl on a
// binding-only primitive), returns "". Fails the test if abs is non-empty
// but does not live under root — that indicates either repo-root detection
// is wrong or the primitive's Impl resolved to a function outside the repo
// tree (e.g., a vendored module cache).
//
// Both root and abs are canonicalized via filepath.EvalSymlinks before the
// relative-path computation: runtime.Caller(0) and runtime.FuncForPC.FileLine
// can disagree on symlink resolution (macOS /Users ↔ /private/Users, or a
// repo checked out through a symlink), and a naive string-prefix trim would
// then silently emit absolute paths that fail the caller's IsAbs check.
// EvalSymlinks falls through to the raw path on error so a genuinely out-of-
// tree source still hits the loud-failure branch below rather than masking it.
func stripRoot(t *testing.T, root, abs string) string {
	t.Helper()
	if abs == "" {
		return ""
	}
	canonRoot := evalSymlinksOrRaw(root)
	canonAbs := evalSymlinksOrRaw(abs)
	rel, err := filepath.Rel(canonRoot, canonAbs)
	if err != nil {
		t.Errorf("stripRoot: filepath.Rel(%q, %q): %v", canonRoot, canonAbs, err)
		return abs
	}
	if rel == ".." || strings.HasPrefix(rel, ".."+string(filepath.Separator)) {
		t.Errorf("stripRoot: %q is not under repo root %q", abs, root)
		return abs
	}
	return filepath.ToSlash(rel)
}

// evalSymlinksOrRaw returns filepath.EvalSymlinks(p), or p unchanged if
// EvalSymlinks fails (e.g., target doesn't exist on disk). Callers should
// still validate the resulting path — this helper is a defensive canonicalizer,
// not a correctness guarantee.
func evalSymlinksOrRaw(p string) string {
	resolved, err := filepath.EvalSymlinks(p)
	if err != nil {
		return p
	}
	return resolved
}
