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
	"go/ast"
	"go/parser"
	"go/token"
	"maps"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

// TestScanFileOptIn checks that only //exhaustive-marked switches are
// returned — by trailing marker or marker-on-the-line-above — and that the
// unmarked switch is ignored. Lines track testdata/sample.go.
func TestScanFileOptIn(t *testing.T) {
	path := filepath.Join("testdata", "sample.go")
	got, err := scanFile(path)
	if err != nil {
		t.Fatalf("scanFile(%q): %v", path, err)
	}

	byLine := map[int]switchInfo{}
	for _, s := range got {
		byLine[s.line] = s
	}

	// markedAbove (24), markedTrailing (34), markedNoValues (51).
	// The unmarked switch (42) must be absent.
	wantLines := []int{24, 34, 51}
	if len(got) != len(wantLines) {
		t.Fatalf("got %d switches at %v, want %d at %v", len(got), keysSorted(byLine), len(wantLines), wantLines)
	}
	for _, ln := range wantLines {
		_, ok := byLine[ln]
		if !ok {
			t.Errorf("expected a marked switch at line %d; got %v", ln, keysSorted(byLine))
		}
	}
	_, unmarkedReported := byLine[42]
	if unmarkedReported {
		t.Errorf("unmarked switch at line 42 should not be reported")
	}

	above := byLine[24]
	if !above.hasValues || len(above.caseTypes) != 2 {
		t.Errorf("line 24: got hasValues=%v cases=%v, want hasValues=true with 2 cases", above.hasValues, above.caseTypes)
	}
	noValues := byLine[51]
	if noValues.hasValues {
		t.Errorf("line 51 (no values.* cases): got hasValues=true, want false")
	}
}

func TestIsExhaustiveMarker(t *testing.T) {
	cases := map[string]bool{
		"//exhaustive":              true,
		"// exhaustive":             true,
		"//exhaustive: must cover":  true,
		"//exhaustive covers Value": true,
		"// not exhaustive":         false,
		"//exhaustively":            false,
		"// some comment":           false,
	}
	for text, want := range cases {
		got := isExhaustiveMarker(text)
		if got != want {
			t.Errorf("isExhaustiveMarker(%q) = %v, want %v", text, got, want)
		}
	}
}

func TestFindMissing(t *testing.T) {
	known := []string{"*values.A", "*values.B", "*values.C"}
	missing := findMissing([]string{"*values.B"}, known)
	want := []string{"*values.A", "*values.C"}
	if strings.Join(missing, ",") != strings.Join(want, ",") {
		t.Errorf("findMissing = %v, want %v", missing, want)
	}
}

// TestKnownValueTypesMatchesSource is the drift guard: it derives the real
// set of exported value types from the values source and asserts the
// hand-maintained knownValueTypes list matches exactly. A new value type, a
// rename, or a removal breaks this test until knownValueTypes is corrected.
func TestKnownValueTypesMatchesSource(t *testing.T) {
	derived, err := exportedValueTypes(filepath.Join("..", "..", "pkg", "values"))
	if err != nil {
		t.Fatalf("deriving value types: %v", err)
	}

	want := map[string]bool{}
	for _, s := range derived {
		want[s] = true
	}
	got := map[string]bool{}
	for _, s := range knownValueTypes {
		got[s] = true
	}

	for s := range want {
		if !got[s] {
			t.Errorf("knownValueTypes missing %s (present in values source)", s)
		}
	}
	for s := range got {
		if !want[s] {
			t.Errorf("knownValueTypes has %s, which no longer implements values.Value (or was renamed/removed)", s)
		}
	}
}

// exportedValueTypes scans the values package source for exported concrete
// types whose method set includes the three values.Value methods. It relies
// on the package not using embedding to provide those methods — which holds
// (every value type defines SchemeString/IsVoid/EqualTo directly).
//
// The receiver's star-ness is recorded per method, not discarded, because it
// decides the spelling a type switch must use. Go's rule: the method set of *T
// holds both receiver kinds, the method set of T holds only value receivers. So
// if any of the three Value methods is declared on *T, only *T satisfies Value
// and *values.T is the sole possible case; if all three are on T, the value form
// satisfies it and is what a Value slot holds. Star-ness of OTHER methods is
// irrelevant — SourceIndexes had pointer-receiver mutators (Inc/NewLine/Tab)
// alongside value-receiver Value methods, and prepending "*" unconditionally is
// how it sat in knownValueTypes as a case no switch could match.
func exportedValueTypes(dir string) ([]string, error) {
	fset := token.NewFileSet()
	// methods[T][m] is true when m is declared on *T, false when on T. Presence
	// is the two-value lookup; the stored bool is star-ness, not existence.
	methods := map[string]map[string]bool{}
	err := filepath.Walk(dir, func(path string, info os.FileInfo, walkErr error) error {
		if walkErr != nil {
			return walkErr
		}
		if info.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		f, parseErr := parser.ParseFile(fset, path, nil, 0)
		if parseErr != nil {
			return parseErr
		}
		for _, d := range f.Decls {
			fn, ok := d.(*ast.FuncDecl)
			if !ok || fn.Recv == nil || len(fn.Recv.List) == 0 {
				continue
			}
			recv := fn.Recv.List[0].Type
			star, isStar := recv.(*ast.StarExpr)
			if isStar {
				recv = star.X
			}
			id, ok := recv.(*ast.Ident)
			if !ok {
				continue
			}
			if methods[id.Name] == nil {
				methods[id.Name] = map[string]bool{}
			}
			methods[id.Name][fn.Name.Name] = isStar
		}
		return nil
	})
	if err != nil {
		return nil, err
	}

	var out []string
	for name, set := range methods {
		if !ast.IsExported(name) {
			continue
		}
		ss, hasSS := set["SchemeString"]
		iv, hasIV := set["IsVoid"]
		eq, hasEQ := set["EqualTo"]
		if !hasSS || !hasIV || !hasEQ {
			continue
		}
		if ss || iv || eq {
			out = append(out, "*values."+name)
			continue
		}
		out = append(out, "values."+name)
	}
	slices.Sort(out)
	return out, nil
}

func keysSorted(m map[int]switchInfo) []int {
	return slices.Sorted(maps.Keys(m))
}
