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

package cxmeasure

import (
	"go/parser"
	"go/token"
	"path/filepath"
	"testing"
)

// fixturePath is the sample measured by every test here. It lives under
// testdata/ so the go tool ignores it as a build target.
func fixturePath() string {
	return filepath.Join("testdata", "sample.go")
}

// measureFixture returns the fixture's functions keyed by qualified name.
func measureFixture(t *testing.T) map[string]FuncStat {
	t.Helper()
	stats, err := ScanFile(fixturePath(), ".")
	if err != nil {
		t.Fatalf("ScanFile(%q): %v", fixturePath(), err)
	}
	q := make(map[string]FuncStat, len(stats))
	for _, s := range stats {
		q[s.Name] = s
	}
	return q
}

// TestFixtureComplexity pins both metrics for every fixture function. These are
// the load-bearing cases: the nesting surcharge, the two flat-cost rules
// (else-if chains and bare else), boolean sequencing, and closure descent.
func TestFixtureComplexity(t *testing.T) {
	stats := measureFixture(t)

	cases := []struct {
		name          string
		wantCognitive int
		wantCyclo     int
	}{
		{"flat", 0, 1},
		{"oneIf", 1, 2},
		{"nestedIfInFor", 3, 3},
		{"elseIfLadder", 3, 4},
		{"bareElse", 2, 2},
		{"wideSwitch", 1, 6},
		{"switchWithNestedIf", 3, 5},
		{"booleans", 3, 4},
		{"withClosure", 2, 2},
		{"*counter.bump", 1, 2},
	}

	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			got, found := stats[c.name]
			if !found {
				t.Fatalf("function %q not measured; fixture and test have drifted", c.name)
			}
			if got.Cognitive != c.wantCognitive {
				t.Errorf("cognitive = %d, want %d", got.Cognitive, c.wantCognitive)
			}
			if got.Cyclomatic != c.wantCyclo {
				t.Errorf("cyclomatic = %d, want %d", got.Cyclomatic, c.wantCyclo)
			}
		})
	}
}

// TestWideIsNotDeep is the tool's whole thesis in one assertion: a five-arm
// dispatch table scores 1 cognitive despite 6 cyclomatic. If breadth ever starts
// costing cognitive points, the ranking this tool produces becomes size-ranking
// with extra steps, and MachineContext.Run stops being distinguishable from a
// genuinely tangled function.
func TestWideIsNotDeep(t *testing.T) {
	stats := measureFixture(t)

	wide := stats["wideSwitch"]
	deep := stats["nestedIfInFor"]

	if wide.Cyclomatic <= deep.Cyclomatic {
		t.Fatalf("fixture broken: wideSwitch cyclomatic %d should exceed nestedIfInFor's %d",
			wide.Cyclomatic, deep.Cyclomatic)
	}
	if wide.Cognitive >= deep.Cognitive {
		t.Errorf("wideSwitch cognitive %d should be BELOW nestedIfInFor's %d: breadth must stay cheap",
			wide.Cognitive, deep.Cognitive)
	}
}

// TestArmExtraction pins the -arms measurement: an arm is scored as though its
// body were a function body, so the switch's own depth surcharge disappears.
func TestArmExtraction(t *testing.T) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, fixturePath(), nil, 0)
	if err != nil {
		t.Fatalf("parse fixture: %v", err)
	}

	cases := []struct {
		fn        string
		wantArms  int
		wantTotal int
		wantMax   int
	}{
		// Five trivial arms: everything collapses to zero once extracted.
		{"wideSwitch", 5, 0, 0},
		// The nested if costs 2 in place (depth 1) and 1 extracted (depth 0).
		{"switchWithNestedIf", 3, 1, 1},
	}

	for _, c := range cases {
		t.Run(c.fn, func(t *testing.T) {
			fn := FindFunc(file, c.fn)
			if fn == nil {
				t.Fatalf("function %q not found in fixture", c.fn)
			}
			_, clauses := WidestSwitch(fn.Body)
			if clauses == nil {
				t.Fatalf("no switch found in %q", c.fn)
			}
			arms := MeasureArms(clauses, fset)
			if len(arms) != c.wantArms {
				t.Fatalf("arm count = %d, want %d", len(arms), c.wantArms)
			}
			total := 0
			for _, a := range arms {
				total += a.Cognitive
			}
			if total != c.wantTotal {
				t.Errorf("extracted total = %d, want %d", total, c.wantTotal)
			}
			// MeasureArms sorts descending, so the first arm is the worst.
			if arms[0].Cognitive != c.wantMax {
				t.Errorf("worst arm = %d, want %d", arms[0].Cognitive, c.wantMax)
			}
		})
	}
}

// TestArmLabels pins the rendering of case guards, including the default arm,
// since the -arms report is only actionable if the arms can be named.
func TestArmLabels(t *testing.T) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, fixturePath(), nil, 0)
	if err != nil {
		t.Fatalf("parse fixture: %v", err)
	}
	fn := FindFunc(file, "wideSwitch")
	if fn == nil {
		t.Fatal("wideSwitch not found in fixture")
	}
	_, clauses := WidestSwitch(fn.Body)
	arms := MeasureArms(clauses, fset)

	seen := make(map[string]bool, len(arms))
	for _, a := range arms {
		seen[a.Label] = true
	}
	for _, want := range []string{"0", "1", "2", "3", "default"} {
		if !seen[want] {
			t.Errorf("arm label %q missing; got %v", want, seen)
		}
	}
}

// TestFindFuncAcceptsQualifiedName confirms a method is reachable by either its
// bare name or its Type.Name rendering, which is what -arms accepts.
func TestFindFuncAcceptsQualifiedName(t *testing.T) {
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, fixturePath(), nil, 0)
	if err != nil {
		t.Fatalf("parse fixture: %v", err)
	}
	for _, name := range []string{"bump", "*counter.bump"} {
		if FindFunc(file, name) == nil {
			t.Errorf("FindFunc(%q) = nil, want the bump method", name)
		}
	}
	if FindFunc(file, "noSuchFunction") != nil {
		t.Error("FindFunc found a function that does not exist")
	}
}

// TestSkippedDirs guards the walk exclusions. testdata is the load-bearing one:
// without it this very fixture would be ranked alongside production code.
func TestSkippedDirs(t *testing.T) {
	for _, dir := range []string{".git", "vendor", "testdata", "dist", "build", "node_modules"} {
		if !skippedDir(dir) {
			t.Errorf("skippedDir(%q) = false, want true", dir)
		}
	}
	for _, dir := range []string{"pkg", "machine", "compilation"} {
		if skippedDir(dir) {
			t.Errorf("skippedDir(%q) = true, want false", dir)
		}
	}
}

// TestMeasurableSkipsTests confirms test files are excluded, matching the
// standalone linters in cmd/.
func TestMeasurableSkipsTests(t *testing.T) {
	cases := []struct {
		path string
		want bool
	}{
		{"pkg/machine/peephole.go", true},
		{"pkg/machine/peephole_test.go", false},
		{"README.md", false},
		{"Makefile", false},
	}
	for _, c := range cases {
		got := measurable(c.path)
		if got != c.want {
			t.Errorf("measurable(%q) = %v, want %v", c.path, got, c.want)
		}
	}
}
