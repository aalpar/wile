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
	"path/filepath"
	"testing"
)

// parseFixture parses testdata/sample.go and returns, per function DECLARATION
// name, the maxNesting depth of that declaration's own body (closures inside it
// are NOT counted — they are their own scopes). It also returns the depths of
// every function LITERAL in the file, in source order.
func parseFixture(t *testing.T) (byName map[string]int, litDepths []int) {
	t.Helper()
	path := filepath.Join("testdata", "sample.go")
	fset := token.NewFileSet()
	file, err := parser.ParseFile(fset, path, nil, 0)
	if err != nil {
		t.Fatalf("parse %q: %v", path, err)
	}

	byName = make(map[string]int)
	ast.Inspect(file, func(n ast.Node) bool {
		switch fn := n.(type) {
		case *ast.FuncDecl:
			byName[fn.Name.Name] = maxNesting(fn.Body).depth
		case *ast.FuncLit:
			litDepths = append(litDepths, maxNesting(fn.Body).depth)
		}
		return true
	})
	return byName, litDepths
}

// TestDeclarationDepths pins the intended depth of each named function. These
// are the load-bearing cases for the metric: guard clauses, if/for/if nesting,
// switch-plus-if, and the two closure-placement mirrors.
func TestDeclarationDepths(t *testing.T) {
	byName, _ := parseFixture(t)

	cases := []struct {
		name string
		want int
	}{
		{"flat", 0},
		{"oneGuard", 1},
		{"nestedThree", 3},
		{"switchWithIf", 2},
		// The declaration's OWN body only returns a closure — no control nesting.
		{"deepBodyShallowPlacement", 0},
		// The enclosing loop/if/if nest is depth 3; the flat closure inside does
		// not add to it.
		{"shallowLiteralDeepPlacement", 3},
	}
	for _, c := range cases {
		got, ok := byName[c.name]
		if !ok {
			t.Errorf("%s: not found in fixture", c.name)
			continue
		}
		if got != c.want {
			t.Errorf("%s: depth = %d, want %d", c.name, got, c.want)
		}
	}
}

// TestLiteralsAreOwnScope confirms function literals are measured independently
// of their placement: the deeply-nested-body closure reports depth 3, the flat
// closure reports depth 0.
func TestLiteralsAreOwnScope(t *testing.T) {
	_, litDepths := parseFixture(t)

	// Source order: the depth-3 closure in deepBodyShallowPlacement, then the
	// flat closure in shallowLiteralDeepPlacement.
	want := []int{3, 0}
	if len(litDepths) != len(want) {
		t.Fatalf("got %d literals %v, want %d %v", len(litDepths), litDepths, len(want), want)
	}
	for i, w := range want {
		if litDepths[i] != w {
			t.Errorf("literal %d: depth = %d, want %d (all: %v)", i, litDepths[i], w, litDepths)
		}
	}
}

// TestScanFileThreshold exercises the end-to-end file scan and its threshold:
// at max=2 only the depth-3 scopes are reported (nestedThree, the depth-3
// closure, and shallowLiteralDeepPlacement); everything at depth <= 2 is clear.
func TestScanFileThreshold(t *testing.T) {
	path := filepath.Join("testdata", "sample.go")
	findings, err := scanFile(path, 2)
	if err != nil {
		t.Fatalf("scanFile(%q): %v", path, err)
	}
	if len(findings) != 3 {
		t.Fatalf("got %d findings, want 3: %+v", len(findings), findings)
	}
	for _, f := range findings {
		if f.depth != 3 {
			t.Errorf("finding %q: depth = %d, want 3", f.desc, f.depth)
		}
	}

	// At a threshold of 3, nothing in the fixture exceeds it.
	none, err := scanFile(path, 3)
	if err != nil {
		t.Fatalf("scanFile(%q, 3): %v", path, err)
	}
	if len(none) != 0 {
		t.Errorf("at max=3, want 0 findings, got %d: %+v", len(none), none)
	}
}

// TestElseIfLadderIsFlat is the linter's core-property test, left for you to
// write (per the project's "Learning Through Exercise" convention).
//
// The property: `elseIfLadder` in the fixture is a five-arm
// if / else-if / else-if / else-if / else chain. gofmt renders the whole chain
// at ONE indentation level, so its nesting depth must be 1 — not 5. This is the
// single subtlest claim the metric makes (see the else-if branch in
// maxNesting): a naive AST walk that treats each `.Else` IfStmt as a child would
// report 5. If this test does not exist, a regression that re-inflates else-if
// chains would pass silently.
//
// TODO(you): using parseFixture(t), assert that byName["elseIfLadder"] == 1.
// One line of setup, one assertion. Consider also adding a fixture function with
// a genuinely nested if-inside-else-body (not else-if) and confirming THAT one
// does count as deeper — the contrast is where the design lives.
func TestElseIfLadderIsFlat(t *testing.T) {
	t.Skip("TODO(you): assert elseIfLadder measures as depth 1, not depth 5")
}
