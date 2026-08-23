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
	"path/filepath"
	"testing"
)

// TestScanFile pins the detector against testdata/sample.go. The fixture's
// layout is fixed; the violation lines below must track it.
func TestScanFile(t *testing.T) {
	path := filepath.Join("testdata", "sample.go")
	findings, err := scanFile(path)
	if err != nil {
		t.Fatalf("scanFile(%q): %v", path, err)
	}

	// Only the single-line declarations: singleLine (line 24) and
	// twoStmtsOneLine (line 28). The multi-line functions, the empty bodies,
	// and BOTH func literals (the single-line one at line 31 and the
	// multi-line one) must NOT be reported.
	wantLines := []int{24, 28}

	gotLines := make([]int, len(findings))
	for i, f := range findings {
		gotLines[i] = f.line
	}

	if len(gotLines) != len(wantLines) {
		t.Fatalf("got %d findings %v, want %d at %v", len(gotLines), gotLines, len(wantLines), wantLines)
	}
	for i, want := range wantLines {
		if gotLines[i] != want {
			t.Errorf("finding %d: got line %d, want %d (full: %v)", i, gotLines[i], want, gotLines)
		}
	}
}

// TestExemptions guards the two explicit carve-outs:
//   - empty bodies (nothing to spread), even though their braces share a line;
//   - func literals, single-line or not (line 31 in the fixture is a
//     single-line literal that must stay unreported).
func TestExemptions(t *testing.T) {
	path := filepath.Join("testdata", "sample.go")
	findings, err := scanFile(path)
	if err != nil {
		t.Fatalf("scanFile(%q): %v", path, err)
	}
	for _, f := range findings {
		switch {
		case f.desc == "func emptyBody" || f.desc == "func first" || f.desc == "func second" || f.desc == "func inner":
			t.Errorf("empty-bodied %s at line %d should be exempt", f.desc, f.line)
		case f.line == 31:
			t.Errorf("single-line func literal at line %d should be exempt", f.line)
		}
	}
}
