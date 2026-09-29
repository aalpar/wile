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
	"maps"
	"strings"
	"testing"
)

// TestParseManifest covers the direction that fails OPEN. A row this parser
// misses is a grant of zero, which is strict; a grant it invents — from a path
// quoted inside the header, say — silently licenses the very operation the
// ratchet exists to reject. Every case below is about that asymmetry.
func TestParseManifest(t *testing.T) {
	tcs := []struct {
		name string
		in   string
		want map[string]int
	}{
		{
			name: "empty manifest grants nothing",
			in:   "()\n",
			want: map[string]int{},
		},
		{
			name: "header alone grants nothing",
			in:   manifestHeader,
			want: map[string]int{},
		},
		{
			name: "a path quoted in a comment is not a grant",
			in:   ";; see (\"pkg/values/scopes.go\" 9) for the boundary\n()\n",
			want: map[string]int{},
		},
		{
			name: "a comment after a row does not shadow the row",
			in:   "((\"a.go\" 3)) ;; and (\"b.go\" 7) is only prose\n",
			want: map[string]int{"a.go": 3},
		},
		{
			name: "one row per line",
			in:   "((\"a.go\" 1)\n (\"b/c_test.go\" 42))\n",
			want: map[string]int{"a.go": 1, "b/c_test.go": 42},
		},
		{
			name: "several rows on one line",
			in:   "((\"a.go\" 1) (\"b.go\" 2))\n",
			want: map[string]int{"a.go": 1, "b.go": 2},
		},
		{
			name: "zero is a real grant, distinct from an absent row",
			in:   "((\"a.go\" 0))\n",
			want: map[string]int{"a.go": 0},
		},
		{
			name: "escapes in a path survive the round trip",
			in:   "((\"od\\\"d\\\\path.go\" 2))\n",
			want: map[string]int{"od\"d\\path.go": 2},
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			got := parseManifest(tc.in)
			if !maps.Equal(got, tc.want) {
				t.Errorf("parseManifest = %v, want %v", got, tc.want)
			}
		})
	}
}

// TestRenderManifestRoundTrips pins renderManifest against parseManifest: the
// file the tool writes must be the file the tool reads, or a regeneration would
// quietly reset every grant to zero and the next run would fail on every row.
func TestRenderManifestRoundTrips(t *testing.T) {
	rows := []FileRow{
		{Path: "pkg/values/scopes.go", Count: 1},
		{Path: "pkg/values/scopes_test.go", Count: 31, Test: true},
		{Path: "od\"d\\path.go", Count: 2},
	}
	text := renderManifest(rows, Census{ExprFiles: []string{"pkg/values/scopes.go"}})
	if !strings.HasPrefix(text, ";;") {
		t.Error("rendered manifest does not start with its header")
	}
	if !strings.Contains(text, "423") {
		t.Error("rendered manifest header dropped the pre-flip baseline")
	}
	got := parseManifest(text)
	want := map[string]int{
		"pkg/values/scopes.go":      1,
		"pkg/values/scopes_test.go": 31,
		"od\"d\\path.go":            2,
	}
	if !maps.Equal(got, want) {
		t.Errorf("round trip = %v, want %v", got, want)
	}
}

// TestRenderManifestEmpty pins the shape of a manifest with no rows: "()", the
// same empty-list spelling testdata/axis-b-manifest.scm uses, and still behind
// the header. Reaching zero rows is the goal state, so it must not render
// something the parser cannot read.
func TestRenderManifestEmpty(t *testing.T) {
	text := renderManifest(nil, Census{})
	if !strings.HasSuffix(text, "()\n") {
		t.Errorf("empty manifest does not end in the empty list: %q", text)
	}
	got := parseManifest(text)
	if len(got) != 0 {
		t.Errorf("empty manifest parsed to %v", got)
	}
}

// TestFileRows pins the aggregation and the test/non-test split, which is the
// distinction the ratchet's load-bearing assertion rests on.
func TestFileRows(t *testing.T) {
	sites := []Site{
		{File: "a.go", Kind: "builtin:len"},
		{File: "a.go", Kind: "range"},
		{File: "a.go", Kind: "builtin:len"},
		{File: "b_test.go", Kind: "composite-lit"},
		{File: "sharedtest_test.go", Kind: "composite-lit"},
	}
	rows := fileRows(sites)
	if len(rows) != 3 {
		t.Fatalf("fileRows returned %d rows, want 3: %v", len(rows), rows)
	}
	if rows[0].Path != "a.go" || rows[1].Path != "b_test.go" || rows[2].Path != "sharedtest_test.go" {
		t.Errorf("rows are not sorted by path: %v", rows)
	}
	if rows[0].Count != 3 || rows[0].Test {
		t.Errorf("a.go = %+v, want count 3 and non-test", rows[0])
	}
	if rows[0].Kinds["builtin:len"] != 2 || rows[0].Kinds["range"] != 1 {
		t.Errorf("a.go kinds = %v", rows[0].Kinds)
	}
	if !rows[1].Test || !rows[2].Test {
		t.Errorf("_test.go files not marked as tests: %v", rows[1:])
	}
	nonTest, test := split(rows)
	if nonTest != 3 || test != 2 {
		t.Errorf("split = (%d, %d), want (3, 2)", nonTest, test)
	}
}

// TestStaleRows pins the informational direction: a grant larger than the
// measurement is slack to be ratcheted down, never a failure.
func TestStaleRows(t *testing.T) {
	granted := map[string]int{"a.go": 5, "b.go": 2, "gone.go": 1}
	rows := []FileRow{
		{Path: "a.go", Count: 5},
		{Path: "b.go", Count: 1},
	}
	got := staleRows(granted, rows)
	want := []string{"b.go", "gone.go"}
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Errorf("staleRows = %v, want %v", got, want)
	}
}

// TestTrimTestVariant pins the collapse of one file's three compilations onto
// one package name in the report.
func TestTrimTestVariant(t *testing.T) {
	tcs := []struct {
		in   string
		want string
	}{
		{"github.com/aalpar/wile/pkg/values", "pkg/values"},
		{"github.com/aalpar/wile/pkg/values [github.com/aalpar/wile/pkg/values.test]", "pkg/values"},
		{"github.com/aalpar/wile/pkg/values_test [github.com/aalpar/wile/pkg/values.test]", "pkg/values"},
		{"github.com/aalpar/wile/pkg/values.test", "pkg/values"},
	}
	for _, tc := range tcs {
		got := trimTestVariant(tc.in)
		if got != tc.want {
			t.Errorf("trimTestVariant(%q) = %q, want %q", tc.in, got, tc.want)
		}
	}
}
