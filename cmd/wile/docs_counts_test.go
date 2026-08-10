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
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
)

// TestProseCountsMatchTheirSource checks counts written out in prose against the
// artifact each one summarises. A count in a document has no compiler and no
// test; it is a claim that was true when it was typed, and the doc sweeps this
// tree already runs (check-readme-links, check-docs-orphans, docs_refs_test.go)
// check links, orphans and path existence — never a number.
//
// **Deletion is the first choice, and a row here is the fallback.** Before
// adding a row, ask whether the count carries information a reader could not
// derive in one command. If it does not, delete the count and name the command
// instead: a checked count still rots between CI runs and costs a row forever,
// a deleted count cannot rot at all. That is step 3 of the 2026-07-26
// CLAUDE.local.md accuracy audit, and the artifact of applying it is visible in
// the root CLAUDE.local.md, whose sentinel count now reads "Grep rather than
// trusting a count — the inventory moves every release". TODO.md's
// "(27 sub-libraries)" was deleted under that rule rather than added here.
// A count survives to this table when it is a *claim* rather than a mirror of
// something a reader can list — coverage, a defect tally, a deliberate scope.
//
// **Scope, and the boundary it inherits.** Deletion was settled for the
// gitignored corpus (CLAUDE*.md) by that same audit; this table is scoped to
// checked-in documents, and the two decisions are complementary, not opposed.
// But a checked-in document may state a count *about* a gitignored one — both
// rows below are TODO.md summarising a plans/ file — and a clone has no
// plans/. Such a row can only be checked on a maintainer's checkout, so it
// skips loudly rather than passing silently. That is the same gitignore
// boundary that keeps `make planlint` and `make indexlint` out of `make ci`.
//
// **Ownership.** Two other artefacts propose a checker over this class: the open
// `make doclint` item in TODO.md (extract `foo.go:N` citations, assert the file
// exists and N is within `wc -l`), and the 2026-07-26 audit's step 5
// (path-existence, written and never wired). Both are *citation* checks —
// does the cited location exist — and this is a *count* check: does the stated
// number equal the number the source produces. They do not overlap, and
// `doclint` remains open and unsuperseded.
func TestProseCountsMatchTheirSource(t *testing.T) {
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")

	for _, c := range proseCounts {
		t.Run(c.name, func(t *testing.T) {
			body, err := os.ReadFile(filepath.Join(root, c.doc))
			if err != nil {
				t.Fatalf("%s: %v", c.doc, err)
			}
			line, lineNo := lineContaining(string(body), c.rowKey)
			if lineNo == 0 {
				t.Fatalf("%s: no line contains %q — the count's locator has moved; re-pin it, do not delete the row",
					c.doc, c.rowKey)
			}
			m := c.claimed.FindStringSubmatch(line)
			if m == nil {
				t.Fatalf("%s:%d: %v does not match the line carrying %q",
					c.doc, lineNo, c.claimed, c.rowKey)
			}
			claimed, err := strconv.Atoi(m[1])
			if err != nil {
				t.Fatalf("%s:%d: %q is not a number: %v", c.doc, lineNo, m[1], err)
			}

			sourceBody, err := os.ReadFile(filepath.Join(root, c.source))
			if err != nil {
				t.Skipf("SKIPPED, NOT PASSED: %s is absent (it is gitignored), so %s:%d's count of %d is unchecked here",
					c.source, c.doc, lineNo, claimed)
			}
			actual := c.derive(t, string(sourceBody))
			if claimed == actual {
				return
			}
			t.Errorf("%s:%d claims %d, but %s gives %d.\n%s",
				c.doc, lineNo, claimed, c.source, actual, c.why)
		})
	}
}

// proseCount is one count written out in a checked-in document, together with
// the artifact it summarises and the expression that recomputes it.
//
// The count is located by rowKey — a substring stable across edits — rather than
// by line number, because a line number is the least durable identifier a
// citation can use and every insertion above it silently re-points the row.
type proseCount struct {
	name    string
	doc     string         // checked-in document carrying the count
	rowKey  string         // substring identifying the line the count sits on
	claimed *regexp.Regexp // one capture group: the count as written
	source  string         // artifact the true number is derived from
	derive  func(t *testing.T, source string) int
	why     string
}

var proseCounts = []proseCount{
	{
		name:    "TODO_wave1_defect_count",
		doc:     "TODO.md",
		rowKey:  "2026-08-07-review-wave1-silent-corruption-design.local.md",
		claimed: regexp.MustCompile(`(\d+) defects`),
		source:  "plans/2026-08-07-review-wave1-silent-corruption-design.local.md",
		derive:  sumScopeSectionCounts,
		why: "The row summarises Wave 1's own Scope header, which states the per-section tally " +
			"and sums it. The plan's header also records that no grouping recovers the older " +
			"number and that this row still carries it.",
	},
	{
		name:    "TODO_wave6_defect_count",
		doc:     "TODO.md",
		rowKey:  "2026-08-07-review-wave6-cli-stdlib-docs-design.local.md",
		claimed: regexp.MustCompile(`(\d+) defects`),
		source:  "plans/2026-08-07-review-wave6-cli-stdlib-docs-design.local.md",
		derive:  sumScopeSectionCounts,
		why: "The row summarises Wave 6's own Scope header. Note the plan offers a second total " +
			"for a different question (its §5 adds one harness defect), so a row asserting that " +
			"total instead would need its own derivation, not a patched number here.",
	},
}

// scopeSectionCount matches one entry of a plan's per-section defect tally, the
// shape both wave plans state their Scope in: "§1 four, §2 three, §3 five".
// The whitespace after the section number is load-bearing twice over: it keeps
// this off possessives like "§4's two mechanisms", which count something else,
// and it must admit a newline, because the tally wraps mid-entry in one of the
// two plans (matching only a literal space silently drops that section and
// under-counts).
var scopeSectionCount = regexp.MustCompile(`§\d+\s+(one|two|three|four|five|six|seven|eight|nine|ten)\b`)

var numberWords = map[string]int{
	"one": 1, "two": 2, "three": 3, "four": 4, "five": 5,
	"six": 6, "seven": 7, "eight": 8, "nine": 9, "ten": 10,
}

// sumScopeSectionCounts recomputes a wave plan's defect count from the plan's
// own Scope paragraph, by summing the per-section tally stated there.
//
// The tally is the derivation rather than the plan's own total sentence on
// purpose: a total is one more prose number that can drift from its parts, and
// the parts are what a reader checks against the section headers.
func sumScopeSectionCounts(t *testing.T, source string) int {
	t.Helper()
	start := strings.Index(source, "**Scope:**")
	if start < 0 {
		t.Fatalf("no **Scope:** paragraph — the derivation's source has moved")
	}
	scope := source[start:]
	end := strings.Index(scope, "\n\n")
	if end >= 0 {
		scope = scope[:end]
	}
	entries := scopeSectionCount.FindAllStringSubmatch(scope, -1)
	if len(entries) == 0 {
		t.Fatalf("the **Scope:** paragraph states no per-section tally:\n%s", scope)
	}
	q := 0
	for _, e := range entries {
		q += numberWords[e[1]]
	}
	return q
}

// lineContaining returns the first line of body holding key, and its 1-based
// line number. A zero line number means no line does.
func lineContaining(body, key string) (string, int) {
	for i, line := range strings.Split(body, "\n") {
		if strings.Contains(line, key) {
			return line, i + 1
		}
	}
	return "", 0
}
