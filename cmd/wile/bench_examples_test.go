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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"
)

// benchmarksExample matches a benchmark list in the forms the script and its
// docs use: a double-quoted example (`BENCHMARKS="a b"`, also with the quotes
// backslash-escaped, as in the script's own echoed help), a single-quoted one,
// an unquoted single name (`BENCHMARKS=tak`), and the script's default
// (`${BENCHMARKS:-a b}`). A quoted or default list may continue across lines
// with a trailing backslash. The leading class keeps LARCENY_BENCHMARKS (a
// different namespace, where `ack` is valid) and the CANONICAL_/EXTRA_/ALL_
// arrays out; the value classes exclude `$` and `(`, so a variable expansion
// or a bash array is not a list. Upper case is admitted so that a misspelled
// name is reported rather than skipped.
var benchmarksExample = regexp.MustCompile(
	`(?:^|[^A-Z_])BENCHMARKS(?:=\\?"(` + benchmarkListBody + `)\\?"|='(` + benchmarkListBody + `)'|=([A-Za-z0-9._-]+)|:-(` + benchmarkListBody + `)\})`)

// benchmarkListBody is a list's names: separated by blanks, and by a
// backslash-newline continuation.
const benchmarkListBody = `(?:[A-Za-z0-9 \t._-]|\\\n)+`

// benchmarksMention matches anything shaped like a list assignment: the
// variable, then `=` (optionally opening a quote) or `:-`, then a character that
// starts a value. Prose (`BENCHMARKS= are ignored`, also with the name in a
// Markdown code span) and variable expansions do not match. Every mention must also be a benchmarksExample match, so a list
// the grammar cannot read is a finding in the file that holds it rather than a
// silent skip.
var benchmarksMention = regexp.MustCompile(
	`(?:^|[^A-Z_])BENCHMARKS(?:=\\?["']?|:-)[^\s$(\\"'` + "`" + `]`)

// benchmarkExampleFindings returns how many names src lists and one finding for
// each name that resolve rejects and for each list-shaped mention the grammar
// cannot read. compare-schemes.sh appends ".scm" to each name, so a listed
// "tak.scm" resolves to tak.scm.scm and is a finding.
func benchmarkExampleFindings(file, src string, resolve func(name string) bool) (int, []string) {
	var checked int
	var findings []string
	read := map[int]bool{}
	for _, m := range benchmarksExample.FindAllStringSubmatchIndex(src, -1) {
		read[m[0]] = true
		for i := 2; i < len(m); i += 2 {
			if m[i] < 0 {
				continue
			}
			list := strings.ReplaceAll(src[m[i]:m[i+1]], "\\\n", " ")
			for bench := range strings.FieldsSeq(list) {
				checked++
				if !resolve(bench) {
					findings = append(findings, file+": BENCHMARKS lists "+bench+", which has no examples/benchmarks/"+bench+".scm")
				}
			}
		}
	}
	for _, m := range benchmarksMention.FindAllStringIndex(src, -1) {
		if read[m[0]] {
			continue
		}
		line, _, _ := strings.Cut(src[m[0]:], "\n")
		findings = append(findings, file+": a BENCHMARKS list the scanner cannot read: "+strings.TrimSpace(line))
	}
	return checked, findings
}

// TestBenchmarkExampleNamesResolve fails when a documented or default
// benchmark list names a benchmark compare-schemes.sh cannot find. The script
// resolves each name to examples/benchmarks/<name>.scm, so the lists are
// checked against that directory rather than against a copy of the names.
func TestBenchmarkExampleNamesResolve(t *testing.T) {
	c := qt.New(t)
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")
	// A listing, not os.Stat: on a case-insensitive filesystem Stat finds
	// TAK.scm for tak.scm, and compare-schemes.sh on Linux does not.
	entries, err := os.ReadDir(filepath.Join(root, "examples", "benchmarks"))
	c.Assert(err, qt.IsNil)
	present := map[string]bool{}
	for _, e := range entries {
		present[e.Name()] = true
	}
	resolve := func(name string) bool {
		return present[name+".scm"]
	}
	files := gitOutput(t, root, "ls-files", "-z", "--", "Makefile", "examples/benchmarks/*.sh", "examples/benchmarks/*.md")
	checked := map[string]int{}
	var findings []string
	for name := range strings.SplitSeq(files, "\x00") {
		if name == "" {
			continue
		}
		src, err := os.ReadFile(filepath.Join(root, name))
		c.Assert(err, qt.IsNil, qt.Commentf("a tracked file in the scan list is unreadable"))
		n, f := benchmarkExampleFindings(name, string(src), resolve)
		checked[filepath.Ext(name)] += n
		findings = append(findings, f...)
	}
	// Each kind of file carries a list today (Makefile has no extension). A
	// kind with none means the grammar or the file list has stopped seeing it.
	for ext, kind := range map[string]string{"": "Makefile", ".sh": "script", ".md": "document"} {
		c.Assert(checked[ext] > 0, qt.IsTrue, qt.Commentf("no benchmark list found in the %s files; the grammar or the file list has drifted", kind))
	}
	c.Assert(findings, qt.HasLen, 0, qt.Commentf("%s", strings.Join(findings, "\n")))
}

// TestBenchmarkExampleGrammar pins each form the scanner reads, the names it
// reports, and the lists it must leave alone.
func TestBenchmarkExampleGrammar(t *testing.T) {
	known := map[string]bool{"tak": true, "fib": true}
	resolve := func(name string) bool {
		return known[name]
	}
	tests := []struct {
		name         string
		src          string
		wantChecked  int
		wantFindings []string
	}{
		{"quoted, all resolve", `BENCHMARKS="tak fib"`, 2, nil},
		{"quoted, later name missing", `BENCHMARKS="tak nosuch"`, 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"quoted, first name missing", `BENCHMARKS="nosuch tak"`, 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"suffixed name is a finding", `BENCHMARKS="tak.scm"`, 1, []string{"f: BENCHMARKS lists tak.scm, which has no examples/benchmarks/tak.scm.scm"}},
		{"escaped quotes", `echo "  BENCHMARKS=\"tak nosuch\" $0"`, 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"unquoted", `make x BENCHMARKS=nosuch`, 1, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"default expansion", `BENCHMARKS="${BENCHMARKS:-tak nosuch}"`, 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"two lists in one source", "BENCHMARKS=\"tak\"\nBENCHMARKS=\"nosuch\"", 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"variable inside quotes", `BENCHMARKS="$x tak"`, 0, nil},
		{"other tool's list", `LARCENY_BENCHMARKS="fib tak ack"`, 0, nil},
		{"array assignment", `CANONICAL_BENCHMARKS=(`, 0, nil},
		{"prose mention", "BENCHMARKS= are silently ignored", 0, nil},
		{"prose mention in a code span", "`BENCHMARKS=` are silently ignored", 0, nil},
		{"variable expansion", `for bench in $BENCHMARKS; do`, 0, nil},
		{"single-quoted", `BENCHMARKS='tak nosuch'`, 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"upper-case name is read", `BENCHMARKS="tak Fib"`, 2, []string{"f: BENCHMARKS lists Fib, which has no examples/benchmarks/Fib.scm"}},
		{"backslash continuation", "BENCHMARKS=\"tak \\\n  nosuch\"", 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"default with continuation", "${BENCHMARKS:-tak \\\n nosuch}", 2, []string{"f: BENCHMARKS lists nosuch, which has no examples/benchmarks/nosuch.scm"}},
		{"unreadable list is a finding", `BENCHMARKS="tak,fib"`, 0, []string{`f: a BENCHMARKS list the scanner cannot read: BENCHMARKS="tak,fib"`}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := qt.New(t)
			checked, findings := benchmarkExampleFindings("f", tt.src, resolve)
			c.Assert(checked, qt.Equals, tt.wantChecked)
			c.Assert(findings, qt.DeepEquals, tt.wantFindings)
		})
	}
}
