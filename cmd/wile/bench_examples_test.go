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

// benchmarksExample matches a benchmark list in the three forms the script and
// its docs use: a quoted example (`BENCHMARKS="a b"`, also with the quotes
// backslash-escaped, as in the script's own echoed help), an unquoted single
// name (`BENCHMARKS=tak`), and the script's default (`${BENCHMARKS:-a b}`). The
// leading class keeps LARCENY_BENCHMARKS (a different namespace, where `ack`
// is valid) and the CANONICAL_/EXTRA_/ALL_ arrays out; the value classes
// exclude `$` and `(`, so a variable expansion or a bash array is not a list.
var benchmarksExample = regexp.MustCompile(
	`(?:^|[^A-Z_])BENCHMARKS(?:=\\?"([a-z0-9 ._-]+)\\?"|=([a-z0-9._-]+)|:-([a-z0-9 ._-]+)\})`)

// benchmarkExampleFindings returns how many names src lists and one finding for
// each name that resolve rejects. compare-schemes.sh appends ".scm" to each
// name, so a listed "tak.scm" resolves to tak.scm.scm and is a finding.
func benchmarkExampleFindings(file, src string, resolve func(name string) bool) (int, []string) {
	var checked int
	var findings []string
	for _, m := range benchmarksExample.FindAllStringSubmatch(src, -1) {
		for _, list := range m[1:] {
			for bench := range strings.FieldsSeq(list) {
				checked++
				if !resolve(bench) {
					findings = append(findings, file+": BENCHMARKS lists "+bench+", which has no examples/benchmarks/"+bench+".scm")
				}
			}
		}
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
	resolve := func(name string) bool {
		_, err := os.Stat(filepath.Join(root, "examples", "benchmarks", name+".scm"))
		return err == nil
	}
	files := gitOutput(t, root, "ls-files", "-z", "--", "Makefile", "examples/benchmarks/*.sh", "examples/benchmarks/*.md")
	var checked int
	var findings []string
	for name := range strings.SplitSeq(files, "\x00") {
		if name == "" {
			continue
		}
		src, err := os.ReadFile(filepath.Join(root, name))
		c.Assert(err, qt.IsNil, qt.Commentf("a tracked file in the scan list is unreadable"))
		n, f := benchmarkExampleFindings(name, string(src), resolve)
		checked += n
		findings = append(findings, f...)
	}
	c.Assert(checked > 0, qt.IsTrue, qt.Commentf("no benchmark list found; the grammar or the file list has drifted"))
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
		{"other tool's list", `LARCENY_BENCHMARKS="fib tak ack"`, 0, nil},
		{"array assignment", `CANONICAL_BENCHMARKS=(`, 0, nil},
		{"prose mention", "BENCHMARKS= are silently ignored", 0, nil},
		{"variable expansion", `for bench in $BENCHMARKS; do`, 0, nil},
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
