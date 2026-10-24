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

// bareBenchmarksExample matches a literal BENCHMARKS="a b c" usage example. The
// leading class keeps LARCENY_BENCHMARKS (a different namespace, where `ack` is
// valid) out, and the value class excludes `$`, so the default-expansion line in
// compare-schemes.sh and its `\"`-escaped echo are not examples.
var bareBenchmarksExample = regexp.MustCompile(`(^|[^A-Z_])BENCHMARKS="([a-z0-9 ._-]+)"`)

// TestBenchmarkExampleNamesResolve fails when a documented BENCHMARKS="..."
// example names a benchmark compare-schemes.sh cannot find. The script resolves
// each name to examples/benchmarks/<name>.scm, so the example list is checked
// against that directory rather than against a copy of the names.
func TestBenchmarkExampleNamesResolve(t *testing.T) {
	c := qt.New(t)
	root := gitOutput(t, ".", "rev-parse", "--show-toplevel")
	var files []string
	for name := range strings.SplitSeq(gitOutput(t, root, "ls-files", "-z"), "\x00") {
		if name == "Makefile" || isBenchmarkDoc(name) {
			files = append(files, name)
		}
	}
	var checked int
	var findings []string
	for _, name := range files {
		src, err := os.ReadFile(filepath.Join(root, name))
		if err != nil {
			continue
		}
		for _, m := range bareBenchmarksExample.FindAllStringSubmatch(string(src), -1) {
			for bench := range strings.FieldsSeq(m[2]) {
				checked++
				bench = strings.TrimSuffix(bench, ".scm")
				_, err := os.Stat(filepath.Join(root, "examples", "benchmarks", bench+".scm"))
				if err != nil {
					findings = append(findings, name+": BENCHMARKS example names "+bench+", which has no examples/benchmarks/"+bench+".scm")
				}
			}
		}
	}
	c.Assert(checked > 0, qt.IsTrue, qt.Commentf("no BENCHMARKS=\"...\" example found; the grammar or the file list has drifted"))
	c.Assert(findings, qt.HasLen, 0, qt.Commentf("%s", strings.Join(findings, "\n")))
}

// isBenchmarkDoc reports whether a tracked path is a script or document under
// examples/benchmarks that may carry a BENCHMARKS example.
func isBenchmarkDoc(name string) bool {
	if !strings.HasPrefix(name, "examples/benchmarks/") {
		return false
	}
	return strings.HasSuffix(name, ".sh") || strings.HasSuffix(name, ".md")
}
