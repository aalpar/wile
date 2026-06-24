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

package coverage

import (
	"fmt"
	"io"
	"sort"
)

// lineStat aggregates per-line coverage.
type lineStat struct {
	file          string
	line          int
	total         int
	covered       int
	maxColReached int
}

// WriteSummary writes a human-readable per-line coverage summary
// with a total footer. Each line emits:
//
//	<file>:<line>  <covered>/<total> covered  max_col_reached=<col>
//
// max_col_reached is the maximum start column of any covered sexpr
// on that line, under the interpretation "how deep did we get."
// Honest under non-branching sequences like (begin a b c);
// approximate under branches — a high max_col_reached with low
// covered/total means execution skipped sexprs in the middle.
//
// Stdlib paths are excluded.
func WriteSummary(w io.Writer, c *Collector) error {
	return writeSummary(w, c, false)
}

// WriteSummaryIncludingStdlib is like WriteSummary but includes
// entries from embedded stdlib files.
func WriteSummaryIncludingStdlib(w io.Writer, c *Collector) error {
	return writeSummary(w, c, true)
}

func writeSummary(w io.Writer, c *Collector, includeStdlib bool) error {
	type key struct {
		file string
		line int
	}
	stats := make(map[key]*lineStat, len(c.Entries()))

	for _, e := range c.Entries() {
		if !includeStdlib && isStdlibPath(e.File) {
			continue
		}
		k := key{e.File, e.StartLine}
		s := stats[k]
		if s == nil {
			s = &lineStat{file: e.File, line: e.StartLine}
			stats[k] = s
		}
		s.total++
		if e.Count > 0 {
			s.covered++
			if e.StartCol > s.maxColReached {
				s.maxColReached = e.StartCol
			}
		}
	}

	keys := make([]key, 0, len(stats))
	for k := range stats {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].file != keys[j].file {
			return keys[i].file < keys[j].file
		}
		return keys[i].line < keys[j].line
	})

	var totalCovered, totalAll int
	for _, k := range keys {
		s := stats[k]
		totalCovered += s.covered
		totalAll += s.total
		_, err := fmt.Fprintf(w, "%s:%d  %d/%d covered  max_col_reached=%d\n",
			s.file, s.line, s.covered, s.total, s.maxColReached)
		if err != nil {
			return err
		}
	}

	_, err := fmt.Fprintf(w, "TOTAL  %d/%d sexprs covered\n", totalCovered, totalAll)
	return err
}
