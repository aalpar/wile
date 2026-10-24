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
	"io/fs"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"
)

// stdlibPaths are the files whose entries are excluded by default: exactly the
// files the embedded stdlib carries, walked from the embed FS so the set cannot
// drift from it. A prefix list did drift: chibi/ and rnrs/ leaked into plain
// reports, and wile/goast/ (which lives in the wile-goast repository, not in
// the embed) was excluded as though it were stdlib.
var stdlibPaths = embeddedStdlibPaths()

// embeddedStdlibPaths walks the embedded stdlib FS into an exact-path set.
func embeddedStdlibPaths() map[string]struct{} {
	q := make(map[string]struct{})
	err := fs.WalkDir(stdlib.FS, ".", func(name string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			return nil
		}
		q[name] = struct{}{}
		return nil
	})
	if err != nil {
		panic(werr.WrapForeignErrorf(err, "coverage: walking the embedded stdlib"))
	}
	return q
}

// WriteGoCover writes the collector's entries in Go cover v1 format
// (mode: set), excluding stdlib paths. The output is consumable by
// 'go tool cover -html -o report.html <file>'.
func WriteGoCover(w io.Writer, c *Collector) error {
	return writeGoCover(w, c, false)
}

// WriteGoCoverIncludingStdlib is like WriteGoCover but includes
// entries from embedded stdlib files.
func WriteGoCoverIncludingStdlib(w io.Writer, c *Collector) error {
	return writeGoCover(w, c, true)
}

func writeGoCover(w io.Writer, c *Collector, includeStdlib bool) error {
	_, err := fmt.Fprintln(w, "mode: set")
	if err != nil {
		return err
	}
	for _, e := range c.Entries() {
		if !includeStdlib && isStdlibPath(e.File) {
			continue
		}
		// SourceIndexes columns are 0-based; Go cover columns are 1-based
		// (both ends exclusive). Lines are 1-based on both sides.
		_, err = fmt.Fprintf(w, "%s:%d.%d,%d.%d 1 %d\n",
			e.File, e.StartLine, e.StartCol+1, e.EndLine, e.EndCol+1, e.Count)
		if err != nil {
			return err
		}
	}
	return nil
}

// isStdlibPath reports whether a file path is part of the embedded
// stdlib and should be excluded from user-facing reports by default.
func isStdlibPath(file string) bool {
	_, ok := stdlibPaths[file]
	return ok
}
