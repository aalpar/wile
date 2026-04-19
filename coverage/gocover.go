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
	"strings"
)

// stdlibPrefixes are paths whose entries are excluded by default.
// They correspond to the embedded R7RS stdlib paths under stdlib/lib/.
var stdlibPrefixes = []string{
	"scheme/",
	"wile/",
	"srfi/",
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
		_, err = fmt.Fprintf(w, "%s:%d.%d,%d.%d 1 %d\n",
			e.File, e.StartLine, e.StartCol, e.EndLine, e.EndCol, e.Count)
		if err != nil {
			return err
		}
	}
	return nil
}

// isStdlibPath reports whether a file path is part of the embedded
// stdlib and should be excluded from user-facing reports by default.
func isStdlibPath(file string) bool {
	for _, prefix := range stdlibPrefixes {
		if strings.HasPrefix(file, prefix) {
			return true
		}
	}
	return false
}
