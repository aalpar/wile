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

package resolver

import (
	"os"
	"path/filepath"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"
)

// TestOpenedNameResolvesOnlyADotDotElement pins openedName's trigger: a ".."
// element is resolved against the filesystem, anything else (including ".."
// inside a name, and a name that no longer resolves) is kept as written.
func TestOpenedNameResolvesOnlyADotDotElement(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses a symlink")
	}
	tmp, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, os.MkdirAll(filepath.Join(tmp, "real", "deep"), 0o755), qt.IsNil)
	qt.Assert(t, os.MkdirAll(filepath.Join(tmp, "a"), 0o755), qt.IsNil)
	qt.Assert(t, os.WriteFile(filepath.Join(tmp, "real", "x.scm"), nil, 0o644), qt.IsNil)
	qt.Assert(t, os.Symlink(filepath.Join(tmp, "real", "deep"), filepath.Join(tmp, "a", "link")), qt.IsNil)

	tcs := []struct {
		name string
		in   string
		want string
	}{
		{name: "dot-dot after a link", in: tmp + "/a/link/../x.scm", want: filepath.Join(tmp, "real", "x.scm")},
		{name: "dot-dot inside a name", in: tmp + "/a..b/x.scm", want: tmp + "/a..b/x.scm"},
		{name: "no dot-dot, link kept", in: tmp + "/a/link/x.scm", want: tmp + "/a/link/x.scm"},
		{name: "unresolvable kept", in: tmp + "/gone/../x.scm", want: tmp + "/gone/../x.scm"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, openedName(tc.in), qt.Equals, tc.want)
		})
	}
}
