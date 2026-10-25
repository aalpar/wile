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
	"io"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/security"
)

// TestOpenedNameTakesTheOpenedNameOnlyForADotDotElement pins openedName's
// trigger: a ".." element reports the name the open reached, anything else
// (including ".." inside a name) is kept as written.
func TestOpenedNameTakesTheOpenedNameOnlyForADotDotElement(t *testing.T) {
	tcs := []struct {
		name    string
		written string
		opened  string
		want    string
	}{
		{name: "dot-dot element", written: "/t/a/link/../x.scm", opened: "/t/real/x.scm", want: "/t/real/x.scm"},
		{name: "dot-dot inside a name", written: "/t/a..b/x.scm", opened: "/t/real/x.scm", want: "/t/a..b/x.scm"},
		{name: "no dot-dot, link kept", written: "/t/a/link/x.scm", opened: "/t/real/deep/x.scm", want: "/t/a/link/x.scm"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			qt.Assert(t, openedName(tc.written, tc.opened), qt.Equals, tc.want)
		})
	}
}

// rootAll confines opens to root and allows everything inside it: the shape of
// a custom RootConfined authorizer, which, unlike the built-in ones, does not
// refuse a ".." after a symlink.
type rootAll struct {
	root string
}

func (rootAll) Authorize(security.AccessRequest) error {
	return nil
}

func (p rootAll) ConfinementRoot() (string, bool) {
	return p.root, true
}

// TestConfinedOpenFileNamesTheFileItOpened pins that each arm reports the file
// it read for "a/link/../x.scm" (a/link -> real/deep): the confined arm opens
// the lexically cleaned a/x.scm, the unconfined arm follows the link to
// real/x.scm. A name resolved apart from the open named real/x.scm under
// confinement while a/x.scm was read.
func TestConfinedOpenFileNamesTheFileItOpened(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("uses a symlink")
	}
	tmp, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, os.MkdirAll(filepath.Join(tmp, "real", "deep"), 0o755), qt.IsNil)
	qt.Assert(t, os.MkdirAll(filepath.Join(tmp, "a"), 0o755), qt.IsNil)
	qt.Assert(t, os.WriteFile(filepath.Join(tmp, "real", "x.scm"), []byte("real"), 0o644), qt.IsNil)
	qt.Assert(t, os.WriteFile(filepath.Join(tmp, "a", "x.scm"), []byte("lexical"), 0o644), qt.IsNil)
	qt.Assert(t, os.Symlink(filepath.Join(tmp, "real", "deep"), filepath.Join(tmp, "a", "link")), qt.IsNil)

	tcs := []struct {
		name     string
		auth     security.Authorizer
		wantName string
		wantData string
	}{
		{name: "confined", auth: rootAll{root: tmp}, wantName: filepath.Join(tmp, "a", "x.scm"), wantData: "lexical"},
		{name: "unconfined", auth: nil, wantName: filepath.Join(tmp, "real", "x.scm"), wantData: "real"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			f, name, err := openAuthorized(tc.auth, tmp+"/a/link/../x.scm")
			qt.Assert(t, err, qt.IsNil)
			defer f.Close() //nolint:errcheck
			data, err := io.ReadAll(f)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, string(data), qt.Equals, tc.wantData)
			qt.Assert(t, name, qt.Equals, tc.wantName)
		})
	}
}
