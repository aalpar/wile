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

package wile_test

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// missEngine is the configuration the chain's absence/denial line matters
// under: a virtual source and the host filesystem, both behind a path-confining
// authorizer. The virtual arm refuses every name; the host arm reports a missing
// file absent. A miss here must read as an absence, not as a refusal. dir holds
// the scripts the tests load, which reach include and import the only way a
// confining authorizer leaves open (eval is denied under it).
func missEngine(t *testing.T, files map[string]string) *wile.Engine {
	t.Helper()
	root := realRoot(t)
	for name, src := range files {
		err := os.WriteFile(filepath.Join(root, name), []byte(src), 0o600)
		qt.Assert(t, err, qt.IsNil)
	}
	t.Chdir(root)
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(security.FilesystemRoot(root)),
		wile.WithSourceFS(fstest.MapFS{}),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(root),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// realRoot is a temp directory with symlinks resolved, so the authorizer's root
// and the resolver's absolute paths agree on macOS (/tmp -> /private/tmp).
func realRoot(t *testing.T) string {
	t.Helper()
	root, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)
	return root
}

// TestMissingSourceIsCatchableUnderAConfiningAuthorizer pins that load, include
// and import of a file that is simply not there are ordinary Scheme errors. A
// virtual-arm denial joined into the miss would make the error match
// security.ErrAccessDenied, which the VM treats as uncatchable, and the guard
// would be bypassed.
func TestMissingSourceIsCatchableUnderAConfiningAuthorizer(t *testing.T) {
	tcs := []struct {
		name string
		file string
		src  string
	}{
		{name: "load", file: "", src: ""},
		{name: "include", file: "inc.scm", src: `(include "missing-xyz.scm")`},
		{name: "import", file: "imp.scm", src: `(import (missing-xyz))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			target := "missing-xyz.scm"
			files := map[string]string{}
			if tc.file != "" {
				target = tc.file
				files[tc.file] = tc.src
			}
			eng := missEngine(t, files)
			v, err := eng.EvalMultiple(context.Background(),
				`(guard (e (#t 'caught)) (load "`+target+`"))`)
			c.Assert(err, qt.IsNil, qt.Commentf("the guard did not catch the miss"))
			c.Assert(v.SchemeString(), qt.Equals, "caught")
		})
	}
}

// TestMissingLibraryMessageIsOneLine pins error-object-message for a library
// miss: the probes of both extensions, on one line, with no newline and no
// repeated category label. errors.Join alone renders a line per member.
func TestMissingLibraryMessageIsOneLine(t *testing.T) {
	c := qt.New(t)
	root := realRoot(t)
	err := os.WriteFile(filepath.Join(root, "imp.scm"), []byte(`(import (scheme nonexistent-xyz))`), 0o600)
	c.Assert(err, qt.IsNil)
	t.Chdir(root)
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{}),
		wile.WithSourceOS(),
		wile.WithLibraryPaths(root),
	)
	c.Assert(err, qt.IsNil)
	defer eng.Close() //nolint:errcheck // test cleanup

	v, err := eng.EvalMultiple(context.Background(),
		`(guard (e (#t (error-object-message e))) (load "imp.scm"))`)
	c.Assert(err, qt.IsNil)
	s, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	c.Logf("message: %q", s.Value)
	c.Assert(strings.Contains(s.Value, "\n"), qt.IsFalse)
	c.Assert(strings.Contains(s.Value, "nonexistent-xyz.sld"), qt.IsTrue)
	c.Assert(strings.Contains(s.Value, "nonexistent-xyz.scm"), qt.IsTrue)
	c.Assert(strings.Contains(s.Value, "file not found"), qt.IsFalse)
}
