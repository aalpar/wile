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
	"testing"

	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// evalInDir evaluates src on a KitchenSink engine whose current directory is a
// fresh temp dir holding files, and returns the displayed string. The files
// are opened by a RELATIVE name, so a port that reported the resolved path
// would differ from the written spelling: every assertion below compares the
// whole name with Equals, never a suffix or Contains, because
// "/abs/dir/i.scm:2:5" ends with "i.scm:2:5".
func evalInDir(t *testing.T, files map[string]string, src string) string {
	t.Helper()
	c := qt.New(t)
	ctx := context.Background()
	dir := t.TempDir()
	for name, body := range files {
		err := os.MkdirAll(filepath.Dir(filepath.Join(dir, name)), 0o700)
		c.Assert(err, qt.IsNil)
		err = os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600)
		c.Assert(err, qt.IsNil)
	}
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	_, err = eng.EvalMultiple(ctx, `(set-current-directory! "`+dir+`")`)
	c.Assert(err, qt.IsNil)
	v, err := eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil)
	str, ok := v.Internal().(*values.String)
	c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
	return str.Value
}

// I055-a1: read-syntax on a file port names the file, not #f.
func TestReadSyntaxFromFilePortNamesTheFile(t *testing.T) {
	got := evalInDir(t,
		map[string]string{"f.scm": "(define x 1)\n  (foo bar)\n"},
		`(let ((p (open-input-file "f.scm")))
		   (read-syntax p)
		   (let ((s (read-syntax p)) (o (open-output-string)))
		     (write (list (syntax-source s) (syntax-line s) (syntax-column s)) o)
		     (get-output-string o)))`)
	qt.New(t).Assert(got, qt.Equals, `("f.scm" 2 2)`)
}

// I055-a2: a read error from a file port names the file and the position in it.
func TestReadErrorFromFilePortNamesTheFile(t *testing.T) {
	got := evalInDir(t,
		map[string]string{"i.scm": "(define x 1)\n(foo ]\n"},
		`(guard (e (#t (error-object-source e)))
		   (let ((p (open-input-file "i.scm")))
		     (read p)
		     (read p)))`)
	qt.New(t).Assert(got, qt.Equals, "i.scm:2:5")
}

// The name is the spelling the program wrote, through every way of opening a
// file port.
func TestFilePortsCarryTheWrittenName(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want string
	}{
		{
			name: "call-with-input-file",
			src: `(call-with-input-file "./sub/../g.scm"
			        (lambda (p) (syntax-source (read-syntax p))))`,
			want: "./sub/../g.scm",
		},
		{
			name: "with-input-from-file",
			src: `(with-input-from-file "g.scm"
			        (lambda () (syntax-source (read-syntax))))`,
			want: "g.scm",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := evalInDir(t, map[string]string{"g.scm": "(a b)\n", "sub/keep": ""}, tc.src)
			qt.New(t).Assert(got, qt.Equals, tc.want)
		})
	}
}

// String ports stay unnamed: Wile's syntax-source is a string or #f, so the
// literal "string" would be indistinguishable from a file named "string".
func TestStringPortStaysUnnamed(t *testing.T) {
	got := evalInDir(t, nil,
		`(let ((o (open-output-string)))
		   (write (syntax-source (read-syntax (open-input-string "(a b)"))) o)
		   (get-output-string o))`)
	qt.New(t).Assert(got, qt.Equals, "#f")
}
