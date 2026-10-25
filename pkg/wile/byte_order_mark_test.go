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

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"
)

const bom = "\uFEFF"

// newByteOrderMarkEngine builds an engine over an in-memory source tree, with
// path naming a BOM-prefixed file on disk for the port rows.
func newByteOrderMarkEngine(t *testing.T) *wile.Engine {
	t.Helper()
	c := qt.New(t)
	fsys := fstest.MapFS{
		"inc.scm":     &fstest.MapFile{Data: []byte(bom + "(define x 1)")},
		"inc-ci.scm":  &fstest.MapFile{Data: []byte(bom + "(DEFINE X 2)")},
		"ld.scm":      &fstest.MapFile{Data: []byte(bom + "(define ld 3)")},
		"bom/lib.sld": &fstest.MapFile{Data: []byte(bom + "(define-library (bom lib) (export y) (import (scheme base)) (begin (define y 7)))")},
	}
	dir := t.TempDir()
	files := map[string]string{
		"bom.scm":     bom + "(define x 1)\n(y z)",
		"lead-ws.scm": " " + bom + "(a)",
		"second.scm":  "(x) " + bom + "(y)",
	}
	for name, data := range files {
		c.Assert(os.WriteFile(filepath.Join(dir, name), []byte(data), 0o600), qt.IsNil)
	}
	engine, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(nil), // the test exercises the reader, not the sandbox
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(fsys),
		wile.WithLibraryPaths("."),
	)
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	c.Assert(engine.Define("dir", wile.WrapValue(values.NewString(dir+string(filepath.Separator)))), qt.IsNil)
	return engine
}

// TestByteOrderMarkOpeningASourceIsSkipped pins X12 at every entry that reads
// source: a UTF-8 byte order mark opening it is skipped once, before the first
// datum. Master raised "unknown token type ... character U+FEFF" at 1:0 on
// every row but the controls. racket and petite both load and read such a file
// ((define x 1)); they split on read-char, where Wile follows racket (#\xFEFF),
// and on a string port, where Wile again follows racket (petite reads a symbol).
func TestByteOrderMarkOpeningASourceIsSkipped(t *testing.T) {
	tcs := []struct {
		name string
		code string
		want string
	}{
		{name: "EvalProgram", code: bom + "(define x 1) (+ x 41)", want: "42"},
		{name: "include", code: `(include "inc.scm") x`, want: "1"},
		{name: "include-ci", code: `(include-ci "inc-ci.scm") x`, want: "2"},
		{name: "load", code: `(load "ld.scm") (eval 'ld (interaction-environment))`, want: "3"},
		{name: "library", code: `(import (bom lib)) y`, want: "7"},
		{name: "read on a fresh file port", code: `(read (open-input-file (string-append dir "bom.scm")))`, want: "(define x 1)"},
		{name: "read-syntax position counts the mark",
			code: `(let ((s (read-syntax (open-input-file (string-append dir "bom.scm"))))) (list (syntax-line s) (syntax-column s)))`,
			want: "(1 1)"},
		{name: "read on a string port", code: `(read (open-input-string (string (integer->char #xFEFF) #\( #\a #\))))`, want: "(a)"},
		// Controls, green on master: the port still delivers the character, and
		// a reader started after it reads on.
		{name: "read-char sees the mark", code: `(read-char (open-input-file (string-append dir "bom.scm")))`, want: `#\xFEFF`},
		{name: "read after read-char took the mark",
			code: `(let ((p (open-input-file (string-append dir "bom.scm")))) (read-char p) (read p))`,
			want: "(define x 1)"},
		{name: "string->number is not a source", code: `(string->number (string (integer->char #xFEFF) #\5))`, want: "#f"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newByteOrderMarkEngine(t)
			got, err := engine.EvalProgram(context.Background(), tc.code, "bom-test.scm")
			c.Assert(err, qt.IsNil)
			c.Assert(got.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestByteOrderMarkOpeningEvalInputIsSkipped covers the two string entries
// beside EvalProgram: EvalMultiple and Parse. Master failed both at 1:0.
func TestByteOrderMarkOpeningEvalInputIsSkipped(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	engine := newByteOrderMarkEngine(t)
	got, err := engine.EvalMultiple(ctx, bom+"(define x 1) (+ x 41)")
	c.Assert(err, qt.IsNil)
	c.Check(got.SchemeString(), qt.Equals, "42")
	expr, err := engine.Parse(ctx, bom+"(+ 1 2)")
	c.Assert(err, qt.IsNil)
	got, err = engine.Eval(ctx, expr)
	c.Assert(err, qt.IsNil)
	c.Check(got.SchemeString(), qt.Equals, "3")
}

// TestByteOrderMarkPastTheStartIsUnchanged pins that only the mark opening a
// source is skipped: one after whitespace, before a later datum, or after
// read-char has moved the port is the read error it was on master.
func TestByteOrderMarkPastTheStartIsUnchanged(t *testing.T) {
	tcs := []struct {
		name string
		code string
	}{
		{name: "source after whitespace", code: " " + bom + "(a)"},
		{name: "source before the second datum", code: "1 " + bom + "2"},
		{name: "read after whitespace", code: `(read (open-input-file (string-append dir "lead-ws.scm")))`},
		{name: "read of the second datum",
			code: `(let ((p (open-input-file (string-append dir "second.scm")))) (read p) (read p))`},
		{name: "read after read-char took a space",
			code: `(let ((p (open-input-file (string-append dir "lead-ws.scm")))) (read-char p) (read p))`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			engine := newByteOrderMarkEngine(t)
			_, err := engine.EvalMultiple(context.Background(), tc.code)
			c.Assert(err, qt.IsNotNil)
			c.Check(strings.Contains(err.Error(), "U+FEFF"), qt.IsTrue, qt.Commentf("got: %v", err))
		})
	}
}
