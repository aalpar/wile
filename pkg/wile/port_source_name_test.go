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
	"runtime"
	"strings"
	"testing"

	"github.com/aalpar/wile/pkg/security"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// portNameFixture is a KitchenSink engine whose current directory is a fresh
// temp dir. dir is the symlink-resolved directory, because a file port's name
// is the engine-resolved absolute path and every assertion below compares the
// WHOLE name with Equals, never a suffix: "/abs/dir/i.scm:2:5" ends with
// "i.scm:2:5".
type portNameFixture struct {
	t   *testing.T
	eng *wile.Engine
	dir string
}

func newPortNameFixture(t *testing.T, files map[string]string) *portNameFixture {
	t.Helper()
	c := qt.New(t)
	ctx := context.Background()
	dir, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	for name, body := range files {
		err = os.MkdirAll(filepath.Dir(filepath.Join(dir, name)), 0o700)
		c.Assert(err, qt.IsNil)
		err = os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600)
		c.Assert(err, qt.IsNil)
	}
	eng, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	f := &portNameFixture{t: t, eng: eng, dir: dir}
	f.eval(`(set-current-directory! "` + dir + `")`)
	return f
}

// installEcho writes a one-line sh script that echoes its arguments to
// dir/cmd/ and returns its absolute path, so a competing inc.scm can sit
// beside "the executable". (A copy of /bin/echo is killed by code signing on
// macOS.)
func (f *portNameFixture) installEcho() string {
	f.t.Helper()
	c := qt.New(f.t)
	dst := filepath.Join(f.dir, "cmd", "echo.sh")
	c.Assert(os.MkdirAll(filepath.Dir(dst), 0o700), qt.IsNil)
	c.Assert(os.WriteFile(dst, []byte("#!/bin/sh\necho \"$@\"\n"), 0o700), qt.IsNil)
	return dst
}

func (f *portNameFixture) eval(src string) values.Value {
	f.t.Helper()
	v, err := f.eng.EvalMultiple(context.Background(), src)
	qt.New(f.t).Assert(err, qt.IsNil)
	return v.Internal()
}

func (f *portNameFixture) evalString(src string) string {
	f.t.Helper()
	v := f.eval(src)
	str, ok := v.(*values.String)
	qt.New(f.t).Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
	return str.Value
}

func (f *portNameFixture) evalSymbol(src string) string {
	f.t.Helper()
	v := f.eval(src)
	sym, ok := v.(*values.Symbol)
	qt.New(f.t).Assert(ok, qt.IsTrue, qt.Commentf("got %T", v))
	return sym.Key
}

// I055-a1: read-syntax on a file port names the file, not #f.
func TestReadSyntaxFromFilePortNamesTheFile(t *testing.T) {
	f := newPortNameFixture(t, map[string]string{"f.scm": "(define x 1)\n  (foo bar)\n"})
	got := f.evalString(`(let ((p (open-input-file "f.scm")))
		   (read-syntax p)
		   (let ((s (read-syntax p)) (o (open-output-string)))
		     (write (list (syntax-source s) (syntax-line s) (syntax-column s)) o)
		     (get-output-string o)))`)
	qt.New(t).Assert(got, qt.Equals, `("`+filepath.Join(f.dir, "f.scm")+`" 2 2)`)
}

// I055-a2: a read error from a file port carries the file and the position in
// it, both in error-object-source and in the message text a program displays.
func TestReadErrorFromFilePortNamesTheFile(t *testing.T) {
	f := newPortNameFixture(t, map[string]string{"i.scm": "(define x 1)\n(foo ]\n"})
	got := f.evalString(`(guard (e (#t (error-object-source e)))
		   (let ((p (open-input-file "i.scm")))
		     (read p)
		     (read p)))`)
	loc := filepath.Join(f.dir, "i.scm") + ":2:5"
	qt.New(t).Assert(got, qt.Equals, loc)
	msg := f.evalString(`(guard (e (#t (error-object-message e)))
		   (let ((p (open-input-file "i.scm")))
		     (read p)
		     (read p)))`)
	qt.New(t).Assert(msg, qt.Contains, "(at "+loc+",")
}

// Every way of opening a file port names it with the engine-resolved absolute
// path, whatever spelling the program wrote: the reader stamps the name into
// SourceContext.File, which include resolution treats as a real path.
func TestEveryFileFactoryNamesItsPort(t *testing.T) {
	tests := []struct {
		name string
		src  string
		file string
	}{
		{"open-input-file", `(open-input-file "g.scm")`, "g.scm"},
		{"open-input-file non-canonical", `(open-input-file "./sub/../g.scm")`, "g.scm"},
		{"open-binary-input-file", `(open-binary-input-file "g.scm")`, "g.scm"},
		{"open-output-file", `(open-output-file "o1.txt")`, "o1.txt"},
		{"open-binary-output-file", `(open-binary-output-file "o2.bin")`, "o2.bin"},
		{"call-with-output-file", `(call-with-output-file "o3.txt" (lambda (p) p))`, "o3.txt"},
		{"call-with-input-file", `(call-with-input-file "g.scm" (lambda (p) p))`, "g.scm"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			f := newPortNameFixture(t, map[string]string{"g.scm": "(a b)\n", "sub/keep": ""})
			p, ok := f.eval(tc.src).(*values.PortObject)
			qt.New(t).Assert(ok, qt.IsTrue)
			// The name is cleaned and symlink-resolved: ".." and "." do not
			// survive into a name a program can read back.
			qt.New(t).Assert(p.PortName(), qt.Equals, filepath.Join(f.dir, tc.file))
		})
	}
}

func TestReadSyntaxThroughCurrentInputPortNamesTheFile(t *testing.T) {
	f := newPortNameFixture(t, map[string]string{"g.scm": "(a b)\n"})
	got := f.evalString(`(with-input-from-file "g.scm" (lambda () (syntax-source (read-syntax))))`)
	qt.New(t).Assert(got, qt.Equals, filepath.Join(f.dir, "g.scm"))
}

// String ports stay unnamed: Wile's syntax-source is a string or #f, so the
// literal "string" would be indistinguishable from a file named "string".
func TestStringPortStaysUnnamed(t *testing.T) {
	f := newPortNameFixture(t, nil)
	got := f.evalString(`(let ((o (open-output-string)))
		   (write (syntax-source (read-syntax (open-input-string "(a b)"))) o)
		   (get-output-string o))`)
	qt.New(t).Assert(got, qt.Equals, "#f")
}

// The port name feeds include resolution (lexicalIncludeContext reads
// SourceContext.File as a path). These pin that a form read from a port
// includes relative to where its file really is, and that a port that is not
// a file never steers an include.
func TestPortNameSteersIncludeConsistently(t *testing.T) {
	files := map[string]string{
		"inc.scm":           "(define y 'top)\n",
		"sub/inc.scm":       "(define y 'sub)\n",
		"sub/incl.scm":      "(include \"inc.scm\")\n",
		"other/sub/inc.scm": "(define y 'other)\n",
		"driver.scm": `(eval (read-syntax (open-input-file "sub/incl.scm")) (interaction-environment))
(set-box! driver-result (eval 'y (interaction-environment)))`,
		"cmd/inc.scm": "(define y 'cmd)\n",
	}

	t.Run("relative file port, no live load stack", func(t *testing.T) {
		f := newPortNameFixture(t, files)
		got := f.evalSymbol(`(eval (read-syntax (open-input-file "sub/incl.scm")) (interaction-environment))
			(eval 'y (interaction-environment))`)
		qt.New(t).Assert(got, qt.Equals, "sub")
	})

	t.Run("cwd moves after the port is opened", func(t *testing.T) {
		f := newPortNameFixture(t, files)
		got := f.evalSymbol(`(define p (open-input-file "sub/incl.scm"))
			(set-current-directory! "` + filepath.Join(f.dir, "other") + `")
			(eval (read-syntax p) (interaction-environment))
			(eval 'y (interaction-environment))`)
		qt.New(t).Assert(got, qt.Equals, "sub")
	})

	t.Run("relative file port under a live load stack", func(t *testing.T) {
		f := newPortNameFixture(t, files)
		f.eval(`(define driver-result (box #f))`)
		f.eval(`(load "driver.scm")`)
		qt.New(t).Assert(f.evalSymbol(`(unbox driver-result)`), qt.Equals, "sub")
	})

	t.Run("pipe from an absolute command includes against the cwd, with no load stack", func(t *testing.T) {
		if runtime.GOOS == "windows" {
			t.Skip("uses /bin/echo")
		}
		f := newPortNameFixture(t, files)
		echo := f.installEcho()
		got := f.evalSymbol(`(eval (read-syntax (process-stdout (process-spawn "` + echo + `" "(include \"inc.scm\")"))) (interaction-environment))
			(eval 'y (interaction-environment))`)
		qt.New(t).Assert(got, qt.Equals, "top")
	})

	t.Run("pipe from an absolute command under a live load stack", func(t *testing.T) {
		if runtime.GOOS == "windows" {
			t.Skip("uses /bin/echo")
		}
		f := newPortNameFixture(t, files)
		echo := f.installEcho()
		err := os.WriteFile(filepath.Join(f.dir, "pipe.scm"), []byte(`(eval (read-syntax (process-stdout (process-spawn "`+echo+`" "(include \"inc.scm\")"))) (interaction-environment))
(set-box! driver-result (eval 'y (interaction-environment)))`), 0o600)
		qt.New(t).Assert(err, qt.IsNil)
		f.eval(`(define driver-result (box #f))`)
		f.eval(`(load "pipe.scm")`)
		qt.New(t).Assert(f.evalSymbol(`(unbox driver-result)`), qt.Equals, "top")
	})
}

// A ..-climbing relative open must not leave the engine directory and its
// climb in the name: the name is readable by the program (syntax-source,
// error-object-source), so an uncleaned join would expose where the engine
// stands even when current-directory is denied.
func TestFilePortNameIsCleanedUnderAConfiningAuthorizer(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()
	root, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	err = os.WriteFile(filepath.Join(root, "f.scm"), []byte("(a b)\n(c ]\n"), 0o600)
	c.Assert(err, qt.IsNil)
	// The host stands outside the authorized region, in a directory the
	// program must not learn the name of. (current-directory) is denied there.
	outside, err := filepath.EvalSymlinks(t.TempDir())
	c.Assert(err, qt.IsNil)
	secret := filepath.Join(outside, "tenant-SECRET")
	c.Assert(os.MkdirAll(secret, 0o700), qt.IsNil)
	t.Chdir(secret)
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithAuthorizer(security.FilesystemRoot(root)))
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	written := strings.Repeat("../", 30) + strings.TrimPrefix(root, "/") + "/f.scm"
	want := filepath.Join(root, "f.scm")

	evalString := func(src string) string {
		v, err := eng.EvalMultiple(ctx, src)
		c.Assert(err, qt.IsNil)
		str, ok := v.Internal().(*values.String)
		c.Assert(ok, qt.IsTrue, qt.Commentf("got %T", v.Internal()))
		return str.Value
	}
	got := evalString(`(syntax-source (read-syntax (open-input-file "` + written + `")))`)
	c.Assert(got, qt.Equals, want)
	c.Assert(strings.Contains(got, "SECRET"), qt.IsFalse)
	c.Assert(evalString(`(guard (e (#t (error-object-source e)))
		(let ((p (open-input-file "`+written+`"))) (read p) (read p)))`), qt.Equals, want+":2:3")
}

// Include resolution reads the port name lexically, so "link/.." in a written
// name must not mis-steer it: the name is the real path, and an include inside
// the form resolves against the real file's directory.
func TestSymlinkedFilePortIncludesAgainstTheRealDirectory(t *testing.T) {
	f := newPortNameFixture(t, map[string]string{
		"real/deep/keep": "",
		"real/incl.scm":  "(include \"inc.scm\")\n",
		"real/inc.scm":   "(define y 'real)\n",
		"a/inc.scm":      "(define y 'decoy)\n",
	})
	err := os.Symlink(filepath.Join(f.dir, "real", "deep"), filepath.Join(f.dir, "a", "link"))
	qt.New(t).Assert(err, qt.IsNil)
	got := f.evalSymbol(`(eval (read-syntax (open-input-file "a/link/../incl.scm")) (interaction-environment))
		(eval 'y (interaction-environment))`)
	qt.New(t).Assert(got, qt.Equals, "real")
}
