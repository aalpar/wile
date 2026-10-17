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

package files_test

import (
	"context"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"syscall"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/values"
)

func TestCreateDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("creates directory", func(t *testing.T) {
		path := filepath.Join(dir, "newdir")
		eval(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
		info, err := os.Stat(path)
		c.Assert(err, qt.IsNil)
		c.Assert(info.IsDir(), qt.IsTrue)
	})

	t.Run("error if already exists", func(t *testing.T) {
		path := filepath.Join(dir, "existing")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		evalExpectError(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
	})

	t.Run("error if parent missing", func(t *testing.T) {
		path := filepath.Join(dir, "no", "parent")
		evalExpectError(t, engine, fmt.Sprintf(`(create-directory %q)`, path))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(create-directory 42)`)
	})
}

func TestDeleteDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("deletes empty directory", func(t *testing.T) {
		path := filepath.Join(dir, "rmme")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		eval(t, engine, fmt.Sprintf(`(delete-directory %q)`, path))
		_, err = os.Stat(path)
		c.Assert(os.IsNotExist(err), qt.IsTrue)
	})

	t.Run("error if not empty", func(t *testing.T) {
		path := filepath.Join(dir, "notempty")
		err := os.Mkdir(path, 0o755)
		c.Assert(err, qt.IsNil)
		writeTestFile(t, path, "child.txt", "data")
		evalExpectError(t, engine, fmt.Sprintf(`(delete-directory %q)`, path))
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(delete-directory %q)`, filepath.Join(dir, "nope")))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(delete-directory 42)`)
	})
}

func TestDirectoryFiles(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)
	dir := t.TempDir()

	t.Run("lists files", func(t *testing.T) {
		writeTestFile(t, dir, "a.txt", "a")
		writeTestFile(t, dir, "b.txt", "b")
		result := eval(t, engine, fmt.Sprintf(`(directory-files %q)`, dir))

		var names []string
		list := result.Internal().(values.Tuple)
		list.ForEach(context.Background(), func(_ context.Context, _ int, _ bool, v values.Value) error {
			names = append(names, v.(*values.String).Value)
			return nil
		})
		slices.Sort(names)
		c.Assert(names, qt.Contains, "a.txt")
		c.Assert(names, qt.Contains, "b.txt")
	})

	t.Run("excludes dot entries", func(t *testing.T) {
		result := eval(t, engine, fmt.Sprintf(`
			(let loop ((fs (directory-files %q)) (ok #t))
			  (if (null? fs)
			      ok
			      (loop (cdr fs)
			            (and ok
			                 (not (string=? (car fs) "."))
			                 (not (string=? (car fs) ".."))))))
		`, dir))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("returns strings", func(t *testing.T) {
		result := eval(t, engine, fmt.Sprintf(`
			(let loop ((fs (directory-files %q)) (ok #t))
			  (if (null? fs)
			      ok
			      (loop (cdr fs) (and ok (string? (car fs))))))
		`, dir))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("empty directory", func(t *testing.T) {
		empty := t.TempDir()
		result := eval(t, engine, fmt.Sprintf(`(null? (directory-files %q))`, empty))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, fmt.Sprintf(`(directory-files %q)`, filepath.Join(dir, "nope")))
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(directory-files 42)`)
	})
}

func TestCurrentDirectory(t *testing.T) {
	c := qt.New(t)
	engine := newEngine(t)

	t.Run("returns a string", func(t *testing.T) {
		result := eval(t, engine, `(string? (current-directory))`)
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})

	t.Run("matches os.Getwd", func(t *testing.T) {
		wd, err := os.Getwd()
		c.Assert(err, qt.IsNil)
		result := eval(t, engine, fmt.Sprintf(
			`(string=? (current-directory) %q)`, wd))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
	})
}

func TestSetCurrentDirectory(t *testing.T) {
	engine := newEngine(t)
	c := qt.New(t)

	hostBefore, err := os.Getwd()
	c.Assert(err, qt.IsNil)

	t.Run("changes the engine directory, not the host's", func(t *testing.T) {
		target := t.TempDir()
		eval(t, engine, fmt.Sprintf(`(set-current-directory! %q)`, target))
		resolvedTarget, err := filepath.EvalSymlinks(target)
		c.Assert(err, qt.IsNil)
		result := eval(t, engine, fmt.Sprintf(
			`(string=? (current-directory) %q)`, resolvedTarget))
		c.Assert(result.Internal(), qt.Equals, values.TrueValue)
		hostAfter, err := os.Getwd()
		c.Assert(err, qt.IsNil)
		c.Assert(hostAfter, qt.Equals, hostBefore)
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! "/nonexistent/path/12345")`)
	})

	// "" names no file. filepath.EvalSymlinks reads it as ".", the host's
	// directory, which once left the engine holding the relative path ".".
	t.Run("empty path is refused", func(t *testing.T) {
		eval(t, engine, fmt.Sprintf(`(set-current-directory! %q)`, t.TempDir()))
		before := eval(t, engine, `(current-directory)`)
		_, err := engine.EvalMultiple(context.Background(), `(set-current-directory! "")`)
		c.Assert(err, qt.ErrorIs, fs.ErrNotExist)
		after := eval(t, engine, `(current-directory)`)
		c.Assert(after.SchemeString(), qt.Equals, before.SchemeString())
		_, err = engine.EvalMultiple(context.Background(), `(directory-files "")`)
		c.Assert(err, qt.ErrorIs, fs.ErrNotExist)
		_, err = engine.EvalMultiple(context.Background(), `(open-input-file "")`)
		c.Assert(err, qt.ErrorIs, fs.ErrNotExist)
		result := eval(t, engine, `(file-exists? "")`)
		c.Assert(result.Internal(), qt.Equals, values.FalseValue)
	})

	t.Run("error if not a directory", func(t *testing.T) {
		file := filepath.Join(t.TempDir(), "f")
		c.Assert(os.WriteFile(file, nil, 0o644), qt.IsNil)
		before := eval(t, engine, `(current-directory)`)
		_, err := engine.EvalMultiple(context.Background(),
			fmt.Sprintf(`(set-current-directory! %q)`, file))
		c.Assert(err, qt.ErrorIs, syscall.ENOTDIR)
		after := eval(t, engine, `(current-directory)`)
		c.Assert(after.SchemeString(), qt.Equals, before.SchemeString())
	})

	// chdir requires search (x) permission on the target itself, which a stat
	// of the target does not: stat needs x only on the parents. Read alone is
	// not enough, and x alone is.
	t.Run("refuses a directory it cannot search", func(t *testing.T) {
		if runtime.GOOS == "windows" || os.Geteuid() == 0 {
			t.Skip("POSIX permission bits; root bypasses them")
		}
		for _, tc := range []struct {
			mode   os.FileMode
			enters bool
		}{
			{0o000, false},
			{0o400, false},
			{0o100, true},
		} {
			dir := filepath.Join(t.TempDir(), "d")
			c.Assert(os.Mkdir(dir, 0o755), qt.IsNil)
			c.Assert(os.Chmod(dir, tc.mode), qt.IsNil)
			t.Cleanup(func() {
				_ = os.Chmod(dir, 0o755)
			})
			before := eval(t, engine, `(current-directory)`)
			_, err := engine.EvalMultiple(context.Background(),
				fmt.Sprintf(`(set-current-directory! %q)`, dir))
			after := eval(t, engine, `(current-directory)`)
			if tc.enters {
				c.Assert(err, qt.IsNil, qt.Commentf("mode %o", tc.mode))
				c.Assert(after.SchemeString(), qt.Not(qt.Equals), before.SchemeString())
				continue
			}
			c.Assert(err, qt.ErrorIs, fs.ErrPermission, qt.Commentf("mode %o", tc.mode))
			c.Assert(after.SchemeString(), qt.Equals, before.SchemeString())
		}
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! 42)`)
	})
}
