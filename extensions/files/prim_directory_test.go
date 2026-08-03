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
	"os"
	"path/filepath"
	"slices"
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

	origDir, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	t.Cleanup(func() {
		os.Chdir(origDir) //nolint:errcheck
	})

	t.Run("changes directory", func(t *testing.T) {
		target := t.TempDir()
		eval(t, engine, fmt.Sprintf(`(set-current-directory! %q)`, target))
		wd, err := os.Getwd()
		c.Assert(err, qt.IsNil)
		resolvedTarget, _ := filepath.EvalSymlinks(target)
		resolvedWd, _ := filepath.EvalSymlinks(wd)
		c.Assert(resolvedWd, qt.Equals, resolvedTarget)
	})

	t.Run("error if nonexistent", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! "/nonexistent/path/12345")`)
	})

	t.Run("wrong type", func(t *testing.T) {
		evalExpectError(t, engine, `(set-current-directory! 42)`)
	})
}
