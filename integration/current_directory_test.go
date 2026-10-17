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

package integration_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/wile"
)

func newCurrentDirectoryEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(), wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = engine.Close()
	})
	return engine
}

func evalString(t *testing.T, engine *wile.Engine, src string) string {
	t.Helper()
	v, err := engine.EvalMultiple(context.Background(), src)
	qt.Assert(t, err, qt.IsNil, qt.Commentf("eval %s", src))
	return v.SchemeString()
}

// TestSetCurrentDirectoryDoesNotMoveTheHost pins I186-R4: the current directory
// is engine state. Engine A entering a directory moves neither the host process
// nor engine B, two engines that share nothing but the process.
func TestSetCurrentDirectoryDoesNotMoveTheHost(t *testing.T) {
	t.Chdir(t.TempDir())
	hostBefore, err := os.Getwd()
	qt.Assert(t, err, qt.IsNil)

	a := newCurrentDirectoryEngine(t)
	b := newCurrentDirectoryEngine(t)
	bBefore := evalString(t, b, `(current-directory)`)

	target, err := filepath.EvalSymlinks(t.TempDir())
	qt.Assert(t, err, qt.IsNil)
	evalString(t, a, fmt.Sprintf(`(set-current-directory! %q)`, target))

	hostAfter, err := os.Getwd()
	qt.Assert(t, err, qt.IsNil)
	qt.Check(t, hostAfter, qt.Equals, hostBefore, qt.Commentf("HOST MOVED"))
	qt.Check(t, evalString(t, b, `(current-directory)`), qt.Equals, bBefore, qt.Commentf("ENGINE B MOVED"))
	qt.Check(t, evalString(t, a, `(current-directory)`), qt.Equals, fmt.Sprintf("%q", target))
}

// TestRelativePathsFollowTheEngineDirectory pins that every relative path an
// engine resolves after set-current-directory! resolves against the ENGINE's
// directory, not the host's. Each probe has a decoy of the same name in the
// host's directory, so a reader that still consults the process cwd answers
// "host" instead of failing, which is the silent shape this regression takes.
// load is the sharpest case: the library registry's default "." search entry is
// ahead of the current-directory entry.
func TestRelativePathsFollowTheEngineDirectory(t *testing.T) {
	host := t.TempDir()
	t.Chdir(host)
	sub := filepath.Join(host, "sub")
	qt.Assert(t, os.Mkdir(sub, 0o755), qt.IsNil)
	for dir, tag := range map[string]string{host: "host", sub: "sub"} {
		qt.Assert(t, os.WriteFile(filepath.Join(dir, "x.scm"),
			[]byte(fmt.Sprintf("(define which-x '%s)\n", tag)), 0o644), qt.IsNil)
		qt.Assert(t, os.WriteFile(filepath.Join(dir, "data.txt"),
			[]byte(tag), 0o644), qt.IsNil)
		qt.Assert(t, os.Mkdir(filepath.Join(dir, "only-in-"+tag), 0o755), qt.IsNil)
	}

	engine := newCurrentDirectoryEngine(t)
	evalString(t, engine, `(set-current-directory! "sub")`)

	qt.Check(t, evalString(t, engine, `(load "x.scm") which-x`), qt.Equals, "sub")
	qt.Check(t, evalString(t, engine,
		`(call-with-input-file "data.txt" read-line)`), qt.Equals, `"sub"`)
	qt.Check(t, evalString(t, engine,
		`(let ((p (open-input-file "data.txt"))) (read-line p))`), qt.Equals, `"sub"`)
	qt.Check(t, evalString(t, engine, `(file-exists? "only-in-sub")`), qt.Equals, "#t")
	qt.Check(t, evalString(t, engine, `(file-exists? "only-in-host")`), qt.Equals, "#f")
	qt.Check(t, evalString(t, engine,
		`(member "only-in-sub" (directory-files "."))`), qt.Not(qt.Equals), "#f")

	if runtime.GOOS != "windows" {
		// A spawned child starts in the engine's directory: the host never
		// moved, so the child would not inherit it.
		qt.Check(t, evalString(t, engine, `(system "test -d only-in-sub")`), qt.Equals, "0")
	}

	evalString(t, engine, `(create-directory "made")`)
	_, err := os.Stat(filepath.Join(sub, "made"))
	qt.Check(t, err, qt.IsNil, qt.Commentf("create-directory resolved outside the engine directory"))
	evalString(t, engine, `(delete-directory "made")`)

	// A relative set-current-directory! is itself relative to the engine.
	evalString(t, engine, `(set-current-directory! "..")`)
	qt.Check(t, evalString(t, engine, `(file-exists? "only-in-host")`), qt.Equals, "#t")
}

// TestEmptySearchEntryFollowsTheEngineDirectory pins that an EMPTY search-path
// entry means the engine's current directory, as "." does. filepath.SplitList
// keeps the empty entry a stray colon leaves in SCHEME_INCLUDE_PATH, and an
// embedder can pass "" to WithLibraryPaths; resolved as-is it cleaned to a
// relative ".", which the stat and the authorizer both read against the host's
// directory. load then took the host's file without error.
func TestEmptySearchEntryFollowsTheEngineDirectory(t *testing.T) {
	host := t.TempDir()
	t.Chdir(host)
	sub := filepath.Join(host, "sub")
	qt.Assert(t, os.Mkdir(sub, 0o755), qt.IsNil)
	qt.Assert(t, os.WriteFile(filepath.Join(host, "only-host.scm"),
		[]byte("(define which 'host)\n"), 0o644), qt.IsNil)
	for dir, tag := range map[string]string{host: "host", sub: "sub"} {
		qt.Assert(t, os.WriteFile(filepath.Join(dir, "x.scm"),
			[]byte(fmt.Sprintf("(define which-x '%s)\n", tag)), 0o644), qt.IsNil)
	}

	t.Run("SCHEME_INCLUDE_PATH trailing colon", func(t *testing.T) {
		t.Setenv("SCHEME_INCLUDE_PATH", "/nonexistent"+string(os.PathListSeparator))
		engine := newCurrentDirectoryEngine(t)
		evalString(t, engine, `(set-current-directory! "sub")`)
		_, err := engine.EvalMultiple(context.Background(), `(load "only-host.scm")`)
		qt.Check(t, err, qt.IsNotNil, qt.Commentf("load found the host's file through the empty entry"))
	})

	t.Run("WithLibraryPaths empty entry", func(t *testing.T) {
		engine, err := wile.NewEngine(context.Background(),
			wile.WithProfile(wile.KitchenSink), wile.WithLibraryPaths(""))
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = engine.Close()
		})
		evalString(t, engine, `(set-current-directory! "sub")`)
		qt.Check(t, evalString(t, engine, `(load "x.scm") which-x`), qt.Equals, "sub")
		_, err = engine.EvalMultiple(context.Background(), `(load "only-host.scm")`)
		qt.Check(t, err, qt.IsNotNil, qt.Commentf("load found the host's file through the empty entry"))
	})
}
