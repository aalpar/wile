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

package eval_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile"
	exteval "github.com/aalpar/wile/internal/extensions/eval"
	"github.com/aalpar/wile/machine"
)

// Helper functions

func newTestEngine(t *testing.T) *wile.Engine {
	t.Helper()
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

func evalCode(t *testing.T, engine *wile.Engine, code string) wile.Value {
	t.Helper()
	result, err := engine.Eval(context.Background(), code)
	qt.Assert(t, err, qt.IsNil)
	return result
}

// Integration tests

func TestLoadPathStack_RelativeLoadResolution(t *testing.T) {
	c := qt.New(t)

	// Create temporary directory structure:
	// tmpDir/
	//   main.scm        → (load "sub/helper.scm")
	//   sub/
	//     helper.scm    → (load "../util.scm")
	//   util.scm
	tmpDir, err := os.MkdirTemp("", "load-path-test-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	subDir := filepath.Join(tmpDir, "sub")
	c.Assert(os.Mkdir(subDir, 0755), qt.IsNil)

	// util.scm - defines a variable
	utilFile := filepath.Join(tmpDir, "util.scm")
	utilCode := `(define util-value 42)`
	c.Assert(os.WriteFile(utilFile, []byte(utilCode), 0644), qt.IsNil)

	// sub/helper.scm - loads util.scm via relative path
	helperFile := filepath.Join(subDir, "helper.scm")
	helperCode := `(load "../util.scm")`
	c.Assert(os.WriteFile(helperFile, []byte(helperCode), 0644), qt.IsNil)

	// main.scm - loads helper.scm and returns util-value
	mainFile := filepath.Join(tmpDir, "main.scm")
	mainCode := `(load "sub/helper.scm") util-value`
	c.Assert(os.WriteFile(mainFile, []byte(mainCode), 0644), qt.IsNil)

	// Load main.scm - should resolve nested relative paths correctly
	engine := newTestEngine(t)

	// Change to a different directory to ensure relative resolution
	// uses the file's directory, not CWD
	oldCwd, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	defer os.Chdir(oldCwd) //nolint:errcheck

	tempCwd, err := os.MkdirTemp("", "load-path-cwd-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tempCwd)
	c.Assert(os.Chdir(tempCwd), qt.IsNil)

	result := evalCode(t, engine, fmt.Sprintf(`(load %q)`, mainFile))
	c.Assert(result.SchemeString(), qt.Equals, "42")
}

func TestLoadPathStack_CurrentLoadPath(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "load-path-current-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	subDir := filepath.Join(tmpDir, "sub")
	c.Assert(os.Mkdir(subDir, 0755), qt.IsNil)

	// sub/nested.scm - returns (current-load-path) and (current-load-directory)
	nestedFile := filepath.Join(subDir, "nested.scm")
	nestedCode := `(list (current-load-path) (current-load-directory))`
	c.Assert(os.WriteFile(nestedFile, []byte(nestedCode), 0644), qt.IsNil)

	// main.scm - loads nested.scm and returns its result
	mainFile := filepath.Join(tmpDir, "main.scm")
	mainCode := `(load "sub/nested.scm")`
	c.Assert(os.WriteFile(mainFile, []byte(mainCode), 0644), qt.IsNil)

	engine := newTestEngine(t)

	result := evalCode(t, engine, fmt.Sprintf(`(load %q)`, mainFile))

	// Result should be (list "/path/to/sub/nested.scm" "/path/to/sub")
	resultStr := result.SchemeString()
	c.Assert(resultStr, qt.Contains, "nested.scm")
	c.Assert(resultStr, qt.Contains, "sub")
}

func TestLoadPathStack_EmptyInREPL(t *testing.T) {
	c := qt.New(t)

	engine := newTestEngine(t)

	// (current-load-path) should return #f in REPL
	result := evalCode(t, engine, "(current-load-path)")
	c.Assert(result.SchemeString(), qt.Equals, "#f")

	// (current-load-directory) should return #f in REPL
	result = evalCode(t, engine, "(current-load-directory)")
	c.Assert(result.SchemeString(), qt.Equals, "#f")

	// (current-load-depth) should return 0 in REPL (stack exists but is empty)
	result = evalCode(t, engine, "(current-load-depth)")
	c.Assert(result.SchemeString(), qt.Equals, "0")
}

func TestLoadPathStack_DepthReturnsToZero(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "load-path-depth-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	// test.scm - just a simple expression
	testFile := filepath.Join(tmpDir, "test.scm")
	c.Assert(os.WriteFile(testFile, []byte("(+ 1 2)"), 0644), qt.IsNil)

	engine := newTestEngine(t)

	// Verify stack starts empty
	c.Assert(engine.CurrentLoadPath(), qt.Equals, "")

	evalCode(t, engine, fmt.Sprintf(`(load %q)`, testFile))

	// Verify stack returns to empty after load completes
	c.Assert(engine.CurrentLoadPath(), qt.Equals, "")
}

func TestLoadPathStack_NestedLoadPaths(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "load-path-nested-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	subDir := filepath.Join(tmpDir, "sub")
	c.Assert(os.Mkdir(subDir, 0755), qt.IsNil)

	// level2.scm - captures current-load-path and depth
	level2File := filepath.Join(subDir, "level2.scm")
	level2Code := `(list (current-load-path) (current-load-depth))`
	c.Assert(os.WriteFile(level2File, []byte(level2Code), 0644), qt.IsNil)

	// level1.scm - captures current-load-path and depth, then loads level2
	level1File := filepath.Join(tmpDir, "level1.scm")
	level1Code := `
		(define level1-path (current-load-path))
		(define level1-depth (current-load-depth))
		(define level2-info (load "sub/level2.scm"))
		(list level1-path level1-depth level2-info)
	`
	c.Assert(os.WriteFile(level1File, []byte(level1Code), 0644), qt.IsNil)

	engine := newTestEngine(t)

	result := evalCode(t, engine, fmt.Sprintf(`(load %q)`, level1File))

	// Should return (level1-path 1 (level2-path 2))
	resultStr := result.SchemeString()
	c.Assert(resultStr, qt.Contains, "level1.scm")
	c.Assert(resultStr, qt.Contains, "level2.scm")
	c.Assert(resultStr, qt.Contains, " 1 ") // level1-depth
	c.Assert(resultStr, qt.Contains, " 2)") // level2-depth at end of nested list
}

func TestLoadPathStack_ErrorIncludesSearchPaths(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "load-path-error-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	// main.scm - tries to load a nonexistent file
	mainFile := filepath.Join(tmpDir, "main.scm")
	mainCode := `(load "nonexistent.scm")`
	c.Assert(os.WriteFile(mainFile, []byte(mainCode), 0644), qt.IsNil)

	engine := newTestEngine(t)

	_, err = engine.Eval(context.Background(), fmt.Sprintf(`(load %q)`, mainFile))
	c.Assert(err, qt.Not(qt.IsNil))

	// Error message should include searched paths
	errMsg := err.Error()
	c.Assert(errMsg, qt.Contains, "nonexistent.scm")
	c.Assert(errMsg, qt.Contains, "not found")
	c.Assert(errMsg, qt.Contains, "searched:")
	c.Assert(errMsg, qt.Contains, tmpDir) // Should list the file's directory
}

func TestLoadPathStack_WithLoadPathAPI(t *testing.T) {
	c := qt.New(t)

	tmpDir, err := os.MkdirTemp("", "load-path-api-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(tmpDir)

	testFile := filepath.Join(tmpDir, "test.scm")
	c.Assert(os.WriteFile(testFile, []byte("(define x 1)"), 0644), qt.IsNil)

	helperFile := filepath.Join(tmpDir, "helper.scm")
	helperCode := `(load "test.scm") x`
	c.Assert(os.WriteFile(helperFile, []byte(helperCode), 0644), qt.IsNil)

	engine := newTestEngine(t)

	// Use WithLoadPath to evaluate code in a specific file context
	var result string
	err = engine.WithLoadPath(helperFile, func() error {
		// Load the file in this context
		_, evalErr := engine.Eval(context.Background(), `(load "test.scm")`)
		if evalErr != nil {
			return evalErr
		}
		// Now evaluate x
		val, evalErr := engine.Eval(context.Background(), `x`)
		if evalErr != nil {
			return evalErr
		}
		result = val.SchemeString()
		return nil
	})

	c.Assert(err, qt.IsNil)
	c.Assert(result, qt.Equals, "1")

	// Verify stack is empty after WithLoadPath completes
	c.Assert(engine.CurrentLoadPath(), qt.Equals, "")
}

// Tests for unified load/include resolution (FileResolver).
// Before unification, load used only LoadPathStack+CWD.
// After unification, load shares OSFileResolver with include,
// so it also searches SCHEME_INCLUDE_PATH and library registry paths.

func TestLoad_ResolvesViaSchemeIncludePath(t *testing.T) {
	c := qt.New(t)

	// Create a file in a directory that is NOT the CWD and NOT on
	// the load path stack — only reachable via SCHEME_INCLUDE_PATH.
	libDir, err := os.MkdirTemp("", "load-include-path-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(libDir)

	helperFile := filepath.Join(libDir, "include-helper.scm")
	c.Assert(os.WriteFile(helperFile, []byte("(define include-helper-val 99)"), 0644), qt.IsNil)

	// Set SCHEME_INCLUDE_PATH to the directory containing the file.
	t.Setenv(machine.SchemeIncludePathEnv, libDir)

	// Move CWD somewhere else so it can't accidentally find the file.
	otherDir, err := os.MkdirTemp("", "load-include-path-cwd-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(otherDir)

	oldCwd, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	defer os.Chdir(oldCwd) //nolint:errcheck
	c.Assert(os.Chdir(otherDir), qt.IsNil)

	engine := newTestEngine(t)

	// (load "include-helper.scm") should find it via SCHEME_INCLUDE_PATH.
	evalCode(t, engine, `(load "include-helper.scm")`)
	result := evalCode(t, engine, `include-helper-val`)
	c.Assert(result.SchemeString(), qt.Equals, "99")
}

func TestLoad_ResolvesViaLibrarySearchPaths(t *testing.T) {
	c := qt.New(t)

	// Create a file reachable only via library search paths.
	libDir, err := os.MkdirTemp("", "load-lib-path-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(libDir)

	helperFile := filepath.Join(libDir, "lib-helper.scm")
	c.Assert(os.WriteFile(helperFile, []byte("(define lib-helper-val 77)"), 0644), qt.IsNil)

	// Clear SCHEME_INCLUDE_PATH so only library paths apply.
	t.Setenv(machine.SchemeIncludePathEnv, "")

	// Move CWD somewhere else.
	otherDir, err := os.MkdirTemp("", "load-lib-path-cwd-")
	c.Assert(err, qt.IsNil)
	defer os.RemoveAll(otherDir)

	oldCwd, err := os.Getwd()
	c.Assert(err, qt.IsNil)
	defer os.Chdir(oldCwd) //nolint:errcheck
	c.Assert(os.Chdir(otherDir), qt.IsNil)

	// Create engine with library paths pointing to our directory.
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(exteval.Extension),
		wile.WithLibraryPaths(libDir),
	)
	c.Assert(err, qt.IsNil)

	// (load "lib-helper.scm") should find it via library search paths.
	evalCode(t, engine, `(load "lib-helper.scm")`)
	result := evalCode(t, engine, `lib-helper-val`)
	c.Assert(result.SchemeString(), qt.Equals, "77")
}
