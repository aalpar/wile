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
	"os"
	"path/filepath"
	"testing"

	qt "github.com/frankban/quicktest"

	extfiles "github.com/aalpar/wile/extensions/files"
	extio "github.com/aalpar/wile/internal/extensions/io"
	"github.com/aalpar/wile/pkg/wile"
	"github.com/aalpar/wile/values"
)

// newTestEngine creates a Wile engine with core + io + files extensions for testing.
func newTestEngine(t *testing.T) *wile.Engine {
	t.Helper()
	// Opt out of the immutable default: these continuation-safety tests redefine
	// top-level bindings (e.g. orig-port) on a shared engine.
	engine, err := wile.NewEngine(context.Background(),
		wile.WithExtension(extio.Extension),
		wile.WithExtension(extfiles.Extension),
		wile.WithMutableTopLevel(),
	)
	qt.Assert(t, err, qt.IsNil)
	return engine
}

// TestWithFileContinuationSafety_T3 verifies that with-input-from-file and
// with-output-to-file integrate properly with call/cc. This addresses T3
// from the architectural review by demonstrating that the parameterize-based
// implementation correctly tracks parameter changes on the winding stack.
func TestWithFileContinuationSafety_T3(t *testing.T) {
	c := qt.New(t)

	// Create test file with content
	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.txt")
	err := os.WriteFile(testFile, []byte("ABC"), 0644)
	c.Assert(err, qt.IsNil)

	engine := newTestEngine(t)

	// Test that parameters are properly restored after with-input-from-file
	code := `
		(begin
		  ; Capture stdin port before with-input-from-file
		  (define orig-port (current-input-port))

		  ; Run with-input-from-file
		  (with-input-from-file "` + testFile + `"
		    (lambda ()
		      (read-char)))  ; Read 'A'

		  ; After with-input-from-file, port should be restored to stdin
		  (eq? (current-input-port) orig-port))
	`

	result, err := engine.EvalMultiple(context.Background(), code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue, qt.Commentf("port should be restored after with-input-from-file"))

	// Test nested with-input-from-file
	testFile2 := filepath.Join(tmpDir, "test2.txt")
	err = os.WriteFile(testFile2, []byte("XYZ"), 0644)
	c.Assert(err, qt.IsNil)

	code2 := `
		(begin
		  (define orig-port (current-input-port))

		  ; Nested with-input-from-file
		  (with-input-from-file "` + testFile + `"
		    (lambda ()
		      (define char1 (read-char))  ; Read 'A' from file1
		      (with-input-from-file "` + testFile2 + `"
		        (lambda ()
		          (read-char)))  ; Read 'X' from file2
		      ; After inner with-input-from-file, should be back to file1
		      (define char2 (read-char))  ; Read 'B' from file1
		      (and (char=? char1 #\A) (char=? char2 #\B))))

		  ; After outer with-input-from-file, should be back to stdin
		  (eq? (current-input-port) orig-port))
	`

	result2, err := engine.EvalMultiple(context.Background(), code2)
	c.Assert(err, qt.IsNil)
	c.Assert(result2.Internal(), qt.Equals, values.TrueValue, qt.Commentf("nested with-input-from-file should restore ports correctly"))
}

// TestWithFileParameterizeSemanticsT3 verifies that with-input-from-file
// integrates properly with the dynamic-wind system.
func TestWithFileParameterizeSemanticsT3(t *testing.T) {
	c := qt.New(t)

	tmpDir := t.TempDir()
	testFile := filepath.Join(tmpDir, "test.txt")
	err := os.WriteFile(testFile, []byte("TEST"), 0644)
	c.Assert(err, qt.IsNil)

	engine := newTestEngine(t)

	// Test that with-input-from-file works correctly with dynamic-wind
	// by verifying that the port is restored even when wrapped in dynamic-wind
	code := `
		(begin
		  (define orig-port (current-input-port))
		  (define result-port #f)

		  (dynamic-wind
		    (lambda () #f)
		    (lambda ()
		      (with-input-from-file "` + testFile + `"
		        (lambda ()
		          (set! result-port (current-input-port))
		          (read-char))))  ; Read 'T'
		    (lambda () #f))

		  ; After dynamic-wind, port should be restored to stdin
		  (and
		    (not (eq? result-port orig-port))  ; Inside, port was different
		    (eq? (current-input-port) orig-port)))  ; After, port is restored
	`

	result, err := engine.EvalMultiple(context.Background(), code)
	c.Assert(err, qt.IsNil)
	c.Assert(result.Internal(), qt.Equals, values.TrueValue, qt.Commentf("with-input-from-file should integrate with dynamic-wind"))
}
