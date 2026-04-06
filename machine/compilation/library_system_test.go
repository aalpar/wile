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

package compilation_test

import (
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/registry/testhelpers"
	"github.com/aalpar/wile/values"
	"github.com/aalpar/wile/values/valuestest"
)

// TestLibrarySystemImport exercises the full import pipeline: file resolution,
// define-library parsing, compilation, execution, and binding installation.
//
// Source: library_loader.go (LoadLibrary, loadLibraryFromReader,
// compileAndExecuteLibrary), library_bindings.go (ApplyToExports,
// CopyLibraryBindingsToEnv), compile_import.go (CompileImport),
// compile_library_forms.go (CompileDefineLibrary),
// compile_time_continuation_library.go (compileLibraryBegin).
func TestLibrarySystemImport(t *testing.T) {
	tcs := []struct {
		name     string
		fs       fstest.MapFS
		code     string
		expected values.Value
	}{
		{
			name: "import library that defines and exports a function",
			fs: fstest.MapFS{
				"test/adder.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test adder)
  (export add-two)
  (import (scheme base))
  (begin (define (add-two x) (+ x 2))))`),
				},
			},
			code:     `(import (test adder)) (add-two 40)`,
			expected: values.NewInteger(42),
		},
		{
			name: "import library that re-exports a bootstrap binding",
			fs: fstest.MapFS{
				"test/alias.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test alias)
  (export my-add)
  (import (scheme base))
  (begin (define my-add +)))`),
				},
			},
			code:     `(import (test alias)) (my-add 10 20 12)`,
			expected: values.NewInteger(42),
		},
		{
			name: "import library with export rename",
			fs: fstest.MapFS{
				"test/renamed.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test renamed)
  (export (rename internal-name public-name))
  (import (scheme base))
  (begin (define (internal-name x) (* x x))))`),
				},
			},
			code:     `(import (test renamed)) (public-name 7)`,
			expected: values.NewInteger(49),
		},
		{
			name: "transitive imports: library A depends on library B",
			fs: fstest.MapFS{
				"test/base-lib.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test base-lib)
  (export base-val)
  (import (scheme base))
  (begin (define base-val 100)))`),
				},
				"test/derived.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test derived)
  (export derived-val)
  (import (scheme base))
  (import (test base-lib))
  (begin (define derived-val (+ base-val 11))))`),
				},
			},
			code:     `(import (test derived)) derived-val`,
			expected: values.NewInteger(111),
		},
		{
			name: "library with multiple begin blocks",
			fs: fstest.MapFS{
				"test/multi-begin.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test multi-begin)
  (export x y)
  (import (scheme base))
  (begin (define x 10))
  (begin (define y 20)))`),
				},
			},
			code:     `(import (test multi-begin)) (+ x y)`,
			expected: values.NewInteger(30),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.SetupEngineTest(t, tc.fs)
			result := testhelpers.EvalSchemeInEnv(t, env, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestLibrarySystemImportModifiers exercises the import set modifiers:
// only, except, prefix, and rename.
//
// Source: library_bindings.go (ApplyToExports), import_set_datum.go
// (ParseImportSetFromDatum).
func TestLibrarySystemImportModifiers(t *testing.T) {
	sharedFS := fstest.MapFS{
		"test/multi.sld": &fstest.MapFile{
			Data: []byte(`(define-library (test multi)
  (export alpha beta gamma)
  (import (scheme base))
  (begin
    (define alpha 1)
    (define beta 2)
    (define gamma 3)))`),
		},
	}

	tcs := []struct {
		name     string
		code     string
		expected values.Value
	}{
		{
			name:     "only imports selected binding",
			code:     `(import (only (test multi) alpha)) alpha`,
			expected: values.NewInteger(1),
		},
		{
			name:     "except excludes specified binding",
			code:     `(import (except (test multi) gamma)) (+ alpha beta)`,
			expected: values.NewInteger(3),
		},
		{
			name:     "prefix adds prefix to all names",
			code:     `(import (prefix (test multi) t:)) (+ t:alpha t:beta t:gamma)`,
			expected: values.NewInteger(6),
		},
		{
			name:     "rename renames specified binding",
			code:     `(import (rename (test multi) (alpha a))) (+ a beta gamma)`,
			expected: values.NewInteger(6),
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.SetupEngineTest(t, sharedFS)
			result := testhelpers.EvalSchemeInEnv(t, env, tc.code)
			qt.Assert(t, result, valuestest.SchemeEquals, tc.expected)
		})
	}
}

// TestLibrarySystemErrors exercises error paths in the library loading pipeline.
//
// Source: library_loader.go (LoadLibrary — missing file, circular dependency,
// malformed library, name mismatch).
func TestLibrarySystemErrors(t *testing.T) {
	tcs := []struct {
		name string
		fs   fstest.MapFS
		code string
	}{
		{
			name: "missing library file",
			fs:   fstest.MapFS{},
			code: `(import (test nonexistent))`,
		},
		{
			name: "circular dependency",
			fs: fstest.MapFS{
				"test/circle-a.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test circle-a)
  (export a-val)
  (import (scheme base))
  (import (test circle-b))
  (begin (define a-val 1)))`),
				},
				"test/circle-b.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test circle-b)
  (export b-val)
  (import (scheme base))
  (import (test circle-a))
  (begin (define b-val 2)))`),
				},
			},
			code: `(import (test circle-a))`,
		},
		{
			name: "malformed library file",
			fs: fstest.MapFS{
				"test/bad.sld": &fstest.MapFile{
					Data: []byte(`not-a-library-form`),
				},
			},
			code: `(import (test bad))`,
		},
		{
			name: "library name mismatch",
			fs: fstest.MapFS{
				"test/mismatch.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test wrong-name)
  (export x)
  (import (scheme base))
  (begin (define x 1)))`),
				},
			},
			code: `(import (test mismatch))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.SetupEngineTest(t, tc.fs)

			// Some library loading errors may manifest as panics from the VM.
			// Use a recover wrapper to catch both returned errors and panics.
			var err error
			func() {
				defer func() {
					r := recover()
					if r != nil {
						// Panic counts as an error for our purposes.
						if e, ok := r.(error); ok {
							err = e
						} else {
							t.Logf("recovered panic: %v", r)
							err = qt.BadCheckf("panic: %v", r)
						}
					}
				}()
				_, err = testhelpers.EvalSchemeInEnvMayFail(t, env, tc.code)
			}()

			qt.Assert(t, err, qt.IsNotNil, qt.Commentf("expected error for %s", tc.name))
		})
	}
}
