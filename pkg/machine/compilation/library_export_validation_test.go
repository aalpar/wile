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
	"errors"
	"strings"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/werr"
)

// TestLibraryExportValidation pins R7RS §5.6 Task 5D: a (define-library ...) that
// exports a name it never defines or imports must error at definition time (when
// the library is loaded/compiled), with the diagnostic naming the missing identifier
// and the library — NOT lazily at the per-name import site.
func TestLibraryExportValidation(t *testing.T) {
	tcs := []struct {
		name        string
		fs          fstest.MapFS
		code        string
		wantErr     bool
		mustContain []string // substrings the error message must include (when wantErr)
	}{
		{
			// Exports both a defined name and an undefined one. The library must
			// fail to load, naming the missing name (not the defined one), even
			// though the import only references the defined name.
			name: "export-of-undefined-name-errors-eagerly",
			fs: fstest.MapFS{
				"test/bad-export.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test bad-export)
  (export defined-name undefined-name)
  (import (scheme base))
  (begin (define defined-name 1)))`),
				},
			},
			code:    `(import (only (test bad-export) defined-name)) defined-name`,
			wantErr: true,
			// The diagnostic names the missing identifier, the library, and BOTH causes
			// of an export gap — a typo, or a primitive the active security profile does
			// not register (the latter is the real cause when e.g. (scheme base) loads
			// under the Tiny profile). See validateLibraryExports.
			mustContain: []string{"undefined-name", "(test bad-export)", "security profile does not register"},
		},
		{
			// Two undefined exports: the error collects ALL missing names in one report.
			name: "export-collects-all-missing-names",
			fs: fstest.MapFS{
				"test/multi-missing.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test multi-missing)
  (export ok-one missing-a missing-b)
  (import (scheme base))
  (begin (define ok-one 1)))`),
				},
			},
			code:        `(import (test multi-missing))`,
			wantErr:     true,
			mustContain: []string{"missing-a", "missing-b"},
		},
		{
			// An export that names an IMPORTED binding (not locally defined) is valid:
			// car is imported from (scheme base).
			name: "export-of-imported-name-is-valid",
			fs: fstest.MapFS{
				"test/reexport.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test reexport)
  (export my-car)
  (import (scheme base))
  (export (rename car my-car)))`),
				},
			},
			code:    `(import (test reexport)) (my-car '(7 8 9))`,
			wantErr: false,
		},
		{
			// Sanity: a fully-defined export list loads and works.
			name: "all-exports-defined-is-valid",
			fs: fstest.MapFS{
				"test/good.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test good)
  (export a b)
  (import (scheme base))
  (begin (define a 1) (define b 2)))`),
				},
			},
			code:    `(import (test good)) (+ a b)`,
			wantErr: false,
		},
	}

	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			env := testhelpers.SetupEngineTest(t, tc.fs)
			_, err := testhelpers.EvalSchemeInEnvMayFail(t, env, tc.code)
			if !tc.wantErr {
				qt.Assert(t, err, qt.IsNil)
				return
			}
			qt.Assert(t, err, qt.IsNotNil)
			// The export gap must surface as ErrUnexportedIdentifier (the name is
			// declared exported but resolves to no binding) or ErrNoSuchBinding.
			isExportErr := errors.Is(err, werr.ErrUnexportedIdentifier) || errors.Is(err, werr.ErrNoSuchBinding)
			qt.Assert(t, isExportErr, qt.IsTrue,
				qt.Commentf("want ErrUnexportedIdentifier/ErrNoSuchBinding, got %v", err))
			for _, sub := range tc.mustContain {
				qt.Assert(t, strings.Contains(err.Error(), sub), qt.IsTrue,
					qt.Commentf("error %q must contain %q", err.Error(), sub))
			}
		})
	}
}
