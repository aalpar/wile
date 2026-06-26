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

	"github.com/aalpar/wile/pkg/registry/testhelpers"
	"github.com/aalpar/wile/pkg/values"
	"github.com/aalpar/wile/pkg/values/valuestest"
)

// TestIncludeCi pins R7RS §4.1.7 / §5.6 Task 5E: include-ci splices the referenced
// file with case folding enabled, so identifiers in the included file are read
// case-insensitively (folded to lowercase). The includer (parsed normally) can then
// reference a folded identifier in lowercase even though the file wrote it uppercase.
func TestIncludeCi(t *testing.T) {
	tcs := []struct {
		name     string
		fs       fstest.MapFS
		code     string
		expected values.Value
	}{
		{
			// Top-level include-ci: file defines FOO; includer references foo.
			name: "top-level include-ci folds identifiers",
			fs: fstest.MapFS{
				"ci/defs.scm": &fstest.MapFile{
					Data: []byte(`(define FOO 42)
(define (Square X) (* X X))`),
				},
			},
			code:     `(import (scheme base)) (include-ci "ci/defs.scm") (+ foo (square 5))`,
			expected: values.NewInteger(67),
		},
		{
			// Library-level include-ci: the library body includes a case-folded file and
			// exports the (lowercased) name it defines.
			name: "library include-ci folds and exports",
			fs: fstest.MapFS{
				"ci/lib-defs.scm": &fstest.MapFile{
					Data: []byte(`(define ANSWER 42)`),
				},
				"test/ci-lib.sld": &fstest.MapFile{
					Data: []byte(`(define-library (test ci-lib)
  (export answer)
  (import (scheme base))
  (include-ci "ci/lib-defs.scm"))`),
				},
			},
			code:     `(import (test ci-lib)) answer`,
			expected: values.NewInteger(42),
		},
		{
			// Regression guard: plain include must NOT fold — uppercase stays uppercase.
			// Including a file that defines BAR and referencing BAR (same case) works;
			// this confirms include-ci's folding is the only difference from include.
			name: "plain include preserves case",
			fs: fstest.MapFS{
				"ci/case.scm": &fstest.MapFile{
					Data: []byte(`(define BAR 9)`),
				},
			},
			code:     `(import (scheme base)) (include "ci/case.scm") BAR`,
			expected: values.NewInteger(9),
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
