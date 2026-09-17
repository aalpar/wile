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
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"
)

// A syntax-rules transformer is phase-1 code, so a renamed or prefixed
// syntax-rules needs its new name at phase 1. racket/base exports syntax-rules,
// ... and _ at phase 1 as well as phase 0 (module->exports, racket 9.2), both
// rows the one phase-0 definition, so rename-in and prefix-in reach the
// transformer position and (require racket/base (for-syntax racket/base)) is no
// conflict. (scheme base) and (scheme r5rs) export the same three names the
// same way.
func TestSchemeBaseExportsSyntaxRulesForSyntax(t *testing.T) {
	files := fstest.MapFS{
		"sr-user.scm": &fstest.MapFile{Data: []byte(`(define-library (sr-user)
  (import (rename (scheme base) (syntax-rules sr)))
  (export two)
  (begin (define-syntax two (sr () ((_) 2)))))
`)},
	}
	tcs := []struct {
		name string
		prog string
	}{
		{
			name: "rename (scheme base) syntax-rules",
			prog: `(import (rename (scheme base) (syntax-rules sr)))
(define-syntax two (sr () ((_) 2)))
(two)`,
		},
		{
			name: "prefix (scheme base)",
			prog: `(import (prefix (scheme base) b:))
(b:define-syntax two (b:syntax-rules () ((_) 2)))
(two)`,
		},
		{
			name: "rename (scheme r5rs) syntax-rules",
			prog: `(import (rename (scheme r5rs) (syntax-rules sr)))
(define-syntax two (sr () ((_) 2)))
(two)`,
		},
		{
			name: "rename inside a library body",
			prog: `(import (scheme base) (sr-user))
(two)`,
		},
		{
			name: "plain and for-syntax import of (scheme base) do not conflict",
			prog: `(import (scheme base) (for-syntax (scheme base)))
(define-syntax two (syntax-rules () ((_) 2)))
(two)`,
		},
		{
			name: "(scheme base) and (scheme r5rs) at both phases do not conflict",
			prog: `(import (scheme base) (scheme r5rs) (for-syntax (scheme base)) (for-syntax (scheme r5rs)))
(define-syntax two (syntax-rules () ((_) 2)))
(two)`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := phaseIsolationEngine(t, files)
			v, err := eng.EvalMultiple(context.Background(), tc.prog)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.Internal().SchemeString(), qt.Equals, "2")
		})
	}
}
