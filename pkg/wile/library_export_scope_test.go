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

// Hygiene of the library EXPORT seam.
//
// A library body's binders carry the library's own scope; an imported binding is
// ambient and carries the empty set. The export lookup resolves under the library
// scope, so maximal resolution decides all four cases below rather than
// slot-insertion order deciding them. See findLibraryBinding.

import (
	"context"
	"strings"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// evalWithLib runs src against an engine that can load the given library source.
func evalWithLib(t *testing.T, libSource, src string) (string, error) {
	t.Helper()
	ctx := context.Background()
	fs := fstest.MapFS{
		"exportlib.scm": &fstest.MapFile{Data: []byte(libSource)},
	}
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fs),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."))
	if err != nil {
		return "", err
	}
	defer eng.Close()
	v, err := eng.EvalMultiple(ctx, src)
	if err != nil {
		return "", err
	}
	return v.SchemeString(), nil
}

// A library that defines a name it also imports must export ITS OWN definition.
// The define sits at {libScope}, the ambient import at {}; maximal resolution
// prefers the define. Before the export lookup was scope-keyed this was decided
// by which slot happened to be created first.
func TestLibraryExport_OwnDefineBeatsAmbientImport(t *testing.T) {
	c := qt.New(t)
	got, err := evalWithLib(t, `(define-library (exportlib)
  (export not)
  (import (scheme base))
  (begin
    (define (not x) 'OVERRIDDEN)))
`, `(import (exportlib)) (not #t)`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "OVERRIDDEN")
}

// A re-exported name has only the ambient {} slot in the library frame. Since
// {} is a subset of {libScope} it still resolves, so re-export needs no special
// case in the export lookup.
func TestLibraryExport_ReExportResolves(t *testing.T) {
	c := qt.New(t)
	got, err := evalWithLib(t, `(define-library (exportlib)
  (export car cdr)
  (import (scheme base)))
`, `(import (exportlib)) (car (list 1 2))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "1")
}

// A binder whose name arrives through a macro PATTERN VARIABLE was written by
// the library author at the use site, so it carries {libScope} and stays
// exportable. define-record-type is the case that matters in practice.
func TestLibraryExport_PatternVariableBinderIsExportable(t *testing.T) {
	c := qt.New(t)
	got, err := evalWithLib(t, `(define-library (exportlib)
  (export made-by-macro rec-a make-r)
  (import (scheme base))
  (begin
    (define-syntax defit (syntax-rules () ((_ n v) (define n v))))
    (defit made-by-macro 42)
    (define-record-type <r> (make-r a) r? (a rec-a))))
`, `(import (exportlib)) (+ made-by-macro (rec-a (make-r 1)))`)
	c.Assert(err, qt.IsNil)
	c.Assert(got, qt.Equals, "43")
}

// A binder introduced by a macro TEMPLATE is hygienically distinct from the
// identifier the export list names (R7RS §4.3.2), so it is NOT exportable. The
// rejection is eager, at define-library time, and the diagnostic must say why
// rather than claiming the name is undefined.
func TestLibraryExport_TemplateIntroducedBinderIsRejected(t *testing.T) {
	c := qt.New(t)
	_, err := evalWithLib(t, `(define-library (exportlib)
  (export template-made)
  (import (scheme base))
  (begin
    (define-syntax hide (syntax-rules () ((_) (define template-made 7))))
    (hide)))
`, `(import (exportlib)) template-made`)
	c.Assert(err, qt.IsNotNil)
	c.Assert(strings.Contains(err.Error(), "macro template"), qt.IsTrue,
		qt.Commentf("diagnostic must name the hygiene cause, got: %v", err))
}
