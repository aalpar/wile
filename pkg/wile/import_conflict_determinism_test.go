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
	"regexp"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// conflictNameRE lifts the identifier out of the refusal
// pkg/machine/compilation/library_bindings.go raises when two imports of one
// name have different provenance roots: `import: identifier %q from %s
// conflicts with a different existing import; ...`.
var conflictNameRE = regexp.MustCompile(`identifier "([^"]+)" from`)

// conflictingLibFS is two libraries that export the same ten names from
// different provenance roots, so every one of the ten is a conflict and the
// refusal has ten equally valid identifiers to choose between. Ten, not two:
// with a single conflicting name there is nothing to order and the test would
// pass on master by construction.
var conflictingLibFS = fstest.MapFS{
	"conf/one.sld": &fstest.MapFile{
		Data: []byte(`(define-library (conf one)
  (import (scheme base))
  (export aa bb cc dd ee ff gg hh ii jj)
  (begin
    (define aa 1) (define bb 1) (define cc 1) (define dd 1) (define ee 1)
    (define ff 1) (define gg 1) (define hh 1) (define ii 1) (define jj 1)))`),
	},
	"conf/two.sld": &fstest.MapFile{
		Data: []byte(`(define-library (conf two)
  (import (scheme base))
  (export aa bb cc dd ee ff gg hh ii jj)
  (begin
    (define aa 2) (define bb 2) (define cc 2) (define dd 2) (define ee 2)
    (define ff 2) (define gg 2) (define hh 2) (define ii 2) (define jj 2)))`),
	},
}

// TestImportConflictNamesTheSameIdentifierEveryTime is the I143 ratchet. Both
// library copy loops used to `range` a map[ExportKey]string directly, so the
// identifier a conflict diagnostic named — and the slot numbering behind it —
// was whichever key the Go map walk reached first. Twenty fresh engines running
// ONE program named ten different identifiers.
//
// The assertion is determinism, not a particular name: R7RS §5.6 fixes that the
// import is an error, not which of the ten conflicting names gets reported. The
// lexicographic-first check below pins the comparator's direction so a later
// change to (Phase, Name) ordering cannot silently reverse it while staying
// deterministic.
//
// Fresh engines, never RunSchemeCode: that helper skips tpl.Optimize() and the
// mutable top level, so a ratchet written on it can pass without the fix.
func TestImportConflictNamesTheSameIdentifierEveryTime(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	const runs = 20
	named := make(map[string]int, runs)

	for i := range runs {
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(conflictingLibFS),
			wile.WithLibraryPaths("."),
		)
		c.Assert(err, qt.IsNil)

		_, err = eng.EvalMultiple(ctx, `(import (conf one) (conf two))`)
		c.Assert(err, qt.IsNotNil, qt.Commentf("run %d: two libraries exporting ten conflicting names must be refused", i))

		m := conflictNameRE.FindStringSubmatch(err.Error())
		c.Assert(m, qt.HasLen, 2, qt.Commentf("run %d: refusal does not name an identifier: %v", i, err))
		named[m[1]]++
	}

	c.Assert(named, qt.HasLen, 1, qt.Commentf("distinct identifiers named across %d runs: %d %v", runs, len(named), named))
	_, first := named["aa"]
	c.Assert(first, qt.IsTrue, qt.Commentf("the sort is (Phase, Name) ascending, so the first conflict of ten same-phase names is %q; got %v", "aa", named))
}
