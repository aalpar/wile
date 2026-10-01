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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A library that writes its own (define-syntax if ...) exports THAT, not the
// core `if` it inherited from its own (scheme base). I134.
//
// Measured against petite 10.4.1, which is the oracle here and disagrees with
// master. Chez REFUSES the library that both imports (rnrs) and defines `if`
// ("multiple definitions for if in body"), so its fixture must spell the
// library's import (except (rnrs) if) — and the row below is written that way
// too, deliberately, so that the comparison is on the shape Chez accepts rather
// than on a shadow only Wile permits. On that shape petite answers lib-if for
// both the plain and the renamed import, and master answers the core form's 1.
//
// Red on master: the phase arm of findLibraryBinding returned on
// BindingType() != Syntax, which ACCEPTS BindingTypePrimitive — the type only
// registerCompileTimeBinding writes, and only into a startup set. So the
// inherited language keyword answered and the library's own row was never
// probed.
func ownSyntaxEngine(t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(context.Background(),
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(fstest.MapFS{
			"myif.scm": &fstest.MapFile{Data: []byte(
				"(define-library (myif)\n" +
					"  (import (except (scheme base) if))\n" +
					"  (export if zif)\n" +
					"  (begin\n" +
					"    (define-syntax if (syntax-rules () ((_ a b c) 'lib-if)))\n" +
					"    (define-syntax zif (syntax-rules () ((_ a b c) 'lib-zif)))))\n")},
			"reexport.scm": &fstest.MapFile{Data: []byte(
				"(define-library (reexport)\n" +
					"  (import (scheme base))\n" +
					"  (export car))\n")},
		}),
		wile.WithSourceFS(stdlib.FS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

func TestLibraryOwnSyntaxBeatsInheritedCoreForm(t *testing.T) {
	for _, row := range []struct {
		name string
		src  string
		want string
	}{
		{"a library's own if is what it exports",
			`(import (except (scheme base) if) (myif)) (if #t 1 2)`, "lib-if"},
		{"and through a rename",
			`(import (except (scheme base) if) (rename (myif) (if qif))) (qif 7 8 9)`, "lib-if"},

		// CONTROLS, green before and after. They are what make the two rows
		// above a measurement of this one pairing rather than a precedence flip:
		// zif is not a core form, so no Primitive slot competes with it, and a
		// plain re-export has no phase-1 syntax row at all.
		{"a non-core name was already right",
			`(import (scheme base) (myif)) (zif 1 2 3)`, "lib-zif"},
		{"a plain re-export still resolves at the export phase",
			`(import (scheme base) (reexport)) (car (list 9 8))`, "9"},
	} {
		t.Run(row.name, func(t *testing.T) {
			eng := ownSyntaxEngine(t)
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
		})
	}
}
