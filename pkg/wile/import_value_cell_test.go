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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"
)

// TestImportSharesTheValueLocation is P3.12's gate (I042, I044). An import
// transports the exporting library's value LOCATION, not a copy of its value, so
// a library that assigns its own exported variable is visible through the import.
//
// R7RS is silent on the fork: §5.2 constrains what the IMPORTER may do to an
// imported binding ("to redefine or mutate an imported binding with a definition
// or with set!") and says nothing about the exporting library's own set!. Of the
// two oracles, Racket reads through and Chez refuses to export an assigned
// variable at all; read-through is the only answer that keeps the programs the
// copy answered wrongly, and every row below carries Racket's answer.
//
// TWO ROWS ARE RED ON MASTER, and they are red for the same reason from opposite
// directions: master froze the importer's value at import time, so the importer
// read a stale 1 ("sees the assignment"), and the compile-time arity check
// believed that frozen value ("withdraws for an assigned export"). The other two
// rows are guards on what this change must NOT do — the supersede de-alias, and
// the non-vacuity of the arity check, which would otherwise be satisfied by
// turning IsStable() off for every import.
func TestImportSharesTheValueLocation(t *testing.T) {
	ctx := context.Background()

	fsys := fstest.MapFS{
		// Assigns its own exported variable, from a procedure that the importer
		// calls. get is the library-side reader, so one program can observe both
		// ends of the edge.
		"p312/counter.sld": &fstest.MapFile{
			Data: []byte(`(define-library (p312 counter)
  (import (scheme base))
  (export c get bump!)
  (begin
    (define c 1)
    (define (get) c)
    (define (bump!) (set! c 2))))`),
		},
		// Same shape, but the exported variable holds a procedure whose arity
		// changes, which is what reaches checkCallArity.
		"p312/arity.sld": &fstest.MapFile{
			Data: []byte(`(define-library (p312 arity)
  (import (scheme base))
  (export c bump!)
  (begin
    (define c (lambda (x) x))
    (define (bump!) (set! c (lambda (x y) (+ x y))))))`),
		},
		// Never assigned: the binding's Imported flag remains standing evidence
		// of rebind-stability, so the compile-time arity check must still fire.
		"p312/fixed.sld": &fstest.MapFile{
			Data: []byte(`(define-library (p312 fixed)
  (import (scheme base))
  (export c)
  (begin
    (define c (lambda (x) x))))`),
		},
	}

	newEngine := func(t *testing.T) *wile.Engine {
		t.Helper()
		eng, err := wile.NewEngine(ctx,
			wile.WithProfile(wile.KitchenSink),
			wile.WithSourceFS(stdlib.FS),
			wile.WithSourceFS(fsys),
			wile.WithLibraryPaths("."),
		)
		qt.Assert(t, err, qt.IsNil)
		t.Cleanup(func() {
			_ = eng.Close()
		})
		return eng
	}

	tcs := []struct {
		name    string
		code    string
		want    string
		wantErr string
		comment string
	}{
		{
			name: "an importer sees the library's assignment",
			code: `(import (p312 counter))
(bump!)
(list 'importer-sees c 'library-sees (get))`,
			want:    "(importer-sees 2 library-sees 2)",
			comment: "RED on master, which answered (importer-sees 1 library-sees 2); racket answers this row",
		},
		{
			name: "a superseding define does not write the library's variable",
			code: `(import (p312 counter))
(define c 99)
(bump!)
(list 'program-sees c 'library-sees (get))`,
			want:    "(program-sees 99 library-sees 2)",
			comment: "the shared-location hazard: without the supersede de-alias the define lands on the library's c",
		},
		{
			name: "the compile-time arity check withdraws for an assigned export",
			code: `(import (p312 arity))
(bump!)
(c 1 2)`,
			want:    "3",
			comment: "RED on master, which refused the call against the frozen 1-argument copy; racket answers 3",
		},
		{
			name: "the compile-time arity check still fires for an unassigned export",
			code: `(import (p312 fixed))
(c 1 2)`,
			wantErr: "call to c: expected 1 argument(s), got 2",
			comment: "NON-VACUITY: withdrawing IsStable() for every import would satisfy the row above and pass the whole suite",
		},
		{
			name:    "the ambient base keeps its stability",
			code:    `(car (list 1) 2)`,
			wantErr: "call to car: expected 1 argument(s), got 2",
			comment: "the same guard on a real base binding rather than a fixture library",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := newEngine(t)
			v, err := eng.EvalMultiple(ctx, tc.code)
			if tc.wantErr != "" {
				qt.Assert(t, err, qt.IsNotNil, qt.Commentf("%s", tc.comment))
				qt.Assert(t, err.Error(), qt.Contains, tc.wantErr, qt.Commentf("%s", tc.comment))
				return
			}
			qt.Assert(t, err, qt.IsNil, qt.Commentf("%s", tc.comment))
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want, qt.Commentf("%s", tc.comment))
		})
	}
}
