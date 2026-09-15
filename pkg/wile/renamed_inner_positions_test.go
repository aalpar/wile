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

	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/wile"

	qt "github.com/frankban/quicktest"
)

// A renamed identifier keeps its meaning in INNER positions too: as a marker
// inside a quasiquote template, and as a macro's pattern literal. R7RS §4.3.2
// states the literal rule in terms of bindings, not spellings. Measured
// 2026-09-15: Racket 9.2 answers 2 and (1 2) for the two shapes below; Petite
// Chez 10.4.1 answers 2 for the literal shape.

type innerPositionRow struct {
	name string
	src  string
	want string
}

func runInnerPositionRows(t *testing.T, rows []innerPositionRow) {
	t.Helper()
	for _, row := range rows {
		t.Run(row.name, func(t *testing.T) {
			eng, err := wile.NewEngine(context.Background(),
				wile.WithProfile(wile.KitchenSink),
				wile.WithSourceFS(stdlib.FS),
				wile.WithLibraryPaths("."),
			)
			qt.Assert(t, err, qt.IsNil)
			t.Cleanup(func() {
				_ = eng.Close()
			})
			v, err := eng.EvalMultiple(context.Background(), row.src)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, v.SchemeString(), qt.Equals, row.want)
		})
	}
}

// Task 2: quasiquote markers.
func TestRenamedQuasiquoteMarkers(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		{"renamed unquote",
			`(import (scheme base) (rename (scheme base) (unquote uq))) (quasiquote (1 (uq (+ 1 1))))`, "(1 2)"},
		{"renamed unquote-splicing",
			`(import (scheme base) (rename (scheme base) (unquote-splicing uqs))) (quasiquote (1 (uqs (list 2 3)) 4))`, "(1 2 3 4)"},
		{"prefixed unquote",
			`(import (prefix (scheme base) b:)) (b:quasiquote (1 (b:unquote (b:+ 1 1))))`, "(1 2)"},
		// The nested marker is DATA, and it renders with the spelling the user
		// wrote: dispatch reads the denotation, rewrapQuasiForm rebuilds with the
		// original symbol. This is the design's choice, not an oracle claim; the
		// control row below shows the same shape with canonical names (measured:
		// `(1 `(2 ,(+ 1 1))) renders as (1 (quasiquote (2 (unquote (+ 1 1)))))).
		{"renamed nested quasiquote keeps the user's spelling as data",
			`(import (scheme base) (rename (scheme base) (quasiquote qq) (unquote uq))) (qq (1 (qq (2 (uq (+ 1 1))))))`,
			"(1 (qq (2 (uq (+ 1 1)))))"},
		{"control: nested markers unrenamed",
			`(import (scheme base)) (quasiquote (1 (quasiquote (2 (unquote (+ 1 1))))))`,
			"(1 (quasiquote (2 (unquote (+ 1 1)))))"},
		{"control: unrenamed markers",
			`(import (scheme base)) (quasiquote (1 (unquote (+ 1 1))))`, "(1 2)"},
		{"control: a local binding of the marker name is not a marker",
			`(import (scheme base)) (let ((uq list)) (quasiquote (1 (uq 2))))`, "(1 (uq 2))"},
	})
}

// Task 3: pattern literals.
func TestRenamedAuxiliaryKeywords(t *testing.T) {
	runInnerPositionRows(t, []innerPositionRow{
		{"renamed else in cond",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (cond (#f 1) (otherwise 2))`, "2"},
		{"renamed else in case",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (case 9 ((1) 'a) (otherwise 'b))`, "b"},
		{"renamed => in cond",
			`(import (scheme base) (rename (scheme base) (=> arrow))) (cond (5 arrow (lambda (x) (+ x 1))))`, "6"},
		{"prefixed else",
			`(import (prefix (scheme base) b:)) (b:cond (#f 1) (b:else 2))`, "2"},
		{"control: unrenamed else",
			`(import (scheme base)) (cond (#f 1) (else 2))`, "2"},
		{"control: a local binding of else is not the literal",
			`(import (scheme base)) (let ((else #t)) (cond (else 3) (#t 4)))`, "3"},
		{"control: an unrelated keyword does not match the literal",
			`(import (scheme base) (rename (scheme base) (else otherwise))) (cond (#f 1) (#t 5))`, "5"},
	})
}
