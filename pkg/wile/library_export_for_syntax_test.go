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
	"errors"
	"testing"
	"testing/fstest"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
)

// A plain export names a phase-0 binding; (for-syntax spec ...) names the same
// thing one phase up, and nests. This is Racket's provide. Every refusal below
// was measured against racket 8, where the plain provide of a begin-for-syntax
// define, of a begin-for-syntax define-syntax, of a phase-3 define, and
// (provide (for-syntax m)) of a phase-0 macro all raise "provided identifier is
// not defined or required".
var forSyntaxExportFS = fstest.MapFS{
	"ct-var.scm": &fstest.MapFile{Data: []byte(`(define-library (ct-var)
  (import (scheme base))
  (export (for-syntax ct-var))
  (begin (begin-for-syntax (define ct-var 1))))
`)},
	"ct-mac.scm": &fstest.MapFile{Data: []byte(`(define-library (ct-mac)
  (import (scheme base) (for-syntax (scheme base)))
  (export (for-syntax ct-mac))
  (begin (begin-for-syntax (define-syntax ct-mac (syntax-rules () ((_) 7))))))
`)},
	"two-phase.scm": &fstest.MapFile{Data: []byte(`(define-library (two-phase)
  (import (scheme base))
  (export both (for-syntax both (rename both both-ct)))
  (begin (define both 0) (begin-for-syntax (define both 1))))
`)},
	"relay.scm": &fstest.MapFile{Data: []byte(`(define-library (relay)
  (import (scheme base) (ct-var))
  (export probe (for-syntax ct-var))
  (begin (define-syntax probe (er-macro-transformer (lambda (f r c) ct-var)))))
`)},
}

func TestLibraryExportRefusesNameNotBoundAtItsPhase(t *testing.T) {
	tcs := []struct {
		name string
		lib  string
		want string // the diagnostic's entry for the refused name
	}{
		{
			name: "plain export of a begin-for-syntax define",
			lib: `(define-library (refused)
  (import (scheme base))
  (export ct-only)
  (begin (begin-for-syntax (define ct-only 1))))`,
			want: "ct-only (bound at phase 1, not phase 0)",
		},
		{
			name: "plain export of a begin-for-syntax define-syntax",
			lib: `(define-library (refused)
  (import (scheme base) (for-syntax (scheme base)))
  (export ct-mac)
  (begin (begin-for-syntax (define-syntax ct-mac (syntax-rules () ((_) 7))))))`,
			want: "ct-mac (bound at phase 1, not phase 0)",
		},
		{
			name: "plain export of a phase-3 define",
			lib: `(define-library (refused)
  (import (scheme base))
  (export deep)
  (begin (begin-for-syntax (begin-for-syntax (begin-for-syntax (define deep 42))))))`,
			want: "deep (bound at phase 3, not phase 0)",
		},
		{
			name: "for-syntax export of a phase-0 macro",
			lib: `(define-library (refused)
  (import (scheme base))
  (export (for-syntax m0))
  (begin (define-syntax m0 (syntax-rules () ((_) 0)))))`,
			want: "m0 (bound at phase 0, not phase 1)",
		},
		{
			name: "for-syntax export of a phase-0 define",
			lib: `(define-library (refused)
  (import (scheme base))
  (export (for-syntax v0))
  (begin (define v0 0)))`,
			want: "v0 (bound at phase 0, not phase 1)",
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := phaseIsolationEngine(t, fstest.MapFS{
				"refused.scm": &fstest.MapFile{Data: []byte(tc.lib)},
			})
			_, err := eng.EvalMultiple(context.Background(), `(import (refused))`)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrUnexportedIdentifier), qt.IsTrue,
				qt.Commentf("want ErrUnexportedIdentifier, got: %v", err))
			qt.Assert(t, err.Error(), qt.Contains, tc.want)
		})
	}
}

// Each program raises unless the imported name has the expected value at the
// phase it reads it from, so a nil error is the whole assertion.
func TestLibraryExportForSyntaxRoundTrips(t *testing.T) {
	tcs := []struct {
		name string
		prog string
	}{
		{
			name: "a phase-1 variable is visible at phase 1",
			prog: `(import (ct-var))
(begin-for-syntax (if (= ct-var 1) #t (error "ct-var" ct-var)))`,
		},
		{
			name: "a phase-1 macro expands in a phase-1 body",
			prog: `(import (ct-mac))
(begin-for-syntax (if (= (ct-mac) 7) #t (error "ct-mac")))`,
		},
		{
			name: "one name at two phases keeps two bindings",
			prog: `(import (two-phase))
(begin-for-syntax (if (= both 1) #t (error "phase-1 both" both)))
(if (= both 0) #t (error "phase-0 both" both))`,
		},
		{
			name: "rename inside for-syntax",
			prog: `(import (two-phase))
(begin-for-syntax (if (= both-ct 1) #t (error "both-ct" both-ct)))`,
		},
		{
			name: "prefix applies at every phase",
			prog: `(import (prefix (two-phase) p:))
(begin-for-syntax (if (= p:both 1) #t (error "phase-1 p:both" p:both)))
(if (= p:both 0) #t (error "phase-0 p:both" p:both))`,
		},
		{
			name: "except removes a name at every phase",
			prog: `(import (except (two-phase) both))
(begin-for-syntax (if (= both-ct 1) #t (error "both-ct" both-ct)))`,
		},
		{
			name: "a for-syntax import shifts a for-syntax export to phase 2",
			prog: `(import (for-syntax (ct-var)) (for-meta 2 (scheme base)))
(begin-for-syntax (begin-for-syntax (if (= ct-var 1) #t (error "ct-var" ct-var))))`,
		},
		{
			name: "a library imports a for-syntax export into its own phase 1 and re-exports it",
			prog: `(import (relay))
(if (= (probe) 1) #t (error "probe"))
(begin-for-syntax (if (= ct-var 1) #t (error "relayed ct-var" ct-var)))`,
		},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := phaseIsolationEngine(t, forSyntaxExportFS)
			_, err := eng.EvalMultiple(context.Background(), tc.prog)
			qt.Assert(t, err, qt.IsNil)
		})
	}
}

// The other half of the round trip: a for-syntax export must not also land at
// the importer's phase 0.
func TestLibraryExportForSyntaxIsNotBoundAtPhaseZero(t *testing.T) {
	tcs := []struct {
		name string
		prog string
	}{
		{name: "plain import", prog: `(import (ct-var)) ct-var`},
		{name: "only keeps the phase-1 row only", prog: `(import (only (two-phase) both-ct)) both-ct`},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			eng := phaseIsolationEngine(t, forSyntaxExportFS)
			_, err := eng.EvalMultiple(context.Background(), tc.prog)
			qt.Assert(t, err, qt.IsNotNil)
			qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
				qt.Commentf("want ErrNoSuchBinding, got: %v", err))
		})
	}
}

func TestLibraryExportForSyntaxMalformed(t *testing.T) {
	eng := phaseIsolationEngine(t, fstest.MapFS{
		"bad.scm": &fstest.MapFile{Data: []byte(`(define-library (bad)
  (import (scheme base))
  (export (for-syntax 1))
  (begin (define x 1)))`)},
	})
	_, err := eng.EvalMultiple(context.Background(), `(import (bad))`)
	qt.Assert(t, err, qt.IsNotNil)
	qt.Assert(t, errors.Is(err, werr.ErrInvalidSyntax), qt.IsTrue,
		qt.Commentf("want ErrInvalidSyntax, got: %v", err))
}
