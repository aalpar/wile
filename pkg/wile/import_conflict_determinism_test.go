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
	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// conflictNameRE lifts the identifier out of the refusals
// pkg/machine/compilation/library_bindings.go raises when two imports of one
// name have different provenance roots: `import: identifier %q from %s
// conflicts with ...`, against an earlier import or another set of the same
// form.
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
// The assertion is determinism, not a particular name: R7RS §5.2 fixes that the
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

// atomicLibFS adds a library to conflictingLibFS whose a0 and a1 sort before the
// conflicting aa, so a copy loop that installs in order and stops at the conflict
// leaves them bound.
var atomicLibFS = fstest.MapFS{
	"conf/three.sld": &fstest.MapFile{
		Data: []byte(`(define-library (conf three)
  (import (scheme base))
  (export a0 a1 aa)
  (begin
    (define a0 3)
    (define-syntax a1 (syntax-rules () ((_) 3)))
    (define aa 3)))`),
	},
}

// macroConflictLibFS is two libraries exporting different macros named mm. The
// second also exports b0, which sorts before mm, so an import that met the
// phase-1 conflict only while installing would leave b0 bound.
var macroConflictLibFS = fstest.MapFS{
	"cm/one.sld": &fstest.MapFile{
		Data: []byte(`(define-library (cm one)
  (import (scheme base))
  (export mm)
  (begin
    (define-syntax mm (syntax-rules () ((_) 1)))))`),
	},
	"cm/two.sld": &fstest.MapFile{
		Data: []byte(`(define-library (cm two)
  (import (scheme base))
  (export b0 mm)
  (begin
    (define b0 2)
    (define-syntax mm (syntax-rules () ((_) 2)))))`),
	},
}

// refusedImportLibFS is libraries whose own import declarations are refused,
// and (bad boom), whose body raises. Line 4 of bad/split.sld is the refused
// declaration; in bad/tail.sld, bad/tailboom.sld and bad/notalist.sld line 3 is
// the improper tail, a line below its declaration.
var refusedImportLibFS = fstest.MapFS{
	"bad/same.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad same)
  (import (scheme base))
  (import (conf three) (conf one))
  (export x)
  (begin (define x 1)))`),
	},
	"bad/split.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad split)
  (import (scheme base))
  (import (conf one))
  (import (conf three))
  (export x)
  (begin (define x 1)))`),
	},
	"bad/boom.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad boom)
  (import (scheme base))
  (export x)
  (begin (define x 1) (error "bad boom: body ran")))`),
	},
	"bad/tailboom.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad tailboom)
  (import (scheme base) (bad boom)
          . foo)
  (export x)
  (begin (define x 1)))`),
	},
	"bad/notalist.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad notalist)
  (import
    . foo)
  (export x)
  (begin (define x 1)))`),
	},
	"bad/tail.sld": &fstest.MapFile{
		Data: []byte(`(define-library (bad tail)
  (import (scheme base)
          . foo)
  (export x)
  (begin (define x 1)))`),
	},
}

// newAtomicImportEngine is an engine that can load every library this file
// defines, with obs as its import observer when non-nil.
func newAtomicImportEngine(t *testing.T, obs func(wile.LibraryImportEvent)) *wile.Engine {
	opts := []wile.EngineOption{
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(conflictingLibFS),
		wile.WithSourceFS(atomicLibFS),
		wile.WithSourceFS(macroConflictLibFS),
		wile.WithSourceFS(refusedImportLibFS),
		wile.WithLibraryPaths("."),
	}
	if obs != nil {
		opts = append(opts, wile.WithImportObserver(obs))
	}
	eng, err := wile.NewEngine(context.Background(), opts...)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestRefusedImportBindsNothing pins that a top-level import form installs every
// binding or none. Each row's form is refused, and afterwards the row's
// references must be unbound. (conf three)'s are a0 and a1, a variable and a
// macro, because a macro's install lands one phase up.
//
// Fails on master, where each set installed in sorted order and a refusal
// returned midway, leaving a0 and a1 bound.
func TestRefusedImportBindsNothing(t *testing.T) {
	ctx := context.Background()
	newEngine := func(t *testing.T) *wile.Engine {
		return newAtomicImportEngine(t, nil)
	}

	// Control: on its own the library binds both names, so the unbound
	// assertions below are about the refusal.
	t.Run("alone", func(t *testing.T) {
		got, err := newEngine(t).EvalMultiple(ctx, `(import (conf three)) (+ a0 (a1))`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got.SchemeString(), qt.Equals, "6")
	})

	three := []string{`a0`, `(a1)`}
	tests := []struct {
		name    string
		setup   string // must succeed; empty for none
		form    string
		want    error
		msg     string // a pattern the refusal must match; empty for unchecked
		unbound []string
	}{
		// Each refusal below points at the failing set, on line 2, not at the form.
		{name: "a later form conflicts", setup: `(import (conf one))`, form: "(import (scheme base)\n        (conf three))", want: werr.ErrDuplicateBinding,
			msg: `(?s).*:2:8: .*"aa" from \(conf three\) conflicts with a different existing import.*`, unbound: three},
		{name: "a later set of the same form conflicts", form: "(import (conf three)\n        (conf one))", want: werr.ErrDuplicateBinding,
			msg: `(?s).*:2:8: .*"aa" from \(conf one\) conflicts with the one from \(conf three\) in the same import form.*`, unbound: three},
		// Refused while planning: a1's propagation one phase above 127 overflows.
		{name: "a later set cannot be planned", form: "(import (scheme base)\n        (for-meta 127 (conf three)))", want: werr.ErrInvalidArgument,
			msg: `(?s).*:2:8: .*`},
		{name: "a later set of the same form is missing", form: `(import (conf three) (conf missing))`, want: werr.ErrFileNotFound, unbound: three},
		{name: "a later set on a later line is missing", form: "(import (conf three)\n        (conf missing))", want: werr.ErrFileNotFound,
			msg: `(?s).*:2:8: .*`, unbound: three},
		{name: "the form is an improper list", form: `(import (conf three) . foo)`, want: werr.ErrInvalidSyntax, unbound: three},
		// No import set before the tail: the same refusal, at the tail.
		{name: "the form is not a list", form: `(import . foo)`, want: werr.ErrInvalidSyntax,
			msg: `(?s).*:1:10: import: import form must be a proper list.*`},
		// Refused before any library loads, so (bad boom)'s body never runs and
		// its error is not the one reported.
		{name: "the improper form names a library whose body raises", form: `(import (bad boom) . foo)`, want: werr.ErrInvalidSyntax},
		// The conflict is mm at phase 1, so only a check made at every phase
		// refuses it before b0 is written.
		{name: "a later form's macro conflicts at phase 1", setup: `(import (cm one))`, form: `(import (cm two))`, want: werr.ErrDuplicateBinding, unbound: []string{`b0`}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := newEngine(t)
			if tt.setup != "" {
				_, err := eng.EvalMultiple(ctx, tt.setup)
				qt.Assert(t, err, qt.IsNil)
			}
			_, err := eng.EvalMultiple(ctx, tt.form)
			qt.Assert(t, err, qt.ErrorIs, tt.want)
			if tt.msg != "" {
				qt.Assert(t, err, qt.ErrorMatches, tt.msg)
			}
			for _, ref := range tt.unbound {
				_, err = eng.EvalMultiple(ctx, ref)
				qt.Assert(t, err, qt.ErrorIs, werr.ErrNoSuchBinding, qt.Commentf("%s after the refused import", ref))
			}
		})
	}
}

// TestRefusedImportFiresNoObserver pins that a top-level import form's observer
// events fire only once the whole form has passed the check, so an observer (the
// doc registration among them) never reports an import the check refused.
func TestRefusedImportFiresNoObserver(t *testing.T) {
	ctx := context.Background()
	var topLevel []string
	eng := newAtomicImportEngine(t, func(evt wile.LibraryImportEvent) {
		if evt.Importer.Key() == "" {
			topLevel = append(topLevel, evt.Library.Key())
		}
	})
	topLevel = nil

	_, err := eng.EvalMultiple(ctx, `(import (conf three) (conf one))`)
	qt.Assert(t, err, qt.ErrorIs, werr.ErrDuplicateBinding)
	qt.Assert(t, topLevel, qt.HasLen, 0, qt.Commentf("events fired for a refused form: %v", topLevel))

	// Control: the observer sees an accepted form, so the empty answer above is
	// about the refusal.
	_, err = eng.EvalMultiple(ctx, `(import (conf three))`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, topLevel, qt.Not(qt.HasLen), 0)
}

// TestRefusedLibraryImportDeclarationBindsNothing pins all or none for a
// library's own import declarations. A refused library leaves no binding a
// program can reach, so the observer is the witness: a declaration fires its
// events only once all of it has passed the check, and fired lists every import
// the library made.
func TestRefusedLibraryImportDeclarationBindsNothing(t *testing.T) {
	tests := []struct {
		lib   string
		want  error
		fired []string
		at    string // the refusal's location; empty for unchecked
	}{
		// Two sets of one declaration conflict: neither installs.
		{lib: "same", want: werr.ErrDuplicateBinding, fired: []string{"scheme/base"}},
		// A declaration conflicts with an earlier one: the check sees the earlier
		// declaration's installs, and the refusal points at the declaration's set.
		{lib: "split", want: werr.ErrDuplicateBinding, fired: []string{"scheme/base", "conf/one"}, at: "bad/split.sld:4:"},
		// The refusal points at the tail, not at its declaration.
		{lib: "tail", want: werr.ErrInvalidSyntax, at: "bad/tail.sld:3:"},
		// Refused before any library loads, so (bad boom)'s body never runs.
		{lib: "tailboom", want: werr.ErrInvalidSyntax, at: "bad/tailboom.sld:3:"},
		// No import set at all before the tail: the same refusal, at the tail.
		{lib: "notalist", want: werr.ErrInvalidSyntax, at: "bad/notalist.sld:3:"},
	}
	for _, tt := range tests {
		t.Run(tt.lib, func(t *testing.T) {
			var fired []string
			eng := newAtomicImportEngine(t, func(evt wile.LibraryImportEvent) {
				if evt.Importer.Key() == "bad/"+tt.lib {
					fired = append(fired, evt.Library.Key())
				}
			})
			_, err := eng.EvalMultiple(context.Background(), `(import (bad `+tt.lib+`))`)
			qt.Assert(t, err, qt.ErrorIs, tt.want)
			if tt.at != "" {
				qt.Assert(t, err.Error(), qt.Contains, tt.at)
			}
			qt.Assert(t, fired, qt.DeepEquals, tt.fired)
		})
	}
}

// TestRefusedNamespaceRequireBindsNothing pins all or none for namespace-require,
// whose ImportSpecInto is also the core of environment and make-namespace.
func TestRefusedNamespaceRequireBindsNothing(t *testing.T) {
	ctx := context.Background()
	t.Run("alone", func(t *testing.T) {
		eng := newAtomicImportEngine(t, nil)
		got, err := eng.EvalMultiple(ctx, `(define ns (make-namespace)) (namespace-require ns '(conf three)) (namespace-bound? ns 'a0)`)
		qt.Assert(t, err, qt.IsNil)
		qt.Assert(t, got.SchemeString(), qt.Equals, "#t")
	})

	tests := []struct {
		name  string
		setup string // must succeed; empty for none
		form  string
	}{
		{name: "a later call conflicts", setup: `(namespace-require ns '(conf one))`, form: `(namespace-require ns '(conf three))`},
		{name: "two sets of one spec conflict", form: `(namespace-require ns '(for-meta 0 (conf three) (conf one)))`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := newAtomicImportEngine(t, nil)
			_, err := eng.EvalMultiple(ctx, `(define ns (make-namespace))`)
			qt.Assert(t, err, qt.IsNil)
			if tt.setup != "" {
				_, err = eng.EvalMultiple(ctx, tt.setup)
				qt.Assert(t, err, qt.IsNil)
			}
			_, err = eng.EvalMultiple(ctx, tt.form)
			qt.Assert(t, err, qt.ErrorIs, werr.ErrDuplicateBinding)
			got, err := eng.EvalMultiple(ctx, `(namespace-bound? ns 'a0)`)
			qt.Assert(t, err, qt.IsNil)
			qt.Assert(t, got.SchemeString(), qt.Equals, "#f", qt.Commentf("a0 after the refused namespace-require"))
		})
	}
}
