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

package wile

import (
	"context"
	"errors"
	"testing"
	"testing/fstest"

	"github.com/aalpar/wile/extensions/math"
	"github.com/aalpar/wile/pkg/machine"
	"github.com/aalpar/wile/pkg/registry"
	"github.com/aalpar/wile/pkg/stdlib"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

func TestExtensionAsLibrary_Import(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (wile math))
		(sqrt 4)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "2")
}

func TestExtensionAsLibrary_Only(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// sqrt should work
	result, err := engine.EvalMultiple(ctx, `
		(import (only (wile math) sqrt))
		(sqrt 16)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "4")
}

func TestExtensionAsLibrary_Prefix(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (prefix (wile math) m:))
		(m:sqrt 9)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "3")
}

func TestExtensionAsLibrary_Rename(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	result, err := engine.EvalMultiple(ctx, `
		(import (rename (wile math) (sqrt square-root)))
		(square-root 25)
	`)
	c.Assert(err, qt.IsNil)
	c.Assert(result.SchemeString(), qt.Equals, "5")
}

func TestExtensionAsLibrary_NoRuntimePrimitives(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Extension that registers no runtime primitives (compile-time only binding).
	compileOnly := registry.NewExtension("compileonly", func(r *registry.PrimitiveRegistry) error {
		r.AddBinding("my-special-form")
		return nil
	})

	engine, err := NewEngine(ctx,
		WithExtension(compileOnly),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// (wile compileonly) should not exist as a library since it has no runtime primitives.
	_, err = engine.EvalMultiple(ctx, `(import (wile compileonly))`)
	c.Assert(err, qt.IsNotNil)
}

// mockLibraryNamerExtension implements both Extension and LibraryNamer.
type mockLibraryNamerExtension struct {
	libName []string
}

func (m *mockLibraryNamerExtension) Name() string {
	return "custom"
}

func (m *mockLibraryNamerExtension) AddToRegistry(r *registry.PrimitiveRegistry) error {
	r.AddPrimitive(registry.PrimitiveSpec{
		Name:       "custom-fn",
		ParamCount: 1,
		Impl: machine.ForeignFunction(func(mc machine.CallContext) error {
			mc.SetValue(nil)
			return nil
		}),
	}, registry.PhaseSetRuntime)
	return nil
}

func (m *mockLibraryNamerExtension) LibraryName() []string {
	return m.libName
}

func TestExtensionAsLibrary_CustomLibraryName(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ext := &mockLibraryNamerExtension{
		libName: []string{"myorg", "utils"},
	}

	engine, err := NewEngine(ctx,
		WithExtension(ext),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	// The library should be registered under (myorg utils), not (wile custom).
	libReg := engine.Environment().LibraryRegistry()
	c.Assert(libReg, qt.IsNotNil)

	// Cast to machine.LibraryRegistry to call Lookup is complex from here,
	// so test via the library being importable by its custom name.
	// Since the Impl is nil, we can't actually call the function, but
	// just importing without error proves the library was registered correctly.
	// A nil Impl would panic at call time, but importing is fine.
	_, err = engine.EvalMultiple(ctx, `(import (myorg utils))`)
	c.Assert(err, qt.IsNil)
}

// TestExtensionAsLibrary_EmptyLibraryNameFallsBack verifies the Phase-2
// semantic: a LibraryNamer that returns an empty slice falls back to the
// (wile <name>) default rather than erroring. This applies uniformly to
// both *ExtensionFunc with an unset WithLibraryName slot and to custom
// types that happen to return [].
func TestExtensionAsLibrary_EmptyLibraryNameFallsBack(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ext := &mockLibraryNamerExtension{libName: nil}

	engine, err := NewEngine(ctx,
		WithExtension(ext),
		WithLibraryPaths(),
	)
	c.Assert(err, qt.IsNil)

	_, err = engine.EvalMultiple(ctx, `(import (wile custom))`)
	c.Assert(err, qt.IsNil)
}

func TestExtensionAsLibrary_NotEnabled(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// Without WithLibraryPaths(), library system is not enabled.
	engine, err := NewEngine(ctx,
		WithExtension(math.Extension),
	)
	c.Assert(err, qt.IsNil)

	// import should fail since the library system is not configured.
	_, err = engine.EvalMultiple(ctx, `(import (wile math))`)
	c.Assert(err, qt.IsNotNil)
}

// TestExtensionLibraryExportIgnoresUserShadow pins I137: a synthetic extension
// library's exports denote the primitives the registry installed, not whatever
// the engine's top level happens to bind by that name now.
//
// A synthetic library's Env IS the live top level, so until the exports were
// resolved at registration time the import path re-resolved each name there and
// a user define supplied the export. The rename rows are the ones that cannot be
// excused: `square-root` is a name the user never wrote, so R7RS §5.2's "it is
// an error" latitude does not reach it. Oracle: racket, the same shape over
// racket/math, answers the library's procedure under the renamed name.
//
// EvalProgram, not EvalMultiple, and that is load-bearing. EvalProgram is the
// CLI's unit — repeated -e are joined into one (begin …) — and a top-level begin
// HOISTS a slot for every define before the body compiles. So the shadow slot
// exists while the import is being resolved whichever side of it the define is
// written on, which is what makes the second row red too. Under EvalMultiple the
// define-after row resolves the import before the define's slot exists and master
// answers correctly by accident.
//
// The plain-import row pins what the fix does NOT change: there the user asked
// for `sqrt` and also defined `sqrt`, so §5.2 does apply and the define keeps
// winning.
func TestExtensionLibraryExportIgnoresUserShadow(t *testing.T) {
	cases := []struct {
		name string
		code string
		want string
	}{
		{
			name: "rename, define before import",
			code: `(define (sqrt x) 'hijacked)
			       (import (rename (wile math) (sqrt square-root)))
			       (square-root 25)`,
			want: "5",
		},
		{
			name: "rename, define after import",
			code: `(import (rename (wile math) (sqrt square-root)))
			       (define (sqrt x) 'hijacked)
			       (square-root 25)`,
			want: "5",
		},
		{
			name: "rename, no define",
			code: `(import (rename (wile math) (sqrt square-root)))
			       (square-root 25)`,
			want: "5",
		},
		{
			name: "plain import, define before, user define still wins",
			code: `(define (sqrt x) 'hijacked)
			       (import (wile math))
			       (sqrt 25)`,
			want: "hijacked",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()

			engine, err := NewEngine(ctx,
				WithExtension(math.Extension),
				WithLibraryPaths(),
			)
			c.Assert(err, qt.IsNil)

			result, err := engine.EvalProgram(ctx, tc.code, "<test>")
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestImportOrderingIsNotChecked pins what docs/reference/r7rs-differences.md says
// about R7RS §5.2's ordering clause, "refer to an identifier before it is
// imported": Wile does not check it. §1.3.2 leaves an "it is an error" clause
// undetected at the implementation's choice, so every row here is green by
// construction; the pin keeps the documented relaxation and the behaviour
// together.
//
// A program unit (EvalProgram: a file, or the CLI's joined -e) scopes an import
// over the whole unit, so a reference written before it resolves to it, as in a
// Racket module and a Chez top-level program. A form-at-a-time unit
// (EvalMultiple) refuses that reference, but only because it refuses every
// forward reference; the last row says so by refusing one with no import in it.
//
// The define-before row is the §5.2 REDEFINE clause meeting an import written
// second, for an ordinary library: the define keeps the name, as it does for the
// extension library in TestExtensionLibraryExportIgnoresUserShadow's last row.
// Racket agrees; Chez refuses the program ("multiple definitions"), so the doc
// line does not claim agreement.
func TestImportOrderingIsNotChecked(t *testing.T) {
	libFS := fstest.MapFS{"ordlib.sld": &fstest.MapFile{Data: []byte(`(define-library (ordlib)
  (import (scheme base))
  (export ord-f)
  (begin (define (ord-f) 'lib)))`)}}

	cases := []struct {
		name     string
		multiple bool
		code     string
		want     string
		wantErr  error
	}{
		{name: "reference before import",
			code: `(ord-f) (import (ordlib)) (ord-f)`, want: "lib"},
		{name: "reference before import, inside a procedure",
			code: `(define (g) (ord-f)) (import (ordlib)) (g)`, want: "lib"},
		{name: "define before import keeps the define",
			code: `(define (ord-f) 'user) (import (ordlib)) (ord-f)`, want: "user"},
		{name: "form at a time refuses any forward reference",
			multiple: true,
			code:     `(define (g) (ord-h)) (define (ord-h) 1) (g)`, wantErr: werr.ErrNoSuchBinding},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			c := qt.New(t)
			ctx := context.Background()
			engine, err := NewEngine(ctx,
				WithProfile(KitchenSink),
				WithSourceFS(stdlib.FS),
				WithSourceFS(libFS),
				WithLibraryPaths("."),
			)
			c.Assert(err, qt.IsNil)
			defer func() {
				_ = engine.Close()
			}()

			var result Value
			if tc.multiple {
				result, err = engine.EvalMultiple(ctx, tc.code)
			} else {
				result, err = engine.EvalProgram(ctx, tc.code, "<test>")
			}
			if tc.wantErr != nil {
				c.Assert(errors.Is(err, tc.wantErr), qt.IsTrue, qt.Commentf("got %v", err))
				return
			}
			c.Assert(err, qt.IsNil)
			c.Assert(result.SchemeString(), qt.Equals, tc.want)
		})
	}
}
