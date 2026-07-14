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

// The GlobalIndex literal-pool collision, end to end.
//
// GlobalIndex is {Index *Symbol, Env *GlobalEnvironmentFrame}, and its EqualTo
// used to compare only Index. The literal pool dedups through EqualTo, so two
// GlobalIndex differing ONLY in Env collapsed onto one pool slot and the second
// emitter silently inherited the first's frame.
//
// Env is not provenance. The VM branches on it: non-nil reads and writes THAT
// frame's bindings slice with no parent walk; nil is a deferred lookup resolved
// from the use site. Collapsing the two breaches cross-library isolation.
//
// ONE top-level unit that both (a) calls a library macro whose template names a
// library-private helper and (b) top-level-defines that same name is the unit
// that mints both literals. Which way it breaks depends on emission order, which
// is why both directions are asserted here.
//
// "One unit" is not incidental — it is the reproduction condition, and getting it
// wrong makes these tests silently vacuous. A literal pool belongs to a template.
// EvalMultiple compiles each top-level form as its OWN unit, so an unwrapped body
// puts the two literals in two pools, where they cannot collide and the tests pass
// against the unfixed code. Every body here is therefore (begin …)-wrapped, which
// is one unit, and which is also how file execution feeds a program to the engine.
//
// reviews/2026-07-13 REVIEW.md §7 lists this exact test as a coverage gap: "No
// test compiles one unit that both references a library binding through a macro
// and defines/set!s a same-named top-level." The unit tests on EqualTo and
// MaybeAppendLiteral pin the mechanism; these pin the symptom. All three were
// verified RED: with the Env conjunct removed from GlobalIndex.EqualTo, all three
// fail.

// globalIndexCollisionFS is a library exporting a macro over a private helper.
// `helper` is NOT exported: the only way a user can reach it is through `mac`,
// so any observation of 1 rather than 99 means the user's own top-level `helper`
// was substituted for the library's.
var globalIndexCollisionFS = fstest.MapFS{
	"mylib.sld": &fstest.MapFile{
		Data: []byte(`(define-library (mylib)
  (import (scheme base))
  (export mac get-helper)
  (begin
    (define helper 99)
    (define (get-helper) helper)
    (define-syntax mac
      (syntax-rules ()
        ((_) helper)))))`),
	},
}

func newCollisionEngine(ctx context.Context, t *testing.T) *wile.Engine {
	t.Helper()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithSourceFS(stdlib.FS),
		wile.WithSourceFS(globalIndexCollisionFS),
		wile.WithLibraryPaths("."),
	)
	qt.Assert(t, err, qt.IsNil)
	t.Cleanup(func() {
		_ = eng.Close()
	})
	return eng
}

// TestGlobalIndexCollision_MacroReadsLibraryBinding covers the plan's case 1: the
// Env==nil `define` store literal wins the dedup, so the macro's LOAD reads the
// user's frame instead of the library's. Before the fix this displayed 1.
func TestGlobalIndexCollision_MacroReadsLibraryBinding(t *testing.T) {
	ctx := context.Background()
	eng := newCollisionEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(import (mylib))
		(begin
		  (define helper 1)
		  (mac))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "99",
		qt.Commentf("the macro's free identifier must resolve to the LIBRARY's private "+
			"helper (99), not the use site's top-level define (1). Reading 1 means the "+
			"define's Env==nil store literal deduped onto the macro's pinned load literal."))
}

// TestGlobalIndexCollision_UserDefineDoesNotWriteLibraryFrame covers the plan's
// case 2, the more serious direction: the library-pinned LOAD literal wins, so
// the user's `(define helper 1)` OpStoreGlobal writes into the LIBRARY's private
// binding. The library's own accessor then reports the user's value, and the
// user's variable reads void.
//
// Emission order decides which case fires, so both are pinned. This one is the
// isolation breach: a user's top-level define silently mutating a library's
// private state.
func TestGlobalIndexCollision_UserDefineDoesNotWriteLibraryFrame(t *testing.T) {
	ctx := context.Background()
	eng := newCollisionEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(import (mylib))
		(begin
		  (mac)
		  (define helper 1)
		  (list (mac) (get-helper) helper))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(99 99 1)",
		qt.Commentf("the library's helper must still be 99 through BOTH the macro and the "+
			"library's own accessor, and the user's helper must be 1. Before the fix this "+
			"was (99 1 #!void): the define's store wrote into the library's frame."))
}

// TestGlobalIndexCollision_SetBangDoesNotWriteLibraryFrame is the set! arm, and
// it is the one that puts BOTH mint sites in one template: a top-level set! mints
// Env==g (the resolving frame) while the define store mints Env==nil, so this unit
// holds all three GlobalIndex shapes at once. Same invariant: the write must land
// in the user's frame, never the library's.
//
// The body is (begin …)-wrapped deliberately. EvalMultiple compiles each top-level
// form as its own unit, and under the default immutable top level a set! in a LATER
// unit than the define is correctly rejected — so an unwrapped body would test that
// rule rather than this one. File execution wraps in begin for the same reason; one
// begin is one compilation unit, which is exactly the "single top-level unit" the
// collision needs.
func TestGlobalIndexCollision_SetBangDoesNotWriteLibraryFrame(t *testing.T) {
	ctx := context.Background()
	eng := newCollisionEngine(ctx, t)

	v, err := eng.EvalMultiple(ctx, `
		(import (mylib))
		(begin
		  (define helper 1)
		  (set! helper 2)
		  (list (mac) (get-helper) helper))
	`)
	qt.Assert(t, err, qt.IsNil)
	qt.Assert(t, v.SchemeString(), qt.Equals, "(99 99 2)",
		qt.Commentf("set! on the user's top-level must not repoint or overwrite the "+
			"library's private binding of the same name."))
}
