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

// White-box (package wile) because a Dialect's InstallForms takes the internal
// *forms.FormRegistry — an external wile_test package cannot import it. This is
// the in-tree-only Dialect boundary made concrete.
package wile

import (
	"context"
	"errors"
	"testing"

	"github.com/aalpar/wile/pkg/internal/forms"
	"github.com/aalpar/wile/pkg/werr"

	qt "github.com/frankban/quicktest"
)

// removeSetBangDialect drops set! from the forms registry, so (set! …) is no
// longer recognized as a special form in an engine built with this dialect.
type removeSetBangDialect struct{}

func (removeSetBangDialect) Name() string {
	return "no-set!"
}

func (removeSetBangDialect) InstallForms(fr *forms.FormRegistry) error {
	fr.Remove("set!")
	return nil
}

// failingDialect fails during InstallForms; the error must abort engine init.
type failingDialect struct{}

func (failingDialect) Name() string {
	return "boom"
}

func (failingDialect) InstallForms(_ *forms.FormRegistry) error {
	return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "failingDialect: intentional install failure")
}

// TestWithDialect_RemovesForm proves a dialect's per-engine FormRegistry drives
// the validate-time call-vs-form decision: with set! removed, (set! …) stops
// being a special form. The default engine (built AFTER the dialect engine) is
// unaffected, proving the dialect's Remove mutated only its own clone (COW).
func TestWithDialect_RemovesForm(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// set! on a LOCAL binding (immutable-top-level, on by default, governs only
	// top-level defines, so this isolates the dialect's effect on the form itself).
	const src = "(let ((x 1)) (set! x 2) x)"

	// Dialect engine first — its InstallForms mutates a cloned registry.
	eng, err := NewEngine(ctx, WithDialect(removeSetBangDialect{}))
	c.Assert(err, qt.IsNil)
	_, err = eng.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("set! must not be a special form under the no-set! dialect"))
	// Pin the mechanism: with set! no longer a special form it is validated as a
	// call, so set! resolves as an unbound reference — not some unrelated error.
	c.Assert(errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("removed set! must fail as an unbound reference, got %v", err))
	c.Assert(err.Error(), qt.Contains, "set!")

	// A default engine built afterwards still has set! — the dialect did not
	// corrupt the shared default registry.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	got, err := base.EvalMultiple(ctx, src)
	c.Assert(err, qt.IsNil,
		qt.Commentf("default engine must retain set! after a dialect removed it on its own clone"))
	c.Assert(got.SchemeString(), qt.Equals, "2")
}

// TestWithDialect_InstallError_AbortsEngineInit proves an InstallForms failure
// surfaces as an ErrEngineInit-wrapped engine construction error, not a silent
// partial engine.
func TestWithDialect_InstallError_AbortsEngineInit(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(failingDialect{}))
	c.Assert(err, qt.IsNotNil)
	c.Assert(eng, qt.IsNil)
	c.Assert(errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
		qt.Commentf("InstallForms failure must wrap ErrEngineInit, got %v", err))
	// Lossless chain: the dialect's own error stays reachable through the wrap.
	c.Assert(errors.Is(err, werr.ErrInvalidArgument), qt.IsTrue,
		qt.Commentf("the dialect's cause must survive the ErrEngineInit wrap, got %v", err))
}

// TestWithDialect_LastWins proves WithDialect is last-wins when supplied twice.
func TestWithDialect_LastWins(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	// First a failing dialect, then the remove dialect: the last one wins, so
	// init succeeds (the failing dialect never runs)...
	eng, err := NewEngine(ctx, WithDialect(failingDialect{}), WithDialect(removeSetBangDialect{}))
	c.Assert(err, qt.IsNil)
	c.Assert(eng, qt.IsNotNil)

	// ...and the LAST dialect is the ACTIVE one: its set!-removal took hold. This
	// rules out a bug that silently drops all dialects (which would also survive
	// the init-succeeds check above).
	_, err = eng.EvalMultiple(ctx, "(let ((x 1)) (set! x 2) x)")
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("the last dialect (no-set!) must be the active one, not silently dropped"))
}

// addWhenTrueDialect adds a new special form "when-true" by cloning if's spec
// (validator AND compiler) under a new name — exercising a dialect installing a
// codegen-bearing form (fr.Register carries Compile), the capability SP1's
// per-engine codegen dispatch unlocked beyond remove/rename.
type addWhenTrueDialect struct{}

func (addWhenTrueDialect) Name() string {
	return "when-true"
}

func (addWhenTrueDialect) InstallForms(fr *forms.FormRegistry) error {
	ifSpec := fr.Lookup("if")
	if ifSpec == nil {
		return werr.WrapForeignErrorf(werr.ErrInvalidArgument, "addWhenTrueDialect: no if spec to clone")
	}
	fr.Register(&forms.FormSpec{Name: "when-true", Validate: ifSpec.Validate, Compile: ifSpec.Compile})
	return nil
}

// TestWithDialect_AddsForm proves a dialect can ADD a special form that then
// compiles and evaluates in that engine — the headline capability SP1's
// per-engine codegen enabled. A default engine has no such form.
func TestWithDialect_AddsForm(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	eng, err := NewEngine(ctx, WithDialect(addWhenTrueDialect{}))
	c.Assert(err, qt.IsNil)
	got, err := eng.EvalMultiple(ctx, "(when-true #t 10 20)")
	c.Assert(err, qt.IsNil,
		qt.Commentf("dialect-installed when-true must compile and run"))
	c.Assert(got.SchemeString(), qt.Equals, "10")

	// A default engine does not know when-true.
	base, err := NewEngine(ctx)
	c.Assert(err, qt.IsNil)
	_, err = base.EvalMultiple(ctx, "(when-true #t 10 20)")
	c.Assert(err, qt.IsNotNil,
		qt.Commentf("default engine must not know the dialect-added when-true form"))
}

// TestWithDialect_WithNamespace_Conflict proves the combination is rejected
// rather than silently ignored: the pre-built-namespace path skips the
// forms-registry customization a dialect performs, so a silently-dropped dialect
// would be a wrong-behavior-with-no-feedback trap.
func TestWithDialect_WithNamespace_Conflict(t *testing.T) {
	c := qt.New(t)
	ctx := context.Background()

	ns, err := NewNamespace(ctx)
	c.Assert(err, qt.IsNil)

	eng, err := NewEngine(ctx, WithNamespace(ns), WithDialect(removeSetBangDialect{}))
	c.Assert(err, qt.IsNotNil)
	c.Assert(eng, qt.IsNil)
	c.Assert(errors.Is(err, werr.ErrEngineInit), qt.IsTrue,
		qt.Commentf("WithDialect+WithNamespace must be a hard error, got %v", err))
}
