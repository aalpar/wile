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

package compilation

import (
	"context"
	"testing"

	"github.com/aalpar/wile/pkg/environment"
	"github.com/aalpar/wile/pkg/internal/validate"
	"github.com/aalpar/wile/pkg/machine"

	qt "github.com/frankban/quicktest"
)

// TestFrameReclaimClassifiesForwardEdgeWithoutMutatingBinding guards the T1.5
// follow-on: the frame-reclaim classifier resolves a forward/mutual same-unit
// edge from the producer's thread-local StableInUnit, NOT by pre-stamping the
// shared *Binding's Stable field.
//
// History: the pre-stamp made a SHARED binding's Stable transiently true, ran the
// classifier (which read b.IsStable()), then reverted it. The atomic-meta
// migration made that transient value race-free but not private — a concurrent
// SRFI-18 compile sharing the namespace could observe the stamp (spurious redefine
// rejection, or an unsound reclaim verdict). The follow-on deletes the pre-stamp
// and reads StableInUnit off the ValidatedDefine, so there is no shared write and
// no window.
//
// Two things must hold, and this test pins both:
//   - the optimization still fires: foo forward-references bar; bar is
//     StableInUnit and non-capturing, so foo's frame is reclaimable. Before the
//     follow-on this required the pre-stamp — calling the classifier directly (no
//     stamp) left bar reading non-stable, so foo classified NON-reclaimable. This
//     assertion is therefore RED against the pre-fix classifier and GREEN after.
//   - no shared write: the bindings' Stable stays false across classification.
//     The classifier is now the only reader and it touches no binding meta.
func TestFrameReclaimClassifiesForwardEdgeWithoutMutatingBinding(t *testing.T) {
	c := qt.New(t)

	env := newNamespace(environment.NewNamespace().Runtime())
	// The reclaim classifier treats a same-unit edge as immutable only under
	// immutable top-level (rebindStable = StableInUnit ∧ this flag), the regime
	// where CompileValidatedBegin invokes it.
	env.Namespace().SetImmutableTopLevel(true)

	// foo forward-references bar; both are function defines (populate the reclaim
	// graph), each defined-once and never set! (StableInUnit=true). bar's body is a
	// literal, so it neither references a capturing operator nor escapes a closure.
	prog := parseSchemeExpr(t, env,
		"(begin (define (foo) (bar)) (define (bar) 1))")
	result := validate.ValidateExpression(context.Background(), env, prog)
	c.Assert(result.Ok(), qt.IsTrue)
	begin, ok := result.Expr.(*validate.ValidatedBegin)
	c.Assert(ok, qt.IsTrue)

	// Precondition: both defines are StableInUnit — the evidence the classifier now
	// reads thread-locally instead of pre-stamping onto the binding.
	byName := map[string]*validate.ValidatedDefine{}
	for _, e := range begin.Body() {
		d := e.(*validate.ValidatedDefine)
		byName[d.Name().Sym.Key] = d
	}
	c.Assert(byName["foo"].StableInUnit, qt.IsTrue)
	c.Assert(byName["bar"].StableInUnit, qt.IsTrue)

	// Predeclare the bindings (Pass 1 of CompileValidatedBegin) so we can prove the
	// classifier leaves their shared Stable field untouched.
	tpl := machine.NewNativeTemplate(0, 0, false)
	ctc := NewCompileTimeContinuation(tpl, env, machine.NewVMMacroEvaluator())
	for _, e := range begin.Body() {
		ctc.predeclareDefineBindingFromValidated(e)
	}
	barName := byName["bar"].Name()
	bar := env.GetBinding(barName.Sym, barName.Scopes())
	c.Assert(bar, qt.IsNotNil)
	c.Assert(bar.IsStable(), qt.IsFalse) // fresh define binding, not yet stable

	// Classify with NO pre-stamp — the way the follow-on's CompileValidatedBegin
	// now calls it.
	verdict := validate.ClassifyFrameReclaim(begin.Body(), env)

	// GREEN post-fix: foo's forward edge to bar resolved immutable via bar's
	// StableInUnit, so foo's frame is reclaimable. RED pre-fix: without the stamp
	// bar read non-stable, the edge was mutable, and foo classified non-reclaimable.
	c.Assert(verdict["foo"], qt.IsTrue,
		qt.Commentf("forward edge foo→bar must resolve immutable from bar's "+
			"thread-local StableInUnit, making foo's frame reclaimable"))
	c.Assert(verdict["bar"], qt.IsTrue)

	// The shared binding was never mutated by classification: no transient Stable
	// window exists for a concurrent compile to observe.
	c.Assert(bar.IsStable(), qt.IsFalse,
		qt.Commentf("classification must not write the shared binding's Stable field"))
}
