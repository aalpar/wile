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
	"strings"
	"testing"

	qt "github.com/frankban/quicktest"

	"github.com/aalpar/wile/pkg/werr"
	"github.com/aalpar/wile/pkg/wile"
)

// nonDestructiveBangs are the bang-suffixed primitives that do NOT destructively
// write into an existing Scheme data structure, and so are correctly left in place
// by the NoMutation dialect.
//
// The bang suffix is a naming convention, not a semantic one. These mutate the
// world (a thread, a lock, the process CWD, a namespace), not a value a program is
// holding — which is the thing NoMutation is about. Each is listed with the reason
// it is out of scope, because "it's concurrency" is exactly the hand-wave that let
// read-bytevector! through.
var nonDestructiveBangs = map[string]string{
	// Concurrency control: they act on synchronisation objects, not on data.
	"thread-start!":                    "thread lifecycle",
	"thread-join!":                     "thread lifecycle",
	"thread-terminate!":                "thread lifecycle",
	"thread-sleep!":                    "thread lifecycle",
	"thread-yield!":                    "thread lifecycle",
	"thread-specific-set!":             "per-thread slot, not a shared data structure",
	"mutex-lock!":                      "lock acquisition",
	"mutex-unlock!":                    "lock release",
	"mutex-specific-set!":              "per-mutex slot",
	"condition-variable-signal!":       "condition signalling",
	"condition-variable-broadcast!":    "condition signalling",
	"condition-variable-specific-set!": "per-condvar slot",

	// Process / environment: outside the value world entirely.
	"set-current-directory!": "process state, not a Scheme value",
	"namespace-define!":      "reflective definition; define is not mutation, and NoMutation keeps define",
	"namespace-undefine!":    "reflective definition removal; the mirror of namespace-define!",
}

// TestNoMutationRemovesEveryDestructivePrimitive is the drift guard REVIEW.md §7
// asks for by name: "No test derives the destructive set from what the engine
// actually registers vs mutationPrimitives() (read-bytevector! sailed through)."
//
// It enumerates every bang-suffixed primitive the engine REGISTERS and requires each
// one to be classified — either removed by NoMutation, or explicitly listed above as
// not-a-destructive-update, with a reason. An unclassified bang fails the test.
//
// This is the shape that matters. A test that merely checked "the 13 names in
// mutationPrimitives() are gone" would have passed for as long as the bug existed,
// because the bug was never about the listed names — it was about the ones nobody
// had listed. string-copy!, string-fill! and read-bytevector! are all destructive
// writes into a caller-supplied object, all registered OUTSIDE registry/core, and
// all left standing by a dialect whose documented scope was "registry/core".
func TestNoMutationRemovesEveryDestructivePrimitive(t *testing.T) {
	ctx := context.Background()

	// KitchenSink so that io, gointerop and the thread extensions are all loaded —
	// otherwise the enumeration cannot see the primitives that actually sailed
	// through, which is the entire point.
	full, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = full.Close()
	}()

	noMut, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithDialect(wile.NoMutation),
	)
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = noMut.Close()
	}()

	// Arm A, over EVERY registered primitive, not only the bang-suffixed ones:
	// the annotation decides, so the check no longer depends on spelling. This
	// is the arm that record-modifier — destructive, no bang — was invisible to.
	for _, r := range full.Registry().Primitives() {
		if !r.Spec.Mutates {
			continue
		}
		name := r.Spec.Name
		t.Run("removed/"+name, func(t *testing.T) {
			_, stillBound := noMut.Get(name)
			qt.Assert(t, stillBound, qt.IsFalse,
				qt.Commentf("%s is annotated Mutates:true but is still bound on a "+
					"NoMutation engine. The annotation and mutationPrimitives() have "+
					"drifted; TestMutatesMatchesMutationPrimitives says which way.", name))
		})
	}

	// Arm B keeps the obligation that caught read-bytevector!, now as a
	// DOCUMENTATION requirement rather than the decision: a bang-suffixed
	// primitive that is not annotated destructive must say why, in writing.
	for _, r := range full.Registry().Primitives() {
		name := r.Spec.Name
		if !strings.HasSuffix(name, "!") || r.Spec.Mutates {
			continue
		}
		t.Run("classified/"+name, func(t *testing.T) {
			reason, exempt := nonDestructiveBangs[name]
			qt.Assert(t, exempt, qt.IsTrue,
				qt.Commentf("%s is bang-suffixed and not annotated Mutates:true. Either "+
					"it destructively updates an existing object — annotate its spec — or "+
					"it does not, and belongs in nonDestructiveBangs with a reason. An "+
					"unclassified bang primitive is how read-bytevector! sailed through.", name))
			_, stillBound := noMut.Get(name)
			qt.Assert(t, stillBound, qt.IsTrue,
				qt.Commentf("%s is exempt (%s), so it must survive NoMutation.", name, reason))
		})
	}
}

// TestMutatesMatchesMutationPrimitives is the ratchet that ties the annotation
// to the removal list, in both directions.
//
// It has to exist because PrimitiveRemover.RemovedPrimitives() takes no
// arguments — the dialect cannot see the registry — so the list stays a
// literal and something must keep it honest. This is exactly the arrangement
// InvokesProcedure already uses: an annotation on the spec, a hand-curated
// list, and a test that refuses to let them diverge.
func TestMutatesMatchesMutationPrimitives(t *testing.T) {
	ctx := context.Background()
	full, err := wile.NewEngine(ctx, wile.WithProfile(wile.KitchenSink))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = full.Close()
	}()

	annotated := map[string]bool{}
	for _, r := range full.Registry().Primitives() {
		if r.Spec.Mutates {
			annotated[r.Spec.Name] = true
		}
	}

	remover, ok := wile.NoMutation.(wile.PrimitiveRemover)
	qt.Assert(t, ok, qt.IsTrue,
		qt.Commentf("NoMutation must implement PrimitiveRemover; that capability is "+
			"the whole mechanism by which it narrows the top level"))
	listed := map[string]bool{}
	for _, name := range remover.RemovedPrimitives() {
		listed[name] = true
	}

	for name := range annotated {
		qt.Assert(t, listed[name], qt.IsTrue,
			qt.Commentf("%s is annotated Mutates:true but is not in mutationPrimitives(), "+
				"so a NoMutation engine still hands it out", name))
	}
	for name := range listed {
		qt.Assert(t, annotated[name], qt.IsTrue,
			qt.Commentf("mutationPrimitives() removes %s but no registered spec is "+
				"annotated Mutates:true for it — either the name is stale or the "+
				"annotation is missing", name))
	}
}

// TestNoMutationKeepsDefineValues records the R7RS expectation for the second half
// of dialect_nomutation.go:55, and is GATED because the implementation cannot
// currently meet it. It is a RED test kept red on purpose, not a passing guard.
//
// The defect is real and reproduces: on a NoMutation engine, (define-values (a b)
// (values 1 2)) dies with "expected a procedure, got #<primitive-expander:set!>".
// define-values is a DEFINITION (R7RS §5.3.3), not a mutation, so removing set!
// should not remove it — but the bootstrap macro expands to a set!, and with the
// set! FORM gone the name resolves cross-phase to set!'s internal expand-time
// object, which then gets applied.
//
// Why it is not fixed here. The obvious repair — rewrite the macro without set!,
// using a hygienic temporary for the value list — was implemented and REVERTED,
// because it regresses every engine:
//
//	a macro-introduced TOP-LEVEL binder is not uniquely renamed. Two define-values
//	in two different compilation units both introduce a global `tmp`, and the second
//	one dies under the default immutable top level with "cannot redefine immutable
//	top-level binding tmp". Within one unit it works; across units — which is exactly
//	the REPL and the embedder's Eval loop — it does not.
//
// And a temporary is unavoidable: without assignment you need one extra binding to
// hold the value list while the user's variables are bound from it. R7RS's own sample
// implementation of define-values uses set! for this reason, as does Chibi's.
//
// So there are two real fixes, both design decisions with blast radius, and REVIEW.md
// §8 asks the maintainer which:
//
//  1. Give Dialect a macro-removal capability (mirroring PrimitiveRemover) and have
//     NoMutation drop define-values outright. Cheap, contained, and honest — the
//     failure becomes "no such binding define-values" instead of an internal object
//     being applied — but it concedes an R7RS form.
//  2. Make macro-introduced top-level binders uniquely named, which is arguably a
//     hygiene bug in its own right (no macro can introduce a top-level temporary
//     today). Then the set!-free define-values above works, and NoMutation keeps it.
//
// RESOLVED: the second fix landed. define-values was rewritten set!-free using a
// template-introduced temporary, and the expander now gives macro-introduced
// top-level binders a fresh unique name (compilation/toplevel_binder_hygiene.go),
// so the temporary is unique per expansion and hidden from the use site. This
// test is now an enforced guard, not a gated gap.
func TestNoMutationKeepsDefineValues(t *testing.T) {
	ctx := context.Background()

	tcs := []struct {
		name string
		src  string
		want string
	}{
		{"proper list", `(begin (define-values (a b) (values 1 2)) (list a b))`, "(1 2)"},
		{"three vars", `(begin (define-values (a b c) (values 1 2 3)) (list a b c))`, "(1 2 3)"},
		{"dotted", `(begin (define-values (a . r) (values 1 2 3)) (list a r))`, "(1 (2 3))"},
	}
	for _, tc := range tcs {
		t.Run(tc.name, func(t *testing.T) {
			// A fresh engine per case: these programs all bind a/b under the
			// immutable top level, so sharing one engine would (correctly) fail
			// the second define with "cannot redefine immutable top-level a" —
			// a property of the immutable top level, not of define-values.
			eng, err := wile.NewEngine(ctx,
				wile.WithProfile(wile.KitchenSink),
				wile.WithDialect(wile.NoMutation),
			)
			qt.Assert(t, err, qt.IsNil)
			defer func() {
				_ = eng.Close()
			}()

			v, err := eng.EvalMultiple(ctx, tc.src)
			qt.Assert(t, err, qt.IsNil,
				qt.Commentf("define-values is a definition, not a mutation (R7RS §5.3.3)"))
			qt.Assert(t, v.SchemeString(), qt.Equals, tc.want)
		})
	}
}

// TestNoMutationStillRemovesSetBang guards the dialect's actual, working guarantee,
// so the skip above cannot be mistaken for "NoMutation is broken".
func TestNoMutationStillRemovesSetBang(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx,
		wile.WithProfile(wile.KitchenSink),
		wile.WithDialect(wile.NoMutation),
	)
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	_, err = eng.EvalMultiple(ctx, `(begin (define x 1) (set! x 2))`)
	qt.Assert(t, err, qt.IsNotNil, qt.Commentf("NoMutation must remove set!"))

	_, err = eng.EvalMultiple(ctx, `(begin (define v (vector 1)) (vector-set! v 0 9))`)
	qt.Assert(t, err, qt.IsNotNil, qt.Commentf("NoMutation must remove vector-set!"))
}

// TestNoMutationRemovedFormInMacroTemplateIsUnbound guards the residue of the
// 2026-07-13 review dialects.md finding: removing a *form* (here set!, via
// fr.Remove) dropped its compiler FormSpec but left the form's expand-phase
// *PrimitiveExpander binding reachable. A user macro whose template names the
// removed form pins that expand-phase binding onto the introduced identifier;
// the compiler, which no longer sees set! as a form, resolved the pin and emitted
// a runtime load of the internal expander. The program then applied
// #<primitive-expander:set!> as a procedure — an internal compile-time object
// leaking into the value world — instead of failing as an unbound reference.
//
// The contract (dialect.go: "referencing a removed name is an unbound reference,
// werr.ErrNoSuchBinding") must hold whether the removed keyword is written
// literally or introduced by a macro template. This is the general defect: it
// applies to any removed form carrying a primitive expander, not just set!.
func TestNoMutationRemovedFormInMacroTemplateIsUnbound(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithDialect(wile.NoMutation))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	// A user macro whose template references the removed set! form. Before the
	// fix this compiled and, at runtime, applied #<primitive-expander:set!>.
	src := `(define-syntax my-set (syntax-rules () ((_ v e) (set! v e))))
	        (let ((x 1)) (my-set x 2) x)`
	_, err = eng.EvalMultiple(ctx, src)
	qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("a removed form named in a macro template must be an unbound reference, got %v", err))
	qt.Assert(t, err.Error(), qt.Contains, "set!",
		qt.Commentf("the unbound identifier must be set! specifically"))
	qt.Assert(t, err.Error(), qt.Not(qt.Contains), "primitive-expander",
		qt.Commentf("the internal expand-phase handler must never leak into runtime, got %v", err))
}

// TestNoMutationRemovesParameterRawSet pins finding 5.1: %parameter-raw-set!
// destructively writes a parameter's held value (prim_parameters.go:
// param.SetValue) and is no longer used by the parameterize macro, which stores
// bindings as continuation marks via %parameter-convert. It is a user-reachable
// destructive update, the direct analogue of the removed set-box!, so NoMutation
// must remove it.
func TestNoMutationRemovesParameterRawSet(t *testing.T) {
	ctx := context.Background()
	eng, err := wile.NewEngine(ctx, wile.WithDialect(wile.NoMutation))
	qt.Assert(t, err, qt.IsNil)
	defer func() {
		_ = eng.Close()
	}()

	// Before the fix this evaluated to 99 — a destructive parameter update
	// surviving a dialect whose whole claim is that no destructive update survives.
	_, err = eng.EvalMultiple(ctx,
		`(let ((p (make-parameter 10))) (%parameter-raw-set! p 99) (p))`)
	qt.Assert(t, errors.Is(err, werr.ErrNoSuchBinding), qt.IsTrue,
		qt.Commentf("%%parameter-raw-set! must be unbound under no-mutation, got %v", err))
	qt.Assert(t, err.Error(), qt.Contains, "%parameter-raw-set!",
		qt.Commentf("the unbound identifier must be %%parameter-raw-set! specifically"))
}
